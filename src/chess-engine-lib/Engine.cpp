#include "Engine.h"

#include "MoveSearcher.h"
#include "Syzygy.h"

#include <algorithm>
#include <atomic>

class Engine::Impl {
  public:
    Impl();
    ~Impl();

    Impl(const Impl&)            = delete;
    Impl& operator=(const Impl&) = delete;

    Impl(Impl&&)            = delete;
    Impl& operator=(Impl&&) = delete;

    TimeManager& getTimeManager();

    void setFrontEnd(IFrontEnd* frontEnd);

    void newGame();

    [[nodiscard]] SearchInfo findMove(
            const GameState& gameState, const std::vector<Move>& searchMoves);

    void interruptSearch();

    [[nodiscard]] int getDefaultTTableSizeInMb() const;

    void setTTableSize(int requestedSizeInMb);

    [[nodiscard]] EvalT evaluate(const GameState& gameState) const;

    void initializeSyzygy(std::string_view syzygyDir);

  private:
    StackOfVectors<Move> moveStack_;
    TimeManager timeManager_;
    Evaluator evaluator_;
    MoveSearcher moveSearcher_;
    IFrontEnd* frontEnd_ = nullptr;

    std::atomic<bool> stopSearch_ = false;

    bool hasSyzygy_ = false;
};

Engine::Impl::Impl()
    : evaluator_(EvalParams::getDefaultParams(), /*usePawnKingEvalHashTable*/ true),
      moveSearcher_(timeManager_, evaluator_) {
    moveStack_.reserve(1'000);
}

Engine::Impl::~Impl() {
    if (hasSyzygy_) {
        tearDownSyzygy();
        hasSyzygy_ = false;
    }
}

TimeManager& Engine::Impl::getTimeManager() {
    return timeManager_;
}

void Engine::Impl::setFrontEnd(IFrontEnd* frontEnd) {
    frontEnd_ = frontEnd;
    timeManager_.setFrontEnd(frontEnd);
    moveSearcher_.setFrontEnd(frontEnd);

    frontEnd_->addOption(FrontEndOption::createString(
            "SyzygyPath", "", [this](const std::string_view v) { initializeSyzygy(v); }));
}

void Engine::Impl::newGame() {
    moveSearcher_.newGame();
    stopSearch_ = false;
}

SearchInfo Engine::Impl::findMove(
        const GameState& gameState, const std::vector<Move>& searchMoves) {
    stopSearch_ = false;

    const auto allLegalMoves = gameState.generateMoves(moveStack_);

    if (allLegalMoves.empty()) {
        throw std::invalid_argument("No legal moves available in the current position.");
    }

    for (const auto& move : searchMoves) {
        bool isLegal = false;
        for (const auto& legalMove : allLegalMoves) {
            if (move == legalMove) {
                isLegal = true;
                break;
            }
        }

        if (!isLegal) {
            throw std::invalid_argument(
                    "Requested move to search is not legal: " + move.toExtendedString());
        }
    }

    bool tbHit = false;

    // Find moves that are optimal based on Syzygy tablebases.
    std::vector<Move> syzygyRootMoves;
    if (hasSyzygy_ && canProbeSyzgyRoot(gameState)) {
        syzygyRootMoves = getSyzygyRootMoves(gameState);

        tbHit = !syzygyRootMoves.empty();
    }

    // If searchMoves was provided, filter optimal moves to that.
    std::vector<Move> syzygySearchMoves;
    if (!syzygyRootMoves.empty() && !searchMoves.empty()) {
        for (const auto& syzygyMove : syzygyRootMoves) {
            if (std::ranges::contains(searchMoves, syzygyMove)) {
                syzygySearchMoves.push_back(syzygyMove);
            }
        }
    }

    // Restrict moves to search to the first valid one of:
    // - TB optimal moves that were included in requested moves to search.
    // - All requested moves to search.
    // - All TB optimal moves.
    // - All moves (nullptr).
    const std::vector<Move>* const movesToSearch = !syzygySearchMoves.empty() ? &syzygySearchMoves
                                                 : !searchMoves.empty()       ? &searchMoves
                                                 : !syzygyRootMoves.empty()   ? &syzygyRootMoves
                                                                              : nullptr;

    const int numMovesToConsider =
            movesToSearch ? (int)movesToSearch->size() : (int)allLegalMoves.size();

    moveSearcher_.resetSearchStatistics();
    moveSearcher_.prepareForNewSearch(gameState, movesToSearch, tbHit);

    GameState copyState(gameState);

    std::optional<EvalT> evalGuess = std::nullopt;
    SearchInfo searchInfo;

    int depth = 1;

    const auto rootNodeInfo = moveSearcher_.getRootNodeInfo(gameState);
    if (rootNodeInfo) {
        depth     = max(depth, rootNodeInfo->depth);
        evalGuess = rootNodeInfo->eval;
    }

    if (frontEnd_) {
        // Preparations are done; we can now safely process an interrupt request.
        // Report to the front end that the search has started.
        frontEnd_->reportSearchHasStarted();
    }

    for (; depth <= MoveSearcher::kMaxDepth; ++depth) {
        // Not const to enable std::move of the PV.
        auto searchResult =
                moveSearcher_.searchForBestMove(copyState, depth, moveStack_, evalGuess);

        evalGuess = searchResult.eval;

        if (searchResult.principalVariation.size() > 0) {
            searchInfo.principalVariation = std::move(searchResult.principalVariation);
        }

        const auto searchStatistics = moveSearcher_.getSearchStatistics();

        searchInfo.score      = searchResult.eval;
        searchInfo.depth      = depth;
        searchInfo.statistics = searchStatistics;

        if (searchResult.wasInterrupted) {
            if (frontEnd_) {
                frontEnd_->reportPartialSearch(searchInfo);
            }
            searchInfo.depth -= 1;
            break;
        }

        if (frontEnd_) {
            frontEnd_->reportFullSearch(searchInfo);
        }

        if (isMate(searchResult.eval)
            && timeManager_.shouldStopAfterMateFound(
                    depth, getMateDistanceInPly(searchResult.eval))) {
            break;
        }

        if (timeManager_.shouldStopAfterFullPly(depth, numMovesToConsider)) {
            break;
        }
    }

    if (frontEnd_) {
        frontEnd_->reportSearchStatistics(searchInfo.statistics);
    }

    if (searchInfo.principalVariation.empty()) {
        if (frontEnd_) {
            frontEnd_->reportError(
                    "Search reported no principal variation! Falling back to any legal move.");
        }
        // Fill in the principal variation with any move we can come up with...

        if (movesToSearch) {
            // If we were searching among a specific set of moves, just grab the first one of those.
            MY_ASSERT(!movesToSearch->empty());
            searchInfo.principalVariation.push_back(movesToSearch->front());
        } else {
            // Otherwise, just grab the first legal move.
            MY_ASSERT(!allLegalMoves.empty());
            searchInfo.principalVariation.push_back(allLegalMoves.front());
        }
    }

    if (timeManager_.isInfiniteSearch() && !stopSearch_) {
        // We're done already but the user hasn't requested the search to stop yet.
        // So we wait until a stop request is issued.
        stopSearch_.wait(/*old*/ false);
    }
    stopSearch_ = false;

    return searchInfo;
}

void Engine::Impl::interruptSearch() {
    moveSearcher_.interruptSearch();
    stopSearch_ = true;
    stopSearch_.notify_one();
}

int Engine::Impl::getDefaultTTableSizeInMb() const {
    return moveSearcher_.getDefaultTTableSizeInMb();
}

void Engine::Impl::setTTableSize(const int requestedSizeInMb) {
    moveSearcher_.setTTableSize(requestedSizeInMb);
}

EvalT Engine::Impl::evaluate(const GameState& gameState) const {
    return evaluator_.evaluate(gameState);
}

void Engine::Impl::initializeSyzygy(std::string_view syzygyDir) {
    if (!syzygyPathIsValid(syzygyDir)) {
        throw std::invalid_argument(std::format(
                "invalid syzygy path. Must be a {}-separated list of directories.",
                getSyzygyPathSeparator()));
    }

    if (hasSyzygy_) {
        tearDownSyzygy();
        hasSyzygy_ = false;
    }

    if (!syzygyDir.empty()) {
        const int tbPieces = initSyzygy(std::string(syzygyDir));

        if (tbPieces == 0) {
            throw std::invalid_argument("Failed to initialize Syzygy tablebases.");
        }

        frontEnd_->reportString(
                "Syzygy tablebases initialized with " + std::to_string(tbPieces) + " pieces.");

        hasSyzygy_ = true;
    }

    moveSearcher_.setSyzygyEnabled(hasSyzygy_);
}

// Implementation of interface: forward to implementation

Engine::Engine() : impl_(std::make_unique<Engine::Impl>()) {}

Engine::~Engine() = default;

TimeManager& Engine::getTimeManager() {
    return impl_->getTimeManager();
}

void Engine::setFrontEnd(IFrontEnd* frontEnd) {
    impl_->setFrontEnd(frontEnd);
}

void Engine::newGame() {
    impl_->newGame();
}

SearchInfo Engine::findMove(const GameState& gameState, const std::vector<Move>& searchMoves) {
    return impl_->findMove(gameState, searchMoves);
}

void Engine::interruptSearch() {
    impl_->interruptSearch();
}

int Engine::getDefaultTTableSizeInMb() const {
    return impl_->getDefaultTTableSizeInMb();
}

void Engine::setTTableSize(const int requestedSizeInMb) {
    impl_->setTTableSize(requestedSizeInMb);
}

EvalT Engine::evaluate(const GameState& gameState) const {
    return impl_->evaluate(gameState);
}
