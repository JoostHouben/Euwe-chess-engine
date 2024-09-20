#include "Engine.h"

#include "MoveSearcher.h"

class Engine::Impl {
  public:
    Impl();

    TimeManager& getTimeManager();

    void setFrontEnd(IFrontEnd* frontEnd);

    void newGame();

    [[nodiscard]] SearchInfo findMove(const GameState& gameState);

    void interruptSearch();

    [[nodiscard]] int getDefaultTTableSizeInMb() const;

    void setTTableSize(int requestedSizeInMb);

    [[nodiscard]] EvalT evaluate(const GameState& gameState) const;

  private:
    StackOfVectors<Move> moveStack_;
    TimeManager timeManager_;
    Evaluator evaluator_;
    MoveSearcher moveSearcher_;
    IFrontEnd* frontEnd_ = nullptr;
};

Engine::Impl::Impl()
    : evaluator_(EvalParams::getDefaultParams()), moveSearcher_(timeManager_, evaluator_) {
    moveStack_.reserve(1'000);
}

TimeManager& Engine::Impl::getTimeManager() {
    return timeManager_;
}

void Engine::Impl::setFrontEnd(IFrontEnd* frontEnd) {
    frontEnd_ = frontEnd;
    timeManager_.setFrontEnd(frontEnd);
    moveSearcher_.setFrontEnd(frontEnd);
}

void Engine::Impl::newGame() {
    moveSearcher_.newGame();
}

SearchInfo Engine::Impl::findMove(const GameState& gameState) {
    int maxDepth = MoveSearcher::kMaxDepth;

    {
        const auto moves = gameState.generateMoves(moveStack_);
        if (moves.size() == 1) {
            // Only one legal move. We still search to get a score and a PV.
            // Search to depth 2 to get a guess for the opponent's follow-up move.
            maxDepth = 2;
        }
    }

    moveSearcher_.prepareForNewSearch(gameState);

    GameState copyState(gameState);

    moveSearcher_.resetSearchStatistics();

    std::optional<EvalT> evalGuess = std::nullopt;
    SearchInfo searchInfo;

    auto startTime = std::chrono::high_resolution_clock::now();

    int depth;
    for (depth = 1; depth <= maxDepth; ++depth) {
        const auto searchResult =
                moveSearcher_.searchForBestMove(copyState, depth, moveStack_, evalGuess);

        evalGuess = searchResult.eval;

        if (searchResult.principalVariation.size() > 0) {
            searchInfo.principalVariation = std::vector<Move>(
                    searchResult.principalVariation.begin(), searchResult.principalVariation.end());
        }

        const auto timeNow = std::chrono::high_resolution_clock::now();
        const auto millisecondsElapsed =
                std::chrono::duration_cast<std::chrono::milliseconds>(timeNow - startTime).count();

        const auto searchStatistics = moveSearcher_.getSearchStatistics();

        const std::uint64_t numNodes =
                searchStatistics.normalNodesSearched + searchStatistics.qNodesSearched;
        const float nodesPerSecond = static_cast<float>(numNodes) / millisecondsElapsed * 1'000.0f;

        searchInfo.score          = searchResult.eval;
        searchInfo.depth          = depth;
        searchInfo.timeMs         = (int)millisecondsElapsed;
        searchInfo.numNodes       = numNodes;
        searchInfo.nodesPerSecond = nodesPerSecond;

        if (searchResult.wasInterrupted) {
            if (frontEnd_) {
                frontEnd_->reportPartialSearch(searchInfo, searchStatistics);
            }
            searchInfo.depth -= 1;
            break;
        }

        if (frontEnd_) {
            frontEnd_->reportFullSearch(searchInfo, searchStatistics);
        }

        if (isMate(searchResult.eval) && getMateDistanceInPly(searchResult.eval) <= depth) {
            break;
        }

        if (timeManager_.shouldStopAfterFullPly(depth)) {
            break;
        }
    }

    if (frontEnd_) {
        frontEnd_->reportSearchStatistics(moveSearcher_.getSearchStatistics());
    }

    return searchInfo;
}

void Engine::Impl::interruptSearch() {
    moveSearcher_.interruptSearch();
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

SearchInfo Engine::findMove(const GameState& gameState) {
    return impl_->findMove(gameState);
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
