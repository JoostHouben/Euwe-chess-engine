#include "Syzygy.h"

#include "Pyrrhic/tbprobe.h"

#include "Macros.h"
#include "Math.h"
#include "MyAssert.h"

#include <algorithm>
#include <filesystem>
#include <ranges>

namespace {

[[nodiscard]] FORCE_INLINE std::uint64_t getSyzygyOccupancy(
        const GameState& gameState, const Side side) {
    return (std::uint64_t)gameState.getSideOccupancy(side);
}

[[nodiscard]] FORCE_INLINE std::uint64_t getSyzygyPieceBb(
        const GameState& gameState, const Piece piece) {
    return (std::uint64_t)(gameState.getPieceBitBoard(Side::White, piece)
                           | gameState.getPieceBitBoard(Side::Black, piece));
}

[[nodiscard]] FORCE_INLINE unsigned getSyzygyEnPassantTarget(const GameState& gameState) {
    const BoardPosition enPassantTarget = gameState.getEnPassantTarget();
    if (enPassantTarget == BoardPosition::Invalid) {
        return 0;
    }
    return (unsigned)enPassantTarget;
}

[[nodiscard]] FORCE_INLINE bool getSyzygySide(const GameState& gameState) {
    const Side side = gameState.getSideToMove();
    return side == Side::White;
}

}  // namespace

char getSyzygyPathSeparator() {
#ifdef _WIN32
    return ';';
#else
    return ':';
#endif
}

bool syzygyPathIsValid(std::string_view syzygyDirs) {
    return std::ranges::all_of(
            syzygyDirs | std::views::split(getSyzygyPathSeparator()), [](const auto& part) {
                const std::filesystem::path p(part.begin(), part.end());
                return std::filesystem::is_directory(p);
            });
}

int initSyzygy(const std::string& syzygyDirs) {
    tb_init(syzygyDirs.c_str());

    return TB_LARGEST;
}

void tearDownSyzygy() {
    tb_free();
}

FORCE_INLINE bool canProbeSyzgyRoot(const GameState& gameState) {
    if (gameState.getCastlingRights() != GameState::CastlingRights::None) {
        return false;
    }
    if (gameState.getNumPieces() > TB_LARGEST) {
        return false;
    }

    return true;
}

std::vector<Move> getSyzygyRootMoves(const GameState& gameState) {
    MY_ASSERT_DEBUG(canProbeSyzgyRoot(gameState));

    TbRootMoves tbRootMoves;

    int probeResult = tb_probe_root_dtz(
            getSyzygyOccupancy(gameState, Side::White),
            getSyzygyOccupancy(gameState, Side::Black),
            getSyzygyPieceBb(gameState, Piece::King),
            getSyzygyPieceBb(gameState, Piece::Queen),
            getSyzygyPieceBb(gameState, Piece::Rook),
            getSyzygyPieceBb(gameState, Piece::Bishop),
            getSyzygyPieceBb(gameState, Piece::Knight),
            getSyzygyPieceBb(gameState, Piece::Pawn),
            gameState.getPlySinceCaptureOrPawn(),
            getSyzygyEnPassantTarget(gameState),
            getSyzygySide(gameState),
            gameState.isRepetition(2),
            &tbRootMoves);

    if (probeResult == 0 || tbRootMoves.size == 0) {
        // Use WDL probe as a fallback.
        // TODO: do we then need to use WDL tables in search?
        probeResult = tb_probe_root_wdl(
                getSyzygyOccupancy(gameState, Side::White),
                getSyzygyOccupancy(gameState, Side::Black),
                getSyzygyPieceBb(gameState, Piece::King),
                getSyzygyPieceBb(gameState, Piece::Queen),
                getSyzygyPieceBb(gameState, Piece::Rook),
                getSyzygyPieceBb(gameState, Piece::Bishop),
                getSyzygyPieceBb(gameState, Piece::Knight),
                getSyzygyPieceBb(gameState, Piece::Pawn),
                gameState.getPlySinceCaptureOrPawn(),
                getSyzygyEnPassantTarget(gameState),
                getSyzygySide(gameState),
                /*useRule50 = */ true,
                &tbRootMoves);

        if (probeResult == 0 || tbRootMoves.size == 0) {
            return {};
        }
    }

    MY_ASSERT(probeResult != 0);
    MY_ASSERT(tbRootMoves.size != 0);

    std::int32_t bestTbRank = tbRootMoves.moves[0].tbRank;
    for (unsigned i = 1; i < tbRootMoves.size; ++i) {
        bestTbRank = max(bestTbRank, tbRootMoves.moves[i].tbRank);
    }

    const auto getMoveFromTb = [&](const TbRootMove& tbMove) -> Move {
        const BoardPosition from = (BoardPosition)PYRRHIC_MOVE_FROM(tbMove.move);
        const BoardPosition to   = (BoardPosition)PYRRHIC_MOVE_TO(tbMove.move);

        MoveFlags flags = MoveFlags::None;
        if (PYRRHIC_MOVE_IS_ENPASS(tbMove.move)) {
            flags |= MoveFlags::IsEnPassant;
        }
        if (PYRRHIC_MOVE_IS_QPROMO(tbMove.move)) {
            flags |= Piece::Queen;
        }
        if (PYRRHIC_MOVE_IS_RPROMO(tbMove.move)) {
            flags |= Piece::Rook;
        }
        if (PYRRHIC_MOVE_IS_BPROMO(tbMove.move)) {
            flags |= Piece::Bishop;
        }
        if (PYRRHIC_MOVE_IS_NPROMO(tbMove.move)) {
            flags |= Piece::Knight;
        }

        if (gameState.getPieceOnSquare(to) != ColoredPiece::Invalid || isEnPassant(flags)) {
            flags |= MoveFlags::IsCapture;
        }

        const Piece pieceToMove = getPiece(gameState.getPieceOnSquare(from));

        return Move{pieceToMove, from, to, flags};
    };

    std::vector<Move> moves;
    for (unsigned i = 0; i < tbRootMoves.size; ++i) {
        const TbRootMove& tbMove = tbRootMoves.moves[i];

        if (tbMove.tbRank != bestTbRank) {
            continue;
        }

        moves.push_back(getMoveFromTb(tbMove));
    }

    return moves;
}

FORCE_INLINE bool canProbeSyzgyWdl(const GameState& gameState) {
    return canProbeSyzgyWdl(
            gameState.getPlySinceCaptureOrPawn(),
            gameState.getCastlingRights(),
            gameState.getNumPieces());
}

FORCE_INLINE bool canProbeSyzgyWdl(
        const int plySinceCaptureOrPawn,
        const GameState::CastlingRights castlingRights,
        const int numPieces) {
    if (plySinceCaptureOrPawn != 0) {
        return false;
    }
    if (castlingRights != GameState::CastlingRights::None) {
        return false;
    }
    if (numPieces > TB_LARGEST) {
        return false;
    }

    return true;
}

FORCE_INLINE std::optional<EvalT> probeSyzygyWdl(const GameState& gameState) {
    MY_ASSERT_DEBUG(canProbeSyzgyWdl(gameState));

    unsigned probeResult = tb_probe_wdl(
            getSyzygyOccupancy(gameState, Side::White),
            getSyzygyOccupancy(gameState, Side::Black),
            getSyzygyPieceBb(gameState, Piece::King),
            getSyzygyPieceBb(gameState, Piece::Queen),
            getSyzygyPieceBb(gameState, Piece::Rook),
            getSyzygyPieceBb(gameState, Piece::Bishop),
            getSyzygyPieceBb(gameState, Piece::Knight),
            getSyzygyPieceBb(gameState, Piece::Pawn),
            getSyzygyEnPassantTarget(gameState),
            getSyzygySide(gameState));

    MY_ASSERT_DEBUG(probeResult != TB_RESULT_FAILED);

    if (probeResult == TB_RESULT_FAILED) {
        return std::nullopt;
    }

    switch (probeResult) {
        case TB_WIN:
            return mateIn(200);

        case TB_CURSED_WIN:
            return (EvalT)1;

        case TB_DRAW:
            return (EvalT)0;

        case TB_BLESSED_LOSS:
            return (EvalT)-1;

        case TB_LOSS:
            return (EvalT)-mateIn(200);

        default:
            UNREACHABLE;
    }
}
