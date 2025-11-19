#include "Bench.h"

#include "MyAssert.h"

#include <format>
#include <string>
#include <vector>

namespace {

//NOLINTBEGIN(bugprone-suspicious-missing-comma)

std::vector<std::string> getBenchPositions() {
    return {
            "position startpos",

            // 'Kiwipete'
            "position fen r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",

            // Opening positions from UHO_Lichess_4852_v1.epd
            "position fen r1b1k1nr/ppp2ppp/2np1q2/8/1bP5/P1NQ1N2/1P2PPPP/R1B1KB1R b KQkq - 0 7",
            "position fen rnbqk2r/ppp2ppp/8/b2pp3/3Pn3/2PBBN2/PP3PPP/RN1QK2R w KQkq - 0 7",
            "position fen rnbq1rk1/ppp3bp/4ppp1/3pPn2/3P1P2/2NB1N2/PPP3PP/R1BQK2R w KQ - 0 9",
            "position fen r3kb1r/pppb1ppp/2np1q1n/4p3/4P3/P1N2N1P/1PPPBPP1/R1BQK2R b KQkq - 0 7",
            "position fen rnb1k2r/ppp2ppp/1b1p1q2/1N6/2BNPPn1/8/PPP3PP/R1BQK2R w KQkq - 3 9",

            // Midgame positions
            "position fen r1bqkbn1/ppp1ppp1/2n5/7r/2Pp4/3P3P/PP2PPB1/RNBQK1NR b KQq - 0 6 moves "
            "g8f6 g2f3 h5e5 b1d2 c8d7 d2e4 a7a5 a2a3 a5a4 c1f4 f6e4 f3e4 "
            "e5c5 f4g3 e7e6 g1f3 f7f5 e4c6 d7c6 h1g1 c6f3 e2f3 b7b5 a1c1 d8d7 c4b5 d7b5 c1c5 b5c5 "
            "e1f1 e8f7 f1g2 c5b6 d1e2 c7c5 g3e5 a8a5 f3f4 f7g8 e2c2 f8d6 g2g3 a5a8 g1b1 d6e5 f4e5 "
            "b6b5",

            "position startpos moves e2e4 c7c5 c2c3 d7d5 e4d5 d8d5 d2d4 c8f5 c1e3 e7e6 b1a3 c5d4 "
            "a3b5 d5d7 d1d4 b8c6 d4d7 e8d7 g1f3 a7a6 e1c1 d7c8 b5d4 g8e7 f3g5 f5g6 d4c6 e7c6 f1d3 "
            "g6d3 g5f7 d3e4 f7h8 e4g2 h1g1",

            "position startpos moves d2d4 g8f6 c1g5 f6e4 g5f4 c7c5 f2f3 e4f6 d4d5 f6h5 f4e3 e7e5 "
            "g2g4 h5f4 e3f2 d8b6 b2b3 h7h5 e2e3 b6a5 c2c3 f4g6 g4h5 h8h5 f3f4 h5h8 f4f5 g6e7 e3e4 "
            "d7d6 d1f3 b7b5 f1d3 b8d7 g1e2 g7g6 b1d2 g6f5 a2a4 a7a6 e1g1 b5a4 e4f5 c8b7 a1a4 a5b6 "
            "c3c4 f8h6 d2e4 e8c8 b3b4 d8g8 g1h1 b6c7 f1b1 f7f6",

            "position startpos moves e2e4 c7c5 b1c3 b8c6 f1b5 g7g6 b5c6 d7c6 f2f4 f8g7 g1f3 g8f6 "
            "e1g1 e8g8 d2d3 d8c7 d1e1 c8g4 c1e3 g4f3 f1f3 b7b6 h2h3 a8d8 a2a3 f6e8 b2b4 g7d4 b4c5 "
            "b6c5 a1b1 c7a5 c3e2 d4e3 f3e3 a5e1 b1e1 e8c7 e4e5 c7b5 a3a4",

            "position startpos moves g1f3 g8f6 g2g3 b7b5 f1g2 c8b7 e1g1 c7c5 d2d3 g7g6 e2e4 d7d6 "
            "d3d4 c5d4 f3d4 a7a6 b1c3 e7e5 d4b3 b8d7 f1e1 h7h5 a2a4 b5b4 c3d5 f6d5 e4d5 a6a5 b3d4 "
            "f8g7 d4c6 d8c7 c2c3 b4c3 b2c3 d7b8 c6d4 e8g8 d4b5 c7e7 a1b1 b7a6 c3c4 b8d7 c1a3 a6b5 "
            "c4b5",

            "position fen 3rr1k1/pp1nq3/4p3/4n1p1/4N3/8/PPPQBPP1/4RRK1 b - - 1 20",
            "position fen r4rk1/p4p2/3b3p/2pp1qp1/8/2P3Q1/PP3PPP/R3R1K1 w - - 0 21",
            "position fen 3r2k1/pp3pb1/6p1/P2p3p/1B2nP1q/1R2PQ1P/1Pr1BP2/3R2K1 w - - 4 27",
            "position fen r3r1k1/1p3pp1/pq6/5bN1/3p1P1p/bP3Q1P/P2B2PK/2R1R3 w - - 1 26",
            "position fen 2r3k1/2pn1p2/pqn3p1/3p2Pp/1p1P1B1P/1P3P2/PKP5/1N1RQ3 w - - 1 26",
            "position fen 1r4k1/5p2/p1qpp1pP/3p3n/3P3P/1rPP1P2/1P1Q4/1K1NR1R1 w - - 2 26",

            // Likely drawn from 3-fold-repetition
            "position startpos moves e2e4 c7c5 g1f3 e7e6 d2d4 c5d4 f3d4 a7a6 b1c3 d8c7 f1d3 f8d6 "
            "d1g4 h7h5 g4e2 g8f6 d4f3 b8c6 h2h3 b7b5 e1g1 c6e5 f3e5 d6e5 f2f4 e5c3 b2c3 c7c3 c1d2 "
            "c3c5 g1h2 d7d6 e4e5 f6d5 c2c4 b5c4 d3c4 d6e5 f4e5 c8d7 a1c1 d7b5 a2a4 b5c4 c1c4 c5a7 "
            "f1c1 a7d7 c4c6 d7b7 d2g5 e8g8 e2h5 a8c8 c6c4 c8c4 c1c4 b7b8",

            // Adjudicated end game positions
            "position fen rn1qkbnr/ppp3pp/3pb3/4p3/4Pp2/1P1P1N1P/P1P2PP1/RNBQKB1R w KQkq - 0 6 "
            "moves d3d4 b8d7 b1d2 d8f6 f1c4 e8c8 c4e6 f6e6 e1g1 g7g5 d2c4 g8f6 "
            "d1e1 g5g4 d4d5 e6g8 h3g4 g8g4 f3h2 g4h5 f2f3 h8g8 f1f2 g8g6 c4d2 c8b8 c1a3 f8e7 c2c4 "
            "d8g8 d2f1 h5g5 g1h1 f6h5 e1d2 h5g3 h1g1 g5h4 c4c5 g6h6 c5d6 e7d6 a3d6 c7d6 a1c1 g3f1 "
            "g1f1 h4h2 d2c3 b8a8 c3c7 h2g3 f1e2 d7b6 c1c2 h6h2 e2f1 g3h4 c7d6 h2g2 d6c5",

            "position fen rn1qk2r/pp3pbp/3p1np1/2pPp3/4P1b1/2N3P1/PPP1NPBP/R1BQ1RK1 b kq - 7 8 "
            "moves e8g8 h2h3 g4d7 c1e3 b8a6 a2a3 b7b5 a3a4 b5b4 c3b5 d8b6 c2c4 "
            "b4c3 e2c3 a6b4 a4a5 b6b8 d1e2 h7h6 g1h2 h6h5 b5a3 d7c8 a3c4 c8a6 b2b3 b8d8 e2d2 a8b8 "
            "f2f4 e5f4 e3f4 h5h4 g3g4 f6h7 h2h1 a6c4 b3c4 d8e7 c3b5 b8d8 a1e1 a7a6 b5c3 g7f6 e4e5 "
            "f6e5 f4e5 d6e5 c3e4 e7c7 d5d6 c7a5 e1d1 d8d7 d2f2 f8c8 f2h4 b4c2 h4h6 a5a3 f1f3 a3a2 "
            "f3f2 a2a4 f2f7 d7f7 h6g6 g8h8 g6f7 c8f8 e4c5 f8f7 c5a4 h7f8 c4c5 c2d4 c5c6 f8e6 d1c1 "
            "d4b5",

            // Positions near SyzygyTB horizon
            "position fen rnbq1rk1/ppp3bp/4ppp1/3pPn2/3P1P2/2NB1N2/PPP3PP/R1BQK2R w KQ - 0 9 moves "
            "e1g1 b8c6 c3e2 c6b4 d3f5 e6f5 b2b3 f8e8 c1a3 a7a5 d1d2 b4c6 h2h3 b7b6 e2g3 c8a6 f1e1 "
            "d8d7 g1h2 a8d8 a1d1 d7f7 a3b2 c6b4 a2a3 b4c6 a3a4 c6b4 b2c3 b4a2 c3a1 a2b4 a1b2 f6e5 "
            "d4e5 h7h6 f3d4 c7c5 d4b5 a6b5 a4b5 d5d4 c2c3 d4c3 d2d8 c3b2 d8b6 g8h7 e5e6 f7f6 b6d6 "
            "b4c2 e1e5 f6e7 d6e7 e8e7 e5e2 c2b4 b5b6 g7d4 h3h4 b4d5 e2b2 e7e6 b2a2 d5c3 d1d4 c5d4 "
            "a2a5 e6b6 a5a7 h7g8 h4h5 c3e4 h5g6 e4g3 h2g3 b6d6 b3b4 d4d3 a7a1 d3d2 a1d1 g8g7 g3f2 "
            "g7g6 f2e3 d6b6 d1d2 b6b4 d2d6 g6h5 d6a6 b4b2 a6f6 b2b3 e3d4 b3b5 f6a6 h5g4 a6h6 g4f4",

            "position fen r1bq1rk1/pp1nppbp/2p2np1/3p4/3P4/1P2P3/PBPNQPPP/2KR1BNR w - - 6 8 moves "
            "g2g4 a7a5 g4g5 f6h5 a2a4 b7b5 a4b5 c6b5 e2b5 d7b6 b2c3 c8d7 b5c5 a5a4 c3a5 a8b8 b3a4 "
            "d7a4 f1a6 f7f6 g1f3 f6g5 f3g5 f8f6 a6d3 g7f8 c5a3 e7e5 a3a2 d8e7 d4e5 e7a3 a2a3 f8a3 "
            "c1b1 f6f2 b1a2 a3e7 g5h3 f2f8 d1b1 b8a8 b1b6 e7d8 d2b3 d8b6 a5b6 f8b8 b6c7 a4b3 a2b2 "
            "b8c8 c2b3 c8c7 h3f4 h5f4 e3f4 c7f7 h1f1 a8f8 f4f5 g6f5 b2c3 f5f4 f1f3 f8b8 c3d4 g8g7 "
            "d3f1 f7b7 d4d5 b7b3 f3f4 b8d8 d5e6 d8e8 e6d6 b3e3 f4f5 g7g6 f5f6 g6h5 f6e6 e8d8 d6c6 "
            "d8d2 f1b5 h5g5 h2h4 g5f4 e6d6 d2c2 c6d7 e3e5 b5d3 c2b2 d7c6 h7h5 d6g6 e5e3 d3a6 b2h2 "
            "g6f6 f4g3 c6d5 h2h4 f6h6 h4a4 a6c8 h5h4 h6g6 g3h2 g6c6 h4h3 c6h6 e3c3",

            "position fen rn1qkbnr/pp3ppp/2p1p3/7b/8/2N2NPP/PPPP1PB1/R1BQ1RK1 w kq - 0 9 moves "
            "d2d4 b8d7 c1f4 g8f6 d1d2 f8e7 a2a4 e8g8 f1e1 a7a5 g3g4 h5g6 f3h4 d7b6 b2b3 e7b4 h4g6 "
            "h7g6 d2d3 b6d5 c3d5 f6d5 f4d2 d8f6 g2d5 c6d5 e1e5 b4d2 d3d2 f8c8 a1c1 c8c6 g1g2 a8c8 "
            "c2c3 f6e7 e5e3 e7g5 d2b2 g5f4 e3f3 f4c7 b2d2 c7b8 d2g5 b8d6 g5d2 d6a3 c1b1 b7b6 f3g3 "
            "g6g5 g3f3 a3d6 b1e1 d6c7 e1c1 c7e7 c1e1 g7g6 e1c1 g8g7 c1e1 e7d8 e1c1 f7f6 c1e1 d8d6 "
            "f3e3 d6f4 d2b2 c8e8 b2d2 e8d8 d2e2 d8c8 e3e6 c6c3 e6b6 g7h6 b6b7 c3c2 e2e3 c2a2 b7d7 "
            "a2d2 d7f7 c8b8 e3f4 g5f4 h3h4 f6f5 e1e7 b8h8 e7d7 f5g4 f7f4 g6g5 f4g4 h8f8 g4g5 f8f2 "
            "g2g3 f2g2 g3f4 d2d4 f4e5 g2d2 d7d6 h6h7 g5h5 h7g7 d6a6 d4e4 e5d6 d2d3 a6a5 d3b3 h5g5 "
            "g7h6 g5d5 e4h4 a5a8 h4g4 a4a5 b3c3 a8e8 g4a4 e8e6 h6g7 d6e7 c3f3 d5g5 g7h7",

            // Matetrack2000.epd
            "position fen BK6/4p1N1/1pN1R1n1/3pn3/4k2P/3pPppP/3P1pr1/5bq1 w - -",
            "position fen 6Q1/4p3/N3K1B1/1p3p2/2k2pR1/1bb2nr1/3pp2B/5R2 w - -",
            "position fen 4r2n/3R3B/p2N4/2RP4/1P1k1P2/1Kn2b1r/2PN1P2/4B3 b - -",
            "position fen 3qrrnk/ppnbbppp/2p5/2PpBP2/1P1P3R/P2B2N1/6PP/R2Q2K1 w - -",
            "position fen 2n2K2/2pp4/6p1/2pB4/2pkP2B/5R1P/2PpPp2/3nb3 b - -",
            "position fen 1NbN4/1p1p4/1P1B1p2/P4Pp1/6P1/5B1p/4PP1P/4K1k1 w - -",
            "position fen 2Bn2qN/4N1P1/2n1PP1p/p1p1P3/2P1P1Rp/pP1QBR1K/p1pp2P1/kbrr2b1 b - -",
    };
}

//NOLINTEND(bugprone-suspicious-missing-comma)

std::string getGoCommand(const BenchSearchDepth benchSearchDepth) {
    switch (benchSearchDepth) {
        case BenchSearchDepth::Deep:
            return "go depth 14";
        case BenchSearchDepth::Shallow:
            return "go depth 9";
        case BenchSearchDepth::TimeControl:
            return "go wtime 6000 winc 600 btime 6000 binc 600";
    }
    UNREACHABLE;
}

}  // namespace

std::vector<std::string> getBenchCommands(const BenchSearchDepth benchSearchDepth) {
    const std::vector<std::string> positions = getBenchPositions();

    const std::string goCommand = getGoCommand(benchSearchDepth);

    std::vector<std::string> benchCommands;

    benchCommands.push_back("startbench");
    for (const auto& position : positions) {
        benchCommands.push_back(position);
        benchCommands.push_back("fen");
        benchCommands.push_back(goCommand);
    }
    benchCommands.push_back("stopbench");

    return benchCommands;
}
