#pragma once

#include "BoardConstants.h"

#include <array>

using EvalCalcT         = float;
using SquareTable       = std::array<EvalCalcT, kSquares>;
using PieceSquareTables = std::array<SquareTable, kNumPieceTypes>;

struct EvalParams {
    std::array<EvalCalcT, kNumPieceTypes> pieceValues = {
            100,  // Pawn
            325,  // Knight
            335,  // Bishop
            500,  // Rook
            975,  // Queen
            0,    // King
    };

    std::array<EvalCalcT, kNumPieceTypes> phaseMaterialValues = {
            0,  // Pawn
            1,  // Knight
            1,  // Bishop
            2,  // Rook
            4,  // Queen
            0,  // King
    };

    // clang-format off
    PieceSquareTables pieceSquareTablesWhiteEarly = {
        // Pawns - stand in front of king and promote
        SquareTable {
             0,  0,  0,  0,  0,  0,  0,  0,
             5, 10, 10,-20,-20, 10, 10,  5,
             5, -5,-10,  0,  0,-10, -5,  5,
             0,  0,  0, 20, 20,  0,  0,  0,
             5,  5, 10, 25, 25, 10,  5,  5,
            10, 10, 20, 30, 30, 20, 10, 10,
            50, 50, 50, 50, 50, 50, 50, 50,
             0,  0,  0,  0,  0,  0,  0,  0,
        },

        // Knights
        SquareTable {
            -50,-40,-30,-30,-30,-30,-40,-50,
            -40,-20,  0,  5,  5,  0,-20,-40,
            -30,  5, 10, 15, 15, 10,  5,-30,
            -30,  0, 15, 20, 20, 15,  0,-30,
            -30,  5, 15, 20, 20, 15,  5,-30,
            -30,  0, 10, 15, 15, 10,  0,-30,
            -40,-20,  0,  0,  0,  0,-20,-40,
            -50,-40,-30,-30,-30,-30,-40,-50,
        },

        // Bishops 
        SquareTable {
            -20,-10,-10,-10,-10,-10,-10,-20,
            -10,  5,  0,  0,  0,  0,  5,-10,
            -10, 10, 10, 10, 10, 10, 10,-10,
            -10,  0, 10, 10, 10, 10,  0,-10,
            -10,  5,  5, 10, 10,  5,  5,-10,
            -10,  0,  5, 10, 10,  5,  0,-10,
            -10,  0,  0,  0,  0,  0,  0,-10,
            -20,-10,-10,-10,-10,-10,-10,-20,
        },

        // Rooks
        SquareTable {
              0,  0,  0,  5,  5,  0,  0,  0,
             -5,  0,  0,  0,  0,  0,  0, -5,
             -5,  0,  0,  0,  0,  0,  0, -5,
             -5,  0,  0,  0,  0,  0,  0, -5,
             -5,  0,  0,  0,  0,  0,  0, -5,
             -5,  0,  0,  0,  0,  0,  0, -5,
              5, 10, 10, 10, 10, 10, 10,  5,
              0,  0,  0,  0,  0,  0,  0,  0,
        },

        // Queens
        SquareTable {
            -20,-10,-10, -5, -5,-10,-10,-20,
            -10,  0,  5,  0,  0,  0,  0,-10,
            -10,  5,  5,  5,  5,  5,  0,-10,
              0,  0,  5,  5,  5,  5,  0, -5,
             -5,  0,  5,  5,  5,  5,  0, -5,
            -10,  0,  5,  5,  5,  5,  0,-10,
            -10,  0,  0,  0,  0,  0,  0,-10,
            -20,-10,-10, -5, -5,-10,-10,-20,
        },

        // King - encourage hiding behind pawns
        SquareTable {
             20, 30, 10,  0,  0, 10, 30, 20,
             20, 20,  0,  0,  0,  0, 20, 20,
            -10,-20,-20,-20,-20,-20,-20,-10,
            -20,-30,-30,-40,-40,-30,-30,-20,
            -30,-40,-40,-50,-50,-40,-40,-30,
            -30,-40,-40,-50,-50,-40,-40,-30,
            -30,-40,-40,-50,-50,-40,-40,-30,
            -30,-40,-40,-50,-50,-40,-40,-30,
        }
    };

    PieceSquareTables pieceSquareTablesWhiteLate = {
        // Pawns - promote
        SquareTable {
             0,  0,  0,  0,  0,  0,  0,  0,
            10, 10, 10, 10, 10, 10, 10, 10,
            10, 10, 10, 10, 10, 10, 10, 10,
            20, 20, 20, 20, 20, 20, 20, 20,
            30, 30, 30, 30, 30, 30, 30, 30,
            50, 50, 50, 50, 50, 50, 50, 50,
            80, 80, 80, 80, 80, 80, 80, 80,
             0,  0,  0,  0,  0,  0,  0,  0,
        },

        // Knights
        SquareTable {
            -50,-40,-30,-30,-30,-30,-40,-50,
            -40,-20,  0,  5,  5,  0,-20,-40,
            -30,  5, 10, 15, 15, 10,  5,-30,
            -30,  0, 15, 20, 20, 15,  0,-30,
            -30,  5, 15, 20, 20, 15,  5,-30,
            -30,  0, 10, 15, 15, 10,  0,-30,
            -40,-20,  0,  0,  0,  0,-20,-40,
            -50,-40,-30,-30,-30,-30,-40,-50,
        },

        // Bishops 
        SquareTable {
            -20,-10,-10,-10,-10,-10,-10,-20,
            -10,  5,  0,  0,  0,  0,  5,-10,
            -10, 10, 10, 10, 10, 10, 10,-10,
            -10,  0, 10, 10, 10, 10,  0,-10,
            -10,  5,  5, 10, 10,  5,  5,-10,
            -10,  0,  5, 10, 10,  5,  0,-10,
            -10,  0,  0,  0,  0,  0,  0,-10,
            -20,-10,-10,-10,-10,-10,-10,-20,
        },

        // Rooks
        SquareTable {
              0,  0,  0,  5,  5,  0,  0,  0,
             -5,  0,  0,  0,  0,  0,  0, -5,
             -5,  0,  0,  0,  0,  0,  0, -5,
             -5,  0,  0,  0,  0,  0,  0, -5,
             -5,  0,  0,  0,  0,  0,  0, -5,
             -5,  0,  0,  0,  0,  0,  0, -5,
              5, 10, 10, 10, 10, 10, 10,  5,
              0,  0,  0,  0,  0,  0,  0,  0,
        },

        // Queens
        SquareTable {
            -20,-10,-10, -5, -5,-10,-10,-20,
            -10,  0,  5,  0,  0,  0,  0,-10,
            -10,  5,  5,  5,  5,  5,  0,-10,
              0,  0,  5,  5,  5,  5,  0, -5,
             -5,  0,  5,  5,  5,  5,  0, -5,
            -10,  0,  5,  5,  5,  5,  0,-10,
            -10,  0,  0,  0,  0,  0,  0,-10,
            -20,-10,-10, -5, -5,-10,-10,-20,
        },

        // King - encourage center
        SquareTable {
            -50,-30,-30,-30,-30,-30,-30,-50,
            -30,-30,  0,  0,  0,  0,-30,-30,
            -30,-10, 20, 30, 30, 20,-10,-30,
            -30,-10, 30, 40, 40, 30,-10,-30,
            -30,-10, 30, 40, 40, 30,-10,-30,
            -30,-10, 20, 30, 30, 20,-10,-30,
            -30,-20,-10,  0,  0,-10,-20,-30,
            -50,-40,-30,-20,-20,-30,-40,-50,
        }
    };
    // clang-format on

    std::array<EvalCalcT, 7> passedPawnBonus = {0, 90, 60, 40, 25, 15, 15};
    EvalCalcT doubledPawnPenalty             = 20;
    EvalCalcT isolatedPawnPenalty            = 30;

    // Penalty for having 0...8 own pawns on the same color as a bishop
    std::array<EvalCalcT, 9> badBishopPenalty = {-40, -30, -20, -10, 0, 10, 20, 30, 40};

    EvalCalcT bishopPairBonus   = 30;
    EvalCalcT knightPairPenalty = 8;
    EvalCalcT rookPairPenalty   = 16;

    EvalCalcT rookSemiOpenFileBonus = 10;
    EvalCalcT rookOpenFileBonus     = 20;

    std::array<EvalCalcT, 9> knightPawnAdjustment = {-20, -16, -12, -8, -4, 0, 4, 8, 12};
    std::array<EvalCalcT, 9> rookPawnAdjustment   = {15, 12, 9, 6, 3, 0, -3, -6, -9};

    EvalCalcT kingVirtualMobilityPenalty = 3;

    std::array<EvalCalcT, kNumPieceTypes> mobilityBonusEarly = {
            0,  // pawns
            1,  // knights
            2,  // bishops
            2,  // rooks
            1,  // queens
            0,  // kings
    };

    std::array<EvalCalcT, kNumPieceTypes> mobilityBonusLate = {
            0,  // pawns
            1,  // knights
            2,  // bishops
            4,  // rooks
            2,  // queens
            0,  // kings
    };
};
