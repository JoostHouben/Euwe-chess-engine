Euwe Chess Engine
=================

**Author: Joost Houben**

Euwe is a UCI chess engine written in modern C++. It is named after
[Max Euwe](https://en.wikipedia.org/wiki/Max_Euwe), a Dutch chess player, Mathematician, 5th world
Chess Champion, and former FIDE president.

## Hardware requirements

Euwe uses BMI2 instructions for bitboard manipulation. This means that it requires a CPU that
supports these instructions. This includes Intel CPUs since about 2013 (Haswell) and AMD CPUs since
about 2017 (Zen). Though note that AMD CPUs prior to Zen 3 (2020) may experience performance issues.

## Features

### Board representation

Euwe uses a bitboard representation. For move generation it uses
[PEXT Bitboards](https://www.chessprogramming.org/BMI2#PEXT_Bitboards).

### Evaluation

Euwe uses a very simple evaluation function that takes into account the following factors:

 - Material balance.
 - Piece square tables.
 - Tapered evaluation.
 - Pawn structure: doubled, isolated, and passed pawns.
 - Piece quality: 'bad bishops', rooks on (semi-)open files.

### Search function

Euwe uses [principal variation search](https://www.chessprogramming.org/Principal_Variation_Search)
with the following enhancements and heuristics.

 - Iterative deepening
 - Transposition table
    - Two-tier system: depth-preferred and always-replace
 - Quiescence search
 - Aspiration windows
 - Move ordering:
    - Hash move
    - Capture ordering based on MVV/LVA
    - Killer moves
    - Counter moves
    - History heuristic
 - Pruning:
    - Null move pruning
    - Delta pruning
 - Selectivity:
    - Late move reduction
    - Check extensions

### Time management

Euwe uses a primitive time management system that tries to distribute the available time equally over
each move.

### Unsupported UCI features

Currently the following UCI features are not supported:

 - Restricting the moves to be searched to a subset of the legal moves (`go searchmoves`).
 - Pondering (`go ponder` and `ponderhit`).
 - Searching for a mate in a given number of moves (`go mate`).
 - All of the standardized UCI options except for `Hash`. Specifically, the following options are not
   supported: `NalimovPath`, `NalimovCache`, `Ponder`, `OwnBook`, `MultiPv`, `UCI_ShowCurrLine`,
   `UCI_ShowRefutations`, `UCI_LimitStrength`, `UCI_Elo`, `UCI_AnalyseMode`, `UCI_Opponent`,
   `UCI_EngineAbout`, `UCI_ShredderrbasesPath`, `UCI_SetPositionValue`.

Other than this the full UCI protocol is supported.

## Technical details

Euwe targets the C++23 standard. It is designed to have minimal external dependencies. Currently, the
only external dependencies are:

 - GTest for unit testing.
 - The Windows API for colored console output.

It was built and developed on Windows using Visual Studio 2022 Community Edition. Care was taken to
ensure that the code is portable and can be built on other platforms. However, this has not been
tested yet.

### Build instructions

Open [Euwe.sln](Euwe.sln) in Visual Studio 2022 and build the solution.

## License

Euwe is licensed under the Apache License, Version 2.0. See the [LICENSE](LICENSE) file for details.
