Euwe Chess Engine
=================

**Author: Joost Houben**

Euwe is a UCI chess engine written in modern C++. It is named after
[Max Euwe](https://en.wikipedia.org/wiki/Max_Euwe), a Dutch chess player, mathematician, former
FIDE president, and the fifth World Chess Champion.

You can play against it online on [lichess](https://lichess.org/@/Euwe-chess-engine).

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
 - Piece quality: bishop pair, 'bad bishops', rooks on (semi-)open files.

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
    - Futility pruning
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

## Usage

Apart from playing against it [on lichess](https://lichess.org/@/Euwe-chess-engine), the recommended
way to use Euwe is using a UCI chess GUI. Some good choices include:

 - [Arena](http://www.playwitharena.com/)
 - [Cute Chess](https://cutechess.com/)
 - [Banksia GUI](https://banksiagui.com/)
 - [Tarrasch](http://www.triplehappy.com/)]

Alternatively, you can use Euwe directly from the command line by manually sending UCI commands. See,
e.g., [here](https://gist.github.com/DOBRO/2592c6dad754ba67e6dcaec8c90165bf) for a description of the
interface. To aid with playing, you can enable printing of the board after each move by enabling
debug mode. To do this, send the command `debug on`.

**NOTE:** to minimize resources on startup, Euwe allocates only a small a transposition table on
start-up. This significantly reduces its playing strength. It is strongly recommended to set the
UCI option 'Hash' to an appopriate value. For example, to use a transposition table of size 512 MB,
run the command `setoption name Hash value 512` (or configure this through a chess GUI).

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

## Release history

### Version 1.1

 - Several bugfixes, including issues #1 and #2.
 - Adds bishop pair to evaluation.
 - Adds futility pruning to search.
 - Approximate strength gain: 64.8 +/- 28.3 Elo.

### Version 1.0

 - Initial release.

## Acknowledgements

The author would like to thank:

 - The [Chess Programming Wiki](https://www.chessprogramming.org/Main_Page),
   [TalkChess](http://talkchess.com/), and the many other freely available resources on chess
   programming.
 - [Sebastian Lague](https://www.youtube.com/@SebastianLague) for his amazing Coding Adventure
   series. The [episodes](https://www.youtube.com/watch?v=U4ogK0MIzqk) on chess programming were
   the inspiration for this project.
 - [lichess](https://lichess.org/) for providing an
   [API](https://github.com/lichess-bot-devs/lichess-bot) and platform for chess engines to play
   online.
 - [Cute Chess](https://github.com/cutechess/cutechess) for providing a valuable testing platform.
 - Microsoft for providing valuable free development tools: Visual Studio Community Edition, GitHub,
   and GitHub Copilot.
 - The LLVM development group for providing valuable free and open source development tools: clang,
   clang-format, and clang-tidy.

## License

Euwe is licensed under the Apache License, Version 2.0. See the [LICENSE](LICENSE) file for details.
