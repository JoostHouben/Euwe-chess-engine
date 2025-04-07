Euwe Chess Engine
=================

[![Tests][tests-badge]][tests-link]

**Author: Joost Houben**

Euwe is a UCI chess engine written in modern C++. It is named after
[Max Euwe](https://en.wikipedia.org/wiki/Max_Euwe), a Dutch chess player, mathematician, former
FIDE president, and the fifth World Chess Champion.

You can play against it online on [lichess](https://lichess.org/@/Euwe-chess-engine).

## Downloads

The latest stable release can be downloaded from the
[releases page](https://github.com/JoostHouben/Euwe-chess-engine/releases/latest).

You can also download pre-release builds from the CI artifacts
[here](https://github.com/JoostHouben/Euwe-chess-engine/actions?query=is%3Asuccess+event%3Apush+branch%3Amain+workflow%3A%22Archive+Engine+Binaries%22).

If you prefer to build the engine yourself, see the build instructions [below](#Build-instructions).

## Hardware requirements

Euwe uses BMI2 instructions for bitboard manipulation. This means that it requires a CPU that
supports these instructions. This includes Intel CPUs since about 2013 (Haswell) and AMD CPUs since
about 2017 (Zen). Though note that AMD CPUs prior to Zen 3 (2020) may experience performance issues.

## Options

Euwe supports the following UCI options:

 - `Hash`: The size of the transposition table in MB. Default is 16 MB.
 - `move_overhead_ms`: The overhead in milliseconds for each move. This is subtracted from the time
   budget for each move. Default is 20 ms. Some GUIs or match managers may have additional overhead
   that requires increasing this value. If  you experience timeouts, try increasing this value.
 - `SyzygyPath`: Path(s) to the Syzygy tablebases. On Windows, this must be a semicolon-separated
   list of directories. On Linux, this must be a colon-separated list of directories. The files in
   each directory will be searched for TB files, but subdirectories will not be searched. The
   default is an empty string, which disables TB probing. In order for DTZ tables to be used (for
   root move filtering), WDL tables must also be present. It is not possible to provide only DTZ
   tables.
 - `SyzygyProbeDepth`: Minimum depth to probe the WDL tablebases at a node in the search. The
   default is 1, meaning that the TBs are probed at all non-quiescence nodes. Increasing this value
   will reduce the number of TB probes. This can improve performance, but will reduce the accuracy
   of the search. If the TBs are stored on an HDD, it is recommended to set this value to 100 to
   completely disable probing during search. Note that regardless of the probe depth setting, the
   engine will still use DTZ tables for root move filtering (if available).

## Features

### Board representation

Euwe uses a bitboard representation. For move generation it uses
[PEXT Bitboards](https://www.chessprogramming.org/BMI2#PEXT_Bitboards).

### Evaluation

Euwe uses a Hand-Crafted Evaluation (HCE) function with automatically tuned parameters. The tuning
approach is based on [Texel's tuning method](https://www.chessprogramming.org/Texel%27s_Tuning_Method).
The tuner code is included (see the `tuner` directory) and uses [Ceres](http://ceres-solver.org/)
for solving the optimization problem. The parameters (see `EvalParams.cpp`) were tuned purely based
on self-play (using Euwe v1.3.0 as a starting point, which contained hand-tuned parameters).

Features considered by the evaluation function include:

 - Tapered evaluation.
 - Material and position using Piece Square Tables.
     - Piece square tables are (partially) reflected depending on the positions of the kings.
     - A separate piece square table is used for passed pawns.
 - Pawn structure:
     - Isolated pawns.
     - Doubled pawns.
     - Passed pawns.
     - Connected passed pawns.
     - Rule of the square.
     - Unstoppable passed pawn.
 - King safety:
     - Open files near the king.
     - King tropism.
         - Calculated between king and own pieces and king and enemy pieces.
         - Calculated for each piece type, and for different pawn types (isolated, doubled, passed).
     - Virtual king mobility.
     - Enemy controlled squares near the king.
     - Number of pieces attacking the king.
     - Attack weight for different piece types.
 - Piece quality:
     - Piece mobility.
     - Value adjustment based on number of own pawns.
         - For bishop: instead consider own and enemy pawns on the same square color.
     - Bishop / knight / rook pair.
     - Rook on (semi-)open file.
     - Piece defended / attacked.
     - Piece pinned.
 - Tempo bonus
 - Scaling for drawish end-games:
     - Opposite colored bishop end games, with scaling dependent on the difference in the number of
       pawns on the board.
     - Scale down the eval if the stronger side has:
         - A single minor piece.
         - Two knights.
         - A rook versus a minor.
         - A rook and a minor vs a rook.

Additionally, Euwe caches the evaluation of the pawn-king structure. Since this tends to be fairly
static, this provides a speed-up during search.

### Search function

Euwe uses [principal variation search](https://www.chessprogramming.org/Principal_Variation_Search)
with the following enhancements and heuristics.

 - Iterative deepening
 - Transposition table
    - Two-tier system: depth-preferred and always-replace
    - Aging: depth preference is reduced by the age of the entry
 - Quiescence search
 - Aspiration windows
 - Move ordering:
    - Hash move
    - Capture ordering based on MVV, capture history, and Static Exchange Evaluation (SEE)
    - Killer moves
    - Counter moves
    - History heuristic
    - Threats by less valuable pieces
 - Pruning:
    - Null move pruning
    - Delta pruning
    - Futility pruning
    - Late move pruning: the futility margin is reduced based on the move index (causing later moves
      to be pruned more aggressively), and once enough quiet moves are pruned, all remaining quiet
      moves are skipped.
    - Reverse futility pruning
 - Extensions and reductions:
    - Late move reductions
    - Check extensions
    - 7th rank extensions: extend pawn pushes to rank 2 and 7

### Time management

Euwe uses a basic time management system that tries to distribute the available time equally over
each move. It uses a 'soft' and 'hard' time limit. The 'soft' time limit is checked each time the
depth is increased, and the 'hard' time limit will interrupt the search.

### Unsupported UCI features

Currently the following UCI features are not supported:

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
 - [Tarrasch](http://www.triplehappy.com/)

Alternatively, you can use Euwe directly from the command line by manually sending UCI commands. See,
e.g., [here](https://gist.github.com/DOBRO/2592c6dad754ba67e6dcaec8c90165bf) for a description of the
interface. To aid with playing, you can enable printing of the board after each move by enabling
debug mode. To do this, send the command `debug on`.

For statistical tests, consider using [Fastchess](https://github.com/Disservin/fastchess).

**NOTE:** to minimize resources on startup, Euwe allocates only a small transposition table on
start-up. This significantly reduces its playing strength. It is strongly recommended to set the
UCI option 'Hash' to an appopriate value. For example, to use a transposition table of size 512 MB,
run the command `setoption name Hash value 512` (or configure this through a chess GUI).

## Technical details

Euwe targets the C++23 standard. The engine itself is designed to have minimal external
dependencies. Currently, the only external dependencies are system APIs (Windows/POSIX) for colored
console output.

The repository also contains helper projects with additional dependencies:

 - The unit tests depend on [GTest](https://github.com/google/googletest).
 - The tuner depends on [Ceres](http://ceres-solver.org/) and its transitive dependencies. Ceres has
   also been configured to use [SuiteSparse](https://github.com/DrTimothyAldenDavis/SuiteSparse).
 - A script for extracting annotated fens from PGNs depends on Python and
   [python-chess](https://python-chess.readthedocs.io/en/latest/).

These dependencies are managed using [vcpkg](https://vcpkg.io/en/) (for C++) and pip (for Python).

### Build instructions

#### Windows

 - Open the root directory of the repository in Visual Studio 2022. Make sure that you have the 
   'Desktop development with C++' workload installed, including at least 'C++ CMake tools for
   Windows' and 'vcpkg package manager'. If you want to build with clang-cl (for best performance),
   you'll also need the 'C++ clang compiler for windows' component.
 - Build the solution. For best playing strength, build using the mode 'Windows clang x64 Release'.

The project has been tested with Visual Studio 2022 Community Edition, version 17.13 (`_MSC_VER`
1943).

The above instructions will compile not only the engine, but also the tests and the tuner. This
requires installing dependencies for the tests and the tuner which may take a while. If you only
want to build the engine, modify `TARGETS_TO_BUILD` in the CMakePresets.json file to just
`"engine"`.

#### Linux

 - [Set up vcpkg](https://learn.microsoft.com/en-us/vcpkg/get_started/get-started?pivots=shell-cmd)
   and make sure that the `VCPKG_ROOT` environment variable is set (or modify the CMakePresets.json
   file to point to your VCPKG installation directly).
 - If needed, modify CMakePresets.json to match your build environment.
 - Run `cmake` with appropriate options. E.g., run
   `cmake --preset linux-clang-release -DTARGETS_TO_BUILD="engine"` to build in release mode.
 - Run `cmake --build` on the output directory. E.g., `cmake --build out/build/linux-clang-release/`.

The project is tested in CI with clang 19, but should also work with clang 20 and gcc 14.

The above instructions will only compile the engine. To also compile the tests and/or the tuner, add
the appropriate target to the `TARGETS_TO_BUILD` option. So to configure for building all 3 targets,
run: `cmake --preset linux-clang-release -DTARGETS_TO_BUILD="engine;tests;tuner"`. This will trigger
installation of the required dependencies using vcpkg.

If `TARGETS_TO_BUILD` is not set, the default is to build all targets.

Note that in order to compile the tuner, you will need a Fortran compiler, such as GFortran; see
[installation instructions](https://fortran-lang.org/en/learn/os_setup/install_gfortran/). This is
because the Tuner uses Ceres configured with SuiteSparse, which in turn depends on LAPACK, which is
written in Fortran.

## Release history

### Version 2.1

Version 2.1 has a significantly improved search function, and now supports Syzygy tablebases.

Approximate strength gain (measured in self-play against v2.0 using an unbalanced opening book):

 - On ultra fast time control, with 6-men Syzygy TBs (3"+0.1"): 288.1 +/- 15.6 Elo
 - On ultra fast time control, without Syzygy TBs (3"+0.1"): 279.5 +/- 15.4 Elo
 - On CCRL Blitz time control, with 6-men Syzygy TBs (2'+1"): 290.7 +/- 28.0 Elo

Note that providing Syzygy TBs provides a strength gain of about 20 Elo (measured in self-play on
ultra fast time control). The difference in strength gain compared to v2.0 is less than that because
v2.1 is so much stronger that the game is typically already decided before the TBs come into play.

Also note that the strength gain from TBs depends on the hardware used to run the engine. In
particular, when the TBs are stored on an HDD it is recommended to disable probing within the search
by setting `SyzygyProbeDepth` to 100. The engine will then only use the TBs for filtering moves when
it has already reached a TB position. This results in a reduced strength gain compared to probing
within the search on an SSD.

Changes to the UCI interface:

 - Added support for the `go searchmoves` subcommand. This allows the user to restrict the moves to
   be searched to a subset of the legal moves.
 - The engine now provides `lowerbound` and `upperbound` information when a search fails the
   aspiration window.
 - The order of information within the `info` output was changed.
 - Added the `SyzygyPath` and `SyzygProbeDepth` options to configure the use of Syzygy tablebases.
 - The `move_overhead_ms` default value was reduced to 20 (from 100).
 - The maximum length of the provided PV is now the greater of depth and seldepth.
 - The PV will now terminate upon the first repetition and upon reaching a draw by the 50-move rule.
   Previously, the PV could continue beyond this point, resulting in PVs containing moves beyond the
   end of the game.
 - When starting a search from a position for which the transposition table already has an exact
   score at a certain depth, the engine will now immediately report this depth, instead of starting
   from depth 1 and very rapidly increasing it.
 - Some debug prints (to stderr) were changed to UCI information (`info string`) prints. Some other
   debug prints were removed.

Bugfixes:

 - The `nodes` and `nps` statistics are now correctly reported. Previously, nodes on the transition
   between regular search and quiescence search would be counted double, resulting in inflated node
   and nps counts.
 - The `quit` command now interrupts the search. Previously, the engine would hang if `quit` was
   sent during a search without first sending `stop`.
 - The `stop` command should now be more reliable. Previously, if `stop` was sent immediately after
   `go`, the engine would sometimes not stop the search.
 - Fix an infinite loop (hang) that would occur if `stdin` is unexpectedly closed.

New or improved features:

 - Changes to search:
    - Improved move ordering:
       - Use history with gravity instead of relative history.
       - Use MVV + capture history instead of MVV/LVA for ordering captures.
       - Use Static Exchange Evaluation (SEE) for detecting losing captures, and search those later.
       - Give bonuses and maluses for moves that escape or enter a capture threat to the moved piece
         by a less valuable piece.
    - Increased selectvitity:
       - Added reverse futility pruning.
       - Allow futility pruning to happen up to depth 5 (up from 3).
       - Reduce the futility margin based on the move index, meaning that more aggressive futility
         pruning is applied to later moves. This is essentially a hybrid between futility pruning
         and late move pruning.
       - Allow losing captures (based on SEE) to be subject to futility pruning.
       - Allow losing captures to be subject to late move reductions.
       - Use SEE on both quiet moves and losing captures to determine if they lose sufficient
         material to be subject to futility pruning. This causes more moves to be pruned.
       - If enough quiet moves are pruned using futility pruning, skip all remaining quiet moves.
       - Scale the depth reduction in late move reductions based on depth and the move index.
    - Use Syzygy tablebases:
       - Probe WDL tables in the search. This makes search in late-game positions more accurate.
       - When the current position is a TB position, the engine will only consider moves that retain
         the same TB verdict. This guarantees perfect play in TB positions. If there is more than
         one such move, the engine will still perform a search to find the move that seems best
         based on normal search/evaluation. This allows finding a faster / more natural mate path in
         won positions, and attempts to create a tricky position in which the opponent might make a
         mistake in drawn and lost positions.
 - Changes to time management:
    - The engine now uses a 'soft' and 'hard' time limit. The 'soft' time limit is checked each time
      the depth is increased, and the 'hard' time limit will interrupt the search.
 - Changes to evaluation:
    - The evaluation function now caches the evaluation of the pawn-king structure, providing a
      speed-up.

Technical changes:

 - Build configuration was moved from CMakeUserPresets.json to CMakePresets.json.
 - Build configurations were added for clang-cl (on Windows) and clang (on Linux).
 - The binaries provided for download are now built with clang-cl on Windows and clang on Linux,
   compared to MSVC on Windows and g++ on Linux in previous versions. This has resulted in improved
   performance on both platforms.

### Version 2.0

Version 2.0 contains a major overhaul of the evaluation function, which now includes many more
features and uses automatically tuned parameters.

Approximate strength gain:
 - On ultra fast time control (3"+0.1"): 123.0 +/- 9.3 Elo.
 - On CCRL Blitz time control (2'+1"): 174.6 +/- 30.9 Elo.

### Version 1.3

Version 1.3 adds Linux build support.

It also includes several minor improvements:

 - Some bugfixes to draw detection code. This fixes issues where Euwe blindly walks into a draw
   from a winning position.
 - Detect drawish material configurations in the evaluation function.
 - Use the transposition table in quiescence search.
 - The selective depth calculation has been modified to match the Stockfish convention (depth of the
   deepest PV line, including nodes in quiescence search).
 - Fixes to reported statistics:
   - Use rounding to nearest permille for reported hashfull (instead of rounding down).
   - Reset selective depth after each full search to a given depth, instead of keeping a running
     maximum for each 'go' command.
 - Approximate strength gain (on ultra fast time control): 27.9 +/- 7.8 Elo.

### Version 1.2

 - Bugfixes:
   - Fixes to bugs in pawn structure evaluation that caused isolated and passed pawn detection to work incorrectly.
   - Fixed bug in bad bishop evaluation that unintentionally caused bishop values to increase as pawns disappeared.
 - New evaluation features:
   - King tropism
   - Virtual king mobility
   - Mobility
   - Adjust values of knights and rooks based on number of own pawns on the board
 - Add transposition table aging
 - Approximate strength gain (on ultra fast time control): 109.1 +/- 38.2 Elo.

### Version 1.1

 - Several bugfixes, including issues #1 and #2.
 - Adds bishop pair to evaluation.
 - Adds futility pruning to search.
 - Approximate strength gain (on ultra fast time control): 64.8 +/- 28.3 Elo.

### Version 1.0

 - Initial release.

## Acknowledgements

The author would like to thank:

 - The folks running [CCRL](https://computerchess.org.uk/ccrl/4040/), including Graham Banks and
   Gabor Szots, for providing a valuable platform for testing and comparing chess engines, as well
   as for gathering a fun community of chess engine enthusiasts.
 - The [Chess Programming Wiki](https://www.chessprogramming.org/Main_Page),
   [TalkChess](http://talkchess.com/), and the many other freely available resources on chess
   programming.
 - [Sebastian Lague](https://www.youtube.com/@SebastianLague) for his amazing Coding Adventure
   series. The [episodes](https://www.youtube.com/watch?v=U4ogK0MIzqk) on chess programming were
   the inspiration for this project.
 - [lichess](https://lichess.org/) for providing an
   [API](https://github.com/lichess-bot-devs/lichess-bot) and platform for chess engines to play
   online.
 - [Fastchess](https://github.com/Disservin/fastchess) and
   [Cute Chess](https://github.com/cutechess/cutechess) for providing valuable testing tools.
 - Stefan Pohl for providing unbalanced opening books for testing. See his website
   [here](https://www.sp-cc.de/anti-draw-openings.htm) and [here](https://www.sp-cc.de/uho_2024.htm).
 - Microsoft for providing valuable free development tools: Visual Studio Community Edition, GitHub,
   and GitHub Copilot.
 - The LLVM development group for providing valuable free and open source development tools: clang,
   clang-format, and clang-tidy.
 - The authors and maintainers of the open source libraries used in this project, including
   [Ceres](http://ceres-solver.org/),
   [SuiteSparse](https://github.com/DrTimothyAldenDavis/SuiteSparse),
   [GTest](https://github.com/google/googletest), and
   [python-chess](https://python-chess.readthedocs.io/en/latest/).
 - Ronald de Man (aka 'Syzygy') for providing the code for Syzygy generation and probing, and 
   [basil00](https://github.com/basil00), [John Dart](https://github.com/jdart1) and
   [Andrew Grant](https://github.com/AndyGrant) for further developing the probing code in the form
   of [Fathom](https://github.com/jdart1/Fathom) and [Pyrrhic](https://github.com/AndyGrant/Pyrrhic).
 - The [Stockfish](https://github.com/official-stockfish/Stockfish) team for providing a valuable
   reference implementation of a chess engine.

## License

Euwe is licensed under the Apache License, Version 2.0. See the [LICENSE](LICENSE) file for details.

Euwe utilizes [Pyrrhic](https://github.com/AndyGrant/Pyrrhic) for Syzygy tablebase probing. Pyrrhic
is licensed under the MIT license, see the [Pyrrhic license](chess-engine-lib/Pyrrhic/LICENSE) for
details.

[tests-badge]: https://github.com/JoostHouben/Euwe-chess-engine/actions/workflows/run-tests.yml/badge.svg?branch=main
[tests-link]: https://github.com/JoostHouben/Euwe-chess-engine/actions/workflows/run-tests.yml
