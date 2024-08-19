import argparse
import io
import multiprocessing
from pathlib import Path
from typing import TextIO

from chess.pgn import Game, read_game

AnnotatedFen = tuple[float, str]

def is_header(line: str) -> bool:
  return line.startswith("[")

def read_game_from_string(s: str) -> Game:
  stream = io.StringIO(s)
  result = read_game(stream)
  assert result is not None
  return result

def read_games_from_pgn(pgn_path: Path) -> list[Game]:
  games: list[Game] = []
  game_pgn = ""
  new_game_on_header = False

  with open(pgn_path) as f:
    for line in f:
      if is_header(line):
        if new_game_on_header:
          games.append(read_game_from_string(game_pgn))
          game_pgn = ""
          new_game_on_header = False
      else:
        new_game_on_header = True

      game_pgn += line

  games.append(read_game_from_string(game_pgn))

  return games

def filter_games(games: list[Game]) -> list[Game]:
  included_termination_reasons = {
    "adjudication"
  }

  def include(game: Game) -> bool:
    if "Termination" not in game.headers:
      return True
    return game.headers["Termination"] in included_termination_reasons

  return [game for game in games if include(game)]

def get_scores(game: Game) -> tuple[float, float]:
  result = game.headers["Result"]
  white_result, black_result = result.split("-")

  def result_to_score(result: str) -> float:
    if result == "1/2":
      return 0.5
    return int(result)

  return result_to_score(white_result), result_to_score(black_result)

def get_fens(game: Game) -> list[str]:
  fens: list[str] = []
  node = game
  while node is not None:
    fens.append(node.board().fen())
    node = node.next()

  return fens

def get_annotated_fens(
    fens: list[str],
    white_score: float,
    black_score: float,
) -> list[AnnotatedFen]:
  result: list[AnnotatedFen] = []
  curr_score, other_score = white_score, black_score

  for fen in fens:
    result.append((curr_score, fen))
    curr_score, other_score = other_score, curr_score

  return result

def get_annotated_fens_for_game(game: Game) -> list[AnnotatedFen]:
  white_score, black_score = get_scores(game)
  fens = get_fens(game)
  return get_annotated_fens(fens, white_score, black_score)

def write_annotated_fens(
    annotated_fens: list[AnnotatedFen],
    f: TextIO,
) -> None:
  for score, fen in annotated_fens:
    f.write(f"score {score} fen {fen}\n")

def export_annotated_pgns(pgn_paths: list[Path], out_path: Path) -> None:
  merged_games: list[Game] = []
  for i, pgn_path in enumerate(pgn_paths):
    print(f"Reading file {i + 1} / {len(pgn_paths)}: {pgn_path.name}...")

    games = read_games_from_pgn(pgn_path)
    filtered = filter_games(games)

    num_filtered = len(games) - len(filtered)
    print(f"Filtered {num_filtered} games")

    merged_games += filtered

  pool_results = multiprocessing.Pool().imap_unordered(get_annotated_fens_for_game, merged_games, chunksize=10)

  print("Annotating games...")
  annotated_fens: list[AnnotatedFen] = []
  for i, pool_result in enumerate(pool_results):
    num = i+1
    if num % 100 == 0 or num == len(merged_games):
      print(f"Annotated {num} / {len(merged_games)} games")
    annotated_fens += pool_result

  print("Writing annotations...")
  with open(out_path, "w") as f:
    write_annotated_fens(annotated_fens, f)

  print("Done.")

def main() -> None:
  parser = argparse.ArgumentParser()
  parser.add_argument("-p", "--pgns", required=True, nargs="+", type=Path)
  parser.add_argument("-o", "--output", required=True, type=Path)

  args = parser.parse_args()

  export_annotated_pgns(args.pgns, args.output)

if __name__ == "__main__":
  main()
