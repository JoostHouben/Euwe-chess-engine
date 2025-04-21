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

def get_fens(game: Game) -> tuple[list[str], int]:
  fens: list[str] = []
  num_filtered = 0
  node = game
  while node is not None:
    include_node = True

    if "book" in node.comment:
      include_node = False
    elif "/" in node.comment:
      score_comment = node.comment.split('/')[0]
      if 'M' in score_comment:
        include_node = False
      else:
        try:
          score = float(score_comment)
          if abs(score) > 20:
            include_node = False
        except:
          pass

    if include_node:
      fens.append(node.board().fen())
    else:
      num_filtered += 1

    node = node.next()

  return fens, num_filtered

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

def get_annotated_fens_for_game(game: Game) -> tuple[list[AnnotatedFen], int]:
  white_score, black_score = get_scores(game)
  fens, num_filtered = get_fens(game)
  return get_annotated_fens(fens, white_score, black_score), num_filtered

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

    num_games_filtered = len(games) - len(filtered)
    print(f"Filtered {num_games_filtered} games")

    merged_games += filtered

  pool_results = multiprocessing.Pool().imap_unordered(get_annotated_fens_for_game, merged_games, chunksize=10)

  print("Annotating games...")
  annotated_fens: list[AnnotatedFen] = []
  total_num_nodes_filtered = 0
  for i, pool_result in enumerate(pool_results):
    num = i+1
    if num % 100 == 0 or num == len(merged_games):
      print(f"Annotated {num} / {len(merged_games)} games")

    fens, num_nodes_filtered = pool_result

    annotated_fens += fens
    total_num_nodes_filtered += num_nodes_filtered

  print(f"Filtered {total_num_nodes_filtered} nodes.")

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
