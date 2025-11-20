import argparse
import sys
from pathlib import Path


def group_log_paths_in_subfolders(root_dir: Path) -> dict[str, list[Path]]:
    result = {}

    for sub_dir in root_dir.iterdir():
        if not sub_dir.is_dir():
            continue

        for log_file in sub_dir.iterdir():
            if not log_file.is_file():
                continue

            result.setdefault(log_file.name, []).append(log_file)

    return result


def get_node_count_from_log_file(log_file: Path) -> int | None:
    prefix = "Total nodes searched: "

    with open(log_file, "r") as f:
        for line in f:
            if line.startswith(prefix):
                return int(line[len(prefix) :])

    return None


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("directory")
    args = parser.parse_args()

    root_dir = Path(args.directory)

    grouped_log_paths = group_log_paths_in_subfolders(root_dir)

    all_equal = True

    for group, paths in grouped_log_paths.items():
        print(f"{group}:")

        node_counts = []

        for path in paths:
            dir_name = path.parent.name
            node_count = get_node_count_from_log_file(path)
            node_counts.append(node_count)

            print(f"\t{dir_name}: {node_count}")

        s = set(node_counts)
        if len(s) != 1:
            print(
                f"\tERROR: Not all node counts for {group} are equal! Unique values: {list(s)}"
            )
            all_equal = False

    if not all_equal:
        sys.exit(1)

    sys.exit(0)


if __name__ == "__main__":
    main()
