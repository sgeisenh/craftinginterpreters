import argparse
from typing import Sequence

def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("input")
    parser.add_argument("output")
    args = parser.parse_args()
    with open(args.input, "r") as js_files, open(args.output, "w") as out:
        for filename in js_files:
            with open(filename.strip(), "r") as js_file:
                out.write(js_file.read())

    return 0

if __name__ == "__main__":
    raise SystemExit(main())
