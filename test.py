import argparse
import asyncio
import os
from contextlib import asynccontextmanager
from dataclasses import dataclass
from typing import Iterator
from typing import Optional
from typing import Sequence


@dataclass
class ExampleResult:
    example_path: str
    stdout: bytes
    stderr: bytes


currently_running = set()


@asynccontextmanager
async def running_example(example_path: str):
    currently_running.add(example_path)
    try:
        yield
    finally:
        currently_running.remove(example_path)


async def run_file(interpreter_path: str, example_path: str, sem: asyncio.Semaphore) -> None:
    async with sem, running_example(example_path):
        currently_running.add(example_path)
        proc = await asyncio.create_subprocess_exec(
                interpreter_path, example_path,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE)

        stdout, stderr = await proc.communicate()

        return ExampleResult(example_path, stdout, stderr)


def find_files(path: str) -> Iterator[str]:
    abspath = os.path.abspath(path)
    for root, dirs, files in os.walk(abspath):
        for file in files:
            file_path = os.path.join(root, file)
            yield file_path


async def print_running_examples():
    print("")
    while True:
        if len(currently_running) > 5:
            message = f"\033[F\33[2K\rCurrently running {len(currently_running)} examples..."
        else:
            paths = [os.path.relpath(path) for path in currently_running]
            message = f"\033[F\33[2K\rCurrently running tasks {', '.join(paths)}"
        print(message)
        await asyncio.sleep(0.1)


async def main(args: Optional[Sequence[str]] = None) -> int:
    parser = argparse.ArgumentParser(description="Run lox tests.")
    parser.add_argument("interpreter_path", type=str, help="Path to lox interpreter.")
    parser.add_argument("examples_path", type=str, help="Path to examples directory.")
    parser.add_argument("--num_processes", type=int, default=10, help="Number of subprocesses to use.")
    ns = parser.parse_args(args)
    sem = asyncio.Semaphore(ns.num_processes)
    tasks = [run_file(ns.interpreter_path, file, sem) for file in find_files(ns.examples_path)]
    print_task = asyncio.create_task(print_running_examples())
    await asyncio.gather(*tasks)
    print_task.cancel()
    return 0


if __name__ == "__main__":
    raise SystemExit(asyncio.run(main()))
