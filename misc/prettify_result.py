#!/usr/bin/env python3
import sys
import json
import argparse
from pathlib import Path


def filter_results(search_results, keep_files):
    def should_keep(s):
        return any(s.endswith(keep_file) for keep_file in keep_files)
    return {k: v for k, v in search_results.items() if should_keep(k)}


def select_results(search_results, select):
    def should_keep(s):
        if type(s) is str and select == s:
            return True
        if type(s) is dict and select == 'succ':
            return True
        return False
    return {k: v for k, v in search_results.items() if should_keep(v)}


def run(args):
    try:
        with open(str(args.input), 'r') as f:
            search_results = json.load(f)

        if args.filter is not None:
            keep_files = []
            with open(str(args.filter), 'r') as f:
                for line in f.readlines():
                    keep_files.append(line.strip())
            search_results = filter_results(search_results, keep_files)

        if args.select is not None:
            search_results = select_results(search_results, args.select)

        if args.output is None:
            print(json.dumps(search_results, indent=2))
        else:
            with open(str(args.output), 'w') as f:
                print(json.dumps(search_results, indent=2), file=f)
    except FileNotFoundError as e:
        print(e, file=sys.stderr)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description='Prettify the search result JSON file')
    parser.add_argument(
        'input',
        metavar='INPUT_FILE',
        type=Path,
        help='JSON file that contains the Coeus search result')
    parser.add_argument(
        '-f',
        '--filter',
        metavar='FILTER_FILE',
        type=Path,
        help='A text file that contains which files need to be kept'
    )
    parser.add_argument(
        '-s',
        '--select',
        metavar='KIND',
        choices=['succ', 'fail', 'timeout'],
        help='Only show results with the given status'
    )
    parser.add_argument(
        '-o',
        '--output',
        metavar='OUTPUT_FILE',
        type=Path,
        help='Specifiy where the output should be saved. Absent means stdout.'
    )
    args = parser.parse_args()
    run(args)
