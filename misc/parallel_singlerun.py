#!/usr/bin/env python3

import os
import sys
import subprocess
import argparse
import signal
import json
from pathlib import Path
from multiprocessing import Pool

args = None


def run_input(input_file):
    print('Processing {}'.format(input_file), file=sys.stderr)

    # Other params can be passed via environment variables
    cmd = '{} run -j --proof {} {}'.format(args.exec, args.proof, input_file)

    with subprocess.Popen(cmd.split(),
                          stdout=subprocess.PIPE,
                          stderr=subprocess.DEVNULL,
                          preexec_fn=os.setpgrp,
                          universal_newlines=True) as process:
        try:
            outs, err = process.communicate(timeout=args.timeout)
            if process.returncode == 0:
                result_json = json.loads(outs)
                if 'rules' in result_json:
                    print('...succeeded with rules {}'.format(
                        ';'.join(result_json['rules'])), file=sys.stderr)
                else:
                    print('...succeeded')
                return result_json
            else:
                print('...failed', file=sys.stderr)
                return 'fail'
        except subprocess.TimeoutExpired:
            # send signal to the process group
            os.killpg(os.getpgid(process.pid), signal.SIGKILL)
            print('...timeout', file=sys.stderr)
            return 'timeout'
        except json.decoder.JSONDecodeError:
            print('...failed', file=sys.stderr)
            return 'fail'


def run(args):
    if args.input.is_file():
        if args.input.suffix == '.coeus':
            input_files = [args.input]
        else:
            input_files = []
    elif args.input.is_dir():
        input_files = [x for x in args.input.iterdir(
        ) if x.is_file() and x.suffix == '.coeus']
    else:
        input_files = []

    with Pool(processes=args.threads) as pool:
        res = pool.map(run_input, input_files)

    succ_files = []
    fail_files = []
    for input_file, r in zip(input_files, res):
        if r == 'fail':
            fail_files.append(str(input_file))
        elif r == 'timeout':
            pass
        else:
            succ_files.append(str(input_file))

    succ_count = len([x for x in res if not isinstance(x, str)])
    total_count = len(res)
    if total_count > 0:
        succ_rate = succ_count / total_count
    else:
        succ_rate = 0.0
    print('Proved: {} / {} ({:.2f}%)'.format(succ_count,
                                             total_count,
                                             succ_rate), file=sys.stderr)

    if args.output is not None:
        results = {str(x.resolve()): y for x, y in zip(input_files, res)}
        with open(str(args.output), 'w') as f:
            print(json.dumps(results), file=f)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description=('Runing proof search on a file or directory of files '
                     'and measure proof success rate'))
    parser.add_argument('input', metavar='FILE',
                        type=Path, help='input Coeus file')
    parser.add_argument('-e', '--exec', metavar='DRIVER', type=Path,
                        help='Where to find the Coeus driver', required=True)
    parser.add_argument('-t', '--timeout', metavar='TIME', type=int,
                        help='Timeout for each run (in sec). Default to 600.',
                        default=600)
    parser.add_argument('--threads', metavar='NUM',
                        type=int,
                        help='Number of parallel jobs. Default to 1',
                        default=1)
    parser.add_argument('-o', '--output', metavar='FILE',
                        type=Path,
                        help=('Specify a file in which all testing results '
                              'can be stored'))
    parser.add_argument(
        '-p',
        '--proof',
        metavar='STRATEGY',
        type=str,
        help='Proof strategy to use. Default to \'random\'', default='random')
    args = parser.parse_args()

    run(args)
