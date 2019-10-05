#!/usr/bin/env python3

import argparse
import csv
import sys
from pathlib import Path
import matplotlib.pyplot as plt
from matplotlib.ticker import PercentFormatter


def check_args(args):
    if args.weight < 0.0 or args.weight > 1.0:
        raise ValueError(
            f'Weight provided is not in the range of 0 to 1: {args.weight}')


def read_csv(input_file, max_step):
    steps = []
    values = []
    with open(input_file, 'r') as f:
        reader = csv.DictReader(f)
        for row in reader:
            step = int(row['Step'])
            value = float(row['Value'])
            if max_step is not None and step > max_step:
                break
            steps.append(step)
            values.append(value)
    return steps, values


def smooth(scalars, weight):
    last_point = scalars[0]
    result = []
    for point in scalars:
        new_point = last_point * weight + (1 - weight) * point
        result.append(new_point)
        last_point = new_point
    return result


def plot(steps, values, output_file):
    plt.xlabel('Training iterations')
    plt.ylabel('Training rollouts success rate')
    plt.ylim(0, 100)
    plt.gca().yaxis.set_major_formatter(PercentFormatter())

    scaled_values = list(map(lambda x: x * 100, values))
    plt.plot(steps, scaled_values)

    if output_file is None:
        plt.show()
    else:
        plt.savefig(str(output_file), format='eps', dpi=1000)


def run(args):
    plt.rcParams['text.usetex'] = True
    steps, values = read_csv(args.input, args.steps)
    values = smooth(values, args.weight)
    plot(steps, values, args.output)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description='Plot learning curve')
    parser.add_argument(
        'input',
        metavar='INPUT_FILE',
        type=Path,
        help='CSV file that contains the data series')
    parser.add_argument(
        '-s',
        '--steps',
        metavar='STEP',
        type=int,
        help='Maximum training steps to consider.'
             'Default to all steps in the input file')
    parser.add_argument(
        '-w',
        '--weight',
        metavar='WEIGHT',
        type=float,
        default=0.6,
        help='Smoothing factor to use. Default to 0.6')
    parser.add_argument(
        '-o',
        '--output',
        metavar='OUTPUT_FILE',
        type=Path,
        help='Output EPS file to write into. By default plots are rendered on screen')
    args = parser.parse_args()
    try:
        check_args(args)
        print(
            f'Plotting with max step {args.steps} '
            f'and smoothing factor {args.weight}',
            file=sys.stderr)
        if args.output is not None:
            print(f'Output writing to {args.output}',
                  file=sys.stderr)
        run(args)
    except FileNotFoundError as e:
        print(e, file=sys.stderr)
    except ValueError as e:
        print(e, file=sys.stderr)
