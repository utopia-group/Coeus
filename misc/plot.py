#!/usr/bin/env python3
import matplotlib.pyplot as plt
from matplotlib.ticker import PercentFormatter
import json
import argparse
import sys
from pathlib import Path

COLORS = [
    '#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd',
    '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22', '#17becf'
]
MARKERS = ['o', 's', '^', 'x', '+', 'd', '|', '*', 'p', '>']


def compute_plot_data(results):
    succ_times = []
    for res in results.values():
        if type(res) is dict and 'time_sec' in res:
            tsec = res['time_sec']
            succ_times.append(tsec)
    sorted_times = sorted(succ_times)
    count_times = [(sorted_times[i], i) for i in range(len(sorted_times))]
    if len(count_times) < len(results):
        count_times.append((300.0, len(count_times)))
    return count_times


def read_json(file):
    with open(str(file), 'r') as f:
        search_results = json.load(f)
    return search_results


def do_plot(data, total, output_file):
    plt.xlabel('Time Limit (sec)')
    plt.ylabel('Percentage of solved problems')
    plt.ylim(0, 100)
    plt.gca().yaxis.set_major_formatter(PercentFormatter())

    legend_lines = []
    for i, (name, values) in enumerate(data):
        xvalues, yvalues = (list(x) for x in zip(*values))
        yvalues = [x / total * 100 for x in yvalues]
        line, = plt.plot(
            xvalues, yvalues,
            color=COLORS[i], marker=MARKERS[i],
            label=name)
        legend_lines.append(line)
    plt.legend(handles=legend_lines)

    plt.tight_layout()
    if output_file is None:
        plt.show()
    else:
        plt.savefig(str(output_file), format='eps', dpi=1000)


def check_args(args):
    if len(args.inputs) > len(COLORS) or len(args.inputs) > len(MARKERS):
        raise ValueError('Not enough plot style is provided')
    if (args.labels is not None and
        len(args.labels) > 0 and
            len(args.labels) != len(args.inputs)):
        raise ValueError('File count and label count do not match')


def get_num_benchs(jsons):
    jlens = list(map(lambda x: len(x), jsons))
    if len(set(jlens)) != 1:
        raise ValueError('Number of benchmarks is not consistent')
    return jlens[0]


def run(args):
    plt.rcParams['text.usetex'] = True
    jsons = list(map(lambda x: read_json(x), args.inputs))
    num_benchs = get_num_benchs(jsons)

    plot_data = list(map(lambda x: compute_plot_data(x), jsons))
    if args.labels is not None:
        named_plot_data = list(zip(args.labels, plot_data))
    else:
        named_plot_data = list(zip([str(i)
                                    for i in range(len(plot_data))],
                                   plot_data))
    do_plot(named_plot_data, num_benchs, args.output)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description='Plot the result JSON file')
    parser.add_argument(
        'inputs',
        metavar='INPUT_FILE',
        nargs='+',
        type=Path,
        help='JSON file that contains the Coeus search result')
    parser.add_argument(
        '-l',
        '--labels',
        metavar='LABEL',
        type=str,
        nargs='*',
        help='Specify labels for each plotted data')
    parser.add_argument(
        '-o',
        '--output',
        metavar='OUTPUT_FILE',
        type=Path,
        help='Output EPS file to write into. By default plots are rendered on screen')
    args = parser.parse_args()
    try:
        check_args(args)
        run(args)
    except FileNotFoundError as e:
        print(e, file=sys.stderr)
    except ValueError as e:
        print(e, file=sys.stderr)
