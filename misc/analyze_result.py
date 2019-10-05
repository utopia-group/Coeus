#!/usr/bin/env python3
import sys
import json
import argparse
from pathlib import Path
from collections import defaultdict


def print_summary(search_results):
    num_files = len(search_results)
    print('Total number of files = {}'.format(num_files))
    if num_files == 0:
        return

    def print_rate(name, count):
        rate = float(count) / num_files
        print('{} count = {} ({:.3g}%)'.format(name, count, rate * 100))
    num_succ = len(list(filter(lambda r: not isinstance(
        r, str), search_results.values())))
    num_fail = len(
        list(filter(lambda r: r == 'fail', search_results.values())))
    num_timeout = len(list(
        filter(lambda r: r == 'timeout', search_results.values())))
    print_rate('Success', num_succ)
    print_rate('Fail', num_fail)
    print_rate('Timeout', num_timeout)


def filter_map(f, l):
    return [x for x in filter(lambda y: y is not None, map(f, l))]


def print_stats(search_results):
    def do_print_stats(name, nums):
        from statistics import mean, median, stdev
        if len(nums) == 0:
            return
        num_max = max(nums)
        num_min = min(nums)
        num_mean = mean(nums)
        num_median = median(nums)
        num_stdev = stdev(nums)
        print((
            '{}:\n'
            '  avg = {:.3g}\n'
            '  max = {:.3g}\n'
            '  min = {:.3g}\n'
            '  median = {:.3g}\n'
            '  stdev = {:.3g}'
        ).format(name, num_mean, num_max, num_min, num_median, num_stdev))

    search_values = list(filter(lambda r: not isinstance(
        r, str), search_results.values()))
    proof_lens = filter_map(lambda r: len(
        r['rules']) if 'rules' in r else None, search_values)
    fails = filter_map(lambda r: r.get('failed_discharges'), search_values)
    timeouts = filter_map(lambda r: r.get('timeout_discharges'), search_values)
    blames = filter_map(lambda r: r.get('num_blames'), search_values)
    blocks = filter_map(lambda r: r.get('blocked_conflicts'), search_values)
    times = filter_map(lambda r: r.get('time_sec'), search_values)
    do_print_stats('Proof length', proof_lens)
    do_print_stats('Running Time', times)
    do_print_stats('Failed attempts', fails)
    do_print_stats('Timeout attempts', timeouts)
    do_print_stats('Blames', blames)
    do_print_stats('Conflict blocking', blocks)


def print_rule_stats(search_results):
    num_rules_total = 0
    rule_counts = defaultdict(int)
    for r in search_results.values():
        if isinstance(r, str):
            continue
        if 'rules' in r:
            for rule in r['rules']:
                rule_counts[rule] += 1
                num_rules_total += 1

    print('Rule statistics:')
    rule_counts = sorted([x for x in rule_counts.items()],
                         key=lambda x: x[1], reverse=True)
    for rule, count in rule_counts:
        rate = float(count) / num_rules_total
        print('  {}: {:.3g}%'.format(rule, rate * 100))


def run(args):
    try:
        with open(str(args.input), 'r') as f:
            search_results = json.load(f)
        print_summary(search_results)
        print_stats(search_results)
        print_rule_stats(search_results)
    except FileNotFoundError as e:
        print(e, file=sys.stderr)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description='Analyze the search result JSON file')
    parser.add_argument(
        'input',
        metavar='FILE',
        type=Path,
        help='JSON file that contains the Coeus search result')
    args = parser.parse_args()
    run(args)
