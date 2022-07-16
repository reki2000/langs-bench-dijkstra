#!/usr/bin/python

"""This program shows `hyperfine` benchmark results as a box and whisker plot.

Quoting from the matplotlib documentation:
    The box extends from the lower to upper quartile values of the data, with
    a line at the median. The whiskers extend from the box to show the range
    of the data. Flier points are those past the end of the whiskers.
"""

import argparse
import json
import matplotlib.pyplot as plt

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("file", help="JSON file with benchmark results")
parser.add_argument("--title", help="Plot Title")
parser.add_argument("--savefile", help="File name of output image")
args = parser.parse_args()

with open(args.file) as f:
    results = json.load(f)["results"]

labels = [b["parameters"]["lang"] for b in results]
times = [b["times"] for b in results]

boxplot = plt.boxplot(times, labels=labels, vert=True, patch_artist=True)

plt.title(args.title)
plt.ylabel("Time [s]")
plt.ylim(0, None)
plt.savefig(args.savefile)
