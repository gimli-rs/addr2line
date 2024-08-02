#!/usr/bin/env python

import argparse
import glob
import json
import matplotlib.pyplot as plt
import sys

parser = argparse.ArgumentParser()
parser.add_argument(
    "-o", "--output", help="Save image to the given filename."
)

args = parser.parse_args()

commands = None
data = []
inputs = []

for filename in glob.glob("scripts/tmp/benchmark-*.json"):
    with open(filename) as f:
        results = json.load(f)["results"]
    commands = [b["command"] for b in results]
    data.append([b["mean"] for b in results])
    inputs.append(results[0]["parameters"]["input"])

data = map(list, zip(*data))
for (times, command) in zip(data, commands):
    plt.plot(inputs, times, label=command)

plt.title("addr2line runtime")
plt.xlabel("Input")
plt.ylabel("Time [s]")
plt.ylim(0, None)
plt.legend(title="Tool")

if args.output:
    plt.savefig(args.output)
else:
    plt.show()
