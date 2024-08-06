#!/usr/bin/env python3

import argparse
import glob
import json
import matplotlib.pyplot as plt
import numpy as np

parser = argparse.ArgumentParser()
parser.add_argument("-o", "--output", help="Save image to the given filename.")

args = parser.parse_args()

commands = None
data = []
inputs = []

for filename in sorted(glob.glob("scripts/tmp/benchmark-*.json")):
    with open(filename) as f:
        results = json.load(f)["results"]
    commands = [b["command"] for b in results]
    data.append([b["mean"] for b in results])

    input = results[0]["parameters"]["input"]
    binary_size = results[0]["parameters"]["size"]
    symbol_queries = results[0]["parameters"]["symbol_queries"]
    inputs.append(f"{input}\n({binary_size}B,\n{symbol_queries} queries)")

data = np.transpose(data)

x = np.arange(len(inputs))  # the label locations
width = 0.2  # the width of the bars

fig, ax = plt.subplots(layout="constrained")
for i, command in enumerate(commands):
    offset = width * (i + 1)
    rects = ax.bar(x + offset, data[i], width, label=command)

ax.set_xticks(x + 0.5, inputs)

plt.title("addr2line runtime")
plt.xlabel("Input")
plt.ylabel("Time [s]")
plt.legend(title="Tool")

if args.output:
    plt.savefig(args.output)
else:
    plt.show()
