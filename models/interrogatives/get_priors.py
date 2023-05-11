import csv
import argparse
import pprint
from collections import defaultdict

parser = argparse.ArgumentParser()
parser.add_argument("-i", "--input", help="Input file")

args = parser.parse_args()
priors = []
item_prior = defaultdict(list)

file = args.input
with open("./data/ternary-priors.csv", mode='r') as fin:
# with open("./data/binary-priors.csv", mode='r') as fin:
# with open("./data/binned-priors.csv", mode='r') as fin:
    filereader = csv.reader(fin)
    next(filereader, None)
    # print(filereader)
    for item, content, prior_type, state, prop in filereader:
    # for item, content, prior_type, state, n, prop in filereader:
    # for prior_type, content, item, state, prop in filereader:
        # item_prior[item].append([float(i) for i in prop])
        item_prior["\'"+item+"\'"].append(prop)

for item, prop in item_prior.items():
    item_prior[item] = [float(i) for i in prop]
    print(item, ":", item_prior[item],",")
# pprint.pprint(item_prior)
    
