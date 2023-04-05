import csv
import pprint
from collections import defaultdict

priors = []
item_prior = defaultdict(list)

with open("./data/binned-priors.csv", mode='r') as fin:
    filereader = csv.reader(fin)
    next(filereader, None)
    # print(filereader)
    for item, content, prior_type, state, n, prop in filereader:
        # item_prior[item].append([float(i) for i in prop])
        item_prior[item].append(prop)

for item, prop in item_prior.items():
    item_prior[item] = [float(i) for i in prop]
    print(item, ":", item_prior[item])
# pprint.pprint(item_prior)
    
