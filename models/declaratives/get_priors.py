import csv
import pprint
from collections import defaultdict

priors = []
item_prior = defaultdict(list)

with open("./data/prior_means.csv", mode='r') as fin:
    filereader = csv.reader(fin)
    next(filereader, None)
    # print(filereader)
    for prior_type, item_type, item, eventitem, Mean, CILow, CIHigh, YMin, YMax in filereader:
        # item_prior[item].append([float(i) for i in prop])
        item_prior[item_type].append(Mean)
    # print(item_prior)

for item_type, Mean in item_prior.items():
    item_prior[item_type] = [float(i) for i in Mean]
    item_prior[item_type].extend([1 - float(i) for i in Mean])
    print(item_type, ":", item_prior[item_type])
# pprint.pprint(item_prior)
    
