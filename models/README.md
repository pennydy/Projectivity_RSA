# models
contains models that we have explored. 

## General information
`chemo`: consider interrogatives and declaratives as alternatives. include both speaker belief and attitude holder belief. 
- chemo.wppl
- chemo_production.wppl: attempts to reverse engineer the production probability based on empirical results for the pragmatic listener.

`declaratives`: consider only the declarative sentences. include both speaker belief and attitude holder belief.
- declaratives.wppl: qud selects which belief is at-issue.
- declaratives_goal.wppl: introduces different "goals" to capture possible speech acts.

`interrogatives`: consider only interrogatives. 
- threshold_qud.wppl: includes only the speaker belief. introduces belief threshold for each predicate. threshold is determined by predicate and qud.
- threshold_chemo.wppl: includes both speaker belief and ah belief, and the threshold for each type of belief varies by predicate. qud selects which belief is at-issue.

## Structure of this repository
```bash
├── chemo
│   ├── chemo.wppl
│   ├── chemo_production.wppl
│   ├── graphs
│   │   ├── chemo
│   │   ├── chemo_production
│   ├── results
│   │   ├── chemo_production
│   ├── rescripts
├── declaratives
│   ├── declaratives.wppl
│   ├── declaratives_goal.wppl
│   ├── data
│   │   ├── prior_means.csv
│   ├── graphs
│   │   results
│   │   rscripts
├── interrogatives
│   ├── threshold_qud.wppl
│   ├── threshold_chemo.wppl
│   ├── data
│   │   ├── binned-priors.csv
│   │   ├── predicate-nai.csv
│   ├── graphs
│   │   results
│   │   ├── threshold_qud
│   │   ├── threshold_chemo
│   │   rscripts
```
- `.wppl`: wppl model
- data: normed data from previous studies
- graphs: graphs of model predictions
- results: model predictions
- rscripts: rwebppl script to simulate results and plot predictions
