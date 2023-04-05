# models
contains models that we explored. 

## General information
`chemo`: first model considers interrogatives and declaratives as alternatives. include both speaker belief and attitude holder belief. 
- chemo.wppl
- chemo_production.wppl: attempts to reverse engineer the production probability based on empirical results for the pragmatic listener.
`declaratives`: consider only the declarative sentences. include both speaker belief and attitude holder belief.
- declaratives.wppl
- declaratives_goal.wppl: introduces different "goals" to capture possible speech acts.
`interrogatives`: consider only interrogatives. include only the speaker belief.
- threshold_qud.wppl: introduces belief threshold for each predicate.

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
│   ├── data
│   │   ├── binned-priors.csv
│   ├── graphs
│   │   results
│   │   ├── threshold_qud
│   │   rscripts
```
- `.wppl`: wppl model
- data: normed data from previous studies
- graphs: graphs of model predictions
- results: model predictions
- rscripts: rwebppl script to simulate results and plot predictions
