# models
this folder contains models that we have explored. 

## General information
`BDA`: all files for Bayesian Data Analysis.

`chemo`: the original chemo model and its variants. consider interrogatives and declaratives as alternatives. include both speaker belief and attitude holder belief. 
- chemo.wppl
- chemo_production.wppl: attempts to reverse engineer the production probability based on empirical results for the pragmatic listener.

`declaratives`: consider only the declarative sentences. include both speaker belief and attitude holder belief.
- declaratives.wppl: qud selects which belief is at-issue.
- declaratives_goal.wppl: introduces different "goals" to capture possible speech acts.

`interrogatives`: consider only interrogatives. 
- threshold_qud.wppl: includes only the speaker belief. introduces belief threshold for each predicate. threshold is determined by predicate and qud.
- threshold_chemo.wppl: includes both speaker belief and ah belief, and the threshold for each type of belief varies by predicate. qud selects which belief is at-issue.
<!-- - threshold_mix.wppl: includes both speaker belief and ah belief in the meaning function (i.e., the meaning function involves checking both the speaker belief and the ah belief against their respective predicate-specific threshold), but only infers the speaker belief. -->
- threshold_mix.wppl: includes only speaker belief. pragmatic listener samples speaker beliefs from either the speaker production probability or the prior belief distribution (based on the qud). literal listener samples beliefs from a uniform prior.
- threshold_cg.wppl (formerly threshold_wonky): includes only speaker belief. both literal and pragmaitc listener samples beliefs from either a uniform distribution or a prior belief distribution based on how likely they think the common ground is shared. the `_qud.wppl` varient of this model assumes this cg likelihood is based on the qud.

## Main structure of this folder
```bash
├── BDA
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
│   ├── threshold_mix.wppl
│   ├── threshold_cg.wppl
│   ├── threshold_cg_qud.wppl
│   ├── data
│   │   ├── binned-priors.csv
│   │   ├── predicate-nai.csv
│   ├── graphs
│   │   results
│   │   ├── threshold_qud
│   │   ├── threshold_chemo
│   │   ├── threshold_mix
│   │   rscripts
```
## Structure of the sub-folders
- `.wppl`: wppl model
- data: normed data from previous studies
- graphs: graphs of model predictions
- results: model predictions
- rscripts: rwebppl script to simulate results and plot predictions
