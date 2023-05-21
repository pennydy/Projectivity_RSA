# Projection Inferences in RSA model

This is the repository that accompanies the following paper:

- Pan, D. and Degen, J. (2023). Towards a computational account of projection inferences in clause-embedding predicates. In *Proceedings of the 45th Annual Conference of the Cognitive Science Society* [to appear].

## General information
We've conducted 3 behavioral experiments and explored multiple Bayesian models. Below is the general information about summary of each experiment and the overview of the repository, and more details can be found in the desginated folder.
- Experiment 1 (`1_listenerProjection`): about inferred speaker belief and speaker certainty in interrogatives
- Experiment 2 (`2_listenerSpeakerAH`): about inferred speaker and attitude holder belief in interrogatives
- Experiment 3 (`3_listenerSpeakerAH_Declaratives`): about inferred speaker and attitude holder belief in declarative sentences 


## Structure of this repository
```bash
├── analysis
│   ├── 1_listenerProjection
│   │   ├── graphs
│   │   └── rscripts
│   └── 2_listenerSpeakerAH
│   │   ├── graphs
│   │   ├── rscripts
│   ├── 3_listenerSpeakerAH_Declaratives
│   │   ├── graphs
│   │   ├── rscripts
├── data
│   ├── 1_listenerProjection
│   │   ├── main
│   │   └── pilot
│   └── 2_listenerSpeakerAH
│   │   ├── main
│   │   └── pilot
│   ├── 3_listenerSpeakerAH_Declaratives
│   │   ├── main
│   │   └── pilot
├── experiments
│   ├── 1_listenerProjection
│   └── 2_listenerSpeakerAH
│   ├── 3_listenerSpeakerAH_Declaratives
└── models
│   ├── BDA
│   └── chemo
│   ├── declaratives
│   └── interrogatives
└── writings
```
- `analysis`: R files for the visualization and main analyses for the bebavioral experiments
- `experiments`: experimental files used to for the bebavioral experiments
- `data`: raw data files from the behavioral experiments (anomyized)
- `models`: models that we've explored as well as rscripts for Bayesian data analysis and model simulations
- `writings`: published paper related to this project