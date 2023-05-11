# Projection Inferences in RSA model

This is the repository that accompanies the following paper:

- Pan, D. and Degen, J. (2023). Towards a computational account of projection inferences in clause-embedding predicates. In *Proceedings of the 45th Annual Conference of the Cognitive Science Society* [to appear].

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
└── writing
```
- `analysis`: R files for the and main analyses
- `experiments`: experimental files used to for the experiments
- `data`: raw data files (anomyized)
- `models`: models that we've explored, scripts for Bayesian data analysis and model simulations
- `writing`: published paper related to this project