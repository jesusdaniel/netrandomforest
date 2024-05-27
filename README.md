# netRandomForest

Original code written by Tiffany Chang and helper functions (`r ase`, `r cluster_testing`, `r clustering_vertices`, etc.) by Jesús Daniel Arroyo Relión. Real data for testing the netRandomForest functions retrieved from "Network classification with applications to brain connectomics" https://arxiv.org/abs/1701.08140


# R Code

The netRandomForest package implements spectral clustering in two different ways (outside of all trees and within each tree) for the standard random forest model. More details within the help files (e.g. `r ?nrf1_training`).


# Installation

```
# install.packages("devtools")
library(devtools)
install_github("jesusdaniel/netrandomforest")
```


# Data

The COBRE data was obtained from http://fcon_1000.projects.nitrc.org/indi/retro/cobre.html.

Aine, C. J., et al. "Multimodal neuroimaging in schizophrenia: description and dissemination." Neuroinformatics 15.4 (2017): 343-364.

Arroyo Relión, J.D., Kessler, D., Levina, E., Taylor, S.F., "Network classification with applications to brain connectomics". Ann. Appl. Stat. 13 (2019), no. 3, 1648--1677
