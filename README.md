# qsm2shade: Analyze shadow casting of QSMs

## Description

`qsm2shade` is an R package ... TODO

## Installation from source

To install the package from github, the package `remotes` is required.

```R
# install package from github
remotes::install_github("zoeschindler/qsm2shade")
```

## Usage

```R
# load qsm
file_path <- system.file("extdata", "Prunus_avium_QSM_simplified.mat", package="qsm2shade")
qsm <- qsm2r::readQSM(file_path)

### plot shade of wood:

# plot qsm
qsm2r::plot(qsm, col = "salmon4", lit = TRUE)

# plot shade of wood
plot_shade_wood(qsm)

### plot shade of wood + simulated leaves:

# create polygons for single item
leaf <- create_single_leaf(leaf_type = "normal", length_m = 0.1)

# add items
leaves <- add_items(qsm, distribution, leaf, item_type = "leaves")

# plot qsm
qsm2r::plot(qsm, col = "salmon4", lit = TRUE)

# plot items
plot_items(leaves, col = "darkolivegreen3")

# plot shade of wood
plot_shade_wood(qsm)

# plot shade of items
plot_shade_items(leaves)

#### plot shade of wood + simulated flowers:

# create dummy item regression
distribution <- dummy_item_distribution()

# create polygons for single item
flower <- create_single_flower()

# create dummy item regression
distribution <- dummy_item_distribution()

# add items
flowers <- add_items(qsm, distribution, flower, item_type = "flowers")

# plot qsm
qsm2r::plot(qsm, col = "salmon4", lit = TRUE)

# plot items
plot_items(flowers, col = "pink2")

# plot shade of wood
plot_shade_wood(qsm)

# plot shade of items
plot_shade_items(flowers)
```

## About

Author: Zoe Schindler, <a href = "https://www.iww.uni-freiburg.de/">Chair of Forest Growth and Dendroecology</a>, <a href = "https://uni-freiburg.de/">University of Freiburg</a>

Shading algorithm based on:
Rosskopf E, Morhart C, Nahm M. Modelling Shadow Using 3D Tree Models in High Spatial and Temporal Resolution. Remote Sensing. 2017; 9(7):719. <a href = "https://doi.org/10.3390/rs9070719">doi: 10.3390/rs9070719</a>

