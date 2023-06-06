# qsm2shade: Analyze shadow casting of QSMs

## Description <img src="https://github.com/zoeschindler/qsm2shade/blob/master/inst/figures/logo.png" align="right" width = 290/>

`qsm2shade` is an R package to calculate the shadow casting of QSMs (quantitative structure models) created in Matlab using <a href = "https://github.com/InverseTampere/TreeQSM">TreeQSM</a>. The package uses `QSM` objects read in using <a href = "https://github.com/zoeschindler/qsm2r">qsm2r</a>. The shade of the tree for one sun position can be plotted using `plot_shade_wood()`. To enable shade estimation of trees carrying leaves or flowers, those can be simulated using  `add_items()`. To plot added items and their shade, use `plot_items()` and `plot_shade_items()`. To simulate the shading of a tree with or without added items over time, the function `shade_tree()` can be used. To increase efficiency, the simulation of the shade can be executed using parallel processing. The used time steps should be one hour or less, the radiation data should be given in hourly steps and the energy unit should correspond to the chosen time steps and raster resolution.

## Installation from source

To install the package from github, the package `remotes` is required.

```R
# install package from github
remotes::install_github("zoeschindler/qsm2shade")
```

## Example: Plot QSM with shade

```R
# load qsm
file_path <- system.file("extdata", "Prunus_avium_QSM_simplified.mat", package="qsm2shade")
qsm <- qsm2r::readQSM(file_path)

# plot qsm
qsm2r::plot(qsm, col = "salmon4", lit = TRUE)

# plot shade of wood
plot_shade_wood(qsm)
```

## Example: Plot QSM with shade & leaves

```R
# load qsm
file_path <- system.file("extdata", "Prunus_avium_QSM_simplified.mat", package="qsm2shade")
qsm <- qsm2r::readQSM(file_path)

# create dummy item regression
distribution <- dummy_item_distribution()

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
```

## Example: Calculate QSM shade for one day

```R
# load qsm
file_path <- system.file("extdata", "Prunus_avium_QSM_simplified.mat", package="qsm2shade")
qsm <- qsm2r::readQSM(file_path)

# get sun position at different times (one day, 10 min steps)
timeframe <- seq(ISOdate(2020, 03, 22, 0, 0), ISOdate(2020, 03, 22, 23, 50), "10 mins")
sun_position <- sun_movement(timeframe, latitude = 48.07, longitude = 7.60)

# create dummy radiation data
radiation_hourly <- dummy_radiation_hourly(ISOdate(2020, 01, 01, 0, 0), ISOdate(2020, 12, 31, 23, 50))

# calculate shade (10cm resolution)
result <- shade_tree(qsm, sun_position, radiation_hourly, resolution = 0.1)

# show shade
plot(result)
```

## Comparison: 3D plot and 2D raster

```R
# load qsm
file_path <- system.file("extdata", "Prunus_avium_QSM_simplified.mat", package="qsm2shade")
qsm <- qsm2r::readQSM(file_path)

# dummy sun direction
sun_dir <- c(0.25, -0.5, -0.75)

# plot qsm
qsm2r::plot(qsm, col = "salmon4", lit = TRUE)

# plot shade of wood
plot_shade_wood(qsm, sun_dir)
```

<img src="https://github.com/zoeschindler/qsm2shade/blob/master/inst/figures/example_plot_shade_wood.png" align="center" width = 600/>

```R
# plot shading raster of wood
sun_position <- sun_movement(ISOdate(2020, 03, 22, 12, 0), latitude = 48.07, longitude = 7.60)
sun_position[,1:3] <- sun_dir
plot(shade_tree(
  qsm, sun_position, dummy_radiation_hourly(), resolution = 0.01,
  xmin = -10, xmax = 10, ymin = -10, ymax = 10), col = c("#296682", "#81B1C4"))
```

<img src="https://github.com/zoeschindler/qsm2shade/blob/master/inst/figures/example_shade_tree.png" align="center" width = 600/>

## About

Author: Zoe Schindler, <a href = "https://www.iww.uni-freiburg.de/">Chair of Forest Growth and Dendroecology</a>, <a href = "https://uni-freiburg.de/">University of Freiburg</a>

**Shading based on:**
Rosskopf E, Morhart C, Nahm M. Modelling Shadow Using 3D Tree Models in High Spatial and Temporal Resolution. Remote Sensing. 2017; 9(7):719. <a href = "https://doi.org/10.3390/rs9070719">doi: 10.3390/rs9070719</a>

