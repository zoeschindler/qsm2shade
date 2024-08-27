# qsm2shade: Analyze shade cast of QSMs

## Description <img src="https://github.com/zoeschindler/qsm2shade/blob/master/inst/figures/logo.png" align="right" width = 290/>

`qsm2shade` is an R package to calculate the shadow casting of QSMs (quantitative structure models) created in Matlab using <a href = "https://github.com/InverseTampere/TreeQSM">TreeQSM</a>. The package uses `QSM` objects read in using <a href = "https://github.com/zoeschindler/qsm2r">qsm2r</a>. The shade of the tree for one sun position can be plotted using `plot_shade_qsm()`. To enable shade estimation of trees carrying leaves or flowers, those can be simulated using  `add_geoms()`. To plot added geoms and their shade, use `plot_geoms()` and `plot_shade_geoms()`. To simulate the shading of a tree with or without added geoms over time, the function `shade_tree()` can be used. To increase efficiency, the simulation of the shade can be executed using parallel processing. The used time steps should be one hour or less, the radiation data should be given in hourly steps and the energy unit should correspond to the chosen time steps and raster resolution.

## References

### Code author

<a href = "https://orcid.org/0000-0003-2972-1920">Zoe Schindler</a>, Chair of Forest Growth and Dendroecology, University of Freiburg

### Presented & validated in

Schindler Z, Larysch E, Frey J, Sheppard JP, Obladen N, Kröner K, Seifert T, Morhart C (2024). From dawn to dusk: High resolution tree shading model based on terrestrial LiDAR data. *Remote Sensing 16*(12), 2189. <a href = "https://doi.org/10.3390/rs16122189">doi: 10.3390/rs16122189</a>

### Shading algorithm based on

Rosskopf E, Morhart C, Nahm M (2017). Modelling Shadow Using 3D Tree Models in High Spatial and Temporal Resolution. *Remote Sensing 9*(7), 719. <a href = "https://doi.org/10.3390/rs9070719">doi: 10.3390/rs9070719</a>

## Installation from source

To install the package from github, the package `remotes` is required.

```R
# install package from github
remotes::install_github("zoeschindler/qsm2shade")
```

## Example: Plot QSM with shade

```R
# load qsm
file_path <- system.file("extdata", "walnut.mat", package="qsm2shade")
qsm <- qsm2r::readQSM(file_path)

# shift qsm to origin
# (shade is always projected to z = 0)
qsm <- qsm2r::set_location(qsm, c(0,0,0))

# plot qsm
qsm2r::plot(qsm, col = "salmon4", lit = TRUE)

# plot shade of wood
plot_shade_qsm(qsm)
```

<img src = "https://github.com/zoeschindler/qsm2shade/blob/master/inst/figures/example_default_tree_shade.PNG" align="center" width = 800/>

## Example: Plot QSM with shade & leaves

```R
# load qsm
file_path <- system.file("extdata", "walnut.mat", package="qsm2shade")
qsm <- qsm2r::readQSM(file_path)

# shift qsm to origin
# (shade is always projected to z = 0)
qsm <- qsm2r::set_location(qsm, c(0,0,0))

# create dummy geom regression
distribution <- dummy_geom_distribution()

# create polygons for single geom
leaf <- create_leaf(type = "normal", length_m = 0.1)

# add geoms
leaves <- add_geoms(qsm, distribution, leaf, geom_type = "leaf")

# plot qsm
qsm2r::plot(qsm, col = "salmon4", lit = TRUE)

# plot geoms
plot_geoms(leaves, col = "darkolivegreen3")

# plot shade of wood
plot_shade_qsm(qsm)

# plot shade of geoms
plot_shade_geoms(leaves)
```

<img src = "https://github.com/zoeschindler/qsm2shade/blob/master/inst/figures/example_default_tree_shade_leaves.PNG" align="center" width = 800/>

## Example: Plot QSM with shade & flowers on sloped ground

```R
# load qsm
file_path <- system.file("extdata", "walnut.mat", package="qsm2shade")
qsm <- qsm2r::readQSM(file_path)

# shift qsm to origin
# (shade is always projected to z = 0)
qsm <- qsm2r::set_location(qsm, c(0,0,0))

# define ground
plane_origin <- c(0,0,0)
plane_normal <- c(0.1,0.2,1)

# create dummy geom regression
distribution <- dummy_geom_distribution()
distribution <- distribution[1:4,]
distribution$m_per_geom <- c(0.02,0.04,0.1,0.2)

# create polygons for single geom
flower <- create_flower(radius_m = 0.015)

# add geoms
flowers <- add_geoms(qsm, distribution, flower, geom_type = "leaf")

# plot qsm
qsm2r::plot(qsm, col = "salmon4", lit = TRUE)

# plot geoms
plot_geoms(flowers, col = "#FFC8B8")

# plot ground
plot_ground(plane_origin = plane_origin, plane_normal = plane_normal,
            radius = 6, col = "#B1DA9B", lit = FALSE)

# plot shade of wood
plot_shade_qsm(qsm, plane_origin = plane_origin, plane_normal = plane_normal)

# plot shade of geoms
plot_shade_geoms(flowers, plane_origin = plane_origin, plane_normal = plane_normal)

# modify plot
rgl::bg3d("#CAEEFF"); rgl::axes3d()
```

<img src = "https://github.com/zoeschindler/qsm2shade/blob/master/inst/figures/example_default_tree_shade_flowers_slope.PNG" align="center" width = 800/>

## Example: Calculate QSM shade for one day

```R
# load qsm
file_path <- system.file("extdata", "walnut.mat", package="qsm2shade")
qsm <- qsm2r::readQSM(file_path)

# shift qsm to origin
# (shade is always projected to z = 0)
qsm <- qsm2r::set_location(qsm, c(0,0,0))

# get sun position at different times (one day, 10 min steps)
timeframe <- seq(ISOdate(2020, 03, 22, 6, 0), ISOdate(2020, 03, 22, 19, 0), "1 hour")
sun_position <- sun_movement(timeframe, latitude = 48.07, longitude = 7.60)

# create dummy radiation data
radiation <- dummy_radiation(ISOdate(2020, 01, 01, 0, 0), ISOdate(2020, 12, 31, 23, 50), "1 hour")

# calculate shade (10cm resolution)
result <- shade_tree(qsm, sun_position = sun_position, radiation = radiation,
                     resolution = 0.1, xmin = -10, xmax = 10, ymin = -5, ymax = 10)

# show shade
terra::plot(result, col = c("#FB6F92", "#FFB3C6"))
```

<img src="https://github.com/zoeschindler/qsm2shade/blob/master/inst/figures/example_shade_over_day.PNG" align="center" width = 800/>

## Comparison: 3D plot and 2D raster

```R
# load qsm
file_path <- system.file("extdata", "walnut.mat", package="qsm2shade")
qsm <- qsm2r::readQSM(file_path)

# shift qsm to origin
# (shade is always projected to z = 0)
qsm <- qsm2r::set_location(qsm, c(0,0,0))

# sun direction
sun_position <- sun_movement(ISOdate(2020, 03, 22, 12, 0), latitude = 48.07, longitude = 7.60)

# plot shade of wood
plot_shade_qsm(qsm, as.numeric(sun_position[,1:3]), col = "grey60", add = FALSE)

# add axes
rgl::axis3d("x"); rgl::axis3d("y")

# plot shading raster of wood
terra::plot(shade_tree(qsm, sun_position = sun_position, resolution = 0.01,
            xmin = -4, xmax = 6, ymin = -1, ymax = 8), col = c("#FB6F92", "#FFB3C6"))
```

<img src="https://github.com/zoeschindler/qsm2shade/blob/master/inst/figures/example_plot_shade_wood.png" align="center" width = 800/>

<img src="https://github.com/zoeschindler/qsm2shade/blob/master/inst/figures/example_shade_tree.png" align="center" width = 800/>
