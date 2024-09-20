################################################################################
# HELPER FUNCTIONS
################################################################################

################################################################################
# MAIN FUNCTIONS
################################################################################

las_slice <- function(las, slice_distance = 5, slice_resolution = 0.1,
                      slice_minimum_n = 5) {

  # get minimum coordinates
  offset_x <- min(las@data$X)
  offset_y <- min(las@data$Y)
  offset_z <- min(las@data$Z)

  # shift point cloud
  las@data$X <- las@data$X - offset_x
  las@data$Y <- las@data$Y - offset_y
  las@data$Z <- las@data$Z - offset_z

  # get slice boundaries
  las_ext <- ext(las)
  slices_x <- seq(plyr::round_any(las_ext$xmin, slice_distance, f = floor), plyr::round_any(las_ext$xmax, slice_distance, f = ceiling), slice_distance)
  slices_y <- seq(plyr::round_any(las_ext$ymin, slice_distance, f = floor), plyr::round_any(las_ext$ymax, slice_distance, f = ceiling), slice_distance)

  # prepare storage
  last_id <- 0
  geoms <- c()

  # loop through slices (x-direction)
  n_slices_x <- length(slices_x) - 1
  for (idx in 1:n_slices_x) {

    # subset point cloud
    slice <- lidR::filter_poi(las, X >= slices_x[idx] & X < slices_x[idx + 1])
    if (lidR::is.empty(slice)) next

    # switch x with z
    temp <- slice@data$Z
    slice@data$Z <- slice@data$X
    slice@data$X <- temp

    # derive raster
    rast_curr <- lidR::pixel_metrics(slice, length(X) >= slice_minimum_n, res = slice_resolution)

    # remove cells with too few points
    rast_curr <- terra::clamp(rast_curr, 1, 1)

    # convert to polygons
    poly_curr <- terra::as.polygons(rast_curr, values = FALSE)

    # derive geoms
    geom_curr <- terra::geom(poly_curr)

    # change ids
    geom_curr[,"part"] <- geom_curr[,"part"] + last_id
    last_id <- max(geom_curr[,"part"])

    # switch z with x
    geom_curr <- cbind(geom_curr[,c("geom", "part")], mean(slices_x[idx:(idx + 1)]), geom_curr[,c("y", "x", "hole")])
    colnames(geom_curr) <- c("geom", "part", "x", "y", "z", "hole")

    # safe geom matrix
    geoms <- rbind(geoms, geom_curr)
  }

  # loop through slices (y-direction)
  n_slices_y <- length(slices_y) - 1
  for (idx in 1:n_slices_y) {

    # subset point cloud
    slice <- lidR::filter_poi(las, Y >= slices_y[idx] & Y < slices_y[idx + 1])
    if (lidR::is.empty(slice)) next

    # switch x with z
    temp <- slice@data$Z
    slice@data$Z <- slice@data$Y
    slice@data$Y <- temp

    # derive raster
    rast_curr <- lidR::pixel_metrics(slice, length(Y) >= slice_minimum_n, res = slice_resolution)

    # remove cells with too few points
    rast_curr <- terra::clamp(rast_curr, 1, 1)

    # convert to polygons
    poly_curr <- terra::as.polygons(rast_curr, values = FALSE)

    # derive geoms
    geom_curr <- terra::geom(poly_curr)

    # change ids
    geom_curr[,"part"] <- geom_curr[,"part"] + last_id
    last_id <- max(geom_curr[,"part"])

    # switch z with y
    geom_curr <- cbind(geom_curr[,c("geom", "part", "x")], mean(slices_y[idx:(idx + 1)]), geom_curr[,c("y", "hole")])
    colnames(geom_curr) <- c("geom", "part", "x", "y", "z", "hole")

    # safe geom matrix
    geoms <- rbind(geoms, geom_curr)
  }

  # shift geoms back
  geoms[,"x"] <- geoms[,"x"] + offset_x
  geoms[,"y"] <- geoms[,"y"] + offset_y
  geoms[,"z"] <- geoms[,"z"] + offset_z

  # return geoms
  return(geoms)
}

################################################################################
