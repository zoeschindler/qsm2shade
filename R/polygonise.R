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

  # do stuff

  # return geoms
  return(NULL)
}

################################################################################
