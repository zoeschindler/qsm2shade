################################################################################
# HELPER FUNCTIONS
################################################################################

# convert radians to degrees
rad2deg <- function(rad) {
  return(rad * 180 / pi)
}

################################################################################

# convert degrees to radians
deg2rad <- function(deg) {
  return(deg * pi / 180)
}

################################################################################

# convert obj object (read with rgl) to geom matrix
obj2geom <- function(obj) {

  # get coordinates
  xyz <- obj[["vb"]][1:3,]

  # empty storage
  geom <- c()
  last_id <- 0

  # get triangles
  if (!is.null(obj[["it"]])) {
    triangle_indices <- obj[["it"]]
    triangle_indices <- triangle_indices[c(1:3,1),]
    for (i in 1:ncol(triangle_indices)) {
      curr <- cbind(last_id + 1,t(xyz[,triangle_indices[,i]]))
      geom <- rbind(geom, curr)
      last_id <- last_id + 1
    }
  }

  # get quads
  if (!is.null(obj[["ib"]])) {
    quad_indices <- obj[["ib"]]
    quad_indices <- quad_indices[c(1:4,1),]
    for (i in 1:ncol(quad_indices)) {
      curr <- cbind(last_id + 1,t(xyz[,quad_indices[,i]]))
      geom <- rbind(geom, curr)
      last_id <- last_id + 1
    }
  }

  # change column names
  colnames(geom) <- c("id", "x", "y", "z")

  # retunr matrix
  return(geom)
}

################################################################################

# normalize an vector
norm_vec <- function(v) {
  return(v / sqrt(sum(v**2)))
}

################################################################################

# calculate normalized crossproduct of xyz matrices
norm_cross <- function(v, w) {

  # object for storage
  cross <- matrix(NA, nrow = nrow(v), ncol = 3)

  # cross product
  cross[,1] <- v[,2] * w[,3] - v[,3] * w[,2]
  cross[,2] <- v[,3] * w[,1] - v[,1] * w[,3]
  cross[,3] <- v[,1] * w[,2] - v[,2] * w[,1]

  # return normalized result
  return(cross / sqrt(cross[,1]**2 + cross[,2]**2 + cross[,3]**2))
}

################################################################################

# calculate crossproduct of xyz matrices
simple_cross <- function(v, w) {

  # object for storage
  cross <- matrix(NA, nrow = nrow(v), ncol = 3)

  # cross product
  cross[,1] <- v[,2] * w[,3] - v[,3] * w[,2]
  cross[,2] <- v[,3] * w[,1] - v[,1] * w[,3]
  cross[,3] <- v[,1] * w[,2] - v[,2] * w[,1]

  # return result
  return(cross)
}

################################################################################

# calculate scalar product of xyz matrices
scalar_prod <- function(v, w) {

  # return scalar product
  return(v[,1] * w[,1] + v[,2] * w[,2] + v[,3] * w[,3])
}

################################################################################

# convert first list element with a geom matrix to polygons
list_polygonize <- function(element) {
  return(terra::vect(element[[1]][,-4], type = "polygons"))
}

################################################################################

# get branch IDs if child branches
find_childs_recursive_branch <- function(cylinder, branch_ID, include_self = TRUE) {

  # get cylinders of the branches
  cyl_sub <- cylinder[cylinder$branch %in% branch_ID,]

  # get all cylinders which are children of the branches
  cyl_childs <- cylinder[cylinder$parent %in% cyl_sub$cyl_id & !(cylinder$branch %in% branch_ID),]

  # return the branch IDs of the children
  if (nrow(cyl_childs) == 0) {
    if (include_self) {
      return(branch_ID)
    } else {
      return(NULL)
    }
  } else {
    id_childs <- unique(cyl_childs$branch)
    id_childs_childs <- find_childs_recursive_branch(cylinder, id_childs)
    if (include_self) {
      return(c(branch_ID, id_childs, id_childs_childs))
    } else {
      return(c(id_childs, id_childs_childs))
    }
  }
}

################################################################################

# prepare qsm for processing
prepare_qsm <- function(qsm, keep_all = FALSE) {

  # check data type
  if (any("QSM" %in% class(qsm))) {
    cylinder <- qsm@cylinder
  } else {
    cylinder <- qsm
  }

  # convert to matrix
  if (!any("matrix" %in% class(cylinder))) {
    cylinder <- as.matrix(cylinder)
  }

  # get relevant columns
  if (!keep_all) {
    cylinder <- cylinder[,c("radius", "length", "start_X", "start_Y", "start_Z", "axis_X", "axis_Y", "axis_Z")]
    # removes cyl_id, parent, extension, added, UnmodRadius, branch, SurfCov, mad, BranchOrder, PositionInBranch
  }

  # calculate end coordinates
  end_X = cylinder[,"start_X"] + cylinder[,"axis_X"] * cylinder[,"length"]
  end_Y = cylinder[,"start_Y"] + cylinder[,"axis_Y"] * cylinder[,"length"]
  end_Z = cylinder[,"start_Z"] + cylinder[,"axis_Z"] * cylinder[,"length"]
  cylinder <- cbind(cylinder, end_X, end_Y, end_Z)

  # return matrix
  return(cylinder)
}

################################################################################

#' Extract stem coordinates from wood geoms
#'
#' @description
#' \code{geom_tree_location} extracts the coordinates of the stem of a single
#' tree at a specified height range. Assumes that the input geoms describe
#' only one tree. The heights refer to the height over the ground.
#'
#' @param geom \code{matrix}, contains matrix with IDs and coordinates of wood
#' geoms.
#' @param lwr_height \code{numeric}, lower height threshold in meters.
#' @param upr_height \code{numeric}, upper height threshold in meters.
#'
#' @return
#' A \code{numeric} containing the \code{xyz}-coordinates of the stem center.
#'
#' @seealso \code{\link{plot_ground}}
#'
#' @examples
#' # load wood geoms
#' file_path <- system.file("extdata", "pear_wood.txt", package="qsm2shade")
#' geom_wood <- read.table(file_path, header = T
#'
#' # get stem location
#' location <- las_tree_location(geom_wood)
#' @export
geom_tree_location <- function(geom, lwr_height = 0.3, upr_height = 0.6) {

  # get the id of every geom within the height section
  min_z <- min(geom[,4])
  id_within <- unique(geom[geom[,4] >= min_z + lwr_height & geom[,4] <= min_z + upr_height, 1])

  # get the x + y values of the geom vertices
  x_vals <- geom[geom[,1] %in% id_within, 2]
  y_vals <- geom[geom[,1] %in% id_within, 3]

  # calculate center of the 95% quantile
  x_center <- sum(quantile(x_vals, p = c(0.025, 0.975)))/2
  y_center <- sum(quantile(y_vals, p = c(0.025, 0.975)))/2

  # return center
  return(c("x" = x_center, "y" = y_center, "z" = min_z))
}

################################################################################

#' Move geoms by an offset
#'
#' @description
#' \code{geom_shift} shifts the coordinates of geoms by specified offsets in
#' the x, y and z direction.
#'
#' @param geom \code{matrix}, contains matrix with IDs and coordinates.
#' @param offset \code{numeric}, contains \code{xyz}-coordinates of the require shift.
#'
#' @return
#' A matrix with the IDs and coordinates of the shifted geoms.
#'
#' @seealso \code{\link{geom_tree_location}}
#'
#' @examples
#' # load wood geoms
#' file_path <- system.file("extdata", "pear_wood.txt", package="qsm2shade")
#' geom_wood <- read.table(file_path, header = T
#'
#' # get stem location
#' location <- las_tree_location(geom_wood)
#'
#' # shift stem location to 0,0,0
#' geom_wood <- geom_shift(geom_wood, location)
#' @export
geom_shift <- function(geom, offset = c(0,0,0)) {

  # shift coordinates one by one
  geom[,2] <- geom[,2] - offset[1]
  geom[,3] <- geom[,3] - offset[2]
  geom[,4] <- geom[,4] - offset[3]

  # return shifted geoms
  return(geom)
}

################################################################################

#' Extract stem coordinates from a point cloud of a single tree
#'
#' @description
#' \code{las_tree_location} extracts the coordinates of the stem of a single
#' tree at a specified height range. Assumes that the input \code{LAS} object
#' contains only one tree. The heights refer to the height over the ground.
#'
#' @param las An object of class \code{LAS}.
#' @param lwr_height \code{numeric}, lower height threshold in meters.
#' @param upr_height \code{numeric}, upper height threshold in meters.
#'
#' @return
#' A \code{numeric} containing the \code{xy}-coordinates of the stem center.
#'
#' @seealso \code{\link{las_tree_location}}, \code{\link{plot_ground}}
#'
#' @examples
#' # load las data
#' file_path <- system.file("extdata", "walnut_with_ground.las", package="qsm2shade")
#' las <- lidR::readLAS(file_path)
#'
#' # get stem location
#' location <- las_tree_location(las)
#' @export
las_tree_location <- function(las, lwr_height = 1.2, upr_height = 1.4) {

  # normalize point cloud
  las <- lidR::classify_ground(las, csf())
  las <- lidR::normalize_height(las, tin())

  # clip circle from las file
  las <- lidR::filter_poi(las, Z >= lwr_height & Z <= upr_height)

  # abort if there is no ground
  if (lidR::is.empty(las)) stop("no points at specified height")

  # extract stem location
  location <- c()
  location[1] <- median(las$X)
  location[2] <- median(las$Y)

  # return stem location
  return(location)
}

################################################################################

#' Extract plane normal of the ground from a point cloud
#'
#' @description
#' \code{las_ground_normal} extracts the plane normal of the ground at a
#' specified area. The area is specified via \code{xy}-coordinates and a radius.
#'
#' @param las An object of class \code{LAS}.
#' @param location \code{numeric}, \code{xy}-coordinates.
#' @param radius \code{numeric}, radius in meters.
#'
#' @return
#' A \code{numeric} containing the \code{xyz}-vector of the ground normal.
#'
#' @seealso \code{\link{plot_ground}}, \code{\link{las_ground_normal}}
#'
#' @examples
#' # load las data
#' file_path <- system.file("extdata", "walnut_with_ground.las", package="qsm2shade")
#' las <- lidR::readLAS(file_path)
#'
#' # get stem location
#' location <- las_tree_location(las)
#'
#' # calculate ground normal
#' ground_normal <- las_ground_normal(las, location)
#'
#' # load qsm
#' file_path <- system.file("extdata", "walnut.mat", package="qsm2shade")
#' qsm <- qsm2r::readQSM(file_path)
#'
#' # shift qsm to origin
#' qsm <- qsm2r::set_location(qsm, c(0,0,0))
#'
#' # plot qsm and ground
#' qsm2r::plot(qsm, col = "salmon4", lit = TRUE)
#' plot_ground(plane_origin = c(0,0,0), plane_normal = ground_normal, radius = 4, add = TRUE)
#' rgl::bg3d("white"); rgl::axes3d()
#'
#' # plot las
#' lidR::plot(las)
#' plot_ground(plane_origin = c(location[1]-min(las$X), location[2]-min(las$Y), min(las$Z)),
#'             plane_normal = ground_normal, radius = 4, add = TRUE)
#' rgl::bg3d("white"); rgl::axes3d()
#' @export
las_ground_normal <- function(las, location = c(0,0), radius = 3) {

  # clip circle from las file
  las <- lidR::clip_circle(las, xcenter = location[1], ycenter = location[2], radius = radius)

  # extract the ground
  las <- lidR::classify_ground(las, csf())
  las <- lidR::filter_ground(las)

  # abort if there is no ground
  if (lidR::is.empty(las)) stop("no ground found")

  # extract ground plane
  ground_points <- cbind(las$X, las$Y, las$Z)
  pca <- prcomp(ground_points, center = TRUE, scale. = FALSE)
  p_normal <- pca$rotation[,3]

  # return normal of the ground
  return(p_normal)
}

################################################################################
