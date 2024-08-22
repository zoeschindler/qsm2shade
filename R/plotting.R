################################################################################
# MAIN FUNCTIONS
################################################################################

#' Plot a single item
#'
#' @description
#' \code{plot_single_item} plots a single item to be used by \code{add_item()}.
#'
#' @param item_poly \code{matrix} or \code{list}, item to be simulated,
#' contains coordinates of a single polygon in a matrix or a list of matrices.
#' @param col \code{character}, color of the item.
#'
#' @return
#' \code{rgl} plot of one item.
#'
#' @seealso \code{\link{add_item}}, \code{\link{create_single_flower}},
#' \code{\link{create_single_leaf}},
#'
#' @examples
#' # create single flower
#' flower <- create_single_flower()
#'
#' # plot single flower
#' plot_single_item (flower)
#'
#' # or:
#'
#' #' # create single leaf
#' leaf <- create_single_leaf(leaf_type = "normal")
#'
#' # plot single leaf
#' plot_single_item (leaf)
#' @export
plot_single_item <- function(item_poly, col = "#F4ACB7") {

  # convert polygon to list of polygons if it isn't
  if (!any("list" %in% class(item_poly))) item_poly <- list(item_poly)

  # plot single flower#
  item_rgl <- lapply(item_poly, function(curr) {
    poly_i <- NULL
    if (is.null(poly_i)) {
      try({poly_i <- rgl::polygon3d(
        x = curr$x, y = curr$y, z = curr$z,
        plot = FALSE, coords = c(1,3))}, silent = T)
      if (is.null(poly_i)) {
        try({poly_i <- rgl::polygon3d(
          x = curr$x, y = curr$y, z = curr$z,
          plot = FALSE, coords = c(2,3))}, silent = T)
        if (is.null(poly_i)) {
          try({poly_i <- rgl::polygon3d(
            x = curr$x, y = curr$y, z = curr$z,
            plot = FALSE, coords = c(1,2))}, silent = T)
        }
      }
    }
    poly_i$material$color <- col
    poly_i
  })
  rgl::shade3d(rgl::shapelist3d(item_rgl, plot = FALSE), lit = TRUE)
}

################################################################################

#' Plot simulated items
#'
#' @description
#' \code{plot_items} plots items simulated by \code{add_item()}.
#'
#' @param item_pts \code{matrix}, contains coordinates of simulated items.
#' @param col \code{character}, color of the items.
#' @param add \code{boolean}, add the plot to current active \code{rgl} plot.
#' @param lit \code{boolean}, whether the items should be lit.
#'
#' @return
#' \code{rgl} plot of all items.
#'
#' @seealso \code{\link{add_item}}, \code{\link{plot_shade_items}}
#'
#' @examples
#' # load qsm
#' file_path <- system.file("extdata", "walnut.mat", package="qsm2shade")
#' qsm <- qsm2r::readQSM(file_path)
#'
#' # create dummy item regression
#' distribution <- dummy_item_distribution()
#'
#' # create polygons for single item
#' flower <- create_single_flower()
#'
#' # add items
#' flowers <- add_items(qsm, distribution, flower, item_type = "flowers")
#'
#' # plot qsm
#' qsm2r::plot(qsm, col = "salmon4", lit = TRUE)
#'
#' # plot items
#' plot_items(flowers, col = "pink2")
#'
#' # or:
#'
#' # create polygons for single item
#' leaf <- create_single_leaf(leaf_type = "normal", length_m = 0.1)
#'
#' # add items
#' leaves <- add_items(qsm, distribution, leaf, item_type = "leaves")
#'
#' # plot qsm
#' qsm2r::plot(qsm, col = "salmon4", lit = TRUE)
#'
#' # plot items
#' plot_items(leaves, col = "darkolivegreen3")
#' @export
plot_items <- function(item_pts, col, add = TRUE, lit = TRUE) {

  # remove items with NAs
  item_pts <- na.omit(item_pts)

  # open new window
  if (!add) rgl::open3d()

  # plot items
  item_rgl <- lapply(1:nrow(item_pts), function(i) {
    poly_i <- NULL
    if (is.null(poly_i)) {
      try({poly_i <- rgl::polygon3d(
        x = as.numeric(item_pts[i,endsWith(colnames(item_pts),"x")]),
        y = as.numeric(item_pts[i,endsWith(colnames(item_pts),"y")]),
        z = as.numeric(item_pts[i,endsWith(colnames(item_pts),"z")]),
        plot = FALSE, coords = c(1,2))}, silent = T)
      if (is.null(poly_i)) {
        try({poly_i <- rgl::polygon3d(
          x = as.numeric(item_pts[i,endsWith(colnames(item_pts),"x")]),
          y = as.numeric(item_pts[i,endsWith(colnames(item_pts),"y")]),
          z = as.numeric(item_pts[i,endsWith(colnames(item_pts),"z")]),
          plot = FALSE, coords = c(2,3))}, silent = T)
        if (is.null(poly_i)) {
          try({poly_i <- rgl::polygon3d(
            x = as.numeric(item_pts[i,endsWith(colnames(item_pts),"x")]),
            y = as.numeric(item_pts[i,endsWith(colnames(item_pts),"y")]),
            z = as.numeric(item_pts[i,endsWith(colnames(item_pts),"z")]),
            plot = FALSE, coords = c(1,3))}, silent = T)
        }
      }
    }
    poly_i$material$color <- col
    poly_i
  })
  rgl::shade3d(rgl::shapelist3d(item_rgl, plot = FALSE), lit = lit)
}

################################################################################

#' Plot polygons
#'
#' @description
#' \code{plot_polys} plots items simulated by \code{add_item()}.
#'
#' @param poly_geom \code{matrix}, contains matrix with IDs and coordinates.
#' @param col \code{character}, color of the items.
#' @param add \code{boolean}, add the plot to current active \code{rgl} plot.
#' @param lit \code{boolean}, whether the polygons should be lit.
#'
#' @return
#' \code{rgl} plot of all items.
#'
#' @seealso \code{\link{add_item}}, \code{\link{plot_shade_items}}
#'
#' @examples
#' # load wood polygons
#' file_path <- system.file("extdata", "pear_wood.txt", package="qsm2shade")
#' poly_wood <- read.table(file_path, header = T)
#'
#' # load leaf polygons
#' file_path <- system.file("extdata", "pear_leaves.txt", package="qsm2shade")
#' poly_leaves <- read.table(file_path, header = T)
#'
#' # plot wood & leaves (takes quite some time)
#' plot_polys(poly_wood, col = "salmon4", add = F)
#' plot_polys(poly_leaves, col = "darkolivegreen3", add = T)
#' @export
plot_polys <- function(poly_geom, col = "grey40", add = TRUE, lit = TRUE) {

  # remove items with NAs
  poly_geom <- na.omit(poly_geom)

  # open new window
  if (!add) rgl::open3d()

  # plot items
  poly_rgl <- lapply(unique(poly_geom[,1]), function(id) {
    poly_id <- NULL
    if (is.null(poly_id)) {
      try({poly_id <- rgl::polygon3d(poly_geom[poly_geom[,1] == id,2:4],
                                     plot = FALSE, coords = c(1,2))}, silent = T)
      if (is.null(poly_id)) {
        try({poly_id <- rgl::polygon3d(poly_geom[poly_geom[,1] == id,2:4],
                                       plot = FALSE, coords = c(2,3))}, silent = T)
        if (is.null(poly_id)) {
          try({poly_i <- rgl::polygon3d(poly_geom[poly_geom[,1] == id,2:4],
                                        plot = FALSE, coords = c(1,3))}, silent = T)
        }
      }
    }
    poly_id$material$color <- col
    poly_id
  })
  rgl::shade3d(rgl::shapelist3d(poly_rgl, plot = FALSE), lit = lit)
}

################################################################################

plot_shade <- function(shade, col, add) {

  # open new window
  if (!add) rgl::open3d()

  # plot shade
  shade_rgl <- lapply(unique(shade[,1]), function(i) {
    subset <- shade[shade[,1] == i,]
    poly_i <- rgl::polygon3d(subset[,2:4], lit = FALSE,plot = FALSE)
    poly_i$material$color <- col
    poly_i
  })
  rgl::shade3d(rgl::shapelist3d(shade_rgl, plot = FALSE), lit = FALSE)
}

################################################################################

#' Plot shadows cast by wood
#'
#' @description
#' \code{plot_shade_wood} plots shadows of a \code{QSM} object. The ground can
#' be specified via a point on the plane and the plane normal. Per default, an
#' even ground at the coordinate origin is assumed.
#'
#' @param qsm An object of class \code{QSM}.
#' @param sun_direction \code{numeric}, vector containing the unit vector of the
#' sun direction.
#' @param plane_origin \code{numeric}, \code{xyz}-vector of a point on the
#' ground.
#' @param plane_norm \code{numeric}, \code{xyz}-vector of the ground normal.
#' @param col \code{character}, color of the shade.
#' @param add \code{boolean}, add the plot to current active \code{rgl} plot.
#'
#' @return
#' \code{rgl} plot of the shade.
#'
#' @seealso \code{\link{plot_shade_items}}
#'
#' @examples
#' # load qsm
#' file_path <- system.file("extdata", "walnut.mat", package="qsm2shade")
#' qsm <- qsm2r::readQSM(file_path)
#'
#' # shift qsm to origin
#' # (shade is always projected to z = 0)
#' qsm <- qsm2r::set_location(qsm, c(0,0,0))
#'
#' # plot qsm
#' qsm2r::plot(qsm, col = "salmon4", lit = TRUE)
#'
#' # plot shade of wood
#' plot_shade_wood(qsm)
#' @export
plot_shade_wood <- function(qsm, sun_direction = c(0.25, 0.5, -0.75),
                            plane_origin = c(0,0,0), plane_norm = c(0,0,1),
                            col = "grey40", add = TRUE) {

  # prepare qsm
  tree <- prepare_qsm(qsm, keep_all = FALSE)

  # prepare sun direction
  # https://doi.org/10.1080/713811744 (N- & S+)
  sun_direction[2] <- sun_direction[2] * (-1)

  # get shadows
  shade_wood <- shade_wood(sun_direction = sun_direction, tree = tree, plane_origin = plane_origin, plane_norm = plane_norm)

  # plot shadows
  plot_shade(shade_wood[[1]], col = col, add = add)
}

################################################################################

#' Plot shadows cast by items
#'
#' @description
#' \code{plot_shade_items} plots shadows of items simulated by
#' \code{add_item()}. The ground can be specified via a point on the plane and
#' the plane normal. Per default, an even ground at the coordinate origin is
#' assumed.
#'
#' @param item_pts \code{matrix}, contains coordinates of simulated items.
#' @param sun_direction \code{numeric}, vector containing the unit vector of the
#' sun direction.
#' @param plane_origin \code{numeric}, \code{xyz}-vector of a point on the
#' ground.
#' @param plane_norm \code{numeric}, \code{xyz}-vector of the ground normal.
#' @param col \code{character}, color of the shade.
#' @param add \code{boolean}, add the plot to current active \code{rgl} plot.
#'
#' @return
#' \code{rgl} plot of the shade.
#'
#' @seealso \code{\link{add_item}}, \code{\link{plot_shade}}
#'
#' @examples
#' # load qsm
#' file_path <- system.file("extdata", "walnut.mat", package="qsm2shade")
#' qsm <- qsm2r::readQSM(file_path)
#'
#' # shift qsm to origin
#' # (shade is always projected to z = 0)
#' qsm <- qsm2r::set_location(qsm, c(0,0,0))
#'
#' # create dummy item regression
#' distribution <- dummy_item_distribution()
#'
#' # create polygons for single item
#' flower <- create_single_flower()
#'
#' # create dummy item regression
#' distribution <- dummy_item_distribution()
#'
#' # add items
#' flowers <- add_items(qsm, distribution, flower, item_type = "flowers")
#'
#' # plot qsm
#' qsm2r::plot(qsm, col = "salmon4", lit = TRUE)
#'
#' # plot items
#' plot_items(flowers, col = "pink2")
#'
#' # plot shade of wood
#' plot_shade_wood(qsm)
#'
#' # plot shade of items
#' plot_shade_items(flowers)
#'
#' # or:
#'
#' # create polygons for single item
#' leaf <- create_single_leaf(leaf_type = "normal", length_m = 0.1)
#'
#' # add items
#' leaves <- add_items(qsm, distribution, leaf, item_type = "leaves")
#'
#' # plot qsm
#' qsm2r::plot(qsm, col = "salmon4", lit = TRUE)
#'
#' # plot items
#' plot_items(leaves, col = "darkolivegreen3")
#'
#' # plot shade of wood
#' plot_shade_wood(qsm)
#'
#' # plot shade of items
#' plot_shade_items(leaves)
#' @export
plot_shade_items <- function(item_pts, sun_direction = c(0.25, 0.5, -0.75),
                             plane_origin = c(0,0,0), plane_norm = c(0,0,1),
                             col = "grey40", add = TRUE) {

  # remove items with NAs
  item_pts <- na.omit(item_pts)

  # prepare sun direction
  # https://doi.org/10.1080/713811744 (N- & S+)
  sun_direction[2] <- sun_direction[2] * (-1)

  # get shadows
  shade_items <- shade_items(sun_direction = sun_direction, item_pts = item_pts, plane_origin = plane_origin, plane_norm = plane_norm)

  # plot shadows
  plot_shade(shade_items[[1]], col = col, add = add)
}

################################################################################

#' Plot ground
#'
#' @description
#' \code{plot_ground} plots a circle of ground. The ground is specified via a
#' point on the plane and the plane normal. For plotting, the radius of the
#' circle and the number of points in the circle outline is required. To
#' prevent plotted shade of being obscured by the ground, the ground is shifted
#' by a small offset along the z-axis.
#'
#' @param plane_origin \code{numeric}, \code{xyz}-vector of a point on the
#' ground.
#' @param plane_norm \code{numeric}, \code{xyz}-vector of the ground normal.
#' @param radius \code{numeric}, circle radius in meters.
#' @param n_dir \code{integer}, the number of directions at which points on the
#' circle are calculated.
#' @param z_offset \code{numeric}, offset along the z-axis in meters.
#' @param col \code{character}, color of the shade.
#' @param add \code{boolean}, add the plot to current active \code{rgl} plot.
#'
#' @return
#' \code{rgl} plot of the ground.
#'
#' @seealso \code{\link{plot_shade}}, \code{\link{plot_shade_items}}
#'
#' @examples
#' # load qsm
#' file_path <- system.file("extdata", "walnut.mat", package="qsm2shade")
#' qsm <- qsm2r::readQSM(file_path)
#'
#' # shift qsm to origin
#' # (shade is always projected to z = 0)
#' qsm <- qsm2r::set_location(qsm, c(0,0,0))
#'
#' # plot qsm
#' qsm2r::plot(qsm, col = "salmon4", lit = TRUE)
#'
#' # define ground
#' ground_origin <- c(0,0,0)
#' ground_normal <- c(0.1,0.05,1)
#'
#' # plot ground
#' plot_ground(plane_origin = ground_origin, plane_norm = ground_normal)
#'
#' # plot shade of wood
#' plot_shade_wood(qsm, plane_origin = ground_origin, plane_norm = ground_normal)
#' @export
plot_ground <- function(plane_origin = c(0,0,0), plane_norm = c(0,0,1),
                        radius = 12, n_dir = 30L, z_offset = -0.005,
                        col = "#B6CC8F", add = TRUE, lit = TRUE) {

  # stop if not enough angles
  if (n_dir <= 3) stop("")

  # define angles at which points should be calculated
  angles <- deg2rad(seq(from = 0, to = 360, length.out = n_dir + 1))

  # change format
  plane_norm <- t(plane_norm)

  # define vectors on the plane
  vec_random <- plane_norm + c(1,0,0)
  vec_plane_1 <- simple_cross(plane_norm, vec_random)
  vec_plane_2 <- simple_cross(plane_norm, vec_plane_1)

  # calculate points
  p_x <- plane_origin[1] + radius * (cos(angles) * vec_plane_1[1] + sin(angles) * vec_plane_2[1])
  p_y <- plane_origin[2] + radius * (cos(angles) * vec_plane_1[2] + sin(angles) * vec_plane_2[2])
  p_z <- plane_origin[3] + radius * (cos(angles) * vec_plane_1[3] + sin(angles) * vec_plane_2[3])
  p_circle <- cbind(1, p_x, p_y, p_z)

  # add z offset
  p_circle[,4] <- p_circle[,4] + z_offset

  # open new window
  if (!add) rgl::open3d()

  # plot shade
  rgl::polygon3d(p_circle[,2:4], col = col, lit = lit)
}

################################################################################
