################################################################################
# MAIN FUNCTIONS
################################################################################

#' Plot a single geom
#'
#' @description
#' \code{plot_geom} plots a single geom to be used by \code{add_geoms()}.
#'
#' @param geom \code{matrix}, contains matrix with IDs and coordinates.
#' @param col \code{character}, color of the geom.
#' @param lit \code{boolean}, whether the polygons should be lit.
#' @param axes \code{boolean}, whether to add axes.
#'
#' @return
#' \code{rgl} plot of one geom.
#'
#' @seealso \code{\link{add_geoms}}, \code{\link{create_flower}},
#' \code{\link{create_leaf}},
#'
#' @examples
#' # create single flower
#' flower <- create_flower()
#'
#' # plot single flower
#' plot_geom (flower, "pink2")
#'
#' # or:
#'
#' # create single leaf
#' leaf <- create_leaf(type = "normal")
#'
#' # plot single leaf
#' plot_geom (leaf, "darkolivegreen3")
#' @export
plot_geom <- function(geom, col = "#6D9DC5", lit = TRUE, axes = TRUE) {

  # open new window
  rgl::open3d()

  # plot single geom
  ids <- unique(geom[,1])
  geom_rgl <- lapply(unique(geom[,1]), function(id) {
    poly_id <- NULL
    if (is.null(poly_id)) {
      try({poly_id <- rgl::polygon3d(geom[geom[,1] == id,2:4],
        plot = FALSE, coords = c(1,3))}, silent = T)
      if (is.null(poly_id)) {
        try({poly_id <- rgl::polygon3d(geom[geom[,1] == id,2:4],
          plot = FALSE, coords = c(2,3))}, silent = T)
        if (is.null(poly_id)) {
          try({poly_id <- rgl::polygon3d(geom[geom[,1] == id,2:4],
            plot = FALSE, coords = c(1,2))}, silent = T)
        }
      }
    }
    poly_id$material$color <- col
    poly_id
  })
  rgl::shade3d(rgl::shapelist3d(geom_rgl, plot = FALSE), lit = lit)

  # add axes + labels
  if (axes) {
    rgl::axes3d()
    rgl::title3d(xlab = "x", ylab = "y", zlab = "z")
  }
}

################################################################################

#' Plot polygons
#'
#' @description
#' \code{plot_geoms} plots geoms, e.g. those simulated by \code{add_geoms()}.
#'
#' @param geoms \code{matrix}, contains coordinates of simulated leaves /
#' flowers.
#' @param col \code{character}, color of the geoms.
#' @param add \code{boolean}, add the plot to current active \code{rgl} plot.
#' @param lit \code{boolean}, whether the polygons should be lit.
#'
#' @return
#' \code{rgl} plot of all geoms.
#'
#' @seealso \code{\link{add_geoms}}, \code{\link{plot_shade_geoms}}
#'
#' @examples
#' # load qsm
#' file_path <- system.file("extdata", "walnut.mat", package="qsm2shade")
#' qsm <- qsm2r::readQSM(file_path)
#'
#' # create dummy geom regression
#' distribution <- dummy_geom_distribution()
#'
#' # create polygons for single geom
#' leaf <- create_leaf(type = "normal", length_m = 0.1)
#'
#' # add geoms
#' leaves <- add_geoms(qsm, distribution, leaf, geom_type = "leaf")
#'
#' # plot qsm
#' qsm2r::plot(qsm, col = "salmon4", lit = TRUE)
#'
#' # plot geoms
#' plot_geoms(leaves, col = "darkolivegreen3")
#'
#' # or:
#'
#' # create polygons for single geom
#' flower <- create_flower(radius_m = 0.02)
#'
#' # add geoms
#' flowers <- add_geoms(qsm, distribution, flower, geom_type = "flower")
#'
#' # plot qsm
#' qsm2r::plot(qsm, col = "salmon4", lit = TRUE)
#'
#' # plot geoms
#' plot_geoms(flowers, col = "steelblue3")
#'
#' # or:
#'
#' #' # load wood polygons
#' file_path <- system.file("extdata", "pear_wood.txt", package="qsm2shade")
#' poly_wood <- read.table(file_path, header = T)
#'
#' # load leaf polygons
#' file_path <- system.file("extdata", "pear_leaves.txt", package="qsm2shade")
#' poly_leaves <- read.table(file_path, header = T)
#'
#' # plot wood & leaves (takes quite some time)
#' plot_geoms(poly_wood, col = "salmon4", add = F)
#' plot_geoms(poly_leaves, col = "darkolivegreen3", add = T)
#' @export
plot_geoms <- function(geoms, col = "#6D9DC5", add = TRUE, lit = TRUE) {

  # remove geoms with NAs
  geoms <- na.omit(geoms)

  # open new window
  if (!add) rgl::open3d()

  # plot geoms
  geom_rgl <- lapply(unique(geoms[,1]), function(id) {
    poly_id <- NULL
    if (is.null(poly_id)) {
      try({poly_id <- rgl::polygon3d(geoms[geoms[,1] == id,2:4],
                                     plot = FALSE, coords = c(1,2))}, silent = T)
      if (is.null(poly_id)) {
        try({poly_id <- rgl::polygon3d(geoms[geoms[,1] == id,2:4],
                                       plot = FALSE, coords = c(2,3))}, silent = T)
        if (is.null(poly_id)) {
          try({poly_id <- rgl::polygon3d(geoms[geoms[,1] == id,2:4],
                                        plot = FALSE, coords = c(1,3))}, silent = T)
        }
      }
    }
    poly_id$material$color <- col
    poly_id
  })
  rgl::shade3d(rgl::shapelist3d(geom_rgl, plot = FALSE), lit = lit)
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
#' \code{plot_shade_qsm} plots shadows of a \code{QSM} object. The ground can
#' be specified via a point on the plane and the plane normal. Per default, an
#' even ground at the coordinate origin is assumed.
#'
#' @param qsm An object of class \code{QSM}.
#' @param sun_direction \code{numeric}, vector containing the unit vector of the
#' sun direction.
#' @param plane_origin \code{numeric}, \code{xyz}-vector of a point on the
#' ground.
#' @param plane_normal \code{numeric}, \code{xyz}-vector of the ground normal.
#' @param col \code{character}, color of the shade.
#' @param add \code{boolean}, add the plot to current active \code{rgl} plot.
#'
#' @return
#' \code{rgl} plot of the shade.
#'
#' @seealso \code{\link{plot_shade_geoms}}
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
#' plot_shade_qsm(qsm)
#' @export
plot_shade_qsm <- function(qsm, sun_direction = c(0.25, 0.5, -0.75),
                            plane_origin = c(0,0,0), plane_normal = c(0,0,1),
                            col = "grey40", add = TRUE) {

  # prepare qsm
  tree <- prepare_qsm(qsm, keep_all = FALSE)

  # prepare sun direction
  # https://doi.org/10.1080/713811744 (N- & S+)
  sun_direction[2] <- sun_direction[2] * (-1)

  # get shadows
  shade_qsm <- shade_qsm_comp(sun_direction = sun_direction, tree = tree, plane_origin = plane_origin, plane_normal = plane_normal)

  # plot shadows
  plot_shade(shade_qsm[[1]], col = col, add = add)
}

################################################################################

#' Plot shadows cast by geoms
#'
#' @description
#' \code{plot_shade_geoms} plots shadows of geoms simulated by
#' \code{add_geoms()}. The ground can be specified via a point on the plane and
#' the plane normal. Per default, an even ground at the coordinate origin is
#' assumed.
#'
#' @param geoms \code{matrix}, contains coordinates of simulated leaves /
#' flowers.
#' @param sun_direction \code{numeric}, vector containing the unit vector of the
#' sun direction.
#' @param plane_origin \code{numeric}, \code{xyz}-vector of a point on the
#' ground.
#' @param plane_normal \code{numeric}, \code{xyz}-vector of the ground normal.
#' @param col \code{character}, color of the shade.
#' @param add \code{boolean}, add the plot to current active \code{rgl} plot.
#'
#' @return
#' \code{rgl} plot of the shade.
#'
#' @seealso \code{\link{add_geoms}}, \code{\link{plot_shade}}
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
#' # create dummy geom regression
#' distribution <- dummy_geom_distribution()
#'
#' # create polygons for single geom
#' flower <- create_flower()
#'
#' # create dummy geom regression
#' distribution <- dummy_geom_distribution()
#'
#' # add geoms
#' flowers <- add_geoms(qsm, distribution, flower, geom_type = "flower")
#'
#' # plot qsm
#' qsm2r::plot(qsm, col = "salmon4", lit = TRUE)
#'
#' # plot geoms
#' plot_geoms(flowers, col = "pink2")
#'
#' # plot shade of wood
#' plot_shade_qsm(qsm)
#'
#' # plot shade of geoms
#' plot_shade_geoms(flowers)
#'
#' # or:
#'
#' # create polygons for single geom
#' leaf <- create_leaf(type = "normal", length_m = 0.1)
#'
#' # add geoms
#' leaves <- add_geoms(qsm, distribution, leaf, geom_type = "leaf")
#'
#' # plot qsm
#' qsm2r::plot(qsm, col = "salmon4", lit = TRUE)
#'
#' # plot geoms
#' plot_geoms(leaves, col = "darkolivegreen3")
#'
#' # plot shade of wood
#' plot_shade_qsm(qsm)
#'
#' # plot shade of geoms
#' plot_shade_geoms(leaves)
#' @export
plot_shade_geoms <- function(geoms, sun_direction = c(0.25, 0.5, -0.75),
                             plane_origin = c(0,0,0), plane_normal = c(0,0,1),
                             col = "grey40", add = TRUE) {

  # remove geoms with NAs
  geoms <- na.omit(geoms)

  # prepare sun direction
  # https://doi.org/10.1080/713811744 (N- & S+)
  sun_direction[2] <- sun_direction[2] * (-1)

  # get shadows
  shade_geoms <- shade_geoms_comp(sun_direction = sun_direction, geoms = geoms, plane_origin = plane_origin, plane_normal = plane_normal)

  # plot shadows
  plot_shade(shade_geoms[[1]], col = col, add = add)
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
#' @param plane_normal \code{numeric}, \code{xyz}-vector of the ground normal.
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
#' @seealso \code{\link{plot_shade}}, \code{\link{plot_shade_geoms}}
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
#' plot_ground(plane_origin = ground_origin, plane_normal = ground_normal)
#'
#' # plot shade of wood
#' plot_shade_qsm(qsm, plane_origin = ground_origin, plane_normal = ground_normal)
#' @export
plot_ground <- function(plane_origin = c(0,0,0), plane_normal = c(0,0,1),
                        radius = 12, n_dir = 30L, z_offset = -0.005,
                        col = "#B6CC8F", add = TRUE, lit = TRUE) {

  # stop if not enough angles
  if (n_dir <= 3) stop("")

  # define angles at which points should be calculated
  angles <- deg2rad(seq(from = 0, to = 360, length.out = n_dir + 1))

  # change format
  plane_normal <- t(plane_normal)

  # define vectors on the plane
  vec_random <- plane_normal + c(1,0,0)
  vec_plane_1 <- simple_cross(plane_normal, vec_random)
  vec_plane_2 <- simple_cross(plane_normal, vec_plane_1)

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
