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
plot_single_item <- function(item_poly, col = "pink2") {

  # convert polygon to list of polygons if it isn't
  if (!any("list" %in% class(item_poly))) {
    item_poly <- list(item_poly)
  }

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
#'
#' @return
#' \code{rgl} plot of all items.
#'
#' @seealso \code{\link{add_item}}, \code{\link{plot_shade_items}}
#'
#' @examples
#' # load qsm
#' file_path <- system.file("extdata", "Prunus_avium_QSM_simplified.mat", package="qsm2shade")
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
plot_items <- function(item_pts, col, add = TRUE) {

  # remove items with NAs
  item_pts <- na.omit(item_pts)

  # open new window
  if (!add) {
    rgl::open3d()
  }

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
  rgl::shade3d(rgl::shapelist3d(item_rgl, plot = FALSE), lit = TRUE)
}

################################################################################

plot_shade <- function(shade, col = "grey", add = TRUE) {

  # open new window
  if (!add) {
    rgl::open3d()
  }

  # plot shade
  shade_rgl <- lapply(unique(shade[,1]), function(i) {
    subset <- shade[shade[,1] == i,]
    poly_i <- rgl::polygon3d(
      x = subset[,2],
      y = subset[,3],
      z = rep(0, nrow(subset)),
      lit = FALSE,
      plot = FALSE)
    poly_i$material$color <- "grey"
    poly_i
  })
  rgl::shade3d(rgl::shapelist3d(shade_rgl, plot = FALSE), lit = FALSE)
}

################################################################################

#' Plot shadows cast by wood
#'
#' @description
#' \code{plot_shade_wood} plots shadows of a \code{QSM} object.
#'
#' @param qsm An object of class \code{QSM}.
#' @param sun_direction \code{numeric}, vector containing the unit vector of the
#' sun direction.
#' @param col \code{character}, color of the shade
#' @param add \code{boolean}, add the plot to current active \code{rgl} plot.
#'
#' @return
#' \code{rgl} plot of the shade.
#'
#' @seealso \code{\link{plot_shade_items}}
#'
#' @examples
#' # load qsm
#' file_path <- system.file("extdata", "Prunus_avium_QSM_simplified.mat", package="qsm2shade")
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
plot_shade_wood <- function(qsm, sun_direction = c(0.25, 0.5, -0.75), col = "grey", add = TRUE) {

  # prepare qsm
  tree <- prepare_qsm(qsm, keep_all = FALSE)

  # get shadows
  shade_wood <- shade_wood(sun_direction = sun_direction, tree = tree)
  shade_wood <- terra::geom(terra::unwrap(shade_wood))[,c("geom", "x", "y")]

  # plot shadows
  plot_shade(shade_wood, col = col, add = add)
}

################################################################################

#' Plot shadows cast by items
#'
#' @description
#' \code{plot_shade_items} plots shadows of items simulated by
#' \code{add_item()}.
#'
#' @param item_pts \code{matrix}, contains coordinates of simulated items.
#' @param sun_direction \code{numeric}, vector containing the unit vector of the
#' sun direction.
#' @param col \code{character}, color of the shade
#' @param add \code{boolean}, add the plot to current active \code{rgl} plot.
#'
#' @return
#' \code{rgl} plot of the shade.
#'
#' @seealso \code{\link{add_item}}, \code{\link{plot_shade}}
#'
#' @examples
#' # load qsm
#' file_path <- system.file("extdata", "Prunus_avium_QSM_simplified.mat", package="qsm2shade")
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
plot_shade_items <- function(item_pts, sun_direction = c(0.25, 0.5, -0.75), col = "grey", add = TRUE) {

  # remove items with NAs
  item_pts <- na.omit(item_pts)

  # get shadows
  shade_items <- shade_items(sun_direction = sun_direction, item_pts = item_pts)
  shade_items <- terra::geom(terra::unwrap(shade_items))[,c("geom", "x", "y")]

  # plot shadows
  plot_shade(shade_items, col = col, add = add)
}

################################################################################
