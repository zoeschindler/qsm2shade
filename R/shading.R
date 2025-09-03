################################################################################
# MAIN FUNCTIONS
################################################################################

#' Calculate sunlight position over time
#'
#' @description
#' \code{sun_movement} calculate the direction of the sunlight over time to be
#' used by \code{shade_tree_qsm()} or \code{shade_tree_geoms()}..
#'
#' @param timeframe \code{POSIXct}, vector with times.
#' @param latitude \code{numeric}, latitude of the tree position.
#' @param longitude \code{numeric}, longitude of the tree position.
#' @param timezone \code{integer}, timezone of time data.
#'
#' @return
#' \code{data.frame}, contains sunlight direction over the given time.
#'
#' @seealso \code{\link{shade_tree_qsm}}
#'
#' @examples
#' # get sun position at different times
#' timeframe <- seq(ISOdate(2020, 03, 22, 0, 0), ISOdate(2020, 03, 22, 23, 50), "10 mins")
#' sun_position <- sun_movement(timeframe, latitude = 48.07, longitude = 7.60)
#'
#' # display values
#' head(sun_position)
#' @export
sun_movement <- function(timeframe, latitude, longitude, timezone = 0) {

  # prepare data
  julianday <- insol::JD(timeframe)

  # calculates unit vector in the sun direction from the observer position
  position_xyz <- as.data.frame(insol::sunvector(jd = julianday, latitude = latitude, longitude = longitude, timezone = timezone))

  # calculates azimuth and zenith angles of the sun
  position_az <- as.data.frame(insol::sunpos(position_xyz))

  # combine data
  position <- cbind(position_xyz, position_az, julianday, timeframe,
                    day = position_az$zenith <= 90)

  # return positions
  return(position)
}

################################################################################

# calculate wood shadow polygons
shade_qsm <- function(sun_direction, tree, plane_origin, plane_normal,
                      res = NULL, xmin = NULL, xmax = NULL, ymin = NULL,
                      ymax = NULL, plot = FALSE) {

  # get cylinder vertices
  # https://www.nagwa.com/en/explainers/616184792816/
  top_m <- tree[,c("start_X", "start_Y", "start_Z")]
  bot_m <-  tree[,c("end_X", "end_Y", "end_Z")]
  sun_m <- matrix(sun_direction, ncol = 3, nrow = nrow(tree), byrow = TRUE)
  #
  cross <- norm_cross(sun_m, top_m - bot_m)
  q1 <- top_m + tree[,c("radius")] * cross
  q2 <- top_m - tree[,c("radius")] * cross
  #
  cross <- norm_cross(top_m - bot_m, q1 - q2)
  q3 <- top_m + tree[,c("radius")] * cross
  q4 <- top_m - tree[,c("radius")] * cross
  #
  cross <- norm_cross(sun_m, bot_m - top_m)
  q5 <- bot_m + tree[,c("radius")] * cross
  q6 <- bot_m - tree[,c("radius")] * cross
  #
  cross <- norm_cross(bot_m - top_m, q5 - q6)
  q7 <- bot_m + tree[,c("radius")] * cross
  q8 <- bot_m - tree[,c("radius")] * cross

  # store cylinder vertices
  cyl_pts <- matrix(c(q1, q2, q3, q4, q5, q6, q7, q8), ncol = 3 * 8)

  # change column names
  cyl_cols <- paste0(paste0("q", rep(1:(ncol(cyl_pts)/3), each = 3)), c("x", "y", "z"))
  colnames(cyl_pts) <- cyl_cols

  # get coordinate cols
  px_cols <- endsWith(cyl_cols, "x")
  py_cols <- endsWith(cyl_cols, "y")
  pz_cols <- endsWith(cyl_cols, "z")

  # calculate intersections between ground and light
  # line:       point + t * sun
  # plane:      normal * (point - origin) = 0
  # insert:     normal * (point + t * sun - origin) = 0
  # rearrange:  t = -(normal * (point - origin))/(normal * sun)
  times_sun <- (
    plane_normal[1] * (cyl_pts[,px_cols] - plane_origin[1]) +
      plane_normal[2] * (cyl_pts[,py_cols] - plane_origin[2]) +
      plane_normal[3] * (cyl_pts[,pz_cols] - plane_origin[3])) / c(plane_normal %*% sun_direction)
  projected <- matrix(c(
    1:nrow(cyl_pts),
    cyl_pts[,px_cols] - times_sun * sun_direction[1],
    cyl_pts[,py_cols] - times_sun * sun_direction[2],
    cyl_pts[,pz_cols] - times_sun * sun_direction[3]),
    ncol = 1 + sum(px_cols) * 3)
  colnames(projected) <- c("id", cyl_cols[px_cols], cyl_cols[py_cols], cyl_cols[pz_cols])

  # get columns with coordinates
  all_cols <- colnames(projected)
  x_cols <- endsWith(all_cols, "x")
  y_cols <- endsWith(all_cols, "y")
  z_cols <- endsWith(all_cols, "z")

  # get coordinates of convex hull
  conv_hulls <- lapply(1:nrow(projected), function(idx) {
    cp <- cbind(as.numeric(projected[idx, x_cols]),
                as.numeric(projected[idx, y_cols]),
                as.numeric(projected[idx, z_cols]))
    hullIdx <- chull(cp)
    cbind(id = idx, cp[c(hullIdx, hullIdx[1]),])
  })
  conv_hulls <- do.call(rbind, conv_hulls)
  colnames(conv_hulls) <- c("id", "x", "y", "z")

  # set z to exactly zero, if ground is flat
  if (all(plane_origin == c(0,0,0)) & all(plane_normal == c(0,0,1))) conv_hulls[,"z"] <- 0

  # return vectors with z value for plots
  if (plot) {
    return(list(conv_hulls))
  }

  # return rasters for calculation
  conv_hulls <- terra::vect(conv_hulls[,1:3], type = "polygons") |>
    terra::rasterize(terra::rast(
      nlyrs = 1, res = res, vals = 0, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), background = 0, fun = "count")

  # return geom raster
  return(terra::wrap(conv_hulls))
}

# compile function to make it faster
shade_qsm_comp <- compiler::cmpfun(shade_qsm)

################################################################################

# calculate shadows for geoms
shade_geoms <- function(sun_direction, geoms, plane_origin, plane_normal,
                        res = NULL, xmin = NULL, xmax = NULL, ymin = NULL,
                        ymax = NULL, plot = FALSE) {

  # calculate intersections between ground and light
  # line:       point + t * sun
  # plane:      normal * (point - origin) = 0
  # insert:     normal * (point + t * sun - origin) = 0
  # rearrange:  t = -(normal * (point - origin))/(normal * sun)
  times_sun <- (
    plane_normal[1] * (geoms[,2] - plane_origin[1]) +
      plane_normal[2] * (geoms[,3] - plane_origin[2]) +
      plane_normal[3] * (geoms[,4] - plane_origin[3])) / c(plane_normal %*% sun_direction)
  geoms[,2] <- geoms[,2] - times_sun * sun_direction[1] # x
  geoms[,3] <- geoms[,3] - times_sun * sun_direction[2] # y
  geoms[,4] <- geoms[,4] - times_sun * sun_direction[3] # z -> do we even need this?
  colnames(geoms) <- c("id", "x", "y", "z") # should be a matrix

  # return vectors with z value for plots
  if (plot) {
    return(list(geoms))
  }
  # return rasters for calculation
  geoms <- terra::vect(geoms[,1:3], type = "polygons") |>
    terra::rasterize(terra::rast(
      nlyrs = 1, res = res, vals = 0, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), background = 0, fun = "count")

  # return geom raster
  return(terra::wrap(geoms))
}

# compile function to make it faster
shade_geoms_comp <- compiler::cmpfun(shade_geoms)

################################################################################

#' Calculate shade of a QSM
#'
#' @description
#' \code{shade_tree_qsm} calculates the shade cast by a tree based on given sun
#' positions. The shade is coded as 0 and light is coded as 1. The function can
#' include the shade of modeled items (leaves, flowers) if provided. The
#' processing may be done sequentially or in parallel. An empty output raster
#' with the desired extent and resolution is required. Further, the transparency
#' of leaves may be configured. The ground can be specified via a point on the
#' plane and the plane normal. Per default, an even ground at the origin is
#' assumed.
#'
#' @param qsm An object of class \code{QSM}.
#' @param sun_position \code{data.frame}, sunlight direction over the time.
#' @param geoms \code{matrix}, contains coordinates of simulated leaves /
#' flowers.
#' @param empty_grid \code{SpatRaster}, with target shade raster location and
#' spatial resolution.
#' @param plane_origin \code{numeric}, \code{xyz}-vector of a point on the
#' ground.
#' @param plane_normal \code{numeric}, \code{xyz}-vector of the ground normal.
#' @param transparency \code{numeric}, transparency of the geom shade,
#' 1 = fully transparent, 0 = fully opaque.
#' @param sequential \code{boolean}, whether sequential (\code{TRUE}) or
#' parallel processing (\code{FALSE}) should be used.
#'
#' @return
#' \code{SpatRaster}, contains light / shade / in-between around the tree for
#' each \code{sun_position}. The single rasters for the time steps are stored as
#' layers with the timestamp as names.
#'
#' @details
#' The parameter \code{sun_position} determines for which time steps the shade
#' is calculated.
#'
#' @seealso \code{\link{shade_summarize}}, \code{\link{sun_movement}},
#' \code{\link{add_geoms}}, \code{\link{add_radiation}}
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
#' # get sun position at different times
#' timeframe <- seq(ISOdate(2020, 03, 22, 0, 0), ISOdate(2020, 03, 22, 23, 50), "10 mins")
#' sun_position <- sun_movement(timeframe, latitude = 48.07, longitude = 7.60)
#'
#' # create geoms for single geom
#' leaf <- create_leaf(type = "normal", length_m = 0.1)
#'
#' # get dummy geom distribution
#' distribution <- dummy_geom_distribution()
#'
#' # create items
#' leaves <- add_geoms(qsm, distribution, leaf, geom_type  = "leaf")
#'
#' # calculate shade
#' result <- shade_tree_qsm(qsm, sun_position = sun_position, geom_other = leaves, transparency = 0.7)
#'
#' # create dummy radiation data
#' radiation <- dummy_radiation(ISOdate(2020, 01, 01, 0, 0), ISOdate(2020, 12, 31, 23, 50), "1 hour")
#'
#' # add radiation data
#' result <- add_radiation(result, radiation)
#'
#' # summarize per day
#' result_daily <- shade_summarize(result, "day", "sum")
#'
#' # plot daily shade
#' terra::plot(result_daily)
#' @export
shade_tree_qsm <- function(
    qsm, sun_position, geom_other = NULL,
    empty_grid = terra::rast(
      nlyrs = 1, res = 0.1, vals = 0,
      xmin = min(qsm@cylinder$start_X) - 20, xmax = max(qsm@cylinder$start_X) + 20,
      ymin = min(qsm@cylinder$start_Y) - 20, ymax = max(qsm@cylinder$start_Y) + 20),
    plane_origin = c(median(qsm@cylinder$start_X), median(qsm@cylinder$start_Y), min(qsm@cylinder$start_Z)),
    plane_normal = c(0,0,1), transparency = 0, sequential = TRUE) {

  # check for other geoms
  if (is.null(geom_other)) {

    # get xy shift
    x_shift <- min(terra::xmin(empty_grid), min(qsm@cylinder$start_X))
    y_shift <- min(terra::ymin(empty_grid), min(qsm@cylinder$start_Y))
  } else {

    # prepare other geoms
    geom_other <- as.matrix(geom_other)

    # get xy shift
    x_shift <- min(terra::xmin(empty_grid), min(qsm@cylinder$start_X), min(geom_other[,2]))
    y_shift <- min(terra::ymin(empty_grid), min(qsm@cylinder$start_Y), min(geom_other[,3]))

    # shift other geoms
    geom_other[,2] <- geom_other[,2] - x_shift
    geom_other[,3] <- geom_other[,3] - y_shift
  }

  # shift data (to help with memory)
  qsm@cylinder$start_X <- qsm@cylinder$start_X - x_shift
  qsm@cylinder$start_Y <- qsm@cylinder$start_Y - y_shift
  plane_origin[1] <- plane_origin[1] - x_shift
  plane_origin[2] <- plane_origin[2] - y_shift
  empty_grid <- terra::shift(empty_grid, dx = -x_shift, dy = -y_shift)

  # prepare tree data
  tree <- prepare_qsm(qsm, keep_all = FALSE)

  # prepare sun data (get day data only)
  timestep <- unique(sun_position$timeframe[sun_position$day])
  sun_position$svy <- sun_position$svy * (-1) # https://doi.org/10.1080/713811744 (N- & S+)
  sun_direction <- t(sun_position[sun_position$day, 1:3])
  sun_position <- sun_position[order(sun_position$timeframe),]

  # get raster values
  res <- unique(terra::res(empty_grid))
  xmin <- terra::xmin(empty_grid)
  xmax <- terra::xmax(empty_grid)
  ymin <- terra::ymin(empty_grid)
  ymax <- terra::ymax(empty_grid)

  # check if there is a time where the sun is there
  if (ncol(sun_position) == 0) stop("there is no sun during the selected time") # check if this works

  # sequential processing
  if (sequential) {

    # calculate wood shadows
    message("... creating wood shadows")
    wood_rast_terra <- apply(
      sun_direction, 2, shade_qsm_comp, tree = tree,
      plane_origin = plane_origin, plane_normal = plane_normal, res = res,
      xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax) |> lapply(terra::unwrap)

    # calculate leaf shadows
    if (!is.null(geom_other)) {
      message("... creating leaf shadows")
      item_rast_terra <- apply(
        sun_direction, 2, shade_geoms_comp, geoms = geom_other,
        plane_origin = plane_origin, plane_normal = plane_normal, res = res,
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax) |> lapply(terra::unwrap)

    } else {
      item_rast_terra <- NULL
    }

  } else {
    # parallel processing
    message("... setting up parallel processing")

    # set up cluster
    numCores <- parallel::detectCores()
    cl <- parallel::makeCluster(numCores, type = "PSOCK")

    # export objects to cores
    parallel::clusterExport(cl, list(
      "tree", "geom_other", "sun_direction", "norm_cross", "plane_origin", "plane_normal",
      "res" ,"xmin", "xmax", "ymin", "ymax"),
      envir = environment())

    # calculate wood shadows
    message("... creating wood shadows")
    wood_rast_terra <- parallel::parApply(
      cl, sun_direction, 2, shade_qsm_comp, tree = tree,
      plane_origin = plane_origin, plane_normal = plane_normal, res = res,
      xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax) |> lapply(terra::unwrap)

    # calculate geom shadows
    if (!is.null(geom_other)) {
      message("... creating geom shadows")
      item_rast_terra <- parallel::parApply(
        cl, sun_direction, 2, shade_geoms_comp, geoms = geom_other,
        plane_origin = plane_origin, plane_normal = plane_normal, res = res,
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax) |> lapply(terra::unwrap)
    } else {
      item_rast_terra <- NULL
    }

    # stop the cluster
    parallel::stopCluster(cl)
  }

  # rasterize the polygons
  message("... deriving rasters")
  result_grid <- lapply(1:length(timestep), function(idx) {

    # combine & rasterize geoms
    if (!is.null(geom_other) & transparency > 0) { # opaque wood + transparent leaves
      curr_wood <- wood_rast_terra[[idx]]
      curr_item <- item_rast_terra[[idx]]
      curr_wood[curr_wood > 0] <- Inf
      polygon_grid <- curr_wood + curr_item

    } else {
      if (!is.null(geom_other) & transparency == 0) { # opaque wood + opaque leaves
        polygon_grid <- wood_rast_terra[[idx]] + item_rast_terra[[idx]]

      } else { # opaque wood
        polygon_grid <- wood_rast_terra[[idx]]
      }
      polygon_grid[polygon_grid > 0] <- Inf
    }

    # delete geoms
    wood_rast_terra[[idx]] <- terra::vect()
    if (!is.null(geom_other)) item_rast_terra[[idx]] <- terra::vect()

    # determine shade yes / no ( in-between)
    polygon_grid[polygon_grid != Inf] <- transparency ** polygon_grid
    polygon_grid[polygon_grid == Inf] <- 0

    # return raster
    return(polygon_grid)
  })

  # stack rasters
  result_grid <- terra::rast(result_grid)

  # set layer names
  names(result_grid) <- format(timestep,  format = "%Y-%m-%d %H:%M")

  # shift data
  result_grid <- terra::shift(result_grid, dx = +x_shift, dy = +y_shift)

  # return result
  return(result_grid)
}

################################################################################

#' Calculate shade of geoms
#'
#' @description
#' \code{shade_tree_geoms} calculates the shade cast by a tree based on given sun
#' positions. The shade is coded as 0 and light is coded as 1. The function can
#' include the shade of modeled items (leaves, flowers) if provided. The
#' processing may be done sequentially or in parallel. An empty output raster
#' with the desired extent and resolution is required. Further, the transparency
#' of leaves may be configured. The ground can be specified via a point on the
#' plane and the plane normal. Per default, an even ground at the origin is
#' assumed.
#'
#' @param geom_wood \code{matrix}, contains matrix with IDs and coordinates of
#' wood geoms.
#' @param sun_position \code{data.frame}, sunlight direction over the time.
#' @param geom_other \code{matrix}, contains matrix with IDs and coordinates of
#' leaf / flower geoms.
#' @param empty_grid \code{SpatRaster}, with target shade raster location and
#' spatial resolution.
#' @param plane_origin \code{numeric}, \code{xyz}-vector of a point on the
#' ground.
#' @param plane_normal \code{numeric}, \code{xyz}-vector of the ground normal.
#' @param transparency \code{numeric}, transparency of the geom shade,
#' 1 = fully transparent, 0 = fully opaque.
#' @param sequential \code{boolean}, whether sequential (\code{TRUE}) or
#' parallel processing (\code{FALSE}) should be used.
#'
#' @return
#' \code{SpatRaster}, contains light / shade / in-between around the tree for
#' each \code{sun_position}. The single rasters for the time steps are stored as
#' layers with the timestamp as names.
#'
#' @details
#' The parameter \code{sun_position} determines for which time steps the shade
#' is calculated.
#'
#' @seealso \code{\link{shade_summarize}}, \code{\link{sun_movement}}, \code{\link{add_radiation}}
#'
#' @examples
#' # load wood geoms
#' file_path <- system.file("extdata", "pear_wood.txt", package="qsm2shade")
#' geom_wood <- as.matrix(read.table(file_path, header = T))
#'
#' # load leaf geoms
#' file_path <- system.file("extdata", "pear_leaves.txt", package="qsm2shade")
#' geom_other <- as.matrix(read.table(file_path, header = T))
#'
#' # get sun position at different times
#' timeframe <- seq(ISOdate(2020, 03, 22, 0, 0), ISOdate(2020, 03, 22, 23, 50), "10 mins")
#' sun_position <- sun_movement(timeframe, latitude = 48.07, longitude = 7.60)
#'
#' # create dummy radiation data
#' radiation <- dummy_radiation(ISOdate(2020, 01, 01, 0, 0), ISOdate(2020, 12, 31, 23, 50), "1 hour")
#'
#' # calculate shade
#' result <- shade_tree_geoms(geom_wood, sun_position = sun_position, geom_other = geom_other, transparency = 0.7)
#'
#' # create dummy radiation data
#' radiation <- dummy_radiation(ISOdate(2020, 01, 01, 0, 0), ISOdate(2020, 12, 31, 23, 50), "1 hour")
#'
#' # add radiation data
#' result <- add_radiation(result, radiation)
#'
#' # summarize per day
#' result_daily <- shade_summarize(result, "day", "sum")
#'
#' # plot daily shade
#' terra::plot(result_daily)
#' @export
shade_tree_geoms <- function(
    geom_wood, sun_position, geom_other = NULL,
    empty_grid = terra::rast(
      nlyrs = 1, res = 0.1, vals = 0,
      xmin = min(geom_wood[,2]) - 20, xmax = max(geom_wood[,2]) + 20,
      ymin = min(geom_wood[,3]) - 20, ymax = max(geom_wood[,3]) + 20),
    plane_origin = c(median(geom_wood[,2]), median(geom_wood[,3]), min(geom_wood[,4])),
    plane_normal = c(0,0,1), transparency = 0, sequential = TRUE) {

  # check for other geoms
  if (is.null(geom_other)) {

    # get xy shift
    x_shift <- min(terra::xmin(empty_grid), min(geom_wood[,2]))
    y_shift <- min(terra::ymin(empty_grid), min(geom_wood[,3]))
  } else {

    # get xy shift
    x_shift <- min(terra::xmin(empty_grid), min(geom_wood[,2]), min(geom_other[,2]))
    y_shift <- min(terra::ymin(empty_grid), min(geom_wood[,3]), min(geom_other[,3]))

    # shift other geoms
    geom_other[,2] <- geom_other[,2] - x_shift
    geom_other[,3] <- geom_other[,3] - y_shift
  }

  # shift data (to help with memory)
  geom_wood[,2] <- geom_wood[,2] - x_shift
  geom_wood[,3] <- geom_wood[,3] - y_shift
  plane_origin[1] <- plane_origin[1] - x_shift
  plane_origin[2] <- plane_origin[2] - y_shift
  empty_grid <- terra::shift(empty_grid, dx = -x_shift, dy = -y_shift)

  # prepare sun data (get day data only)
  timestep <- unique(sun_position$timeframe[sun_position$day])
  sun_position$svy <- sun_position$svy * (-1) # https://doi.org/10.1080/713811744 (N- & S+)
  sun_direction <- t(sun_position[sun_position$day, 1:3])
  sun_position <- sun_position[order(sun_position$timeframe),]

  # get raster values
  res <- unique(terra::res(empty_grid))
  xmin <- terra::xmin(empty_grid)
  xmax <- terra::xmax(empty_grid)
  ymin <- terra::ymin(empty_grid)
  ymax <- terra::ymax(empty_grid)

  # check if there is a time where the sun is there
  if (ncol(sun_position) == 0) stop("there is no sun during the selected time") # check if this works

  # sequential processing
  if (sequential) {

    # calculate wood shadows
    message("... creating wood shadows")
    wood_rast_terra <- apply(
      sun_direction, 2, shade_geoms_comp, geoms = geom_wood,
      plane_origin = plane_origin, plane_normal = plane_normal, res = res,
      xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax) |> lapply(terra::unwrap)

    # calculate leaf shadows
    if (!is.null(geom_other)) {
      message("... creating leaf shadows")
      item_rast_terra <- apply(
        sun_direction, 2, shade_geoms_comp, geoms = geom_other,
        plane_origin = plane_origin, plane_normal = plane_normal, res = res,
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax) |> lapply(terra::unwrap)

    } else {
      item_rast_terra <- NULL
    }

  } else {
    # parallel processing
    message("... setting up parallel processing")

    # set up cluster
    numCores <- parallel::detectCores()
    cl <- parallel::makeCluster(numCores, type = "PSOCK")

    # export objects to cores
    parallel::clusterExport(cl, list(
      "geom_wood", "geom_other", "sun_direction", "norm_cross", "plane_origin",
      "plane_normal", "res" ,"xmin", "xmax", "ymin", "ymax"),
      envir = environment())

    # calculate wood shadows
    message("... creating wood shadows")
    wood_rast_terra <- parallel::parApply(
      cl, sun_direction, 2, shade_geoms_comp, geoms = geom_wood,
      plane_origin = plane_origin, plane_normal = plane_normal, res = res,
      xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax) |> lapply(terra::unwrap)

    # calculate geom shadows
    if (!is.null(geom_other)) {
      message("... creating geom shadows")
      item_rast_terra <- parallel::parApply(
        cl, sun_direction, 2, shade_geoms_comp, geoms = geom_other,
        plane_origin = plane_origin, plane_normal = plane_normal, res = res,
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax) |> lapply(terra::unwrap)
    } else {
      item_rast_terra <- NULL
    }

    # stop the cluster
    parallel::stopCluster(cl)
  }

  # rasterize the polygons
  message("... deriving rasters")
  result_grid <- lapply(1:length(timestep), function(idx) {

    # combine & rasterize geoms
    if (!is.null(geom_other) & transparency > 0) { # opaque wood + transparent leaves
      curr_wood <- wood_rast_terra[[idx]]
      curr_item <- item_rast_terra[[idx]]
      curr_wood[curr_wood > 0] <- Inf
      polygon_grid <- curr_wood + curr_item

    } else {
      if (!is.null(geom_other) & transparency == 0) { # opaque wood + opaque leaves
        polygon_grid <- wood_rast_terra[[idx]] + item_rast_terra[[idx]]

      } else { # opaque wood
        polygon_grid <- wood_rast_terra[[idx]]
      }
      polygon_grid[polygon_grid > 0] <- Inf
    }

    # delete geoms
    wood_rast_terra[[idx]] <- terra::vect()
    if (!is.null(geom_other)) item_rast_terra[[idx]] <- terra::vect()

    # determine shade yes / no ( in-between)
    polygon_grid[polygon_grid != Inf] <- transparency ** polygon_grid
    polygon_grid[polygon_grid == Inf] <- 0

    # return raster
    return(polygon_grid)
  })

  # stack rasters
  result_grid <- terra::rast(result_grid)

  # set layer names
  names(result_grid) <- format(timestep,  format = "%Y-%m-%d %H:%M")

  # shift data
  result_grid <- terra::shift(result_grid, dx = +x_shift, dy = +y_shift)

  # return result
  return(result_grid)
}

################################################################################

#' Add radiation values to simulated shade
#'
#' @description
#' \code{add_radiation} adds radiation values to the rasters produced by
#' \code{shade_tree_qsm()} or \code{shade_tree_geoms()}.
#'
#' @param raster \code{SpatRaster}, stacked set of shade rasters from
#' \code{shade_tree_qsm()} or \code{shade_tree_geoms()}.
#' @param radiation \code{data.frame}, diffuse and global radiation over time,
#' must be formatted as the example from \code{dummy_radiation()}.
#'
#' @return
#' \code{SpatRaster}, contains radiation for each time period, the
#' layer names indicate the respective time period.
#'
#' @seealso \code{\link{shade_tree_qsm}}, \code{\link{shade_tree_geoms}}, \code{\link{shade_summarize}}
#'
#' @details
#' The spatial resolution of \code{empty_grid} should correspond
#' to the unit used in \code{radiation}, e.g. when using a resolution of 0.1,
#' the radiation should be given per dm². If the raster cell contains any
#' shade, the diffuse radiation is assigned. Otherwise, the global radiation is
#' assigned. Turning parallel processing on (\code{sequential = FALSE}) might
#' only work for windows computers.
#'
#' @examples
#' # load wood geoms
#' file_path <- system.file("extdata", "pear_wood.txt", package="qsm2shade")
#' geom_wood <- as.matrix(read.table(file_path, header = T))
#'
#' # load leaf geoms
#' file_path <- system.file("extdata", "pear_leaves.txt", package="qsm2shade")
#' geom_other <- as.matrix(read.table(file_path, header = T))
#'
#' # get sun position at different times
#' timeframe <- seq(ISOdate(2020, 03, 22, 0, 0), ISOdate(2020, 03, 22, 23, 50), "10 mins")
#' sun_position <- sun_movement(timeframe, latitude = 48.07, longitude = 7.60)
#'
#' # calculate shade
#' result <- shade_tree_geoms(geom_wood, sun_position = sun_position, geom_other = geom_other, transparency = 0.7)
#'
#' # create dummy radiation data
#' radiation <- dummy_radiation(ISOdate(2020, 01, 01, 0, 0), ISOdate(2020, 12, 31, 23, 50), "1 hour")
#'
#' # add radiation data to shade polygons
#' result <- add_radiation(result, radiation)
#'
#' # show results
#' print(result)
#' @export
add_radiation <- function(raster, radiation) {

  # get timesteps
  timesteps <- as.POSIXct(names(raster), tz = "UTC")

  # prepare radiation data
  rad_interval <- as.numeric(names(which.max(table(difftime(
    radiation$timestamp[2:nrow(radiation)],
    radiation$timestamp[1:(nrow(radiation) - 1)],
    units = "secs")))))

  # get temporal resolution
  raster_interval <- as.numeric(names(which.max(table(difftime(
    timesteps[2:length(timesteps)],
    timesteps[1:(length(timesteps) - 1)],
    units = "secs")))))

  # get factor by which the energy has to be divided
  if (raster_interval == 0) {
    rad_factor <- 1
  } else {
    rad_factor <- rad_interval / raster_interval
    rad_factor <- ifelse(is.na(rad_factor), 1, rad_factor)
  }

  # prepare radiation data
  radiation$direct_energy_per_area   <- radiation$global_energy_per_area - radiation$diffuse_energy_per_area
  radiation$direct_energy_per_area   <- radiation$direct_energy_per_area  / rad_factor
  radiation$diffuse_energy_per_area  <- radiation$diffuse_energy_per_area / rad_factor
  radiation$global_energy_per_area   <- radiation$global_energy_per_area  / rad_factor
  lubridate::tz(radiation$timestamp) <- "UTC"

  # loop through time steps
  radiation_grid <- lapply(1:length(timesteps), function(idx) {

    # calculate radiation
    curr_rad  <- radiation[radiation$timestamp == lubridate::ceiling_date(timesteps[idx], paste(rad_interval, "aseconds")),]
    curr_rast <- raster[[idx]]
    curr_rast<- curr_rad[,"diffuse_energy_per_area"] + curr_rad[,"direct_energy_per_area"] * curr_rast
    return(curr_rast)
  })

  # stack rasters
  radiation_grid <- terra::rast(radiation_grid)

  # return radiation raster
  return(radiation_grid)
}

################################################################################

#' Create dummy radiation data
#'
#' @description
#' \code{dummy_radiation} creates dummy radiation data to be used
#' by \code{add_radiation()}.
#'
#' @param start \code{POSIXct}, start time of the radiation data.
#' @param end \code{POSIXct}, end time of the radiation data.
#' @param interval \code{character}, time interval.
#'
#' @return
#' \code{data.frame}, contains diffuse and global radiation over time.
#'
#' @seealso \code{\link{add_radiation}}
#'
#' @examples
#' # create dummy radiation data
#' radiation <- dummy_radiation(ISOdate(2020, 01, 01, 0, 0), ISOdate(2020, 12, 31, 23, 50), "1 hour")
#'
#' # display values
#' head(radiation)
#' @export
dummy_radiation <- function(
    start = ISOdate(2020, 01, 01, 0, 0),
    end = ISOdate(2020, 12, 31, 23, 50),
    interval = "1 hour") {

  # create dummy data
  timestamp <- seq(start, end, by = interval)
  diffuse_energy_per_area <- runif(length(timestamp), 0 , 5)
  global_energy_per_area <- diffuse_energy_per_area * runif(length(timestamp), 1, 2)

  # combine data
  dummy <- data.frame(timestamp, diffuse_energy_per_area, global_energy_per_area)

  # set 6pm to 6am to zero (night)
  dummy[lubridate::hour(timestamp) < 6 & lubridate::hour(timestamp) >= 18,
        c("diffuse_energy_per_area", "global_energy_per_area")] <- 0

  # return dummy data
  return(dummy)
}

################################################################################

#' Summarize shade per period
#'
#' @description
#' \code{shade_summarize} summarizes the rasters produced by
#' \code{shade_tree_qsm()} or \code{shade_tree_geoms()} in variable time
#' periods.
#'
#' @param radiation_grid \code{SpatRaster}, stacked set of shade rasters from
#' \code{shade_tree_qsm()} or \code{shade_tree_geoms()}.
#' @param period \code{character}, length of summary period, either \code{"hour"},
#' \code{"day"}, \code{"month"}, \code{"year"} or \code{"total"}.
#' @param type \code{character}, type of summary statistic, either \code{"sum"},
#' \code{"mean"}, \code{"min"} or \code{"max"}.
#' @param na.rm \code{boolean}, whether NAs should be removed prior to the
#' calculation.
#'
#' @return
#' \code{SpatRaster}, stacked shade rasters summarized by time periods, the
#' layer names indicate the respective time period.
#'
#' @seealso \code{\link{shade_tree_qsm}}, \code{\link{shade_tree_geoms}}, \code{\link{add_radiation}}
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
#' # get sun position at different times
#' timeframe <- seq(ISOdate(2020, 03, 22, 0, 0), ISOdate(2020, 03, 22, 23, 50), "10 mins")
#' sun_position <- sun_movement(timeframe, latitude = 48.07, longitude = 7.60)
#'
#' # calculate shade
#' result <- shade_tree_qsm(qsm, sun_position = sun_position)
#'
#' # create dummy radiation data
#' radiation <- dummy_radiation(ISOdate(2020, 01, 01, 0, 0), ISOdate(2020, 12, 31, 23, 50), "1 hour")
#'
#' # add radiation data
#' result <- add_radiation(result, radiation)
#'
#' # summarize per day, sum
#' daily_sum <- shade_summarize(result, "day", "sum")
#' terra::plot(daily_sum)
#'
#' # summarize everything, average
#' total_mean <- shade_summarize(result, "total", "mean")
#' terra::plot(total_mean)
#' @export
shade_summarize <- function(radiation_grid, period = c("hour", "day", "month", "year", "total"), type = c("sum", "mean", "min", "max"), na.rm = TRUE) {

  # check input validity
  if (length(period) > 1 | !any(period %in% c("hour", "day", "month", "year", "total"))) {
    message("period must be 'hour', day', 'month', 'year' or 'total'")
    message("defaulting to 'total'")
    period <- "total"
  }

  if (length(type) > 1 | !any(type %in% c("sum", "mean", "min", "max"))) {
    message("type must be 'sum', mean', 'min', or 'max'")
    message("defaulting to 'sum'")
    type <- "sum"
  }

  # extract times from raster names
  timestep <- as.POSIXct(strptime(names(radiation_grid), "%Y-%m-%d %H:%M"))

  # add everything together
  if (period == "total") {
    if (type == "mean") {
      summarized <- terra::mean(radiation_grid, na.rm = na.rm)
    } else {
      summarized <- do.call(type, list(radiation_grid, na.rm = na.rm))
    }
    names(summarized) <- paste0(period, ", ", type)
    return(summarized)
  }

  # summarize depending on period
  period_all <- sort(unique(lubridate::floor_date(timestep, period)))
  summarized <- apply(matrix(period_all), 1, function(period_curr) {
    period_layers <- lubridate::floor_date(timestep, period) == period_curr
    if (type == "mean") {
      period_grid <- terra::mean(radiation_grid[[period_layers]], na.rm = na.rm)
    } else {
      period_grid <- do.call(type, list(radiation_grid[[period_layers]], na.rm = na.rm))
    }
    return(period_grid)})
  summarized <- terra::rast(summarized)

  # rename depending on period
  format_dict <- list("hour" = "%Y-%m-%d %H", "day" = "%Y-%m-%d", "month" = "%Y-%m", "year" = "%Y")
  names(summarized) <- format(period_all,  format = format_dict[[period]])

  # return summarized data
  return(summarized)
}

################################################################################

#' Merge shade of multiple trees
#'
#' @description
#' \code{shade_merge} merges single or summarised rasters produced by
#' \code{shade_tree_qsm()} or \code{shade_tree_geoms()}.
#'
#' @param rasters \code{list}, single or summarised rasters from
#' \code{shade_tree_qsm()} or \code{shade_tree_geoms()}.
#' @param resolution \code{numeric}, spatial resolution of input rasters.
#' @param background \code{numeric}, desired raster value of regions covered by
#' none of the input rasters, shade is coded as 0 and light is coded as 1.
#'
#' @return
#' \code{SpatRaster}, contains light / shade / in-between for each time period,
#' the layer names indicate the respective time period.
#'
#' @seealso \code{\link{shade_tree_qsm}}, \code{\link{shade_tree_geoms}}, \code{\link{add_radiation}}
#'
#' @examples
#' # load wood geoms
#' file_path <- system.file("extdata", "pear_wood.txt", package="qsm2shade")
#' geom_wood <- as.matrix(read.table(file_path, header = T))
#'
#' # load leaf geoms
#' file_path <- system.file("extdata", "pear_leaves.txt", package="qsm2shade")
#' geom_other <- as.matrix(read.table(file_path, header = T))
#'
#' # get sun position at different times
#' timeframe <- seq(ISOdate(2020, 03, 22, 10, 0), ISOdate(2020, 03, 22, 13, 50), "10 mins")
#' sun_position <- sun_movement(timeframe, latitude = 48.07, longitude = 7.60)
#'
#' # create dummy radiation data
#' radiation <- dummy_radiation(ISOdate(2020, 01, 01, 0, 0), ISOdate(2020, 12, 31, 23, 50), "1 hour")
#'
#' # calculate shade
#' result <- shade_tree_geoms(geom_wood, sun_position = sun_position, geom_other = geom_other, transparency = 0.7)
#'
#' # summarize per day
#' summary_a <- shade_summarize(result, "day", "mean")
#'
#' # move raster to simulate more trees
#' summary_b <- terra::shift(summary_a, dx = 13, dy = 5)
#' summary_c <- terra::shift(summary_a, dx = -7, dy = -3)
#'
#' # merge rasters
#' summary_merged <- qsm2shade:::shade_merge(list(summary_a, summary_b, summary_c))
#'
#' # show results
#' terra::plot(summary_merged)
#'
#' # set radiation values
#' diffuse <- 0.7
#' direct  <- 1.3
#'
#' # add radiation
#' summary_merged_radiation <- diffuse + direct * summary_merged
#'
#' # show results
#' terra::plot(summary_merged_radiation)
#' @export
shade_merge <- function(rasters, resolution = unique(terra::res(rasters[[1]])),
                        background = 1) {

  # create empty template
  ext_all <- Reduce(terra::union, lapply(rasters, terra::ext))
  template <- terra::rast(ext_all, res = resolution)

  # extend rasters to same extent and set background to 1 (sun)
  rasters <- lapply(rasters, function(r) {
    r <- terra::extend(r, template)
    r[is.na(r)] <- background
    return(r)})

  # stack rasters as layers
  rasters <- terra::rast(rasters)

  # multiply rasters with each other (shade = 0, light = 1)
  rasters <- prod(rasters)

  # return merged raster
  return(rasters)
}

################################################################################
