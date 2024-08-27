################################################################################
# MAIN FUNCTIONS
################################################################################

#' Create dummy radiation data
#'
#' @description
#' \code{dummy_radiation} creates dummy radiation data to be used
#' by \code{shade_tree()}.
#'
#' @param start \code{POSIXct}, start time of the radiation data.
#' @param end \code{POSIXct}, end time of the radiation data.
#' @param interval \code{character}, time interval.
#'
#' @return
#' \code{data.frame}, contains diffuse and global radiation over time.
#'
#' @seealso \code{\link{shade_tree}}
#'
#' @examples
#' # create dummy radiation data
#' radiation <- dummy_radiation(ISOdate(2020, 01, 01, 0, 0), ISOdate(2020, 12, 31, 23, 50), "1 hour")
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

#' Calculate sunlight position over time
#'
#' @description
#' \code{sun_movement} calculate the direction of the sunlight over time to be
#' used by \code{shade_tree()}.
#'
#' @param timeframe \code{POSIXct}, vector with times.
#' @param latitude \code{numeric}, latitude of the tree position.
#' @param longitude \code{numeric}, longitude of the tree position.
#' @param timezone \code{integer}, timezone of time data.
#'
#' @return
#' \code{data.frame}, contains sunlight direction over the given time.
#'
#' @seealso \code{\link{shade_tree}}
#'
#' @examples
#' # get sun position at different times
#' timeframe <- seq(ISOdate(2020, 03, 22, 0, 0), ISOdate(2020, 03, 22, 23, 50), "10 mins")
#' sun_position <- sun_movement(timeframe, latitude = 48.07, longitude = 7.60)
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
shade_qsm <- function(sun_direction, tree, plane_origin, plane_normal) {

  # get cylinder vertices
  # https://www.nagwa.com/en/explainers/616184792816/
  top_m <- tree[,c("start_X", "start_Y", "start_Z")]
  bot_m <-  tree[,c("end_X", "end_Y", "end_Z")]
  sun_m <- matrix(sun_direction, ncol = 3, nrow = nrow(tree), byrow = TRUE)
  #
  cross <- qsm2shade:::norm_cross(sun_m, top_m - bot_m)
  q1 <- top_m + tree[,c("radius")] * cross
  q2 <- top_m - tree[,c("radius")] * cross
  #
  cross <- qsm2shade:::norm_cross(top_m - bot_m, q1 - q2)
  q3 <- top_m + tree[,c("radius")] * cross
  q4 <- top_m - tree[,c("radius")] * cross
  #
  cross <- qsm2shade:::norm_cross(sun_m, bot_m - top_m)
  q5 <- bot_m + tree[,c("radius")] * cross
  q6 <- bot_m - tree[,c("radius")] * cross
  #
  cross <- qsm2shade:::norm_cross(bot_m - top_m, q5 - q6)
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

  # return polygons
  return(list(conv_hulls))
}

# compile function to make it faster
shade_qsm_comp <- compiler::cmpfun(shade_qsm)

################################################################################

# calculate shadows for geoms
shade_geoms <- function(sun_direction, geoms, plane_origin, plane_normal) {

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
  geoms[,4] <- geoms[,4] - times_sun * sun_direction[3] # z
  colnames(geoms) <- c("id", "x", "y", "z") # should be a matrix

  # return geoms
  return(list(geoms))
}

# compile function to make it faster
shade_geoms_comp <- compiler::cmpfun(shade_geoms)

################################################################################

#' Calculate shade of a QSM
#'
#' @description
#' \code{shade_tree} calculates the shade cast by a tree based on given sun
#' positions. The radiation within this time frame may be specified, otherwise,
#' shade is coded as 0 and light is coded as 1. The function can include the
#' shade of modeled items (leaves, flowers) if provided. The processing may be
#' done sequentially or in parallel. For the output rasters, the resolution and
#' extent is required. Further, the transparency of leaves may be configured.
#' The ground can be specified via a point on the plane and the plane normal.
#' Per default, an even ground at the origin is assumed.
#'
#' @param qsm An object of class \code{QSM}.
#' @param sun_position \code{data.frame}, sunlight direction over the time.
#' @param radiation \code{data.frame}, diffuse and global radiation over time,
#' must be formatted as the example from \code{dummy_radiation()}, leave empty
#' to obtain only light intensity.
#' @param geoms \code{matrix}, contains coordinates of simulated leaves /
#' flowers.
#' @param sequential \code{boolean}, whether sequential (\code{TRUE}) or
#' parallel processing (\code{FALSE}) should be used.
#' @param resolution \code{numeric}, spatial resolution of output rasters.
#' @param xmin,xmax,ymin,ymax \code{numeric}, extent of the shading raster in
#' meters, relative to the stem base.
#' @param transparency \code{numeric}, transparency of the item shade,
#' 1 = fully transparent, 0 = fully opaque.
#' @param plane_origin \code{numeric}, \code{xyz}-vector of a point on the
#' ground.
#' @param plane_normal \code{numeric}, \code{xyz}-vector of the ground normal.
#'
#' @return
#' \code{SpatRaster}, contains radiation around the tree for each
#' \code{sun_position}, the unit of the radiation depends on the unit used in
#' \code{radiation}. The single rasters for the time steps are stored as
#' layers with the timestamp as names.
#'
#' @details
#' The parameter \code{sun_position} determines for which time steps the shade
#' is calculated. The \code{resolution} of the shade raster should correspond to
#' the unit used in \code{radiation}, e.g. when using a resolution of 0.1,
#' the radiation should be given per dm². If the raster cell contains any
#' shade, the diffuse radiation is assigned. Otherwise, the global radiation is
#' assigned. Turning parallel processing on (\code{sequential = FALSE}) might
#' only work for windows computers.
#'
#' @seealso \code{\link{shade_summarize}}, \code{\link{sun_movement}},
#' \code{\link{add_geoms}}
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
#' # create dummy radiation data
#' radiation <- dummy_radiation(ISOdate(2020, 01, 01, 0, 0), ISOdate(2020, 12, 31, 23, 50), "1 hour")
#'
#' # without leaves:
#'
#' # calculate shade
#' result <- shade_tree(qsm, sun_position, radiation)
#'
#' # show shade
#' plot(result)
#'
#' # with leaves:
#'
#' # create geoms for single item
#' leaf <- create_leaf(type = "normal", length_m = 0.1)
#'
#' # get dummy item distribution
#' distribution <- dummy_item_distribution()
#'
#' # create items
#' leaves <- add_geoms(qsm, distribution, leaf, item_type = "leaf")
#'
#' # calculate shade
#' result <- shade_tree(qsm, sun_position, geoms = leaves, radiation = radiation)
#'
#' # summarize per day
#' result_daily <- shade_summarize(result, "day")
#'
#' # plot daily shade
#' terra::plot(result_daily)
#' @export
shade_tree <- function(
    qsm, sun_position, geoms = NULL, radiation = NULL, resolution = 0.1,
    sequential = TRUE, xmin = -20, xmax = 20, ymin = -20, ymax = 20,
    transparency = 0, plane_origin = c(0,0,0), plane_normal = c(0,0,1)) {

  # prepare tree data
  tree <- qsm2shade:::prepare_qsm(qsm, keep_all = FALSE)

  # prepare sun data (get day data only)
  timestep <- unique(sun_position$timeframe[sun_position$day])
  sun_position$svy <- sun_position$svy * (-1) # https://doi.org/10.1080/713811744 (N- & S+)
  sun_direction <- t(sun_position[sun_position$day, 1:3])
  sun_position <- sun_position[order(sun_position$timeframe),]

  # check if there is a time where the sun is there
  if (ncol(sun_position) == 0) stop("there is no sun during the selected time") # check if this works

  # prepare radiation data
  if (!is.null(radiation)) {

    # get temporal resolution from sun_position
    sun_interval <- as.numeric(names(which.max(table(difftime(
      sun_position$timeframe[2:nrow(sun_position)],
      sun_position$timeframe[1:(nrow(sun_position) - 1)],
      units = "secs")))))

    # get temporal resolution from radiation
    rad_interval <- as.numeric(names(which.max(table(difftime(
      radiation$timestamp[2:nrow(radiation)],
      radiation$timestamp[1:(nrow(radiation) - 1)],
      units = "secs")))))

    # get factor by which the energy has to be divided
    if (sun_interval == 0) {
      rad_factor <- 1
    } else {
      rad_factor <- rad_interval / sun_interval
      rad_factor <- ifelse(is.na(rad_factor), 1, rad_factor)
    }

    # prepare radiation data
    radiation$direct_energy_per_area  <- radiation$global_energy_per_area - radiation$diffuse_energy_per_area
    radiation$direct_energy_per_area  <- radiation$direct_energy_per_area  / rad_factor
    radiation$diffuse_energy_per_area <- radiation$diffuse_energy_per_area / rad_factor
    radiation$global_energy_per_area  <- radiation$global_energy_per_area  / rad_factor
  }

  # sequential processing
  if (sequential) {

    # calculate wood shadows
    message("... creating wood shadows")
    wood_poly_terra <- apply(
      sun_direction, 2, qsm2shade:::shade_qsm_comp, tree = tree,
      plane_origin = plane_origin, plane_normal = plane_normal) |> lapply(qsm2shade:::list_polygonize)

    # calculate item shadows
    if (!is.null(geoms)) {
      message("... creating item shadows")
      item_poly_terra <- apply(
        sun_direction, 2, qsm2shade:::shade_geoms_comp, geoms = geoms,
        plane_origin = plane_origin, plane_normal = plane_normal) |> lapply(qsm2shade:::list_polygonize)

    } else {
      item_poly_terra <- NULL
    }

  } else {
    # parallel processing

    # set up cluster
    numCores <- parallel::detectCores()
    cl <- parallel::makeCluster(numCores, type = "PSOCK")

    # necessary packages
    packages_cl <- list("data.table")

    # export objects to cores
    parallel::clusterExport(cl, list(
      "packages_cl", "tree", "geoms", "sun_direction", "norm_cross", "plane_origin", "plane_normal"),
      envir = environment())

    # execute on all cores
    parallel::clusterEvalQ(cl, {

      # load packages
      lapply(packages_cl, require, character.only = T)
    })

    # calculate wood shadows
    message("... creating wood shadows")
    wood_poly_terra <- parallel::parApply(
      cl, sun_direction, 2, qsm2shade:::shade_qsm_comp, tree = tree,
      plane_origin = plane_origin, plane_normal = plane_normal) |> lapply(qsm2shade:::list_polygonize)

    # calculate item shadows
    if (!is.null(geoms)) {
      message("... creating item shadows")
      item_poly_terra <- parallel::parApply(
        cl, sun_direction, 2, qsm2shade:::shade_geoms_comp, geoms = geoms,
        plane_origin = plane_origin, plane_normal = plane_normal) |> lapply(qsm2shade:::list_polygonize)
    } else {
      item_poly_terra <- NULL
    }

    # stop the cluster
    parallel::stopCluster(cl)
  }

  # prepare empty raster for storage
  message("... deriving rasters")
  stem_base <- qsm2r::get_location(qsm)
  empty_grid <- terra::rast(nlyrs = 1, res = resolution, vals = 0,
                            xmin = stem_base[["x"]] + xmin,
                            xmax = stem_base[["x"]] + xmax,
                            ymin = stem_base[["y"]] + ymin,
                            ymax = stem_base[["y"]] + ymax)

  # rasterize the polygons
  radiation_grid <- apply(matrix(1:length(timestep)), 1, function(idx) {

    # combine & rasterize polygons
    if (!is.null(geoms) & transparency > 0) { # opaque wood + transparent leaves
      curr_wood <- wood_poly_terra[[idx]]
      curr_item <- item_poly_terra[[idx]]
      curr_wood$transparent <- Inf
      curr_item$transparent <- 1
      poly_terra <- rbind(curr_wood, curr_item)
      polygon_grid <- terra::rasterize(poly_terra, empty_grid, field = "transparent", fun = "sum", background = 0, na.rm = TRUE)

    } else {
      if (!is.null(geoms) & transparency == 0) { # opaque wood + opaque leaves
        poly_terra <- rbind(
          wood_poly_terra[[idx]],
          item_poly_terra[[idx]])

      } else { # opaque wood
        poly_terra <- wood_poly_terra[[idx]]
      }
      polygon_grid <- terra::rasterize(poly_terra, empty_grid, background = 0)
      polygon_grid[polygon_grid == 1] <- Inf
    }

    # delete polygons
    wood_poly_terra[[idx]] <- terra::vect()
    if (!is.null(geoms)) {
      item_poly_terra[[idx]] <- terra::vect()
    }

    # check if we have radiation
    if (!is.null(radiation)) {

      # add radiation data
      # (assumes that radiation is radiation sum until the previous measurement)
      # (converts from radiation resolution (sum) to sun direction resolution (avg))
      radiation_curr <- radiation[radiation$timestamp == lubridate::ceiling_date(timestep[idx], paste(rad_interval, "aseconds")),]
      polygon_grid[polygon_grid != Inf] <- radiation_curr[,"diffuse_energy_per_area"] + radiation_curr[,"direct_energy_per_area"] * transparency ** polygon_grid
      polygon_grid[polygon_grid == Inf] <- radiation_curr[,"diffuse_energy_per_area"]
    } else {

      # only determine shade yes / no
      polygon_grid[polygon_grid != Inf] <- transparency ** polygon_grid
      polygon_grid[polygon_grid == Inf] <- 0
    }

    # return raster
    return(polygon_grid)
  })

  # stack rasters
  radiation_grid <- terra::rast(radiation_grid)

  # set layer names
  names(radiation_grid) <- format(timestep,  format = "%Y-%m-%d %H:%M")

  # return result
  return(radiation_grid)
}

################################################################################

#' Calculate shade of geoms
#'
#' @description
#' \code{shade_tree_geoms} calculates the shade cast by a tree based on given sun
#' positions. The radiation within this time frame may be specified, otherwise,
#' shade is coded as 0 and light is coded as 1. The function can include the
#' shade of modeled items (leaves, flowers) if provided. The processing may be
#' done sequentially or in parallel. For the output rasters, the resolution and
#' extent is required. Further, the transparency of leaves may be configured.
#' The ground can be specified via a point on the plane and the plane normal.
#' Per default, an even ground at the origin is assumed.
#'
#' @param geom_wood \code{matrix}, contains matrix with IDs and coordinates of
#' wood geoms.
#' @param sun_position \code{data.frame}, sunlight direction over the time.
#' @param geom_other \code{matrix}, contains matrix with IDs and coordinates of
#' leaf / flower geoms.
#' @param radiation \code{data.frame}, diffuse and global radiation over time,
#' must be formatted as the example from \code{dummy_radiation()}, leave empty
#' to obtain only light intensity.
#' @param sequential \code{boolean}, whether sequential (\code{TRUE}) or
#' parallel processing (\code{FALSE}) should be used.
#' @param resolution \code{numeric}, spatial resolution of output rasters.
#' @param xmin,xmax,ymin,ymax \code{numeric}, extent of the shading raster in
#' meters, relative to the stem base.
#' @param transparency \code{numeric}, transparency of the item shade,
#' 1 = fully transparent, 0 = fully opaque.
#' @param plane_origin \code{numeric}, \code{xyz}-vector of a point on the
#' ground.
#' @param plane_normal \code{numeric}, \code{xyz}-vector of the ground normal.
#'
#' @return
#' \code{SpatRaster}, contains radiation around the tree for each
#' \code{sun_position}, the unit of the radiation depends on the unit used in
#' \code{radiation}. The single rasters for the time steps are stored as
#' layers with the timestamp as names.
#'
#' @details
#' The parameter \code{sun_position} determines for which time steps the shade
#' is calculated. The \code{resolution} of the shade raster should correspond to
#' the unit used in \code{radiation}, e.g. when using a resolution of 0.1,
#' the radiation should be given per dm². If the raster cell contains any
#' shade, the diffuse radiation is assigned. Otherwise, the global radiation is
#' assigned. Turning parallel processing on (\code{sequential = FALSE}) might
#' only work for windows computers.
#'
#' @seealso \code{\link{shade_summarize}}, \code{\link{sun_movement}}
#'
#' @examples
#' # load wood geoms
#' file_path <- system.file("extdata", "pear_wood.txt", package="qsm2shade")
#' geom_wood <- read.table(file_path, header = T)
#'
#' # load leaf geoms
#' file_path <- system.file("extdata", "pear_leaves.txt", package="qsm2shade")
#' geom_other <- read.table(file_path, header = T)
#'
#' # shift stem to 0,0,0
#' stem_location <- geom_tree_location(geom_wood)
#' wood <- geom_shift(geom_wood, stem_location)
#' leaves <- geom_shift(geom_other, stem_location)
#'
#' # get sun position at different times
#' timeframe <- seq(ISOdate(2020, 03, 22, 0, 0), ISOdate(2020, 03, 22, 23, 50), "10 mins")
#' sun_position <- sun_movement(timeframe, latitude = 48.07, longitude = 7.60)
#'
#' # create dummy radiation data
#' radiation <- dummy_radiation(ISOdate(2020, 01, 01, 0, 0), ISOdate(2020, 12, 31, 23, 50), "1 hour")
#'
#' # calculate shade
#' result <- shade_tree_geoms(wood, sun_position = sun_position, geom_other = leaves, radiation = radiation)
#'
#' # summarize per day
#' result_daily <- shade_summarize(result, "day")
#'
#' # plot daily shade
#' terra::plot(result_daily)
#' @export
shade_tree_geoms <- function(
    geom_wood, sun_position, geom_other = NULL, radiation = NULL, resolution = 0.1,
    sequential = TRUE, xmin = -20, xmax = 20, ymin = -20, ymax = 20,
    transparency = 0, plane_origin = c(0,0,0), plane_normal = c(0,0,1)) {

  # prepare geom data
  geom_wood <- as.matrix(geom_wood)
  if (!is.null(geom_other)) geom_other <- as.matrix(geom_other)

  # prepare sun data (get day data only)
  timestep <- unique(sun_position$timeframe[sun_position$day])
  sun_position$svy <- sun_position$svy * (-1) # https://doi.org/10.1080/713811744 (N- & S+)
  sun_direction <- t(sun_position[sun_position$day, 1:3])
  sun_position <- sun_position[order(sun_position$timeframe),]

  # check if there is a time where the sun is there
  if (ncol(sun_position) == 0) stop("there is no sun during the selected time") # check if this works

  # prepare radiation data
  if (!is.null(radiation)) {

    # get temporal resolution from sun_position
    sun_interval <- as.numeric(names(which.max(table(difftime(
      sun_position$timeframe[2:nrow(sun_position)],
      sun_position$timeframe[1:(nrow(sun_position) - 1)],
      units = "secs")))))

    # get temporal resolution from radiation
    rad_interval <- as.numeric(names(which.max(table(difftime(
      radiation$timestamp[2:nrow(radiation)],
      radiation$timestamp[1:(nrow(radiation) - 1)],
      units = "secs")))))

    # get factor by which the energy has to be divided
    if (sun_interval == 0) {
      rad_factor <- 1
    } else {
      rad_factor <- rad_interval / sun_interval
      rad_factor <- ifelse(is.na(rad_factor), 1, rad_factor)
    }

    # prepare radiation data
    radiation$direct_energy_per_area  <- radiation$global_energy_per_area - radiation$diffuse_energy_per_area
    radiation$direct_energy_per_area  <- radiation$direct_energy_per_area  / rad_factor
    radiation$diffuse_energy_per_area <- radiation$diffuse_energy_per_area / rad_factor
    radiation$global_energy_per_area  <- radiation$global_energy_per_area  / rad_factor
  }

  # sequential processing
  if (sequential) {

    # calculate wood shadows
    message("... creating wood shadows")
    wood_poly_terra <- apply(
      sun_direction, 2, qsm2shade:::shade_geoms_comp, geoms = geom_wood,
      plane_origin = plane_origin, plane_normal = plane_normal) |> lapply(qsm2shade:::list_polygonize)

    # calculate leaf shadows
    if (!is.null(geom_other)) {
      message("... creating leaf shadows")
      item_poly_terra <- apply(
        sun_direction, 2, qsm2shade:::shade_geoms_comp, geoms = geom_other,
        plane_origin = plane_origin, plane_normal = plane_normal) |> lapply(qsm2shade:::list_polygonize)

    } else {
      item_poly_terra <- NULL
    }

  } else {
    # parallel processing

    # set up cluster
    numCores <- parallel::detectCores()
    cl <- parallel::makeCluster(numCores, type = "PSOCK")

    # necessary packages
    packages_cl <- list("data.table")

    # export objects to cores
    parallel::clusterExport(cl, list(
      "packages_cl", "geom_wood", "geom_other", "sun_direction", "norm_cross", "plane_origin", "plane_normal"),
      envir = environment())

    # execute on all cores
    parallel::clusterEvalQ(cl, {

      # load packages
      lapply(packages_cl, require, character.only = T)
    })

    # calculate wood shadows
    message("... creating wood shadows")
    wood_poly_terra <- parallel::parApply(
      cl, sun_direction, 2, qsm2shade:::shade_geoms_comp, geoms = geom_wood,
      plane_origin = plane_origin, plane_normal = plane_normal) |> lapply(qsm2shade:::list_polygonize)

    # calculate item shadows
    if (!is.null(geom_other)) {
      message("... creating item shadows")
      item_poly_terra <- parallel::parApply(
        cl, sun_direction, 2, qsm2shade:::shade_geoms_comp, geoms = geom_other,
        plane_origin = plane_origin, plane_normal = plane_normal) |> lapply(qsm2shade:::list_polygonize)
    } else {
      item_poly_terra <- NULL
    }

    # stop the cluster
    parallel::stopCluster(cl)
  }

  # prepare empty raster for storage
  message("... deriving rasters")
  stem_base <- qsm2shade::geom_tree_location(geom_wood, 0, 0.3)
  empty_grid <- terra::rast(nlyrs = 1, res = resolution, vals = 0,
                            xmin = stem_base[["x"]] + xmin,
                            xmax = stem_base[["x"]] + xmax,
                            ymin = stem_base[["y"]] + ymin,
                            ymax = stem_base[["y"]] + ymax)

  # rasterize the geoms
  radiation_grid <- apply(matrix(1:length(timestep)), 1, function(idx) {

    # combine & rasterize geoms
    if (!is.null(geom_other) & transparency > 0) { # opaque wood + transparent leaves
      curr_wood <- wood_poly_terra[[idx]]
      curr_item <- item_poly_terra[[idx]]
      curr_wood$transparent <- Inf
      curr_item$transparent <- 1
      poly_terra <- rbind(curr_wood, curr_item)
      polygon_grid <- terra::rasterize(poly_terra, empty_grid, field = "transparent", fun = "sum", background = 0, na.rm = TRUE)

    } else {
      if (!is.null(geom_other) & transparency == 0) { # opaque wood + opaque leaves
        poly_terra <- rbind(
          wood_poly_terra[[idx]],
          item_poly_terra[[idx]])

      } else { # opaque wood
        poly_terra <- wood_poly_terra[[idx]]
      }
      polygon_grid <- terra::rasterize(poly_terra, empty_grid, background = 0)
      polygon_grid[polygon_grid == 1] <- Inf
    }

    # delete geoms
    wood_poly_terra[[idx]] <- terra::vect()
    if (!is.null(geom_other)) {
      item_poly_terra[[idx]] <- terra::vect()
    }

    # check if we have radiation
    if (!is.null(radiation)) {

      # add radiation data
      # (assumes that radiation is radiation sum until the previous measurement)
      # (converts from radiation resolution (sum) to sun direction resolution (avg))
      radiation_curr <- radiation[radiation$timestamp == lubridate::ceiling_date(timestep[idx], paste(rad_interval, "aseconds")),]
      polygon_grid[polygon_grid != Inf] <- radiation_curr[,"diffuse_energy_per_area"] + radiation_curr[,"direct_energy_per_area"] * transparency ** polygon_grid
      polygon_grid[polygon_grid == Inf] <- radiation_curr[,"diffuse_energy_per_area"]
    } else {

      # only determine shade yes / no
      polygon_grid[polygon_grid != Inf] <- transparency ** polygon_grid
      polygon_grid[polygon_grid == Inf] <- 0
    }

    # return raster
    return(polygon_grid)
  })

  # stack rasters
  radiation_grid <- terra::rast(radiation_grid)

  # set layer names
  names(radiation_grid) <- format(timestep,  format = "%Y-%m-%d %H:%M")

  # return result
  return(radiation_grid)
}

################################################################################

#' Summarize shade per period
#'
#' @description
#' \code{shade_summarize} summarizes the rasters produced by \code{shade_tree()}
#' in variable time periods.
#'
#' @param radiation_grid \code{SpatRaster}, stacked set of shade rasters from
#' \code{shade_tree()}.
#' @param period \code{character}, length of summary period, either \code{"hour"},
#' \code{"day"}, \code{"month"}, \code{"year"} or \code{"total"}.
#' @param na.rm \code{boolean}, whether NAs should be removed prior to the
#' calculation.
#'
#' @return
#' \code{SpatRaster}, stacked shade rasters summarized by time periods, the
#' layer names indicate the respective time period.
#'
#' @seealso \code{\link{shade_tree}}
#'
#' @examples
#' # load qsm
#' file_path <- system.file("extdata", "walnut.mat", package="qsm2shade")
#' qsm <- qsm2r::readQSM(file_path)
#'
#' # get son position at different times
#' sun_position <- sun_movement(ISOdate(2020, 03, 22, 0, 0), ISOdate(2020, 03, 23, 23, 50), "10 mins", lat = 48.07, lon = 7.60)
#'
#' # create dummy radiation data
#' radiation <- dummy_radiation(ISOdate(2020, 01, 01, 0, 0), ISOdate(2020, 12, 31, 23, 50), "1 hour")
#'
#' # calculate shade
#' result <- shade_tree(qsm, sun_position = sun_position, radiation = radiation)
#'
#' # summarize per day
#' result_daily <- shade_summarize(result, "day")
#'
#' # plot daily shade
#' terra::plot(result_daily)
#' @export
shade_summarize <- function(radiation_grid, period = c("hour", "day", "month", "year", "total"), na.rm = TRUE) {

  # check input validity
  if (length(period) > 1 | !any(period %in% c("hour", "day", "month", "year", "total"))) {
    stop("period must be 'hour', day', 'month', 'year' or 'total")
  }

  # extract times from raster names
  timestep <- as.POSIXct(strptime(names(radiation_grid), "%Y-%m-%d %H:%M"))

  # add everything together
  if (period == "total") {
    summarized <- sum(radiation_grid, na.rm = na.rm)
    return(summarized)
  }

  # summarize depending on period
  period_all <- sort(unique(lubridate::floor_date(timestep, period)))
  summarized <- apply(matrix(period_all), 1, function(period_curr) {
    period_layers <- lubridate::floor_date(timestep, period) == period_curr
    period_grid <- sum(radiation_grid[[period_layers]], na.rm = na.rm)
    return(period_grid)})
  summarized <- terra::rast(summarized)

  # rename depending on period
  format_dict <- list("hour" = "%Y-%m-%d %H", "day" = "%Y-%m-%d", "month" = "%Y-%m", "year" = "%Y")
  names(summarized) <- format(period_all,  format = format_dict[[period]])

  # return summarized data
  return(summarized)
}

################################################################################
