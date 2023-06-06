################################################################################
# MAIN FUNCTIONS
################################################################################

#' Create dummy radiation data
#'
#' @description
#' \code{dummy_radiation_hourly} creates dummy radiation data to be used
#' by \code{shade_tree()}.
#'
#' @param start \code{POSIXct}, start time of the radiation data.
#' @param end \code{POSIXct}, end time of the radiation data.
#'
#' @return
#' \code{data.frame}, contains diffuse and global radiation over time.
#'
#' @seealso \code{\link{shade_tree}}
#'
#' @examples
#' # create dummy radiation data
#' radiation_hourly <- dummy_radiation_hourly(ISOdate(2020, 01, 01, 0, 0), ISOdate(2020, 12, 31, 23, 50))
#' @export
dummy_radiation_hourly <- function(
    start = ISOdate(2020, 01, 01, 0, 0), end = ISOdate(2020, 12, 31, 23, 50)) {

  # create dummy data
  timestamp <- seq(start, end, by = "1 hour")
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
shade_wood <- function(sun_direction, tree) {

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
  px_cols <- which(endsWith(cyl_cols, "x"))
  py_cols <- which(endsWith(cyl_cols, "y"))
  pz_cols <- which(endsWith(cyl_cols, "z"))

  # prepare storage
  projected <- matrix(NA, nrow = nrow(tree), ncol = c(2 * 8)) # for 8 x-y coordinates

  # projection
  dist <- -cyl_pts[,pz_cols] / sun_direction[3]
  projected <- matrix(c(
    1:nrow(cyl_pts),
    cyl_pts[,px_cols] + dist * sun_direction[1],
    cyl_pts[,py_cols] + dist * sun_direction[2]),
    ncol = 1 + length(px_cols) * 2)
  colnames(projected) <- c("id", cyl_cols[px_cols], cyl_cols[py_cols])

  # get columns with coordinates
  all_cols <- colnames(projected)
  x_cols <- endsWith(all_cols, "x")
  y_cols <- endsWith(all_cols, "y")

  # get coordinates of convex hull
  chull_list <- lapply(1:nrow(projected), function(idx) {
    cp <- cbind(as.numeric(projected[idx, x_cols]),
                as.numeric(projected[idx, y_cols]))
    hullIdx <- chull(cp)
    cbind(id = idx, cp[c(hullIdx, hullIdx[1]),])
  })

  # convert to polygons
  wood_poly_terra <- do.call(rbind, chull_list) |> terra::vect("polygons")

  # return polygons
  return(terra::wrap(wood_poly_terra))
}

# compile function to make it faster
shade_wood_comp <- compiler::cmpfun(shade_wood)

################################################################################

# calculate item shadow polygons
shade_items <- function(sun_direction, item_pts) {

  # get coordinate cols
  poly_cols <- colnames(item_pts)
  px_cols <- endsWith(poly_cols, "x")
  py_cols <- endsWith(poly_cols, "y")
  pz_cols <- endsWith(poly_cols, "z")

  # projection
  dist <- -item_pts[,pz_cols] / sun_direction[3]
  projected <- matrix(c(
    1:nrow(item_pts),
    item_pts[,px_cols] + dist * sun_direction[1],
    item_pts[,py_cols] + dist * sun_direction[2]),
    ncol = 1 + length(px_cols) * 2)
  colnames(projected) <- c("id", poly_cols[px_cols], poly_cols[py_cols])

  # convert to geom format
  item_dt_x <- data.table::melt(data.table::data.table(projected[,c("id", poly_cols[px_cols])]), id.vars = c("id"))
  item_dt_y <- data.table::melt(data.table::data.table(projected[,c("id", poly_cols[py_cols])]), id.vars = c("id"))
  item_dt_x$pos <- as.numeric(gsub("\\D", "", item_dt_x$variable))
  item_dt_y$pos <- as.numeric(gsub("\\D", "", item_dt_y$variable))
  item_dt_x$variable <- item_dt_y$variable <- NULL
  item_dt <- merge(item_dt_x, item_dt_y, by = c("id", "pos"))
  data.table::setorder(item_dt, id, pos)
  item_dt$pos <- NULL

  # convert to polygons
  item_poly_terra <- terra::vect(as.matrix(item_dt), "polygons")

  # return polygons
  return(terra::wrap(item_poly_terra))
}

# compile function to make it faster
shade_items_comp <- compiler::cmpfun(shade_items)

################################################################################

#' Calculate shade of a QSM
#'
#' @description
#' \code{shade_tree}.
#'
#' @param qsm An object of class \code{QSM}.
#' @param sun_position \code{data.frame}, sunlight direction over the time.
#' @param radiation_hourly \code{data.frame}, diffuse and global radiation over time.
#' @param item_pts \code{matrix}, contains coordinates of simulated items.
#' @param sequential \code{boolean}, whether sequential (\code{TRUE}) or
#' parallel processing (\code{FALSE}) should be used.
#' @param resolution \code{numeric}, extent of the shading raster in
#' meters.
#' @param xmin,xmax,ymin,ymax \code{numeric}, extent of the shading raster in
#' meters.
#'
#' @return
#' \code{SpatRaster}, contains radiation around the tree for each
#' \code{sun_position}, the unit of the radiation depends on the unit used in
#' \code{radiation_hourly}. The single rasters for the time steps are stored as
#' layers with the timestamp as names.
#'
#' @details
#' The parameter \code{sun_position} determines for which time steps the shade
#' is calculated. The \code{resolution} of the shade raster should correspond to
#' the unit used in \code{radiation_hourly}, e.g. when using a resolution of
#' 0.1, the radiation should be given per dm². If the raster cell contains any
#' shade, the diffuse radiation is assigned. Otherwise, the global radiation is
#' assigned. Turning parallel processing on (\code{sequential = FALSE}) might
#' only work for windows computers.
#'
#' @seealso \code{\link{shade_summarize}}, \code{\link{sun_movement}},
#' \code{\link{add_items}}
#'
#' @examples
#' # load qsm
#' file_path <- system.file("extdata", "Prunus_avium_QSM_simplified.mat", package="qsm2shade")
#' qsm <- qsm2r::readQSM(file_path)
#'
#' # get sun position at different times
#' timeframe <- seq(ISOdate(2020, 03, 22, 0, 0), ISOdate(2020, 03, 22, 23, 50), "10 mins")
#' sun_position <- sun_movement(timeframe, latitude = 48.07, longitude = 7.60)
#'
#' # create dummy radiation data
#' radiation_hourly <- dummy_radiation_hourly(ISOdate(2020, 01, 01, 0, 0), ISOdate(2020, 12, 31, 23, 50))
#'
#' # without leaves:
#'
#' # calculate shade
#' result <- shade_tree(qsm, sun_position, radiation_hourly)
#'
#' # show shade
#' plot(result)
#'
#' # with leaves:
#'
#' # create polygons for single item
#' leaf <- create_single_leaf(leaf_type = "normal", length_m = 0.1)
#'
#' # get dummy item distribution
#' distribution <- dummy_item_distribution()
#'
#' # create items
#' leaf_pts <- add_items(qsm, distribution, leaf, item_type = "leaves")
#'
#' # calculate shade
#' result <- shade_tree(qsm, sun_position, radiation_hourly, item_pts = leaf_pts)
#'
#' # show shade
#' plot(result)
#' @export
shade_tree <- function(
    qsm, sun_position, radiation_hourly, resolution = 0.1, item_pts = NULL,
    sequential = TRUE, xmin = -50, xmax = 50, ymin = -25, ymax = 50) {

  # prepare tree data
  tree <- prepare_qsm(qsm)

  # prepare sun data (get day data only)
  timestep <- unique(sun_position$timeframe[sun_position$day])
  sun_position$svy <- sun_position$svy * (-1) # https://doi.org/10.1080/713811744 (N- & S+)
  sun_direction <- t(sun_position[sun_position$day, 1:3])

  # prepare radiation data
  sun_position <- sun_position[order(sun_position$timeframe),]

  # get factor by which the energy has to be divided
  sun_time_step <- difftime(sun_position$timeframe[2], sun_position$timeframe[1], unit = "secs")
  sun_factor_hourly <- lubridate::hours(1) / lubridate::seconds(sun_time_step)
  sun_factor_hourly <- ifelse(is.na(sun_factor_hourly), 1, sun_factor_hourly)

  # sequential processing
  if (sequential) {

    # calculate wood shadows
    message("... creating wood shadows")
    wood_poly_terra <- apply(sun_direction, 2, shade_wood_comp, tree = tree)

    # calculate item shadows
    if (!is.null(item_pts)) {
      message("... creating item shadows")
      item_poly_terra <- apply(sun_direction, 2, shade_items_comp, item_pts = item_pts)

    } else {
      item_poly_terra <- NULL
    }

  } else {
    # parallel processing

    # set up cluster
    numCores <- parallel::detectCores()
    cl <- parallel::makeCluster(numCores, type = "PSOCK")

    # necessary packages
    # TODO: shorten, old: list("terra", "raster", "sp", "data.table")
    packages_cl <- list("terra", "data.table")

    # export objects to cores
    # TODO: shorten
    parallel::clusterExport(cl, list(
      "packages_cl", "tree", "item_pts", "sun_direction", "norm_cross"),
      envir = environment())

    # execute on all cores
    parallel::clusterEvalQ(cl, {

      # load packages
      lapply(packages_cl, require, character.only = T)
    })

    # calculate wood shadows
    message("... creating wood shadows")
    wood_poly_terra <- parallel::parApply(cl, sun_direction, 2, shade_wood_comp, tree = tree)

    # calculate item shadows
    if (!is.null(item_pts)) {
      message("... creating item shadows")
      item_poly_terra <- parallel::parApply(cl, sun_direction, 2, shade_items_comp, item_pts = item_pts)
    } else {
      item_poly_terra <- NULL
    }

    # stop the cluster
    parallel::stopCluster(cl)
  }

  # TODO: parallelize raster creation as well?
  # rasterize the polygons
  message("... deriving rasters")
  empty_grid <- terra::rast(nlyrs = 1, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, res = resolution, vals = 0)
  radiation_grid <- apply(matrix(1:length(timestep)), 1, function(idx) {

    # combine polygons
    if (!is.null(item_pts)) {
      poly_terra <- rbind(
        terra::vect(wood_poly_terra[[idx]]),
        terra::vect(item_poly_terra[[idx]]))
    } else {
      poly_terra <- terra::vect(wood_poly_terra[[idx]])
    }

    # rasterize polygons
    polygon_grid <- terra::rasterize(poly_terra, empty_grid, background = 0)

    # add radiation data
    radiation_curr <- radiation_hourly[radiation_hourly$timestamp == lubridate::floor_date(timestep[idx], "hour"),]
    polygon_grid <- polygon_grid * radiation_curr[,"diffuse_energy_per_area"] / sun_factor_hourly # divide to get from 1h to 10min
    polygon_grid[polygon_grid == 0] <- radiation_curr[,"global_energy_per_area"] / sun_factor_hourly # divide to get from 1h to 10mi

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

#' Summarize shade per period
#'
#' @description
#' \code{shade_summarize} summarizes the rasters produced by \code{shade_tree()}
#' in variable time periods.
#'
#' @param radiation_grid \code{SpatRaster}, stacked set of shade rasters from
#' \code{shade_tree()}.
#' @param period \code{character}, length of summary period, either
#' \code{"day"}, \code{"month"}, \code{"year"} or \code{"total"}.
#'
#' @return
#' \code{SpatRaster}, stacked shade rasters summarized by time periods, the
#' layer names indicate the respective time period.
#'
#' @seealso \code{\link{shade_tree}}
#'
#' @examples
#' # load qsm
#' file_path <- system.file("extdata", "Prunus_avium_QSM_simplified.mat", package="qsm2shade")
#' qsm <- qsm2r::readQSM(file_path)
#'
#' # get son position at different times
#' sun_position <- sun_movement(ISOdate(2020, 03, 22, 0, 0), ISOdate(2020, 03, 23, 23, 50), "10 mins", lat = 48.07, lon = 7.60)
#'
#' # create dummy radiation data
#' radiation_hourly <- dummy_radiation_hourly(ISOdate(2020, 01, 01, 0, 0), ISOdate(2020, 12, 31, 23, 50))
#'
#' # calculate shade
#' result <- shade_tree(qsm, sun_position, radiation_hourly)
#'
#' # summarize per day
#' result_daily <- shade_summarize(result, "day")
#'
#' # plot daily shade
#' terra::plot(result_daily)
#' @export
shade_summarize <- function(radiation_grid, period = c("day", "month", "year", "total")) {

  # check input validity
  if (length(period) > 1 | !any(period %in% c("day", "month", "year", "total"))) {
    stop("period must be 'day', 'month', 'year' or 'total")
  }

  # extract times from raster names
  timestep <- as.POSIXct(strptime(names(radiation_grid), "%Y-%m-%d %H:%M"))

  # add everything together
  if (period == "total") {
    summarized <- sum(radiation_grid)
    return(summarized)
  }

  # summarize depending on period
  period_all <- sort(unique(lubridate::floor_date(timestep, period)))
  summarized <- apply(matrix(period_all), 1, function(period_curr) {
    period_layers <- lubridate::floor_date(timestep, period) == period_curr
    period_grid <- sum(radiation_grid[[period_layers]])
    return(period_grid)})
  summarized <- terra::rast(summarized)

  # rename depending on period
  format_dict <- list("day" = "%Y-%m-%d", "month" = "%Y-%m", "year" = "%Y")
  names(summarized) <- format(period_all,  format = format_dict[[period]])

  # return summarized data
  return(summarized)
}

################################################################################
