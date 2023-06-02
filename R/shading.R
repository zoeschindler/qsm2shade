################################################################################
# MAIN FUNCTIONS
################################################################################

sun_move <- function(start, end, timezone, lat, lon, frequency) {

  # "sun_move" is based on the functions "sunvector" and "sunpos" both from the "insol" package in R
  # "sunvector" calculates a unit vector in the direction of the sun from the observer position
  # "sunpos" returns a matrix of azimuth and zenith angles of the sun given the unit vectors from the observer to the direction of the sun

  # Our function requires information on the latitude and longitude, the time zone and the time frame.
  # With this input it calculates azimuth and zenith of the sun on the research plot for every timeframe we need on every location we have.

  # The output of "sun_move": Cartesian coordinates of the sun, azimuth and zenith of the sun, the Julian day, the date and hour of the day and if the sun is set or not.
  # The output is maintained in hourly time steps.

  # prepare data
  position <- matrix(0, ncol = 8, nrow = 500)
  position_df <- matrix(0, ncol = 8, nrow = 500)
  timeframe <- seq(start, end, length.out = frequency)
  julianday <- insol::JD(timeframe)

  # sunvector: calculates a unit vector in the direction of the sun from the observer position
  position_xyz <- as.data.frame(insol::sunvector(jd = julianday, latitude = lat, longitude = lon, timezone = timezone))

  # sunpos: returns a matrix of azimuth and zenith angles of the sun given the unit vectors from the observer to the
  #         direction of the sun, sunnposition in breisach (azimuth und zenit)
  position_az <- as.data.frame(insol::sunpos(position_xyz))

  # combine data
  position <- cbind(position_xyz, position_az, julianday, seq(start, end, length.out = frequency))
  daytime <- position$zenith <= 90
  position$dn <- ifelse(daytime, "sun", "nosun")

  # change names
  colbr <- colnames(position)
  colbr[7] <- "timeframe"
  colnames(position) <- colbr

  # # ggplot option
  # ggplot() +
  #   geom_point(aes(y = 90 - position$zenith[which(position$dn == "sun")],
  #                  x = position$azimuth[which(position$dn == "sun")],
  #                  color = factor(position$timeframe[which(position$dn == "sun")])), size = 5) +
  #   coord_polar(theta = 'x', start = 0, direction = 1) +
  #   scale_x_continuous(limits = c(0, 360), expand = c(0, 0), breaks = seq(0, 360 - 1, by = 45)) +
  #   labs(x = "Azimuth", y = "Zenith") +
  #   scale_colour_manual(name = "Time",
  #                       values = rainbow(length(which(position$dn == "sun"))),
  #                       breaks = as.character(position$timeframe[which(position$dn == "sun")]),
  #                       labels = as.character(strftime(position[,7][which(position$dn == "sun")], format = "%H:%M"))) +
  #   theme_bw() +
  #   theme(panel.grid.major = element_line(colour = "grey"),
  #         panel.grid.minor = element_line(colour = "grey", linetype = "dotted")) +
  #   ggtitle("Sun Position")

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
  x_cols <- which(endsWith(all_cols, "x"))
  y_cols <- which(endsWith(all_cols, "y"))

  # get coordinates of convex hull
  chull_list <- lapply(1:nrow(projected), function(idx) {
    cp <- cbind(as.numeric(projected[idx, x_cols]),
                as.numeric(projected[idx, y_cols]))
    hullIdx <- chull(cp)
    cbind(id = idx, cp[c(hullIdx, hullIdx[1]),])
  })

  # convert to polygons
  wood_poly_terra <- do.call(rbind, chull_list) |> vect("polygons")

  # return polygons
  return(wrap(wood_poly_terra))
}

# compile function to make it faster
shade_wood_comp <- compiler::cmpfun(shade_wood)

################################################################################

# calculate item shadow polygons
shade_items <- function(sun_direction, item_pts) {

  # get coordinate cols
  poly_cols <- colnames(item_pts)
  px_cols <- which(endsWith(poly_cols, "x"))
  py_cols <- which(endsWith(poly_cols, "y"))
  pz_cols <- which(endsWith(poly_cols, "z"))

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
  setorder(item_dt, id, pos)
  item_dt$pos <- NULL

  # convert to polygons
  item_poly_terra <- terra::vect(as.matrix(item_dt), "polygons")

  # return polygons
  return(terra::wrap(item_poly_terra))
}

# compile function to make it faster
shade_items_comp <- compiler::cmpfun(shade_items)

################################################################################

shade_tree <- function(
    qsm, sun_position, real_radiation = NULL, items = FALSE, item_type = c("leaves", "flowers"),
    item_distribution = NULL, cylinder_classes = NULL, item_poly = NULL, stem_len = 0.01,
    item_angle = 45, overall = TRUE, monthly = FALSE, sequential = TRUE) {

  # prepare tree data
  tree <- prepare_qsm(qsm)

  # prepare sun data
  shine_idx <- which(sun_position$dn == "sun")
  timestep <- unique(sun_position$timeframe[shine_idx])
  sun_position$svy <- sun_position$svy * (-1) # https://doi.org/10.1080/713811744 (N- & S+)
  sun_direction <- t(sun_position[shine_idx, 1:3])

  # generate items
  if (items) {
    message("... generating items")
    item_pts <- add_items_comp(
      tree, item_distribution, item_poly, stem_len, item_type, item_angle, cylinder_classes)
  } else {
    item_pts <- NULL
  }

  # sequential processing
  if (sequential) {

    # calculate wood shadows
    message("... creating wood shadows")
    wood_poly_terra <- apply(sun_direction, 2, shade_wood_comp, tree = tree)

    # calculate item shadows
    if (items) {
      message("... creating item shadows")
      item_poly_terra <- apply(sun_direction, 2, shade_items_comp, item_pts = item_pts)

    } else {
      item_poly_terra <- NULL
    }

  } else {
    # parallel processing

    # set up cluster
    numCores <- detectCores()
    cl <- makeCluster(numCores, type = "PSOCK")

    # necessary packages
    packages_cl <- list("terra", "raster", "sp", "data.table")

    # export objects to cores
    # TODO: shorten
    clusterExport(cl, list(
      "packages_cl", "tree", "real_radiation", "item_pts",
      "timestep", "sun_direction", "shine_mat",
      "norm_cross"),
      envir = environment())

    # execute on all cores
    clusterEvalQ(cl, {

      # load packages
      lapply(packages_cl, require, character.only = T)
    })

    # calculate wood shadows
    message("... creating wood shadows")
    wood_poly_terra <- parApply(cl, shine_mat, 1, shade_wood_comp, sun_direction = sun_direction, tree = tree)

    # calculate item shadows
    if (items) {
      message("... creating item shadows")
      item_poly_terra <- parApply(cl, shine_mat, 1, shade_items_comp, sun_direction = sun_direction, item_pts = item_pts)
    } else {
      item_poly_terra <- NULL
    }

    # stop the cluster
    stopCluster(cl)
  }

  # rasterize the polygons
  message("... deriving rasters")
  empty_grid <- rast(nlyrs = 1, xmin = -50, xmax = 50, ymin = -25, ymax = 50, vals = 0, resolution = 0.1)
  radiation_grid <- apply(matrix(1:length(shine_mat)), 1, function(idx) {

    # get sun index
    sun_idx <- shine_mat[idx]

    # combine polygons
    if (items) {
      poly_terra <- rbind(
        vect(wood_poly_terra[[idx]]),
        vect(item_poly_terra[[idx]]))
    } else {
      poly_terra <- vect(wood_poly_terra[[idx]])
    }

    # rasterize polygons
    polygon_grid <- rasterize(poly_terra, empty_grid, background = 0)

    # real weather conditions
    radi_year_curr <- real_radiation[which(real_radiation$hour_format == format(timestep[sun_idx], "%Y-%m-%d %H")),]
    radi_year_curr <- radi_year_curr[,c("timestamp", "diffuse", "global")]
    radi_year_curr[,2:3] <- round(radi_year_curr[,2:3], digits = 3)

    # add radiation data
    polygon_grid <- polygon_grid * radi_year_curr[,"diffuse"] / 6 # divided by 6 to get from 1h to 10min
    polygon_grid[polygon_grid == 0] <- radi_year_curr[,"global"] / 6 # divided by 6 to get from 1h to 10min

    return(polygon_grid)
  })

  # stack rasters
  radiation_grid <- rast(radiation_grid)

  # summed up monthly shade
  if (monthly) {
    month_factor <- as.matrix(1:12)
    overall_radiation <- apply(month_factor, 1, function(month) {
      month_layers <- which(as.numeric(format(timestep, "%m")) %in% month_factor[month])
      if (length(month_layers) > 0) {
        radiation_grid <- sum(radiation_grid[[month_layers]])
      } else {
        radiation_grid <- rast(ext(radiation_grid), vals = 0, resolution = res(radiation_grid))
      }
    })
    overall_radiation <- rast(overall_radiation)
  }

  # summed up annual shade
  if (overall) {
    overall_radiation <- sum(radiation_grid)
  }

  # return result
  return(overall_radiation)
}

################################################################################
