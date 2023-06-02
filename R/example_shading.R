# ################################################################################
# # PREPARATION
# ################################################################################
#
# # necessary
# if (!require(parallel)) {install.packages("parallel"); require(parallel)}
# packages <- list("compiler", "ggplot2", "plyr", "dplyr", "insol", "raster",
#                  "sp", "terra", "tidyr", "lubridate", "data.table")
# lapply(packages, function(name) {
#   if (!require(name, character.only = TRUE)) {
#     install.packages(name); require(name, character.only = TRUE)}
# })
#
# # set working directory
# path_curr <- "U:/Paper/Paper_Shading"
# setwd(path_curr)
#
# # load other scripts
# library(qsm2r)
# source("D:/github/qsm_classification_function.R")
# ################################################################################
# # EXECUTION - SUN POSITION
# ################################################################################
#
# # input time (in UTC)
# # start <- ISOdate(2014, 01, 01, 0, 0)
# # end  <-  start + years(1) - minutes(10)
# start <- ISOdate(2014, 01, 15, 0, 0)
# end  <-  start + lubridate::days(1) - lubridate::minutes(10)
#
# # coordinates
# lat  <- 48.07
# lon <- 7.60
#
# # time information
# frequency <- difftime(end, start, units = "days") * 24 * 6 + 1
# timezone <- 0 # earlier: 2, weather data is UTC
#
# # title information
# end_str  <- strftime(end,  format = "%d.%m.%Y")
# start_str <- strftime(start, format = "%d.%m.%Y")
#
# # derive sun position data
# sun_position <- sun_move(start, end, timezone, lat, lon, frequency)
#
# # show sun position
# ggplot(sun_position) +
#   geom_point(aes(x = svx, y = -svy, col = dn), size = 3, alpha = 0.5) +
#   labs(x = "West → East", y = "South → North") +
#   geom_point(aes(x = 0, y = 0), size = 6, alpha = 0.5, col = "grey50") +
#   scale_color_manual(values = list("nosun" = "darkblue", "sun" = "orange"),
#                      labels = c("no", "yes"), name = "Sunshine") +
#   coord_fixed() +
#   theme_light()
#
# ################################################################################
# # EXECUTION - RADIATION
# ################################################################################
#
# # TODO: warum nicht gleich stündliche Werte?
# # TODO: stimmt die Zeitzone überein mit den sunn-Werten?
# #       (sunn-Position: Zeitzone 1, Strahlung in UTC)
#
# # input hourly sums of the insolation data:
# real_radiation <- read.table("produkt_st_stunde_19730101_20150131_01443.txt", sep = ";", header = T)
#
# # extract data
# real_radiation$timestamp <- as.POSIXct(strptime(real_radiation[,9], "%Y%m%d%H:%M", tz = "UTC")) # prepare time format
#
# # extract data
# real_radiation$y   <- as.numeric(format(real_radiation$timestamp, "%Y")) # get year
# real_radiation$m   <- as.numeric(format(real_radiation$timestamp, "%m")) # get month
# real_radiation$ym  <- as.factor(paste(real_radiation$y, real_radiation$m, sep = "_"))
# real_radiation$h   <- as.numeric(format(real_radiation$timestamp, "%H")) # get hour
# real_radiation$ymh <- as.factor(paste(real_radiation$ym, real_radiation$h, sep = "_"))
# real_radiation$mh  <- as.factor(paste(real_radiation$m, real_radiation$h, sep = "_"))
# real_radiation$hour_format <- strftime(real_radiation$timestamp, format = "%Y-%m-%d %H")
#
# # rename
# real_radiation$diffuse <- real_radiation$FD_LBERG
# real_radiation$global  <- real_radiation$FG_LBERG
# real_radiation$FD_LBERG <- NULL
# real_radiation$FG_LBERG <- NULL
#
# # no values measured as -999 is a number, better NA
# real_radiation$diffuse[which(real_radiation$diffuse == -999)] <- NA
# real_radiation$global[which(real_radiation$global  == -999)]  <- NA
#
# # convert units (J / cm² -> Wh / dm²)
# real_radiation$diffuse <- real_radiation$diffuse / 36
# real_radiation$global  <- real_radiation$global  / 36
#
# # TODO: I don't know if this step is so good...
#
# # aggregate per hour in each month, for all years
# # (average )
# radiation_month_hour <- real_radiation |>
#   rename(month_hour = mh, month = m, hour = h) |>
#   group_by(month_hour, month, hour) |>
#   summarize(
#     global = mean(global, na.rm = T),
#     diffuse = mean(diffuse, na.rm = T)) |>
#   data.frame()
#
# # mean over all years
# radiation_month_hour_all <- gather(radiation_month_hour, "Type", "value", -c(month_hour, hour, month))
# ggplot(radiation_month_hour_all) +
#   geom_point(aes(x = hour, y = value, shape = Type, col = Type)) +
#   geom_line(aes(x = hour, y = value, lty = Type, col = Type)) +
#   xlab("Hour") + ylab("Wh/dm²") +
#   ggtitle("Mean Hourly Insolation Energy (1977 - 2015)") +
#   facet_wrap(~month, ncol = 12) +
#   theme_light() +
#   scale_x_continuous(breaks = seq(3, 21, 9))
#
# # aggregate per hour in each month, for one year
# real_radiation_year <- real_radiation[which(format(real_radiation$timestamp, "%Y") %in% as.numeric(format(start, "%Y"))),]
# radiation_month_hour_year <- real_radiation_year |>
#   rename(month_hour = mh, month = m, hour = h) |>
#   group_by(month_hour, month, hour) |>
#   summarize(
#     global = mean(global, na.rm = T),
#     diffuse = mean(diffuse, na.rm = T)) |>
#   data.frame()
#
# # mean over all years
# radiation_month_hour_year <- gather(radiation_month_hour_year, "Type", "value", -c(month_hour, hour, month))
# ggplot(radiation_month_hour_year) +
#   geom_point(aes(x = hour, y = value, shape = Type, col = Type)) +
#   geom_line(aes(x = hour, y = value, lty = Type, col = Type)) +
#   xlab("Hours") + ylab("Wh/dm²") +
#   ggtitle(paste0("Mean Hourly Insolation Energy (", format(real_radiation_year$timestamp[1], "%Y"), ")")) +
#   facet_wrap(~month, ncol = 12) +
#   theme_light() +
#   scale_x_continuous(breaks = seq(3, 21, 9))
#
# ################################################################################
# # EXECUTION - SHADOWS
# ################################################################################
#
# # load tree data
# qsm <- readQSM("U:/Paper/Paper_Shading/dummy_data/cherry_tree_simplified.mat")
#
# # get cylinder classifications
# cylinder_classes <- classify_crown(qsm)
#
# # set dummy variables for item generation (in meters)
# leaf_distribution <- expand.grid(
#   compass = c("N", "NO", "O", "SO", "S", "SW", "W", "NW"),
#   crown_bmt = c("bot", "mid", "top"),
#   crown_io = c("in", "out"),
#   diam_start_m = seq(0, 0.19, 0.01))
# leaf_distribution$diam_end_m <- leaf_distribution$diam_start_m + 0.01
# leaf_distribution$m_per_item <- rep(round(seq(0.01, 1, length.out = 20), 3), each = 48)
# leaf_distribution$item_scaling <- rep(round(seq(15, 5, length.out = 20), 3), each = 48)
# leaf_distribution$m_per_item[leaf_distribution$crown_bmt == "top"] = leaf_distribution$m_per_item[leaf_distribution$crown_bmt == "top"] * 0.75
# leaf_distribution$m_per_item[leaf_distribution$crown_bmt == "bot"] = leaf_distribution$m_per_item[leaf_distribution$crown_bmt == "bot"] * 1.25
# leaf_distribution$item_scaling[leaf_distribution$compass %in% c("N", "NW", "NO")] = leaf_distribution$item_scaling[leaf_distribution$compass %in% c("N", "NW", "NO")] * 0.75
# leaf_distribution$item_scaling[leaf_distribution$compass %in% c("S", "SW", "SO")] = leaf_distribution$item_scaling[leaf_distribution$compass %in% c("S", "SW", "SO")] * 1.25
#
# # set variables for item generation (in meters)
# leaf_poly <- as.matrix(data.frame(
#   "x" = c(0, 0.03, 0.07, 0.10, 0.07, 0.03, 0),
#   "y" = c(0, 0.04, 0.02, 0, -0.02, -0.04, 0),
#   "z" = c(0, 0, 0, 0, 0, 0, 0))) / 10
# leaf_angle <- 45 # degree
# stem_len <- 0.01
#
# # # set dummy variables for item generation (in meters)
# # flower_distribution <- expand.grid(
# #   compass = c("N", "NO", "O", "SO", "S", "SW", "W", "NW"),
# #   crown_bmt = c("bot", "mid", "top"),
# #   crown_io = c("in", "out"),
# #   diam_start_m = seq(0, 0.19, 0.01))
# # flower_distribution$diam_end_m <- flower_distribution$diam_start_m + 0.01
# # flower_distribution$m_per_item <- rep(round(seq(0.05, 1, length.out = 20), 3), each = 48)
# # flower_distribution$item_scaling <- rep(round(seq(3, 1, length.out = 20), 3), each = 48)
# # flower_distribution$m_per_item[flower_distribution$crown_bmt == "top"] = flower_distribution$m_per_item[flower_distribution$crown_bmt == "top"] * 0.75
# # flower_distribution$m_per_item[flower_distribution$crown_bmt == "bot"] = flower_distribution$m_per_item[flower_distribution$crown_bmt == "bot"] * 1.25
# # flower_distribution$item_scaling[flower_distribution$compass %in% c("N", "NW", "NO")] = flower_distribution$item_scaling[flower_distribution$compass %in% c("N", "NW", "NO")] * 0.75
# # flower_distribution$item_scaling[flower_distribution$compass %in% c("S", "SW", "SO")] = flower_distribution$item_scaling[flower_distribution$compass %in% c("S", "SW", "SO")] * 1.25
# #
# # # set variables for item generation (in meters)
# # flower_poly <- create_item_poly(0.01)
# # flower_angle <- NULL # degree
# # stem_len <- 0.01
#
# ################################################################################
#
# # execution (no items)
# t <- Sys.time()
# result <- shade_tree(
#   qsm, sun_position, real_radiation, items = FALSE,
#   frames = NULL, overall = TRUE, monthly = FALSE, sequential = FALSE)
# Sys.time() - t
#
# # plot it and save it
# plot(result, col = rev(grey.colors(100)))
# writeRaster(result, paste0("grid_shadow_par_item-off_", start_str, "-", end_str, ".tif"), overwrite = TRUE)
# rm(result); gc()
#
# # execution (leaves)
# t <- Sys.time()
# result <- shade_tree(
#   qsm, sun_position, real_radiation, items = TRUE, item_type = "leaves",
#   item_distribution = leaf_distribution, cylinder_classes = cylinder_classes,
#   item_poly = leaf_poly, stem_len = stem_len, item_angle = leaf_angle,
#   frames = NULL, overall = TRUE, monthly = FALSE, sequential = FALSE)
# Sys.time() - t
#
# # # execution (flowers)
# # t <- Sys.time()
# # result <- shade_tree(
# #   qsm, sun_position, real_radiation, items = TRUE, item_type = "flowers",
# #   item_distribution = flower_distribution, cylinder_classes = cylinder_classes,
# #   item_poly = flower_poly, stem_len = stem_len, item_angle = flower_angle,
# #   frames = NULL, overall = TRUE, monthly = FALSE, sequential = FALSE)
# # Sys.time() - t
#
# # plot it and save it
# plot(result, col = rev(grey.colors(100)))
# writeRaster(result, paste0("grid_shadow_par_item-on_", start_str, "-", end_str, ".tif"), overwrite = TRUE)
# rm(result); gc()
#
# ################################################################################
#
# # RUNTIME: APPROX. 1 HOUR
#
# # t <- Sys.time()
# # # loop through months
# # for (m in 1:12) {
# #
# #   # progress
# #   print("------------------------")
# #   print(Sys.time())
# #   print(paste0("month: ", m))
# #   print("------------------------")
# #
# #   # subset sun positions
# #   start <- ISOdate(2014, m, 1, 0, 0)
# #   end <- ISOdate(ifelse(m < 12, 2014, 2015), ifelse(m < 12, m + 1, 1), 1, 0, 0) - lubridate:: minutes(1)
# #   sun_position_m <- sun_position[sun_position$timeframe >= start & sun_position$timeframe < end,]
# #
# #   # calculate monthly raster
# #   result_m <- shade_tree(
# #     qsm, sun_position_m, real_radiation, items = FALSE,
# #     item_distribution = NULL, cylinder_classes = NULL,
# #     item_poly = NULL, stem_len = NULL, item_angle = NULL, frames = NULL,
# #     overall = TRUE, monthly = FALSE)
# #
# #   # save monthly raster
# #   writeRaster(
# #     result_m, paste0("grid_shadow_item-off_", strftime(start,  format = "%d.%m.%Y"),
# #     "-", strftime(end,  format = "%d.%m.%Y"), ".tif"), overwrite = TRUE)
# #
# #   # clean storage
# #   gc()
# # }
# # Sys.time() - t
# #
# # # combine monthly rasters
# # rast_paths <- list.files(path_curr, "grid_shadow_item-off_")
# # rast_combined <- rast(lapply(rast_paths, rast))
# # rast_combined <- sum(rast_combined)
# # writeRaster(rast_combined, paste0("grid_shadow_item-off_combined.tif"), overwrite = TRUE)
#
# ################################################################################
#
# # rm(result_m, rast_paths, rast_combined); gc()
#
# ################################################################################
#
# # RUNTIME: APPROX. 1.5 HOURS
#
# # # set variables for item generation (in meters)
# # item_distribution <- data.frame(
# #   "diam_start_m" = 0:19 / 100,
# #   "diam_end_m" = 1:20 / 100,
# #   "m_per_item" = round(seq(0.01, 1, length.out = 20), 3),
# #   "item_scaling" = round(seq(0.15, 0.05, length.out = 20), 3))
# # item_poly <- as.matrix(data.frame(
# #   "x" = c(0, 0.03, 0.07, 0.10, 0.07, 0.03, 0),
# #   "y" = c(0, 0.04, 0.02, 0, -0.02, -0.04, 0),
# #   "z" = c(0, 0, 0, 0, 0, 0, 0))) / 10
# # stem_len <- 0.01
# # item_angle <- 45 # degree
# #
# # t <- Sys.time()
# # # loop through months
# # for (m in 1:12) {
# #
# #   # progress
# #   print("------------------------")
# #   print(Sys.time())
# #   print(paste0("month: ", m))
# #   print("------------------------")
# #
# #   # subset sun positions
# #   start <- ISOdate(2014, m, 1, 0, 0)
# #   end <- ISOdate(ifelse(m < 12, 2014, 2015), ifelse(m < 12, m + 1, 1), 1, 0, 0) - lubridate:: minutes(1)
# #   sun_position_m <- sun_position[sun_position$timeframe >= start & sun_position$timeframe < end,]
# #
# #   # set item parameters month-wise
# #   if (m < 4 | m > 9) {
# #     # jan, feb, mar, oct, nov, dec
# #     item_bool <- FALSE
# #   } else if  (m == 4) {
# #     # apr
# #     item_bool <- TRUE
# #     item_distribution$item_scaling <- round(seq(5, 5, length.out = 20), 3)
# #   } else if  (m == 5) {
# #     # may
# #     item_bool <- TRUE
# #     item_distribution$item_scaling <- round(seq(7, 5, length.out = 20), 3)
# #   } else if  (m == 6) {
# #     # jun
# #     item_bool <- TRUE
# #     item_distribution$item_scaling <- round(seq(9, 5, length.out = 20), 3)
# #   } else {
# #     # jul, aug, sep
# #     item_bool <- TRUE
# #     item_distribution$item_scaling <- round(seq(11, 5, length.out = 20), 3)
# #   }
# #
# #   # calculate monthly raster
# #   result_m <- shade_tree(
# #     qsm, sun_position_m, real_radiation, items = item_bool,
# #     item_distribution = item_distribution, cylinder_classes = cylinder_classes,
# #     item_poly = item_poly, stem_len = stem_len, item_angle = item_angle, frames = NULL,
# #     overall = TRUE, monthly = FALSE)
# #
# #   # save monthly raster
# #   writeRaster(
# #     result_m, paste0("grid_shadow_item-on_", strftime(start,  format = "%d.%m.%Y"),
# #                      "-", strftime(end,  format = "%d.%m.%Y"), ".tif"), overwrite = TRUE)
# #
# #   # clean storage
# #   gc()
# # }
# # Sys.time() - t
# #
# # # combine monthly rasters
# # rast_paths <- list.files(path_curr, "grid_shadow_item-on_")
# # rast_combined <- rast(lapply(rast_paths, rast))
# # rast_combined <- sum(rast_combined)
# # writeRaster(rast_combined, paste0("grid_shadow_item-on_combined.tif"), overwrite = TRUE)
