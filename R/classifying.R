################################################################################
# MAIN FUNCTIONS
################################################################################

# TODO: why are there NA values?

#' Classify crown cylinders into crown sections
#'
#' @description
#' \code{classify_crown} classifies the cylinders of a \code{QSM} object into
#' separate  crown sections. It can use a variable number of compass directions
#' and vertical sections and classify the crown into an outer and an inner
#' horizontal section.
#'
#' @param qsm An object of class \code{QSM}.
#' @param crs_epsg \code{integer}, EPSG code of the coordinate reference system.
#' @param compass_directions \code{integer}, number of compass directions.
#' @param min_cylinders_cbh \code{integer}, minimum number of cylinders of the lowest branch
#' defining crown base height.
#' @param vertical_sections \code{integer}, number of vertical crown sections.
#' @param outside_buffer_m \code{numeric}, width of the buffer defining the outer and inner
#' horizontal section in meters.
#'
#' @return
#' \code{matrix}, contains three columns: \code{compass} (compass directions),
#' \code{vertical} (vertical crown sections) and \code{horizontal} (horizontal
#' crown sections).
#'
#' @seealso \code{\link{add_items}}
#'
#' @examples
#' # load qsm
#' file_path <- system.file("extdata", "Prunus_avium_QSM_simplified.mat", package="qsm2shade")
#' qsm <- qsm2r::readQSM(file_path)
#'
#' # classify crown cylinders
#' classify_crown(qsm)
#' @export
classify_crown <- function(qsm, crs_epsg = 25832, compass_directions = 8,
                           min_cylinders_cbh = 3, vertical_sections = 3,
                           outside_buffer_m = 2) {

  # prepare tree data
  tree <- prepare_qsm(qsm, center = TRUE, keep_all = TRUE)

  # classification output
  cylinder_class <- c()

  # get xy-centers of cylinders
  cyl_center_x <- (tree[,"start_X"] + tree[,"end_X"]) / 2
  cyl_center_y <- (tree[,"start_Y"] + tree[,"end_Y"]) / 2
  cyl_center_z <- (tree[,"start_Z"] + tree[,"end_Z"]) / 2

  # PART 1: COMPASS DIRECTION --------------------------------------------------

  # set options
  compass_directions <- 8
  section_angle <- 360 / compass_directions

  # set section boundaries
  section_center <- seq(0, compass_directions - 1, 1) * section_angle
  section_lower  <- ifelse(section_center - section_angle / 2 < 0,
                           360 + section_center - section_angle / 2,
                           section_center - section_angle / 2)
  section_upper  <- section_center + section_angle / 2

  # create section lookup
  sections <- data.frame(
    "name" = c("N", "NO", "O", "SO", "S", "SW", "W", "NW"),
    "lower" = section_lower,
    "upper" = section_upper)

  # # stem base approach
  # stembase <- tree[1, c("start_X", "start_Y")]
  # cyl_azimuth <- rad2deg(atan2(cyl_center_x - stembase["start_X"],
  #                              cyl_center_y - stembase["start_Y"]))
  # cyl_azimuth <- ifelse(cyl_azimuth < 0, cyl_azimuth + 360, cyl_azimuth)

  # get crown center
  crown_center_x <- as.numeric((min(cyl_center_x) + max(cyl_center_x)) / 2)
  crown_center_y <- as.numeric((min(cyl_center_y) + max(cyl_center_y)) / 2)

  # calculate azimuth angle step base to cylinder (N = 0, O = 90, S = 180, W = 270)
  cyl_azimuth <- rad2deg(atan2(cyl_center_x - crown_center_x,
                               cyl_center_y - crown_center_y))
  cyl_azimuth <- ifelse(cyl_azimuth < 0, cyl_azimuth + 360, cyl_azimuth)

  # add according section name to cylinder
  cyl_compass <- lapply(1:nrow(tree), function(idx) {
    compass <- sections$name[sections$lower <= cyl_azimuth[idx] & sections$upper > cyl_azimuth[idx]]
    if (length(compass) == 0) {
      compass <- "N"
    }
    return(compass)
  })
  cyl_compass <- do.call(c, cyl_compass)

  # PART 2: CROWN BOT / MID / TOP ----------------------------------------------

  # add first order branch to each branch
  cyl_branches <- data.frame(cbind("ID" = 1:nrow(tree), tree, "FirstOrderBranch" = NA))
  cyl_branches$FirstOrderBranch[cyl_branches$BranchOrder == 0] <- 0
  for (main_branch in unique(cyl_branches[cyl_branches$BranchOrder == 1, "branch"])) {
    child_branch <- find_my_childs_recursive(cyl_branches, main_branch)
    cyl_branches$FirstOrderBranch[cyl_branches$branch %in% c(main_branch, child_branch)] <- main_branch
  }

  # get lowest first order branch with at least min_cylinders_cbh cylinders
  cyl_count <- table(cyl_branches$FirstOrderBranch[cyl_branches$BranchOrder > 0])
  lowest_cyl_id <- min(as.numeric(names(cyl_count[cyl_count >= min_cylinders_cbh])))
  lowest_cyl_z  <- cyl_branches$start_Z[cyl_branches$branch == lowest_cyl_id & cyl_branches$PositionInBranch == 1]

  # assign branches above this branch to crown
  # (those which are not crown will NOT receive leaves later on)
  cyl_branches$crown <- ifelse((cyl_branches$BranchOrder == 0 & cyl_branches$start_Z >= lowest_cyl_z) | # stem above crown base height
                                 (cyl_branches$BranchOrder > 0 & cyl_branches$FirstOrderBranch >= lowest_cyl_id), # branches above crown base height
                               TRUE, FALSE)
  cyl_branches$crown[is.na(cyl_branches$crown)] <- FALSE # handling "floating" cylinders

  # derive section boundaries
  vertical_sections <- 3
  lower_bound <- min(cyl_branches[cyl_branches$crown, c("start_Z", "end_Z")])
  upper_bound <- max(cyl_branches[cyl_branches$crown, c("start_Z", "end_Z")])
  section_bound <- seq(lower_bound, upper_bound, length.out = vertical_sections + 1)

  # create section lookup
  sections <- data.frame(
    "name" = c("bot", "mid", "top"),
    "lower"  = section_bound[1:vertical_sections],
    "upper"  = section_bound[2:(vertical_sections + 1)])

  # add section name to cylinder
  cyl_vertical <- lapply(1:nrow(tree), function(idx) {
    crown <- sections$name[sections$lower <= cyl_center_z[idx] & sections$upper > cyl_center_z[idx]]
    if (length(crown) == 0) {
      crown <- NA
    }
    return(crown)
  })
  cyl_vertical <- do.call(c, cyl_vertical)

  # PART 3: CROWN IN / OUT -----------------------------------------------------

  # derive section boundaries
  lower_bound <- min(cyl_branches[cyl_branches$crown, c("start_Z", "end_Z")])
  upper_bound <- max(cyl_branches[cyl_branches$crown, c("start_Z", "end_Z")])
  section_bound <- seq(lower_bound, upper_bound, 0.5)

  # create section lookup
  sections <- data.frame(
    "num" = 1:(length(section_bound) - 1),
    "lower"  = section_bound[1:(length(section_bound) - 1)],
    "upper"  = section_bound[2:length(section_bound)])

  # object for storage
  cyl_horizontal <- matrix(NA, nrow = nrow(tree))

  # loop through layers
  for (i in 1:nrow(sections)) {

    # get points in section
    lyr_idx <- which(cyl_center_z >= sections$lower[i] & cyl_center_z < sections$upper[i])

    # create convex hull around layer
    ch_idx <- chull(cbind(cyl_center_x[lyr_idx], cyl_center_y[lyr_idx]))
    ch_idx <- lyr_idx[c(ch_idx, ch_idx[1])]
    ch_crd <- cbind(id = sections$num[i], x = cyl_center_x[ch_idx], y = cyl_center_y[ch_idx])

    # get negative space inside 2m buffer
    ch_lin <- terra::vect(ch_crd, "polygons", crs = paste0("EPSG:", crs_epsg))
    ch_buff <- terra::buffer(ch_lin, -outside_buffer_m)

    # check if there is negative space
    if (terra::expanse(ch_buff) > 0) {

      # get indices of points within the negative space
      pts <- terra::vect(cbind(x = cyl_center_x[lyr_idx], y = cyl_center_y[lyr_idx]), "points", crs = paste0("EPSG:", crs_epsg))
      inside_idx <- terra::is.related(pts, ch_buff, "within")

      # within buffer -> outside crown | outside buffer -> inside crown
      cyl_horizontal[lyr_idx[inside_idx]] <- "in"
      cyl_horizontal[lyr_idx[!inside_idx]] <- "out"

    # no negative space -> all point outside crown
    } else {
      cyl_horizontal[lyr_idx] <- "out"
    }
  }

  # RETURN & COMBINE RESULTS ---------------------------------------------------

  # save in matrix
  cylinder_class <- cbind(cylinder_class,
                          "compass" = c(cyl_compass),
                          "vertical" = c(cyl_vertical),
                          "horizontal" = c(cyl_horizontal))

  # return results
  return(cylinder_class)
}

################################################################################
