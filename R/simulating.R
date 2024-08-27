################################################################################
# MAIN FUNCTIONS
################################################################################

#' Create single flower polygons
#'
#' @description
#' \code{create_flower} creates polygons of a single flower to be used
#' by \code{add_geoms()}.
#'
#' @param radius_m \code{numeric}, radius of the pentagon for the flower base
#' in meters.
#'
#' @return
#' \code{matrix}, contains coordinates of a single flower.
#'
#' @seealso \code{\link{add_geoms}}, \code{\link{plot_geom}}
#'
#' @examples
#' # create polygons for single flower
#' flower <- create_flower()
#' @export
create_flower <- function(radius_m = 0.01) {

  # geom base
  pentagon <- data.frame(
    "x" = c(0,
            radius_m*cos(qsm2shade:::deg2rad(18)),
            radius_m*cos(qsm2shade:::deg2rad(-54)),
            -radius_m*cos(qsm2shade:::deg2rad(-54)),
            -radius_m*cos(qsm2shade:::deg2rad(18))),
    "y" = c(radius_m,
            radius_m*sin(qsm2shade:::deg2rad(18)),
            radius_m*sin(qsm2shade:::deg2rad(-54)),
            radius_m*sin(qsm2shade:::deg2rad(-54)),
            radius_m*sin(qsm2shade:::deg2rad(18))),
    "z" = 0)
  pentagon <- rbind(pentagon, pentagon[1,])

  # geom petals
  petals <- list(pentagon)
  for (i in 1:5) {

    # start & end
    ps <- cbind(pentagon[i,1:2], "z" = 0) # start
    pe <- cbind(pentagon[i + 1,1:2], "z" = 0) # end

    # get direction vector
    pd <- pentagon[i,1:2] - pentagon[i + 1,1:2]

    # get orthogonal vector
    pco <- cbind(pd[2], -pd[1])

    # normalize orthogonal vector
    pcon <- pco[1,] / sqrt(pco[1,1]**2 + pco[1,2]**2) * radius_m

    # get petal corners
    pl <- cbind(pentagon[i,1:2] + pcon, "z" = radius_m) # left
    pr <- cbind(pentagon[i + 1,1:2] + pcon, "z" = radius_m) # right

    # get center between point and next point
    pc <- data.frame("x" = mean(pentagon[i:(i + 1),1]),
                     "y" = mean(pentagon[i:(i + 1),2]),
                     "z" = mean(pentagon[i:(i + 1),3]))

    # get mid between corners
    pm <- data.frame("x" = mean(c(pl[,1], pr[,1])),
                     "y" = mean(c(pl[,2], pr[,2])),
                     "z" = mean(c(pl[,3], pr[,3])))

    # add points to data frame
    petals[[i + 1]] <-  rbind(ps, pl, pr, pe, ps)
  }

  # convert to geom format
  flower_geom <- as.matrix(do.call(rbind, lapply(seq_along(petals), function(i) cbind(id = i, petals[[i]]))))

  # return polygon
  return(flower_geom)
}

################################################################################

#' Create single leaf polygon
#'
#' @description
#' \code{create_leaf} creates a polygon of a single leaf to be used
#' by \code{add_geoms()}.
#'
#' @param type \code{character}, shape of the leaf, should be one of
#' \code{c("normal", "heart", "feather")}.
#' @param length_m \code{numeric}, length of a single leaf in meters.
#'
#' @return
#' \code{matrix}, contains coordinates of a single leaf.
#'
#' @seealso \code{\link{add_geoms}}, \code{\link{plot_geom}}
#'
#' @examples
#' # create polygon for single leaf
#' leaf <- create_leaf()
#' @export
create_leaf <- function(type = c("normal", "heart", "feather"), length_m = 0.01) {

  # check input validity
  if (length(type) > 1 | !any(type %in% c("normal", "heart", "feather"))) {
    stop("geom_type must be 'normal', 'heart' or 'feather'")
  }

  # create polygons
  if (type == "normal") {
    leaf_poly <- data.frame(
      "x" = c(0, 3, 7, 10, 7, 3, 0),
      "y" = c(0, 4, 2, 0, -2, -4, 0),
      "z" = c(0, 0, 0, 0, 0, 0, 0)) / 100 / (0.10 / length_m)
  } else if (type == "heart") {
    leaf_poly <- data.frame(
      "x" = c(0, 5, 8, 9, 10, 8, 10, 9, 8, 5, 0),
      "y" = c(0, 5, 6, 4, 2, 0, -2, -4, -6, -5, 0),
      "z" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)) / 100 / (0.10 / length_m)
  } else if (type == "feather") {
    leaf_poly <- data.frame(
      "x" = c(0, 1, 3, 2.5, 0.2, 3.0, 4, 6, 5.5, 3.1, 5.5, 7, 10,  7,  5.5,  3.1,  5.5,  6,  4,  3.0,  0.2,  2.5,  3,  1, 0),
      "y" = c(0, 2, 3, 1.0, 0.1, 0.1, 2, 3, 1.0, 0.1, 0.1, 1, 00, -1, -0.1, -0.1, -1.0, -3, -2, -0.1, -0.1, -1.0, -3, -2, 0),
      "z" = c(0, 0, 0, 0.0, 0.0, 0.0, 0, 0, 0.0, 0.0, 0.0, 0, 00,  0,  0.0,  0.0,  0.0,  0,  0,  0.0,  0.0,  0.0,  0,  0, 0)) / 100 / (0.10 / length_m)
  }

  # convert to geom format
  leaf_geom <- as.matrix(cbind("id" = 1, leaf_poly))

  # return polygon
  return(leaf_geom)
}

################################################################################

#' Create a dummy geom distribution
#'
#' @description
#' \code{dummy_geom_distribution} creates a dummy geom distribution to be used
#' by \code{add_geoms()}. When setting \code{classification = TRUE}, 8 compass
#' directions, 2 horizontal sections and 3 vertical sections are used.
#'
#' @param cylinder_classes \code{boolean}, whether the variables from
#' \code{classify_crown()} should be included.
#'
#' @return
#' \code{data.frame}, contains an geom distribution.
#'
#' @seealso \code{\link{add_geoms}}
#'
#' @examples
#' # create dummy geom regression
#' distribution <- dummy_geom_distribution()
#' @export
dummy_geom_distribution <- function(cylinder_classes = FALSE) {

  # base structure
  if (!cylinder_classes) {
    dummy <- data.frame(
      "diam_start_m" = seq(0, 0.09, 0.01),
      "diam_end_m" = seq(0.01, 0.10, 0.01),
      "m_per_geom" = seq(0.01, 1, length.out = 10),
      "geom_scaling" = seq(1.5, 0.5, length.out = 10))

  } else {
    # add cylinder classes
    dummy <- expand.grid(
      compass = c("N", "NO", "O", "SO", "S", "SW", "W", "NW"),
      vertical = c("bot", "mid", "top"),
      horizontal = c("in", "out"),
      diam_start_m = seq(0, 0.09, 0.01))
    dummy$diam_end_m <- dummy$diam_start_m + 0.01
    dummy$m_per_geom <- rep(round(seq(0.05, 0.5, length.out = 10), 3), each = 48)
    dummy$geom_scaling <- rep(round(seq(3, 2, length.out = 10), 3), each = 48)
    dummy$m_per_geom[dummy$vertical == "top"] = dummy$m_per_geom[dummy$vertical == "top"] * 0.8
    dummy$m_per_geom[dummy$vertical == "bot"] = dummy$m_per_geom[dummy$vertical == "bot"] * 1.2
    dummy$geom_scaling[dummy$compass %in% c("N", "NW", "NO")] = dummy$geom_scaling[dummy$compass %in% c("N", "NW", "NO")] * 0.8
    dummy$geom_scaling[dummy$compass %in% c("S", "SW", "SO")] = dummy$geom_scaling[dummy$compass %in% c("S", "SW", "SO")] * 1.2
  }

  # return dummy
  return(dummy)
}

################################################################################

#' Add leaves or flowers to QSM
#'
#' @description
#' \code{add_geoms} simulates leaves or flowers for a \code{QSM} object.
#'
#' @param qsm An object of class \code{QSM}.
#' @param geom_distribution \code{data.frame}, describes the geom distribution,
#' with columns \code{c("diam_start_m", "diam_end_m", "m_per_geom",
#' "geom_scaling")}, optionally including variables from \code{classify_crown()}.
#' @param geom \code{matrix}, contains matrix with IDs and coordinates.
#' @param stem_len \code{numeric}, geom stem length.
#' @param geom_type \code{character}, type of added geoms, either
#' \code{"leaves"} or \code{"flowers"}
#' @param geom_angle \code{numeric}, the angle between the ground and the leaf
#' axis in degrees when simulating leaves.
#' @param cylinder_classes \code{matrix} (optional), crown classification of
#' each cylinder when using an geom distribution including variables from
#' \code{classify_crown()}.
#' @param add_noise \code{numeric} (optional), fraction of the stem length used
#' for adding noise to the geom location and stem length, 0 means no noise.
#'
#' @return
#' \code{matrix}, contains coordinates of the simulated geoms. For each polygon
#' node of the initial \code{geom}, three columns for the xyz-coordinates
#' are given.
#'
#' @seealso \code{\link{dummy_geom_distribution}},
#' \code{\link{create_flower}}, \code{\link{create_leaf}}
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
#' # create polygons for single geom
#' flower <- create_flower()
#'
#' # without crown classification:
#'
#' # create dummy geom regression
#' distribution <- dummy_geom_distribution()
#'
#' # add geoms
#' add_geoms(qsm, distribution, flower, geom_type = "flower")
#'
#' # with crown classification:
#'
#' # classify crown cylinders
#' classes <- classify_crown(qsm, compass_directions = 8, outside_buffer_m = 2, vertical_sections = 3)
#'
#' # create dummy geom regression
#' distribution_classes <- dummy_geom_distribution(classification = TRUE)
#'
#' # add geoms
#' add_geoms(qsm, distribution_classes, flower, geom_type = "flower", cylinder_classes = classes)
#' @export
#' @importFrom data.table set
add_geoms <- function(qsm, geom_distribution, geom, stem_len = 0.01,
                      geom_type = c("leaf", "flower"), geom_angle = 45,
                      cylinder_classes = NULL, add_noise = 0) {

  # check input validity
  if (length(geom_type) > 1 | !any(geom_type %in% c("leaf", "leaves", "flower", "flowers"))) {
    stop("geom_type must be 'leaf' or 'flower'")
  }

  # prepare tree data
  tree <- qsm2shade:::prepare_qsm(qsm, keep_all = FALSE)

  # add diameter class to each cylinder
  class_width <- unique(round(geom_distribution$diam_end_m - geom_distribution$diam_start_m, 4))
  tree <- cbind(tree, diam_start_m = plyr::round_any(tree[,"radius"]*2, class_width, f = floor))

  # add geom_distribution data to each cylinder
  if (is.null(cylinder_classes)) {
    tree <- as.matrix(merge(tree, geom_distribution[,c("diam_start_m", "m_per_geom", "geom_scaling")], by = "diam_start_m"))

  } else {
    # add geom_distribution data based on cylinder classifications
    cylinder_classes <- cbind(id = 1:nrow(tree), diam_start_m = tree[,"diam_start_m"], cylinder_classes)
    cylinder_classes <- as.matrix(merge(cylinder_classes, geom_distribution, by = colnames(cylinder_classes)[2:ncol(cylinder_classes)], all.x = TRUE))
    cylinder_classes <- cylinder_classes[order(as.numeric(cylinder_classes[,"id"])),]
    cylinder_classes <- matrix(as.numeric(cylinder_classes[,c("m_per_geom", "geom_scaling")]), ncol = 2)
    colnames(cylinder_classes) <- c("m_per_geom", "geom_scaling")
    tree <- cbind(tree, cylinder_classes)
  }

  # non-crown cylinders don't get geoms
  tree <- na.omit(tree)

  # add number  of geoms to cylinder
  tree <- cbind(tree, "geom_p" = tree[,"length"] / tree[,"m_per_geom"])
  tree <- cbind(tree, "geoms" = ifelse(
    tree[,"geom_p"] >= 1,
    round(tree[,"geom_p"]),
    rbinom(length(tree[,"geom_p"]), 1, tree[,"geom_p"] %% 1))) # modulo 1 to avoid warnings

  # remove unnecessary columns
  tree <- tree[,!colnames(tree) %in% c("diam_start_m")]

  # get stem starting points
  tree_geoms <- tree[tree[,"geoms"] > 0,]
  tree_geoms <- lapply(1:nrow(tree_geoms), function(idx) {
    sub <- tree_geoms[idx,]

    # set lengths at which there are geoms
    # (must be shifted by 0.5 * m_per_geom for rounded up geom numbers)
    num_geoms <- sub["geoms"]
    length <- sub["m_per_geom"] * 1:num_geoms - 0.5 * sub["m_per_geom"]

    # create noise (stem base)
    if (add_noise > 0) {
      length <- length + runif(length(length), min = -stem_len * add_noise, max = +stem_len * add_noise)
    }

    # check if length smaller than cylinder length
    if (sub["geoms"] == 1 & sub["geom_p"] < 1) {
      length <- sub["length"] / 2
    }

    # create new matrix with one row per geom
    return(cbind(
      matrix(tree_geoms[rep(idx, num_geoms),c("radius", "geom_scaling", "axis_X", "axis_Y", "axis_Z")], ncol = 5),
      sub["start_X"] + sub["axis_X"] * length,
      sub["start_Y"] + sub["axis_Y"] * length,
      sub["start_Z"] + sub["axis_Z"] * length))
  })
  tree_geoms <- do.call(rbind, tree_geoms)
  colnames(tree_geoms) <- c("radius", "geom_scaling",
                            "cyl_axis_X", "cyl_axis_Y", "cyl_axis_Z",
                            "stem_start_X", "stem_start_Y", "stem_start_Z")

  # get stem axis by creating vector orthogonal to cylinder axis at stem point
  stem_axis_X <- rnorm(nrow(tree_geoms))
  stem_axis_Y <- rnorm(nrow(tree_geoms))
  stem_axis_Z <- (-tree_geoms[,"cyl_axis_X"] * stem_axis_X - tree_geoms[,"cyl_axis_Y"] * stem_axis_Y) / tree_geoms[,"cyl_axis_Z"]

  # remove unnecessary columns
  tree_geoms <- tree_geoms[,!colnames(tree_geoms) %in% c("cyl_axis_X", "cyl_axis_Y", "cyl_axis_Z")]

  # normalize stem axis by dividing it by its length
  stem_axis_len <- sqrt(stem_axis_X**2 + stem_axis_Y**2 + stem_axis_Z**2) # 3D length
  tree_geoms <- cbind(
    tree_geoms,
    "stem_axis_X" = stem_axis_X / stem_axis_len,
    "stem_axis_Y" = stem_axis_Y / stem_axis_len,
    "stem_axis_Z" = stem_axis_Z / stem_axis_len)
  rm(stem_axis_len, stem_axis_X, stem_axis_Y, stem_axis_Z)

  # add noise (stem length)
  if (add_noise > 0) {
    stem_len <- stem_len + runif(nrow(tree_geoms), min = -stem_len * add_noise, max = +stem_len * add_noise)
  }

  # get geom starting points
  tree_geoms <- cbind(
    tree_geoms,
    "geom_base_X" = tree_geoms[,"stem_start_X"] + (tree_geoms[,"radius"] + stem_len) * tree_geoms[,"stem_axis_X"],
    "geom_base_Y" = tree_geoms[,"stem_start_Y"] + (tree_geoms[,"radius"] + stem_len) * tree_geoms[,"stem_axis_Y"],
    "geom_base_Z" = tree_geoms[,"stem_start_Z"] + (tree_geoms[,"radius"] + stem_len) * tree_geoms[,"stem_axis_Z"])

  # remove unnecessary columns
  tree_geoms <- tree_geoms[,!colnames(tree_geoms) %in% c("stem_start_X", "stem_start_Y", "stem_start_Z", "radius")]

  # leaf rotation (leaf axis at an angle to leaf stem)
  if (geom_type %in% c("leaf", "leaves")) {

    # get geom axis (x and y the same as stem axis, but z is changed)
    geom_degree <- geom_angle / -45
    geom_axis_len <- sqrt(tree_geoms[,"stem_axis_X"]**2 + tree_geoms[,"stem_axis_Y"]**2) # 2D length
    geom_axis_X <- tree_geoms[,"stem_axis_X"]
    geom_axis_Y <- tree_geoms[,"stem_axis_Y"]
    geom_axis_Z <- geom_degree * geom_axis_len # oder: nur für 1 Zylinder berechnen

    # remove unnecessary columns
    tree_geoms <- tree_geoms[,!colnames(tree_geoms) %in% c("stem_axis_X", "stem_axis_Y", "stem_axis_Z")]

    # normalize geom axis by dividing it by its length
    geom_axis_len <- sqrt(geom_axis_X**2 + geom_axis_Y**2 + geom_axis_Z**2) # 3D length
    tree_geoms <- cbind(
      tree_geoms,
      "geom_axis_X" = geom_axis_X / geom_axis_len,
      "geom_axis_Y" = geom_axis_Y / geom_axis_len,
      "geom_axis_Z" = geom_axis_Z / geom_axis_len)
    rm(geom_axis_X, geom_axis_Y, geom_axis_Z)

    # get target axes
    vec_axis <- cbind(tree_geoms[,c("geom_axis_X", "geom_axis_Y", "geom_axis_Z")])
    vec_orth <- cbind(tree_geoms[,"geom_axis_Y"], -tree_geoms[,"geom_axis_X"], 0)
    vec_norm <- matrix(NA, nrow = nrow(tree_geoms), ncol = 3)
    vec_norm[,1] <- vec_orth[,2] * vec_axis[,3] - vec_orth[,3] * vec_axis[,2]
    vec_norm[,2] <- vec_orth[,3] * vec_axis[,1] - vec_orth[,1] * vec_axis[,3]
    vec_norm[,3] <- vec_orth[,1] * vec_axis[,2] - vec_orth[,2] * vec_axis[,1]

    # normalize vectors
    vec_orth <- vec_orth / sqrt(vec_orth[,1]**2 + vec_orth[,2]**2 + vec_orth[,3]**2)
    vec_axis <- vec_axis / sqrt(vec_axis[,1]**2 + vec_axis[,2]**2 + vec_axis[,3]**2)
    vec_norm <- vec_norm / sqrt(vec_norm[,1]**2 + vec_norm[,2]**2 + vec_norm[,3]**2)

    # arrange rotation matrix
    mat_rotation_x <- cbind(vec_axis[,1], vec_orth[,1], vec_norm[,1])
    mat_rotation_y <- cbind(vec_axis[,2], vec_orth[,2], vec_norm[,2])
    mat_rotation_z <- cbind(vec_axis[,3], vec_orth[,3], vec_norm[,3])
    rm(vec_orth, vec_axis, vec_norm)

  } else if (geom_type %in% c("flower", "flowers")) {
    # flower rotation (flower base orthogonal to flower stem)

    # get target axes
    vec_axis <- cbind(tree_geoms[,c("stem_axis_X", "stem_axis_Y", "stem_axis_Z")])
    vec_orth <- cbind(tree_geoms[,"stem_axis_Y"], -tree_geoms[,"stem_axis_X"], 0)
    vec_norm <- matrix(NA, nrow = nrow(tree_geoms), ncol = 3)
    vec_norm[,1] <- vec_orth[,2] * vec_axis[,3] - vec_orth[,3] * vec_axis[,2]
    vec_norm[,2] <- vec_orth[,3] * vec_axis[,1] - vec_orth[,1] * vec_axis[,3]
    vec_norm[,3] <- vec_orth[,1] * vec_axis[,2] - vec_orth[,2] * vec_axis[,1]

    # normalize vectors
    vec_orth <- vec_orth / sqrt(vec_orth[,1]**2 + vec_orth[,2]**2 + vec_orth[,3]**2)
    vec_axis <- vec_axis / sqrt(vec_axis[,1]**2 + vec_axis[,2]**2 + vec_axis[,3]**2)
    vec_norm <- vec_norm / sqrt(vec_norm[,1]**2 + vec_norm[,2]**2 + vec_norm[,3]**2)

    # arrange rotation matrix
    mat_rotation_x <- cbind(vec_norm[,1], vec_orth[,1], vec_axis[,1])
    mat_rotation_y <- cbind(vec_norm[,2], vec_orth[,2], vec_axis[,2])
    mat_rotation_z <- cbind(vec_norm[,3], vec_orth[,3], vec_axis[,3])
    rm(vec_orth, vec_axis, vec_norm)
  }

  # loop through different item sizes
  unique_scales <- unique(tree_geoms[,"geom_scaling"])
  done_geoms <- lapply(unique_scales, function(curr_size) {

    # get indices of items with this dimension
    curr_idx <- tree_geoms[,"geom_scaling"] == curr_size
    curr_idx_num <- which(curr_idx)

    # subset offsets
    offset_x_curr <- tree_geoms[curr_idx, "geom_base_X"]
    offset_y_curr <- tree_geoms[curr_idx, "geom_base_Y"]
    offset_z_curr <- tree_geoms[curr_idx, "geom_base_Z"]

    # subset rotation matrices
    mat_rotation_x_curr <- mat_rotation_x[curr_idx,]
    mat_rotation_y_curr <- mat_rotation_y[curr_idx,]
    mat_rotation_z_curr <- mat_rotation_z[curr_idx,]

    # get required polygons
    geom_scaled <- geom
    geom_scaled[,2:4] <- geom_scaled[,2:4] * curr_size

    # loop through polygon nodes
    geom_nodes <- lapply(1:nrow(geom_scaled), function(node_idx) {

      # return zeros for (0|0|0)
      if (all(geom_scaled[node_idx,] == 0)) {
        poly_x <- poly_y <- poly_z <- matrix(0, ncol = 1, nrow = sum(curr_idx))

      } else {
        # rotate coordinates
        poly_xyz <- geom_scaled[node_idx,2:4]
        poly_x <- mat_rotation_x_curr %*% poly_xyz
        poly_y <- mat_rotation_y_curr %*% poly_xyz
        poly_z <- mat_rotation_z_curr %*% poly_xyz
      }

      # translate coordinates
      poly_x <- poly_x + offset_x_curr
      poly_y <- poly_y + offset_y_curr
      poly_z <- poly_z + offset_z_curr

      # return coordinates
      return(cbind(curr_idx_num, geom_scaled[node_idx,1], node_idx, poly_x, poly_y, poly_z))
      })
    return(do.call(rbind, geom_nodes))
  })
  done_geoms <- do.call(rbind, done_geoms)
  colnames(done_geoms) <- c("object", "polygon", "node", "x", "y", "z")

  # sort data
  done_geoms <- done_geoms[order(done_geoms[,"object"], done_geoms[,"polygon"], done_geoms[,"node"]),]

  # unique id
  n_reps <- nrow(tree_geoms)
  offsets <- seq(0, by = length(unique(geom[,1])), length.out = n_reps)
  ids <- rep(geom[,1], times = n_reps) + rep(offsets, each = nrow(geom))
  done_geoms <- cbind("id" = ids, done_geoms[, c("x", "y", "z")])

  # return sorted data
  return(done_geoms)
}

################################################################################
