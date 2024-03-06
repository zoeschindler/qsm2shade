################################################################################
# MAIN FUNCTIONS
################################################################################

#' Create single flower polygons
#'
#' @description
#' \code{create_single_flower} creates polygons of a single flower to be used
#' by \code{add_item()}.
#'
#' @param radius_m \code{numeric}, radius of the pentagon for the flower base
#' in meters.
#'
#' @return
#' \code{list}, each item contains a \code{data.frame} with coordinates of one
#' of the polygons of a single flower.
#'
#' @seealso \code{\link{add_item}}, \code{\link{plot_single_item}}
#'
#' @examples
#' # create polygons for single flower
#' flower <- create_single_flower()
#' @export
create_single_flower <- function(radius_m = 0.01) {

  # item base
  pentagon <- data.frame(
    "x" = c(0, radius_m*cos(deg2rad(18)), radius_m*cos(deg2rad(-54)), -radius_m*cos(deg2rad(-54)), -radius_m*cos(deg2rad(18))),
    "y" = c(radius_m, radius_m*sin(deg2rad(18)), radius_m*sin(deg2rad(-54)),  radius_m*sin(deg2rad(-54)),  radius_m*sin(deg2rad(18)))
  )
  pentagon[,"z"] <- 0
  pentagon <- rbind(pentagon, pentagon[1,])

  # item petals
  item_poly <- list(pentagon)
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
    # get petal axis
    pa <- pm - pc

    # normalize petal axis
    pan <- pa[1,] / sqrt(pa[1,1]**2 + pa[1,2]**2 + pa[1,3]**2) * radius_m

    # derive petal tip
    pt <- pm + pa / 2 # tip (1/2 of length)

    # add points to data frame
    item_poly[[i]] <-  rbind(ps, pl, pt, pr, pe, ps)
  }

  # combine petals and base
  item_poly[[6]] <- pentagon

  # return polygon
  return(item_poly)
}

################################################################################

#' Create single leaf polygon
#'
#' @description
#' \code{create_single_leaf} creates a polygon of a single leaf to be used
#' by \code{add_item()}.
#'
#' @param leaf_type \code{character}, shape of the leaf, should be one of
#' \code{c("normal", "heart", "feather")}.
#' @param length_m \code{numeric}, length of a single leaf in meters.
#'
#' @return
#' \code{data.frame}, contains coordinates of a single leaf.
#'
#' @seealso \code{\link{add_item}}, \code{\link{plot_single_item}}
#'
#' @examples
#' # create polygon for single leaf
#' leaf <- create_single_leaf()
#' @export
create_single_leaf <- function(leaf_type = c("normal", "heart", "feather"), length_m = 0.01) {

  # check input validity
  if (length(leaf_type) > 1 | !any(leaf_type %in% c("normal", "heart", "feather"))) {
    stop("item_type must be 'normal', 'heart' or 'feather'")
  }

  # create polygons
  if (leaf_type == "normal") {
    leaf_poly <- data.frame(
      "x" = c(0, 3, 7, 10, 7, 3, 0),
      "y" = c(0, 4, 2, 0, -2, -4, 0),
      "z" = c(0, 0, 0, 0, 0, 0, 0)) / 100 / (0.10 / length_m)
  } else if (leaf_type == "heart") {
    leaf_poly <- data.frame(
      "x" = c(0, 5, 8, 9, 10, 8, 10, 9, 8, 5, 0),
      "y" = c(0, 5, 6, 4, 2, 0, -2, -4, -6, -5, 0),
      "z" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)) / 100 / (0.10 / length_m)
  } else if (leaf_type == "feather") {
    leaf_poly <- data.frame(
      "x" = c(0, 1, 3, 2.5, 0.2, 3.0, 4, 6, 5.5, 3.1, 5.5, 7, 10,  7,  5.5,  3.1,  5.5,  6,  4,  3.0,  0.2,  2.5,  3,  1, 0),
      "y" = c(0, 2, 3, 1.0, 0.1, 0.1, 2, 3, 1.0, 0.1, 0.1, 1, 00, -1, -0.1, -0.1, -1.0, -3, -2, -0.1, -0.1, -1.0, -3, -2, 0),
      "z" = c(0, 0, 0, 0.0, 0.0, 0.0, 0, 0, 0.0, 0.0, 0.0, 0, 00,  0,  0.0,  0.0,  0.0,  0,  0,  0.0,  0.0,  0.0,  0,  0, 0)) / 100 / (0.10 / length_m)
  }

  # return polygon
  return(leaf_poly)
}

################################################################################

#' Create a dummy item distribution
#'
#' @description
#' \code{dummy_item_distribution} creates a dummy item distribution to be used
#' by \code{add_item()}. When setting \code{classification = TRUE}, 8 compass
#' directions, 2 horizontal sections and 3 vertical sections are used.
#'
#' @param classification \code{boolean}, whether the variables from
#' \code{classify_crown()} should be included.
#'
#' @return
#' \code{data.frame}, contains an item distribution.
#'
#' @seealso \code{\link{add_item}}
#'
#' @examples
#' # create dummy item regression
#' distribution <- dummy_item_distribution()
#' @export
dummy_item_distribution <- function(classification = FALSE) {

  # base structure
  if (!classification) {
    dummy <- data.frame(
      "diam_start_m" = seq(0, 0.09, 0.01),
      "diam_end_m" = seq(0.01, 0.10, 0.01),
      "m_per_item" = seq(0.01, 1, length.out = 10),
      "item_scaling" = seq(1.5, 0.5, length.out = 10))

  } else {
    # add classification
    dummy <- expand.grid(
      compass = c("N", "NO", "O", "SO", "S", "SW", "W", "NW"),
      vertical = c("bot", "mid", "top"),
      horizontal = c("in", "out"),
      diam_start_m = seq(0, 0.09, 0.01))
    dummy$diam_end_m <- dummy$diam_start_m + 0.01
    dummy$m_per_item <- rep(round(seq(0.05, 0.5, length.out = 10), 3), each = 48)
    dummy$item_scaling <- rep(round(seq(3, 2, length.out = 10), 3), each = 48)
    dummy$m_per_item[dummy$vertical == "top"] = dummy$m_per_item[dummy$vertical == "top"] * 0.8
    dummy$m_per_item[dummy$vertical == "bot"] = dummy$m_per_item[dummy$vertical == "bot"] * 1.2
    dummy$item_scaling[dummy$compass %in% c("N", "NW", "NO")] = dummy$item_scaling[dummy$compass %in% c("N", "NW", "NO")] * 0.8
    dummy$item_scaling[dummy$compass %in% c("S", "SW", "SO")] = dummy$item_scaling[dummy$compass %in% c("S", "SW", "SO")] * 1.2
  }

  # return dummy
  return(dummy)
}

################################################################################

#' Add leaves or flowers to QSM
#'
#' @description
#' \code{add_items} simulates leaves or flowers for a \code{QSM} object.
#'
#' @param qsm An object of class \code{QSM}.
#' @param item_distribution \code{data.frame}, describes the item distribution,
#' with columns \code{c("diam_start_m", "diam_end_m", "m_per_item",
#' "item_scaling")}, optionally including variables from \code{classify_crown()}.
#' @param item_poly \code{matrix} or \code{list}, item to be simulated,
#' contains coordinates of a single polygon in a matrix or a list of matrices.
#' @param stem_len \code{numeric}, item stem length.
#' @param item_type \code{character}, type of added items, either
#' \code{"leaves"} or \code{"flowers"}
#' @param item_angle \code{numeric}, the angle between the ground and the leaf
#' axis in degrees when simulating leaves.
#' @param cylinder_classes \code{matrix} (optional), crown classification of
#' each cylinder when using an item distribution including variables from
#' \code{classify_crown()}.
#' @param add_noise \code{numeric} (optional), fraction of the stem length used
#' for adding noise to the item location and stem length, 0 means no noise.
#'
#' @return
#' \code{matrix}, contains coordinates of the simulated items. For each polygon
#' node of the initial \code{item_poly}, three columns for the xyz-coordinates
#' are given.
#'
#' @seealso \code{\link{dummy_item_distribution}},
#' \code{\link{create_single_flower}}, \code{\link{create_single_leaf}}
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
#' # create polygons for single item
#' flower <- create_single_flower()
#'
#' # without classification:
#'
#' # create dummy item regression
#' distribution <- dummy_item_distribution()
#'
#' # add items
#' add_items(qsm, distribution, flower, item_type = "flowers")
#'
#' # with classification:
#'
#' # classify crown cylinders
#' classes <- classify_crown(qsm, compass_directions = 8, outside_buffer_m = 2, vertical_sections = 3)
#'
#' # create dummy item regression
#' distribution_classes <- dummy_item_distribution(classification = TRUE)
#'
#' # add items
#' add_items(qsm, distribution_classes, flower, item_type = "flowers", cylinder_classes = classes)
#' @export
add_items <- function(qsm, item_distribution, item_poly, stem_len = 0.01,
                      item_type = c("leaves", "flowers"), item_angle = 45,
                      cylinder_classes = NULL, add_noise = 0) {

  # check input validity
  if (length(item_type) > 1 | !any(item_type %in% c("leaves", "flowers"))) {
    stop("item_type must be 'leaves' or 'flowers'")
  }

  # prepare tree data
  tree <- prepare_qsm(qsm, keep_all = FALSE)

  # add diameter class to each cylinder
  class_width <- unique(round(item_distribution$diam_end_m - item_distribution$diam_start_m, 4))
  tree <- cbind(tree, diam_start_m = plyr::round_any(tree[,"radius"]*2, class_width, f = floor))

  # add item_distribution data to each cylinder
  if (is.null(cylinder_classes)) {
    tree <- as.matrix(merge(tree, item_distribution[,c("diam_start_m", "m_per_item", "item_scaling")], by = "diam_start_m"))

  } else {
    # add item_distribution data based on cylinder classifications
    cylinder_classes <- cbind(id = 1:nrow(tree), diam_start_m = tree[,"diam_start_m"], cylinder_classes)
    cylinder_classes <- as.matrix(merge(cylinder_classes, item_distribution, by = colnames(cylinder_classes)[2:ncol(cylinder_classes)], all.x = TRUE))
    cylinder_classes <- cylinder_classes[order(as.numeric(cylinder_classes[,"id"])),]
    cylinder_classes <- matrix(as.numeric(cylinder_classes[,c("m_per_item", "item_scaling")]), ncol = 2)
    colnames(cylinder_classes) <- c("m_per_item", "item_scaling")
    tree <- cbind(tree, cylinder_classes)
  }

  # non-crown cylinders don't get items
  tree <- na.omit(tree)

  # add number  of items to cylinder
  tree <- cbind(tree, "item_p" = tree[,"length"] / tree[,"m_per_item"])
  tree <- cbind(tree, "items" = ifelse(
    tree[,"item_p"] >= 1,
    round(tree[,"item_p"]),
    rbinom(length(tree[,"item_p"]), 1, tree[,"item_p"] %% 1))) # modulo 1 to avoid warnings

  # remove unnecessary columns
  tree <- tree[,!colnames(tree) %in% c("diam_start_m")]

  # get stem starting points
  tree_items <- tree[tree[,"items"] > 0,]
  tree_items <- lapply(1:nrow(tree_items), function(idx) {
    sub <- tree_items[idx,]

    # set lengths at which there are items
    # (must be shifted by 0.5 * m_per_item for rounded up item numbers)
    num_items <- sub["items"]
    length <- sub["m_per_item"] * 1:num_items - 0.5 * sub["m_per_item"]

    # create noise (stem base)
    if (add_noise > 0) {
      length <- length + runif(length(length), min = -stem_len * add_noise, max = +stem_len * add_noise)
    }

    # check if length smaller than cylinder length
    if (sub["items"] == 1 & sub["item_p"] < 1) {
      length <- sub["length"] / 2
    }

    # create new matrix with one row per item
    return(cbind(
      t(replicate(num_items, sub[c("radius", "item_scaling", "axis_X", "axis_Y", "axis_Z")])),
      as.numeric(sub["start_X"] + sub["axis_X"] * length),
      as.numeric(sub["start_Y"] + sub["axis_Y"] * length),
      as.numeric(sub["start_Z"] + sub["axis_Z"] * length)))
  })
  tree_items <- do.call(rbind, tree_items)
  colnames(tree_items) <- c("radius", "item_scaling",
                            "cyl_axis_X", "cyl_axis_Y", "cyl_axis_Z",
                            "stem_start_X", "stem_start_Y", "stem_start_Z")

  # get stem axis by creating vector orthogonal to cylinder axis at stem point
  stem_axis_X <- rnorm(nrow(tree_items))
  stem_axis_Y <- rnorm(nrow(tree_items))
  stem_axis_Z <- (-tree_items[,"cyl_axis_X"] * stem_axis_X - tree_items[,"cyl_axis_Y"] * stem_axis_Y) / tree_items[,"cyl_axis_Z"]

  # remove unnecessary columns
  tree_items <- tree_items[,!colnames(tree_items) %in% c("cyl_axis_X", "cyl_axis_Y", "cyl_axis_Z")]

  # normalize stem axis by dividing it by its length
  stem_axis_len <- sqrt(stem_axis_X**2 + stem_axis_Y**2 + stem_axis_Z**2) # 3D length
  tree_items <- cbind(
    tree_items,
    "stem_axis_X" = stem_axis_X / stem_axis_len,
    "stem_axis_Y" = stem_axis_Y / stem_axis_len,
    "stem_axis_Z" = stem_axis_Z / stem_axis_len)
  rm(stem_axis_len, stem_axis_X, stem_axis_Y, stem_axis_Z)

  # add noise (stem length)
  if (add_noise > 0) {
    stem_len <- stem_len + runif(nrow(tree_items), min = -stem_len * add_noise, max = +stem_len * add_noise)
  }

  # get item starting points
  tree_items <- cbind(
    tree_items,
    "item_base_X" = tree_items[,"stem_start_X"] + (tree_items[,"radius"] + stem_len) * tree_items[,"stem_axis_X"],
    "item_base_Y" = tree_items[,"stem_start_Y"] + (tree_items[,"radius"] + stem_len) * tree_items[,"stem_axis_Y"],
    "item_base_Z" = tree_items[,"stem_start_Z"] + (tree_items[,"radius"] + stem_len) * tree_items[,"stem_axis_Z"])

  # remove unnecessary columns
  tree_items <- tree_items[,!colnames(tree_items) %in% c("stem_start_X", "stem_start_Y", "stem_start_Z", "radius")]

  # leaf rotation (leaf axis at an angle to leaf stem)
  if (item_type == "leaves") {

    # get item axis (x and y the same as stem axis, but z is changed)
    item_degree <- item_angle / -45
    item_axis_len <- sqrt(tree_items[,"stem_axis_X"]**2 + tree_items[,"stem_axis_Y"]**2) # 2D length
    item_axis_X <- tree_items[,"stem_axis_X"]
    item_axis_Y <- tree_items[,"stem_axis_Y"]
    item_axis_Z <- item_degree * item_axis_len # oder: nur für 1 Zylinder berechnen

    # remove unnecessary columns
    tree_items <- tree_items[,!colnames(tree_items) %in% c("stem_axis_X", "stem_axis_Y", "stem_axis_Z")]

    # normalize item axis by dividing it by its length
    item_axis_len <- sqrt(item_axis_X**2 + item_axis_Y**2 + item_axis_Z**2) # 3D length
    tree_items <- cbind(
      tree_items,
      "item_axis_X" = item_axis_X / item_axis_len,
      "item_axis_Y" = item_axis_Y / item_axis_len,
      "item_axis_Z" = item_axis_Z / item_axis_len)
    rm(item_axis_X, item_axis_Y, item_axis_Z)

    # get target axes
    vec_axis <- cbind(tree_items[,c("item_axis_X", "item_axis_Y", "item_axis_Z")])
    vec_orth <- cbind(tree_items[,"item_axis_Y"], -tree_items[,"item_axis_X"], 0)
    vec_norm <- matrix(NA, nrow = nrow(tree_items), ncol = 3)
    vec_norm[,1] <- vec_orth[,2] * vec_axis[,3] - vec_orth[,3] * vec_axis[,2]
    vec_norm[,2] <- vec_orth[,3] * vec_axis[,1] - vec_orth[,1] * vec_axis[,3]
    vec_norm[,3] <- vec_orth[,1] * vec_axis[,2] - vec_orth[,2] * vec_axis[,1]

    # normalize vectors
    vec_orth <- vec_orth / sqrt(vec_orth[,1]**2 + vec_orth[,2]**2 + vec_orth[,3]**2)
    vec_axis <- vec_axis / sqrt(vec_axis[,1]**2 + vec_axis[,2]**2 + vec_axis[,3]**2)
    vec_norm <- vec_norm / sqrt(vec_norm[,1]**2 + vec_norm[,2]**2 + vec_norm[,3]**2)

    # arrange rotation matrix
    mat_rotation_x <- matrix(c(vec_axis[,1], vec_orth[,1], vec_norm[,1]), ncol = 3)
    mat_rotation_y <- matrix(c(vec_axis[,2], vec_orth[,2], vec_norm[,2]), ncol = 3)
    mat_rotation_z <- matrix(c(vec_axis[,3], vec_orth[,3], vec_norm[,3]), ncol = 3)
    rm(vec_orth, vec_axis, vec_norm)

  } else if (item_type == "flowers") {
    # flower rotation (flower base orthogonal to flower stem)

    # get target axes
    vec_axis <- cbind(tree_items[,c("stem_axis_X", "stem_axis_Y", "stem_axis_Z")])
    vec_orth <- cbind(tree_items[,"stem_axis_Y"], -tree_items[,"stem_axis_X"], 0)
    vec_norm <- matrix(NA, nrow = nrow(tree_items), ncol = 3)
    vec_norm[,1] <- vec_orth[,2] * vec_axis[,3] - vec_orth[,3] * vec_axis[,2]
    vec_norm[,2] <- vec_orth[,3] * vec_axis[,1] - vec_orth[,1] * vec_axis[,3]
    vec_norm[,3] <- vec_orth[,1] * vec_axis[,2] - vec_orth[,2] * vec_axis[,1]

    # normalize vectors
    vec_orth <- vec_orth / sqrt(vec_orth[,1]**2 + vec_orth[,2]**2 + vec_orth[,3]**2)
    vec_axis <- vec_axis / sqrt(vec_axis[,1]**2 + vec_axis[,2]**2 + vec_axis[,3]**2)
    vec_norm <- vec_norm / sqrt(vec_norm[,1]**2 + vec_norm[,2]**2 + vec_norm[,3]**2)

    # arrange rotation matrix
    mat_rotation_x <- matrix(c(vec_norm[,1], vec_orth[,1], vec_axis[,1]), ncol = 3)
    mat_rotation_y <- matrix(c(vec_norm[,2], vec_orth[,2], vec_axis[,2]), ncol = 3)
    mat_rotation_z <- matrix(c(vec_norm[,3], vec_orth[,3], vec_axis[,3]), ncol = 3)
    rm(vec_orth, vec_axis, vec_norm)
  }

  # convert polygon to list of polygons if it isn't
  if (!any("list" %in% class(item_poly))) {
    item_poly <- list(item_poly)
  }

  # loop through different item sizes
  item_sizes <- unique(tree_items[,"item_scaling"])
  item_pts <- lapply(item_sizes, function(curr_size) {

    # get indices of items with this dimension
    curr_idx <- tree_items[,"item_scaling"] == curr_size

    # subset rotation matrices
    mat_rotation_x_curr <- mat_rotation_x[curr_idx,]
    mat_rotation_y_curr <- mat_rotation_y[curr_idx,]
    mat_rotation_z_curr <- mat_rotation_z[curr_idx,]

    # loop through different polygons
    item_pts_sub <- lapply(1:length(item_poly), function(poly_idx) {

      # get required polygons
      item_poly_curr <- as.matrix(item_poly[[poly_idx]]) * curr_size

      # loop through polygon nodes
      item_pts_sub_sub <- lapply(1:nrow(item_poly_curr), function(node_idx) {

        # return zeros for (0|0|0)
        if (all(item_poly_curr[node_idx,] == 0)) {
          poly_x <- poly_y <- poly_z <- matrix(0, ncol = 1, nrow = sum(curr_idx))

        } else {
          # rotate coordinates
          poly_xyz <- item_poly_curr[node_idx,]
          poly_x <- mat_rotation_x_curr %*% poly_xyz
          poly_y <- mat_rotation_y_curr %*% poly_xyz
          poly_z <- mat_rotation_z_curr %*% poly_xyz
        }

        # translate coordinates
        poly_x <- poly_x + tree_items[curr_idx, "item_base_X"]
        poly_y <- poly_y + tree_items[curr_idx, "item_base_Y"]
        poly_z <- poly_z + tree_items[curr_idx, "item_base_Z"]

        # return coordinates
        return(matrix(c(poly_x, poly_y, poly_z), ncol = 3))
      })
      return(do.call(cbind, item_pts_sub_sub))
    })
    return(do.call(rbind, item_pts_sub))
  })
  item_pts <- do.call(rbind, item_pts)

  # change column names
  poly_cols <- paste0(paste0("p", rep(1:(ncol(item_pts)/3), each = 3)), c("x", "y", "z"))
  colnames(item_pts) <- poly_cols

  # get coordinate cols
  px_cols <- which(endsWith(poly_cols, "x"))
  py_cols <- which(endsWith(poly_cols, "y"))
  pz_cols <- which(endsWith(poly_cols, "z"))

  # return points
  return(item_pts)
}

# compile function to make it faster
add_items_comp <- compiler::cmpfun(add_items)

################################################################################
