################################################################################
# HELPER FUNCTIONS
################################################################################

# convert radians to degrees
rad2deg <- function(rad) {
  return(rad * 180 / pi)
}

################################################################################

# convert degrees to radians
deg2rad <- function(deg) {
  return(deg * pi / 180)
}

################################################################################

# calculate normalized crossproduct of xyz matrices
norm_cross <- function(v, w) {

  # object for storage
  cross <- matrix(NA, nrow = nrow(v), ncol = 3)

  # cross product
  cross[,1] <- v[,2] * w[,3] - v[,3] * w[,2]
  cross[,2] <- v[,3] * w[,1] - v[,1] * w[,3]
  cross[,3] <- v[,1] * w[,2] - v[,2] * w[,1]

  # return normalized result
  return(cross / sqrt(cross[,1]**2 + cross[,2]**2 + cross[,3]**2))
}

################################################################################

# get branch IDs if child branches
find_childs_recursive_branch <- function(cylinder, branch_ID, include_self = TRUE) {

  # get cylinders of the branches
  cyl_sub <- cylinder[cylinder$branch %in% branch_ID,]

  # get all cylinders which are children of the branches
  cyl_childs <- cylinder[cylinder$parent %in% cyl_sub$ID & !(cylinder$branch %in% branch_ID),]

  # return the branch IDs of the children
  if (nrow(cyl_childs) == 0) {
    return(NULL)
  } else {
    id_childs <- unique(cyl_childs$branch)
    id_childs_childs <- find_childs_recursive_branch(cylinder, id_childs)
    if (include_self) {
      return(c(branch_ID, id_childs, id_childs_childs))
    } else {
      return(c(id_childs, id_childs_childs))
    }
  }
}

################################################################################

# prepare qsm for processing
prepare_qsm <- function(qsm, center = TRUE, keep_all = FALSE) {

  # check data type
  if (any("QSM" %in% class(qsm))) {
    cylinder <- qsm@cylinder
  } else {
    cylinder <- qsm
  }

  # convert to matrix
  if (!any("matrix" %in% class(cylinder))) {
    cylinder <- as.matrix(cylinder)
  }

  # get relevant columns
  if (!keep_all) {
    cylinder <- cylinder[,c("radius", "length", "start_X", "start_Y", "start_Z", "axis_X", "axis_Y", "axis_Z")]
  }

  # center coordinates
  if (center) {
    stembase <- cylinder[1,]
    cylinder[,"start_X"] = cylinder[,"start_X"] - stembase["start_X"]
    cylinder[,"start_Y"] = cylinder[,"start_Y"] - stembase["start_Y"]
    cylinder[,"start_Z"] = cylinder[,"start_Z"] - stembase["start_Z"]
  }

  # calculate end coordinates
  end_X = cylinder[,"start_X"] + cylinder[,"axis_X"] * cylinder[,"length"]
  end_Y = cylinder[,"start_Y"] + cylinder[,"axis_Y"] * cylinder[,"length"]
  end_Z = cylinder[,"start_Z"] + cylinder[,"axis_Z"] * cylinder[,"length"]
  cylinder <- cbind(cylinder, end_X, end_Y, end_Z)

  # return matrix
  return(cylinder)
}

################################################################################
