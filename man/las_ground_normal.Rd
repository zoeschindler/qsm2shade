% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{las_ground_normal}
\alias{las_ground_normal}
\title{Extract plane normal of the ground from a point cloud}
\usage{
las_ground_normal(las, location = c(0, 0), radius = 3)
}
\arguments{
\item{las}{An object of class \code{LAS}.}

\item{location}{\code{numeric}, \code{xy}-coordinates.}

\item{radius}{\code{numeric}, radius in meters.}
}
\value{
A \code{numeric} containing the \code{xyz}-vector of the ground normal.
}
\description{
\code{las_ground_normal} extracts the plane normal of the ground at a
specified area. The area is specified via \code{xy}-coordinates and a radius.
}
\examples{
# load las data
file_path <- system.file("extdata", "walnut_with_ground.las", package="qsm2shade")
las <- lidR::readLAS(file_path)

# get stem location
location <- las_tree_location(las)

# calculate ground normal
ground_normal <- las_ground_normal(las, location)

# load qsm
file_path <- system.file("extdata", "walnut.mat", package="qsm2shade")
qsm <- qsm2r::readQSM(file_path)

# shift qsm to origin
qsm <- qsm2r::set_location(qsm, c(0,0,0))

# plot qsm and ground
qsm2r::plot(qsm, col = "salmon4", lit = TRUE)
plot_ground(plane_origin = c(0,0,0), plane_normal = ground_normal, radius = 4, add = TRUE)
rgl::bg3d("white"); rgl::axes3d()

# plot las
lidR::plot(las)
plot_ground(plane_origin = c(location[1]-min(las$X), location[2]-min(las$Y), min(las$Z)),
            plane_normal = ground_normal, radius = 4, add = TRUE)
rgl::bg3d("white"); rgl::axes3d()
}
\seealso{
\code{\link{plot_ground}}, \code{\link{las_ground_normal}}
}
