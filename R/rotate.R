#' Rotate layout positions by a custom angle
#'
#' This function rotates each node's position by a specified angle.
#'
#' @param layout A 2-column matrix or dataframe of position data, where the columns are the x- and y-coordinates of each node, respectively.
#' @param angle The angle by which to rotate the layout, in degrees.
#'
#' @return A matrix of node positions.
#'
#' @examples
#'
#' # Create a random graph
#' library(igraph)
#' g <- erdos.renyi.game(100, 0.05)
#'
#' # Initializing position vector and plotting
#' position <- as.matrix(data.frame(X = c(1, 2, 3), Y = c(4, 5, 6)))
#' plot(g, layout = position)
#'
#' # Rotating position vector 90 degrees and plotting
#' rotated_df <- rotate_layout(position, 90)
#' plot(g, layout = rotated_df)
#'
#' # Rotating position vector 283 degrees and plotting
#' rotated_df <- rotate_layout(position, 283)
#' plot(g, layout = rotated_df)
#'
#' @details This function rotates each node position in a 2-column matrix/dataframe of position data by a specified angle.
#'
#' @import igraph
#'
#' @export
rotate_layout <- function(layout, angle) {
  # Check if the input is a dataframe or matrix
  if (!is.matrix(layout) && !is.data.frame(layout)) {
    stop("Input must be a matrix or dataframe")
  }

  # Check if the input has exactly two columns
  if (ncol(layout) != 2) {
    stop("Input must have exactly two columns")
  }

  # Convert angle from degrees to radians
  angle_rad <- angle * pi / 180

  # Create the rotation matrix
  rotation_matrix <- matrix(c(cos(angle_rad), -sin(angle_rad), sin(angle_rad), cos(angle_rad)), nrow = 2, byrow = TRUE)

  # Apply the rotation
  rotated_layout <- t(rotation_matrix %*% t(as.matrix(layout)))

  return(as.matrix(rotated_layout))
}
