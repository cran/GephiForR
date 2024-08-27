#' Scale node positions in an \code{igraph} object
#'
#' This function scales the positions of nodes in a graph relative to their centroid; compatible with expansions and contractions.
#'
#' @param layout A 2-column matrix representing the position data, where the columns are the x- and y-coordinates of each node, respectively.
#' @param scale_factor A numeric value indicating the factor by which to scale the node positions. The default is \code{scale_factor = 1.2}.
#'
#' @return A matrix of updated node positions.
#'
#' @examples
#'   set.seed(10)
#'   library(igraph)
#'
#'   # Generating graph and setting initial layout
#'   g <- erdos.renyi.game(100, 0.05)
#'   layout <- layout_with_fr(g)
#'
#'   # Plotting original graph, maintaining fixed scale so expansion is clear
#'   plot(layout, main="Original Graph", xlab="", ylab="", xlim=c(-20, 15), ylim=c(-20, 15))
#'
#'   # Expanding node positions
#'   layout_expanded <- scale_node_positions(layout, 2)
#'
#'   # Plotting expanded graph, maintaining fixed scale so expansion is clear
#'   plot(layout_expanded, main="Expanded Graph", xlab="", ylab="", xlim=c(-20, 15), ylim=c(-20, 15))
#'
#'   # Contract node positions
#'   layout_cont <- scale_node_positions(layout, 0.8)
#'
#'   # Plotting contracted graph, maintaining fixed scale so transformation is clear
#'   plot(layout_cont, main="Contracted Graph", xlab="", ylab="", xlim=c(-20, 15), ylim=c(-20, 15))
#'
#'
#'   # Note that igraph plots like below make it difficult to see the transformation,
#'   # because they are autoscaled.
#'   # plot(g, layout = layout_expanded)
#'   # The change is easy to see in scaled plots, as shown.
#'
#' @details This function expands or contracts the graph around a centroid, facilitating visualization. Factors greater than 1 expand the network around the centroid while those less than 1 contract it around the centroid.
#' \code{scale_factor = 1} is no change. Note that unscaled plot commands like \code{plot(g, layout = layout_expanded)} make the change difficult to see, so scaled plots are recommended for comparison.
#'
#' @import igraph
#'
#' @export
scale_node_positions <- function(layout, scale_factor = 1.2) {
  # Ensure the layout is a matrix with two columns
  if (!is.matrix(layout) || ncol(layout) != 2) {
    stop("Layout must be a matrix with two columns for x and y coordinates.")
  }

  # Compute the centroid of the positions
  centroid <- colMeans(layout)

  # Expand positions relative to the centroid
  expanded_positions <- centroid + scale_factor * (layout - centroid)

  return(expanded_positions)
}
