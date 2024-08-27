#' Assign edge colors based on source node colors
#'
#' This function assigns colors to edges in an \code{igraph} based on the colors of the source nodes, with an optional transparency adjustment.
#'
#' @param graph An \code{igraph} object. The graph must contain vertex color attributes.
#' @param transparency A numeric value between 0 and 1 indicating the transparency level of the edge colors. Default is 0.4.
#'
#' @return The input graph with updated edge color attributes.
#'
#' @examples
#'   library(igraph)
#'
#'   # Creating a sample graph
#'   g <- erdos.renyi.game(10, 0.3)
#'   V(g)$name <- letters[1:10]
#'   V(g)$color <- rainbow(10)
#'
#'   # Assigning edge colors based on source node colors
#'   g <- assign_edge_colors(g, transparency = 0.4)
#'
#'   # Plotting the graph
#'   plot(g, edge.color = E(g)$color, vertex.color = V(g)$color)
#'
#' @import igraph
#' @importFrom grDevices adjustcolor rainbow
#' @importFrom stats setNames
#'
#' @export
assign_edge_colors <- function(graph, transparency = 0.4) {
  # Ensure the graph has the required vertex color attribute
  if (is.null(V(graph)$color)) {
    stop("Graph does not contain vertex colors. Please provide a 'color' vertex attribute (for instance, by using assign_node_colors()).")
  }

  # Initialize edge colors
  edge_colors <- character(ecount(graph))

  # Assign colors to edges based on source node's color
  for (i in 1:ecount(graph)) {
    source_node <- get.edgelist(graph)[i, 1]
    source_color <- V(graph)$color[which(V(graph)$name == source_node)]
    edge_colors[i] <- source_color
  }

  # Convert edge colors to RGBA format with transparency adjustment
  edge_colors_rgba <- adjustcolor(edge_colors, alpha.f = transparency)

  # Update edge color attribute
  E(graph)$color <- edge_colors_rgba

  return(graph)
}
