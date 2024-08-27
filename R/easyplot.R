#' Easy way to plot networks similar to `Gephi'
#'
#' This function provides an easy interface for plotting a graph in a similar style to `Gephi'.
#' It has a customizable layout, label size, edge color, vertex size, edge arrow size, and vertex label color.
#'
#' @param graph An `igraph` object. The graph must contain vertex color attributes.
#' @param layout A matrix representing the layout of the graph. Typically obtained from layout functions like `layout_with_fr` or `layout.forceatlas2`.
#' @param label_size A numeric value indicating the size of the vertex labels. Default is 3. Note that when exporting a plot, label size will likely need to be adjusted depending on the plot size.
#' @param edge_color A character vector of colors for the edges. If not provided, the default color is black.
#' @param vertex_size A numeric vector indicating the size of the vertices. Default is 15 for all vertices.
#' @param edge_arrow_size A numeric value indicating the size of the arrows at the end of the edges. Default is 0.2.
#' @param vertex_label_color A character string specifying the color of the vertex labels. Default is "black".
#'
#'
#' @return A plot of the graph with the specified parameters.
#'
#' @examples
#'   library(igraph)
#'
#'   # Creating a sample graph
#'   g <- erdos.renyi.game(10, 0.3)
#'   V(g)$name <- letters[1:10]
#'   V(g)$color <- rainbow(10)
#'   layout <- layout_with_fr(g)
#'
#'   # Plot the graph using easyplot
#'   easyplot(g, layout, label_size = 1, vertex_size = rep(10, vcount(g)))
#'
#'   # Assign edge colors based on source node colors
#'   g <- assign_edge_colors(g, transparency = 0.4)
#'
#'   # Plot the graph using easyplot, now with edge color
#'   easyplot(g, layout, label_size = 1, vertex_size = rep(10, vcount(g)))
#'
#' @import igraph
#' @importFrom grDevices adjustcolor
#' @importFrom graphics plot
#'
#' @export
easyplot <- function(graph, layout, label_size = 3, edge_color = NULL, vertex_size = rep(3, vcount(graph)),
                     edge_arrow_size = 0.2, vertex_label_color = "black") {
  # Ensure the graph has the required vertex color attribute
  if (is.null(V(graph)$color)) {
    stop("Graph does not contain vertex colors. Please provide a 'color' vertex attribute.")
  }

  # Set edge colors if not provided
  if (is.null(edge_color)) {
    if (is.null(E(graph)$color)) {
      edge_color <- "black"
      message("Edge color not provided and no edge color attribute found. Setting edge color to black.")
    } else {
      edge_color <- E(graph)$color
    }
  }

  # Check if vertex_size is a numeric vector with the same length as the number of vertices
  if (length(vertex_size) != vcount(graph)) {
    stop("vertex_size must be a numeric vector with the same length as the number of vertices.")
  }

  # Plot the graph with specified parameters
  plot(
    graph,
    layout = layout,
    vertex.label.family = "sans",
    vertex.label.font = 2,
    edge.arrow.size = edge_arrow_size,
    vertex.size = vertex_size,
    edge.color = edge_color,
    frame = FALSE,
    mark.groups = NULL,
    bg = "transparent",
    vertex.label.cex = label_size,
    vertex.frame.color = V(graph)$color,
    vertex.label.color = vertex_label_color,
    vertex.color = V(graph)$color
  )
}
