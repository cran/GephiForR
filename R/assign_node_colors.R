#' Assign node colors based on a set of attributes
#'
#' This function assigns colors to nodes in a graph based on specified attributes.
#'
#' @param graph An \code{igraph} object. The graph must contain a vertex attribute `name`.
#' @param attributes A two-column matrix or data frame. The first column must contain node names, and the second column must contain the attributes that colors will be assigned off of.
#' @param custom_colors A character vector of colors to be used for different attribute values. If not provided, a default palette from `rainbow` will be used.
#'
#' @return The input graph with updated vertex color attributes.
#'
#' @examples
#'   library(igraph)
#'
#'   # Creating a sample graph
#'   g <- erdos.renyi.game(10, 0.3)
#'   V(g)$name <- letters[1:10]
#'
#'   # Creating a sample attributes data frame
#'   attributes <- data.frame(
#'     Node = letters[1:10],
#'     Attribute = rep(c("Group1", "Group2", "Group3"), length.out = 10)
#'   )
#'
#'   # Assigning node colors using default colors
#'   g <- assign_node_colors(g, attributes)
#'
#'   # Plotting the graph
#'   plot(g, vertex.color = V(g)$color)
#'
#'   ##### Example with custom colors ####
#'
#'   # Defining custom colors
#'   custom_colors <- c("red", "yellow", "pink")
#'
#'   # Assigning node colors
#'   g <- assign_node_colors(g, attributes, custom_colors)
#'
#'   # Plotting the graph
#'   plot(g, vertex.color = V(g)$color)
#'
#' @import igraph
#' @importFrom grDevices adjustcolor rainbow
#' @importFrom stats setNames
#'
#' @export
assign_node_colors <- function(graph, attributes, custom_colors = NULL) {
  # Ensure the graph has the required vertex attribute
  if (is.null(V(graph)$name)) {
    stop("Graph does not contain vertex names. Please provide a 'name' vertex attribute.")
  }

  # Ensure attributes has exactly two columns
  if (ncol(attributes) != 2) {
    stop("Attributes must be a two-column matrix or data frame.")
  }

  # Ensure the first column matches the node names
  if (!all(V(graph)$name %in% attributes[, 1])) {
    stop("Some node names in the graph are not present in the attributes data.")
  }

  # Extract node names from the graph
  node_names <- V(graph)$name

  # Match node names with attribute values
  attribute_vals <- attributes[, 2][match(node_names, attributes[, 1])]

  # Get unique attribute values
  unique_attributes <- unique(attribute_vals)

  # Set default colors if custom_colors is not provided
  if (is.null(custom_colors)) {
    custom_colors <- rainbow(length(unique_attributes))
  }

  # Check if there are enough colors
  if (length(unique_attributes) > length(custom_colors)) {
    stop("Not enough colors provided for the unique attributes.")
  }

  # Create a mapping from attributes to colors
  attribute_to_color <- setNames(custom_colors[1:length(unique_attributes)], unique_attributes)

  # Assign colors to nodes based on attribute values
  node_colors <- sapply(attribute_vals, function(x) attribute_to_color[as.character(x)])

  # Update vertex color attribute
  V(graph)$color <- node_colors

  return(graph)
}
