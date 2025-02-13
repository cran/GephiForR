% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assign_edge_colors.R
\name{assign_edge_colors}
\alias{assign_edge_colors}
\title{Assign edge colors based on source node colors}
\usage{
assign_edge_colors(graph, transparency = 0.4)
}
\arguments{
\item{graph}{An \code{igraph} object. The graph must contain vertex color attributes.}

\item{transparency}{A numeric value between 0 and 1 indicating the transparency level of the edge colors. Default is 0.4.}
}
\value{
The input graph with updated edge color attributes.
}
\description{
This function assigns colors to edges in an \code{igraph} based on the colors of the source nodes, with an optional transparency adjustment.
}
\examples{
  library(igraph)

  # Creating a sample graph
  g <- erdos.renyi.game(10, 0.3)
  V(g)$name <- letters[1:10]
  V(g)$color <- rainbow(10)

  # Assigning edge colors based on source node colors
  g <- assign_edge_colors(g, transparency = 0.4)

  # Plotting the graph
  plot(g, edge.color = E(g)$color, vertex.color = V(g)$color)

}
