#' Apply ForceAtlas2 layout to a graph
#'
#' This function applies Jacomy et al. (2014)'s 'ForceAtlas2' layout algorithm to an \code{igraph} object.
#'
#' @param g An \code{igraph} object representing the graph.
#' @param iterations Integer. The number of iterations to run the algorithm. Default is 100.
#' @param linlog Logical. If \code{linlog = TRUE}, uses the Noack LinLog model implemented for `Gephi' to calculate attractive and repulsive forces (see Noack 2009). Default is \code{linlog = FALSE}.
#' @param pos A 2-column matrix of initial positions, where the columns contain x-coordinates and y-coordinates, respectively. If \code{pos = NULL}, positions for the first iteration are generated randomly. Default is \code{pos = NULL}.
#' @param gravity Numeric. The strength of the gravity force. Default is 1. Note that this is only included in calculations if \code{stronggravity = TRUE}. Higher gravity values result in tighter networks.
#' @param center A numeric vector of length 2 specifying the center of gravity. If \code{ center = NULL}, the center is calculated automatically. Default is \code{ center = NULL}.
#' @param plotstep Integer. The number of iterations between plots. If \code{plotstep = 0}, no plotting is done. Default is \code{plotstep = 10}. These plots appear as intermediate output in the console.
#' @param plotlabels Logical. If \code{plotlabels = TRUE}, plot node labels appear during the intermediate plotstep graphs. Default is \code{plotlabels = TRUE}.
#' @param scalingratio Numeric. The scaling ratio of the layout. Default is 10, in line with `Gephi'.
#' @param stronggravity Logical. If \code{stronggravity = TRUE}, gravity will be an additional force acting on each node, and the gravity parameter kicks in. Default is \code{stronggravity = FALSE}.
#' @param jittertol Numeric. The tolerance for jittering nodes. Default is \code{jittertol = 1};  Jacomy et al. (2014) do not recommend increasing it above 1.
#'
#' @return A matrix of node positions.
#'
#' @examples
#' # Create a random graph
#' library(igraph)
#' g <- erdos.renyi.game(100, 0.05)
#'
#' # Assign non-numeric row names
#' V(g)$name <- paste0("node", 1:vcount(g))
#'
#' # Apply ForceAtlas2 layout
#' pos <- layout.forceatlas2(g, linlog = TRUE, iterations = 100, scalingratio = 10)
#' plot(g, layout = pos)
#'
#' @details This function implements Jacomy et al. (2014)'s ForceAtlas2 layout algorithm on an \code{igraph} object.
#' It can handle large graphs and is particularly suitable for visualizing networks. It also includes LinLog mode and a stronger gravity feature, like `Gephi'.
#'
#' @references{
#' \insertRef{jacomy_forceatlas2_2014}{GephiForR} \cr
#' \insertRef{noack_modularity_2009}{GephiForR}
#' }
#'
#'
#' @import igraph
#' @importFrom graphics text
#' @importFrom Rdpack reprompt
#' @importFrom stats dist runif
#'
#' @export
layout.forceatlas2 <- function(g, iterations = 100, linlog = FALSE, pos = NULL, gravity = 1,
                               center = NULL, plotstep = 10, plotlabels = TRUE, scalingratio = 10, stronggravity = FALSE,
                               jittertol = 1){

  # Check if vertex names are assigned
  if (is.null(igraph::V(g)$name)) {
    stop("Error: The igraph object does not have vertex names assigned. Please assign non-numeric vertex names before applying the layout.")
  }

  # Check if pos is provided and has the correct dimensions
  if (!is.null(pos)) {
    if (!is.matrix(pos) && !is.data.frame(pos)) {
      stop("Error: The 'pos' parameter must be a matrix or data frame.")
    }
    if (ncol(pos) != 2) {
      stop("Error: The 'pos' parameter must have exactly two columns: one for x positions and one for y positions.")
    }
  }

  # Get the adjacency matrix
  A <- igraph::get.adjacency(g, type="both", sparse=FALSE)

  # Setting dimension
  dim <- 2

  # Set the default center if not provided
  if (is.null(center)) {
    center <- rep(0, dim)
  } else {
    # Check if center has the correct dimensions
    if (!is.numeric(center) || length(center) != 2) {
      stop("Error: The 'center' parameter must be a numeric vector of length 2.")
    }
  }

  # Extract edges and node degrees
  edges <- as.data.frame(igraph::get.edgelist(g))
  node_degrees <- igraph::degree(g)

  # Initialize variables
  nnodes <- nrow(A)
  adjust <- A
  adjust[adjust != 0] <- 1
  Deg <- rowSums(adjust)
  Forces_new <- matrix(0, nrow = dim, ncol = nnodes)

  # Set initial positions
  if (is.null(pos)) {
    position <- matrix(runif(nnodes * dim, -1000, 1000), nnodes, dim)
  } else {
    position <- pos
  }
  rownames(position) <- igraph::V(g)$name

  # Adjust positions coinciding with the center
  incenter <- which(rowSums(position == center) == dim)
  if (length(incenter) > 0) {
    position[incenter, ] <- position[incenter, ] + 0.01
  }

  # Initialize displacement matrix
  displacement <- matrix(0, dim, nnodes)

  # Extract edge weights
  edge_weights <- igraph::E(g)$weight
  if (is.null(edge_weights)) edge_weights <- rep(1, length(igraph::E(g)))

  # Precompute degree for each node
  node_degrees_vector <- as.numeric(node_degrees[match(rownames(position), names(node_degrees))])

  # Initialize speed parameters
  Global_speed <- 1
  speedEfficiency <- 1

  for (iteration in 1:iterations) {
    Force_old <- Forces_new
    Forces_new <- matrix(0, nrow = dim, ncol = nnodes)

    # Calculate distances between nodes
    distances <- as.matrix(dist(position))
    distances[distances < 0.01] <- 0.01

    # Calculate forces for each edge
    for (i in 1:nrow(edges)) {
      node1_index <- match(edges[i, 1], rownames(position))
      node2_index <- match(edges[i, 2], rownames(position))
      edge_weight <- edge_weights[i]
      degree1 <- node_degrees_vector[node1_index]
      degree2 <- node_degrees_vector[node2_index]

      dx <- position[node1_index, ][1] - position[node2_index, ][1]
      dy <- position[node1_index, ][2] - position[node2_index, ][2]

      # Compute repulsion and attraction forces, updating attraction if linlog = TRUE
      repulsion_force <- Repulsion(dx, dy, scalingratio, degree1, degree2)
      if (linlog == TRUE){
        attraction_force <- logAttraction(dx, dy, edge_weight)

      } else {
        attraction_force <- Attraction(dx, dy, edge_weight)
      }

      # Update forces
      Forces_new[, node1_index] <- Forces_new[, node1_index] + repulsion_force + attraction_force
      Forces_new[, node2_index] <- Forces_new[, node2_index] - repulsion_force - attraction_force

    }

    # Apply strong gravity if enabled
    if (stronggravity == TRUE) {
      for (j in 1:nnodes) {
        gravity_force <- StrongGravity(position[j, ], scalingratio, node_degrees_vector[j], gravity)
        Forces_new[, j] <- Forces_new[, j] + gravity_force
      }
    }

    # Calculate swinging and effective traction
    swing <- sqrt(colSums((Forces_new - Force_old)^2))
    Global_swing <- sum((Deg + 1) * swing)
    tra <- sqrt(colSums(sqrt((Forces_new + Force_old)^2))) / 2
    Global_tra <- sum((Deg + 1) * tra)

    # Optimize jitter tolerance
    #The 'right' jitter tolerance for this network. Bigger networks need more tolerance.
    #Denser networks need less tolerance. Fully empiric.
    estimatedOptimalJitterTolerance <- 0.05 * sqrt(nnodes)
    minJT <- sqrt(estimatedOptimalJitterTolerance)
    maxJT <- 10
    jt <- jittertol * max(minJT, min(maxJT, estimatedOptimalJitterTolerance * Global_tra / nnodes^2))

    # Speed efficiency variables
    minSpeedEfficiency <- 0.05

    # Adjust speed efficiency based on swinging to effective traction ratio
    if (Global_swing / Global_tra > 2.0) {
      if (speedEfficiency > minSpeedEfficiency) {
        speedEfficiency <- speedEfficiency * 0.5
      }
      jt <- max(jt, jittertol)
    }

    # Target speed calculation
    target_speed <- jt * speedEfficiency * Global_tra / Global_swing

    # Speed efficiency is how the speed really corresponds to the swinging vs. convergence tradeoff
    # Adjust speed efficiency based on convergence
    if (Global_swing > jt * Global_tra) {
      if (speedEfficiency > minSpeedEfficiency) {
        speedEfficiency <- speedEfficiency * 0.7
      }
    } else if (Global_speed < 1000) {
      speedEfficiency <- speedEfficiency * 1.3
    }

    # Limit the rise of speed
    maxRise <- 0.5  # Max rise: 50%
    Global_speed <- Global_speed + min(target_speed - Global_speed, maxRise * Global_speed)

    # Adaptive auto-speed: the speed of each node is lowered when the node swings
    speed <- Global_speed / (1 + sqrt(Global_speed * swing))

    # Apply forces and update positions
    displacement <- t(t(Forces_new) * speed)
    position <- position + t(displacement)

    # Plot the graph at intervals if specified
    if (plotstep > 0 && dim == 2 && iteration %% plotstep == 0) {
      plot(position, main = paste0("Iteration: ", iteration), xlab = "", ylab = "")
      if (plotlabels) text(position, labels = igraph::V(g)$name, cex = 0.7, pos = 3)
    }
  }

  return(position)
}
#'
#' @keywords internal
Attraction <- function(dx, dy, edge_weight) {
  attraction <- -edge_weight
  forceX <- dx * attraction
  forceY <- dy * attraction

  return(c(forceX, forceY))
}
#'
#' @keywords internal
Repulsion <- function(dx, dy, scalingratio, degree1, degree2) {

  distance <- sqrt(dx^2 + dy^2)
  if (distance < 0.01) distance <- 0.01

  repulsion <- ((degree1 +1) * (degree2 + 1) * scalingratio) / (distance^2)
  forceX <- dx * repulsion
  forceY <- dy * repulsion

  return(c(forceX, forceY))
}
#'
#' @keywords internal
logAttraction <- function(dx, dy, edge_weight) {
  distance <- sqrt(dx^2 + dy^2)

  attraction <- -edge_weight*log((1+distance))/distance
  forceX <- dx * attraction
  forceY <- dy * attraction

  return(c(forceX, forceY))
}
#'
#' @keywords internal
StrongGravity <- function(node1, scalingratio, degree1, gravity) {
  dx <- node1[1]
  dy <- node1[2]
  distance <- sqrt(dx^2 + dy^2)

  if (distance > 0){
    gravitycalc <- ((degree1 + 1)) * gravity
    forceX <- -dx * gravitycalc
    forceY <- -dy * gravitycalc
  }
  else{
    forceX <- 0
    forceY <- 0
  }

  return(c(forceX, forceY))
}
