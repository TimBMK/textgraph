#' Compute Adjacency Matrix for a Monoplex Network
#'
#' This helper function computes the adjacency matrix for a monoplex network.
#'
#' @param x A multiplex network object.
#'
#' @details
#' An adjusted version of the compute.adjacency.matrix() function from
#'  the RandomWalkRestartMH package (https://github.com/alberto-valdeolivas/RandomWalkRestartMH/),
#'  making monoplex network preparation more efficient by dropping unnecessary overhead.
#'  Specifically, it vastly reduces the use of RAM (which would require
#'  manual flushing with gc() every time) by dropping everything connected
#'  to the line "offdiag <- (delta/(L-1))*Idem_Matrix" which is not needed
#'  for monoplex networks
#'
#' @return A dgCMatrix object representing the adjacency matrix of the network.
#'
#' @importFrom igraph is_weighted as_adjacency_matrix
#' @importFrom Matrix Diagonal bdiag
#' @importFrom RandomWalkRestartMH isMultiplex isMultiplexHet
#'
#' @keywords internal
compute.adjacency.matrix.mono <- function(x) # delta is no longer needed for monoplex networks
{


  if (!isMultiplex(x) & !isMultiplexHet(x)) {
    stop("Not a Multiplex or Multiplex Heterogeneous object")
  }


  N <- x$Number_of_Nodes_Multiplex
  L <- x$Number_of_Layers

  Layers_Names <- names(x)[seq(L)]

  ## IDEM_MATRIX.
  Idem_Matrix <- Matrix::Diagonal(N, x = 1)

  counter <- 0
  Layers_List <- lapply(x[Layers_Names],function(x){

    counter <<- counter + 1;
    if (is_weighted(x)){
      Adjacency_Layer <-  igraph::as_adjacency_matrix(x,sparse = TRUE,
                                                      attr = "weight")
    } else {
      Adjacency_Layer <-  igraph::as_adjacency_matrix(x,sparse = TRUE)
    }

    Adjacency_Layer <- Adjacency_Layer[order(rownames(Adjacency_Layer)),
                                       order(colnames(Adjacency_Layer))]
    colnames(Adjacency_Layer) <-
      paste0(colnames(Adjacency_Layer),"_",counter)
    rownames(Adjacency_Layer) <-
      paste0(rownames(Adjacency_Layer),"_",counter)
    Adjacency_Layer
  })

  MyColNames <- unlist(lapply(Layers_List, function (x) unlist(colnames(x))))
  MyRowNames <- unlist(lapply(Layers_List, function (x) unlist(rownames(x))))
  names(MyColNames) <- c()
  names(MyRowNames) <- c()
  SupraAdjacencyMatrix <- Matrix::bdiag(unlist(Layers_List))
  colnames(SupraAdjacencyMatrix) <-MyColNames
  rownames(SupraAdjacencyMatrix) <-MyRowNames

  SupraAdjacencyMatrix <- as(SupraAdjacencyMatrix, "dgCMatrix")
  return(SupraAdjacencyMatrix)
}

