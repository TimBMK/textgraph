#' Create Multiplex Network Objects
#'
#' This function creates and preprocesses a multiplex network for later use in Random Walks with Restart.
#'
#' @param dat Data frame to pass to calculate_network function. Expects a dataframe in the format of an edge list, where each row depicts an edge between `vertex_a` and `vertex_b`.
#' @param vertex_a String; Name of the vertex A column. When calculating a PMI-weighted network, this is the document ID.
#' @param vertex_b String; Name of the vertex B column. When calculating a PMI-weighted network, this is the feature.
#' @param directed Logical indicating whether the network is directed or not. PMI-weighted networks are always undirected.
#' @param pmi_weight Logical indicating whether weights should be calculated based on the PMI (Pointwise Mutual Information) of Vertex A and B.
#' @param keep_negative_weights Logical indicating whether edges with negative PMI weight should be retained. Discarding them can make for a smaller network.
#' @param network Pre-calculated network to turn into a multiplex network; if `NULL`, a network will be calculated from the vertices. Expects either `NULL` or an igraph graph.
#' @param keep_igraph_network Logical indicating whether the igraph network should be kept separately.
#' @param keep_multiplex_network Logical indicating whether the multiplex network should be kept separately.
#' @param keep_adjacency_matrix Logical indicating whether the non-normalized adjacency matrix should be kept.
#' @param keep_normalized_adjacency_matrix Logical indicating whether to keep the normalized adjacency matrix.
#'
#' @details
#' This function either turns a data frame of edges or an existing igraph object into a multiplex network object. The functionality to weight the network via the Pointwise
#'  Mutual Information (PMI) between two nodes is especially useful for text networks. Additional options to return only specific parts of the multiplex object are provided.
#'  Note that, for use with the `get_rwr_terms()` function, the normalized adjacency matric (`keep_normalized_adjacency_matrix = TRUE`) and the multiplex network
#'  (`keep_multiplex_network = TRUE`) are required. If only the igraph network is required, one can only set `keep_igraph_network` to `TRUE`. The calculation of the adjecency
#'  matrix will be skipped in this case.
#'
#' @return A list containing various elements such as the igraph network, multiplex network, and adjacency matrices, to be used in `get_rwr_terms()`.
#'
#' @examples
#' data("de_pol_twitter")
#'
#' multiplex_text_network <- make_multiplex_objects(de_pol_twitter,
#'                                                  vertex_a = "doc_id",
#'                                                  vertex_b = "lemma",
#'                                                  directed = FALSE,
#'                                                  pmi_weight = TRUE,
#'                                                  keep_negative_weights = TRUE,
#'                                                  network = NULL,
#'                                                  keep_igraph_network = FALSE,
#'                                                  keep_multiplex_network = TRUE,
#'                                                  keep_adjacency_matrix = FALSE,
#'                                                  keep_normalized_adjacency_matrix = TRUE)
#'
#'
#' @importFrom RandomWalkRestartMH create.multiplex normalize.multiplex.adjacency
#' @importFrom igraph is.igraph
#'
#' @export
make_multiplex_objects <- function(dat,
                                   vertex_a,
                                   vertex_b,
                                   directed = FALSE,
                                   pmi_weight = TRUE,
                                   keep_negative_weights = TRUE,
                                   network = NULL,
                                   keep_igraph_network = FALSE,
                                   keep_multiplex_network = TRUE,
                                   keep_adjacency_matrix = FALSE,
                                   keep_normalized_adjacency_matrix = TRUE
)

{

  if (!is.null(network) && !igraph::is.igraph(network)) {
    stop("The network provided is not a valid igraph object.\n")
  }

  if (is.null(network)) {
    network <- calculate_network(
      dat,
      vertex_a = vertex_a,
      vertex_b = vertex_b,
      directed = directed,
      pmi_weight = pmi_weight,
      as_data_frame = F # return as igraph object
    )
  }

  if (keep_multiplex_network | keep_adjacency_matrix | keep_normalized_adjacency_matrix){
    multiplex <- RandomWalkRestartMH::create.multiplex(list(network = network)) # make multiplex object for random walks
  }

  if (keep_adjacency_matrix | # calculate adjacency matrix if needed
      keep_normalized_adjacency_matrix) {
    AdjMatrix <- compute.adjacency.matrix.mono(multiplex)
  }

  if (keep_normalized_adjacency_matrix){ # calculate normalized adjacency matrix if needed
    AdjMatrixNorm <- RandomWalkRestartMH::normalize.multiplex.adjacency(AdjMatrix)
  }

  return(c(
    if(keep_igraph_network) {
      list(network = network)
    },
    if(keep_multiplex_network) {
      list(multiplex = multiplex)
    },
    if(keep_adjacency_matrix){
      list(AdjMatrix = AdjMatrix)
    },
    if(keep_normalized_adjacency_matrix){
      list(AdjMatrixNorm = AdjMatrixNorm)
    }
  ))

}




