#' Calculate a Text Cooccurrence Network
#'
#' This function calculates a pointwise mutual information (PMI) weighted network from vertex data.
#'
#' @param data Data frame containing the network data.
#' @param document String; Name of the document ID column.
#' @param feature String; Name of the feature (e.g. token) column.
#' @param pmi_weight Logical indicating whether weights should be calculated based on the PMI (Pointwise Mutual Information) of `document` and `feature`. If `FALSE`, a simple cooccurrence weighting is performed.
#' @param as_data_frame Logical indicating whether to output a data frame with merged edges (a to b | b to a)
#' @param keep_negative_weights Logical indicating whether to keep edges with negative PMI weights. Only applies if `pmi_weight = TRUE`.
#'
#' @return A weighted igraph object if pmi_weight is TRUE, otherwise an igraph object or data frame depending on as_data_frame.
#'
#' @details This is effectively a wrapper for the internal function `calculate_networks()`, allowing to calculate a text cooccurrence network
#'  from a data frame.
#'  
#' @examples 
#' data("de_pol_twitter")
#' text_network <- make_textnetwork(data = de_pol_twitter,
#'                                 document = "doc_id", 
#'                                 feature = "lemma",
#'                                 pmi_weight = TRUE,
#'                                 as_data_frame = FALSE,
#'                                 keep_negative_weights = TRUE)
#'
#' @export
#' 







make_textnetwork <- function(data,
                             document,
                             feature,
                             pmi_weight = TRUE,
                             as_data_frame = FALSE,
                             keep_negative_weights = TRUE){
  
  if (!(document %in% names(data))) {
    stop(paste0("document '", document, "' not in data.\n"))
  }

  if (!(feature %in% names(data)) ){
    stop(paste0("feature '", feature, "' not in data.\n"))
  }

  if (!(is.data.frame(data))) {
    stop("'data' must be a data frame.\n")
  }

  network <- calculate_network(
    data = data,
    document = document,
    feature = feature,
    pmi_weight = pmi_weight,
    as_data_frame = as_data_frame,
    keep_negative_weights = keep_negative_weights)
  
  return(network)
}
