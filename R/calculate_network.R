#' Calculate PMI Weighted Network
#'
#' This function calculates a pointwise mutual information (PMI) weighted network from vertex data.
#'
#' @param data Data frame containing the network data.
#' @param vertex_a Name of the vertex A column.
#' @param vertex_b Name of the vertex B column.
#' @param directed Logical indicating whether the network is directed.
#' @param pmi_weight Logical indicating whether to calculate PMI weights.
#' @param as_data_frame Logical indicating whether to output a data frame with merged edges (a to b | b to a); only relevant when not weighted by PMI.
#' @param keep_negative_weights Logical indicating whether to keep edges with negative PMI weights. Only applies if `pmi_weight = TRUE`.
#'
#' @return A weighted igraph object if pmi_weight is TRUE, otherwise an igraph object or data frame depending on as_data_frame.
#'
#' @importFrom dplyr "%>%"
#' @importFrom dplyr select filter rename distinct as_tibble
#' @importFrom tidyr spread
#' @importFrom igraph graph_from_data_frame as_data_frame
#' @importFrom widyr pairwise_pmi_
#'
#' @keywords internal
calculate_network <-             # function to be mapped over timeframes
  function(data,
           vertex_a,
           vertex_b,
           directed = FALSE,
           pmi_weight = TRUE,
           as_data_frame = FALSE,
           keep_negative_weights = TRUE){

    if (pmi_weight == TRUE) {
      suppressWarnings({ # suppress warnings about deprecated matrix function in pmi calculation
        slice <-
          data %>%
          dplyr::select({{ vertex_a }}, {{ vertex_b }}) %>%
          widyr::pairwise_pmi_(feature =  {{vertex_a}}, item = {{vertex_b}}, sort = F) %>% dplyr::rename(weight = pmi) %>% # calculate PMI as weight (use pairwise_pmi_() avoid problems with column specification)
          igraph::graph_from_data_frame(directed = F) %>%  # make igraph object for slice
          igraph::as_data_frame(what = "edges") %>% # temporarily convert to dataframe to identify identical a-b b-a edges
          dplyr::distinct(from, to, .keep_all = TRUE)   # remove duplicated edges introduced by PMI (a to b, b to a)

        if (!keep_negative_weights){
          slice <- slice %>% dplyr::filter(weight > 0)
        }

        if (!as_data_frame){
          slice <- slice %>% igraph::graph_from_data_frame(directed = F)  # convert to igraph object. Always undirected if PMI weighted
        }

      })

    } else {
      # unweighted if not pmi-weighted
      slice <-
        data %>% dplyr::as_tibble() %>%
        dplyr::select({{ vertex_a }}, {{ vertex_b }})

      if (!as_data_frame) {
        slice <- slice %>% igraph::graph_from_data_frame(directed = directed) # convert to igraph object
      }
    }

    return(slice)

  }
