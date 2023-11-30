#' Run Random Walk with Restart for Each Seed Node
#'
#' This helper function runs Random Walk with Restart for each seed node in a given network. Used internally by `get_rwr_terms()`.
#'
#' @param seed A single seed node identifier.
#' @param walk_network An object containing the multiplex network and various matrices.
#' @param normalize_score How scores should be normalized.
#' @param positive_scores_only Logical indicating whether to drop negative scores before normalization.
#'
#' @return A dataframe with Random Walk with Restart results for the given seed node.
#'
#' @importFrom dplyr "%>%"
#' @importFrom dplyr as_tibble mutate filter
#' @importFrom scales rescale
#' @importFrom tibble as_tibble
#'
#' @keywords internal
seed_walk <- function(seed, walk_network, normalize_score, positive_scores_only){ # convenience function for the seed walks to be used in get_rwr_terms()

  res <- tryCatch(# capture non-standard errors thrown by Random.Walk.Restart.Multiplex
    Random.Walk.Restart.Multiplex.failsafe(
      x = walk_network$AdjMatrixNorm,
      MultiplexObject = walk_network$multiplex,
      Seeds = seed
    ),
    error  = function(e) NULL # return NULL for errors
  )

  if (!is.null(res)) { # process further if the walk was successfull

    res <- res %>%
      .[["RWRM_Results"]] %>%  # pull relevant data
      dplyr::as_tibble() %>%
      dplyr::mutate(seed_node = seed) # and set the seed node indicator

    if (positive_scores_only) {
      res <- res %>%
        dplyr::filter(Score > 0)
    }

    if (!is.null(normalize_score)) {

      if ("seeds" %in% normalize_score){ # normalization and score filtering on Seed Level (for each Random Walk)
        res <- res %>%
          dplyr::mutate(ScoreNorm = scales::rescale(Score, to = c(0, 1)))
      }

    }

  }

  return(res)
}

