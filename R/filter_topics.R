#' Filter the topics in a textgraph_topics object
#'
#' This function allows to filter the topics in a textrgraph topic by providing a
#'  numerical vector of topics to be retained. Note that this also updates the metrics
#'  where applicable (see Details).
#'
#' @param textgraph_topics A textgraph_topics object, as provided by the `calculate_topics()` function.
#' @param topic_numbers A numerical vector containing the numbers of the topics to be retained.
#'
#' @return A textgraph_topics object with updated contents and metrics.
#'
#' @details The returned textgraph_topics object has its metrics updated where applicable.
#'  Specifically, metrics related to the clustering algorithm are NOT updated, but conserved
#'  from the original topic calculation. Metrics concerning the number of topics, and their mean
#'  and median number of entities, however, are updated.
#'
#'@examples
#' \dontrun{
#' filtered_topics <- filter_topics(textgraph_topics = topics,
#'                                  topic_numbers = c(1, 14, 76))
#' }
#'
#' @importFrom dplyr "%>%"
#' @importFrom dplyr filter distinct summarise pull
#' @importFrom purrr map
#' @importFrom stats median
#'
#' @export

filter_topics <- function(textgraph_topics,
                          topic_numbers){

  # checks
  if (class(textgraph_topics) != "textgraph_topics") {
    stop("Must be a textgraph_topics object.")
  }

  if (!is.vector(topic_numbers) | !is.numeric(topic_numbers)) {
    stop("'topics' must be provided as a numerical vector of topic numbers.")
  }

  # Temporarily remove metrics and cluster object
  metrics <- textgraph_topics$metrics # safe metrics for later

  res <- textgraph_topics[setdiff(names(textgraph_topics), "metrics")] # temporarily drop the metrics

  if ("igraph_cluster" %in% names(textgraph_topics)) {
    igraph_cluster <- textgraph_topics$igraph_cluster

    res <- res[setdiff(names(res), "igraph_cluster")] # temporarily drop the cluster object
  }

  if ("snapshots" %in% names(textgraph_topics)) {
    snapshots <- textgraph_topics$snapshots

    res <- res[setdiff(names(res), "snapshots")] # temporarily drop the snapshots object
  }


  # filter topics
  res <- res %>% purrr::map(\(object) {
    object %>% dplyr::filter(topic %in% topic_numbers)
  })


  # update metrics
  metrics$nr_topics <- res$topics %>% dplyr::distinct(topic) %>% nrow()

  metrics$mean_topic_entities <- res$entities %>%
    dplyr::summarise(n = n(), .by = topic) %>%
    dplyr::pull(n) %>% mean()

  metrics$median_topic_entities <- res$entities %>%
    dplyr::summarise(n = n(), .by = topic) %>%
    dplyr::pull(n) %>% stats::median()

  if ("documents" %in% names(textgraph_topics)) {
    metrics$mean_document_occurrences <- res$topics$document_occurrences %>%
      mean()
    metrics$median_document_occurrences <- res$topics$document_occurrences %>%
      stats::median()
  }

  res <- c(list(metrics = metrics), res) # add metrics as first object

  if ("igraph_cluster" %in% names(textgraph_topics)) { # add cluster object
    res$igraph_cluster <- igraph_cluster
  }

  if ("igraph_cluster" %in% names(textgraph_topics)) { # add snapshots object
    res$snapshots <- snapshots
  }

  class(res) <- "textgraph_topics"

  return(res)

}
