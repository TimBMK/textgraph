#' Calculate Metrics for Clusters
#'
#' Internal function that calculates the metrics of clusters calculated during the topic calculations
#'
#' @param cluster Cluster as provided within the function
#' @param verbose Print out Metrics?
#' @param ... Additional parameters of the clustering algorithm
#'
#' @return Metrics
#'
#' @importFrom igraph algorithm modularity sizes
#'
#' @keywords internal
calculate_cluster_metrics <- function(cluster, verbose, page_rank_calculation, ...) {

  # Calculate Cluster Metrics
  algorithm <- tryCatch({igraph::algorithm(cluster)},
                        error=function(error_message) {
                          return(error_message$message)})

  nr_topics <- tryCatch(unique(cluster$membership) %>% length(),
                        error=function(error_message) {
                          return(error_message$message)})

  modularity <- tryCatch({igraph::modularity(cluster)}, # Modularity as calculated by most algorithms
                         error=function(error_message) {
                           return(error_message$message)})

  if (!is.null(cluster$quality)) { # a metric specific to the leiden algorithm
    quality <- cluster$quality
  } else {
    quality <- "Quality was not calculated"
  }

  mean_topic_entities <- tryCatch({igraph::sizes(cluster) %>% mean()},
                                  error=function(error_message) {
                                    return(error_message$message)})

  median_topic_entities <- tryCatch({igraph::sizes(cluster) %>% stats::median()},
                                    error=function(error_message) {
                                      return(error_message$message)})

  metrics <- list(
    algorithm = algorithm)

  params <- list(...) # get additional parameters

  if (length(params) > 0) {
    metrics <- c(metrics, params)
  }


  metrics <- c(metrics,
               list(
                 nr_topics = nr_topics,
                 quality = quality,
                 modularity = modularity,
                 page_rank_calculation = page_rank_calculation,
                 mean_topic_entities = mean_topic_entities,
                 median_topic_entities = median_topic_entities)
  )

  if (verbose) {
    cat(paste(
      "\nAlgorithm:", algorithm,
      "\nNumber of Topics:", nr_topics,
      "\nModularity:", modularity,
      "\nQuality:", quality,
      "\nPage Rank Calculation:", page_rank_calculation,
      "\nMean Number of Entities per Topic:", mean_topic_entities,
      "\nMedian Number of Entities per Topic:", median_topic_entities
    ))
  }

  return(metrics)
}
