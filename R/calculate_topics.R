#' Calculate Topic Cluster from a Text Network
#'
#' This function takes a text network represented as an igraph graph and performs
#' topic clustering using the specified clustering algorithm. It then calculates
#' additional metrics such as Page Rank, cluster metrics, and more.
#'
#' @param text_network An igraph graph representing the text network.
#' @param documents Optional, a data frame containing document information. Expects one row per token. NULL to skip.
#'  Returns additional information on topic-relevant documents if provided. `document_tokens` and `document_ids`
#'  variables are required. Additional variables (such as document meta data) can be passed. Note, however,
#'  that these should contain no more than one unique value per document (see examples).
#' @param document_tokens String; The column name in documents containing the document tokens.
#' @param document_ids String; The column name in documents containing document identifiers.
#' @param negative_edge_weights Logical indicating whether to consider only edges
#'   with positive weights. If the chosen clustering algorithm does not support negative edge weights,
#'   set this to `FALSE`.
#' @param page_rank_calculation Character specifying the type of Page Rank calculation.
#'   Options are "global" for global network Page Rank, or
#'   "cluster" for calculating the Page Rank of the entities within each topic cluster.
#' @param cluster_function The clustering function to use. Can be any igraph clustering function.
#'  Default is `igraph::cluster_leiden`.
#' @param ... Additional arguments passed to `cluster_function`.
#' @param keep_cluster_object Logical indicating whether to keep the igraph cluster object.
#'  Can be helpful for additional checks. See `help(igraph::membership)`
#' @param verbose Logical indicating whether to print clustering metrics.
#'
#' @return A list object of class "textgraph_topics" containing topic entities,
#'   document data (optional), clustering metrics, and the cluster object (optional).
#'
#' @details The function performs topic clustering on the input text network, calculates
#'  additional metrics based on the specified parameters, and returns a structured object
#'  containing topics and metrics. Returns a list with model metrics, a data frame with a topic
#'  overview, a data frame with the entities associated with each topic, and (optionally)
#'  a data frame with the documents associated with each topic.
#'  Note that, depending on the `cluster_function` used, the results may be non-deterministic
#'  and dependant on a seed. Therefore, for algorithms like the suggested Leiden algorithm, it is
#'  strongly encouraged to a) run the function several times with different/no random seeds to
#'  ensure results are comparable across runs; and b) use a fixed random seed for reproducible
#'  results.
#'  The Page Rank of entities serves as a measure of their
#'  relative importance in a topic cluster. It can be calculated either globally with
#'  `page_rank_calculation = "global"`, as the Page Rank in the complete `text_network` (faster),
#'  or locally for each cluster with `page_rank_calculation = "global"`, where a
#'  subgraph is induced containing only the nodes of the given cluster. This takes longer,
#'  but can provide a better measure of a term's relevance within a topic. For the latter option,
#'  multithreading is supported if a plan was set up with `future::plan()`.
#'  The `document_relevance` calculated for the optionally provided documents indicates
#'  how relevant a document is to a given topics. It is calculated as the re-scaled sum of
#'  all topic-relevant entities' Page Rank in the document multiplied by their tf-idf
#'  (Term Frequency - Inverse Document Frequency).
#'
#' @examples
#' \dontrun{
#'topics <- calculate_topics(text_network,
#'                           documents = NULL,
#'                           document_tokens = NULL,
#'                           document_ids = NULL,
#'                           negative_edge_weights = TRUE,
#'                           page_rank_calculation = "global",
#'                           cluster_function = cluster_leiden,
#'                           objective_function = "CPM",
#'                           keep_cluster_object = FALSE,
#'                           verbose = TRUE)
#' # optionally, we can add a dataframe with document information to get relevant documents
#' data("de_pol_twitter")
#'topics <- calculate_topics(text_network,
#'                           documents = de_pol_twitter %>% # when passing document data...
#'                               dplyr::select(doc_id, lemma, # ...we reduce the columns to the required variables...
#'                                             created_at, author_id, party, tweet_url), #...and document (not token!) metadata
#'                           document_tokens = "lemma",
#'                           document_ids = "doc_id",
#'                           negative_edge_weights = TRUE,
#'                           page_rank_calculation = "global",
#'                           cluster_function = cluster_leiden,
#'                           objective_function = "CPM",
#'                           keep_cluster_object = FALSE,
#'                           verbose = TRUE)
#' }
#'
#' @importFrom igraph is_igraph subgraph.edges induced_subgraph page_rank modularity sizes cluster_leiden V E
#' @importFrom dplyr mutate left_join summarise arrange desc distinct n across select_if
#' @importFrom dplyr "%>%"
#' @importFrom tibble tibble
#' @importFrom furrr future_map
#' @importFrom data.table rbindlist
#' @importFrom stats median
#' @importFrom rlang arg_match
#' @importFrom tidyr any_of
#' @importFrom tidytext bind_tf_idf
#' @importFrom tidyselect everything
#' @importFrom scales rescale
#'
#' @export

calculate_topics <- function(text_network,
                             documents = NULL,
                             document_tokens = NULL,
                             document_ids = NULL,
                             negative_edge_weights = TRUE,
                             page_rank_calculation = c("global", "cluster"),
                             cluster_function = igraph::cluster_leiden,
                             ...,
                             keep_cluster_object = FALSE,
                             verbose = TRUE
) {
  # Argument & data checks
  rlang::arg_match(page_rank_calculation)

  if (!(igraph::is_igraph(text_network))){
    stop("text_network must be an igraph graph")
  }

  if (!is.null(documents)){
    if (is.null(document_tokens) | is.null(document_ids)){
      stop("'document_tokens' and 'document_ids' columns must be specified.")
    }
    if (!(document_tokens %in% names(documents))) {
      stop(paste(document_tokens, "not present in 'documents' object."))
    }
    if (!(document_ids %in% names(documents))) {
      stop(paste(document_ids, "not present in 'documents' object."))
    }

  }



  # Drop negatively weighted edges
  if (!(negative_edge_weights)){
    text_network <- igraph::subgraph.edges(text_network,
                                           which(E(text_network)$weight > 0))
  }

  # Calcualte Topic Clusters
  cluster <- cluster_function(text_network, ...)

  topics <- tibble::tibble(entity = igraph::V(text_network)$name,
                           topic = igraph::membership(cluster) %>% as.numeric())

  # Calculate Page Rank
  if (page_rank_calculation == "global") { # calculate global page rank
    page_rank <- igraph::page_rank(text_network)$vector

    topics <- topics %>% dplyr::mutate(page_rank = page_rank)
  }

  if (page_rank_calculation == "cluster") { # subset the graph by cluster and calculate page rank in clusters
    igraph::V(text_network)$cluster <- igraph::membership(cluster)

    page_rank <- unique(topics$topic) %>% # to do: check if future_map speeds this up
      furrr::future_map(\(topic) {
        subgraph <- igraph::induced_subgraph(text_network,
                                             which(V(text_network)$cluster == topic),
                                             impl = "create_from_scratch")

        page_rank <- igraph::page_rank(subgraph)$vector

        page_rank_cluster <- tibble::tibble(entity = names(page_rank),
                                            page_rank = page_rank,
                                            topic = topic)
        return(page_rank_cluster)
      }) %>% data.table::rbindlist()

    topics <- topics %>% dplyr::left_join(page_rank, by = c("entity", "topic"))

  }

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

  # prepare document data
  if (!is.null(documents)) {
    tf_idf <- topics %>%
      dplyr::left_join(documents,
                       by = dplyr::join_by(entity == !!as.name(document_tokens))) %>%
      dplyr::summarise(n = dplyr::n(), # calculate tf_idf
                       .by = c(entity, !!as.name(document_ids),
                               topic, page_rank)) %>%
      tidytext::bind_tf_idf(entity, !!as.name(document_ids), n) %>%
      dplyr::mutate(
        term_relevance = (tf_idf * page_rank) %>% # calculate topic term relevance as normalized (tf_idf * page_rank)
          scales::rescale(to = c(0, 1)),
        .by = topic
      )

    document_data <- tf_idf %>%
      dplyr::summarise(entities = list(entity),
                       document_relevance = sum(term_relevance), # sum up term relevance per doc
                       .by = c(topic, !!as.name(document_ids))) %>%
      dplyr::mutate(document_relevance = scales::rescale(document_relevance, # rescale per topic
                                                         to = c(0,100)),
                    .by = topic) %>%
      dplyr::arrange(topic, dplyr::desc(document_relevance))

    document_data <- document_data %>%
      dplyr::left_join(documents %>%
                         dplyr::select(!(!!as.name(document_tokens))) %>%
                         dplyr::distinct(!!as.name(document_ids),
                                         .keep_all = TRUE), by = document_ids)

    missing_data <- document_data %>% # collect missing meta data
      dplyr::select(!c(topic, entities, document_relevance)) %>%
      dplyr::select_if(~ any(is.na(.))) %>%
      dplyr::summarise(dplyr::across(tidyselect::everything(),
                                     ~ (sum(is.na(.)) / n()))) %>%
      dplyr::select_if(~ (. > 0))

    if (nrow(missing_data) > 0) {
      warning("Missing data in variables ",
              paste(names(missing_data), collapse = ", "),
              ".\n",
              paste(names(missing_data %>%
                            dplyr::select_if(~ (. > 0.1))),
                    collapse = ", "),
              " miss more than 10% of data.\n",
              "Did you provide the correct 'document' data?")
    }

  }

  # make topic overviews
  topic_overview <- topics %>%
    dplyr::summarise(nr_entities = dplyr::n(), .by = topic) %>%
    dplyr::arrange(dplyr::desc(nr_entities))

  if (!is.null(documents)) {
    document_overview <- document_data %>%
      dplyr::summarise(document_occurrences = dplyr::n(),
                       .by = topic)

    topic_overview <- topic_overview %>%
      dplyr::left_join(document_overview, by = "topic") %>%
      dplyr::arrange(dplyr::desc(document_occurrences)) # overwrite ordering

    # additional metrics
    mean_document_occurrences <- mean(topic_overview$document_occurrences)

    median_document_occurrences <- stats::median(topic_overview$document_occurrences)

    metrics$mean_document_occurrences <- mean_document_occurrences

    metrics$median_document_occurrences <- median_document_occurrences

  }

  if (verbose) {
    if (!is.null(documents)) {
        cat(paste(
          "\nMean Number of Documents per Topic:", mean_document_occurrences,
          "\nMedian Number of Documents per Topic:", median_document_occurrences
        ))
    }
    cat("\n")
  }



  # topic_object <- topics %>%
  #   split(.$topic)%>%
  #   purrr::imap(\(topic, id)
  #               {
  #                 if (!is.null(documents)) {
  #                   entities <- topic %>%
  #                     dplyr::select(!tidyr::any_of(names(documents))) %>%
  #                     dplyr::distinct() %>%
  #                     dplyr::arrange(dplyr::desc(page_rank))
  #                 } else {
  #                   entities <- topic %>%
  #                     dplyr::arrange(dplyr::desc(page_rank))
  #                 }
  #
  #                 res <- list(entities = entities)
  #
  #                 if (!is.null(documents)){ # add document data with tf_idf
  #                   document_data <- tf_idf %>%
  #                     dplyr::filter(topic == id) %>%
  #                     dplyr::summarise(entities = list(entity),
  #                                      document_relevance = sum(term_relevance), # maybe rescale this, but fine-grained
  #                                      .by = document_ids) %>%
  #                     dplyr::mutate(topic = id) %>%
  #                     dplyr::arrange(dplyr::desc(document_relevance))
  #
  #                   res$documents <- document_data
  #                 }
  #                 return(res)
  #   })
  #
  # textgraph_topic <- list(
  #   topics = topic_object,
  #   metrics = metrics
  # )

  # Finalize Topic Object
  textgraph_topic <- list(
    metrics = metrics,
    topics = topic_overview,
    entities = topics %>%
      dplyr::arrange(topic, dplyr::desc(page_rank))
  )

  if (!is.null(documents)) {
    textgraph_topic$documents <- document_data
  }

  if (keep_cluster_object) {
    textgraph_topic$igraph_cluster <- cluster
  }

  class(textgraph_topic) <- "textgraph_topics"

  return(textgraph_topic)
}

