#' Calculate Time-Dynamic Topic Clusters
#'
#' A function to calculate textgraph Topic Clusters dynamically by calculating clusters for each snapshot
#'  and matching clusters between the snapshots via the Hungarian Method. Note that, depending on network
#'  structure and lookback, not all entities in the network may end up in a topic.
#'  Can be parallelized with [future::plan()].
#'  Requires a Python Environment, preferably installed with [install_community_matching()].
#'
#' @param data Data frame containing the network data.
#' @param document String; Name of the document ID column in `data` and (optionally) `full_documents`.
#' @param feature String; Name of the feature (e.g. token) column in `data` and (optionally) `full_documents`.
#' @param timeframe String; Name of the column indicating the timeframe to base the snapshots on
#' @param lookback Numerical; Length of the memory for the matching of snapshot communities. See details.
#' @param full_documents Optional, a data frame containing document information. Expects one row per token. NULL to skip.
#'  Document ID and feature columns must have the same names as in `data`. If provided, returns additional information on topic-relevant documents.
#'  Additional variables (such as document meta data) can be passed. Note, however,
#'  that these should contain no more than one unique value per document (see examples).
#' @param pmi_weight Logical indicating whether weights should be calculated based on the PMI (Pointwise Mutual Information) of
#'  `document` and `feature`. If `FALSE`, a simple co-occurrence weighting is performed.
#' @param negative_edge_weights Logical indicating whether to consider only edges
#'   with positive weights. If the chosen clustering algorithm does not support negative edge weights,
#'   set this to `FALSE`.
#' @param page_rank_calculation Character specifying the type of Page Rank calculation.
#'   Options are "global" for global network Page Rank, calculating a full network after the snapshots;
#'   "cluster" for calculating the Page Rank of the entities within each dynamic topic cluster by making subgraphs from the global network (slow, but can be parallelized);
#'   and "avg" for averaging the page rank of each entity over the snapshots (fastest).
#' @param cluster_function The clustering function to use. Can be any igraph clustering function.
#'  Default is `igraph::cluster_leiden`.
#' @param ... Additional arguments passed to `cluster_function`.
#' @param match_clusters Logical. Should the matching algorithm be employed to match the snapshot clusters into temporal topics? Requires
#'  `python_env` if `TRUE`. Setting this to `FALSE` can be helpful to test different clustering settings without running the (potentiall costly)
#'  matching algorithm.
#' @param python_env Python environment to use for the dynamic community matching. Set this up with [install_community_matching()]. See details.
#' @param keep_cluster_objects Logical indicating whether to keep the igraph cluster object of each snapshot.
#'  Can be helpful for additional checks. See `help(igraph::membership)`. Also returns clustering metrics for each snapshot cluster.
#' @param keep_networks Logical indicating whether to keep the igraph network of each snapshot.
#' @param verbose Logical indicating whether to print clustering metrics.
#' @param seed Seed for the parallelization, if a parallel plan is set up (see details).
#'  TRUE to let future set a seed. Numerical value to set a seed. NULL to not set any seed.
#'
#' @return A list object of class "textgraph_topics" containing topic entities,
#'   document data (optional), clustering metrics, and, optionally, the snapshot data (cluster, networks).
#'
#' @details For each snapshot (determined by the `timeframe` variable in `data`), a static cluster is calculated
#'  via the specified `cluster_function`. Then, these results are matched with the results of adjacent timesteps
#'  to get a temporal cluster detection. The number previous clusters considered in the matching can be set with `lookback`.
#'  The matching implementation is Philipp Lorenz' (\href{https://github.com/philipplorenz/memory_community_matching}{Memory Community Matching}).
#'  As this is a Python implementation, an installation of Python and Reticulate are required. The necessary Python
#'  environment can then be set up with [install_community_matching()]. If no environment is specified there,
#'  it conveniently sets up a fresh `textgraph` environment to which `calculate_dynamic_topics()` defaults.
#'
#'  As the process of calculating snapshots and their clusters can be somewhat time consuming for large networks,
#'  this function can be parallelized. To make use of parallelization, simply set up a parallelization through the
#'  `future` package with [future::plan()] - no further action required. If parallelization is set up, the page rank calculation for
#'  `page_rank_calculation = "cluster"` is parallelized as well. If parallelization is active, it is advised to
#'  set a seed for the random number generation involved in the clustering. Setting `seed = TRUE` lets `future` set
#'  a seed, but a custom seed can be provided for reproducability.
#'
#'
#' @references Lorenz, Philipp et al. 2018. “Capturing the Dynamics of Hashtag-Communities.” In: Complex Networks & Their Applications VI, Studies in Computational Intelligence.
#' (\href{https://doi.org/10.1007/978-3-319-72150-7_33}{DOI: 10.1007/978-3-319-72150-7_33})
#'
#' @importFrom dplyr "%>%"
#' @importFrom dplyr mutate n dense_rank rename distinct mutate pull filter summarise left_join join_by arrange desc n
#' @importFrom stats median
#' @importFrom furrr future_map furrr_options
#' @importFrom purrr imap map
#' @importFrom reticulate use_virtualenv source_python py_capture_output
#' @importFrom data.table as.data.table rbindlist
#' @importFrom igraph cluster_leiden page_rank
#' @importFrom ggplot2 ggplot labs geom_point aes
#' @importFrom tibble tibble rownames_to_column
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data("de_pol_twitter")
#' dynamic_topics <- calculate_dynamic_topics(
#'   data = de_pol_twitter %>%
#'     dplyr::mutate(day = lubridate::floor_date(created_at, unit = "days")),
#'   document = "doc_id",
#'   feature = "lemma",
#'   timeframe = "day",
#'   lookback = 3,
#'   full_documents = NULL,
#'   pmi_weight = TRUE,
#'   negative_edge_weights = TRUE,
#'   page_rank_calculation = "avg",
#'   cluster_function = igraph::cluster_leiden,
#'   python_env = "textgraph")
#'
#' # optionally, we can add a dataframe with document information to get relevant documents
#' dynamic_topics <- calculate_dynamic_topics(
#'   data = de_pol_twitter %>%
#'     dplyr::mutate(day = lubridate::floor_date(created_at, unit = "days")), # make a "day" indicator to use as "timeframe"
#'   document = "doc_id",
#'   feature = "lemma",
#'   timeframe = "day",
#'   lookback = 3,
#'   full_documents = de_pol_twitter %>% # when passing document data...
#'     dplyr::select(doc_id, lemma, # ...we reduce the columns to the required variables...
#'                   created_at, author_id, party, tweet_url), #...and document (not token!) metadata
#'   pmi_weight = TRUE,
#'   negative_edge_weights = TRUE,
#'   page_rank_calculation = "avg",
#'   cluster_function = igraph::cluster_leiden,
#'   python_env = "textgraph")
#' }

calculate_dynamic_topics <- function(data,
                                     document,
                                     feature,
                                     timeframe,
                                     lookback,
                                     full_documents = NULL,
                                     pmi_weight = TRUE,
                                     negative_edge_weights = TRUE,
                                     page_rank_calculation = c("global",
                                                               "cluster",
                                                               "avg"),
                                     cluster_function = igraph::cluster_leiden,
                                     ...,
                                     seed = TRUE,
                                     match_clusters = TRUE,
                                     python_env = "textgraph",
                                     keep_cluster_objects = FALSE,
                                     keep_networks = FALSE,
                                     verbose = TRUE
){

  # Load Python
  if (match_clusters){
    reticulate::use_virtualenv(python_env)
    reticulate::source_python(file.path(path.package("textgraph"),    # load the python function
                                        "dynamic_community_matching.py"))
  }

  # Data Checks
  if (!(document %in% names(data))) {
    stop(paste0("document '", document, "' not in data.\n"))
  }

  if (!(feature %in% names(data)) ){
    stop(paste0("feature '", feature, "' not in data.\n"))
  }

  if (!(is.data.frame(data))) {
    stop("'data' must be a data frame.\n")
  }


  if (verbose) {
    cat("Making Network Snapshots...\n")
  }

  snapshots <- data %>%
    data.table::as.data.table() %>%
    split(by = timeframe) %>%
    furrr::future_map(\(snapshot)
                      {
                        # calculate the network for each snapshot
                        text_network <- calculate_network(data = snapshot,
                                                          document = document,
                                                          feature = feature,
                                                          pmi_weight = pmi_weight,
                                                          as_data_frame = FALSE,
                                                          keep_negative_weights = negative_edge_weights)

                        # calculate the clusters for each snapshot
                        cluster <- cluster_function(text_network, ...)

                        out <- list(cluster = cluster)

                        # calculate global snapshot page rank
                        if (page_rank_calculation == "avg") {
                          page_rank <- igraph::page_rank(text_network)$vector
                          out$page_rank <- page_rank
                        }

                        # return output
                        if (keep_networks) {
                          out$network <- text_network
                        }

                        return(out)
    },
    .progress = verbose,
    .options = furrr::furrr_options(seed = seed)) # let future_map set a seed for the RNG involved in the cluster calculation

  community_data <- snapshots %>%
    purrr::imap(\(snapshot, time)
               {
                 data.frame(
                   time = time, # add timeframe indicator
                   community = snapshot %>% .[["cluster"]] %>% .$membership,
                   node = snapshot %>% .[["cluster"]] %>% .$name
                 )
    }) %>% data.table::rbindlist()

  if (match_clusters) {

    if (verbose) {
      cat("\nDynamic Community Matching...")
    }

    reticulate::py_capture_output({ # this suppresses any additional python printout
      dynamic_communities <- dynamic_community_matching(community_data,
                                                        as.integer(lookback)) %>%
        suppressWarnings() # supresses warning about missing row names
    })

    community_data <- community_data %>% # add temporal communities
      mutate(snapshot = dplyr::dense_rank(time)) %>%
      dplyr::left_join(dynamic_communities, by = c("snapshot", "community"))

    topics <- community_data %>%
      dplyr::distinct(node, temporal_community)

  } else {

    topics <- community_data %>%
      dplyr::mutate(temporal_community = paste0(time, "_", community))

  }

  if (verbose) {
    cat("\nCalculating Page Ranks...")
  }

  if (page_rank_calculation == "avg") {
    avg_pageranks <- snapshots %>%
      purrr::imap(\(snapshot, time)
                 {
                   snapshot$page_rank %>%
                     as.data.frame() %>%
                     tibble::rownames_to_column(var = "node") %>%
                     dplyr::rename(page_rank = ".") %>%
                     dplyr::mutate(time = time)
      }) %>% data.table::rbindlist() %>%
      dplyr::summarise(page_rank = mean(page_rank),
                       .by = node)

    topics <- topics %>%
      dplyr::left_join(avg_pageranks, by = "node")
  } else {
    # if not using average page ranks from the snapshots, calculate the global network
    text_network <- calculate_network(data = data,
                                      document = document,
                                      feature = feature,
                                      pmi_weight = pmi_weight,
                                      as_data_frame = FALSE,
                                      keep_negative_weights = negative_edge_weights)

    if (page_rank_calculation == "global") { # global network
      page_rank <- igraph::page_rank(text_network)$vector %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "node") %>%
        dplyr::rename(page_rank = ".")

      topics <- topics %>% dplyr::left_join(page_rank, by = "node")
    }

    if (page_rank_calculation == "cluster") {
      # global page_rank within each cluster
      page_rank <- topics %>%
        dplyr::filter(!is.na(temporal_community)) %>%
        dplyr::left_join(data,
                         by = dplyr::join_by(node == !!as.name(feature)),
                         relationship = "many-to-many") %>% # nodes can be in multiple communities, hence "many-to-many"
        split(.$temporal_community) %>%
        furrr::future_imap(\(nodes, community)
                           { text_network <- calculate_network(data = nodes,
                                                               document = document,
                                                               feature = "node",
                                                               pmi_weight = pmi_weight,
                                                               as_data_frame = FALSE,
                                                               keep_negative_weights = negative_edge_weights)

                             page_rank <- igraph::page_rank(text_network)$vector

                             page_rank_cluster <- tibble::tibble(node = names(page_rank),
                                                                 page_rank = page_rank,
                                                                 temporal_community = as.numeric(community))
                             return(page_rank_cluster)
        }) %>% data.table::rbindlist()

      topics <- topics %>% dplyr::left_join(page_rank, by = c("node", "temporal_community"))
    }
  }

  # Calculate Metrics

  if (verbose) {
    cat("\nPreparing Topics...")
  }

  snapshot_metrics <- snapshots %>%
    purrr::map(\(snapshot)
               {
                 calculate_cluster_metrics(snapshot$cluster,
                                           page_rank_calculation,
                                           verbose = F, ...)
    })

  ## Metrics Overview Plot Data
  plot_data <- snapshot_metrics %>%
    data.table::rbindlist(idcol = "snapshot")

  plot <- plot_data %>% # plot skeleton
    ggplot2::ggplot(ggplot2::aes(x = nr_topics, color = snapshot)) +
    theme_bw()

  ## Algorithm Metrics
  algorithm <- unique(plot_data$algorithm)

  metrics <- list(
    algorithm = algorithm)

  if (match_clusters) {
    metrics$lookback <- lookback
  }

  ## additional clustering algorithm parameters
  params <- list(...) # get additional parameters

  if (length(params) > 0) {
    metrics <- c(metrics, params)
  }

  ## Overall Metrics

  page_rank_calculation <- unique(plot_data$page_rank_calculation)

  if (match_clusters) {
    nr_temporal_topics <- topics %>%
      dplyr::filter(!is.na(temporal_community)) %>%
      dplyr::distinct(temporal_community) %>%
      nrow()

    mean_temporal_topic_entities <- topics %>%
      dplyr::filter(!is.na(temporal_community)) %>%
      dplyr::summarise(n = n(), .by = temporal_community) %>%
      dplyr::pull(n) %>%
      mean()

    median_temporal_topic_entities <- topics %>%
      dplyr::filter(!is.na(temporal_community)) %>%
      dplyr::summarise(n = n(), .by = temporal_community) %>%
      dplyr::pull(n) %>%
      stats::median()

    topicless_entities <- topics %>%
      dplyr::filter(is.na(temporal_community)) %>%
      dplyr::distinct(node) %>%
      nrow()

    topic_entities <- topics %>%
      dplyr::filter(!is.na(temporal_community)) %>%
      dplyr::distinct(node) %>%
      nrow()
  } else {
    unavailable <- "No Temporal Topics Calculated."

    nr_temporal_topics <- unavailable

    mean_temporal_topic_entities <- unavailable

    median_temporal_topic_entities <- unavailable

    topicless_entities <- unavailable

    topic_entities <- unavailable
  }

  mean_nr_snapshot_topics <- plot_data %>%
    dplyr::filter(!is.na(nr_topics)) %>%
    dplyr::pull(nr_topics) %>%
    mean()

  median_nr_snapshot_topics <- plot_data %>%
    dplyr::filter(!is.na(nr_topics)) %>%
    dplyr::pull(nr_topics) %>%
    stats::median()

  metrics <- c(metrics,
               list(
                 page_rank_calculation = page_rank_calculation,
                 nr_temporal_topics = nr_temporal_topics,
                 mean_temporal_topic_entities = mean_temporal_topic_entities,
                 median_temporal_topic_entities = median_temporal_topic_entities,
                 entities_in_topics = topic_entities,
                 entities_without_topics = topicless_entities,
                 mean_nr_snapshot_topics = mean_nr_snapshot_topics,
                 median_nr_snapshot_topics = median_nr_snapshot_topics
               ))

  ## Metric-dependent variables
  if (is.numeric(plot_data$quality)) {
    mean_snapshot_quality <- plot_data %>%
      dplyr::filter(!is.na(nr_topics)) %>%
      dplyr::pull(quality) %>%
      mean()

    median_snapshot_quality <- plot_data %>%
      dplyr::filter(!is.na(nr_topics)) %>%
      dplyr::pull(quality) %>%
      stats::median()

    metrics$mean_snapshot_quality <- mean_snapshot_quality
    metrics$median_snapshot_quality <- median_snapshot_quality

    metrics$snapshot_plot <- plot +
      ggplot2::geom_point(ggplot2::aes(y = quality)) +
      ggplot2::labs(title = "Quality and Entities of Topics in Snapshots")
  } else {
    metrics$snapshot_quality <- unique(plot_data$quality)
  }

  if (is.numeric(plot_data$modularity)) {
    mean_snapshot_modularity <- plot_data %>%
      dplyr::filter(!is.na(nr_topics)) %>%
      dplyr::pull(modularity) %>%
      mean()

    median_snapshot_modularity <- plot_data %>%
      dplyr::filter(!is.na(nr_topics)) %>%
      dplyr::pull(modularity) %>%
      stats::median()

    metrics$mean_snapshot_modularity <- mean_snapshot_modularity
    metrics$median_snapshot_modularity <- median_snapshot_modularity

    metrics$snapshot_plot <- plot +
      ggplot2::geom_point(ggplot2::aes(y = modularity)) +
      ggplot2::labs(title = "Modularity and Entities of Topics in Snapshots")
  } else {
    metrics$snapshot_modularity <- unique(plot_data$modularity)
  }

  ## Verbosity Printout & Plot
  if (verbose) {
    cat(
      "\nAlgorithm:", algorithm,
      "\nLookback:", lookback,
      "\nPage Rank Calculation:", page_rank_calculation,
      "\nNumber of Temproal Topics:", nr_temporal_topics,
      "\nTotal Entities in Temporal Topics:", topic_entities,
      "\nTotal Entities without Temporal Topic:", topicless_entities,
      "\nMean Number of Entities per Temporal Topic:", mean_temporal_topic_entities,
      "\nMedian Number of Entities per Temporal Topic:", median_temporal_topic_entities,
      "\nSnapshot Metrics:",
      "\n Mean Number of Topics in Snapshots:", mean_nr_snapshot_topics,
      "\n Median Number of Topics in Snapshots:", median_nr_snapshot_topics
    )

    if (is.numeric(plot_data$quality)) {
      cat(
        "\n Mean Snapshot Clustering Quality:", mean_snapshot_quality,
        "\n Median Snapshot Clustering Quality:", median_snapshot_quality
      )
      print(metrics$snapshot_plot)
    } else {
      cat(
        "\n Snapshot Quality:", unique(plot_data$quality))}

    if (is.numeric(plot_data$modularity)) {
      cat(
        "\n Mean Snapshot Clustering Quality:", mean_snapshot_modularity,
        "\n Median Snapshot Clustering Quality:", median_snapshot_modularity
      )
      print(metrics$snapshot_plot)
    } else {
      cat(
        "\n Snapshot Modularity:", unique(plot_data$modularity))}
  }

  # Prepare output
  topics <- topics %>%
    dplyr::rename(entity = node, # rename for consistency
                  topic = temporal_community) %>%
    dplyr::filter(!is.na(topic)) # drop entities without a topic

  if (!is.null(full_documents)) {
    document_data <- prepare_document_data(topics,
                                           full_documents,
                                           document_tokens = feature,
                                           document_ids = document)
  }

  # make topic overviews
  topic_overview <- topics %>%
    dplyr::summarise(nr_entities = dplyr::n(), .by = topic) %>%
    dplyr::arrange(dplyr::desc(nr_entities))

  if (!is.null(full_documents)) {
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
    if (!is.null(full_documents)) {
      if (match_clusters) {
        cat(
          "\nDocument Metrics for Temporal Topics:"
        )
      } else {
        cat(
          "\nDocument Metrics for Snapshot Topics:"
        )
      }
      cat(
        "\n Mean Number of Documents per Topic:", mean_document_occurrences,
        "\n Median Number of Documents per Topic:", median_document_occurrences
        )
    }
    cat("\n")
  }

  textgraph_topic <- list(
    metrics = metrics,
    topics = topic_overview,
    entities = topics %>%
      dplyr::arrange(topic, dplyr::desc(page_rank))
  )

  if (!is.null(full_documents)) {
    textgraph_topic$documents <- document_data
  }

  if (keep_cluster_objects) {
    textgraph_topic$snapshots$igraph_cluster <- snapshots %>%
      purrr::map(\(snapshot) snapshot$cluster)

    textgraph_topic$snapshots$cluster_metrics <- snapshot_metrics
  }

  if (keep_networks) {
    textgraph_topic$snapshots$networks <- snapshots %>%
      purrr::map(\(snapshot) snapshot$network)
  }

  class(textgraph_topic) <- "textgraph_topics"

  return(textgraph_topic)
}
