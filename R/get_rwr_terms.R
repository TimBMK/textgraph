#' Random Walk with Restart on Multiplex Networks
#'
#' This function performs Random Walk with Restart on multiplex networks
#' and retrieves terms associated with mutliple seed nodes. It supports mutli-threading, see Details
#'
#' @param walk_network An object created by `make_multiplex_objects()` that contains
#'        the multiplex network and the Normalized Adjacency Matrix.
#' @param network_name Optional name of the network used for matching the match_var;
#'        usually the index when implemented in imap functions; set to NULL to skip.
#'        Requires match_var.
#' @param seeds Either a data frame including `seed_var` and `group_name` as variables, or a list of data frames,
#'        where each entry is a dataframe with the `seed_var`.
#' @param seed_var String; variable name in the seed dataframes containing the seed terms.
#' @param match_var Optional variable in the seed dataframes used to match on the
#'        network_name variable; set to NULL to skip. Requires network_name. This allows to
#'        e.g. only run random walks from seeds associated with a specific timeframe if the
#'        timeframe is provided as the network_name.
#' @param flatten_results Logical indicating whether to flatten the results into a single
#'        dataframe or return a list of dataframes for each group.
#' @param group_name String; Name of the grouping variable.
#' @param positive_scores_only Logical indicating whether to drop negative Walk Scores
#'        and 0 scores ( = very unlikely connection) before normalization.
#' @param normalize_score How to normalize scores; "seeds" normalizes the scores for each seed walk,
#'        "group" normalizes within the grouping vars; NULL to skip. Normalizes the raw scores between 0 and 1.
#'        Non-normalized scores are always returned. To return normalized scores over both seeds and groups,
#'        set c("seeds", "groups").
#' @param walk_score_measure Basis for `walk_score` filtering when multiple normalized scores are returned;
#'        can be "default" (the chosen normalization, defaults to its mean if `calculate_means` is `TRUE`),
#'        "raw" (the raw score), "seeds", "seeds_mean" (if `calculate_means` is `TRUE`), "group",
#'        or "group_mean" (if `calculate_means` is `TRUE`).
#' @param walk_score Numerical; Minimum walk score to retain in results; applied to normalized or raw scores
#'        depending on `normalize_score`; always applies to normalized score if available.
#'        Set `normalize_score` to `NULL` to filter on the raw score (not recommended).
#'        `NULL` to skip. Note that, depending on the network, the return object can become very large when skipped,
#'        as a score for each node in the network is returned.
#'        If `walk_score_quantile` is `TRUE`, this value specifies the quantile in each group.
#' @param walk_score_quantile Logical indicating whether to calculate a dynamic minimum walk_score
#'        for each group based on quantiles. If `TRUE`, `walk_score` specifies the quantile, rather than a
#'        fixed value, that is, values lower than the value of this quantile are dropped.
#' @param report_quantiles Logical indicating whether to print quantile values for each group. Returns the quantiles of
#'        the selected `walk_score_measure` before any walk_score filtering (i.e. the quantiles used if `walk_score_quantile` is `TRUE`)
#' @param calculate_means Logical indicating whether to calculate score means. If `TRUE`, means for all scores,
#'        normalized or not, are returned.
#' @param normalize_means Logical indicating whether to normalize the mean scores between 0 and 1.
#' @param reduce_to_means Logical indicating whether to reduce output to mean score values only. Recommend for large networks,
#'        as this significantly reduces the size of the returned object.
#' @param keep_seed_terms Logical indicating whether seed terms should always be retained regardless
#'        of their score; only available if flatten_results is TRUE.
#' @param seedterm_value Optional numerical value to overwrite the score of seed terms with a fixed value, e.g. to rank them generally
#' higher than the terms calculated by the random walks. Set to NULL to skip.
#' @param progress Logical indicating whether to show the progress for the map function.
#'
#' @return A dataframe or a list of dataframes with Random Walk with Restart results, based on flatten_results.
#'
#' @details
#' The function allows to run Random Walks with Restart (RWR) over a number of seeds of different groups, returning results for each group.
#'  It can be used on a text network to extract features functionally equivalent to a number of seed nodes, returning additional terms
#'  closely connected to the seed terms provided. By supporting multiple groups of seed nodes, it is easy to compare and analyse cases
#'  such as the terms connected to the seeds for different topics or those provided by different groups of actors.
#'  It uses the RandomWalkRestartMH Algorithm developed by Valdeolivas et al. (https://github.com/alberto-valdeolivas/RandomWalkRestartMH).
#'  Note that, if multiple seeds are provided in a group, the function returns results for each individual seed, rather than running RWRs
#'  starting at multiple nodes at once.
#'
#' Multi-threading is supported via the furrr/future packages. Set up a multi-threading plan with `future::plan()` before executing the
#'  function to enable multithreading for multiple cores, sessions or clusters. Note that, for multi-threading, the full network needs to
#'  be exported to each node. When exporting large networks in this way, ensure that you have sufficient RAM, as nodes running out of
#'  RAM may fail abruptly and without notice.
#'
#' @references A Valdeolivas, L Tichit, C Navarro, S Perrin, G Odelin, N Levy, P Cau, E Remy, and A Baudot. 2018.
#'  “Random walk with restart on multiplex and heterogeneous biological networks.” Bioinformatics 35 (3)
#'
#' @examples
# the multiplex_text_network object is created by make_multiplex_object() - see help(make_multiplex_object)
# the seed_terms object is created by get_seed_terms() - see help(get_seed_terms)
#'
#' rwr_terms <- get_rwr_terms(walk_network = multiplex_text_network,
#'                            network_name = NULL,
#'                            seeds = seed_terms,
#'                            seed_var = "feature",
#'                            match_var = NULL,
#'                            flatten_results = TRUE,
#'                            group_name = "ministry_name",
#'                            positive_scores_only = FALSE,
#'                            normalize_score = "seeds",
#'                            walk_score = 0.9,
#'                            walk_score_quantile = TRUE,
#'                            report_quantiles = TRUE,
#'                            walk_score_measure = "seeds_mean",
#'                            calculate_means = TRUE,
#'                            normalize_means = TRUE,
#'                            reduce_to_means = TRUE,
#'                            keep_seed_terms = TRUE,
#'                            seedterm_value = NULL,
#'                            progress = TRUE)
#'
#'
#' @importFrom dplyr "%>%"
#' @importFrom rlang arg_match
#' @importFrom stats quantile
#' @importFrom dplyr filter distinct pull mutate case_when left_join rename_with summarise across starts_with ends_with relocate select group_by ungroup
#' @importFrom purrr imap compact set_names
#' @importFrom data.table rbindlist
#' @importFrom scales rescale
#' @importFrom furrr future_map
#' @importFrom tibble as_tibble
#' @importFrom RandomWalkRestartMH isMultiplex
#'
#' @export
get_rwr_terms <- function(walk_network,
                          network_name = NULL,
                          seeds,
                          seed_var,
                          match_var = NULL,
                          flatten_results = TRUE,
                          group_name = "group",
                          positive_scores_only = FALSE,
                          normalize_score = c(NULL, "seeds", "group"),
                          walk_score = 0.9,
                          walk_score_quantile = TRUE,
                          report_quantiles = TRUE,
                          walk_score_measure = c("default", "raw", "seeds", "seeds_mean", "group", "group_mean"),
                          calculate_means = TRUE,
                          normalize_means = TRUE,
                          reduce_to_means = FALSE,
                          keep_seed_terms = TRUE,
                          seedterm_value = NULL,
                          progress = FALSE)
{

  if (!is.null(normalize_score)){ # arg_match does not handle NULL values, thus we skip it if the score normalization is skipped via NULL
    normalize_score <- rlang::arg_match(normalize_score, multiple = TRUE) # check for correct argument specification here
  }
  walk_score_measure <- rlang::arg_match(walk_score_measure, multiple = FALSE) # check for correct argument specification here

  # format checks
  if (is.null(walk_network$multiplex) | is.null(walk_network$AdjMatrixNorm)) {
    stop("walk_network is missing the multiplex object or the adjacency matrix")
  }

  if (!RandomWalkRestartMH::isMultiplex(walk_network$multiplex)) {
    stop("walk_network$multiplex is not a valid multiplex object")
  }

  if (class(walk_network$AdjMatrixNorm) != "dgCMatrix") {
    stop("walk_network$AdjMatrixNorm is not a valid dgCMatrix")
  }


  # setting of variables
  if (reduce_to_means & !calculate_means) { # set reduce_to_means to FALSE if no means are calculated
    message("No means calculated. Not reducing output to means.")
    reduce_to_means <- FALSE
  }

  #  if (!is.null(walk_score)) { # set the variable that the walk_score filter will be applied on

  if (length(normalize_score) > 1 &
      walk_score_measure == "default") {
    stop("More than one normalization method selected, but no valid walk_score_measure selected. Select one of the normalization methods to filter the walk_score on, or set walk_score to NULL to skip")
  }

  if (walk_score_measure == "default") { # set default measure for filtering...

    if (is.null(normalize_score)) {
      filter_var <- "Score"
      message("Applying walk_score to raw RWR scores. You can change this by setting the walk_score_measure")
    } else {

      if (calculate_means) {

        if (normalize_score == "seeds") {
          filter_var <- "ScoreNormMean"
          message("Applying walk_score to mean seed scores. You can change this by setting the walk_score_measure")
        }

        if (normalize_score == "group" ) {
          filter_var <- "ScoreNormGroupMean"
          message("Applying walk_score to mean group scores. You can change this by setting the walk_score_measure")
        }

      } else {

        if (normalize_score == "seeds") {
          filter_var <- "ScoreNorm"
          message("Applying walk_score to seed-normalized scores. You can change this by setting the walk_score_measure")
        }

        if (normalize_score == "group" ) {
          filter_var <- "ScoreNormGroup"
          message("Applying walk_score to group-normalized scores. You can change this by setting the walk_score_measure")
        }
      }
    }
  } else { # ... or accommodate explicit settings

    if (walk_score_measure == "raw") {
      filter_var <- "Score"
    }

    if (walk_score_measure == "seeds") {
      filter_var <- "ScoreNorm"
    }

    if (walk_score_measure == "seeds_mean") {
      filter_var <- "ScoreNormMean"
    }

    if (walk_score_measure == "group" ) {
      filter_var <- "ScoreNormGroup"
    }

    if (walk_score_measure == "group_mean" ) {
      filter_var <- "ScoreNormGroupMean"
    }
  }

  #  }

  if (!is.null(seedterm_value) & !is.numeric(seedterm_value)) {
    stop("seedterm_value must either be a numerical value to set the seed term mean to or NULL to skip")
  }

  # Run Random Walks

  if (is.data.frame(seeds)) { # if the seed terms are not provided as a list of dataframes for each group, we make one here
    if (is.null(match_var)){
      seeds <- seeds %>%
        dplyr::distinct(!!as.name(seed_var), !!as.name(group_name)) %>% # drop duplicates ....
        split(.[[group_name]])                        # ... and split
    } else {
      seeds <- seeds %>%
        dplyr::distinct(!!as.name(seed_var), !!as.name(group_name), !!as.name(match_var)) %>% # drop duplicates ....
        split(.[[group_name]])                        # ... and split
    }
  }

  rwr_results <-
    seeds %>% purrr::imap(\(seed_group, name)
                          {
                            if (!is.null(match_var) & !is.null(network_name)) { # match network names and match var
                              seed_group <- seed_group %>%
                                dplyr::mutate({{match_var}} := as.character(!!as.name(match_var))) %>%
                                dplyr::filter(!!as.name(match_var) == network_name)}

                            seed_group <- seed_group %>%  dplyr::filter(feature %in% walk_network$multiplex$Pool_of_Nodes) # drop seeds not in the network

                            if (nrow(seed_group) > 0) { # make sure there are seeds available (esp. important utilizing a match var!)

                              group_results <- seed_group %>%
                                dplyr::distinct(!!as.name(seed_var)) %>% dplyr::pull() %>%
                                furrr::future_map(\(seed) # running parallelization over the seeds (instead of whole groups) might add stability
                                                  {# calling an external functions is more robust in terms of not copying unnecessary object to the workers
                                                    seed_walk_res <-
                                                      seed_walk(
                                                        seed = seed,
                                                        walk_network = walk_network,
                                                        normalize_score = normalize_score,
                                                        positive_scores_only = positive_scores_only
                                                      )
                                                    if (is.null(seed_walk_res)) { # NULL returns from seed_walk indicate errors
                                                      message(paste("No results returned for seed", seed, "in group", name, "\n"))
                                                    }
                                                    return(seed_walk_res)
                                }) %>%
                                purrr::compact() %>%  # remove NULLs introduced by tryCatch for erroneous walks
                                data.table::rbindlist(use.names = TRUE)

                              group_results <- group_results %>%
                                dplyr::mutate(seed_term = dplyr::case_when( # mark seed terms of the group/policy field
                                  NodeNames %in% seed_node ~ TRUE, .default = FALSE))

                              if(calculate_means) { # calculate means for non-normalized score
                                group_results <- group_results %>%
                                  dplyr::mutate(ScoreMean = mean(Score), .by = c(seed_term, NodeNames))
                              }

                              if (!is.null(normalize_score)) { # calculate means for normalized score
                                if (("seeds" %in% normalize_score) & calculate_means) { # calculate means of normalized seed scores in group and normalize again
                                  group_results <- group_results %>%
                                    dplyr::mutate(ScoreNormMean = mean(ScoreNorm), .by = c(seed_term, NodeNames))

                                  if (normalize_means) {
                                    group_results <- group_results %>%
                                      dplyr::mutate(ScoreNormMean = scales::rescale(ScoreNormMean, to = c(0, 1)))
                                  }
                                }

                                if ("group" %in% normalize_score) { # normalization on Group Level (within each group, e.g. policy field)
                                  group_results <- group_results %>%
                                    dplyr::mutate(ScoreNormGroup = scales::rescale(Score, to = c(0, 1)))

                                  if (calculate_means) { # calculation of means within group
                                    group_results <- group_results %>%
                                      dplyr::mutate(ScoreNormGroupMean = mean(ScoreNormGroup), .by = NodeNames)

                                    if (normalize_means) {
                                      group_results <- group_results %>%
                                        dplyr::mutate(ScoreNormGroupMean = scales::rescale(ScoreNormGroupMean, to = c(0, 1)))
                                    }
                                  }
                                }
                              }

                              if (report_quantiles) {
                                cat(paste0(group_name, ": ", name, "\n"))
                                cat(paste(filter_var, "Quantiles:\n"))
                                group_results %>% dplyr::pull(!!as.name(filter_var)) %>%
                                  stats::quantile() %>% print()
                                cat("\n")
                              }

                              if (reduce_to_means) {
                                group_results <- group_results %>%
                                  dplyr::distinct(dplyr::across(c(NodeNames, seed_term,
                                                                  dplyr::ends_with("Mean"))))
                              }

                              # overwrite seed term values with a fixed value if desired
                              if (!is.null(seedterm_value)) {
                                group_results <- group_results %>%
                                  dplyr::mutate(dplyr::across(c(dplyr::starts_with("Score")),
                                                              dplyr::case_when(
                                                                seed_term == T ~ seedterm_value,
                                                                .default = .
                                                              )))
                              }

                              # Score filtering
                              if (!is.null(walk_score) & !walk_score_quantile) {

                                if (keep_seed_terms) {
                                  group_results <- group_results %>%
                                    dplyr::filter(!!as.name(filter_var) >= walk_score |
                                                    seed_term == TRUE)
                                } else {
                                  group_results <- group_results %>%
                                    dplyr::filter(!!as.name(filter_var) >= walk_score)
                                }

                              }

                              if (!is.null(walk_score) & walk_score_quantile) {

                                quantile_value <- group_results %>%
                                  dplyr::pull(!!as.name(filter_var)) %>%
                                  stats::quantile(walk_score) %>%
                                  .[[1]]

                                cat(paste0("Quantile-based threshold (",
                                           walk_score, " quantile)",
                                           " for ",
                                           group_name, " ",
                                           name, " in ",
                                           filter_var, ": ",
                                           quantile_value, "\n"))

                                if (keep_seed_terms) {
                                  group_results <- group_results %>%
                                    dplyr::filter(!!as.name(filter_var) >= quantile_value |
                                                    seed_term == TRUE)
                                } else {
                                  group_results <- group_results %>%
                                    dplyr::filter(!!as.name(filter_var) >= quantile_value)
                                }

                              }



                              return(group_results)

                            } else return(NULL) # return NULL if no seeds are available
    },
    .progress = progress)

  if (flatten_results) { # flatten results into a single dataframe if desired
    rwr_results %>%
      purrr::compact() %>%  # remove NULLs introduced by missing seeds
      data.table::rbindlist(idcol = group_name, use.names = TRUE) %>%
      dplyr::relocate(dplyr::any_of(c("Score", "ScoreMean", # set a more convenient col order
                                      "ScoreNorm", "ScoreNormMean",
                                      "ScoreNormGroup", "ScoreNormGroupMean")),
                      .after = dplyr::last_col()) %>%
      return()
  } else {
    return(rwr_results)
  }

}
