#' @title get_seed_terms
#'
#' @description
#' This function identifies key terms for each group specified by a given variable from a dataframe containing tokenized text.
#'
#'
#' @param data A dataframe
#' @param doc_id String; Name of the variable containing document IDs. Optional, but can speed up computation for large data sets. Skip with `NULL`
#' @param tokens String; Name of the variable containing tokens (or lemmas) as a string
#' @param grouping_var String; name of the variable to accumulate and compare the tokens by. Note that rows with missing (`NA`) values in in the `grouping_var` are ignored.
#' @param measure String; the keyness measure. Can be one of "chi2", "exact" (Fisher's exact test), "lr" (likelihood ratio), or "pmi" (pointwise mutual information). See `quanteda.textstats::textstat_keyness()` for details
#' @param threshold Numerical; the keyness threshold for filtering results. Terms below the threshold are discarded. If `NULL`, no threshold is applied. Default is `NULL`.
#' @param max_results Numerical; the maximum number of terms to return per group, sorted in descending order according to the keyness measure. Threshold still applies. If `NULL`, all results are returned. Default is `NULL`.
#' @param max_result_ties Logical; indicates whether to drop tied values when `max_results` is applied. If `TRUE`, this can can result in more rows being returned than specified in `max_results`. Default is `FALSE`, selecting on of the ties values at random.
#' @param min_results Numerical; the minimum number of terms to return per group, overriding the `threshold` if necessary. Default is `NULL`.
#' @param show_plots Logical; indicates whether to display the keyness results plots for each group. Note: Plot represent results before `threshold`, `max_results`, or `min_results` are applied. Default is `FALSE`.
#' @param save_plots Logical; indicates whether to save the keyness result plots and return them with the keyterm data. Default is `FALSE`. If `TRUE`, the function returns a list where the first element contains the key terms, and the second element contains the plots
#'
#' @return If `save_plots` is `FALSE`, a data frame object containing the result of the keyness measure, inlcuding the `grouping_var`. If `save_plots` is `TRUE`, a list with two elements.
#' The first element is a data frame object containing the results of the keyness measure, inlcuding the `grouping_var`. The second object is a list with a keyness result plot for each element of the `grouping_var`.
#'
#' @details
#' This function is intended to produce seed terms for additional analysis by utilizing one of the keyness measures provided in the quanteda.textstats package.
#' Essentially, it compares tokenized documents between the values of a given grouping variable. For example, it can be used to extract the most distinctive features between multiple authors or groups of social media accounts.
#' Additionally, it conveniently outputs and saves plots of the keyness if requested. As keyness measures can be somewhat context-dependant, additional options to filter the results are provided, by setting a `threshold` or the number of `min_results` or `max_results`.
#'
#' @examples
#' # In this example, we extract key terms for each official ministry account from a tokenized sample of German politicians' tweets sent over one week.
#' # We save and display the plots to inspect the results. Note that tokens with the value `NA` in the grouping_var "ministry_name" are excluded from the analysis!
#'
#' data(de_pol_twitter)
#'
#' seed_terms <- get_seed_terms(de_pol_twitter,
#'                              doc_id = "doc_id",
#'                              tokens = "lemma",
#'                              grouping_var = "ministry_name",
#'                              measure = "chi2",
#'                              threshold = NULL,
#'                              max_results = NULL,
#'                              max_result_ties = FALSE,
#'                              min_results = NULL,
#'                              show_plots = TRUE,
#'                              save_plots = TRUE)
#'
#' @export
#'
#' @importFrom dplyr "%>%"
#' @importFrom dplyr summarise mutate filter slice_max as_tibble distinct pull
#' @importFrom purrr map list_rbind compact
#' @importFrom quanteda corpus tokens dfm tokens_group docvars
#' @importFrom quanteda.textstats textstat_keyness
#' @importFrom quanteda.textplots textplot_keyness
#' @importFrom rlang arg_match

get_seed_terms <- function(data,
                           doc_id = NULL,
                           tokens,
                           grouping_var,
                           measure = c("chi2", "exact", "lr","pmi"),
                           threshold = NULL,
                           max_results = NULL,
                           max_result_ties = FALSE,
                           min_results = NULL,
                           show_plots = FALSE,
                           save_plots = FALSE)
{
  require(dplyr)
  require(purrr)
  require(quanteda)
  require(quanteda.textstats)
  require(quanteda.textplots)


  rlang::arg_match(measure)

  # prepare corpus

  if(!is.null(doc_id)) {
    corpus <- data %>%
      dplyr::summarise(text = paste(!!as.name(tokens), collapse = " "), .by = c(!!as.name(doc_id), !!as.name(grouping_var))) %>% # not strictly necessary, but speeds up computation
      dplyr::mutate(temp_id = 1:nrow(.)) %>% # generate a temporary ID to allow for duplicates (i.e. same tweet in multiple fields)
      quanteda::corpus(text_field = "text", docid_field = "temp_id")
  } else { # if no doc_id is given, each row becomes a document
    corpus <- data %>%
      dplyr::mutate(temp_id = 1:nrow(.)) %>% # generate a temporary ID to allow for duplicates (i.e. same tweet in multiple fields)
      quanteda::corpus(text_field = "text", docid_field = "temp_id")
  }

  # prepare DFM (incl. grouping and stopword removal)
  dfm <- corpus %>%
    quanteda::tokens(remove_punct = TRUE, remove_symbols = TRUE,
                     remove_numbers = TRUE,  remove_url = TRUE) %>%
    # quanteda::tokens_remove(remove_tokens) %>%  # done during preprocessing / filter_tokens() now
    quanteda::tokens_group(groups = quanteda::docvars(., grouping_var)) %>% # by ministry
    quanteda::dfm()

  # Calculate Keyness
  dfm_groups <- dfm %>% quanteda::docvars() %>% dplyr::distinct() %>% dplyr::pull()

  if (save_plots == T) {
    plots <- vector(mode = "list", length = length(dfm_groups)) %>%  # container
      setNames(dfm_groups) # with names
  }

  keyness <- dfm_groups %>%
    purrr:::map(~ tryCatch({

      group <- .

      textstat <- quanteda.textstats::textstat_keyness(dfm,
                                                       measure = measure,
                                                       target = group)

      if (show_plots == TRUE) {
        print(quanteda.textplots::textplot_keyness(textstat))
        readline(prompt="Press [enter] to show next plot")
      }

      if (save_plots == TRUE) {
        plots[[group]] <<- tryCatch(quanteda.textplots::textplot_keyness(textstat), # assign to plots object outside of function
                                    error = function(e) NULL) # failsafe
      }

      textstat_res <- textstat %>%
        dplyr::as_tibble() %>%
        dplyr::mutate({{grouping_var}} := group)


      # Filter results

      if (!is.null(max_results)) {
        textstat_res <- textstat_res %>%
          dplyr::slice_max(!!as.name(measure),
                           n = max_results,
                           with_ties = max_result_ties)
      }

      if (!is.null(threshold)) {

        # if min_results are specified, overwrite threshold as needed
        if (!is.null(min_results)) { # check potential nr of results
          number_results <- textstat_res %>%
            dplyr::filter(!!as.name(measure) >= threshold) %>% nrow()
        }

        if (!is.null(min_results) && number_results < min_results) {
          textstat_res <- textstat_res %>%
            dplyr::slice_max(!!as.name(measure),
                             n = min_results,
                             with_ties = TRUE)

        } else { # if no min results are specified or there are sufficient results, simply filter for keyness
          textstat_res <- textstat_res %>% dplyr::filter(!!as.name(measure) >= threshold)
        }

      }

      return(textstat_res)

    }), error = function(e) NULL) %>%
    purrr::compact() %>%
    purrr::list_rbind()



  # handle plot saving

  if (save_plots == TRUE) {
    result <- list(keyness,
                   plots)
    names(result) <- c("key_terms", "plots")
  } else {
    result <- keyness
  }

  return(result)
}
