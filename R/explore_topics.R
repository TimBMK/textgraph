#' Explore textgraph topics
#'
#' This function allows to explore a textgraph_topics object through a markdown-rendered
#'  html document.
#'
#'  @param textgraph_topics A textgraph_topics object, as provided by the `calculate_topics()` function.
#'  @param topic_threshold Numerical; optional threshold how large a topic needs to be before getting rendered in percent.
#'                          If the `textgraph_object` contains document data, the minimum document occurrences
#'                          of a topic as the percentage of all documents. If it does not contain document data,
#'                          the number of entities in a topic as percentage of all entities in the network. Set to 0 to skip.
#'  @param n_top_terms Numerical; number of terms to print out per topic (descending by Page Rank)
#'  @param n_top_docs Numerical; number of documents to print out per topic (descending by Document Relevance). If a
#'                     `split_var` is specified, this number of documents is returned for each of its values.
#'  @param time_var Optional string; name of a variable in the document section of the `textgraph_object`
#'                    containing time-related metadata of documents (e.g. publication date). If provided,
#'                    allows to plot topic occurrence over time.
#'  @param floor_time_var_by Optional string; allows to floor the `time_var` before plotting, e.g. from a
#'                             YMD-HMS timestamp to the day (YMD), effectively adjusting the x-axis bins of the plot.
#'                             See `?lubridate::floor_date()` for options. If provided, `time_var` must be a
#'                             valid POSIXct format.
#'  @param split_var Optional string; name of a variable in the document section of the `textgraph_object`
#'                    containing metadata of documents to split topic occurrences by (e.g. outlet or party).
#'                    If provided, allows to plot topic occurrence between its values. If `time_var` is also
#'                    provided, plots topic occurrences over time, split by the `split_var` (e.g. topic occurrence
#'                    in outlets over time).
#'  @param text_var Optional string; name of a variable in the document section of the `textgraph_object`
#'                    containing the text of the document. If provided, will print out the text of the `n_top_docs`
#'                    of each topic.
#'  @param document_ids Optional string; name of a variable in the document section of the `textgraph_object`
#'                    containing the ID of the document. If provided, will print out the ID of the `n_top_docs`
#'                    of each topic.
#'  @param output_file Name of the html file to be written. If `NULL`, the result will be displayed in the default
#'                      browser, but not saved.
#'  @param output_dir Name of the directory to safe the `output_file` to. Defaults to the current working directory.
#'  @param ... Additional arguments passed to `rmarkdown::render()`
#'
#'  @return A html file
#'
#' @examples
#' \dontrun{
#' data("de_pol_twitter")
#'
#' topics <- calculate_topics(text_network,
#'                           documents = de_pol_twitter %>% #' when passing document data...
#'                               dplyr::select(doc_id, lemma, #' ...we reduce the columns to the required variables...
#'                                             created_at, author_id, party, tweet_url), #'...and document (not token!) metadata
#'                           document_tokens = "lemma",
#'                           document_ids = "doc_id",
#'                           negative_edge_weights = TRUE,
#'                           page_rank_calculation = "global",
#'                           cluster_function = cluster_leiden,
#'                           objective_function = "CPM",
#'                           keep_cluster_object = FALSE,
#'                           verbose = TRUE)
#'
#' explore_topics(textgraph_topics = topics,
#'                topic_threshold = 0.01,
#'                n_top_terms = 10,
#'                n_top_docs = 5,
#'                time_var = "created_at", # we plot over time...
#'                floor_time_var_by = "days", # ...by days
#'                split_var = "party", # we want information for each party
#'                text_var = "tweet_url", # instead of the text, we display the tweet URL
#'                document_ids = "doc_id",
#'                output_file = NULL)
#' }
#'
#' @importFrom dplyr "%>%"
#' @importFrom dplyr summarise mutate filter slice_max pull arrange desc
#' @importFrom ggplot2 ggplot aes geom_line theme_bw labs
#' @importFrom lubridate floor_date
#' @importFrom scales percent
#' @importFrom tidyr replace_na
#'
#' @export


explore_topics <- function(
    textgraph_topics,
    topic_threshold = 0.01,
    n_top_terms = 10,
    n_top_docs = 5,
    time_var = NULL,
    floor_time_var_by = NULL,
    split_var = NULL,
    text_var = NULL,
    document_ids = NULL,
    output_file = NULL,
    output_dir = getwd(),
    ...
){

  if (is.null(output_file)) {
    no_output <- TRUE
    output_file <- tempfile()
    output_dir <- NULL
  } else {
    output_dir <- getwd()
  }

  rmarkdown::render(
    input = file.path(path.package("textgraph"), "R", "explore_topics.Rmd"),
    params = list(textgraph_topics = textgraph_topics,
                  topic_threshold = topic_threshold,
                  n_top_terms = n_top_terms,
                  n_top_docs = n_top_docs,
                  time_var = time_var,
                  floor_time_var_by = floor_time_var_by,
                  split_var = split_var,
                  text_var = text_var,
                  document_ids = document_ids),
    output_format = "html_document",
    output_file = output_file,
    output_dir = output_dir,
    envir = new.env(),
    ...
  )

  if (no_output) {
    browseURL(paste0(output_file, ".html"))
    Sys.sleep(5) # let some time pass to make sure the file has been opened
    unlink(paste0(output_file, ".html"))
  }
}

