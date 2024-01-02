#' Explore textgraph topics
#' 
#' This function allows to explore a textgraph_topics object through a markdown-rendered
#'  html document.
#'  
#'  @param textgraph_object A textgraph_topics object, as provided by the `calculate_topics()` function.
#'  @param topic_threshold Numerical; optional threshold how large a topic needs to be before getting rendered in percent.
#'                          If the `textgraph_object` contains document data, the minimum document occurrences 
#'                          of a topic as the percentage of all documents. If it does not contain document data,
#'                          the number of entities in a topic as percentage of all entities in the network. Set to 0 to skip.
#'  @param n_top_terms Numerical; number of terms to print out per topic (descending by Page Rank)
#'  @param n_top_docs Numerical; number of documents to print out per topic (descending by Document Relevance)
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
#'  @param output_file Name (and optionally path) of the html file to be written.
#'  @param ... Additional arguments passed to `rmarkdown::render()`
#'  
#'  @return Saves the output as html under the specified `output_file`.
#'  
#' @examples
#' \dontrun{
#' data("de_pol_twitter")
#' topics <- calculate_topics(text_network,
#'                           documents = de_pol_twitter %>% #' when passing document data...
#'                               dplyr::select(doc_id, lemma, #' ...we reduce the columns to the required variables...
#'                                             created_at, author_id, ministry_name), #'...and document (not token!) metadata
#'                           document_tokens = "lemma",
#'                           document_ids = "doc_id",
#'                           negative_edge_weights = TRUE,
#'                           page_rank_calculation = "global",
#'                           cluster_function = cluster_leiden,
#'                           objective_function = "CPM",
#'                           keep_cluster_object = FALSE,
#'                           verbose = TRUE)
#' explore_topics(topics,
#'                topic_threshold = 0.01,
#'                n_top_terms = 10,
#'                n_top_docs = 5,
#'                time_var = "created_at",
#'                floor_time_var_by = "days",
#'                split_var = "ministry_name",
#'                text_var = NULL,
#'                document_ids = "doc_id",
#'                output_file = "topic_exploration.html")
#' }
#' 
#' @importFrom dplyr "%>%"
#' @importFrom dplyr summarise mutate filter slice_max pull
#' @importFrom ggplot2 ggplot aes geom_line theme_bw labs
#' @importFrom lubridate floor_date
#' @importFrom scales percent
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
    output_file,
    ...
){
  xfun::Rscript_call(
    rmarkdown::render,
    list(input = file.path(path.package("textgraph"), "R", "explore_topics.Rmd"),
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
         ...)
  )
}

