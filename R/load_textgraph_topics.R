#' Load textgraph topics
#'
#' A function to load the textgraph_topics object created by `calculate_topics()`
#'   or `calculate_dynamic_topics()` and saved via `save_textgraph_topics()`
#'
#' @param file The `.tar.gz` file to load
#'
#' @return Returns the `textgraph_topics` object
#' @export
#'
#' @examples
#' \dontrun{
#' my_textgraph_topics <- load_textgraph_topics("my_textgraph_topics.tar.gz")
#' }
#'
#' @importFrom dplyr "%>%"
#' @importFrom vroom vroom
#' @importFrom utils untar
#' @importFrom data.table as.data.table
#'
load_textgraph_topics <- function(file, verbose) {

  temp <- tempdir() # temp folder to build the object

  utils::untar(tarfile = file, exdir = temp)

  path <- file.path(temp, "textgraph_topics")

  textgraph_topics <- list()

  if (verbose) cat("Load Metrics...\n")
  textgraph_topics$metrics <- vroom::vroom(file.path(path, "metrics.tar.gz"),
                                           progress = verbose,
                                           show_col_types = FALSE) %>%
    as.list()

  if (verbose) cat("Load Topics...\n")
  textgraph_topics$topics <- vroom::vroom(file.path(path, "topics.tar.gz"),
                                          progress = verbose,
                                          show_col_types = FALSE)

  if (verbose) cat("Load Entities...\n")
  textgraph_topics$entities <- vroom::vroom(file.path(path, "entities.tar.gz"),
                                            progress = verbose,
                                          show_col_types = FALSE)

  if (file.exists(file.path(path, "documents.tar.gz"))){ # check if there are documents

    if (verbose) cat("Load Documents...\n")

    textgraph_topics$documents <- vroom::vroom(file.path(path, "documents.tar.gz"),
                                               progress = verbose,
                                              col_types =  "c") %>% # assuming the first col is the ID - which should always be the case
      data.table::as.data.table()

    document_ids <- colnames(textgraph_topics$documents)[1] # get name of ID col

    textgraph_topics$documents <- textgraph_topics$documents[, .(entities = list(entities)), # make entities into list again list(entity) is surprisingly costly (but faster than paste())),
                          by = c("topic", document_ids)]
  }

  unlink(path, recursive = TRUE)

  class(textgraph_topics) <- "textgraph_topics"

  return(textgraph_topics)
}
