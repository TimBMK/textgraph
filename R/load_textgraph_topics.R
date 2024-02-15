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
#' @importFrom ggplot2 ggplot aes geom_point labs
#'
load_textgraph_topics <- function(file, verbose = TRUE) {

  temp <- tempdir() # temp folder to build the object

  utils::untar(tarfile = file, exdir = temp)

  path <- file.path(temp, ".textgraph_topics")

  textgraph_topics <- list()

  if (verbose) cat("Load Metrics...\n")
  textgraph_topics$metrics <- vroom::vroom(file.path(path, "metrics.tar.gz"),
                                           progress = verbose,
                                           show_col_types = FALSE) %>%
    as.list()

  if (file.exists(file.path(path, "plot_data.tar.gz"))){ # check if there is plot data
    plot_data <- vroom::vroom(file.path(path, "plot_data.tar.gz"),
                              progress = verbose,
                              show_col_types = FALSE)

    plot <- plot_data %>% # recreate plot skeleton
      ggplot2::ggplot(ggplot2::aes(x = nr_topics, color = snapshot)) +
      theme_bw()

    if (is.numeric(plot_data$quality)) { # draw metrics-specific plot
      textgraph_topics$metrics$snapshot_plot <- plot +
        ggplot2::geom_point(ggplot2::aes(y = quality)) +
        ggplot2::labs(title = "Quality and Entities of Topics in Snapshots")
    }

    if (is.numeric(plot_data$modularity)) { # draw metrics-specific plot
      textgraph_topics$metrics$snapshot_plot <- plot +
        ggplot2::geom_point(ggplot2::aes(y = modularity)) +
        ggplot2::labs(title = "Modularity and Entities of Topics in Snapshots")
    }

  }

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

    textgraph_topics$documents <- vroom::vroom(list.files(file.path(path, "documents"), full.names = T), # read docs for each topic
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
