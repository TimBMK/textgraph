#' Save textgraph topics
#'
#' A function to save the textgraph_topics object created by `calculate_topics()`
#'   or `calculate_dynamic_topics()`. Will save as `.tar.gz` file
#'
#' @param textgraph_topics The textgraph_topics object to save
#' @param file String; the `.tar.gz` file to save to
#' @param verbose Logical; sets Verbosity
#' @param include_document_entities Logical; Should the entities attached to the document data
#'                                   get saved? Saving them can be helpful for future analyses;
#'                                   excluding them can speed up the save/load process and save
#'                                   disk space
#'
#' @return writes the object under the specified `file` path. If no path is
#'          specified, writes the file to the current working directory
#' @export
#'
#' @examples
#' \dontrun{
#' save_textgraph_topics(my_textgraph_topics, "my_textgraph_topics.tar.gz")
#' }
#'
#' @importFrom dplyr "%>%"
#' @importFrom tidyr unnest
#' @importFrom vroom vroom_write
#' @importFrom tibble as_tibble
#' @importFrom utils tar
#' @importFrom data.table as.data.table
#'
save_textgraph_topics <- function(textgraph_topics,
                                  file,
                                  include_document_entities = TRUE,
                                  verbose = TRUE) {

  # Data checks
  if (class(textgraph_topics) != "textgraph_topics") {
    stop("Requires a textgraph_topics object.")
  }

  # temp <- tempdir() # temp folder to build the object

  # path <- file.path(temp, "textgraph")

  path <- ".textgraph_topics" # temporary directory (using an actual tempdir() makes the file paths within the tar object very long and hard to recreate)

  if (dir.exists(path)) unlink(path, recursive = TRUE) # remove temp directory if it already exists

  dir.create(path)

  if (verbose) cat("Save Metrics...\n")
  vroom::vroom_write(x = textgraph_topics$metrics[!names(textgraph_topics$metrics) # drop plot if it's there
                                                  %in% "snapshot_plot"] %>%
                       tibble::as_tibble(), # from list to tibble before saving
                     progress = verbose,
                     file = file.path(path, "metrics.tar.gz"))

  if ("snapshot_plot" %in% names(textgraph_topics$metrics)) {
    vroom::vroom_write(x = textgraph_topics$metrics$snapshot_plot$data, # save plot data
                       progress = verbose,
                       file = file.path(path, "plot_data.tar.gz"))
  }

  if (verbose) cat("Save Topics...\n")
  vroom::vroom_write(x = textgraph_topics$topics,
                     progress = verbose,
                     file = file.path(path, "topics.tar.gz"))

  if (verbose) cat("Save Entities...\n")
  vroom::vroom_write(x = textgraph_topics$entities,
                     progress = verbose,
                     file = file.path(path, "entities.tar.gz"))

  if ("documents" %in% names(textgraph_topics)) {

    if (verbose) cat("Save Documents...\n")

    dir.create(file.path(path, "documents"))

    textgraph_topics$documents %>%
      data.table::as.data.table() %>%
      split(by = "topic") %>%  # split into topics and write topics seperately (more stable for large data)
      purrr::iwalk(\(data, topic)
                   { if (include_document_entities) {
                     res <- data %>%
                       tidyr::unnest(cols = entities)
                   } else {
                     res <- data %>% dplyr::select(!entities)
                   }
                     vroom::vroom_write(x = res,
                                        file = file.path(path,
                                                         "documents",
                                                         paste0(
                                                           topic, ".tar.gz"
                                                         )),
                                        progress = FALSE)

      }, .progress = verbose)

  }

  utils::tar(tarfile = file,
             files = list.files(path, full.names = TRUE),
             compression = "gzip")

  unlink(path, recursive = TRUE) # delete contents of temp folder

}
