#' Print or Return Top Terms per Group
#'
#' @description This function displays or returns the top terms for each group based on
#'  the results of document classification.
#'
#' @param classification_result The result of `classify_documents()`. Must contain
#'   walk terms (`return_walk_terms = TRUE` in `classify_document()`).
#' @param group_name String; The name of the group variable.
#' @param classification_measure String; The classification measure used for selecting
#'   the top terms. Must be present in the data. Provided by `get_rwr_terms()` and selected as
#'   `classification_measure` in `classify_documents()`.
#' @param include_seed_terms Logical indicating whether to include seed terms in the output
#'   (default: TRUE).
#' @param n The maximum number of terms to return per group (default: 20).
#' @param with_ties Logical indicating tie handling. If TRUE, will include
#'   tied values, potentially leading to more results returned than specified with `n`.
#'   If FALSE, will select tied values at random (default: TRUE).
#' @param mode The mode to operate in, either "print" to print results or
#'   "return" to return a data frame of results (default: "print").
#'
#' @return Depending on `mode`, either prints the results or
#'   returns a data frame containing the specified number of top terms for each group.
#'
#' @examples
#' \dontrun{
#' # the classified_documents object in this example is created by classify_document() - see help(classify_documents)
#'
#' top_group_terms(classification_result = classified_documents,
#'                 group_name = "ministry_name",
#'                 classification_measure =  "ScoreNormMean",
#'                 include_seed_terms = TRUE,
#'                 n = 20,
#'                 with_ties = TRUE,
#'                 mode = "print")
#'}
#'
#' @export
#' @importFrom rlang arg_match
#' @importFrom dplyr filter select slice_max
#' @importFrom dplyr "%>%"
#' @importFrom data.table as.data.table
#' @importFrom purrr iwalk
top_group_terms <- function(
  classification_result,
  group_name,
  classification_measure,
  include_seed_terms = TRUE,
  n = 20,
  with_ties = TRUE,
  mode = c("print", "return")
){

  # some checks
  rlang::arg_match(mode)

  if (!("walk_terms" %in% names(classification_result))) {
    stop("Requires walk terms within the classifcation_result. Specify return_walk_terms = TRUE in classify_documents() to obtain them.")
  }

  if (!(group_name %in% names(classification_result$walk_terms))) {
    stop("Group name not found in the walk_terms data provided.")
  }

  if (!(classification_measure %in% names(classification_result$walk_terms))) {
    stop("Classification Measure not found in the walk_terms data provided.")
  }

  if (include_seed_terms && !("seed_term" %in% names(classification_result$walk_terms))) {
    warning("No seed_term indicator found the data provided.")
    include_seed_terms <- TRUE # if no seed term indicator is found, the filtering step will be skipped without throwing an error
  }

  # Filter out Seed Terms if desired
  if (include_seed_terms == FALSE) {
    walk_terms <- classification_result %>% .[["walk_terms"]] %>%
      dplyr::filter(seed_term == FALSE)
  } else {
    walk_terms <- classification_result %>% .[["walk_terms"]]
  }

  # Printout
  if(mode == "print") {
    cat(paste("\nTop", n, "Terms per", group_name, "\n"))

    walk_terms %>%
      data.table::as.data.table() %>% split(by = group_name) %>%
      purrr::iwalk(\(group, name)
                   {cat(paste0("\n", group_name, " ", name, ":\n"))
                     group %>% dplyr::slice_max(order_by = !!as.name(classification_measure),
                                                n = n, with_ties = with_ties) %>%
                       dplyr::select(!dplyr::any_of(group_name)) %>% print()})}

  # Return data
  if (mode == "return") {
    walk_terms %>%
      dplyr::slice_max(order_by = !!as.name(classification_measure),
                       n = n, with_ties = with_ties,
                       by = !!as.name(group_name)) %>%
      return()
  }
}
