#' Get Unclassified Documents
#'
#' @description A function to return or print a random sample of documents not classified by `classify_documents()`.
#'
#' @param classification_result The result of `classify_documents()`. Must contain
#'   unclassified documents (`return_unclassified_docs = TRUE` in `classify_document()`).
#' @param documents The full document data to be matched to the classification result for output. Usually the non-tokenized documents.
#' @param doc_id The name of the document identifier used for matching.
#'   Can be a `dplyr::join_by()` function where `classification_result` ID = `a` and `documents` ID = `b`.
#' @param n The number of documents to print or return. If larger or equal to the number of unclassified documents,
#'   all documents will be returned, rather than a random sample (default: 20).
#' @param mode The mode to operate in, either "print" to print results or
#'   "return" to return a data frame of results (default: "print").
#'
#' @return Depending on `mode`, either prints the results or
#'   returns a data frame containing the specified number of missing documents.
#'
#' @examples
#' # the classified_documents object in this example is created by classify_document() - see help(classify_documents)
#' # note that you would usually want to use non-tokenized documents as 'documents'. This data, however, does not come
#' #   with the package. Instead, we paste the tokens of each document together
#'
#' data("de_pol_twitter")
#'
#' get_unclassified_documents(classification_result = classified_documents,
#'                            documents = de_pol_twitter %>% dplyr::summarise(content = paste(lemma, collapse = ","), .by = doc_id),
#'                            doc_id = "doc_id",
#'                            n = 20,
#'                            mode = "print")
#'
#' @export
#'
#' @importFrom rlang arg_match
#' @importFrom dplyr slice_sample left_join
#' @importFrom tibble as_tibble
get_unclassified_documents <- function(
  classification_result,
  documents,
  doc_id,
  n = 20,
  mode = c("print", "return")
) {

  # some checks

  rlang::arg_match(mode)

  if (!("unclassified_documents" %in% names(classification_result))) {
    stop("Requires unclassified documents within the classifcation_result. Specify return_unclassified_docs = TRUE in classify_documents() to obtain them.")
  }

  if (is.character(doc_id) && !(doc_id %in% names(classification_result$unclassified_documents))) {
    stop(paste(doc_id, "not found in the classification_results provided."))
  }

  if (is.character(doc_id) && !(doc_id %in% names(documents))) {
    stop(paste(doc_id, "not found in the documents data provided."))
  }

  if (nrow(classification_result$unclassified_documents) > 0){ # check if there are unclassified docs

    if (mode == "print") {
      if (n < nrow(classification_result$unclassified_documents)){
        # return sample...
        cat(paste("\nRandom Sample of", n, "out of",
                  nrow(classification_result$unclassified_documents),
                  "unclassified Documents:\n"))
        classification_result$unclassified_documents %>%
          dplyr::slice_sample(n = n) %>%
          dplyr::left_join(documents, by = doc_id) %>%
          print()
      } else {
        # ... or return all unclassified documents if n_return >= nr of unclassified docs
        cat(paste("\nReturning all",
                  nrow(classification_result$unclassified_documents),
                  "unclassified Documents:\n"))
        classification_result$unclassified_documents %>%
          dplyr::left_join(documents, by = doc_id)  %>%
          print()
      }}

    if (mode == "return") {
      classification_result$unclassified_documents %>%
        dplyr::slice_sample(n = n) %>%
        dplyr::left_join(documents, by = doc_id) %>%
        return()
    }
  }
}
