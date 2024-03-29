% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/top_group_documents.R
\name{top_group_documents}
\alias{top_group_documents}
\title{Print or Return Top Documents per Group}
\usage{
top_group_documents(
  classification_result,
  documents,
  doc_id,
  group_name,
  classification_score,
  n = 20,
  with_ties = TRUE,
  mode = c("print", "return")
)
}
\arguments{
\item{classification_result}{The result of `classify_documents()`. Must contain
walk terms (`return_walk_terms = TRUE` in `classify_document()`).}

\item{documents}{The full document data to be matched to the classification result for output. Usually the non-tokenized documents.}

\item{doc_id}{The name of the document identifier used for matching.
Can be a `dplyr::join_by()` function where `classification_result` ID = `a` and `documents` ID = `b`.}

\item{group_name}{String; The name of the group variable.}

\item{classification_score}{String; The classification score used for selecting
the top documents Must be present in the data. Returned by `classify_documents()`. Note that this
differs from the `classification_measure` used in `classify_documents()` and `top_group_terms()`}

\item{n}{The maximum number of documents to print or return per group. (default: 20)}

\item{with_ties}{Logical indicating tie handling. If TRUE, will include
tied values, potentially leading to more results returned than specified with `n`.
If FALSE, will select tied values at random (default: TRUE).}

\item{mode}{The mode to operate in, either "print" to print results or
"return" to return a data frame of results (default: "print").}
}
\value{
Depending on `mode`, either prints the results or
  returns a data frame containing the specified number of top documents for each group.
}
\description{
This function displays or returns the top documents for each group based on
 the results of document classification.
}
\examples{
\dontrun{
# the classified_documents object in this example is created by classify_document() - see help(classify_documents)
# note that you would usually want to use non-tokenized documents as 'documents'. This data, however, does not come
#   with the package. Instead, we paste the tokens of each document together

data("de_pol_twitter")

top_group_documents(classification_result = classified_documents,
                    documents = de_pol_twitter \%>\% dplyr::summarise(content = paste(lemma, collapse = ","), .by = doc_id),
                    doc_id = "doc_id",
                    group_name = "ministry_name",
                    classification_score = "score_norm",
                    n = 20,
                    with_ties = FALSE,
                    mode = "print")
}

}
