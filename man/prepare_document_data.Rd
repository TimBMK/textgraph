% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare_document_data.R
\name{prepare_document_data}
\alias{prepare_document_data}
\title{Prepare Document Data during Topic Calculations}
\usage{
prepare_document_data(
  topics,
  documents,
  document_tokens,
  document_ids,
  document_relevance = c("pagerank_tfidf", "network"),
  pmi_weight = TRUE
)
}
\arguments{
\item{topics}{Topics object as provided within the function workflow}

\item{documents}{document data}

\item{document_tokens}{tokens in document data}

\item{document_ids}{IDs in document data}

\item{document_relevance}{String; How the `document_relevance` should be calculated.
"pagerank_tfidf" calculates the re-scaled sum of all topic-relevant entities'
Page Rank in the document multiplied by their tf-idf
(Term Frequency - Inverse Document Frequency). "network" calculates the
page rank of each document in a topic-specific document-document network
projected from the co-occurrence of topic-relevant entities in the document.
Depending on the settings for `pmi_weight`, the
latter will use a PMI-weighted with potentially negative weights.}

\item{pmi_weight}{Logical indicating whether weights should be calculated based on the PMI (Pointwise Mutual Information) of `document` and `feature`. If `FALSE`, a simple cooccurrence weighting is performed.}

\item{keep_negative_weights}{Logical indicating whether to keep edges with negative PMI weights. Only applies if `pmi_weight = TRUE`.}
}
\value{
prepared document data
}
\description{
Internal function to process documents during topic calculations
}
\keyword{internal}
