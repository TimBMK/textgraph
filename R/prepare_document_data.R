#' Prepare Document Data during Topic Calculations
#'
#' Internal function to process documents during topic calculations
#'
#' @param topics Topics object as provided within the function workflow
#' @param documents document data
#' @param document_tokens tokens in document data
#' @param document_ids IDs in document data
#' @param document_relevance String; How the `document_relevance` should be calculated.
#'   "pagerank_tfidf" calculates the re-scaled sum of all topic-relevant entities'
#'   Page Rank in the document multiplied by their tf-idf
#'   (Term Frequency - Inverse Document Frequency). "network" calculates the
#'   page rank of each document in a topic-specific document-document network
#'   projected from the co-occurrence of topic-relevant entities in the document.
#'   Depending on the settings for `pmi_weight`, the
#'   latter will use a PMI-weighted with potentially negative weights.
#' @param pmi_weight Logical indicating whether weights should be calculated based on the PMI (Pointwise Mutual Information) of `document` and `feature`. If `FALSE`, a simple cooccurrence weighting is performed.
#' @param keep_negative_weights Logical indicating whether to keep edges with negative PMI weights. Only applies if `pmi_weight = TRUE`.
#'
#' @return prepared document data
#'
#' @importFrom dplyr "%>%"
#' @importFrom dplyr summarise n mutate case_when select distinct select_if across
#' @importFrom tidytext bind_tf_idf
#' @importFrom scales rescale
#' @importFrom tidyselect everything
#' @importFrom data.table as.data.table setkeyv merge.data.table setorder
#'
#' @keywords internal


prepare_document_data <- function(topics,
                                  documents,
                                  document_tokens,
                                  document_ids,
                                  document_relevance = c("pagerank_tfidf",
                                                         "network"),
                                  pmi_weight = TRUE) {


  join_dat <- documents %>% # prepare data for data.table join
    dplyr::select(!!as.name(document_tokens),
                  !!as.name(document_ids)) %>%
    dplyr::rename(entity = !!as.name(document_tokens)) %>%
    data.table::as.data.table()

  topics <- data.table::as.data.table(topics)

  data.table::setkeyv(join_dat, "entity")
  data.table::setkeyv(topics, "entity")

  dat <- data.table::merge.data.table(topics, join_dat, by = "entity",
                                      allow.cartesian = T) # allows to join large data with entities belonging to multiple topics and occurring in multiple docs

  invisible(gc()) # run gc() after a cartesian join


  if (document_relevance == "pagerank_tfidf") {

    documents <- data.table::as.data.table(documents)

    tf_idf <- documents[ , .N, by = c(document_tokens, document_ids)] %>%  # count duplicated entities
      tidytext::bind_tf_idf(!!as.name(document_tokens), !!as.name(document_ids), N) %>%  # calculate tf-idf
      dplyr::rename(entity = !!as.name(document_tokens))

    data.table::setkeyv(dat, c("entity", document_ids))
    data.table::setkeyv(tf_idf, c("entity", document_ids))

    dat <- data.table::merge.data.table(dat, tf_idf, by = c("entity", document_ids))

    dat <- dat %>% # calculate term relevance
      dplyr::mutate(page_rank = dplyr::case_when(is.na(page_rank) ~ 0, # set missing page ranks to 0 (missing if a term has no connection to other terms in page_rank = "cluster")
                                                 .default = page_rank)) %>%
      dplyr::mutate(term_relevance = (tf_idf * page_rank) %>% # calculate topic term relevance as normalized (tf_idf * page_rank)
                      scales::rescale(to = c(0, 1)), # rescale
                    .by = topic)

    document_data <- dat[ , .(entities = list(entity),# summarise entities and document relevance. list(entity) is surprisingly costly (but faster than paste())
                              document_relevance = sum(term_relevance)),
                          by = c("topic", document_ids)]

    document_data[ , document_relevance := scales::rescale(document_relevance, # rescale per topic
                                                           to = c(0,100)),
                   by = "topic"]

    data.table::setorder(document_data, topic, -document_relevance) # order

    invisible(gc()) # run gc() to clear memory
  }

  if (document_relevance == "network") {

    document_data <- dat %>%
      split(by = "topic") %>%
      furrr::future_imap(\(data, topic)
                         { network <- calculate_network(data,
                                                        document = "entity", # doc and feature are reversed to get doc-doc-networks
                                                        feature = document_ids,
                                                        pmi_weight = pmi_weight,
                                                        as_data_frame = FALSE,
                                                        keep_negative_weights = TRUE
                                                        )

                           if ((igraph::E(network) %>% length()) > 0) {
                             page_rank <- igraph::page_rank(network)$vector %>%
                               scales::rescale(to = c(0,100)) # rescale
                             names <- names(page_rank)
                           } else {
                             page_rank <- NA
                             names <- data %>%
                               dplyr::distinct(!!as.name(document_ids)) %>%
                               dplyr::pull()
                           }

                           document_relevance <- data.table::data.table(document_ids = names,
                                                                        document_relevance = page_rank)
                           data.table::setnames(document_relevance, "document_ids", document_ids) # rename column to original name

                           document_data <- data[ , .(entities = list(entity)), # summarise entities. list(entity) is surprisingly costly (but faster than paste())
                                                 by = c("topic", document_ids)]

                           data.table::setkeyv(document_data, document_ids)
                           data.table::setkeyv(document_relevance, document_ids)

                           document_data <- data.table::merge.data.table(document_data, # merge data
                                                                         document_relevance,
                                                                         by =  document_ids)

                           data.table::setorder(document_data, topic, -document_relevance) # order

                           return(document_data)

      }) %>% data.table::rbindlist()

    warning(paste("Unable to calculate networked document relevance for",
                  document_data %>%
                    dplyr::filter(is.na(document_relevance)) %>%
                    dplyr::distinct(topic) %>% nrow(),
                  "out of",
                  document_data %>%
                    dplyr::distinct(topic) %>% nrow(),
                  "topics. Likely reason: too few entities or documents in these topics."))

  }


  join_dat <- documents %>% # prepare 2nd join for additional metadata
    dplyr::select(!(!!as.name(document_tokens))) %>%
    dplyr::distinct(!!as.name(document_ids),
                    .keep_all = TRUE) %>%
    data.table::as.data.table()

  data.table::setkeyv(join_dat, document_ids)
  data.table::setkeyv(document_data, document_ids)

  document_data <- data.table::merge.data.table(document_data,
                                                join_dat,
                                                by = document_ids)

  missing_data <- document_data %>% # collect missing meta data
    dplyr::select(!c(topic, entities, document_relevance)) %>%
    dplyr::select_if(~ any(is.na(.))) %>%
    dplyr::summarise(dplyr::across(tidyselect::everything(),
                                   ~ (sum(is.na(.)) / n()))) %>%
    dplyr::select_if(~ (. > 0))

  if (nrow(missing_data) > 0 & ncol(missing_data) > 0) {
    warning("Missing data in variables ",
            paste(names(missing_data), collapse = ", "),
            ".\n",
            paste(names(missing_data %>%
                          dplyr::select_if(~ (. > 0.1))),
                  collapse = ", "),
            " miss more than 10% of data.\n",
            "Did you provide the correct 'document' data?")
  }

  return(document_data)
}
