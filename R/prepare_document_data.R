#' Prepare Document Data during Topic Calculations
#'
#' Internal function to process documents during topic calculations
#'
#' @param topics Topics object as provided within the function workflow
#' @param documents document data
#' @param document_tokens tokens in document data
#' @param document_ids IDs in document data
#'
#' @return prepared document data
#' @keywords internal


prepare_document_data <- function(topics,
                                  documents,
                                  document_tokens,
                                  document_ids) {
  tf_idf <- topics %>%
    dplyr::left_join(documents,
                     by = dplyr::join_by(entity == !!as.name(document_tokens))) %>%
    dplyr::summarise(n = dplyr::n(), # calculate tf_idf
                     .by = c(entity, !!as.name(document_ids),
                             topic, page_rank)) %>%
    tidytext::bind_tf_idf(entity, !!as.name(document_ids), n) %>%
    dplyr::mutate(term_relevance = (tf_idf * page_rank)) %>% # calculate topic term relevance as normalized (tf_idf * page_rank)
    dplyr::mutate(
      term_relevance = dplyr::case_when(!is.na(term_relevance) ~ scales::rescale(to = c(0, 1)), # rescale
                                        .default = 1),
      # set to 1 if no page rank is available (aka only 1 entity is in the topic)
      .by = topic
    )

  document_data <- tf_idf %>%
    dplyr::summarise(entities = list(entity),
                     document_relevance = sum(term_relevance), # sum up term relevance per doc
                     .by = c(topic, !!as.name(document_ids))) %>%
    dplyr::mutate(document_relevance = scales::rescale(document_relevance, # rescale per topic
                                                       to = c(0,100)),
                  .by = topic) %>%
    dplyr::arrange(topic, dplyr::desc(document_relevance))

  document_data <- document_data %>%
    dplyr::left_join(documents %>%
                       dplyr::select(!(!!as.name(document_tokens))) %>%
                       dplyr::distinct(!!as.name(document_ids),
                                       .keep_all = TRUE), by = document_ids)

  missing_data <- document_data %>% # collect missing meta data
    dplyr::select(!c(topic, entities, document_relevance)) %>%
    dplyr::select_if(~ any(is.na(.))) %>%
    dplyr::summarise(dplyr::across(tidyselect::everything(),
                                   ~ (sum(is.na(.)) / n()))) %>%
    dplyr::select_if(~ (. > 0))

  if (nrow(missing_data) > 0) {
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