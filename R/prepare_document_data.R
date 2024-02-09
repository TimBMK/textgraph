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
                                  document_ids) {

  join_dat <- documents %>% # prepare data for data.table join
    dplyr::select(!!as.name(document_tokens),
                  !!as.name(document_ids)) %>%
    dplyr::rename(entity = !!as.name(document_tokens)) %>%
    data.table::as.data.table()

  data.table::setkeyv(join_dat, "entity")

  topics <- data.table::as.data.table(topics)

  data.table::setkeyv(topics, "entity")

  tf_idf <- data.table::merge.data.table(topics, join_dat, by = "entity", allow.cartesian = T)

  invisible(gc())

  tf_idf <- tf_idf[ , .N, by = c("entity", document_ids, "topic", "page_rank")] # count duplicated entities

  tf_idf <- tf_idf %>%
    tidytext::bind_tf_idf(entity, !!as.name(document_ids), N) %>% # calculate tf-idf
    dplyr::mutate(page_rank = dplyr::case_when(is.na(page_rank) ~ 0, # set missing page ranks to 0 (missing if a term has no connection to other terms in page_rank = "cluster")
                                               .default = page_rank)) %>%
    dplyr::mutate(term_relevance = (tf_idf * page_rank) %>% # calculate topic term relevance as normalized (tf_idf * page_rank)
                    scales::rescale(to = c(0, 1)), # rescale
                  .by = topic)

  document_data <- tf_idf[ , .(entities = list(entity),# summarise entities and document relevance
                               document_relevance = sum(term_relevance)),
                           by = c("topic", document_ids)]

  document_data[ , document_relevance := scales::rescale(document_relevance, # rescale per topic
                                                         to = c(0,100)),
                 by = "topic"]

  data.table::setorder(document_data, topic, -document_relevance)


  join_dat <- documents %>% # prepare 2nd join
    dplyr::select(!(!!as.name(document_tokens))) %>%
    dplyr::distinct(!!as.name(document_ids),
                    .keep_all = TRUE) %>%
    data.table::as.data.table()

  data.table::setkeyv(join_dat, document_ids)
  data.table::setkeyv(document_data, document_ids)

  document_data <- document_data %>%
    data.table::merge.data.table(join_dat, by = document_ids)

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
