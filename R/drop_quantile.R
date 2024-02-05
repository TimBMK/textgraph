#' Drop lower quantiles of tokens
#'
#' This function drops all tokens in a data frame occurring below the threshold of a given quantile
#'
#' @param data Data Frame
#' @param tokens String; column containing the tokens to count and drop
#' @param quantile Either Numerical or a Vector of length 2; If a single value: the quantile up to and including which tokens are dropped.
#'  If a vector of two numerical values, the first indicates the lower, the second the upper threshold. E.g., c(0.1, 0.95) drops tokens
#'  below the 10\% and above the 95\% quantile.
#' @param ignore_case Logical; should upper/lower case of the tokens be ignored? Preserves original case
#' @param group String; grouping column, e.g. type of word, for report statistics
#' @param verbose Logical; should statistics be printed out?
#'
#' @return A data frame containing all the variables of the original `data` as well as the additional
#'  variable `entity count` indicating the number of times a token appears in the data. All tokens below
#'  the `entity_count` specified through the quantile (and printed through `verbose`) are dropped.
#'
#' @details Tokens falling within or below the specified quantile will be dropped. E.g., when specifying
#'  `quantile = 0.1`, all tokens within or below the 10\% quantile of the entity count will be dropped.
#'
#' @export
#'
#' @importFrom dplyr "%>%"
#' @importFrom dplyr mutate n ungroup group_by tibble distinct pull filter summarise left_join arrange add_row
#' @importFrom stringr str_to_lower
#' @importFrom scales percent
#' @importFrom stats quantile
#'
#' @examples
#' data("de_pol_twitter")
#'
#' de_pol_twitter_reduced <- drop_quantile(data = de_pol_twitter,
#'                                        tokens = "lemma",
#'                                        quantile = 0.1,
#'                                        ignore_case = FALSE, # in this case, "lemma" has already been set to lower case
#'                                        group = "tag",
#'                                        verbose = TRUE)

drop_quantile <- function(data,
                          tokens,
                          quantile = 0.1,
                          ignore_case = TRUE,
                          group,
                          verbose = TRUE
)
{

  if(length(quantile) > 2) {
    stop("'Quantile' must be either a single numerical value or a numerical vector of length 2.")
  }

  if (ignore_case == TRUE) {
    data <- data %>%
      dplyr::mutate(
        orig_tokens = !!as.name(tokens), # preserve original tokens
        {{tokens}} := stringr::str_to_lower(!!as.name(tokens)))  # convert to lower case to ignore case
  }

  # add count for tokens
  res <- data %>%
    dplyr::group_by(!!as.name(tokens)) %>%
    dplyr::mutate(entity_count = dplyr::n()) %>%
    dplyr::ungroup()

  # save min and max counts for verbose output
  if (verbose == TRUE) {
    range <- dplyr::tibble(min = min(res$entity_count),
                           max = max(res$entity_count))
  }

  # calculate quantile for filtering
  if(length(quantile) == 1) { # one value
    threshold <- res %>%
      dplyr::distinct(!!as.name(tokens), .keep_all = T) %>%
      dplyr::pull(entity_count) %>%
      stats::quantile(probs = quantile) %>%
      .[[1]]

    # drop quantile
    res <- res %>%
      dplyr::filter(entity_count > threshold)
    } else { # value range
    threshold_1 <- res %>%
      dplyr::distinct(!!as.name(tokens), .keep_all = T) %>%
      dplyr::pull(entity_count) %>%
      stats::quantile(probs = quantile[1]) %>%
      .[[1]]

    threshold_2 <- res %>%
      dplyr::distinct(!!as.name(tokens), .keep_all = T) %>%
      dplyr::pull(entity_count) %>%
      stats::quantile(probs = quantile[2]) %>%
      .[[1]]

    # drop quantile
    res <- res %>%
      dplyr::filter(entity_count > threshold_1 & entity_count < threshold_2)
    }

  if (verbose == TRUE) {

    if(length(quantile) == 1) {
    cat(paste(scales::percent(quantile, accuracy = quantile*100), "Quantile:", threshold, "\n"))
    } else {
      cat(paste(scales::percent(quantile[1], accuracy = quantile[1]*100), "Quantile:", threshold_1, "\n"))
      cat(paste(scales::percent(quantile[2], accuracy = quantile[2]*100), "Quantile:", threshold_2, "\n"))
      }

    cat(paste("Count Range:", range$min, "-", range$max, "\n"))

    original_count <- data %>% # save unprocessed count statistics
      dplyr::distinct(!!as.name(tokens), !!as.name(group)) %>%
      dplyr::summarise(distinct_unprocessed = dplyr::n(), .by = {{group}})

    res %>%
      dplyr::distinct(!!as.name(tokens),!!as.name(group)) %>%
      dplyr::group_by(!!as.name(group)) %>%
      dplyr::summarise(distinct_processed = n()) %>%
      dplyr::left_join(original_count, by = group) %>%
      dplyr::arrange(!!as.name(group)) %>%
      dplyr::add_row(
        {{group}} := "total",
        distinct_unprocessed = sum(.$distinct_unprocessed),
        distinct_processed = sum(.$distinct_processed)
      ) %>%
      dplyr::mutate(
        dropped = distinct_unprocessed - distinct_processed,
        reduction =  scales::percent(1 - distinct_processed / distinct_unprocessed)
      ) %>%
      print()
  }

  if (ignore_case == TRUE) {
    res <- res %>% dplyr::mutate({{tokens}} := orig_tokens) %>% # restore original tokens
      dplyr::select(!orig_tokens)
  }

  return(res)

}
