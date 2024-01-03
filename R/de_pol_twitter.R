#' Sample data of Tokenized Tweets from German Politicians
#'
#' Data contains the tokenized tweets of elected German polticians sent between 31st of July and 6th of August 2022. Tokenization and lemmatization via SpaCy's de_core_news_lg model. 
#'  Lemmas have additionally been set to lower case. 
#' `doc_id` describes the tweet ID, `author_id` the user ID of the authoring account, and `created_at` the moment of tweeting. `tweet_url` contains the link to the original tweet. 
#'  Additionally, the variable `party` has been added, describing the party association of the account in question (`NA` if the account has no party association).
#' For demonstration purposes in the package, the variable `ministry_name` has been added, with `NA` for all tweets by accounts that do not represent a German ministry.
#'
#' @docType data
#'
#' @usage data(de_pol_twitter)
#'
#' @format description
#'
#' @keywords datasets
#'
#' @references König et al. (2022) The EPINetz Twitter Politicians Dataset 2021. A New Resource for the Study of the German Twittersphere and Its Application for the 2021 Federal Elections. In: Politische Vierteljahresschrift 63 (June): 529–47.
#' (\href{https://doi.org/10.1007/s11615-022-00405-7}{DOI: 10.1007/s11615-022-00405-7})
"de_pol_twitter"
