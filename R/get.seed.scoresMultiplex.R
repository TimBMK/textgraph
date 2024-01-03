#' Get Scores for the seeds of the multiplex network
#'
#' @param Seeds
#' @param Number_Layers
#' @param tau
#'
#' @details An internal function from the RandomWalkRestartMH package, required for
#'   the RWR calculations. Pulled from https://github.com/alberto-valdeolivas/RandomWalkRestartMH/blob/master/R/InternalFunctions.R
#'
#' @return
#' @keywords internal
#'

get.seed.scoresMultiplex <- function(Seeds,Number_Layers,tau) {

  Nr_Seeds <- length(Seeds)

  Seeds_Seeds_Scores <- rep(tau/Nr_Seeds,Nr_Seeds)
  Seed_Seeds_Layer_Labeled <-
    paste0(rep(Seeds,Number_Layers),sep="_",rep(seq(Number_Layers),
                                                length.out = Nr_Seeds*Number_Layers,each=Nr_Seeds))

  Seeds_Score <- data.frame(Seeds_ID = Seed_Seeds_Layer_Labeled,
                            Score = Seeds_Seeds_Scores, stringsAsFactors = FALSE)

  return(Seeds_Score)
}
