#' Calculate Geometric Mean
#'
#' @param Scores Scores
#' @param L L
#' @param N N
#'
#' @details An internal function from the RandomWalkRestartMH package, required for
#'   the RWR calculations. Pulled from https://github.com/alberto-valdeolivas/RandomWalkRestartMH/blob/master/R/InternalFunctions.R
#'
#' @return Score
#' @keywords internal
#'
geometric.mean <- function(Scores, L, N) {

  FinalScore <- numeric(length = N)

  for (i in seq_len(N)){
    FinalScore[i] <- prod(Scores[seq(from = i, to = N*L, by=N)])^(1/L)
  }

  return(FinalScore)
}
