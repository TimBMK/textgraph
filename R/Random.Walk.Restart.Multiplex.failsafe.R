#' Random Walk with Restart Failsafe for Multiplex Networks
#'
#' This adjusted version of the RandomWalkRestartMH function includes a failsafe for infinity values during computation.
#'
#' @param x A dgCMatrix object of the adjacency matrix.
#' @param MultiplexObject A Multiplex object.
#' @param Seeds A character vector of seed nodes.
#' @param r The restart parameter, should be between 0 and 1.
#' @param tau A numerical vector specifying layer weights, summing up to 1 when divided by number of layers.
#' @param MeanType A string specifying the mean type to compute global scores, one of "Geometric", "Arithmetic", "Sum".
#' @param DispResults A string to decide results display method, either "TopScores" or "Alphabetic".
#' @param ... Additional parameters.
#'
#' @return A list with Random Walk with Restart results and meta-information.
#'
#' @details
#' An adjusted version of https://github.com/alberto-valdeolivas/RandomWalkRestartMH/blob/master/R/RWRandMatrices.R
#'  including a failsafe for values in the proximity vectors reaching (-)Inf
#'
#' @importFrom RandomWalkRestartMH isMultiplex get.seed.scoresMultiplex geometric.mean
#'
#' @keywords internal
Random.Walk.Restart.Multiplex.failsafe <- function(x, MultiplexObject, Seeds,
                                                   r=0.7,tau,MeanType="Geometric", DispResults="TopScores",...){

  ### We control the different values.
  if (!is(x,"dgCMatrix")){
    stop("Not a dgCMatrix object of Matrix package")
  }

  if (!isMultiplex(MultiplexObject)) {
    stop("Not a Multiplex object")
  }

  L <- MultiplexObject$Number_of_Layers
  N <- MultiplexObject$Number_of_Nodes

  Seeds <- as.character(Seeds)
  if (length(Seeds) < 1 | length(Seeds) >= N){
    stop("The length of the vector containing the seed nodes is not
         correct")
  } else {
    if (!all(Seeds %in% MultiplexObject$Pool_of_Nodes)){
      stop("Some of the seeds are not nodes of the network")

    }
  }

  if (r >= 1 || r <= 0) {
    stop("Restart parameter should be between 0 and 1")
  }

  if(missing(tau)){
    tau <- rep(1,L)/L
  } else {
    tau <- as.numeric(tau)
    if (sum(tau)/L != 1) {
      stop("The sum of the components of tau divided by the number of
           layers should be 1")
    }
  }

  if(!(MeanType %in% c("Geometric","Arithmetic","Sum"))){
    stop("The type mean should be Geometric, Arithmetic or Sum")
  }

  if(!(DispResults %in% c("TopScores","Alphabetic"))){
    stop("The way to display RWRM results should be TopScores or
         Alphabetic")
  }

  ## We define the threshold and the number maximum of iterations for
  ## the random walker.
  Threeshold <- 1e-10
  NetworkSize <- ncol(x)

  ## We initialize the variables to control the flux in the RW algo.
  residue <- 1
  iter <- 1

  ## We compute the scores for the different seeds.
  Seeds_Score <- RandomWalkRestartMH:::get.seed.scoresMultiplex(Seeds,L,tau)

  ## We define the prox_vector(The vector we will move after the first RWR
  ## iteration. We start from The seed. We have to take in account
  ## that the walker with restart in some of the Seed nodes, depending on
  ## the score we gave in that file).
  prox_vector <- matrix(0,nrow = NetworkSize,ncol=1)

  prox_vector[which(colnames(x) %in% Seeds_Score[,1])] <- (Seeds_Score[,2])

  prox_vector  <- prox_vector/sum(prox_vector)
  restart_vector <-  prox_vector

  vector_range <- 0

  while(!any(c(-Inf, Inf) %in% range(vector_range)) && # a failsafe to stop the loop from breaking when values reach -Inf or Inf
        residue >= Threeshold){

    old_prox_vector <- prox_vector
    prox_vector <- (1-r)*(x %*% prox_vector) + r*restart_vector
    vector_range <- range(prox_vector@x)
    if (any(c(-Inf, Inf) %in% range(vector_range))) { # print the last Residue before infinity
      warning(paste("Vector Range reached infinite values.",
                    "Stopping Random Walk at", iter, "Iterations with Residue",
                    residue, "instead of the intended Threshold of", Threeshold,
                    "for Seed", Seeds))
    }
    residue <- sqrt(sum((prox_vector-old_prox_vector)^2))
    iter <- iter + 1;
  }


  NodeNames <- character(length = N)
  Score = numeric(length = N)

  rank_global <- data.frame(NodeNames = NodeNames, Score = Score)
  rank_global$NodeNames <- gsub("_1", "", row.names(prox_vector)[seq_len(N)])

  if (MeanType=="Geometric"){
    rank_global$Score <- RandomWalkRestartMH:::geometric.mean(as.vector(prox_vector[,1]),L,N)
  } else {
    if (MeanType=="Arithmetic") {
      rank_global$Score <- regular.mean(as.vector(prox_vector[,1]),L,N)
    } else {
      rank_global$Score <- sumValues(as.vector(prox_vector[,1]),L,N)
    }
  }

  if (DispResults=="TopScores"){
    ## We sort the nodes according to their score.
    Global_results <-
      rank_global[with(rank_global, order(-Score, NodeNames)), ]

    ### We remove the seed nodes from the Ranking and we write the results.
    Global_results <-
      Global_results[which(!Global_results$NodeNames %in% Seeds),]
  } else {
    Global_results <- rank_global
  }

  rownames(Global_results) <- c()

  RWRM_ranking <- list(RWRM_Results = Global_results,Seed_Nodes = Seeds)

  class(RWRM_ranking) <- "RWRM_Results"
  return(RWRM_ranking)
}

