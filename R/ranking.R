# M = matrix(1:20, nc =4)
# set.seed(132)
# M = matrix(sample(M, 20, replace = F), nc =4)
# M = data.frame(M)
# rownames(M) = c(1:nrow(M))
# M
#
# # which column it falls into is its ranked vote
# ans = rbind(c(4,5,1,3,2), c(2,4,1,5,3), c(2,1,5,3,4), c(4,1,5,2,3))
#
# # for each column, to return the index of the rows from greatest to least
# # then transpose the matrix to get the ranking of the obs for each
# # indicator
# ballots = t(apply(M, 2, function(x) names(x)[(order(x,decreasing = T))]))
#
# library(votesys)
# ranked_votes = create_vote(ballots, xtype = 2, candidate = rownames(M))
#
# win1 = cdc_simple(ranked_votes)
# win2 <- cdc_simple(win1$cdc)
#
# win3 = borda_method(ranked_votes)
# win3


#' Non-Compensatory Ranking of Composite Index Observations
#'
#' This function takes in a dataframe which is the composite index dataset (after standardization and variable weights are applied) and ranks observations according to the Borda Count method or the Condorcet method. These methods are non-compensatory aggregation approaches to produce ranks for when a singular composite score is less preferable to a scoreboard- eg. when variables are negatively correlated.
#'
#' @param X The composite index dataset
#' @param method A voting method for ranked choices c('borda', 'condorcet'). Defaults to 'borda'.
#'
#' @return A votesys object which are the ranked choices.
#'
#' @export
rankings = function(X, method = c('borda', 'condorcet')){
  method = match.arg(method)
  X = data.frame(X)
  rownames(X) = c(1:nrow(X))

  ballots = t(apply(X, 2, function(x) names(x)[(order(x,decreasing = T))]))
  ranked_votes = votesys::create_vote(ballots, xtype = 2, candidate = rownames(X))

  if (method == 'borda') {
    return(votesys::borda_method(ranked_votes))
  }
  if (method == 'condorcet') return(votesys::cdc_simple(ranked_votes))
}
