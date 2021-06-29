#' Objective Function to Optimize Over
#'
#' This function is the objective function to be optimized over. It first calculates the Shapley Effects of a set variables in a composite index given a set of variable weights and then calculates the Euclidean distance between the Shapley Effects and a given vector of desired importances. The Shapley Effects are estimated using the sobolshap_knn funciton from the sensitivity package in R. The model is function which applies the variable weights and outputs the composite index scores.
#'
#' @param wts An initial set of variable weights. If omitted, equal weights are assumed.
#' @param impt The target vector of variable importances.
#' @param model A function to apply variable weights and aggregate variables to output composite index scores
#' @param data The composite index dataset.
#' @param ... Additional arguements to pass into the sobolshap_knn function
#' @return A numeric which is the Euclidean distance between estimated Shapley Effects and desired importances.
#'
#' @examples
#' standardize(X, agg_method = 'z_scores')
#'
#' @export
weights_shapley_diff = function(wts = NULL, impt = NULL, model = NULL, data, id.cat = NULL,
                                U = NULL, method = "knn", n.knn = 2, return.shap = FALSE, randperm = FALSE,
                                n.perm = 10000, rescale = FALSE, n.limit = 2000, noise = FALSE, ...){
  # if (any(is.na(X)) ) message('Data has missing values')
  if (is.null(wts)){
    message('Weights not provided, using equal weights')
    wts = rep(1/ncol(X), ncol(X))
  }
  if (is.null(impt)){
    message('Importances not provided, assuming equal importance')
    impt = rep(1/ncol(X), ncol(X))
  }
  wts= wts/sum(wts)

  #calculating the shapely effects
  res = sensitivity::sobolshap_knn(model = model, X = data, id.cat = id.cat,
                      U = U, method = method, n.knn = n.knn, return.shap = return.shap, randperm = randperm,
                      n.perm = n.perm, rescale = rescale, n.limit = n.limit, noise = noise, var_wts = wts,...)

  #distance between shapley effects and desried importances
  distance = stats::dist(rbind(res$Shap,impt))
  return(as.numeric(distance))
}

