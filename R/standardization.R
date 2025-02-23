#' Standardize Composite Index Variables
#'
#' This function takes a matrix representing a composite index dataset and a method to standardize the columns by. Many common transformations are featured and the default method is to do a min-max transformation..
#'
#' @param X The composite index dataset.
#' @param method The method of standardization. Takes values c('min_max', 'z_scores', 'ratio_mean', 'ratio_leader', 'distance_mean', 'distance_leader') with the default being 'min-max'.
#'
#' @return A matrix with standardized column values.
#'
#' @examples
#' standardize(X, method = 'z_scores')
#'
#' @export
standardize = function(X, method = c('min_max', 'z_scores', 'ratio_mean', 'ratio_leader', 'distance_mean', 'distance_leader')){
  X = as.matrix(X)
  method = match.arg(method)
  if (!is.numeric(X)) stop('Must pass a numeric matrix, check for non-numeric entries')
  if (any(is.na(X))) message('Data has missing values')

  # calcs the observed quantiles of each indicator
  if (method == 'min_max'){
    std_tbl = apply(X, 2, function(x) (x - min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T)))
  }

  #  the observed z scores for each indicator
  if (method == 'z_scores') {
    std_tbl = apply(X, 2, function(x) (x - mean(x, na.rm = T))/sd(x, na.rm = T))
  }
  # calcs the ratio of each observation to the mean
  if (method == 'ratio_mean'){ #
    std_tbl = apply(X, 2, function(x) x/mean(x, na.rm = T))
  }

  # calcs the ratio of each observation to the group leader (max)
  if (method == 'ratio_leader'){
    std_tbl = apply(X, 2, function(x) x/max(x, na.rm = T))
  }

  # calcs the distance ratio of each observation to the group mean
  if (method == 'distance_mean'){
    std_tbl = apply(X, 2, function(x) (x-mean(x, na.rm = T))/mean(x, na.rm = T))
  }

  # calcs the distance ratio of each observation to the group leader (max)
  if (method == 'distance_leader'){
    std_tbl = apply(X, 2, function(x) (x-max(x, na.rm = T))/max(x, na.rm = T))
  }
  return(std_tbl)
}
