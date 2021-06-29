#' Plot Desired Importance versus Shapley Effects Before and After Optimization
#'
#' This function takes in three vectors: the desired variable importances, the Shapley Effects resulting from the original variable weight loadings, and the Shapley Effects resulting from the optimized variable weights, and graphs them. It takes the column names from the dataframe df.
#'
#' @param df The composite index dataset
#' @param desired_impt The vector of desired variable importances
#' @param shapley_values The vector of Shapley Effects that result from the original loading of variable weights
#' @param optim_shapley The vector of Shapley Effects that result from the optimized variable weights
#'
#' @return A ggplot object depicting the importances
#'
#' @export
desired_v_shapley = function(df, desired_impt, shapley_values,optim_shapley){
  data.frame(variable = colnames(df),desired = desired_impt, e_shapley = shapley_values, optimized = optim_shapley) %>%
    tidyr::pivot_longer( -variable, names_to="impt", values_to="value") %>%
    ggplot2::ggplot(aes(variable,value,fill = impt)) +
    ggplot2::geom_bar(stat = "identity",position=ggplot2::position_dodge())+
    ggplot2::geom_text(aes(label=round(value,2)), vjust=1.6, color = 'white',
              position = ggplot2::position_dodge(.9), size=3)+
    ggplot2::xlab('Dimension')+
    ggplot2::ylab('Importance')+
    ggplot2::theme_light()+
    ggplot2::scale_fill_manual(name = "Importance", values = c("deepskyblue3", 'springgreen4',"orange"),
                        labels = c("Desired", "Original\nShapley\nEffect","Optimized\nShapley\nEffect"))
}


#' Plot Original Variable Weights versus Optimized Variable Weights
#'
#' This function takes in two vectors: the original variable weights and the optimized variable weights and plots them against one another.
#' It takes the column names from the dataset df.
#'
#' @param df The composite index dataset
#' @param old_weights The vector of original variable weights
#' @param optim_wts The vector of optimized variable weights
#'
#' @return A ggplot object depicting the weights
#'
#' @export
wts_v_optim_wts = function(df, old_weights, optim_wts){
  data.frame(variable = colnames(df),old_weights = old_weights,
             optimized_weights = optim_wts) %>%
    tidyr::pivot_longer( -variable, names_to="weight", values_to="value") %>%
    ggplot2::ggplot(aes(variable,value, fill = weight)) +
    ggplot2::geom_bar(stat = "identity",position = ggplot2::position_dodge())+
    ggplot2::geom_text(aes(label=round(value,2)), vjust=1.6, color = 'white',
              position = ggplot2::position_dodge(.9), size=3.5)+
    ggplot2::xlab('Dimension')+
    ggplot2::ylab('Weight')+
    ggplot2::theme_light()+
    ggplot2::scale_fill_manual(name = "Weights", values = c("deepskyblue3", "orange"),
                        labels = c("Original", "Optimized"))
}


# #https://stackoverflow.com/questions/35717353/split-violin-plot-with-ggplot2
# GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin,
#                            draw_group = function(self, data, ..., draw_quantiles = NULL) {
#                              data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
#                              grp <- data[1, "group"]
#                              newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
#                              newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
#                              newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
#                              if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
#                                stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
#                                                                          1))
#                                quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
#                                aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
#                                aesthetics$alpha <- rep(1, nrow(quantiles))
#                                both <- cbind(quantiles, aesthetics)
#                                quantile_grob <- GeomPath$draw_panel(both, ...)
#                                ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
#                              }
#                              else {
#                                ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
#                              }
#                            })
#
# geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ...,
#                               draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE,
#                               show.legend = NA, inherit.aes = TRUE) {
#   layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin,
#         position = position, show.legend = show.legend, inherit.aes = inherit.aes,
#         params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
# }
