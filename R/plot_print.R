#' Generic plot function for local surrogate explainers
#'
#' @param x object of class local_surrogate_explainer
#' @param ... other objects of class local_surrogate_explainer.
#' If provided, models will be plotted in rows, response levels in columns.
#' @param geom If "point", lines with points at the end will be plotted,
#' if "bar", bars will be plotted and if "arrow", arrows.
#'
#' @import ggplot2
#'
#' @importFrom stats reorder
#'
#' @export
#'
#' @examples
#' # Example based on apartments data from DALEX package.
#' library(DALEX)
#' library(randomForest)
#' library(localModel)
#' data('apartments')
#' mrf <- randomForest(m2.price ~., data = apartments, ntree = 50)
#' explainer <- explain(model = mrf,
#'                      data = apartments[, -1])
#' model_lok <- individual_surrogate_model(explainer, apartments[5, -1],
#'                                         size = 500, seed = 17)
#' model_lok
#' plot(model_lok)
#'

plot.local_surrogate_explainer <- function(x, ..., geom = "point") {
  variable <- estimated <- intercept <- NULL

  models <- do.call("rbind", c(list(x), list(...)))

  if(all(models$estimated[models$original_variable != ""] == 0)) {
    message("All estimated feature effects are equal to 0.")
    return(ggplot())
  }

  models$labeller <- paste(
    models$model,
    "prediction: ",
    round(models$predicted_value, 2)
  )

  models <- do.call("rbind", by(
    models,
    models$response,
    function(y) {
      y$labeller <- paste(
        unique(y$response),
        paste(unique(y$labeller),
              sep = "\n ", collapse = "\n "),
        sep = "\n ")
      y
    }
  ))

  models$sign <- as.factor(as.character(sign(models$estimated)))
  models <- models[models$variable != "(Intercept)", ]

  models <- do.call("rbind", by(
    models,
    list(models$model, models$response),
    function(y) {
      y$intercept <- y$estimated[y$variable == "(Model mean)"]
      y
    }
  ))

  models <- do.call("rbind", by(
    models,
    list(models$model, models$response),
    function(y) {
      y$estimated = ifelse(
        y$variable == "(Model mean)",
        y$estimated,
        y$estimated + y$intercept
      )
      y
    }
  ))

  models <- do.call("rbind", by(
    models,
    list(models$original_variable),
    function(y) {
      y$all_zero <- all(y$sign == 0)
      y
    }
  ))

  models <- models[!models$all_zero, ]
  models <- models[models$variable != "(Model mean)", ]

  if(geom == "point") {
    final_geom <- geom_pointrange(aes(ymin = intercept, ymax = estimated),
                                  size = 2)
  } else if(geom == "bar") {
    final_geom <- geom_segment(aes(xend = variable, yend = intercept, y = estimated),
                               size = 10, lineend = "butt")
  } else {
    final_geom <- geom_segment(aes(xend = variable, yend = intercept, y = estimated),
                               size = 2,
                               arrow = arrow(length=unit(0.20,"cm"),
                                             ends="first", type = "closed"))
  }

  ggplot(models, aes(x = reorder(variable, -abs(estimated)),
                     y = estimated,
                     color = sign)) +
    theme_bw() +
    geom_hline(aes(yintercept = intercept),
               size = 1)  +
    facet_grid(model~labeller, scales = "free_y") +
    final_geom +
    coord_flip() +
    ylab("Feature influence") +
    xlab("") +
    scale_color_manual(values =  c(`-1` = "#d8b365",
                                   `0` = "#f5f5f5",
                                   `1` = "#5ab4ac")) +
    guides(color = "none")
}


#' Generic print function for local surrogate explainers
#'
#' @param x object of class local_surrogate_explainer
#' @param ... currently ignored
#'
#' @export
#'
#' @importFrom utils head
#'
#' @examples
#' # Example based on apartments data from DALEX package.
#' library(DALEX)
#' library(randomForest)
#' library(localModel)
#' data('apartments')
#' mrf <- randomForest(m2.price ~., data = apartments, ntree = 50)
#' explainer <- explain(model = mrf,
#'                      data = apartments[, -1])
#' model_lok <- individual_surrogate_model(explainer, apartments[5, -1],
#'                                         size = 500, seed = 17)
#' plot(model_lok)
#' model_lok
#'

print.local_surrogate_explainer <- function(x, ...) {
  print(head(as.data.frame(x)))
}