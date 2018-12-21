#' LIME kernel that treats all observations as equally similar to the observation of interest.
#'
#' Kernels are meant to be used as an argument to individual_surrogate_model function.
#' Other custom functions can be used. Such functions take two vectors and
#' return a single number.
#'
#' @param explained_instance explained instance
#' @param simulated_instance new observation
#'
#' @return numeric
#'
#' @export
#'
#' @examples
#' library(DALEX2)
#' library(randomForest)
#' library(localModel)
#' data('apartments')
#' mrf <- randomForest(m2.price ~., data = apartments, ntree = 50)
#' explainer <- explain(model = mrf,
#'                      data = apartments[, -1])
#' model_lok <- individual_surrogate_model(explainer, apartments[5, -1],
#'                                         size = 500, seed = 17,
#'                                         kernel = identity_kernel)
#' # In this case each simulated observation has equal weight
#' # when explanation model (LASSO) is fitted.
#' model_lok
#' plot(model_lok)
#'

identity_kernel <- function(explained_instance, simulated_instance) {
  1
}


#' LIME kernel from the original article with sigma = 1.
#'
#' Since only binary features are used, the weight associated with an observation
#' is simply exp(-\{number of features that were changed compared to the original observation\}).
#' Kernels are meant to be used as an argument to individual_surrogate_model function.
#' Other custom functions can be used. Such functions take two vectors and
#' return a single number.
#'
#' @param explained_instance explained instance
#' @param simulated_instance new observation
#'
#' @return numeric
#'
#' @export
#'
#' @examples
#' library(DALEX2)
#' library(randomForest)
#' library(localModel)
#' data('apartments')
#' mrf <- randomForest(m2.price ~., data = apartments, ntree = 50)
#' explainer <- explain(model = mrf,
#'                      data = apartments[, -1])
#' model_lok <- individual_surrogate_model(explainer, apartments[5, -1],
#'                                         size = 500, seed = 17,
#'                                         kernel = gaussian_kernel)
#' # In this case each simulated observation has weight
#' # that is small when the distance from original observation is large,
#' # so closer observation have more weight.
#' model_lok
#' plot(model_lok)
#'

gaussian_kernel <- function(explained_instance, simulated_instance) {
  exp(-sum((explained_instance - simulated_instance)^2))
}
