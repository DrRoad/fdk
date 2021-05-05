#' Fit Croston model
#'
#' @param .data Data frame or tibble with a response variable.
#' @param parameter list: Combination of parameter to estimate the model.
#'
#' @import forecast
#' @import stats
#' @return data-frame
#' @export
#'
#' @examples
#' \dontrun{
#' get_croston()
#' }
fit_croston <- function(.data, parameter = NULL){
  globalVariables(c(".log"))
  horizon <- 1
  # Ts object
  y_var_int <- ts(.data[["y_var"]], frequency = .log$prescription$freq)
  # Fit
  if(is.null(parameter)){
    model_fit <- croston(y_var_int, h = horizon)
  }else{
    model_fit <- croston(y_var_int
                         , alpha = parameter[["croston"]][["alpha"]]
                         , h = horizon)
  }
  # Output
  #class(model_fit) <- "num_fit"
  return(model_fit)
}
