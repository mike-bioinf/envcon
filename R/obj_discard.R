#' Remove objects in global env.
#'
#' Remove the object in the global environment which names are passed in.
#'
#' @param obj_names Character vector of names as the one given in output by obj_exp.
#'
#' @return Removes objects which names have been passed.
#' @export
#'
#' @examples
#'
#' obj_discard(obj_names = c("x2", "x1"))
#'
#' obj_exp(pattern = "x") |> obj_discard()

obj_discard <- function(obj_names){
  rm(list = obj_names, envir = globalenv())
}
