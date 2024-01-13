#' Keep objects in global env.
#'
#' Keep the objects in the global environment which names are passed in.
#'
#' @param obj_names Character vector of names of objects as the one given in output by obj_exp.
#'
#' @return Removes objects which names have not been passed.
#' @export
#'
#' @examples
#'
#' obj_keep(obj_names = c("x2", "x1"))
#'
#' obj_exp(pattern = "x") |> obj_keep()

obj_keep <- function(obj_names){
  p <- globalenv()
  all_obj <- ls(p)
  v2r <- all_obj[!all_obj %in% obj_names]
  rm(list = v2r, envir = p)
}
