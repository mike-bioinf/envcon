#' Expand a pattern found or not in global env objects.
#'
#' Function used to select the names of the global environment objects based on a pattern accepted by grep. Regular expression can be used.
#'
#' @param pattern A regular expression accepted by grep.
#' @param on_fun Logical value indicating if the expansion must be done on the function names too. Default FALSE.
#' @param out_fun Logical value indicating if the function names must be kept regardless of the pattern. Default FALSE.
#' @param invert Logical value indicating if you want to select the objects that not contains the pattern. Default FALSE.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#'
#' home11 <- "nice"
#' home2 <- "ugly"
#' temperature <- 22
#'
#' obj_exp(pattern = "home", invert = FALSE)
#' ## Output: "home11" "home2"

obj_exp <- function(pattern, on_fun = FALSE, out_fun = FALSE, invert = FALSE){

  if(!is.character(pattern)){
    stop("pattern must be a character expression compatible with grep")
  }

  if(on_fun & out_fun){
    stop("Is not possible to set on_fun and out_fun both as TRUE, check the documentation for major details about these arguments")
  }

  p <- globalenv()
  all_obj <- ls(p)

  vfun <- c()
  for(i in all_obj){
    if(is.function(eval(parse(text = i), envir = p))){
      vfun <- c(vfun, i)
    }
  }

  if(on_fun){
    obj <- all_obj
  } else {
    obj <- all_obj[!all_obj %in% vfun]
  }

  vp <- Filter(function(x)(invert && !grepl(pattern, x)) || (!invert && grepl(pattern, x)), obj)

  if(out_fun){
    vp <- c(vp, vfun)
  }

  return(vp)
}


