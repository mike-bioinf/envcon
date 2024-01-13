#' Expand a pattern found or not in glabal env objects.
#'
#' @param pattern A regular expression accepted by grep.
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
#'

obj_exp <- function(pattern, invert = FALSE){

  p <- globalenv()
  all_obj <- ls(p)

  for(i in all_obj){
    if(is.function(eval(parse(text = i), envir = p))){
      all_obj <- all_obj[!all_obj %in% i]
    }
  }

  vp <- c()

  for(i in all_obj){
    if(invert){
      if(!grepl(pattern = pattern, x = i)){
        vp <- c(vp, i)
      }
    }else if(grepl(pattern = pattern, x = i)){
      vp <- c(vp, i)
    }
  }

  return(vp)
}


