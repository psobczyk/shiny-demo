

#' Capitalize word
#'
#' @param x 
#'
#' @return varchar
#' @export
#'
capitalize <- function(x){
  paste0(toupper(substring(x, 1, 1)), tolower(substring(x, 2, nchar(x))))
}
