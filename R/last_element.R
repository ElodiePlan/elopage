#' get the last element of a vector
#' @param .x vector
#' @examples last_element(c(1:10))
#' @export

last_element <- function(.x){
  return(.x[length(.x)])
}


