#' Test Function
#'
#' @description will square any number
#'
#' @param x a number
#' @examples
#' \dontrun{
#' test_function(2)
#' }
#' @export

test_function <- function(x){
  x ** 2
}

#' Test Function that fails
#'
#' @description this function always fails
#'
#' @param x a number
#' @examples
#' \dontrun{
#' test_function_fail(2)
#' }
#' @export

test_function_fail <- function(x){
  stop("This function will fail")
}