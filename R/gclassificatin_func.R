#  -------------------------------------------------------------

#' How to document functions.
#'
#' This is an example showing how to document functions to be included in the package.
#'
#' @export
#' @name parse_outlet
#' @author Stefan Lüdtke
#' @param text Message to print.
#' @seealso \url{http://r-pkgs.had.co.nz/man.html}
#' @examples
#' parse_outlet('Hello world!')

parse_outlet = function(node, func_station_id = NA) {
  result = node$station_id
  if(is.null(result)){
    node$station_id = func_station_id
    return(node)
  } else{
    result =  sapply(node$children, parse_outlet, func_station_id = result)
    return(node)
  }
}

#  -------------------------------------------------------------

#' How to document functions.
#'
#' This is an example showing how to document functions to be included in the package.
#'
#' @export
#' @name examplefunction
#' @author Francisco Rodriguez-Sanchez
#' @param text Message to print.
#' @seealso \url{http://r-pkgs.had.co.nz/man.html}
#' @examples
#' examplefunction('Hello world!')

examplefunction <- function(text){
  return(text)
}
