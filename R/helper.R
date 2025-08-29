#' Check for required elements
#'
#' Insure that elements of a vector or the names of data frame contain the
#' required information otherwise throw an error indicating the missing elements.
#'
#' @param x the object to check
#' @param req vector of the required elements
#' @noRd
req_check <- function(x, req){
    missing <- setdiff(req, x)
    if(!length(missing) == 0){
        problem <- paste0("x is misisng: ", paste0(missing, collapse = ", "))
        stop(problem)
    }
    return(TRUE)
}





