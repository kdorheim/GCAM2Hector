# Get the historical emissions used to drive Hector before GCAM emissions
# are supplied to the Hector core.
# Args
#   file: path to the gcam emissions file, default is the data included in the auxiliary data
# Returns: data.frame of the emissions used to drive Hector's historical period


#' Get the historical emissions used to drive Hector before GCAM emissions
#'
#' Get the historical emissions used to drive Hector before GCAM emissions
#' are supplied to the Hector core.
#'
#'
#' @param file path to the gcam emissions file, default is the data included in the auxiliary data
#' @noRd
get_pregcam_emiss <- function(file = NULL){

    if(is.null(file)){
        return(PREGCAM_EMISS_DF)
    }

    emiss <- read_hector_csv(file)

    # There should be no emissions greater than the transition date.
    stopifnot(all(emiss$year <= TRANSITION_DATE))

    # Make sure that all the required emissions are present!
    missing <- setdiff(emiss$variable, GCAM_EMISS)
    stopifnot(length(missing) == 0)

    return(emiss)
}

#' Get the default emissions used regardless of GCAM scenario
#'
#' @param file path to the gcam emissions file, if set to NULL it will return internal data
#' @return data.frame of the emissions used to drive Hector's historical period
#' @noRd
get_default_emiss <- function(file = NULL){

    if(is.null(file)){
        return(DEFAULT_EMISS_DF)
    }

    emiss <- read_hector_csv(file)

    # There should be emissions up until th year 2100
    stopifnot(max(emiss$year) >= 2100)

    # Make sure that all the required emissions are present!
    missing <- setdiff(emiss$variable, DEFAULT_EMISS)
    stopifnot(length(missing) == 0)

    return(emiss)
}

