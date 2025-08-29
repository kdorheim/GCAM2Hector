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

#' Replicate data over scenarios
#'
#' This is used to replicate default of preGCAM emissions which are needed
#' to complete scenarios.
#'
#' @param x data frame containing the information that needs to be replicated over n number of scenarios
#' @param scns vector of the scenarios
#' @return data frame of results now containing scenario information
#' @noRd
repeat_for_scns <- function(x, scns){

    # There data frame should not contain any scenario information,
    # since that is what we are adding...
    stopifnot(!"scenario" %in% names(x))

    out <- data.frame()

    # Make a copy of the data frame for each scenario.
    for(i in seq_along(scns)){
        out <- rbind(out, x)
    }

    # Add the scenario information to the data frame
    scenario_vector <- rep(scns, each = nrow(out)/length(scns))
    out$scenario <- scenario_vector


    return(out)

}

#' Read in a Hector input csv table with a nice format
#'
#'
#' @param file character vectors, containing the path name to the hector input csv file.
#' @return data frame of Hector inputs
#' @noRd
read_hector_csv <- function(file){

    wide_df <- read.csv(file, comment.char = ";")

    long_df <- reshape(wide_df,
                       varying = names(wide_df)[-1], # Select all columns except 'Date'
                       v.names = "Value",           # Name for the new value column
                       timevar = "Variable",        # Name for the new variable column
                       times = names(wide_df)[-1],  # Names of variables to appear in the 'Variable' column
                       idvar = "Date",              # Identifier column
                       direction = "long")

    # Clean up the data frame
    rownames(long_df) <- NULL
    colnames(long_df) <- c("year", "variable", "value")

    # TODO this suppress warning can be dropped when upgraded >= hector v3.5
    suppressWarnings({
        long_df$units <- hector::getunits(long_df$variable)
    })

    return(long_df)

}





