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

#' Add missing years of data for a single scenario
#'
#' Used inside \code{\link{add_missing_yrs}} this function adds extra
#' years of data to a data frame, uses linear interpolation to fill in
#' years between points.
#'
#' @param df data frame that does not have entries of required years
#' @param req_years vector of all the years that are expected in the df
#' @return data frame with entries for all the years listed in req_years
#' @noRd
internal.add_missing_yrs_1scn <- function(df, req_years){

    # Save a copy of the meta data
    meta_names <- setdiff(names(df), c("year", "value"))
    meta_data <- unique(df[meta_names])
    missing_yrs_df <- cbind(data.frame(year = setdiff(req_years, df$year),
                                 value = NA),
                            meta_data, row.names = NULL)


    # This df contains the original values and NA entries
    # for the years of data that was missing in the original df.
    df2 <- rbind(missing_yrs_df, df, row.names = NULL)

    # Arrange by year and then use linear interpolation
    # to replace NA values.
    df2 <- df2[order(df2$year), ]
    df2$value <- approx(x = df2$year, y = df2$value, xout = df2$year)$y

    return(df2)

}

#' Add missing years of data for mulitple scenarios
#'
#'
#' @param df data frame that does not have entries of required years
#' @param req_years vector of all the years that are expected in the df
#' @return data frame with entries for all the years listed in req_years
#' @noRd
add_missing_yrs <- function(df, req_years){

    req_cols <- c("year", "value", "scenario")
    req_check(names(df), req_cols)

    split(df, df$scenario) %>%
        lapply(internal.add_missing_yrs_1scn, req_years = req_years)


    out_list <- lapply(X = split(df, df$scenario), FUN = internal.add_missing_yrs_1scn, req_years = req_years)
    out <- do.call(rbind, out_list)
    row.names(out) <- NULL
    return(out)

}














