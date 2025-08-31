#' Hector inputs and emissions used before GCAM
#'
#' The inputs that drive Hector from 1745 until the GCAM run begins
#' and future emissions are passed directly from GCAM to Hector.
#'
#' @format ## `PREGCAM_EMISS_DF`
#' A data frame with 6,003 rows and 4 columns:
#' \describe{
#'   \item{year}{year}
#'   \item{variable}{hector input variable name}
#'   \item{value}{value}
#'   \item{units}{variable units}
#' }
#' @source <https://github.com/JGCRI/gcam-core/blob/master/input/climate/gcam_emissions.csv>
"PREGCAM_EMISS_DF"


#' Hector inputs and emissions that remain constant regardless of GCAM run
#'
#' These input variables are set to some default value that does not
#' vary with GCAM run, these variables are not modeled by GCAM V8.2.
#'
#' @format ## `DEFAULT_EMISS_DF`
#' A data frame with 10,008 rows and 4 columns:
#' \describe{
#'   \item{year}{year}
#'   \item{variable}{hector input variable name}
#'   \item{value}{value}
#'   \item{units}{variable units}
#' }
#' @source <https://github.com/JGCRI/gcam-core/blob/master/input/climate/default_emissions.csv>
"DEFAULT_EMISS_DF"


#' Data frame of the GCAM variable to hector input mapping
#'
#' @format ## `EMISS_MAP_DF`
#' A data frame with 10,008 rows and 4 columns:
#' \describe{
#'   \item{ghg}{GCAM ghg name}
#'   \item{agg.gas}{GCAM label}
#'   \item{unit.conv}{conversion factor to be applied to the GCAM results}
#'   \item{hector.name}{hector variable name}
#'   \item{hector.units}{hector variable units}
#' }
"EMISS_MAP_DF"


