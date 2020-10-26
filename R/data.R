#' Example data with proper names from two different sources
#'
#' @format A data.frame with 6 rows and 2 variables, both of class character:
#' \describe{
#' \item{name_source1}{name from source 1}
#' \item{name_source2}{name from source 2}
#' }
"names_ex"


#' Example hospital datasets containing proper names in different formats
#'
#' @description Example hospital datasets, one from an in-patient department
#' (`dat_ipd`), and the other from an ICU department (`dat_icu`). The datasets
#' contain some common patients but with variation in how the names are written.
#' Note these data are fake -- the patient names are simply random combinations
#' of common French names.
#'
#' @name dat_hospital
#' @aliases dat_ipd dat_icu
#'
#' @format
#' Data frames each with two columns:
#'
#' \describe{
#' \item{name_ipd/name_icu}{patient name}
#' \item{date_ipd/date_icu}{date of entry to given department}
#' }
NULL

