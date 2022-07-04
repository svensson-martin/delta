#' Find whether the beach is in US or UK
#'
#' @param dat A data frame listing beaches
#'
#' @return A data frame with a column indicating US or UK
#' @export
localize_beach <- function(dat) {
  dplyr::left_join(dat, lookup_table)
}


f_to_c <- function(x) (x - 32) * 5 / 9

#' Modifies temp to Celcius if country is US
#'
#' @param dat A data frame with countries and temperatures
#'
#' @return A data frame where the temperatures is in Farenheit
#' @export
celsify_temp <- function(dat) {
  dplyr::mutate(dat, temp = dplyr::if_else(english == "US", f_to_c(temp), temp))
}

timestamp <- function(time = Sys.time()) {
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")
  format(time, "%Y-%B-%d_%H-%M-%S")
}

#' Adds a time stamp on a file
#'
#' @param infile A csv file
#'
#' @return A csv file with time stamp in title
#' @export
outfile_path <- function(infile) {
  ts <- timestamp(Sys.time())
  paste0(ts, "_", sub("(.*)([.]csv$)", "\\1_clean\\2", infile))
}
