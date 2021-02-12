# suppressing column names vision as global vars
# in order to avoid "no visible binding for global variable" package check error
globalVariables(c('STATE', 'MONTH', 'year', 'n'), 'my.far.pkg', add = TRUE)

#' Reads in data files
#'
#' @description
#' Reads data from .csv files from the US
#' National Highway Traffic Safety Administration's Fatality Analysis
#' Reporting System (FARS),
# ' https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars
#' - public yearly data, regarding fatal injuries suffered in motor vehicle traffic crashes
#' and converts them into data frame tbl
#'
#' @importFrom readr read_csv
#'
#' @param filename A character string with the name/path to file to read
#'  function will rise an error if input data file doesn't exist or path to it is incorrectly specified
#'
#' @return A data frame tbl with data readed from the csv file, or an error if the
#'   file does not exists.
#'
#' @examples
#' \dontrun{
#'   library(dplyr)
#'   library(readr)
#'   yr <- 2014
#'   data <- yr %>%
#'   make_filename %>%
#'   fars_read
#' head(data)
#' }
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Makes proper filename (path to file) given year
#'
#' @description
#' Makes input data file name using correct name template 'accident_<year>.csv.bz2'
#'
#'
#' @param year character or integer value for a year
#'  function will rise an error if no data found for specified year
#'
#' @return This function returns a string with the data file name for a given year
#'
#' @examples
#' \dontrun{
#'   make_filename(2013)
#' }
#'
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Reads FARS data in year by year
#'
#' @description
#' Function to read data given specified set of years
#'
#' @importFrom magrittr "%>%"
#'
#' @param years a list of years
#
#' @return A data frame tbl including entries in data by month, or NULL if the
#' year is not valid
#'
#' @examples
#' \dontrun{
#'   fars_read_years(2013)
#' }
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarize data by years
#'
#' @description
#' Summarizes yearly accidents data, by year, month
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by_
#' @importFrom dplyr summarize_
#' @importFrom tidyr spread_
#'
#' @importFrom magrittr "%>%"
#'
#' @param years A vector with a list of years to summarize by.
#'
#' @return A data.frame with number of accidents by years summarized by month
#'
#' @examples
#' \dontrun{
#'   fars_summarize_years(c(2013, 2014, 2015))
#' }
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Display accidents map by state and year
#'
#' @description
#' Displays accidents (using latitude and longitude coordinates provided in dataset) at states map plot
#'
#' @importFrom graphics points
#'
#' @importFrom maps map
#'
#' @param state.num An Integer with the State Code
#' in case of incorrect state number provided function will rise the error
#'
#' @param year A string, or an integer, with the year input
#'
#' @return map plot
#'
#' @examples
#' \dontrun{
#'   fars_map_state(17, 2013)
#' }
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
