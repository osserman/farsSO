#' Print "Fars Read"
#'
#' This is a simple function that takes a filename (provided by user
#' with \code{filename} argument), checks if file exists and if so reads
#' the files contents as a csv and outputs the data as a tbl. Errors could
#' result from the file being a format the cannot be read by readr::read_csv
#'
#' @param filename A character string giving the name of the file to be read
#'
#' @return This function returns the data in user-provided \code{filename}
#'    as a tbl.
#'
#' @examples
#' \dontrun{
#' fars_read('myfile.csv')
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Print "Make Filename"
#'
#' This is a simple function that takes a year (provided by user
#' with \code{year} argument), and returns a character string filename
#' of the format "accident_[YEAR].csv.bz2". Errors could result from
#' the year being given in a format that cannot be coerced to integer.
#'
#' @param year A string or numeric year.
#'
#' @return This function returns a string of the file name for the
#'    given year.
#'
#' @examples
#' \dontrun{
#' make_filename('2013')
#' }
#'
#' @export

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Print "Fars Read Years"
#'
#' This is a function that takes a vector or list of years (provided by user
#' with \code{years} argument), and outputs a list of years each containing a
#' dataframe of MONTHS and year, for all rows in the file with the input year.
#'
#' Errors could come from years being improperly formatted.
#'
#' @param years A list (or vector) of years
#'
#' @return a list with items for each inputed year, with month and year
#' from the file for all years with corresponding files and NULLs for all
#' years without files.
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' fars_read_years(list('2013','2015'))
#' }
#'
#' @export

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

#' Print "Fars Summarize Years"
#'
#' This is a function that takes a vector or list of years (provided by user
#' with \code{years} argument), and outputs a dataframe with columns for each
#' year and rows for each month containing counts of rows (accidents) in each
#' year's files for each month.
#'
#' Errors could come from years being improperly formatted.
#'
#' @param years A list (or vector) of years
#'
#' @return dataframe with columns for each user inputed year that had an associated
#' file, and rows (accidents) for each MONTH, containing counts of rows for each month/year
#' across files.
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(list('2013','2015'))
#' }
#'
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Print "Fars Map State"
#'
#' This is a function that takes \code{state.num} and \code{year} arguments
#' and maps the locations of rows (accidents) within the referenced state.
#'
#' Errors could come from years being improperly formatted or state.num not
#' being coercible to integer.
#'
#' @param state.num integer (or integer-coercible) for state.
#' @param year integer (or integer-coercible) 4-digit year.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{
#' fars_map_state(13, 2013)
#' }
#'
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

