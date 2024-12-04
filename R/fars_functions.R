#' Read FARS csv data 
#'
#' Read a FARS csv data file and return the content as a tibble.
#'
#' @param filename Path to the FARS csv data file. If file is not a csv file, 
#' an error will occur. If file does not exists, execution will stop. 
#' 
#' @return Content of file as a tibble. 
#'
#' @examples
#' \dontrun{
#' my_data <- fars_read(filename = "path//to//my//csv//file.csv")
#' }
#' 
#' @importFrom readr read_csv
#' @importFrom tibble as_tibble
#' 
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  #dplyr::tbl_df(data)
  tibble::as_tibble(data)
}


#' Construct file name 
#'
#' Construct file name from year.
#'
#' @param year Year as string or integer. 
#' 
#' @return A FARS data file name on the form  accident_yyyy.csv.bz2
#'
#' @examples
#' \dontrun{
#' make_filename(year=2013)
#' make_filename(year="2014")
#' }
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Read multiple FARS data files 
#'
#' Apply function (\code{fars_read}) to a set of files by year references.
#'
#' @param years String vector or integer vector of year to be read. If data for 
#' one or more supplied years does not exist, execution will stop. 
#' 
#' @return List of tibbles. 
#'
#' @examples
#' \dontrun{
#' my_data <- fars_read_years(years = c(2013, 2014, 2015))
#' }
#' 
#' @importFrom readr read_csv
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr select
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


#' Summarize multiple FARS data files 
#'
#' Read and summarize FARS files by year references (group by years).
#'
#' @inheritParams fars_read_years
#' 
#' @return List of tibbles.
#'
#' @examples
#' \dontrun{
#' my_data <- fars_summarize_years(years = c(2013, 2014, 2015))
#' }
#' 
#' @importFrom readr read_csv
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread 
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Map FARS data 
#'
#' Read files by year references and map out state data for selected state.
#'
#' @param state.num String or integer vector of year to be read. If data for 
#' one or more supplied years does not exist, execution will stop.
#' @param year Year as string or integer. 
#' 
#' @return A state map with all year accidents.
#'
#' @examples
#' \dontrun{
#' fars_map_state(state.num = 1, year = 2013)
#' }
#' 
#' @importFrom readr read_csv
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @importFrom dplyr filter 
#' @importFrom maps map
#' @importFrom graphics points  
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