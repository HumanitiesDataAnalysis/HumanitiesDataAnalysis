
#' Return a database connection to a 2020 dump of the Pleiades gazetteer of the
#' ancient world.
#'
#' Source: Roger Bagnall, et al. (eds.), Pleiades: A Gazetteer of Past Places, 2016, <http://pleiades.stoa.org/> [Accessed: July 26, 2016 8:09 pm].
#'
#' @return A database connection
#' @export
#'
#' @examples
#'
#' pleaides_db %>% tbl("names") %>% filter(title == "Jerusalem")
#'
pleiades_db = function() {
  RSQLite::dbConnect(RSQLite::SQLite(), system.file("extdata/pleiades.sqlite3", package="HumanitiesDataAnalysis"))
}


#' Get State of the union addresses
#'
#' @return
#' @export
#'
#' @examples
#' get_recent_SOTUs() %>% filter(president == "Ronald Reagan")
get_recent_SOTUs = function(min_year = 1981) {
  extract_SOTUs()
  list.files("SOTUS", full.names = TRUE) %>%
    keep(~.x %>% str_extract("[0-9]{4}") %>% as.numeric() >= min_year) %>%
    map_dfr(read_tokens) %>%
    mutate(year = as.numeric(filename)) %>%
    inner_join(presidents)
}
#' Extract State of the Union addresses into a local directory.
#'
#' @return
#' @export
#'
#' @examples
#'
extract_SOTUs = function() {
  f = system.file("extdata/SOTUS.zip", package="HumanitiesDataAnalysis")
  suppressWarnings(unzip(f, exdir = ".", overwrite = FALSE))
}
