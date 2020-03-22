
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
