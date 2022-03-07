
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
  if (!file.exists("pleaides.duckdb")) {
    for (fname in c("locations", "places", "names")) {
      download.file(paste0("https://benschmidt.org/files/parquet/pleiades_", fname, ".parquet"), paste0(fname, ".parquet"))
    }
    con = DBI::dbConnect(duckdb::duckdb(), dbdir="pleaides.duckdb", read_only=FALSE)
    DBI::dbExecute(con, "CREATE TABLE names AS SELECT * FROM 'names.parquet'")
    DBI::dbExecute(con, "CREATE TABLE places AS SELECT * FROM 'places.parquet'")
    DBI::dbExecute(con, "CREATE TABLE locations AS SELECT * FROM 'locations.parquet'")
    DBI::dbDisconnect(con)
    file.remove("names.parquet")
    file.remove("places.parquet")
    file.remove("locations.parquet")
  }
  con = DBI::dbConnect(duckdb::duckdb(), dbdir="pleaides.duckdb", read_only=TRUE)
  return(con)
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
