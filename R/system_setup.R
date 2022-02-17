
install_custom_markdown_template = function () {
  if (!dir.exists("~/.config/rstudio/templates")) dir.create("~/.config/rstudio/templates", recursive = TRUE)
  file.copy(system.file("extdata", "default.Rmd", package = "HumanitiesDataAnalysis"), "~/.config/rstudio/templates")
}

#' Code placeholder
#'
#' A placeholder in problem sets that swallows up data.
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' KEEP_GOING("This is discarded")
#'
KEEP_GOING = function(...) {
  if (interactive()) {
    message("You need to replace the part that says KEEP_GOING with more code!")
  }
}

#' Download problem sets
#'
#' Download all problem sets into the current direct as .Rmd files.
#'
#' @return Nothing
#' @export
#'
download_problem_sets = function() {
  req <- httr::GET("https://hdf.benschmidt.org/R/toc.json?min=1")
  httr::stop_for_status(req)
  cont = httr::content(req) |> purrr::map_dfr(tibble::as_tibble) |> dplyr::filter(level==1)
  cont$number = 1:nrow(cont)
  print(cont |> dplyr::select(number, title))
  name <- readline(prompt=stringr::str_glue("Enter the NUMBER to download to the current folder: "))
  f = as.numeric(name)
  if (is.na(f)) {stop("You must supply the number next to the filename")}
  slug = cont$href[f] |> stringr::str_sub(2)
  path = stringr::str_glue("{slug}_exercises.Rmd")
  remote = stringr::str_glue("https://hdf.benschmidt.org/R/{slug}/exercises.Rmd")
  message(c(path, remote))
  if (file.exists(path)) {
    stop("You already have a file at '", path, "'. Please move or rename it.")
  }
  download.file(remote, path, mode="wb")
}

#' Update the Humanities Data Analysis package.
#'
#' @return Nothing
#' @export
#'
update_HDA = function(update = FALSE) {
  remotes::install_github("HumanitiesDataAnalysis/HumanitiesDataAnalysis", update=update)
}
