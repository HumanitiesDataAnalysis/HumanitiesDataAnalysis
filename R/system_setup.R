
install_custom_markdown_template = function () {
  if (!dir.exists("~/.config/rstudio/templates")) dir.create("~/.config/rstudio/templates", recursive = TRUE)
  file.copy(system.file("extdata", "default.Rmd", package = "HumanitiesDataAnalysis"), "~/.config/rstudio/templates")
}


#' Download problem sets
#'
#' Download all problem sets into the current direct as .Rmd files.
#'
#' @return Nothing
#' @export
#'
download_problem_sets = function() {
  req <- httr::GET("https://api.github.com/repos/HumanitiesDataAnalysis/book/git/trees/master?recursive=1")
  httr::stop_for_status(req)
  filelist <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = F)
  sets = filelist[grepl("extracted_problem.*.Rmd", filelist)]
  downloads = FALSE
  lapply(sets, function(set) {
    if (!file.exists(set)) {
      downloads <<- TRUE
      url = stringr::str_glue("https://raw.githubusercontent.com/HumanitiesDataAnalysis/book/master/{set}")
      message(stringr::str_glue("Downloading {set} to current directory."))
      set = str_replace(set, ".*/", "")
      download.file(url, set, quiet = TRUE)
    }
  })
  if (downloads == FALSE) {
    message("You've downloaded all the problem sets already.")
  }
}

#' Update the Humanities Data Analysis package.
#'
#' @return Nothing
#' @export
#'
update_HDA = function(update = FALSE) {
  remotes::install_github("HumanitiesDataAnalysis/HumanitiesDataAnalysis", update=update)
}
