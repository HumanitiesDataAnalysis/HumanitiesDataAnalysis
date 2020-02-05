
install_custom_markdown_template = function () {
  if (!dir.exists("~/.config/rstudio/templates")) dir.create("~/.config/rstudio/templates", recursive = TRUE)
  file.copy(system.file("extdata", "default.Rmd", package = "HumanitiesDataAnalysis"), "~/.config/rstudio/templates")
}
