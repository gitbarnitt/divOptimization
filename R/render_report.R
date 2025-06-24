# R/render_report.R

render_report <- function() {
  output_file <- "outputs/detection_report.html"
  dir.create("outputs", showWarnings = FALSE)
  rmarkdown::render("detection_report.Rmd", output_file = output_file, quiet = TRUE)
  return(output_file)
}
