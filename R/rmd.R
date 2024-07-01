#' Get the reporting R Markdown file location
#'
#' @return The location of the R markdown file included in the package (for copying etc)
#' @export
get_markdown_report <- function() {
  system.file("rmd", "report.Rmd", package="masseygrading")
}

#' Copy the reporting R Markdown file to a chosen path
#'
#' @param the path to copy to.
#' @return The location of the R markdown file included in the package (for copying etc)
#' @export
copy_markdown_report <- function(path) {
  file.copy(from=get_markdown_report(),
            to=path, overwrite=TRUE)
}
