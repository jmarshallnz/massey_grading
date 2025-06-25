#' Converts a grade character string to a factor
#' @param grades the grades as a character vector
#' @return a factor variable ordered appropriately
#' @export
grade_factor <- function(grades) {
  level_list <- c("A+", "A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D", "E", "MA", "MC", "F", "P", "DC", "AG", "WD", "WS", "NF")
  if (!all(grades %in% level_list)) {
    stop(paste("We have grades that aren't known:", setdiff(grades, level_list), "\n"))
  }
  factor(grades, levels=level_list)
}

#' Computes the grade category (A, B, C, F, DC, NF) for a vector of grades
#' @param grades the grades as a character vector
#' @return a factor variable with grade categories
#' @export
grade_category <- function(grades) {
  grade_cat <- dplyr::case_when(
                        grades == "NF" ~ "NF",
                        grades %in% c("MA", "MC", "DC", "WD", "WS") ~ "DC",
                        grades %in% c("D", "E") ~ "F",
                        grades %in% c("AG", "P") ~ "C",
                        TRUE ~ stringr::str_sub(grades, 1, 1)
                        )
  factor(grade_cat, levels = c("A", "B", "C", "F", "DC", "NF"))
}

#' Round a mark to the nearest integer
#'
#' The standard `round()` function rounds to nearest even. This routine
#' rounds to nearest integer regardless of even or odd (i.e. should be consistent)
#' with SMS
#'
#' @param marks A vector of marks to round
#' @return The rounded marks
#' @export
round_mark <- function(marks) {
  trunc(marks + sign(marks) * 0.5)
}
