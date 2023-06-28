#' Converts a grade character string to a factor
#' @param grades the grades as a character vector
#' @return a factor variable ordered appropriately
#' @export
grade_factor <- function(grades) {
  factor(grades, levels=c("A+", "A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D", "E", "MA", "F", "DC", "AG", "WD", "NF"))
}

#' Computes the grade category (A, B, C, F, DC, NF) for a vector of grades
#' @param grades the grades as a character vector
#' @return a factor variable with grade categories
#' @export
grade_category <- function(grades) {
  grade_cat <- dplyr::case_when(
                        grades == "NF" ~ "NF",
                        grades %in% c("MA", "DC", "WD") ~ "DC",
                        grades %in% c("D", "E") ~ "F",
                        grades == "AG" ~ "C",
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
