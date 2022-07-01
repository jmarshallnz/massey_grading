#' College of Science guidelines for the percentage of graded students who should pass a course.
#'
#' @param level the level (100, 200, 300, 700) to retrieve guidelines for
#' @return A one-row tibble with columns `min`, `max`
#' @export
cos_pass_guidelines <- function(level) {
  tibble::tribble(~paper_level, ~min, ~max,
          100, 80, 90,
          200, 85, 100,
          300, 90, 100,
          700, 90, 100) |>
    dplyr::filter(paper_level == level) |>
    dplyr::select(-paper_level)
}

#' College of Sciences guidelines for the percentage of passing students to be awarded a grade category
#'
#' @param level the level (100, 200, 300, 700) to retrieve guidelines for
#' @return A tibble with columns `gradecat`, `min`, `max`
#' @export
cos_grade_guidelines <- function(level) {
  cos_guidelines <- tibble::tribble(~paper_level, ~A_min, ~A_max, ~B_min, ~B_max,
                            100, 5, 20, 30, 50,
                            200, 5, 25, 30, 55,
                            300, 5, 30, 30, 55,
                            700, 5, 30, 30, 55) |>
    dplyr::mutate(C_min = 100-(A_max + B_max), C_max = 100-(A_min + B_min)) |>
    tidyr::pivot_longer(-paper_level, names_to=c("gradecat", "which"), values_to="value", names_sep="_") |>
    tidyr::pivot_wider(names_from=which, values_from=value)

  cos_guidelines |>
    dplyr::filter(paper_level == level) |>
    dplyr::select(-paper_level)
}
