#' @importFrom stats as.formula lm p.adjust pt qt
#' @importFrom dplyr filter select summarise case_when mutate across n_distinct
#' @importFrom tidyr expand_grid pivot_wider pivot_longer unnest extract
#' @importFrom tibble tribble
#' @importFrom purrr map2
#' @importFrom rsample loo_cv training testing
#' @importFrom stringr str_trim str_sub
#' @importFrom readr read_csv
#' @importFrom tidyselect contains
#' @importFrom rstudioapi askForPassword
#' @importFrom httr GET build_url parse_url content status_code
NULL

#' Imputation of all marks via leave one out prediction of each student
#' mark.
#'
#' Uses a simple linear model trained on all other students to infer a mark
#' and prediction interval for each assessment item for each student.
#'
#' The prediction interval is then used to compute a P-value for each assessment
#' item, and these are corrected to control for false discovery rate.
#'
#' @param marks A tibble of marks with columns `student_id` and then the remaining assessment items in separate columns.
#' @return A tibble with columns `student_id`, `assessement`, `mark`, `predicted`, `P`
#' @export
impute_all_marks <- function(marks) {
  fit_model <- function(split, variable) {
    dat <- rsample::training(split) |> dplyr::select(-student_id)
    formula <- as.formula(paste(variable, "~."))
    m1 <- lm(formula, data=dat)
    t_val <- qt(0.975, df=nrow(dat)-2)
    broom::augment(m1, newdata=testing(split), interval='prediction') |>
      dplyr::summarise(student_id = student_id,
                assessment = variable,
                .fitted = .fitted,
                mark = .fitted+.resid,
                .resid = .resid,
                .upper = .upper,
                t = ifelse(abs(.resid) < 1e-5, 0, .resid/(.upper-.fitted)*t_val),
                .pval = 2*pt(t, df=nrow(dat)-2, lower.tail = .resid <= 0))
  }

  variable_names <- marks |> dplyr::select(-student_id) |> names()
  splits <- rsample::loo_cv(marks)
  splits |>
    dplyr::full_join(tidyr::expand_grid(variable=variable_names), by=character()) |>
    dplyr::mutate(out = purrr::map2(splits, variable, fit_model)) |>
    tidyr::unnest(out) |>
    dplyr::mutate(P = p.adjust(.pval, method='fdr')) |>
    dplyr::select(student_id, assessment, mark, predicted=.fitted, P)
}
