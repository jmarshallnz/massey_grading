#' Read .csv file from SMS reporting
#'
#' All grades are converted to factors.
#'
#' @seealso `extract_paper_information`
#'
#' @param path the path to the .csv file.
#' @return A tibble with columns `paper`, `mode`, `year`, `semester`,
#' `number`, `type`, `weight`, `student_id`
#' `surname`, `firstname`, `mark`, `grade`, `agreed_mark`, `agreed_grade`,
#' `overall_mark`, `overall_grade`, `agreed_overall_mark`, `agreed_overall_grade`,
#' `minutes`.
#' @export
read_sms_reporting_csv <- function(path) {
  raw_sms <- readr::read_csv(path)

  raw_sms |> dplyr::select(paper = Textbox107,
                     assessment = Assesment_Type_Label2,
                     student_id = Student_Number,
                     surname = Student_Surname,
                     firstname = Student_Forename,
                     mark = Student_Assesment_Score1,
                     grade = Textbox15,
                     agreed_mark = Student_Assesment_Score3,
                     agreed_grade = Textbox105,
                     overall_mark = Student_Paper_Actual_Mark,
                     overall_grade = Student_Paper_Actual_Grade,
                     agreed_overall_mark = Student_Paper_Mark,
                     agreed_overall_grade = Student_Paper_Grade,
                     minutes = Notes) |>
    tidyr::extract(assessment,
            into=c("number", "type", "weight"),
            regex = "\\(([0-9]+)\\) (.*?) \\(([0-9]+).*",
            convert = TRUE) |>
    tidyr::extract(paper,
            into=c("paper", "mode", "year", "semester"),
            regex="(.*) ?(AKLB|AKLI|DISD|MTUI) ([0-9]+) (.*)") |>
    dplyr::mutate(paper = stringr::str_trim(paper)) |>
    dplyr::mutate(dplyr::across(tidyselect::contains('grade'), grade_factor))
}

#' Extract paper information from SMS data read using `read_sms_reporting_csv()`
#'
#' @seealso `read_sms_reporting_csv`
#'
#' @param data the data read via `read_sms_reporting_csv()`.
#' @return A list with entries `paper`, `year`, `semester`, `level`, `students`.
#' @export
extract_paper_information <- function(data) {
  year_level <- data |>
    dplyr::pull(paper) |>
    unique() |>
    stringr::str_sub(start=4, end=4) |>
    as.numeric()

  info <- data |>
    select(paper, year, semester) |>
    unique() |>
    as.list()

  info$level = year_level * 100
  info$students = sms |>
    dplyr::summarise(n=dplyr::n_distinct(student_id)) |>
    dplyr::pull(n)

  info
}
