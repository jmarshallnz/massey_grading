#' Authenticate credentials on the Massey system
#'
#' @param user an (optional) user name, defaults to NULL
#' @param pass an (optional) password, defaults to NULL
#' @return A `list` with values `user` and `pass`
#' @export
get_credentials <- function(user=NULL, pass=NULL) {
  if (is.null(user)) {
    user <- rstudioapi::askForPassword("Massey Username")
  }
  if (is.null(pass)) {
    pass <- rstudioapi::askForPassword("Password")
  }
  list(user = user, pass = pass)
}

#' Get URL for SMS reporting for a paper
#'
#' @seealso `read_sms_reporting`, `parse_sms_reporting`
#'
#' @param paper the paper to download
#' @param year the year to download
#' @param semester the semester to download
#' @param location the location to download, defaults to "ALL". Other values are "MTUI", "AKLI", "DISD"
#'
#' @return The URL.
#' @export
get_url_for_sms <- function(paper, year, semester, location="ALL") {
  # SMS reporting URL
  # I found the URL by downloading the .atomsvc from the "Export Data Feed" button in the viewer.

  base_url <- httr::parse_url("https://smsreporting.massey.ac.nz:443/SitsProd?%2fSmsReporting%2fExaminations%20and%20Assessments%2fGrade%20Distribution%20Reports%2fClass%20List%20with%20Marks%20and%20Grades")
  base_url$query = c(base_url$query,
                     Academic_Year = year,
                     Course = paper,
                     Course_Filter = "",
                     Location = location,
                     Semester = semester,
                     `rs:Format` = "CSV")

  url <- httr::build_url(base_url)
  url <- stringr::str_replace_all(url, "Grades=", "Grades")

  url
}

#' Download raw grades from SMS reporting
#'
#' @seealso `read_sms_reporting`, `parse_sms_reporting`
#'
#' @param paper the paper to download
#' @param year the year to download
#' @param semester the semester to download
#' @param location the location to download, defaults to "ALL". Other values are "MTUI", "AKLI", "DISD"
#' @param credentials the credentials from `get_credentials`.
#'
#' @return The raw marks.
download_raw_from_sms <- function(paper, year, semester, location="ALL", credentials) {
  # SMS reporting URL
  url <- get_url_for_sms(paper, year, semester, location)

  # fetch URL
  response <- httr::GET(url, httr::authenticate(credentials$user,
                                    credentials$pass,
                                    "ntlm"),
                        httr::verbose(data_in = TRUE, info=TRUE, ssl=TRUE))
  if (httr::status_code(response) == 200) {
    httr::content(response, show_col_types=FALSE)
  } else {
    print(response)
    data.frame()
  }
}

#' Download grades from SMS reporting
#'
#' @seealso `read_sms_reporting`, `parse_sms_reporting`
#'
#' @param paper the paper to download
#' @param year the year to download
#' @param semester the semester to download
#' @param location the location to download, defaults to "ALL". Other values are "MTUI", "AKLI", "DISD"
#' @param credentials the credentials from `get_credentials`.
#'
#' @return A tibble with columns `paper`, `mode`, `year`, `semester`,
#' `number`, `type`, `weight`, `student_id`
#' `surname`, `firstname`, `mark`, `grade`, `agreed_mark`, `agreed_grade`,
#' `overall_mark`, `overall_grade`, `agreed_overall_mark`, `agreed_overall_grade`,
#' `minutes`.
#' @export
download_from_sms <- function(paper, year, semester, location="ALL", credentials) {
  raw_sms <- download_raw_from_sms(paper, year, semester, location, credentials)
  parse_raw_sms_data(raw_sms)
}
