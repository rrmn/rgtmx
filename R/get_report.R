#' @title Get status and meta data of a specific report
#'
#' @description Get status and meta data of a specific GTmetrix report.
#'
#' @param report_id ID of a GTmetrix report. (string)
#' @param api_key An active GTmetrix API key. (string)
#'
#' @return  A data.frame object that contains a GTmetrix report and its
#' meta data.
#' @examples
#' \dontrun{output_table <- get_report(
#'                            test_id = "REPORT_ID",
#'                            api_key = "API_KEY"
#'                          )}
#' @export

get_report <- function(report_id, api_key) {

  check_input(input = report_id, input_type = "character",
              variable_name = "report_id",
              is_missing = missing(report_id))

  check_input(input = api_key, input_type = "character",
              variable_name = "api_key",
              is_missing = missing(api_key))

  res <- httr::GET(
    url = paste0("https://gtmetrix.com/api/2.0/reports/", report_id),
    httr::authenticate(api_key, ""),
    httr::content_type("application/vnd.api+json")
  )

  # Throw exception if there's an error
  if (httr::status_code(res) != 200) {
    error <- jsonlite::fromJSON(rawToChar(res$content))$error
    stop(paste0(error$title, ifelse(
      !is.null(error$detail),
      paste0(" - ", error$detail), ""), " (", error$code, ")"))
  }

  data_raw <- jsonlite::fromJSON(rawToChar(res$content))
  # has to stay stringsAsFactors = FALSE for R < v4.0
  data <- as.data.frame(data_raw$data, stringsAsFactors = FALSE)
  rm(data_raw)

  return(data)

}
