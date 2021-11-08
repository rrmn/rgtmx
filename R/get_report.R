#' @title Get status and meta data of a specific report
#'
#' @description Get status and meta data of a specific GTmetrix report.
#'
#' @param report_id ID of a GTmetrix report. (string)
#' @param api_key An active GTmetrix API key. (string)
#'
#' @return  A data.frame object that contains a GTmetrix report and its meta data.
#' @examples
#' \dontrun{output_table <- get_report(
#'                            test_id = "REPORT_ID",
#'                            api_key = "API_KEY"
#'                          )}
#' @export

get_report <- function(report_id, api_key) {

  if (missing(report_id)) {
    stop("Please enter a report ID")
  } else if (is.null(report_id)) {
    stop("Please enter a report ID")
  } else if (is.na(report_id)) {
    stop("Please enter a report ID")
  } else if (report_id == "") {
    stop("Please enter a report ID")
  } else if (!is.character(report_id)) {
    stop("Please enter the report ID as a character string")
  }

  if (missing(api_key)) {
    stop("Please enter a API key")
  } else if (is.null(api_key)) {
    stop("Please enter a API key")
  } else if (is.na(api_key)) {
    stop("Please enter a API key")
  } else if (api_key == "") {
    stop("Please enter a API key")
  } else if (!is.character(api_key)) {
    stop("Please enter the API key as a character string")
  }


  res <- httr::GET(
    url = paste0("https://gtmetrix.com/api/2.0/reports/", report_id),
    httr::authenticate(api_key, ""),
    httr::content_type("application/vnd.api+json")
  )

  # Throw exception if there's an error
  if (httr::status_code(res) != 200) {
    error <- jsonlite::fromJSON(rawToChar(res$content))$error
    stop(paste0(error$title, ifelse(!is.null(error$detail), paste0(" - ", error$detail), ""), " (", error$code, ")"))
  }

  data <- as.data.frame(jsonlite::fromJSON(rawToChar(res$content))$data)

  return(data)

}
