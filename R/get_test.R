#' @title Get status and meta data of a specific test
#'
#' @description Get the status and meta data of a specific GTmetrix test. Returns the associated report instead, if the report is already completed.
#'
#' @param test_id ID of a GTmetrix test. (string)
#' @param api_key An active GTmetrix API key. (string)
#' @param wait_for_completion Whether the function should wait for the completion of the test. If TRUE (default), the report associated with the test ID will be requested in roughly 3 second intervals and returned, when successful. If FALSE, the meta data of the test will be returned. (TRUE, FALSE)
#'
#' @return  A data.frame object that contains either the test meta data or the GTmetrix report (if it's already completed)
#' @examples
#' \dontrun{output_table <- get_test(
#'                            test_id = "TEST_ID",
#'                            api_key = "API_KEY"
#'                          )}
#' @export

get_test <- function(test_id, api_key, wait_for_completion = TRUE) {

  if (missing(test_id)) {
    stop("Please enter a test ID")
  } else if (is.null(test_id)) {
    stop("Please enter a test ID")
  } else if (is.na(test_id)) {
    stop("Please enter a test ID")
  } else if (test_id == "") {
    stop("Please enter a test ID")
  } else if (!is.character(test_id)) {
    stop("Please enter the test ID as a character string")
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
    url = paste0("https://gtmetrix.com/api/2.0/tests/", test_id),
    httr::authenticate(api_key, ""),
    httr::content_type("application/vnd.api+json")
  )

  # Throw exception if there's an error
  if (httr::status_code(res) != 200) {
    error <- jsonlite::fromJSON(rawToChar(res$content))$error
    stop(paste0(error$title, ifelse(!is.null(error$detail), paste0(" - ", error$detail), ""), " (", error$code, ")"))
  }

  data <- as.data.frame(jsonlite::fromJSON(rawToChar(res$content))$data)


  if (wait_for_completion == TRUE) {

    type <- data$type

    while (type == "test") {

      timeout <- round(stats::runif(1, 3, 4), 2)
      message(paste0("Report not generated, yet. Retrying in ", timeout, " seconds..."))
      Sys.sleep(timeout)

      data <- get_test(test_id, api_key)

      type <- data$type

    }

  }

  return(data)

}
