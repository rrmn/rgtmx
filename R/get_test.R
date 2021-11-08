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
