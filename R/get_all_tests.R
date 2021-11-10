#' @title get_all_tests
#'
#' @description Get a table of tests, their report IDs and other meta data.
#'
#' @param api_key An active GTmetrix API key. (string)
#' @param page_size Page size (default 50, max 500)
#' @param page_number Page (default 1)
#'
#' @return A data.frame object that contains test IDs and their meta data.
#' @examples
#' \dontrun{output_table <- get_all_tests(api_key = "API_KEY")}
#' @export

get_all_tests <- function(api_key, page_size = 50, page_number = 1) {

  check_input(input = api_key, input_type = "character",
              variable_name = "api_key",
              is_missing = missing(api_key))

  check_input(input = page_number, input_type = "numeric",
              variable_name = "page_number", min_value = 1L, max_value = 500L)

  check_input(input = page_number, input_type = "numeric",
              variable_name = "page_number", min_value = 1L)


  res <- httr::GET(
    url = paste0("https://gtmetrix.com/api/2.0/tests", "?page[number]=", page_number, "&page[size]=", page_size),
    httr::authenticate(api_key, ""),
    httr::content_type("application/vnd.api+json")
  )

  # Throw exception if there's an error
  if (httr::status_code(res) != 200) {
    error <- jsonlite::fromJSON(rawToChar(res$content))$error
    stop(paste0(error$title, ifelse(!is.null(error$detail), paste0(" - ", error$detail), ""), " (", error$code, ")"))
  }

  data_raw <- jsonlite::fromJSON(rawToChar(res$content))
  # has to stay stringsAsFactors = FALSE for R < v4.0
  data <- as.data.frame(data_raw$data, stringsAsFactors = FALSE)
  rm(data_raw)

  return(data)

}
