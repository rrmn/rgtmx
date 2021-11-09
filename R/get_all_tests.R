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


  if (is.null(page_size)) {
    stop("Please enter a page size")
  } else if (is.na(page_size)) {
    stop("Please enter a page size")
  } else if (page_size == "") {
    stop("Please enter a page size")
  } else if (!is.numeric(page_size)) {
    stop("Please enter the page size as a number")
  } else if (!length(page_size) == 1L) {
    stop("Please enter the page size as a single value")
  } else if (page_size > 500L) {
    stop("Please enter a page size of 500 or smaller")
  } else if (page_size < 1L) {
    stop("Please enter a page size of 1 or larger")
  }

  if (is.null(page_number)) {
    stop("Please enter a page number")
  } else if (is.na(page_number)) {
    stop("Please enter a page number")
  } else if (page_number == "") {
    stop("Please enter a page number")
  } else if (!is.numeric(page_number)) {
    stop("Please enter the page number as a number")
  } else if (!length(page_number) == 1L) {
    stop("Please enter the page number as a single value")
  } else if (page_number < 1L) {
    stop("Please enter a page number of 1 or larger")
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
