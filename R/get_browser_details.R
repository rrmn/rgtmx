#' @title Show browser details
#'
#' @description Get details for a specific browsers ID.
#'
#' @param browser_id Browser ID. (integer)
#' @param api_key An active GTmetrix API key. (string)
#'
#' @return A data.frame object that contains available browsers and their meta data.
#' @examples
#' \dontrun{output_table <- get_browser_details(browser_id = 3, api_key = "API_KEY")}
#' @export

get_browser_details <- function(browser_id, api_key) {

  if (missing(browser_id)) {
    stop("Please enter a browser ID")
  } else if (is.null(browser_id)) {
    stop("Please enter a browser ID")
  } else if (is.na(browser_id)) {
    stop("Please enter a browser ID")
  } else if (browser_id == "") {
    stop("Please enter a browser ID")
  } else if (!is.numeric(browser_id)) {
    stop("Please enter the browser ID as a number")
  } else if (!length(browser_id) == 1L) {
    stop("Please enter the browser ID as a single value")
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
    url = paste0("https://gtmetrix.com/api/2.0/browsers/", browser_id),
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
