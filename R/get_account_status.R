#' @title Get the status of your GTmetrix account
#'
#' @description Show available credits and other meta data for the
#' supplied API key.
#'
#' @param api_key An active GTmetrix API key. (string)
#'
#' @return  A data.frame that contains meta data of a GTmetrix account.
#' @examples
#' \dontrun{output_table <- get_account_status(
#'                            api_key = "API_KEY"
#'                          )}
#' @export

get_account_status <- function(api_key) {

  check_input(input = api_key, input_type = "character",
              variable_name = "api_key",
              is_missing = missing(api_key))

  res <- httr::GET(
    url = paste0("https://gtmetrix.com/api/2.0/status"),
    httr::authenticate(api_key, ""),
    httr::content_type("application/vnd.api+json")
  )

  # Throw exception if there's an error
  if (httr::status_code(res) != 200) {
    error <- jsonlite::fromJSON(rawToChar(res$content))$error
    stop(paste0(error$title, ifelse(
      !is.null(error$detail),
      paste0(" - ", error$detail), ""),
      " (", error$code, ")"))
  }

  data_raw <- jsonlite::fromJSON(rawToChar(res$content))
  # has to stay stringsAsFactors = FALSE for R < v4.0
  data <- as.data.frame(data_raw$data, stringsAsFactors = FALSE)
  rm(data_raw)

  return(data)

}
