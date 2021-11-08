#' @title Get the status of your GTmetrix account
#'
#' @description Show available credits and other meta data for the suppliad API key.
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
    url = paste0("https://gtmetrix.com/api/2.0/status"),
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
