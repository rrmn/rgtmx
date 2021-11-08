#' @title Show location details
#'
#' @description Get details for a specific locations ID.
#'
#' @param location_id Location ID. (integer)
#' @param api_key An active GTmetrix API key. (string)
#'
#' @return A data.frame object that contains available locations and their meta data.
#' @examples
#' \dontrun{output_table <- get_location_details(location_id = 3, api_key = "API_KEY")}
#' @export

get_location_details <- function(location_id, api_key) {

  if (missing(location_id)) {
    stop("Please enter a location ID")
  } else if (is.null(location_id)) {
    stop("Please enter a location ID")
  } else if (is.na(location_id)) {
    stop("Please enter a location ID")
  } else if (location_id == "") {
    stop("Please enter a location ID")
  } else if (!is.numeric(location_id)) {
    stop("Please enter the location ID as a number")
  } else if (!length(location_id) == 1L) {
    stop("Please enter the location ID as a single value")
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
    url = paste0("https://gtmetrix.com/api/2.0/locations/", location_id),
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
