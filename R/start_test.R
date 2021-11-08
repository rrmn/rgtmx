#' @title Start a GTmetrix test
#'
#' @description \code{start_test} starts a GTmetrix test and returns either the test itself (incl. meta data) or the associated report.
#'
#' @param url The URL of the page to test. (string)
#' @param api_key An active GTmetrix API key (string)
#' @param wait_for_completion Whether the function should wait for the completion of the test. If TRUE (default), the report associated with the test ID will be requested in roughly 3 second intervals and returned, when successful. If FALSE, the meta data of the test will be returned. (TRUE, FALSE)
#' @param location Location ID. Default = "1"
#' @param browser Location ID. Default = "3"
#' @param report A string for the type of report. "lighthouse" (default) for Lighthouse, "legacy" for PageSpeed/YSlow, "lighthouse,legacy" for both, "none" for a metrics-only report.
#' @param retention Choose how long (in months) the report will be retained and accessible. Valid values: 1 (default), 6, 12, 24.
#' @param httpauth_username Username for the test page HTTP access authentication. (string)
#' @param httpauth_password Password for the test page HTTP access authentication. (string)
#' @param adblock Enable AdBlock. 1 (default) = yes, 0 = no.
#' @param cookies Specify cookies to supply with test page requests.
#' @param video Enable generation of video. 0 (default) = no, 1 = yes
#' @param stop_onload Stop the test at window.onload instead of after the page has fully loaded (ie. 2 seconds of network inactivity).	0 (default) = no, 1 = yes
#' @param throttle Throttle the connection. Speed measured in Kbps, latency in ms. Format: "up/down/latency"
#' @param allow_url Only load resources that match one of the URLs on this list. This uses the same syntax as the web front end.
#' @param block_url Prevent loading of resources that match one of the URLs on this list. This occurs after the Only Allow URLs are applied. This uses the same syntax as the web front end.
#' @param dns Use a custom DNS host and IP to run the test with.
#' @param simulate_device Simulate the display of your site on a variety of devices using a pre-selected combination of Screen Resolutions, User Agents, and Device Pixel Ratios. (Expected: Device ID)
#' @param user_agent Use a custom User Agent string.
#' @param browser_width Set the width of the viewport for the analysis. Also requires browser_height to be set.
#' @param browser_height Set the height of the viewport for the analysis. Also requires browser_width to be set.
#' @param browser_dppx Set the device pixel ratio for the analysis. Decimals are allowed.
#' @param browser_rotate Swaps the width and height of the viewport for the analysis. \code{simulate_device} overrides this parameter with preset values.
#'
#' @return A data.frame object that contains either the test meta data or the GTmetrix report.
#' @examples
#' \dontrun{output_table <- start_test(
#'                            url = "google.com",
#'                            api_key = "API_KEY",
#'                            wait_for_completion = TRUE
#'                          )}
#' @export

start_test <- function(url, api_key, wait_for_completion = TRUE, location = 1,
                       browser = 3, report = "lighthouse", retention = 1, httpauth_username = NULL, httpauth_password = NULL, adblock = 1, cookies = NULL,
                       video = 0, stop_onload = 0, throttle = NULL,
                       allow_url = NULL, block_url = NULL, dns = NULL,
                       simulate_device = NULL, user_agent = NULL,
                       browser_width = NULL, browser_height = NULL,
                       browser_dppx = NULL, browser_rotate = NULL) {

  if (missing(url)) {
    stop("Please enter a URL")
  } else if (is.null(url)) {
    stop("Please enter a URL")
  } else if (is.na(url)) {
    stop("Please enter a URL")
  } else if (url == "") {
    stop("Please enter a URL")
  } else if (!is.character(url)) {
    stop("Please enter the URL as a character string")
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

  attributes <- list(
    "url" = url
  )
  if (!missing(location)) {
    attributes <- append(attributes, c("location" = location))
  }
  if (!missing(browser)) {
    attributes <- append(attributes, c("browser" = browser))
  }
  if (!missing(report)) {
    attributes <- append(attributes, c("report" = report))
  }
  if (!missing(retention)) {
    attributes <- append(attributes, c("retention" = retention))
  }
  if (!missing(httpauth_username)) {
    attributes <- append(attributes, c("httpauth_username" = httpauth_username))
  }
  if (!missing(httpauth_password)) {
    attributes <- append(attributes, c("httpauth_password" = httpauth_password))
  }
  if (!missing(adblock)) {
    attributes <- append(attributes, c("adblock" = adblock))
  }
  if (!missing(cookies)) {
    attributes <- append(attributes, c("cookies" = cookies))
  }
  if (!missing(video)) {
    attributes <- append(attributes, c("video" = video))
  }
  if (!missing(stop_onload)) {
    attributes <- append(attributes, c("stop_onload" = stop_onload))
  }
  if (!missing(throttle)) {
    attributes <- append(attributes, c("throttle" = throttle))
  }
  if (!missing(allow_url)) {
    attributes <- append(attributes, c("allow_url" = allow_url))
  }
  if (!missing(block_url)) {
    attributes <- append(attributes, c("block_url" = block_url))
  }
  if (!missing(dns)) {
    attributes <- append(attributes, c("dns" = dns))
  }
  if (!missing(simulate_device)) {
    attributes <- append(attributes, c("simulate_device" = simulate_device))
  }
  if (!missing(user_agent)) {
    attributes <- append(attributes, c("user_agent" = user_agent))
  }
  if (!missing(browser_width)) {
    attributes <- append(attributes, c("browser_width" = browser_width))
  }
  if (!missing(browser_height)) {
    attributes <- append(attributes, c("browser_height" = browser_height))
  }
  if (!missing(browser_dppx)) {
    attributes <- append(attributes, c("browser_dppx" = browser_dppx))
  }
  if (!missing(browser_rotate)) {
    attributes <- append(attributes, c("browser_rotate" = browser_rotate))
  }

  res  <- httr::POST(
    url = "https://gtmetrix.com/api/2.0/tests",
    httr::authenticate(api_key, ""),
    httr::content_type("application/vnd.api+json"),
    body = jsonlite::toJSON(
      list(
        "data" = list(
          "type" = "test",
          "attributes" = attributes
        )
      ),
      auto_unbox = TRUE
    ),
    encode = "raw"
  )

  # Throw exception if there's an error
  if (httr::status_code(res) != 202) {
    error <- jsonlite::fromJSON(rawToChar(res$content))$error
    stop(paste0(error$title, ifelse(!is.null(error$detail), paste0(" - ", error$detail), ""), " (", error$code, ")"))
  }

  data_raw <- jsonlite::fromJSON(rawToChar(res$content))
  meta <- data_raw$meta
  data <- as.data.frame(data_raw$data)
  rm(data_raw)

  message(paste0("Credits Used (Credits Left): ", meta$credits_used, " (", meta$credits_left, ")"))

  if (wait_for_completion == TRUE) {

    get_test(data$id, api_key, wait_for_completion = TRUE)

  }

  return(data)

}
