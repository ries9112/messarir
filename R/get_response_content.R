#'@importFrom httr content
#'@importFrom jsonlite fromJSON
#'@export
get_response_content <- function(api_response) {
  httr::content(api_response,
                type = "text",
                encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyDataFrame = FALSE)
}
