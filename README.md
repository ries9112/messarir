# messarir
R wrapper to the [Messari API for cryptocurrency data](https://messari.io/api/)


## To-do:

get_response_content <- function(api_response) {
  httr::content(api_response,
                type = "text",
                encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyDataFrame = FALSE)
}

get_response_content(httr::GET("https://data.messari.io/api/v1/assets/btc/metrics"))

1. Make function that adjusts `btc` to be chosen by the user


2. Function for all assets (top 20): https://data.messari.io/api/v1/assets
    Also option to be more specific as parameter: https://data.messari.io/api/v1/assets?with-metrics
    
    
3. Function for markets: https://data.messari.io/api/v1/markets


4. Functions for news: https://data.messari.io/api/v1/news
    Also option by asset: https://data.messari.io/api/v1/news/btc
