# messarir
R wrapper to the [Messari API for cryptocurrency data](https://messari.io/api/)

This is a non-official utility made by a user with no association to [Messari](https://messari.io).

In order to use this package, you will need to make an account on the website [Messari.io](https://messari.io/). Then, you will be able to create an API key [at the following link: https://messari.io/account/api](https://messari.io/account/api):

![](/images/api_key_page.png)

Free accounts are limited to 20 requests per minute. For heavier usage, you will need a [**Messari Pro Subscription.** Click on this link to make a subscription](https://messari.io/pricing). 

Once you have an API key, you can use it to pull cryptocurrency data using this package. (Update according to to-do below)

## To-do:

1. Make function that adjusts `btc` to be chosen by the user


2. Function for all assets (top 20): https://data.messari.io/api/v1/assets
    Also option to be more specific as parameter: https://data.messari.io/api/v1/assets?with-metrics
    
    
3. Function for markets: https://data.messari.io/api/v1/markets


4. Functions for news: https://data.messari.io/api/v1/news
    Also option by asset: https://data.messari.io/api/v1/news/btc
    
5. Functions for historical data


6. Write all documentation + usage and examples


7. **Add instructions on storing the data using googlesheets**


8. Provide templates for using code from bookdown tutorial on data collected using googlesheets instructions/template


9. Write automated tests using testthat to check each field has at least 1 valid result and create code coverage
