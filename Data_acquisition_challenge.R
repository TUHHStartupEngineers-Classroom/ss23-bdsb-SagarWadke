# Challenge_1: 
## Get some data via an API.

library(tidyverse)
library(httr)
library(jsonlite)
library(dplyr)

res = GET("https://api.wazirx.com/sapi/v1/tickers/24hr")
rawToChar(res$content);
data = fromJSON(rawToChar(res$content))
names(data);
data$symbol;
table <- res %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON()
limited_data <- table[1:10,1:10] 
print(limited_data)

# Challenge_2: 
## Scrape one of the competitor websites of canyon

library(RSQLite)
library(tidyverse)
library(httr)
library(glue)
library(jsonlite)
library(rvest)
library(stringi)
library(xopen)
library(dplyr)

base_url <- 'https://www.rosebikes.com/bikes'
# 1. Function to get bike family URLs.
get_bike_family_urls <- function(base_url) {
  
  bike_family_urls <- read_html(base_url) %>%
    html_nodes(css = ".catalog-categories-item > a") %>%
    html_attr('href') %>%
    
    # Convert vector to tibble
    
    enframe(name = "position", value = "subdirectory") %>%
    # Add the domain because we will get only the subdirectories
    mutate(
      url = glue('https://www.rosebikes.com{subdirectory}')
    ) 
  
  bike_family_urls <- bike_family_urls %>% 
    filter(!grepl('sale', url)) %>%
    filter(!grepl('kids', url))
  bike_family_urls <- bike_family_urls['url']
  
}
# 2. Function to get bike family URLs.
get_model_urls <- function(url) {
  
  bike_type_url <- read_html(url) %>%
    html_nodes(css = ".catalog-category-bikes__content > a") %>%
    html_attr('href') %>%
    enframe(name = "position", value = "url") %>%
    mutate(url = glue('https://www.rosebikes.com{url}')) 
}
# 3. Function to get the names of each bike 
get_bike_names <- function(url) {
  
  bike_model_name_tbl <- read_html(url) %>%
    html_nodes(css = ".catalog-category-model__title") %>%
    html_text() %>%
    # Convert vector to tibble
    as_tibble()
  
  
}
# 4. Function to get the prices of each bike 
get_bike_prices <- function(url) {
  
  bike_model_price_tbl <- read_html(url) %>%
    html_nodes(css = ".product-tile-price__current-value") %>%
    html_text() %>%
    # Convert vector to tibble
    as_tibble()
  
}
#### APPLYING ABOVE FUNCTIONS
bike_family_url_tbl <- get_bike_family_urls(base_url)
bike_family_url_tbl <- bike_family_url_tbl %>%
  slice(1:3) # Pick 3 categories
# Create a table with bike model URLS
bike_model_url_tbl <- tibble()
for (i in seq_along(bike_family_url_tbl$url)) {
  
  web <- toString(bike_family_url_tbl$url[i])
  bike_model_url_tbl <- bind_rows(bike_model_url_tbl, get_model_urls(web))
  
}
# Create a table with bike model names
bike_model_names_tbl <- tibble()
for (i in seq_along(bike_model_url_tbl$url)) {
  
  web <- toString(bike_model_url_tbl$url[i])
  bike_model_names_tbl <- bind_rows(bike_model_names_tbl, get_bike_names(web))
  
}
# Rename cols
names(bike_model_names_tbl)[1] <- "Bike Model"
# Create a table with bike prices
bike_model_prices_tbl <- tibble()
for (i in seq_along(bike_model_url_tbl$url)) {
  web <- toString(bike_model_url_tbl$url[i])
  bike_model_prices_tbl <- bind_rows(bike_model_prices_tbl, get_bike_prices(web))
}
# Rename cols
names(bike_model_prices_tbl)[1] <- "Bike Prices"
# Join into one table
table_of_prices <- bind_cols(bike_model_names_tbl,bike_model_prices_tbl)
knitr::kable(table_of_prices[1:10, ], caption = 'Rosebike.com bicycle prices')
