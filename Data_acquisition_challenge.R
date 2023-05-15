#Challenge Data acquisition

library(RSQLite)
library(dplyr)
library(httr)
library(jsonlite)
library(xml2)

url <- "https://restcountries.com/v3.1/all"
response <- GET(url)

headers <- headers(response)
print(headers[["content-type"]])

data <- content(response, as = "text")

df <- fromJSON(data)  
  
View(df)

#Challenege Data acqusition 2

library(rvest)

#URL to scrape
url <- "https://www.radon-bikes.de/mountainbike/hardtail/"

#read the HTML content
webpage <- read_html(url)

#Extract the required data
product_names <- webpage %>%
  html_nodes(".bikeTitle") %>%
  html_text()

prices <- webpage %>%
  html_nodes(".currentPrice") %>%
  html_text()

# Create a data frame with the extracted data
data <- data.frame(ProductName = product_names, Price = prices)

# Print the data frame
print(data)
