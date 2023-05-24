#Challenge Data acquisition

library(RSQLite)
library(dplyr)
library(httr)
library(jsonlite)
library(xml2)

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
