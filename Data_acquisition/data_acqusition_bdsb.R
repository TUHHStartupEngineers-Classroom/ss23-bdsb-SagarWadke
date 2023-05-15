
#Library

library(RSQLite)
library(dplyr)

con <- RSQLite::dbConnect(drv    = SQLite(), 
                          dbname = "C:\\Users\\Sagar\\Documents\\GitHub\\ss23-bdsb-SagarWadke\\Data_acquisition\\Chinook_Sqlite.sqlite")

dbListTables(con)

tbl(con, "Album")

album_tbl <- tbl(con, "Album") %>% collect()

dbDisconnect(con)
con

library(glue)
name <- "Fred"
glue('My name is {name}.')


library(httr)
resp <- GET("https://swapi.dev/api/people/1/")

# Wrapped into a function
sw_api <- function(path) {
  url <- modify_url(url = "https://swapi.dev", path = glue("/api{path}"))
  resp <- GET(url)
  stop_for_status(resp) # automatically throws an error if a request did not succeed
}

resp <- sw_api("/people/1")
resp

rawToChar(resp$content)

data_list <- list(strings= c("string1", "string2"), 
                  numbers = c(1,2,3), 
                  TRUE, 
                  100.23, 
                  tibble(
                    A = c(1,2), 
                    B = c("x", "y")
                  )
)

library(jsonlite)
resp %>%
  .$content %>%
  rawToChar() %>%
  fromJSON()

content(resp, as = "text")


resp <- GET('https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=WDI.DE')
resp

token    <- "my_individual_token"
response <- GET(glue("https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=WDI.DE&apikey={token}"))
response

