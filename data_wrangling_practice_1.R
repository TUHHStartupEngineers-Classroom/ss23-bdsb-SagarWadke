library(tidyverse)
library(readxl)
library(lubridate)
library(dplyr)
library(writexl)

bikes_tbl <- read_excel("C:\\Users\\Sagar\\Documents\\GitHub\\ss23-bdsb-SagarWadke\\bikes.xlsx") %>%
  separate(col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " - ") %>%
  
  # Renaming columns
  set_names(names(.) %>% str_replace_all("\\.", "_"))  

bikes_tbl %>%
  select(model, price)

bikes_tbl %>%
  select(category_1:category_3, everything())

?starts_with

bikes_tbl %>%
  select(starts_with("model"))

bikes_tbl %>%
  # select(price) %>% Does not work
  pull(price) %>%
  mean()

?where

bikes_tbl %>%
  select(where(is.character))

bikes_tbl %>%
  select(where(is.numeric))

bikes_tbl %>%
  select(!where(is.numeric))

bikes_tbl %>%
  select(model, category_1, category_2, category_3, price) %>% 
  rename(
    Model           = model,
    `Bike Family`   = category_1,
    `Ride Style`    = category_2,
    `Bike Category` = category_3,
    `Price in Euro` = price
  )

bikes_tbl %>%
  select(model, category_1, category_2, category_3, price) %>% 
  set_names(c("Model", "Bike Family", "Ride Style", "Bike Category", "Price in Euro"))

bikes_tbl %>%
  select(model, category_1, category_2, category_3, price) %>% 
  set_names(names(.) %>% str_replace("_", " ") %>% str_to_title())

bikes_tbl %>%
  select(model, price) %>%
  arrange(desc(price)) %>%
  View()

bikes_tbl %>%
  select(model, price) %>%
  filter(price > mean(price)) %>%
  arrange(desc(price)) %>%
  view() 

bikes_tbl %>%
  pull(price) %>%
  mean()
  
bikes_tbl %>%
  select(model, price) %>%
  filter((price > 5000) | (price < 1000)) %>%
  arrange(desc(price)) %>%
  View()

bikes_tbl %>%
  select(model, price) %>%
  filter(price > 5000,
         model %>% str_detect("Endurace")
  )

bikes_tbl %>%
  filter(category_1 %in% c("Hybrid / City", "E-Bikes")) %>%
  view()

bikes_tbl %>%
  filter(category_2 == "E-Mountain")

bikes_tbl %>%
  filter(category_2 != "E-Mountain")


bikes_tbl %>%
  filter(!(category_2 %in% c("Hybrid / City", "E-Bikes")))

bikes_tbl %>%
  arrange(desc(price)) %>%
  slice(1:5)

bikes_tbl %>%
  arrange(price) %>%
  slice(1:5)

bike_orderlines_tbl <- read_excel("C:\\Users\\Sagar\\Documents\\GitHub\\ss23-bdsb-SagarWadke\\bike_orderlines.xlsx")
  
bike_orderlines_tbl %>%
  mutate(freight_costs = 2 * weight) %>%
  view()

bike_orderlines_tbl %>%
  mutate(total_price = log(total_price)) %>%
  view()

bike_orderlines_tbl %>%
  mutate(is_strive = model %>% str_to_lower() %>% str_detect("strive")) %>%
  filter(is_strive) %>%
  view()

bike_orderlines_tbl %>%
  mutate(price_binned = ntile(total_price, 3)) %>% 
  select(total_price, price_binned, everything()) %>%
  view()

bike_orderlines_tbl %>%
  mutate(price_binned = ntile(total_price, 3)) %>%
  mutate(price_binned2 = case_when(
    total_price > quantile(total_price, 0.75) ~ "High",
    total_price > quantile(total_price, 0.25) ~ "Medium",
    TRUE ~ "Low" # Everything else
  )) %>% 
  select(total_price, price_binned, price_binned2, everything()) %>%
  view()

bike_orderlines_tbl %>%
  mutate(bike_type = case_when(
    model %>% str_to_lower() %>% str_detect("aeroad") ~ "Aeroad",
    model %>% str_to_lower() %>% str_detect("ultimate") ~ "Ultimate",
    TRUE ~ "Not Aeroad or Ultimate" # Everything else
  )) %>% 
  select(bike_type, everything()) %>%
  view

bike_orderlines_tbl %>%
  summarise(
    revenue = sum(total_price)
  )

bike_orderlines_tbl %>%
  group_by(category_1) %>%
  summarise(revenue = sum(total_price)) %>%
  ungroup()

bike_orderlines_tbl %>%
  group_by(category_1, category_2) %>%
  summarise(revenue = sum(total_price)) %>%
  # Always ungroup() after you summarise(). Left-over groups will cause difficult-to-detect errors.
  ungroup() %>%
  arrange(desc(revenue))

bike_orderlines_missing <- bike_orderlines_tbl %>%
  mutate(total_price = c(rep(NA, 4), total_price[5:nrow(.)])
         
         # detect missing (absolute)
         bike_orderlines_missing %>%
           summarise(across(everything(), ~sum(is.na(.))))
         
         # detect missing (relative)
         bike_orderlines_missing %>%
           summarise(across(everything(), ~sum(is.na(.)) / length(.)))
         
         # Handling missing data
         bike_orderlines_missing %>%
           filter(!is.na(total_price)) %>%
         view()
         
bikeshop_revenue_tbl <- bike_orderlines_tbl %>%
  select(bikeshop, category_1, total_price) %>%
  group_by(bikeshop, category_1) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  arrange(desc(sales)) %>%
  view()

bikeshop_revenue_formatted_tbl <- bikeshop_revenue_tbl %>%
  pivot_wider(names_from  = category_1,
              values_from = sales) %>%
  mutate(
    Mountain = scales::dollar(Mountain, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " ???"),
    Gravel = scales::dollar(Gravel, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " ???"),
    Road     = scales::dollar(Road, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " ???"),
    `Hybrid / City` = scales::dollar(`Hybrid / City`, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " ???"),
    `E-Bikes` = scales::dollar(`E-Bikes`, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " ???")
  ) %>%
  view()

bikeshop_revenue_formatted_tbl %>%
  pivot_longer(cols           = c(names(.)[2:6]),
               names_to       = "category_1",
               values_to      = "sales",
               values_drop_na = T) %>%
  mutate(sales =  sales %>% str_remove_all("???|\\.") %>% as.double()) %>%
  view()

order_dates_tbl <- bike_orderlines_tbl %>% select(1:3)
order_items_tbl  <- bike_orderlines_tbl %>% select(1:2,4:8)

  order_dates_tbl %>%
    
    # By argument not necessary, because both tibbles share the same column names
    left_join

bike_orderlines_tbl %>%
  select(-contains("category")) %>%
  
  bind_cols(bike_orderlines_tbl %>% 
  select(category_1)) %>% 
  view

train_tbl <- bike_orderlines_tbl %>%
  slice(1:(nrow(.)/2))

test_tbl <- bike_orderlines_tbl %>%
  slice((nrow(.)/2 + 1):nrow(.))

train_tbl %>%
  bind_rows(test_tbl)

bike_orderlines_tbl %>% 
  select(order_date) %>% 
  mutate(order_date = as.character(order_date)) %>%
  
  # separate
  separate(col  = order_date,
           into = c("year", "month", "day"),
           sep  = "-", remove = FALSE) %>%
  
  mutate(
    year  = as.numeric(year),
    month = as.numeric(month),
    day   = as.numeric(day)
  ) %>%
  
  # unite
  unite(order_date_united, year, month, day, sep = "-", remove = FALSE) %>%
  mutate(order_date_united = as.Date(order_date_united)) %>%
  view()


library(data.table)
url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
covid_data_dt <- fread(url)

class(covid_data_dt)
##  "data.table" "data.frame"

test_dt <- data.table(ID = c("b","b","b","a","a","c"),
                      a  = 1:6,
                      b  = 7:12,
                      c  = 13:18)

covid_data_dt[i, j, by]

# Example (filter by year, sum cases, group by continent)
covid_data_dt[year == 2019, sum(cases), by = continentExp]

covid_data_dt[countriesAndTerritories == "Germany" & 
                lubridate::month(dateRep, label = T, abbr = F) == "June"]

covid_data_dt[order(year, month, day, -countriesAndTerritories)] %>%
  view()

covid_data_dt[,geoId]

covid_data_dt[,c("geoId", "countriesAndTerritories")] %>%
  view()

covid_data_dt[,list(geoId)] %>%
  view()

covid_data_dt[,.(geoId, countriesAndTerritories)] %>%
  view()

covid_data_dt[,.(CountryCode = geoId, country = countriesAndTerritories)] %>%
  view()

select_cols = c("cases", "deaths")
covid_data_dt[, ..select_cols] %>%
  view()

colnames(covid_data_dt)
setnames(covid_data_dt, "dateRep", "date")
setnames(covid_data_dt, "countriesAndTerritories", "country")
setnames(covid_data_dt, "continentExp", "continent")
view(covid_data_dt)

# Solution 1
aq_dt <- data.table(airquality)
aq_dt[!is.na(Ozone), c("Solar.R", "Wind", "Temp")]

covid_data_dt[,sum(deaths > 1000)]

  covid_data_dt[, deaths_per_capita := deaths / popData2019] %>%
    view()

covid_data_dt[,  `:=`(deaths_per_capita = deaths / popData2019,
                      cases_per_capita = cases / popData2019,
                      deaths_per_cases = deaths / cases)] %>%
  view()

covid_data_dt[, deaths_per_cases := NULL]

covid_data_dt[,date := lubridate::ymd(date)] %>%
  view()

data("mtcars") # step not absolutely necessary
mtcars$carname <- rownames(mtcars)
mtcars_dt <- as.data.table(mtcars)
mtcars_dt[, mileage_type := ifelse(mpg > 20, 'high', 'low')]

covid_data_dt[country == "Germany" & month == 4, 
              .(m_cases = mean(cases), 
                m_death = mean(deaths)
              )] %>%
  view()

covid_data_dt[country == "United_States_of_America" & 
                month == 5 & deaths < 1000, 
              length(day)] %>%
  view

covid_data_dt[deaths > 1000, .N, by = country] %>%
  view()

covid_data_dt[,.I[deaths > 1000]]%>%
  view()

covid_data_dt[continent == "Europe",
              .(mean(cases), mean(deaths)),
              by = .(country, month, year)
]%>%
  view()

mtcars_dt[, .(.N, mileage = mean(mpg) %>% round(2)), by=gear]%>%
  view()

covid_cases_means <- covid_data_dt[,.(m_cases  = mean(cases) %>% round(1), 
                                      m_deaths = mean(deaths) %>% round(1)), 
                                   by = .(country)
]%>%
  view()

covid_data_dt[, .N, 
              .(
                death_gt_1k = deaths > 1000, 
                cases_lt_1k = cases < 1000
              )
]%>%
  view()

covid_data_dt[, print(.SD), by = year]%>%
  view()

covid_data_dt[, lapply(.SD, mean), 
              by = .(year, month), 
              .SDcols = c("cases", "deaths")
]