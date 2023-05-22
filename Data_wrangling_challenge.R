# Tidyverse
library(tidyverse)
library(vroom)
library(tidyverse)
library(readxl)
library(lubridate)
library(dplyr)
library(writexl)
library(data.table)
library(tictoc)
library(magrittr)


col_types <- list(
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
  withdrawn = col_double()
)

assignee_tbl <- vroom(
  file       = "C:\\Users\\Sagar\\Documents\\GitHub\\ss23-bdsb-SagarWadke\\assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

patent_assignee_tbl <- vroom(
  file       = "C:\\Users\\Sagar\\Documents\\GitHub\\ss23-bdsb-SagarWadke\\patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

uspc_tbl <- vroom(
  file       = "C:\\Users\\Sagar\\Documents\\GitHub\\ss23-bdsb-SagarWadke\\uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

patent_tbl <- vroom(
  file       = "C:\\Users\\Sagar\\Documents\\GitHub\\ss23-bdsb-SagarWadke\\patent.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

setDT(assignee_tbl)
setDT(patent_assignee_tbl)

#Challenge_1
setnames(assignee_tbl, "id", "assignee_id")

combined_patent_data <- merge(x = patent_assignee_tbl, y = assignee_tbl, 
                       by    = "assignee_id", 
                       all.x = TRUE, 
                       all.y = FALSE)

# combined_patent_data <- combined_patent_data %>%
#   mutate(country = case_when(
#     organization %>% str_to_lower() %>% str_detect("llc|corporation|inc.") ~ "USA",
#     TRUE ~ "Not USA" # Everything else
#   )) %>%
#   select(country, everything()) %>%
#   view()

result <- combined_patent_data %>%
  filter(type == "2") %>%
  group_by(organization) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
  
#challenge_2
setnames(patent_tbl, "id", "patent_id")

combined_patent_data_2 <- merge(x = combined_patent_data, y = patent_tbl, 
                              by    = "patent_id", 
                              all.x = TRUE, 
                              all.y = FALSE)

result_2 <- combined_patent_data_2 %>%
  mutate(date_column = as.Date(date)) %>%
  filter(format(date_column, "%Y-%m") == "2014-08") %>%
  arrange(date_column)%>%
  filter(type == "2") %>%
  group_by(organization) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  view()
 
#challenge 3

combined_patent_data <- combined_patent_data %>%
  mutate(patent_id = as.double(patent_id))

combined_patent_data_3 <- merge(x = combined_patent_data, y = uspc_tbl, 
                                by    = "patent_id", 
                                all.x = TRUE, 
                                all.y = FALSE)

combined_patent_data$patent_id <- as.character(combined_patent_data$patent_id)  # Convert to character
uspc_tbl$patent_id <- as.character(uspc_tbl$patent_id)  # Convert to character

combined_patent_data_3 <- merge(x = combined_patent_data, y = uspc_tbl, 
                                by = "patent_id", 
                                all.x = TRUE, 
                                all.y = FALSE) 
setDT(combined_patent_data_3)

result_3 <- combined_patent_data_3[complete.cases(combined_patent_data_3$mainclass_id), ] %>%
  group_by(mainclass_id) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1:10) %>%
  mutate(tech_sector = c("Active solid-state devices", "Telecommunications", "Multiplex communications", "Semiconductor device manufacturing: process",
                         "Electrical computers and digital processing systems: multicomputer data transferring","Chemistry: molecular biology and microbiology",
                         "Drug, bio-affecting and body treating compositions","Computer graphics processing and selective visual display systems",
                         "Television","Drug, bio-affecting and body treating compositions"))%>%
  view()

#challenge 4
