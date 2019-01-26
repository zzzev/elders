library(tidyverse)
library(rvest)
library(magrittr)
library(lubridate)

html_clean_text <- function(node) node %>%
  html_text %>%
  str_replace_all("\\[.*\\]", "")

html_clean_date <- function(node) node %>%
  html_clean_text %>%
  str_replace_all("Sept", "Sep") %>%
  mdy

process_race <- function(raw_race) {
  if (raw_race == "O" | raw_race == "EA") return("Asian")
  if (raw_race == "W") return("White")
  if (raw_race == "B") return("Black")
  if (raw_race == "M") return("Multiracial")
  if (raw_race == "H") return("Hispanic")
  NA
}

html <- read_html(read_file("raw.html"))
html %>%
  html_nodes("tr") %>%
  extract(9:73) %>%
  map_dfr(function(row) {
    tds <- html_nodes(row, "td")
    birthplace <- tds[[2]] %>% html_clean_text
    name <- tds[[3]] %>% html_clean_text
    born <- tds[[4]] %>% html_clean_date
    died <- tds[[5]] %>% html_clean_date
    race <- tds[[8]] %>% html_clean_text %>% process_race
    sex <- tds[[9]] %>% html_clean_text
    deathplace <- tds[[10]] %>% html_clean_text
    tibble(birthplace, name, born, died, race, sex, deathplace)
  }) %>%
  filter(is.na(race))
