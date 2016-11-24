# Load package
library(rvest)
library(httr)
library(stringr)
library(dplyr)
library(parallel)

# Get list of buildings
getBuildingURLs <- function(base_url, page_num){
  url <- sprintf("%s?page=%d", base_url, page_num)
  message(sprintf("Scraping %s", url))
  buildings_list <- read_html(url)
  building_urls <- buildings_list %>%
    html_nodes(".details-title > a") %>%
    html_attr("href")
  return(building_urls)
}
getAllBuildingURLs <- function(base_url){
  num_pages <- read_html(base_url) %>% 
    html_nodes(".gap+ .page a") %>%
    html_text() %>%
    as.integer()
  building_urls <- lapply(seq_len(num_pages), getBuildingURLs, base_url = base_url)
  return(unique(unlist(building_urls)))
}

# Extract transactions from building pages
extractActivityId <- function(building_url){
  url <- sprintf("http://streeteasy.com%s#tab_building_detail=3", building_url)
  message(sprintf("Scraping %s", url))
  page <- GET(url)
  page_content <- content(page, as = "text")
  id_string <- str_extract(page_content, "past_transactions_component/[[0-9]]+\\?")
  id <- as.numeric(gsub("[^[:digit:]]", "", id_string))
  return(id)
}
getTransactions <- function(building_url){
  id <- extractActivityId(building_url)
  url <- sprintf("http://streeteasy.com/nyc/property_activity/past_transactions_component/%d?show_rentals=true", id)
  message(sprintf("Scraping %s", url))
  activity_site <- read_html(url)
  tables <- activity_site %>%
    html_nodes("#past_transactions_table") %>% 
    html_table()
  if (length(tables) == 0) {
    return(NULL)
  } else {
    table <- tables[[1]]
    links <- activity_site %>% 
      html_nodes(".activity_unit") %>% 
      html_children() %>% 
      html_attr("href")
    table <- data.frame(listing_url = links, table)
    return(table)
  }
}

# Run
building_urls <- getAllBuildingURLs("http://streeteasy.com/buildings/hudson-heights")
transactions <- mclapply(building_urls, getTransactions, mc.cores = 5)
transactions <- mapply(FUN = cbind, 
                       building_url = building_urls[!sapply(transactions, is.null)], 
                       transactions[!sapply(transactions, is.null)],
                       SIMPLIFY = FALSE)
transactions_bind <- bind_rows(transactions)
