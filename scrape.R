# Load package
library(rvest)
library(httr)
library(stringr)
library(dplyr)
library(parallel)
library(ggplot2)

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
getStreetAddress <- function(page){
  address <- content(page) %>%
    html_nodes("h1+ .subtitle") %>% 
    html_text()
  return(address)
}
getActivityId <- function(page){
  page_content <- content(page, as = "text")
  id_string <- str_extract(page_content, "past_transactions_component/[[0-9]]+\\?")
  id <- as.numeric(gsub("[^[:digit:]]", "", id_string))
  return(id)
}
getLatLon <- function(page){
  page_content <- content(page, as = "text")
  latlon_string <- str_extract(page_content, "se:map:point='[[0-9|.|-]]+,[[0-9|.|-]]+'")
  latlon_string <- gsub("'", "", substring(latlon_string, 15))
  latlon <- as.numeric(unlist(strsplit(latlon_string, ",")))
  return(latlon)
}
getBuildingInfo <- function(building_url){
  url <- sprintf("http://streeteasy.com%s#tab_building_detail=3", building_url)
  message(sprintf("Scraping %s", url))
  page <- GET(url)
  street_address <- getStreetAddress(page)
  activity_id <- getActivityId(page)
  lat_lon <- getLatLon(page)
  return(list(building_url = building_url,
              street_address = street_address,
              activity_id = activity_id,
              lat = lat_lon[1],
              lon = lat_lon[2]))
}

getTransactions <- function(info){
  url <- sprintf("http://streeteasy.com/nyc/property_activity/past_transactions_component/%d?show_rentals=true", 
                 info$activity_id)
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
    table <- data.frame(listing_url = links, 
                        building_url = info$building_url, 
                        street_address = info$street_address,
                        lat = info$lat,
                        lon = info$lon,
                        table)
    return(table)
  }
}

# Format transactions
formatTransactions <- function(transactions_raw){
  # Bind rows
  transactions_bind <- bind_rows(transactions_raw)
  
  # Rename columns
  transactions_bind$Floorplan <- NULL
  colnames(transactions_bind) <- tolower(gsub("\\.", "", colnames(transactions_bind)))
  
  # Clean up column values
  transactions_bind$date <- as.Date(substr(transactions_bind$date, 0, 10), format = "%m/%d/%Y")
  transactions_bind$rent <- as.numeric(gsub("[^[:digit:]]", "", str_extract(transactions_bind$rent, "^\\$.+ ")))
  transactions_bind$ft <- as.numeric(gsub("[^[:digit:]]", "", transactions_bind$ft))
  transactions_bind$ft[transactions_bind$ft == 0] <- NA
  
  # Remove bad data, deduplicate, and fill in misisng sizes
  transactions_clean <- transactions_bind %>%
    filter(rent <= 5000 & (ft > 300 | is.na(ft)) & unit != "Unknown") %>%
    group_by(building_url, unit) %>%
    arrange(date) %>%
    filter(lead(date) - date > 90  | is.na(lead(date))) %>%
    mutate(ft = round(mean(ft, na.rm = TRUE))) %>%
    ungroup()
  
  # Clean up column values
  transactions_clean$beds <- recode(transactions_clean$beds, 
                                    `1.5 beds` = "1 bed",
                                    `2,200 beds` = "2 beds",
                                    `2.5 beds` = "3 beds",
                                    `3.5 beds` = "4 beds")
  transactions_clean$beds[transactions_clean$beds == ""] <- NA
  transactions_clean$baths[transactions_clean$baths == ""] <- NA
  transactions_clean$baths[transactions_clean$baths == "1,150 baths"] <- 1
  
  # Return
  return(transactions_clean)
  
}

# Run
building_urls <- getAllBuildingURLs("http://streeteasy.com/buildings/hudson-heights")
building_info <- mclapply(building_urls, getBuildingInfo, mc.cores = 5)
transactions_raw <- mclapply(building_info, getTransactions, mc.cores = 5)
transactions_clean <- formatTransactions(transactions_raw)

# Plot
transactions_clean %>%
  filter(!is.na(ft)) %>%
  group_by(beds) %>%
  filter(n() > 50) %>%
  ungroup() %>%
  ggplot(aes(ft, rent, group = beds, colour = beds)) +
  geom_smooth(se = FALSE)

transactions_clean %>%
  group_by(beds) %>%
  filter(n() > 50) %>%
  ungroup() %>%
  ggplot(aes(date, rent, group = beds, colour = beds)) +
  geom_smooth(se = FALSE)
