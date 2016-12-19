# Load package
library(rvest)
library(httr)
library(stringr)
library(dplyr)
library(party)

# Get list of buildings
fetchBuildingURLs <- function(base_url, page_num){
  url <- sprintf("%s?page=%d", base_url, page_num)
  message(sprintf("Scraping %s", url))
  buildings_list <- read_html(url)
  building_urls <- buildings_list %>%
    html_nodes(".details-title > a") %>%
    html_attr("href")
  return(building_urls)
}
fetchAllBuildingURLs <- function(base_url){
  num_pages <- read_html(base_url) %>% 
    html_nodes(".gap+ .page a") %>%
    html_text() %>%
    as.integer()
  building_urls <- lapply(seq_len(num_pages), fetchBuildingURLs, base_url = base_url)
  return(unique(unlist(building_urls)))
}

# Get building information
extractStreetAddress <- function(page){
  address <- content(page) %>%
    html_nodes("h1+ .subtitle") %>% 
    html_text()
  return(address)
}
extractBuildingClass <- function(page){
  class <- content(page) %>%
    html_nodes(".clickable") %>% 
    html_text()
  return(class)
}
fetchAddressInfo <- function(house_number, street){ 
  app_id <- Sys.getenv("NYC_API_ID")
  app_key <- Sys.getenv("NYC_API_KEY")
  base_url <- "https://api.cityofnewyork.us/geoclient/v1/address.json?houseNumber=%s&street=%s&borough=Manhattan&app_id=%s&app_key=%s"
  url <- URLencode(sprintf(base_url, house_number, street, app_id, app_key))
  message(sprintf("Hitting %s", url))
  response <- content(GET(url))
  return(response$address)
}
extractAddresses <- function(address_info){
  low_numbers <- address_info[grep("giLowHouseNumber", names(address_info))]
  high_numbers <- address_info[grep("giHighHouseNumber", names(address_info))]
  street_names <- address_info[grep("giStreetName", names(address_info))]
  
  low_numbers_ind <- gsub("[^[:digit:]]", "", names(low_numbers))
  high_numbers_ind <- gsub("[^[:digit:]]", "", names(high_numbers))
  street_names_ind <- gsub("[^[:digit:]]", "", names(street_names))
  matched_inds <- Reduce(intersect, list(low_numbers_ind, high_numbers_ind, street_names_ind))
  
  low_numbers_matched <- low_numbers[low_numbers_ind %in% matched_inds]
  high_numbers_matched <- high_numbers[high_numbers_ind %in% matched_inds]
  street_names_matched <- street_names[street_names_ind %in% matched_inds]
  transposed_addresses <- mapply(FUN = list, 
                                 low_number = low_numbers_matched,
                                 high_number = high_numbers_matched,
                                 street_name = street_names_matched, 
                                 SIMPLIFY = FALSE, USE.NAMES = FALSE)
}
fetchNormalizedAddresses <- function(address){
  house_number <- as.numeric(str_extract(address, "^[[0-9]]+"))
  street <- str_extract(address, "^[[0-9]]+[[:alnum:]|[:space:]]+,")
  street <- gsub("^[[:digit:]]+ |,", "", street)
  address_info <- fetchAddressInfo(house_number, street)
  transposed_addresses <- extractAddresses(address_info)
  return(transposed_addresses)
}
checkElevatorStatusSingle <- function(normalized_address, elevator_buildings){
  low_number <- as.integer(normalized_address$low_number)
  high_number <- as.integer(normalized_address$high_number)
  if (is.na(low_number) | is.na(high_number)){
    return(FALSE)
  }
  address_numbers <- seq(low_number, high_number, by = 2)
  match_number <- elevator_buildings$house_number %in% address_numbers
  match_street <- elevator_buildings$street_name == normalized_address$street_name
  return(any(match_number & match_street))
}
checkElevatorStatus <- function(normalized_addresses, elevator_buildings){
  has_elevator <- sapply(normalized_addresses, checkElevatorStatusSingle, elevator_buildings = elevator_buildings)
  any(has_elevator)
}
extractActivityId <- function(page){
  page_content <- content(page, as = "text")
  id_string <- str_extract(page_content, "past_transactions_component/[[0-9]]+\\?")
  id <- as.numeric(gsub("[^[:digit:]]", "", id_string))
  return(id)
}
extractLatLon <- function(page){
  page_content <- content(page, as = "text")
  latlon_string <- str_extract(page_content, "se:map:point='[[0-9|.|-]]+,[[0-9|.|-]]+'")
  latlon_string <- gsub("'", "", substring(latlon_string, 15))
  latlon <- as.numeric(unlist(strsplit(latlon_string, ",")))
  return(latlon)
}
fetchBuildingInfo <- function(building_url, elevator_buildings){
  url <- sprintf("http://streeteasy.com%s#tab_building_detail=3", building_url)
  message(sprintf("Scraping %s", url))
  page <- GET(url)
  street_address <- extractStreetAddress(page)
  class <- extractBuildingClass(page)
  normalized_addresses <- fetchNormalizedAddresses(street_address)
  elevator_status <- checkElevatorStatus(normalized_addresses, elevator_buildings)
  activity_id <- extractActivityId(page)
  lat_lon <- extractLatLon(page)
  return(list(building_url = building_url,
              street_address = street_address,
              class = class,
              normalized_addresses = normalized_addresses,
              on_elevator_list = elevator_status,
              activity_id = activity_id,
              lat = lat_lon[1],
              lon = lat_lon[2]))
}

# Get list of transactions
extractTransactionTable <- function(activity_site){
  tables <- activity_site %>%
    html_nodes("#past_transactions_table") %>% 
    html_table()
  if (length(tables) == 0) {
    return(NULL)
  } else {
    return(tables[[1]])
  }
}
extractListingUrls <- function(activity_site){
  listing_urls <- activity_site %>% 
    html_nodes(".activity_unit") %>% 
    html_children() %>% 
    html_attr("href")
  return(listing_urls)
}
fetchTransactions <- function(info){
  url <- sprintf("http://streeteasy.com/nyc/property_activity/past_transactions_component/%d?show_rentals=true", 
                 info$activity_id)
  message(sprintf("Scraping %s", url))
  activity_site <- read_html(url)
  transaction_table <- extractTransactionTable(activity_site)
  if (is.null(transaction_table)){
    return(NULL)
  } else {
    listing_urls <- extractListingUrls(activity_site)  
    transactions <- data.frame(listing_url = listing_urls, 
                               transaction_table)
    return(transactions)
  }
}

# Get listing information
extractListingDescription <- function(page){
  description <- page %>%
    html_nodes(".listings_sections:nth-child(1)") %>%
    html_text()
}
extractListingAmenities <- function(page){
  non_amenities <- c("", "This highlight has been verified by StreetEasy", "Highlights", 
                     "googletag.display(\"rtt_main_p1\");", "Amenities", "Building Amenities",
                     "googletag.cmd.push(function() {", "});", "Rental", "Listing Amenities")
  amenities <- page %>%
    html_nodes(".amenities") %>%
    html_text() %>% 
    strsplit("\n") %>% 
    unlist() %>% 
    trimws() %>% 
    setdiff(., non_amenities)
  return(amenities)
}
fetchListingInfo <- function(listing_url){
  url <- sprintf("http://streeteasy.com%s", listing_url)
  message(sprintf("Scraping %s", url))
  page <- read_html(url)
  description <- extractListingDescription(page)
  amenities <- extractListingAmenities(page)
  return(list(listing_url = listing_url,
              amenities = amenities,
              description = description))
}

# Get raw data
building_urls <- fetchAllBuildingURLs("http://streeteasy.com/buildings/hudson-heights")
elevator_buildings <- read.csv("data/manhattan_elevator_buildings.csv", stringsAsFactors = FALSE)
building_info <- lapply(building_urls, fetchBuildingInfo, elevator_buildings = elevator_buildings)
transactions_raw <- lapply(building_info, fetchTransactions)
listing_info <- lapply(transactions_raw, function(x) lapply(x$listing_url, fetchListingInfo))

# Save raw data
save(building_info, transactions_raw, listing_info, file = "data/raw.RData")
