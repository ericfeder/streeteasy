# Load package
library(dplyr)
library(stringr)

# Load raw data
load("data/raw.RData")

# Format transactions
appendAmenities <- function(transactions_raw, listing_info, building_info){
  if (!is.null(transactions_raw)){
    transactions_raw <- transactions_raw %>%
      mutate(building_url = building_info$building_url,
             street_address = building_info$street_address,
             lat = building_info$lat,
             lon = building_info$lon,
             elevator = building_info$on_elevator_list,
             laundry_in_building = mean(sapply(listing_info, function(x) "Laundry in Building" %in% x$amenities)) > 0.15,
             laundry_in_unit = sapply(listing_info, function(x) "Washer/Dryer In-Unit" %in% x$amenities),
             dishwasher = sapply(listing_info, function(x) "Dishwasher" %in% x$amenities) | 
               grepl("dish *washer", sapply(listing_info, function(x) x$description), ignore.case = TRUE)
      )
  }
  return(transactions_raw)
}
formatTransactions <- function(transactions_raw, listing_info, building_info){
  # Append amenities information
  transactions_append <- mapply(FUN = appendAmenities, transactions_raw, listing_info, building_info, SIMPLIFY = FALSE)
  
  # Bind rows
  transactions_bind <- bind_rows(transactions_append)
  
  # Rename columns
  transactions_bind$Floorplan <- NULL
  colnames(transactions_bind) <- tolower(gsub("[^[:alpha:]|_]", "", colnames(transactions_bind)))
  
  # Clean up column values
  transactions_bind$date <- as.Date(substr(transactions_bind$date, 0, 10), format = "%m/%d/%Y")
  transactions_bind$rent <- as.numeric(gsub("[^[:digit:]]", "", str_extract(transactions_bind$rent, "^\\$.+ ")))
  transactions_bind$ft <- as.numeric(gsub("[^[:digit:]]", "", transactions_bind$ft, perl = TRUE))
  transactions_bind$ft[transactions_bind$ft == 0] <- NA
  
  # Remove bad data, deduplicate, and fill in misisng sizes
  transactions_clean <- transactions_bind %>%
    filter(rent <= 5000 & 
             (ft > 300 | is.na(ft)) & 
             !unit %in% c("Unknown", "#RETAIL", "", "#STORE") &
             date > "2010-01-01") %>%
    group_by(building_url, unit, beds) %>%
    arrange(date) %>%
    filter(lead(date) - date > 90  | is.na(lead(date))) %>%
    mutate(ft = round(mean(ft, na.rm = TRUE)),
           ft = ifelse(is.nan(ft), NA, ft)) %>%
    ungroup()
  
  # Clean up column values
  transactions_clean$beds <- recode(transactions_clean$beds, 
                                    `1.5 beds` = "2 beds",
                                    `2,200 beds` = "2 beds",
                                    `2.5 beds` = "3 beds",
                                    `3.5 beds` = "4 beds",
                                    `4.5 beds` = "4 beds")
  transactions_clean$beds[transactions_clean$beds == ""] <- NA
  transactions_clean$baths[transactions_clean$baths == ""] <- NA
  transactions_clean$baths[transactions_clean$baths == "1,150 baths"] <- "1 bath"
  
  # Return
  return(transactions_clean)
  
}

# Prepare data for model training
prepareForTraining <- function(data){
  data <- data %>% filter(!is.na(beds))
  data$extra_bath <- !is.na(data$baths) && data$baths != "1 bath"
  data$beds[data$beds %in% c("4 beds", "5 beds", "6 beds")] <- "4+ beds"
  data$beds <- factor(data$beds, levels = c("studio", "1 bed", "2 beds", "3 beds", "4+ beds"))
  data$days_ago <- as.numeric(Sys.Date() - data$date)
  data <- data[, c("rent", "beds", "extra_bath", "days_ago", 
                   "ft", "lat", "lon", "dishwasher", "elevator",
                   "laundry_in_unit", "laundry_in_building")]
  wrong_class <- sapply(data, is.character) | sapply(data, is.logical)
  data[wrong_class] <- lapply(data[wrong_class], as.factor)
  return(data)
}

# Format data
transactions_clean <- formatTransactions(transactions_raw, listing_info, building_info)
transactions_train <- prepareForTraining(transactions_clean)