# Load packages
library(dplyr)
library(httr)

# Load data
elevators <- read.csv("https://raw.githubusercontent.com/datanews/elevators/master/elevators.csv",
                      stringsAsFactors = FALSE)

# Clean up data
elevators <- elevators %>% 
  filter(Borough == "Manhattan") %>%
  select(HOUSE_NUMBER, STREET_NAME) %>%
  distinct()

# Normalize addresses
getAddressInfo <- function(house_number, street, app_id = Sys.getenv("NYC_API_ID"), app_key = Sys.getenv("NYC_API_KEY")){
  base_url <- "https://api.cityofnewyork.us/geoclient/v1/address.json?houseNumber=%s&street=%s&borough=Manhattan&app_id=%s&app_key=%s"
  url <- URLencode(sprintf(base_url, house_number, street, app_id, app_key))
  message(sprintf("Hitting %s", url))
  response <- content(GET(url))
  return(list(street_name_normalized = response$address$giStreetName1,
              zip_code = response$address$zipCode))
}
tryAddressInfo <- function(house_number, street){
  try(getAddressInfo(house_number, street))
}
address_info <- mapply(FUN = tryAddressInfo, elevators$HOUSE_NUMBER, elevators$STREET_NAME, SIMPLIFY = FALSE, USE.NAMES = FALSE)
address_found <- sapply(address_info, function(x) !is.null(x$street_name_normalized))

# Write to csv
elevators_write <- elevators %>%
  filter(address_found) %>%
  transmute(house_number = HOUSE_NUMBER,
            street_name = sapply(address_info[address_found], function(x) x$street_name_normalized),
            zip_code = as.character(sapply(address_info[address_found], function(x) x$zip_code)),
            zip_code = ifelse(zip_code == "NULL", NA, zip_code)) %>%
  arrange(street_name, as.integer(house_number))
write.csv(elevators_write, file = "manhattan_elevator_buildings.csv", row.names = FALSE)
