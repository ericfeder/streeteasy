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
  return(response$address$firstStreetNameNormalized)
}
tryAddressInfo <- function(house_number, street){
  try(getAddressInfo(house_number, street))
}
elevators$street_name_normalized <- mapply(FUN = tryAddressInfo, elevators$HOUSE_NUMBER, elevators$STREET_NAME)

# Write to csv
elevators_write <- elevators %>%
  transmute(house_number = as.integer(HOUSE_NUMBER),
            street_name = street_name_normalized) %>%
  filter(!is.na(house_number)) %>%
  arrange(street_name, house_number)
write.csv(elevators_write, file = "manhattan_elevator_buildings.csv", row.names = FALSE)
