source("R/00renv.R") #log into DB, load common packages

library(furrr)
options(future.rng.onMisuse = "ignore")
plan(multisession) 


#The goal here is to create a measure of traffic harm from smog and pollution near properties


#First import each property
query <- paste0("SELECT lat,lon FROM dwellings_urban_development_program")

properties_raw <- dbGetQuery(con, query )  %>% 
  distinct() %>% 
  filter(!is.na(lat)) 

properties <- properties_raw %>% 
  st_as_sf(coords = c("lon",
                      "lat"),
           crs = "wgs84") %>% 
  bind_cols(properties_raw %>% select(lat,lon))


#Find properties that have already been coded....

# Check if the table exists
if (dbExistsTable(con, "traffic_danger")) { 
  
  # SQL query to get lat lon for roads already encoded
  query <- "SELECT DISTINCT lat, lon FROM traffic_danger"
  
  # Fetch the data
  already_encoded <- dbGetQuery(con, query)

  properties <- properties %>% 
                anti_join(already_encoded)  
}

#import data on roads with traffic volumes on each road
traffic <- read_sf("data/traffic_volume/Traffic_Volume.shp") %>% 
  st_transform(st_crs(properties)) %>%
  filter(TWO_WAY_AA>5000) %>% 
  st_make_valid() %>% 
  arrange(desc(YR)) %>% 
  distinct(HMGNS_LNK_,.keep_all = T) %>% 
  select( TWO_WAY_AA,
          foi_linkage = OBJECTID) 

#Let's assess traffic pollution as a funciton of number of vehicles with an exponential decay
#This seems reasonable function given https://www.nber.org/system/files/working_papers/w15413/w15413.pdf

run_for_one_property <- function(x){
  
traffic_danger <- st_distance(traffic, properties[x,]) %>% 
    as_tibble() %>%
    st_drop_geometry() %>%
    bind_cols(traffic %>% st_drop_geometry()) %>%
    mutate(distance = pmax(5,as.numeric(value))) %>% #Everywhere is at least 5m from a road otherwise exponents are undefined. 
    select(-value) %>% 
    ungroup() %>% 
    filter(distance<1000 | distance == min(distance)) %>% #2km limit is somewhat arbitrary but reasonable. 
    mutate(traffic_risk = 1/(exp(1)^(distance/50))) %>% #This shape of curve is a bit off tbh
    mutate(lat = properties[x,]$lat,
           lon = properties[x,]$lon) 
return(traffic_danger)
}


#Lots of db writes here so we'll speed it up by only checking if there is a db already in place once...

run_for_first_property_db <- function(x) {
  
  traffic_danger <- run_for_one_property(x)
# Check if the table exists
if (!dbExistsTable(con, "traffic_danger")) {
  # If the table doesn't exist, create it
  dbWriteTable(con, "traffic_danger", traffic_danger)
} else {
  # If the table exists, append data to it
  dbWriteTable(con, "traffic_danger", traffic_danger, append = TRUE)
}
}

walk(1,run_for_first_property_db,.progress = T)


run_for_all_property_db <- function(x) {
    traffic_danger <- run_for_one_property(x)
      # If the table exists, append data to it
      dbWriteTable(con, "traffic_danger", traffic_danger, append = TRUE)
   }


walk(seq(2,nrow(properties)),run_for_all_property_db,.progress = T)

traffic_risk <- tbl(con, "traffic_danger") %>%
  distinct(lat,lon,traffic_risk,foi_linkage,.keep_all = T) %>% 
  group_by(lat,lon,foi_linkage) %>%  
  filter(row_number() == 1) %>% # Should only be one row per home/road combination
  group_by(lat,lon) %>% 
  summarise(traffic_pollution = sum(traffic_risk)) %>% 
  collect() %>% 
  ungroup() 

dbWriteTable(con, "traffic_pollution_aggregated", traffic_risk,overwrite = T)


  

