#get list of locations for properties 
source("R/00renv.R") #log into DB, load common packages
library(httr)
library(ptvapi)





tram_routes  <- routes() %>% filter(route_type_description  == "Tram")
train_routes <- routes() %>% filter(route_type_description  == "Train")
vline_routes <- routes() %>% filter(route_type_description  == "Vline") 
bus_routes   <- routes() %>% filter(route_type_description  == "Bus") 

tram_stops   <- map_df(tram_routes$route_id,  ~stops_on_route(.,"Tram"))
train_stops  <- map_df(train_routes$route_id, ~stops_on_route(.,"Train")) 
vline_stops  <- map_df(vline_routes$route_id, ~stops_on_route(.,"Vline")) 
bus_stops    <- map_df(bus_routes$route_id,   ~stops_on_route(.,"Bus")) 


vline_stops_clean <- vline_stops %>%
               mutate(route_type_description = if_else(str_detect(stop_name,
                                                      "Railway Station"),
                                                      "Vline train","Vline bus")) %>%
               filter(!(stop_id %in% c(1021,
                                       1028,
                                       1036,
                                       1040,
                                       1044,
                                       1049,
                                       1064,
                                       1071,
                                       1072,
                                       1144,
                                       1153,
                                       1162,
                                       1181,
                                       1187,
                                       1202,
                                       1218,
                                       4406,
                                       13970,
                                       4406,
                                       4529,
                                       4488,
                                       4407,
                                       4547,
                                       4407,
                                       10391,
                                       26954,
                                       4324,
                                       4597,
                                       4437,
                                       4537,
                                       4452,
                                       4411,
                                       4348,
                                       14118))) #filter out the city vline stops like southern cross, since these are more like destinations and not accessibility improving options. 

# QC to check filters for vline stops in the city (you can remove the ! above to check the ones filtered are correct too)
#library(leaflet)
# library(sf)
# vline_stops_clean %>%
#   leaflet() %>%
#   addTiles() %>%
#   addMarkers(lng = manual_stops$stop_longitude,
#              lat = manual_stops$stop_latitude)

bus_stops_clean <- bus_stops %>% 
  filter(!(stop_id %in% vline_stops_clean$stop_id)) # all vline bus stops are also coded as regular bus stops which is annoying - lets's make them distinct. 

manual_stops <- 
  tribble(~stop_latitude, ~stop_longitude, ~stop_name,   ~stop_id, ~route_type_description,   ~note,
          -37.8011991,    144.9414942,     "Arden",      111111112,    "train",       "New metro station",
          -37.8336503,    144.9732977,     "Domain",     111111113,    "train",       "New metro station",
          -37.8150279,    144.9663688,     "CBD South",  111111114,    "train",       "New metro station",
          -37.8076793,    144.9660151,     "CBD North",  111111115,    "train",       "New metro station",
          -37.7996481,    144.9575026,     "Parkville",  111111116,    "train",       "New metro station",
          -37.9565458,    145.0489606,     "Cheltenham", 111111117,    "train_future","SRL",
          -37.905716,     145.1379483,     "Monash uni", 111111118,    "train_future","SRL",
          -37.8503509,    145.1113166,     "Deakin",     111111119,    "train_future","SRL",
          -37.7789257,    145.0800368,     "Buleen",     111111120,    "brt",         "NE link BRT",
          -37.7891444,    145.1030616,     "Doncaster",  111111121,    "brt",         "NE link BRT",
          -37.7253487,    144.8663083,     "Keilor East",111111122,    "train_future","SRL Airport"
          )

all_stops <- tram_stops %>% 
             bind_rows(train_stops) %>% 
             bind_rows(bus_stops_clean) %>% 
             bind_rows(vline_stops_clean) %>% 
             bind_rows(manual_stops) %>% 
             distinct(stop_id,route_type_description,.keep_all = TRUE)  %>%
  select(-c(stop_sequence,
            disruption_ids)) %>% 
  mutate(id = row_number()) %>% 
  rename(ptv_id = stop_id) %>% 
  mutate(lat_stop = stop_latitude,
         lon_stop = stop_longitude) %>% 
  st_as_sf(coords = c("stop_longitude","stop_latitude"), crs = "wgs84")%>% 
  mutate(route_type_description = tolower(str_replace_all(route_type_description," ","_")))



dbWriteTable(con,
             "pt_stops", 
             all_stops,
             overwrite = T)
