
source("R/00renv.R") #log into DB, load common packages

#This file merges heritage database lat lon data with urban development program data

# The way we've collected the database has been an attempt to merge in as many ways as possible so we expect each property to have multiple
# matches. 

dwelling_data <-  st_read(con, query = "SELECT dwellings_est,lat,lon,address,geometry FROM dwellings_urban_development_program") %>%
  group_by(lat,lon,address) %>% 
  arrange(desc(dwellings_est)) %>% 
  filter(row_number() == 1) %>% 
  ungroup() 

dwelling_data_gnaf <- standardize_address(dwelling_data$address) %>% 
  bind_cols(dwelling_data) %>% 
  filter(!is.na(NUMBER_FIRST),
         !is.na(STREET_NAME),
         !is.na(POSTCODE),
         !is.na(STREET_TYPE),
  ) 


heritage_database <- st_read(con, query = 'SELECT status,id, "NUMBER_FIRST", "STREET_NAME", "POSTCODE", "STREET_TYPE", "LOCALITY_NAME", heritage_authority_name, address, geometry FROM heritage_database') %>% 
  rename(heritage_status = status,
         heritage_db_id  = id) %>% 
  mutate(heritage_order = case_when(heritage_status == "Victorian Heritage Register"~1, 
                                    heritage_status == "Overlay - Significant" ~2, 
                                    heritage_status == "Overlay - Contributory" ~ 3,
                                    heritage_status == "Overlay - Not Signficant"~4, 
                                    heritage_status == "Overlay - Type of Listing Not Recorded"~5))


#First step, see what joins geometrically.... 

#Join with heritage, and when there is more than one heritage designation for a property pick the most restrictive
heritage_data_aggregated <- dwelling_data %>% 
  select(lat,lon) %>% 
  st_join(heritage_database) %>% 
  group_by(lat,lon) 


## Second step, if it won't join geometrically try joining using PNAF address.... 

not_joined <- heritage_database %>% 
  st_drop_geometry() %>% 
  anti_join(heritage_data_aggregated %>% st_drop_geometry(), by = "heritage_db_id") %>% 
  filter(!is.na(NUMBER_FIRST),
         !is.na(STREET_NAME),
         !is.na(POSTCODE),
         !is.na(STREET_TYPE),
         ) 

joined_dwelling_data <- dwelling_data_gnaf %>% 
  inner_join(not_joined)


both_db_joined <- heritage_data_aggregated%>% 
  bind_rows(joined_dwelling_data) %>% 
  group_by(lat,lon) %>% 
  arrange(heritage_order,heritage_db_id) %>% 
  filter(row_number() ==1) %>% 
  select(-heritage_order) %>% 
  st_drop_geometry() %>% 
  filter(!is.na(heritage_status)) %>%
  select(lat,lon,heritage_db_id,heritage_status,heritage_authority_name)%>% 
  mutate(heritage_db_id = str_remove(heritage_db_id,
                                     "heritage_db_geocoded_manually_with_pnaf_add")
         )
both_db_joined %>% 
  st_drop_geometry() %>% 
  group_by(heritage_authority_name,heritage_status) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  spread(heritage_status,n) %>% view()

dbWriteTable(con, 
             "heritage_data_aggregated", 
             both_db_joined %>% select(-heritage_authority_name),
             overwrite = T)



#Join with heritage, and when there is more than one heritage designation for a property pick the most restrictive
heritage_files_not_geocoded <- heritage_database %>% 
  st_drop_geometry() %>% 
  filter(!str_detect(heritage_db_id,"heritage_db_geocoded_manually_with_pnaf_add")) %>% 
  anti_join(distinct(both_db_joined,.keep_all = T)) %>% 
  left_join(heritage_database)

heritage_files_not_geocoded%>% 
  write_sf("test.shp")

