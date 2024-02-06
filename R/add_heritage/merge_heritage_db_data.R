
source("R/00renv.R") #log into DB, load common packages

#This file merges heritage database lat lon data with urban development program data


dwelling_data <-  st_read(con, query = "SELECT dwellings_est,lat,lon,geometry FROM dwellings_urban_development_program") %>%
  group_by(lat,lon) %>% 
  arrange(desc(dwellings_est)) %>% 
  filter(row_number() == 1) %>% 
  ungroup() 


heritage_database <- st_read(con, query = "SELECT status,id, geometry FROM heritage_database") %>% 
  st_transform(st_crs(dwelling_data)) %>% 
  rename(heritage_status = status,
         heritage_db_id  = id) %>% 
  mutate(heritage_order = case_when(heritage_status == "Victorian Heritage Register"~1, 
                                    heritage_status == "Overlay - Significant" ~2, 
                                    heritage_status == "Overlay - Not Signficant"~3, 
                                    heritage_status == "Overlay - Type of Listing Not Recorded"~4))

#Join with heritage, and when there is more than one heritage designation for a property pick the most restrictive
heritage_data_aggregated <- dwelling_data %>% 
  select(lat,lon) %>% 
  st_join(heritage_database) %>% 
  group_by(lat,lon) %>% 
  arrange(heritage_order) %>% 
  filter(row_number() ==1) %>% 
  select(-heritage_order) %>% 
  st_drop_geometry() %>% 
  filter(!is.na(heritage_status))
  

dbWriteTable(con, "heritage_data_aggregated", heritage_data_aggregated,overwrite = T)
