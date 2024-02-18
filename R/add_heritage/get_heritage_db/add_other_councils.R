source("R/00renv.R")

#Since port phillip council has provided us with a detailed shape file, let's filter out 
#Any heritage register stuff that is in Port phillip outside of the register, then join them up. 


source("R/03import_filtered_heritage_db.R")



dwelling_data_crs <-  st_read(con, query = "SELECT dwellings_est,lat,lon,geometry FROM dwellings_urban_development_program
                          LIMIT 1") %>% 
  st_crs()


heritage_db <- import_filtered_heritage_db() %>% 
  st_transform(dwelling_data_crs)


port_phillip <- read_sf("data/port_phillip_shapefile/Hertiage_policy grading_gazetted.shp") %>% 
  st_make_valid() %>% 
  st_transform(st_crs(heritage_db)) %>% 
  mutate(id = paste0("port_phillip_",COPP_PFI),
         status = case_when(CATEGORY == "Contributory Heritage Place - inside HO" ~ "Overlay - Contributory", 
                            CATEGORY == "Significant Heritage Place - inside HO" ~ "Overlay - Significant" ,
                            CATEGORY == "Non Contributory - inside HO" ~ "Overlay - Not Signficant"),
         heritage_authority_name = "Port Phillip") %>% 
  select(id,
         status) %>% 
  st_centroid() %>% 
  st_transform(dwelling_data_crs) 

other_councils <- read_rds("working_data/com_and_coy_heritage_db.rds") %>% 
  mutate(id =as.character(id)) %>% 
  st_set_crs("wgs84") %>% 
  st_transform(dwelling_data_crs)

  

all_heritage <- heritage_db %>% 
  mutate(id = as.character(id)) %>% 
  bind_rows(port_phillip) %>% 
  distinct() %>%
  bind_rows(other_councils)%>% 
  select(-NUMBER_SUFFIX) %>% 
  st_transform(dwelling_data_crs) %>% 
  mutate(status = case_when(status == "Contributory" ~ "Overlay - Contributory",
                            status == "Significant" ~ "Overlay - Significant",
                            status == "Individually significant" ~ "Overlay - Significant",
                            status == "individually significant" ~ "Overlay - Significant",
                            status == "Individually Significant" ~ "Overlay - Significant",
                            status == "Victorian Heritage register" ~ "Victorian Heritage Register",
                            status == "Unknown" ~ "Overlay - Type of Listing Not Recorded",
                            status == "Not contributory" ~ "Overlay - Not Signficant" ,
                            status == "-" ~ "Overlay - Type of Listing Not Recorded",
                            T ~ status))



st_write(obj = all_heritage,
         dsn = con, 
         Id(schema="public", 
            table = 'heritage_database'))
