#This file downloads victoria's zoning information from the VICMAP servers

source("R/00renv.R")


#Get right CRS 

udp_crs <- read_sf(con,query = "SELECT geometry FROM dwellings_urban_development_program") %>% st_crs()
#Get all of Victoria's zones and write to a database

vicmap_zones_rest_api_url <- 'https://services6.arcgis.com/GB33F62SbDxJjwEL/ArcGIS/rest/services/Vicmap_Planning/FeatureServer/3'

zones <- arcpullr::get_spatial_layer(vicmap_zones_rest_api_url) 

zones <- zones %>% mutate(zone_short =  str_replace(zone_code, "[0-9]+$", ""),
                          zone_short = case_when(str_detect(zone_short,"IN|PZ")~ "Industrial",
                                                 zone_short == "CA" ~ "Commonwealth land",
                                                 zone_short %in% c("R1Z","GRZ") ~ "General residential",
                                                 zone_short %in% c("DZ",
                                                                   "CCZ",
                                                                   "CDZ",
                                                                   "MUZ",
                                                                   "ACZ",
                                                                   "PDZ")     ~ "Mixed use",
                                                 substr(zone_short,1,1) =="B" ~ "Mixed use",#https://www.pc.gov.au/research/completed/vic-commercial-zoning/vic-commercial-zoning.pdf
                                                 zone_code == "C1Z|C3Z"~            "Mixed use", #c1 replaced the B zones. 
                                                 zone_short == "RGZ" ~ "Residential Growth",
                                                 zone_short == "UGZ" ~ "Greenfield",
                                                 substr(zone_short,1,1) =="C" ~ "Commercial only",
                                                 substr(zone_short,1,1) %in% c("R","F") ~ "Rural/regional",
                                                 substr(zone_short,1,2) == "GW" ~ "Rural/regional",
                                                 zone_short == "TZ" ~ "Rural/regional",
                                                 zone_short %in% c("PPRZ","PUZ","PCRZ","SUZ","UFZ","TRZ") ~ "Civic land",
                                                 zone_short == "NRZ" ~ "Neighbourhood residential",
                                                 zone_short == "LDRZ" ~ "Low density residential",
                          T ~ zone_short)) %>% 
  mutate(zoning_permits_housing = case_when(zone_short %in% c("General residential",
                                                     "Low density residential",
                                                     "Mixed use",
                                                     "Residential Growth",
                                                     "Neighbourhood residential",
                                                     "Greenfield") ~ "Housing permitted",
                                   zone_short == "Rural/regional" ~ "Rural/regional",
                                   T ~ "Housing not generally permitted"))
                                                 
                          
                          
st_write(obj = zones, 
         dsn = con, 
         Id(schema="public", 
            table = 'vicmap_zones'))

zones %>% 
  mutate(zones_for_paul_only_four_options = case_when(zoning_permits_housing == "Housing not generally permitted" ~ "Housing not generally permitted",
                                                      zone_short %in% c("Low density residential",
                                                                        "Neighbourhood residential",
                                                                        "Rural/regional",
                                                                        "Greenfield") ~ "NRZ and other low density zones",
                                                      zone_short %in% c("Residential Growth","Mixed use") ~ "Higher density zones",
                                                                        T ~ zone_short) ) %>% 
  st_drop_geometry() %>% 
  group_by(zone_short,
           zones_for_paul_only_four_options,
           zone_code,
           zone_description
          ) %>% 
  summarise(n=n()) %>% view()
  write_csv("zone_types_for_Paul.csv")


