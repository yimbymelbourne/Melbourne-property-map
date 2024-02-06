#This file downloads victoria's heritage overlay information from the VICMAP servers


source("R/00renv.R")

#Get right CRS 

udp_crs <- read_sf(con,query = "SELECT geometry FROM dwellings_urban_development_program") %>% st_crs()
#Get all of Victoria's zones and write to a database


#Get all of Victoria's overlays and write to a database
vicmap_overlay_rest_api_url <- 'https://services6.arcgis.com/GB33F62SbDxJjwEL/ArcGIS/rest/services/Vicmap_Planning/FeatureServer/2'


overlays <- arcpullr::get_spatial_layer(vicmap_overlay_rest_api_url) 

st_write(obj = overlays, 
         dsn = con, 
         Id(schema="public", 
            table = 'vicmap_overlays'))


vicmap_heritage_overlays <- st_read(con, query = "SELECT zone_code,geoms FROM vicmap_overlays
                                                  WHERE scheme_code = 'HO'") %>% 
  rename(heritage_overlay = zone_code) %>% 
  st_transform(st_crs(udp_crs)) 

write_sf(obj = vicmap_heritage_overlays,
         dsn = con, 
         Id(schema="public", 
            table = 'vicmap_heritage_overlays'))