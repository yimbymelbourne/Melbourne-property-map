#Get all of Victoria's features_of_interest and write to a database
source("R/00renv.R")

vicmap_pois_rest_api_url <- 'https://services6.arcgis.com/GB33F62SbDxJjwEL/ArcGIS/rest/services/Vicmap_Features_of_Interest/FeatureServer/3'

fois <- arcpullr::get_spatial_layer(vicmap_pois_rest_api_url) %>% 
  mutate(feature_type_short = case_when( feature_type %in% c("reserve",
                                                             "sport facility",
                                                             "community space",
                                                             "pipeline facility",
                                                             "power facility",
                                                             "recreational resource",
                                                             "dumping ground",
                                                             "emergency facility",
                                                             "landmark",
                                                             "excavation site",
                                                             "education centre",
                                                             "admin facility",
                                                             "hospital",
                                                             "industrial facility",
                                                             "defence site",
                                                             "storage facility", # industrial storage...
                                                             "place" #this one is a heritage site labelling - only has 3 rows. 
  ) ~ "not developable land",
  feature_type %in% c("residential building",
                      "commercial facility",
                      "storage facility",
                      "agricultural area") ~ "developable land",
  T ~ "error"))

st_write(obj = fois, 
         dsn = con, 
         Id(schema="public", 
            table = 'vicmap_fois'))

