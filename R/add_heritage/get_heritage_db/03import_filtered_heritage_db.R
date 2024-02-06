#filter heritage_db

import_filtered_heritage_db <- function(){
  
  read_csv("working_data/heritage_db.csv") %>% 
  filter(!(heritage_authority_name %in% c("National Trust",
                                          'Victorian Heritage Inventory',
                                          'Vic. War Heritage Inventory'
  )),
  !(status %in% c('Not researched - evaluate later',
                  'Heritage Inventory Site',
                  'Recommended for VHI',
                  'Victorian Heritage Inventory',
                  'Recommended for VHI',
                  'Not Recommended',
                  'Demolished/Removed',
                  'Included in a Significant Landscape Overlay',
                  'Rec for other form of protection'))) %>% 
  mutate(status = case_when(status %in% c('Incl in HO area non-contributory',
                                          'Incl in HO area not sig',
                                          'Rec for HO area not sig') ~ "Overlay - Not Signficant",
                            status %in% c('Incl in HO area indiv sig',
                                          'Incl in HO area Significant',
                                          'Rec for HO area indiv sig') ~ "Overlay - Significant",
                            status %in% c('Incl in HO area contributory',
                                          'Rec for HO area contributory') ~ "Overlay - Contributory",
                            status %in% c('2003','Stage 2 study complete',
                                          'Recommended for Heritage Overlay',
                                          'Included in Heritage Overlay') ~ "Overlay - Type of Listing Not Recorded",
                            status %in% c('Recommended for VHR',
                                          'Registered') ~ "Victorian Heritage Register",
                            T~'other')) %>% 
  filter(!is.na(heritage_authority_name)) %>% 
  select(status,
         latitude,
         longitude,
         id,
         heritage_authority_name) %>% 
  filter(!is.na(latitude)) %>% 
  st_as_sf(coords = c("longitude","latitude"),
           crs = "wgs84")
  
}
