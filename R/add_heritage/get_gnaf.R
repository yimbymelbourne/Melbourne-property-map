create_gnaf_dataset <- function(){
  
  
  
  gnaf_geo <- read_delim("working_data/g-naf_nov23_allstates_gda2020_psv_1013/G-NAF/G-NAF NOVEMBER 2023/Standard/VIC_ADDRESS_DEFAULT_GEOCODE_psv.psv", 
                         delim = "|")%>% 
    filter(is.na(DATE_RETIRED)) %>% 
    select(-DATE_CREATED, -DATE_RETIRED) 
  
  gnaf_address <-    read_delim("working_data/g-naf_nov23_allstates_gda2020_psv_1013/G-NAF/G-NAF NOVEMBER 2023/Standard/VIC_ADDRESS_DETAIL_psv.psv",
                                delim = "|") %>% 
    filter(is.na(DATE_RETIRED)) %>% 
    select(-DATE_CREATED, -DATE_RETIRED)  
  
  
  gnaf_street <- read_delim("working_data/g-naf_nov23_allstates_gda2020_psv_1013/G-NAF/G-NAF NOVEMBER 2023/Standard/VIC_STREET_LOCALITY_psv.psv",
                            delim = "|") %>% 
    filter(is.na(DATE_RETIRED)) %>% 
    select(GNAF_STREET_PID,STREET_NAME,STREET_TYPE_CODE,GNAF_STREET_PID,STREET_LOCALITY_PID)
  
  gnaf_locality <- read_delim("working_data/g-naf_nov23_allstates_gda2020_psv_1013/G-NAF/G-NAF NOVEMBER 2023/Standard/VIC_LOCALITY_psv.psv",
                              delim = "|") %>% 
    filter(is.na(DATE_RETIRED)) %>% 
    select(LOCALITY_PID,
           LOCALITY_NAME,
           PRIMARY_POSTCODE,
           GNAF_LOCALITY_PID)
  
  gnaf <- gnaf_geo %>% 
    left_join(gnaf_address) %>% 
    left_join(gnaf_street) %>% 
    left_join(gnaf_locality) %>% 
    filter(!is.na(LATITUDE)) %>% 
    select(NUMBER_FIRST, STREET_NAME, POSTCODE, STREET_TYPE = STREET_TYPE_CODE, LOCALITY_NAME,LATITUDE,LONGITUDE) %>% 
    distinct(NUMBER_FIRST, STREET_NAME, POSTCODE, STREET_TYPE,LOCALITY_NAME,.keep_all = T )
  
  return(gnaf)
}
