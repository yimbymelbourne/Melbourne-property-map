source("R/00renv.R")

#Since port phillip council has provided us with a detailed shape file, let's filter out 
#Any heritage register stuff that is in Port phillip outside of the register, then join them up. 


source("R/03import_filtered_heritage_db.R")
heritage_db <- import_filtered_heritage_db()

port_phillip <- read_sf("input_data/port_phillip/Hertiage_policy grading_gazetted.shp") %>% 
  st_make_valid() %>% 
  st_transform(st_crs(heritage_db)) %>% 
  mutate(id = paste0("port_phillip_",COPP_PFI),
         status = case_when(CATEGORY == "Contributory Heritage Place - inside HO" ~ "Overlay - Contributory", 
                            CATEGORY == "Significant Heritage Place - inside HO" ~ "Overlay - Significant" ,
                            CATEGORY == "Non Contributory - inside HO" ~ "Overlay - Not Signficant"),
         heritage_authority_name = "Port Phillip") %>% 
  select(id,
         status) %>% 
  st_centroid()

all_heritage <- heritage_db %>% 
  mutate(id = as.character(id)) %>% 
  bind_rows(port_philip) %>% 
  distinct()


# create connection to postgres 
con <- dbConnect(RPostgres::Postgres(),
                 host = 'localhost', # host name, can be website/server
                 port = 5433, # default port of postgres
                 dbname = 'postgres', # name of database in postgres
                 user = 'postgres', # the default user
                 password = rstudioapi::askForPassword(), # password of user
                 options="-c search_path=public" # specify what schema to connect to
)


write_sf(obj = all_heritage,
         dsn = con, 
         Id(schema="public", 
            table = 'heritage_database'))
