
#one-time use
#renv::init()
#save packages status
#renv::snapshot()
#load package from save file
renv::restore()

library(DBI)
library(RPostgres)
library(sf)
library(tidyverse)
library(httr)
library(jsonlite)
library(leaflet)
library(furrr)

# create connection to postgres 
con <- dbConnect(RPostgres::Postgres(),
                 host = 'localhost', # host name, can be website/server
                 port = 5433, # default port of postgres
                 dbname = 'postgres', # name of database in postgres
                 user = 'postgres', # the default user
                 password = rstudioapi::askForPassword(), # password of user
                 options="-c search_path=public" # specify what schema to connect to
)

options(digits=6)

###API KEYS

#Public transport locations
#https://www.ptv.vic.gov.au/footer/data-and-reporting/datasets/ptv-timetable-api/
#Sys.setenv("PTV_USER_ID" = rstudioapi::showPrompt(title = "Enter details",
#                                                  message="Enter PTV username"))
#Sys.setenv("PTV_API_KEY" = rstudioapi::showPrompt(title = "Enter details",
#                                                  message = "Enter PTV API key"))

#Heritage database
#Get an AKI key from https://www.developer.vic.gov.au/
#Sys.setenv(vic_gov_api = rstudioapi::showPrompt(title = "Enter developer vic api key",
#                                                  message="Enter") )

