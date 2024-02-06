#Recreating this data takes time and effort. 
#This file is designed to give you an idea of how to do it,
#but there hasn't been extensive testing and you might need to edit the files to make them work on your machine. 
#They rely on a vast array of datasets that change frequently, and so some manual fiddling is required. 

#Some pieces of code take a long time to run, so strap yourselves in! 
#The model relies upon the baseline property db in create_property_db, but everything else can be added as needed in a modular fashion. 

### Get ready ###
#Install POSTGRES. I used POSTGRES 15. 
#Get API keys for PTV, as well as the VICGOV heritage API
#update 00renv.R with the details of your database + api keys.
#Install ORS for distance to PT in a docker. This is a good instruction manual. 
#The settings files for ORS can be a pain, so they have been included in this repo under ors

##### Download dwellings data #####
#Dwelling data is only available through a manual download

#To reproduce this data, first download the stock and flow data from Victoria's website:

#Place in a subfloder labelled data/data_stock:

 #   Stock data 2016 https://datashare.maps.vic.gov.au/search?q=uuid%3Dec2429b3-0ad5-50ff-affb-b12f7803a73f

#Place in a subfolder labelled data/data_flow:
# Flow data from 2016 to 2022 https://datashare.maps.vic.gov.au/search?q=Major%20Redevelopment%20Sites
#Within the data_flow folder, place each group of files in a subfolder referencing the year this data represents e.g. data_flow/2022/.... 

#The data from the department comes in many subfolders - leave the data structure as it comes.

#Set projection as Geographicals on GDA2020, no buffer, ESRI Shapefile, select all area available The code will then run in R.

# Currently 2022 data was manually requested from the dept, although in time this will probably be on mapshare. 


#once this is complete run
source("R/01create_property_db/create_property_db_from_udp.R")

##### Get heritage DB ##### - est time to run - 24 hours 

#The goal is to create a table that has the same lat,lon values as the dwelling database above, with details of the heritage associated with each parcel of land. 

#Heritage database information has information about the level of heritage of a property. It's invaluable for the small number of heritage register properties
# but also somewhat useful for some councils who have submitted heritage significance details for some of their properties (e.g. Yarra)
# Start by running the heritage DB downloader. This needs to be run over 24 hours due to a rate limit on the VIC gov API. 
source("R/add_heritage/get_heritage_db/01get_json_from_web.R")
source("R/add_heritage/get_heritage_db/02turn_json_into_tibble.R")
source("R/add_heritage/get_heritage_db/03import_filtered_heritage_db.R")
source("R/add_heritage/get_heritage_db/add_other_councils.R") # This is port phillip data which was emailed manually. Contact repo owner for a copy! 

#Now merge the heritage db to the property database, resulting in a dataset that has one row for each property with a heritage DB entry
source("R/add_heritage/merge_heritage_db_data.R")

#Now get the broader heritage overlays - these are polygons that state if there is any HO on a property. Merge that with the property database
source("R/add_heritage/get_heritage_overlays.R")
source("R/add_heritage/merge_heritage_overlays.R")


##### Other vicmap info #####
#The goal is to create a table that has the same lat,lon values as the dwelling database above, with details of the zoning, overlays, and features of itnerest of each parcel of land. 

# Other info from VICMAP includes zones, Design and Development Overlays and Neighbourhood Character overlays. 
# For each of these we download and then join based on the zone/overlay with the greatest overlap of the property. 
source("R/add_zones_and_overlays/get_overlays.R") # ddos and ncos are both types of overlay
source("R/add_zones_and_overlays/merge_ddos.R")
source("R/add_zones_and_overlays/merge_ncos.R")

source("R/add_zones_and_overlays/get_zones.R")
source("R/add_zones_and_overlays/merge_zones.R")

source("R/add_fois/get_foi.R")
source("R/add_fois/merge_foi.R")


##### Add Public Transport ##### - est time to run - 24 hours 
#The goal is to create a table that has the same lat,lon values as the dwelling database above, with details near public transport to each parcel of land 

#This has two elements crow flies distance and walking distance

# To add crow flies distance start by downloading PT data. 
source("R/add_public_transport/get_pt_stops.R")

#Then find any PT near each property (within 2km)
source("R/add_public_transport/add_pt.R")

#Now find the walking distance/time to each stop (within 1.5km)
source("R/add_public_transport/add_ors_distance.R")

#Now fid the **closest** stop to each location and put it into a wide dataframe
source("R/add_public_transport/combine_both_pt.R")

##### Add Traffic #####  - est time to run - 12 hours 

#The goal is to create a table that has the same lat,lon values as the dwelling database above, with details of traffic pollution near these parcels of land
#This feature is still experimental

#Download traffic volume shape files from the Vic government website 
#https://vicroadsopendata-vicroadsmaps.opendata.arcgis.com/datasets/traffic-volume
source("R/add_traffic/add_traffic.R")


##### Add ASGS #####  - est time to run - 12 hours 
#Finally we add addrress details like suburb, LGA etc. 
source("R/add_misc/add_asgs.R")



##### Compile into one file #####  - est time to run - 12 hours 
#Finally we add addrress details like suburb, LGA etc. 
source("R/02compile.R")




