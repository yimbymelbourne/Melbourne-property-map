# Dwellings in Melbourne

This project provides a map of every physical piece of land in Melbourne, with an estimate of how many residential dwellings are on each site. It also includes information on how many new dwellings have been constructed on the site since 2005. The data is provided as R code which makes a shapefile for use in mapping software such as QGIS. The file is reproducible using R.  

# Data Source

The data comes from a database of residential dwellings in Victoria's [Major Development Program](https://www.planning.vic.gov.au/guides-and-resources/data-and-insights/urban-development-program/urban-development-program-2022-metropolitan-melbourne/major-redevelopment) which contains the best list of residential dwellings in Melbourne. 

# Data Quality

The Department released a list of every piece of physical land in 2016 (stock), as well as physical land with new dwellings from 2005 to 2022 (flow). 

Because the department releases the data in yearly files, it's not simple to bring them together into a single view of what housing stock there is in 2022. Some lots get subdivided, consolidated, or developed more than once. 

The source files have different information for the period 2005-2016,(the pre stock flow data), to after 2016 (post stock flow data). While the post-stock flow data has more detailed information about each development, it only has information about developments of at least 10 dwellings or more. As a result, any small development since 2016 has not been included in this data. 

# Data cleaning process

The cleaning process in the file aims to keep a row for each piece of physical land in Melbourne, with information about it's most recent development. 

To do this we start with the 2016 'stock' map of Melbourne and complete the following steps with the 'flow' data: 
1) Only the most recent redevelopment on a piece of land is considered. 
2) Where there has been no consolidation or subdivision of land since 2016, information about the development is joined to the stock data
3) Where redevelopment since 2016 has subdivided or consolidated the land, the new properties replace the older properties in the data
4) Where a pre-2016 development is the most recent in the flow data, the 2016 geometry is utilised, with data about development joined to the data. Where the land has been subdivided in the 2016 stock data, new dwellings in the pre 2016 flow data are divided evenly amonst the 2016 geometries. 

# Other data sources

What if you want developments <10 post 2016? Another dataset that might enable you to do so is the vicmap property or parcel data. This data contains one row for each legal piece of 'land' - here land is not physical land but rather legal rights to property. So an apartment building might have a property/parcel for each apartment, as well as one for the body corporate or driveway. 

This data is more detailed and complex - but 'properties' include things that aren't dwellings such as driveways or commercial/industrial properties. 

One option to get a more accurate list of new dwellings could be to import the parcel data, then filter to parcels that have been created in residential areas since 2016 with fewer than 10 properties.

# Reproducibility

To reproduce this data, first download the stock and flow data from Victoria's website: 

Place in a subfloder labelled data_stock:
* [stock data 2016](https://datashare.maps.vic.gov.au/search?q=uuid%3Dec2429b3-0ad5-50ff-affb-b12f7803a73f)

Place in a subfolder labelled data_flow:
* [Flow data from 2016 to 2022 (one file for each year)](https://datashare.maps.vic.gov.au/search?q=Major%20Redevelopment%20Sites)
* [Flow data from 2005 to 2016](https://datashare.maps.vic.gov.au/search?q=uuid%3D9893c72d-f85f-57ea-ab2c-076ca6f9c758)

Set projection as Geographicals on GDA2020, no buffer, ESRI Shapefile, select all area available
The code will then run in R. 

Within the data_flow folder, place each group of files in a subfolder referencing the year this data represents e.g. data_flow/2022/.... The data from the department comes in many subfolders - leave the data structure as it comes.  

# Data Dictionary

There is one row for each piece of physical land in Greater Melbourne. 

"id_stock"   
An internal ID created by this project. This id is unique for every row in the dataset            

"id_flow"                
An internal ID created by this project. This id is unique for every change in the number of dwellings that have occured. In the case where a 2005-2016 development is across more than one piece of physical land in the 2016 dataset, there will be duplicates of this id. 

"development_project_id" 
Project ID for each development from the flow data in 2016-2022. 

"projname"               
Project name for each development from the flow data in 2016-2022. 

"status"                 
The current status of the project. Values are either: 

1. "development from old dataset before 2016" - For old developments
2. "Completed" - Development finished in the most recent flow data.
3. "Under Costruction" - Development under construction in the most recent flow data. 
4. "leftover land from a development" - Where there is a large piece of land in the stock data, then the flow data divides the property up into smaller lots with a development only on part of the land, this data is the leftover land. This might be area that is being redeveloped at a later date, but often this land is a park, or even an error in the data. 
5. NA - No development on this site. 

The original flow data also has information about planned projects - this data has not been included. 

"id_stock_former"        
Where status is "leftover land from a development" - this is the old larger id_stock that this piece of land came from. 

"year_development"
Year the development was completed in the flow data. (It's unclear what this represents pre 2016, although it's likely when the development was completed).

"year_data"              
The most recent year that this development appears in the flow data.

"dwellings_in_2016"
The number of dwellings that were on the property in 2016 - this is the most reliable number of dwellings on the land. 

"dwellings_total_new"    
The total new dwellings in the most recent flow data for this lot. This is not the net new dwellings but the new dwellings. So if a house is demolished and turned into a duplex this number will be 2. 

"dwellings_net_new"     
The number of net new dwellings in the development - only present for 2005-2016 data.  

"height_storeys"         
Maximum height of any development (only present since 2016).

"vacant"                 
Whether the property was vacant in 2016. 
         
"zonecode"               
Zoning of the property (Unclear if this is the zoning currently or in 20016)

"lat" / "lon"
lat/lon for the centroid of each development

"geometry"               
geometric data describing the area of the property

