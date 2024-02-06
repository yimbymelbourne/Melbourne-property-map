#This script takes each property from the property database and finds out which NCO has the greatest overlap with it. 

source("R/00renv.R") #log into DB, load common packages


#NC overlays
dbExecute(con,  "UPDATE vicmap_nc_overlays
SET geoms = ST_MakeValid(geoms)
WHERE NOT ST_IsValid(geoms);")

sql_query <- 'CREATE INDEX ON vicmap_nc_overlays USING GIST (geoms);'
dbExecute(con, sql_query)


dbExecute(con,"DROP TABLE IF EXISTS dwellings_udp_ncos")
sql_query <- "CREATE TABLE dwellings_udp_ncos AS
SELECT 
    d.lat, d.lon,
    v.neighbourhood_character_overlay
FROM 
    dwellings_urban_development_program d
LEFT JOIN LATERAL (
    SELECT 
        vicmap_nc_overlays.neighbourhood_character_overlay,
        vicmap_nc_overlays.geoms  -- Required for the ST_Intersects and ST_Area conditions
    FROM 
        vicmap_nc_overlays
    WHERE 
        ST_Intersects(d.geometry, vicmap_nc_overlays.geoms)
    ORDER BY 
        ST_Area(ST_Intersection(d.geometry, vicmap_nc_overlays.geoms)) DESC
    LIMIT 1
) v ON true;
"

dbExecute(con, sql_query)