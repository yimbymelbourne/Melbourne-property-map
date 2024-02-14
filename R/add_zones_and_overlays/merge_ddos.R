#This script takes each property from the property database and finds out which DDO has the greatest overlap with it. 

source("R/00renv.R") #log into DB, load common packages

dbExecute(con, "UPDATE dwellings_urban_development_program_buffered
SET geometry = ST_MakeValid(geometry)
WHERE NOT ST_IsValid(geometry);")


#Create indexes....
dbExecute(con, 'CREATE INDEX ON dwellings_urban_development_program_buffered USING GIST (geometry);')


##DDOS

sql_query <- "UPDATE vicmap_ddos
SET geoms = ST_MakeValid(geoms)
WHERE NOT ST_IsValid(geoms);"

sql_query <- 'CREATE INDEX ON vicmap_ddos USING GIST (geoms);'
dbExecute(con, sql_query)


dbExecute(con, sql_query)

dbExecute(con,"DROP TABLE IF EXISTS dwellings_udp_ddos")

sql_query <- "CREATE TABLE dwellings_udp_ddos AS
SELECT 
    d.lat, d.lon,
    v.ddo
FROM 
    dwellings_urban_development_program_buffered d
LEFT JOIN LATERAL (
    SELECT 
        vicmap_ddos.ddo,
        vicmap_ddos.geoms  -- Required for the ST_Intersects and ST_Area conditions
    FROM 
        vicmap_ddos
    WHERE 
        ST_Intersects(d.geometry, vicmap_ddos.geoms)
    ORDER BY 
        ST_Area(ST_Intersection(d.geometry, vicmap_ddos.geoms)) DESC
    LIMIT 1
) v ON true;
"

dbExecute(con, sql_query)
