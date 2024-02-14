
dbExecute(con,"-- Create a buffered table with geometries buffered by -1m, transformed for Melbourne, Victoria
CREATE TABLE dwellings_urban_development_program_buffered AS
SELECT 
lat, 
lon,
ST_Transform(
  ST_Buffer(
    ST_Transform(geometry, 3111),  -- Transform to Vicgrid94 (EPSG:3111)
    -1.5  -- Apply a -1 meter buffer
  ), 
  7844  -- Transform back to GDA2020 (EPSG:7844)
) AS geometry
FROM 
dwellings_urban_development_program;")
