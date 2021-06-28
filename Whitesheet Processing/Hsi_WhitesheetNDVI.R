install.packages("raster")
library(raster)
install.packages("sp")
install.packages("rgdal")
library(rgdal)
library(dplyr)
library(tidyr)

GDALinfo("/Users/annikahsi/Downloads/MOD13Q1.006__250m_16_days_NDVI_doy2021145_aid0001.tif")

NDVI <- raster(x = "/Users/annikahsi/Downloads/MOD13Q1.006__250m_16_days_NDVI_doy2021145_aid0001.tif")

summary(NDVI)
str(NDVI)

NDVI_df <- as.data.frame(NDVI, xy = TRUE)
str(NDVI_df)

whitesheets <- readr::read_csv("/Users/annikahsi/Downloads/Gotts_ConvertedWhitesheets.csv")
gps <- whitesheets %>% select("Latitude", "Longitude")

NDVI_edited <- drop_na(NDVI_df) %>% 
    mutate("Normalized NDVI" = MOD13Q1.006__250m_16_days_NDVI_doy2021145_aid0001 * .0001)
str(NDVI_edited)

i <- 1
zebra_NDVIs <- c()

for (i in 1:nrow(gps)) {
    test_long <- gps$Longitude[i]
    test_lat <- gps$Latitude[i]
    
    zebra_NDVIs[i] <- NDVI_edited %>% mutate("Diff" = ((x - test_long)^2 + (y - test_lat)^2)) %>% 
        arrange(Diff) %>%
        slice_head() %>% 
        select("Normalized NDVI")
}
zebra_NDVIs <- zebra_NDVIs %>% unlist()