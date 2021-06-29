rm(list=ls())

library(raster)
library(rgdal)
library(dplyr)
library(ggpubr)

NDVI_raster <- stack("/Users/maxgotts/Desktop/MPALA/Maps/MODUS\ Data/6-3-6-18/MOD13Q1.006__250m_16_days_NDVI_doy2021145_aid0001.tif")

NDVI <- as.data.frame(NDVI_raster, xy = TRUE)
NDVI <- filter(NDVI, !is.na(MOD13Q1.006__250m_16_days_NDVI_doy2021145_aid0001))
NDVI$NDVI <- NDVI$MOD13Q1.006__250m_16_days_NDVI_doy2021145_aid0001 * .0001
NDVI$MOD13Q1.006__250m_16_days_NDVI_doy2021145_aid0001 <- NULL
colnames(NDVI) <- c("Longitude","Latitude","NDVI")