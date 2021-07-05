rm(list=ls())




## SETUP ##

source('~/Desktop/MPALA/mpala.R')

library(dplyr)
library(lubridate)
library(raster)
library(rgdal)
library(dplyr)
library(ggpubr)

ws <- read.csv("/Users/maxgotts/Desktop/MPALA/Whitesheets/Whitesheet\ Processing/RawWhitesheets.csv")
df <- filter(ws,!is.na(Whitesheet.Filename),!is.na(GPS.x),!is.na(GPS.y))
df[,c("X", "X.1")] <- NULL

## USEFUL FUNCTIONS ##

military_to_24 <- function(military) { # military is a number
  military_time <- paste0(military)
  split_military_time <- strsplit(military_time,"")[[1]]
  if (length(split_military_time) == 3) split_military_time <- c("0", split_military_time)
  split_hour24_time <- c(split_military_time[1], split_military_time[2], ":", split_military_time[3], split_military_time[4])
  if (split_hour24_time[1] == 0) split_hour24_time <- split_hour24_time[2:5]
  hour24_time <- paste(split_hour24_time, collapse = "")
  return(hour24_time)
}



####### GRASS ABBREVIATIONS #######

## Convert grass abbreviations to common names and flag unknown species
abbr.pre6.16 <- read.csv("/Users/maxgotts/Desktop/MPALA/Whitesheets/Whitesheet\ Processing/SpeciesAbbreviations_pre6-16.csv")
abbr.post6.16 <- read.csv("/Users/maxgotts/Desktop/MPALA/Whitesheets/Whitesheet\ Processing/SpeciesAbbreviations_post6-16.csv")



grass_abbr <- function(string, date) {
  if (string == "") { return("") }
  before <- time_length(interval(ymd("2021-06-16"),mdy(date)),"hour") <= 0
  if (before) { 
    if (!is.na(match(string,abbr.pre6.16$Species))) {
      return(string)
    } else {
      string_clean <- gsub("[[:punct:][:blank:]]+","",tolower(string))
      for (potential_grass_ID in 1:nrow(abbr.pre6.16)) {
        raw_potential_grass <- abbr.pre6.16[potential_grass_ID,"Abbreviation"]
        potential_grass_list <- strsplit(raw_potential_grass,",")[[1]]
        potential_grasses <- gsub("[[:punct:][:blank:]]+","",tolower(potential_grass_list))
        if (!is.na(match(string_clean,potential_grasses))) { # grepl(string_clean, potential_grasses, fixed = TRUE)
          return(abbr.pre6.16[potential_grass_ID,"Species"])
        }
      }
      cat("Warning: `",string,"` does not correspond to any species in the data base (pre-6/16)\n", sep="")
      return(string)
    }
  } else if (!before) {
    if (!is.na(match(string,abbr.post6.16$Species))) {
      return(string)
    } else {
      string_clean <- gsub("[[:punct:][:blank:]]+","",tolower(string))
      for (potential_grass_ID in 1:nrow(abbr.post6.16)) {
        raw_potential_grass <- abbr.post6.16[potential_grass_ID,"Abbreviation"]
        potential_grass_list <- strsplit(raw_potential_grass,",")[[1]]
        potential_grasses <- gsub("[[:punct:][:blank:]]+","",tolower(potential_grass_list))
        if (!is.na(match(string_clean,potential_grasses))) {
          return(abbr.post6.16[potential_grass_ID,"Species"])
        }
      }
      cat("Warning: `",string,"` does not correspond to any species in the data base (post-6/16)\n", sep="")
      return(string)
    }
  }
}

grasses_abbr <- function(vec,date) {
  new_vec <- c()
  for (grid in 1:length(vec)) {
    gr <- vec[grid]
    new_vec <- c(new_vec, grass_abbr(gr,date[grid]))
  }
  return(new_vec)
}


df$Grass.spp.1 <- grasses_abbr(df$Grass.spp.1,df$Date)
df$Grass.spp.2 <- grasses_abbr(df$Grass.spp.2,df$Date)
df$Grass.spp.3 <- grasses_abbr(df$Grass.spp.3,df$Date)
df$Grass.spp.4 <- grasses_abbr(df$Grass.spp.4,df$Date)


cat("Grass abbreviations done\n")


####### GRASS COLOUR ABBREVIATIONS #######
grass_colour <- data.frame(x=c("B","BG","GB","G"),
                           y=c("brown","brown with some green","green with some brown","green"))
df$Grass.color <- find_replace(df$Grass.color,grass_colour)
cat("Grass colours done\n")


####### BUSH TYPE ABBREVIATIONS #######
bush_type <- data.frame(x=c("LB","MB","TB","OG","LB,MB"),
                        y=c("light","medium", "thick","open grassland","medium"))
df$Bush.type <- find_replace(df$Bush.type,bush_type)
cat("Bush type done\n")


####### ACTIVITY ABBREVIATIONS #######
activity <- data.frame(x=c("St", "Wa", "Gr", "Dr", "Re"),
                       y=c("Standing", "Walking", "Grazing", "Drinking", "Resting"))
df$Activity <- find_replace(df$Activity,activity)
cat("Activity done\n")


####### RAIN ABBREVIATIONS #######
rain <- data.frame(x=c("NR","LR","HR"),
                   y=c("no rain","light","heavy"))
df$Rain <- find_replace(df$Rain,rain)
cat("Rain done\n")


####### SUN ABBREVIATIONS #######
sun <- data.frame(x=c("FS","PS","NS"),
                  y=c("full","part","no sun"))
df$Sun <- find_replace(df$Sun,sun)
cat("Sun done\n")


####### WIND ABBREVIATIONS #######
wind <- data.frame(x=c("NW","LW","MW", "SW"),
                   y=c("no wind","light","medium", "strong"))
df$Wind <- find_replace(df$Wind,wind)
cat("Wind done\n")



####### SPECIES ABBREVIATIONS #######
species_abbr <- data.frame(x=c("GZ",
                               "PZ",
                               "Cattle",
                               "Cam",
                               "SH",
                               "Mpala Cattle",
                               "Community Cattle",
                               "Zainab Camels",
                               "Community Kaparo Cattle",
                               "Community Camels"
                               ),
                           y=c("GZ",
                               "PZ",
                               "Cattle",
                               "Camel",
                               "Sheep",
                               "MC",
                               "CC",
                               "ZC",
                               "CKC",
                               "Comm_Camel"))
df$Species <- find_replace(df$Species,species_abbr)

# cattle.abbr <- c("Cattle","CKC","CC","MC")
# camel.abbr <- c("Camel","ZC","Comm_Camel")
# zebra.abbr <- c("GZ","PZ")
df$QuickSpecies <- NA
df[df$Species%in%cattle.abbr,"QuickSpecies"] <- "Cattle"
# df[df$Species%in%zebra.abbr,"QuickSpecies"] <- "Zebra"
df[df$Species%in%camel.abbr,"QuickSpecies"] <- "Camel"
df[df$Species=="PZ","QuickSpecies"] <- "PZ"
df[df$Species=="GZ","QuickSpecies"] <- "GZ"
cat("Species & QuickSpecies done\n")

####### EXTRACT DATE-TIME #######
df$Year <- NA
df$Month <- NA
df$Day <- NA
df$Hour <- NA
df$Minute <- NA
for (dazzle in 1:nrow(df)) {
  formatted_t <- paste(df[dazzle,"Date"],military_to_24(df[dazzle,"Time"]))
  t <- mdy_hm(formatted_t) # Date-time at the current dazzle
  df[dazzle,"Year"] <- year(t)
  df[dazzle,"Month"] <- month(t)
  df[dazzle,"Day"] <- day(t)
  df[dazzle,"Hour"] <- hour(t)
  df[dazzle,"Minute"] <- minute(t)
}
cat("Extract time done\n")



####### SPLIT OTHER SPECIES #######
df$Other.species.1 <- NA
df$Other.species.2 <- NA
df$Other.species.3 <- NA

for (dazzle in 1:nrow(df)) {
  other.species <- c("ERROR","ERROR","ERROR")
  if (df[dazzle,"Other.species"] == "None") {
    other.species <- c("","","")
  } else {
    other.species <- c(strsplit(df[dazzle,"Other.species"],",")[[1]],"","","","","")[1:3]
  }
  df[dazzle,c("Other.species.1","Other.species.2","Other.species.3")] <- other.species
}
df$Other.species <- NULL

cat("Split other species done\n")

####### NUMBER OF GRASS SPECIES #######
df$Number.grasses <- (df$Grass.spp.1!="")+(df$Grass.spp.2!="")+(df$Grass.spp.3!="")+(df$Grass.spp.4!="")
cat("Total other species done\n")


####### REMOVE EXTRANEOUS #######
df$GPS.x <- NULL
df$GPS.y <- NULL
df$Distance <- NULL
df$Direction <- NULL
df$Corrected.GPS.x <- NULL
df$Corrected.GPS.y <- NULL
cat("Remove extraneous columns done\n")


####### TOTAL ANIMALS #######
colnames(df)[23] <- "Total.animals"
cat("Total.zebras renamed\n")


####### CONVERT LAT/LONG TO NUMERIC #######
df$Longitude <- as.double(df$Longitude)
df$Latitude <- as.double(df$Latitude)
cat("Long/Lat to numeric done\n")


####### HABITAT, NDVI, EVI #######
df$Primary.habitat <- NA # Closest
df$Secondary.habitat <- NA # Second-closest
df$Tertiary.habitat <- NA # Furthest
df$Distance.secondary <- NA # Distance to second-closest
df$Distance.tertiary <- NA # Distance to furthest
df$NDVI <- NA
df$EVI <- NA

Habitat <- read.csv("/Users/maxgotts/Desktop/MPALA/Maps/Habitat/Habitat.csv")

for (dazzle in 1:nrow(df)) {
  SortedHabitat <- Habitat %>% mutate("Distance" = ((Longitude - df$Longitude[dazzle])^2 + (Latitude - df$Latitude[dazzle])^2)) %>% 
    arrange(Distance)
  df[dazzle,"Primary.habitat"] <- SortedHabitat[1,"Habitat"]
  df[dazzle,"Secondary.habitat"] <- SortedHabitat[2,"Habitat"]
  df[dazzle,"Distance.secondary"] <- SortedHabitat[2,"Distance"]
  df[dazzle,"Tertiary.habitat"] <- SortedHabitat[3,"Habitat"]
  df[dazzle,"Distance.tertiary"] <- SortedHabitat[3,"Distance"]
  df[dazzle,"NDVI"] <- SortedHabitat[1,"NDVI"]
  df[dazzle,"EVI"] <- SortedHabitat[1,"EVI"]
}
cat("Habitat, NDVI, and EVI done\n")



####### DISTANCE TO MOB #######
df$Distance.from.cattle <- NULL



df$Distance.to.mob <- NA
df$Closest.mob.size <- NA
days <- 3

mobs <- filter(df, Species%in%cattle.abbr)
for (dazzle in 1:nrow(df)) {
  if (!(df[dazzle,"Species"]%in%zebra.abbr)) next
  mob.s <- mobs
  mob.s$DaysTillZebra <- time_length(interval(mdy(mob.s$Date),mdy(df[dazzle,"Date"])),"day")
    # + => cattle are before zebras; - => cattle are after after zebras
  mob.s <- filter(mob.s, DaysTillZebra<=days, DaysTillZebra>=0)
  
  if (nrow(mob.s) == 0) {
    df[dazzle,"Distance.to.mob"] <- NA #1
    df[dazzle,"Closest.mob.size"] <- NA
    next
  }
  
  mob.s.arr <- mob.s %>% mutate("Distance" = ((Longitude - df$Longitude[dazzle])^2 + (Latitude - df$Latitude[dazzle])^2)) %>%
    arrange(Distance)
  df[dazzle,"Distance.to.mob"] <- mob.s.arr[1,"Distance"]
  df[dazzle,"Closest.mob.size"] <- mob.s.arr[1,"Total.animals"]
}
df$Scaled.mob.size <- df$Closest.mob.size/(df$Distance.to.mob+1e-10)
cat("Distance to mob etc. done\n")



####### CONVERT NA TO "" #######
for (coln in colnames(df)) {
  column <- df[,coln]
  column[is.na(column)]<-""
  df[,coln] <- column
}
cat("NA to empty string done\n")


####### ORDER CSV #######
df <- df %>% arrange(Photos.begin) 
cat("Data frame ordered\n")




####### WRITE OUT #######
cat("Writing...\n")

write.csv(df,paste0("/Users/maxgotts/Desktop/MPALA/Whitesheets/BACKUP/ConvertedWhitesheets_",
                        today(),".csv"), row.names=FALSE)
write.csv(df,"/Users/maxgotts/Desktop/MPALA/Whitesheets/ConvertedWhitesheets.csv", row.names=FALSE)


if (FALSE) {
  source('/Users/maxgotts/Desktop/MPALA/Whitesheets/Whitesheet Processing/ConvertAbbrevations.R')
}




########################################################################################################






###### DELETED NDVI, HABITAT CODE ######
# df$NDVI <- NA
# df$EVI <- NA

# NDVI_raster <- stack("/Users/maxgotts/Desktop/MPALA/Maps/MODUS\ Data/6-3-6-18/MODUS_NDVI_6-3-6-18.tif")
# NDVI <- as.data.frame(NDVI_raster, xy = TRUE)
# colnames(NDVI) <- c("Longitude","Latitude","raw.NDVI")
# NDVI <- filter(NDVI, !is.na(raw.NDVI))
# NDVI$NDVI <- NDVI$raw.NDVI * .0001
# NDVI$raw.NDVI <- NULL
# 
# EVI_raster <- stack("/Users/maxgotts/Desktop/MPALA/Maps/MODUS\ Data/6-3-6-18/MODUS_EVI_6-3-6-18.tif")
# EVI <- as.data.frame(EVI_raster, xy = TRUE)
# colnames(EVI) <- c("Longitude","Latitude","raw.EVI")
# EVI <- filter(EVI, !is.na(raw.EVI))
# EVI$EVI <- EVI$raw.EVI * .0001
# EVI$raw.EVI <- NULL
# VI <- read.csv("/Users/maxgotts/Desktop/MPALA/Maps/MODUS Data/VegIndex.csv")
# 
# 
# for (dazzle in 1:nrow(df)) {
#   vi.arr <- VI %>% mutate("Distance" = ((Longitude - df$Longitude[dazzle])^2 + (Latitude - df$Latitude[dazzle])^2)) %>% 
#     arrange(Distance)
#   df[dazzle,"NDVI"] <- vi.arr[1,"NDVI"]
#   df[dazzle,"EVI"] <- vi.arr[1,"EVI"]
# }
# 
# Habitat_raster <- stack("/Users/maxgotts/Desktop/MPALA/Maps/Habitat/Habitat_2021_06_30_clipped.tif")
# Habitat <- as.data.frame(Habitat_raster, xy = TRUE)
# colnames(Habitat) <- c("Longitude","Latitude","Habitat")
# Habitat <- filter(Habitat, !is.na(Habitat))
# 
# bush <- data.frame(inp=0:3,out=c(NA,"OG","LB","MB"))
# 
# df$Primary.habitat <- find_replace(df$Primary.habitat, bush)
# df$Secondary.habitat <- find_replace(df$Secondary.habitat, bush)
# df$Tertiary.habitat <- find_replace(df$Tertiary.habitat, bush)



#[order(df$Photos.begin),]