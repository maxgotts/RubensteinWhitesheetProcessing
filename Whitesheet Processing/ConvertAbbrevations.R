rm(list=ls())



## SETUP ##

source('~/Desktop/MPALA/mpala.R')

library(dplyr)
library(lubridate)
library(dplyr)
library(ggpubr)

ws <- read.csv("/Users/maxgotts/Desktop/MPALA/Whitesheets/Whitesheet\ Processing/RawWhitesheets.csv")
df <- filter(ws,!is.na(Whitesheet.Filename),!is.na(GPS.x),!is.na(GPS.y))
df[,c("X", "X.1")] <- NULL


####### DATE TO DAYS #######
df$Days <- days_since_start(mdy(df$Date))


####### SPECIES ABBREVIATIONS #######
species_abbr <- data.frame(x=c("GZ",
                               "PZ",
                               "Cattle",
                               "Camel",
                               "Cam",
                               "Community Sheep",
                               "Sheep",
                               "SH",
                               "Mpala Cattle",
                               "Mpala Steers",
                               "MC",
                               "Community Cattle",
                               "Putonois Cattle",
                               "CC",
                               "Zainab Camels",
                               "Zainab Camel",
                               "ZC",
                               "Community Kaparo Cattle",
                               "Kaparo",
                               "CKC",
                               "Community Camels",
                               "Community Camel",
                               "Comm_Camel"
                            ),
                            y=c("GZ",
                                "PZ",
                                "Cattle",
                                "Camel",
                                "Camel",
                                "SH",
                                "SH",
                                "SH",
                                "MC",
                                "MC",
                                "MC",
                                "CC",
                                "CC",
                                "CC",
                                "ZC",
                                "ZC",
                                "ZC",
                                "CKC",
                                "CKC",
                                "CKC",
                                "Comm_Camel",
                                "Comm_Camel",
                                "Comm_Camel"))
df$Species <- find_replace(df$Species,species_abbr)

# cattle.abbr <- c("Cattle","CKC","CC","MC")
# camel.abbr <- c("Camel","ZC","Comm_Camel")
# zebra.abbr <- c("GZ","PZ")
df$QuickSpecies <- NA
df[df$Species%in%cattle.abbr,"QuickSpecies"] <- "Cattle"
df[df$Species%in%camel.abbr,"QuickSpecies"] <- "Camel"
df[df$Species=="PZ","QuickSpecies"] <- "PZ"
df[df$Species=="GZ","QuickSpecies"] <- "GZ"
df[df$Species=="SH","QuickSpecies"] <- "SH"
cat("* Species & QuickSpecies done\n")


####### ADD IDENTIFIER #######
df$Identifier <- NA
df[df$Species=="GZ","Identifier"] <- paste0("GZ",1:nrow(df[df$Species=="GZ",]))
df[df$Species=="PZ","Identifier"] <- paste0("PZ",1:nrow(df[df$Species=="PZ",]))
df[df$QuickSpecies=="Cattle","Identifier"] <- paste0("CT",1:nrow(df[df$QuickSpecies=="Cattle",]))
df[df$QuickSpecies=="Camel","Identifier"] <- paste0("CM",1:nrow(df[df$QuickSpecies=="Camel",]))
df[df$QuickSpecies=="SH","Identifier"] <- paste0("SH",1:nrow(df[df$QuickSpecies=="SH",]))

df <- filter(df, !(Identifier%in%blacklist))
cat("* Identifier and filter by blacklist done\n")



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


cat("* Grass.spp.1 abbreviation\n")
df$Grass.spp.1 <- grasses_abbr(df$Grass.spp.1,df$Date)
cat("* Grass.spp.2 abbreviation\n")
df$Grass.spp.2 <- grasses_abbr(df$Grass.spp.2,df$Date)
cat("* Grass.spp.3 abbreviation\n")
df$Grass.spp.3 <- grasses_abbr(df$Grass.spp.3,df$Date)
cat("* Grass.spp.4 abbreviation\n")
df$Grass.spp.4 <- grasses_abbr(df$Grass.spp.4,df$Date)


cat("* Grass abbreviations done\n")


####### GRASS COLOUR ABBREVIATIONS #######
grass_colour <- data.frame(x=c("B","BG","GB","G"),
                           y=c("brown","brown with some green","green with some brown","green"))
df$Grass.color <- find_replace(df$Grass.color,grass_colour)
cat("* Grass colours done\n")


####### BUSH TYPE ABBREVIATIONS #######
bush_type <- data.frame(x=c("LB","MB","TB","OG","LB,MB"),
                        y=c("light","medium", "thick","open grassland","medium"))
df$Bush.type <- find_replace(df$Bush.type,bush_type)
cat("* Bush type done\n")


####### ACTIVITY ABBREVIATIONS #######
activity <- data.frame(x=c("St", "Wa", "Gr", "Dr", "Re"),
                       y=c("Standing", "Walking", "Grazing", "Drinking", "Resting"))
df$Activity <- find_replace(df$Activity,activity)
cat("* Activity done\n")


####### RAIN ABBREVIATIONS #######
rain <- data.frame(x=c("NR","LR","HR"),
                   y=c("no rain","light","heavy"))
df$Rain <- find_replace(df$Rain,rain)
cat("* Rain done\n")


####### SUN ABBREVIATIONS #######
sun <- data.frame(x=c("FS","PS","NS"),
                  y=c("full","part","no sun"))
df$Sun <- find_replace(df$Sun,sun)
cat("* Sun done\n")


####### WIND ABBREVIATIONS #######
wind <- data.frame(x=c("NW","LW","MW", "SW"),
                   y=c("no wind","light","medium", "strong"))
df$Wind <- find_replace(df$Wind,wind)
cat("* Wind done\n")



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
cat("* Extract time done\n")



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

cat("* Split other species done\n")

####### NUMBER OF GRASS SPECIES #######
df$Number.grasses <- (df$Grass.spp.1!="")+(df$Grass.spp.2!="")+(df$Grass.spp.3!="")+(df$Grass.spp.4!="")
cat("* Total other species done\n")


####### REMOVE EXTRANEOUS #######
df$GPS.x <- NULL
df$GPS.y <- NULL
df$Distance <- NULL
df$Direction <- NULL
df$GPS.x <- df$Corrected.GPS.x
df$GPS.y <- df$Corrected.GPS.y
df$Corrected.GPS.x <- NULL
df$Corrected.GPS.y <- NULL
cat("* Remove extraneous columns done\n")


####### TOTAL ANIMALS #######
colnames(df)[23] <- "Total.animals"
cat("* Total.zebras renamed\n")


####### CONVERT LAT/LONG TO NUMERIC #######
df$Longitude <- as.double(df$Longitude)
df$Latitude <- as.double(df$Latitude)
cat("* Long/Lat to numeric done\n")


####### HABITAT, NDVI, EVI, VEG #######
df$Primary.habitat <- NA # Closest
df$Secondary.habitat <- NA # Second-closest
df$Tertiary.habitat <- NA # Furthest
df$Distance.secondary <- NA # Distance to second-closest
df$Distance.tertiary <- NA # Distance to furthest
df$NDVI <- NA
df$EVI <- NA

Habitat <- read.csv("/Users/maxgotts/Desktop/MPALA/Maps/HabitatTIFF.csv")
VI <- read.csv("/Users/maxgotts/Desktop/MPALA/Maps/VITIFF.csv") # Loops_1_2_
Veg <- read.csv("/Users/maxgotts/Desktop/MPALA/Vegetation/ConvertedVegetation.csv")
colnames(Veg) <- gsub("..", ".", colnames(Veg), fixed=TRUE)

for (dazzle in 1:nrow(df)) {
  SortedHabitat <- Habitat %>% mutate("Distance" = sqrt(((Longitude - df$Longitude[dazzle])^2 + (Latitude - df$Latitude[dazzle])^2))) %>% 
    arrange(Distance)
  df[dazzle,"Primary.habitat"] <- SortedHabitat[1,"Habitat"]
  df[dazzle,"Secondary.habitat"] <- SortedHabitat[2,"Habitat"]
  df[dazzle,"Distance.secondary"] <- SortedHabitat[2,"Distance"]
  df[dazzle,"Tertiary.habitat"] <- SortedHabitat[3,"Habitat"]
  df[dazzle,"Distance.tertiary"] <- SortedHabitat[3,"Distance"]
  
  SortedVI <- VI %>% mutate("Distance" = sqrt(((Longitude - df$Longitude[dazzle])^2 + (Latitude - df$Latitude[dazzle])^2))) %>% 
    arrange(Distance)
  for (date_id in 1:nrow(vi_dates)) {
    start <- vi_dates[date_id,"start"]
    end <- vi_dates[date_id,"end"]
    date_range <- paste0(start,"-",end)
    date_range_mod <- gsub("-","_",date_range)
    if (is.after(mdy(df[dazzle,"Date"]),ymd(paste0("2021-",start))) && is.before(mdy(df[dazzle,"Date"]),ymd(paste0("2021-",end)))) {
      # cat("Ahoy",date_range_mod,"\n")
      df[dazzle,"NDVI"] <- SortedVI[1,paste0("NDVI_",date_range_mod)]
      df[dazzle,"EVI"] <- SortedVI[1,paste0("EVI_",date_range_mod)]
      break
    }
  }
  
  SortedVeg <- Veg %>% mutate("Distance" = sqrt(((Longitude - df$Longitude[dazzle])^2 + (Latitude - df$Latitude[dazzle])^2))) %>% 
    arrange(Distance)
  df[dazzle,colnames(Veg)[9:19]] <- SortedVeg[1,9:19]
  
}
df$Distance.secondary <- degrees_to_meters(df$Distance.secondary)
df$Distance.tertiary <- degrees_to_meters(df$Distance.tertiary)
cat("* Habitat, NDVI, EVI, and Veg done\n")


# for (i in 1:10) {
#   cat("new:",i,"\n")
#   for (j in 1:10) {
#     cat(i," : ",j,"\n")
#     if (i == 3 && j == 5) {
#       break
#     }
#   }
# }


####### DISTANCE TO MOB #######
df$Distance.from.mob <- NA
# df$Distance.from.mob.d <- NA
df$Closest.mob.size <- NA
days <- 0

mobs <- filter(df, QuickSpecies=="Cattle")
for (dazzle in 1:nrow(df)) {
  if (!(df[dazzle,"Species"]%in%zebra.abbr)) next
  mob.s <- mobs
  mob.s$DaysTillZebra <- time_length(interval(mdy(mob.s$Date),mdy(df[dazzle,"Date"])),"day")
    # + => cattle are before zebras; - => cattle are after after zebras
  mob.s <- filter(mob.s, DaysTillZebra<=days, DaysTillZebra>=0)
  
  if (nrow(mob.s) == 0) {
    df[dazzle,"Distance.from.mob"] <- NA #1
    df[dazzle,"Closest.mob.size"] <- NA
    next
  }
  
  mob.s.arr <- mob.s %>% mutate("Distance" = sqrt((GPS.x - df$GPS.x[dazzle])^2 + (GPS.y - df$GPS.y[dazzle])^2)) %>%
    arrange(Distance)
  df[dazzle,"Distance.from.mob"] <- mob.s.arr[1,"Distance"]
  df[dazzle,"Closest.mob.size"] <- mob.s.arr[1,"Total.animals"]
  mob.s.arr <- mob.s %>% mutate("Distance" = sqrt((Longitude - df$Longitude[dazzle])^2 + (Latitude - df$Latitude[dazzle])^2)) %>%
    arrange(Distance)
  # df[dazzle,"Distance.from.mob.d"] <- sqrt((mob.s.arr[1,"Longitude"] - df$Longitude[dazzle])^2 + (mob.s.arr[1,"Latitude"] - df$Latitude[dazzle])^2)
}
cat("* Distance to mob etc. done\n")



####### APPROXIMATE DENSITY #######
df$Approximate.density <- NA
df$Cattle.density <- NA

for (dazzle in 1:nrow(df)) {
  if (df[dazzle,"Species"]%in%zebra.abbr) {
    df[dazzle,"Approximate.density"] <- df %>% filter(Species %in% zebra.abbr) %>%
      mutate("Distance" = sqrt((GPS.x - df$GPS.x[dazzle])^2 + (GPS.y - df$GPS.y[dazzle])^2)) %>%
      filter(Distance <= 1000, Date==df[dazzle,"Date"]) %>%
      dplyr::select("Total.animals") %>% sum(na.rm=T)/pi
  } else if (df[dazzle,"Species"]%in%cattle.abbr) {
    df[dazzle,"Approximate.density"] <- df %>% filter(Species%in%cattle.abbr) %>%
      mutate("Distance" = sqrt((GPS.x - df$GPS.x[dazzle])^2 + (GPS.y - df$GPS.y[dazzle])^2)) %>%
      filter(Distance <= 1000, Date==df[dazzle,"Date"]) %>%
      dplyr::select("Total.animals") %>% sum(na.rm=T)/pi
  }
  num.km <- 2
  df[dazzle,"Cattle.density"] <- (df %>% filter(Species%in%cattle.abbr) %>%
    mutate("Distance" = sqrt((GPS.x - df$GPS.x[dazzle])^2 + (GPS.y - df$GPS.y[dazzle])^2)) %>%
    filter(Distance <= num.km*1000, Date==df[dazzle,"Date"]))[,"Total.animals"] %>%
    sum(na.rm=T)/(pi*num.km^2)
}
cat("* Approximate and cattle density done\n")


####### DISTANCE TO CLOSEST HERD #######
df$Distance.from.herd <- NA
df$Closest.herd.size <- NA
df$Distance.from.herd.opp.sp <- NA
df$Closest.herd.size.opp.sp <- NA

zebras <- filter(df, Species%in%zebra.abbr)
for (dazzle in 1:nrow(df)) {
  if (!(df[dazzle,"Species"]%in%zebra.abbr)) next
  zebra.s <- zebras
  zebra.s$DaysTillZebra <- time_length(interval(mdy(zebra.s$Date),mdy(df[dazzle,"Date"])),"day")
  # + => cattle (zebras) are before zebras; - => cattle (zebras) are after after zebras
  zebra.s <- filter(zebra.s, DaysTillZebra<=days, DaysTillZebra>=0)
  
  if (nrow(zebra.s) == 0) next
  
  zebra.s.arr <- zebra.s %>% mutate("Distance" = sqrt((GPS.x - df$GPS.x[dazzle])^2 + (GPS.y - df$GPS.y[dazzle])^2)) %>%
    arrange(Distance) %>% filter(Distance > 0)
  df[dazzle,"Distance.from.herd"] <- zebra.s.arr[1,"Distance"]
  df[dazzle,"Closest.herd.size"] <- zebra.s.arr[1,"Total.animals"]
  
  if (df[dazzle,"Species"]=="GZ") opp.sp <- "PZ"
  if (df[dazzle,"Species"]=="PZ") opp.sp <- "GZ"
  zebra.s.arr.opp.sp <- filter(zebra.s.arr, Species==opp.sp)
  df[dazzle,"Distance.from.opp.sp"] <- zebra.s.arr.opp.sp[1,"Distance"]
  df[dazzle,"Closest.opp.sp.size"] <- zebra.s.arr.opp.sp[1,"Total.animals"]
}
cat("* Distance to herd etc. & opposite species done\n")


####### DISTANCE TO CLOSEST HERD #######
df$Distance.to.water <- sqrt((df$GPS.x-260184)^2 + (df$GPS.y-56014)^2)
cat("* Distance to water done\n")




####### ADD RANDOM NDVI, EVI #######
library(reshape2)
vi <- get_vi()

vi$ID <- paste0(vi$Latitude,":",vi$Longitude)
vi.sample <- melt(vi %>% dplyr::select(3,5,7,9,11) %>% sample_n(1000))
colnames(vi.sample) <- c("ID","Date","NDVI")
vi.sample$Date <- ymd(paste0("2021-",gsub("_","-",substr(vi.sample$Date,6,10))))
vi.sample$Days <- days_since_start(vi.sample$Date)
vi.sample$Species <- "Random"
vi.lm <- lm(NDVI ~ Days, vi.sample)
df$Random.NDVI <- vi.lm$coefficients['(Intercept)']+vi.lm$coefficients['Days']*df$Days
cat("* Random NDVI done\n")

# vi <- get_vi()
# vi$ID <- paste0(vi$Latitude,":",vi$Longitude)
# vi.sample <- melt(vi %>% dplyr::select(3,5,7,9,11) %>% sample_n(1000))
# colnames(vi.sample) <- c("ID","Date","EVI")
# vi.sample$Date <- ymd(paste0("2021-",gsub("_","-",substr(vi.sample$Date,6,10))))
# vi.sample$Days <- days_since_start(vi.sample$Date)
# vi.sample$Species <- "Random"
# vi.lm <- lm(EVI ~ Days, vi.sample)
# df$Random.EVI <- vi.lm$coefficients['(Intercept)']+vi.lm$coefficients['Days']*df$Days
# cat("* Random NDVI done\n")


####### CONVERT DEGREES TO METERS #######
# df$Distance.to.water <- degrees_to_meters(df$Distance.to.water)



####### CONVERT NA TO "" #######
for (coln in colnames(df)) {
  column <- df[,coln]
  column[is.na(column)]<-""
  df[,coln] <- column
}
cat("* NA to empty string done\n")



####### ORDER CSV #######
df <- df %>% arrange(Photos.begin) 
cat("* Data frame ordered\n")



####### WRITE OUT #######
cat("* Writing...\n")

write.csv(df,paste0("/Users/maxgotts/Desktop/MPALA/Whitesheets/BACKUP/ConvertedWhitesheets_",
                        today(),".csv"), row.names=FALSE)
write.csv(df,"/Users/maxgotts/Desktop/MPALA/Whitesheets/ConvertedWhitesheets.csv", row.names=FALSE)








########################################################################################################




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


# military_to_24 <- function(military) { # military is a number
#   military_time <- paste0(military)
#   split_military_time <- strsplit(military_time,"")[[1]]
#   if (length(split_military_time) == 3) split_military_time <- c("0", split_military_time)
#   split_hour24_time <- c(split_military_time[1], split_military_time[2], ":", split_military_time[3], split_military_time[4])
#   if (split_hour24_time[1] == 0) split_hour24_time <- split_hour24_time[2:5]
#   hour24_time <- paste(split_hour24_time, collapse = "")
#   return(hour24_time)
# }