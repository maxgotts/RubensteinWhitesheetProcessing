rm(list=ls())



## SETUP ##

library(dplyr)
library(lubridate)

ws <- read.csv("~/Desktop/MPALA/Whitesheets/Whitesheet\ Processing/RawWhitesheets.csv")
df <- filter(ws,!is.na(Whitesheet.Filename),!is.na(GPS.x),!is.na(GPS.y))
df[,c("X", "X.1")] <- NULL


## USEFUL FUNCTIONS ##
find_replace <- function(vec, dictionary) {
  new_vec <- rep(NA,times=length(vec))
  for (vid in 1:length(vec)) {
    v <- vec[vid]
    found <- FALSE
    for (entry in 1:nrow(dictionary)) {
      if (v == "") {
        new_vec[vid] <- ""
        found <- TRUE
      }
      if (v == dictionary[entry,1]) {
        new_vec[vid] <- dictionary[entry,2]
        found <- TRUE
      }
    }
    if (!found) {
      cat("Warning: `",v,"` not found in supplied dictionary, returned without changing\n",sep="")
      new_vec[vid] <- v
    }
  }
  return(new_vec)
}

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
abbr.pre6.16 <- read.csv("~/Desktop/MPALA/Whitesheets/Whitesheet\ Processing/SpeciesAbbreviations_pre6-16.csv")
abbr.post6.16 <- read.csv("~/Desktop/MPALA/Whitesheets/Whitesheet\ Processing/SpeciesAbbreviations_post6-16.csv")

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






####### GRASS COLOUR ABBREVIATIONS #######
grass_colour <- data.frame(x=c("B","BG","GB","G"),
           y=c("brown","brown with some green","green with some brown","green"))
df$Grass.color <- find_replace(df$Grass.color,grass_colour)


####### GRASS HEIGHT ABBREVIATIONS #######
grass_height <- data.frame(x=c("hoof","hock","belly"),
                           y=c("< hoof","< hock","< belly"))
df$Grass.height <- find_replace(df$Grass.height,grass_height)


####### BUSH TYPE ABBREVIATIONS #######
bush_type <- data.frame(x=c("LB","MB","TB","OG","LB,MB"),
                           y=c("light","medium", "thick","open grassland","medium"))
df$Bush.type <- find_replace(df$Bush.type,bush_type)


####### ACTIVITY ABBREVIATIONS #######
activity <- data.frame(x=c("St", "Wa", "Gr", "Dr", "Re"),
                       y=c("Standing", "Walking", "Grazing", "Drinking", "Resting"))
df$Activity <- find_replace(df$Activity,activity)


####### RAIN ABBREVIATIONS #######
rain <- data.frame(x=c("NR","LR","HR"),
                       y=c("no rain","light","heavy"))
df$Rain <- find_replace(df$Rain,rain)



####### SUN ABBREVIATIONS #######
sun <- data.frame(x=c("FS","PS","NS"),
                   y=c("full","part","no sun"))
df$Sun <- find_replace(df$Sun,sun)


####### WIND ABBREVIATIONS #######
wind <- data.frame(x=c("NW","LW","MW", "SW"),
                  y=c("no wind","light","medium", "strong"))
df$Wind <- find_replace(df$Wind,wind)



####### SPECIES ABBREVIATIONS #######
species_abbr <- data.frame(x=c("GZ","PZ"),
                   y=c("grevyi","quagga"))
df$Species.binomial <- find_replace(df$Species,species_abbr)


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



####### NUMBER OF OTHER SPECIES #######
# df$Number.other <- (df$Other.species.1!="")+(df$Other.species.2!="")+(df$Other.species.3!="")



####### NUMBER OF GRASS SPECIES #######
df$Number.grasses <- (df$Grass.spp.1!="")+(df$Grass.spp.2!="")+(df$Grass.spp.3!="")+(df$Grass.spp.4!="")


####### NUMBER DOING ACTIVITY #######
df$Number.standing <- ""
df$Number.grazing <- ""
df$Number.walking <- ""
df$Number.drinking <- ""
df$Number.resting <- ""

for (dazzle in 1:nrow(df)) {
  if (df[dazzle,"Activity"] != "") {
    df[dazzle,paste0("Number.",tolower(df[dazzle,"Activity"]))] <- df[dazzle,"Total.zebras"]
  }
}


####### REORDER DATA FRAME #######
df$Mpala.North <- "Mpala.North"
df$Equus <- "Equus"
df$alive <- "alive"
df$Adult <- "Adult"
df$Space <- ""

new_df <- df[,c("Multispecies","Photos.begin","Photos.end","Mpala.North",
                "Notes","Latitude","Longitude","Year","Month","Day","Hour",
                "Minute","Space","Equus","Species.binomial","alive","Space",
                "Adult","Activity",
                "Grass.height","Grass.color","Grass.spp.1","Grass.spp.2",
                "Grass.spp.3","Bush.type","Space","Other.species.1",
                "Other.species.2","Other.species.3","Rain","Sun","Wind",
                "Space","Space","Total.zebras","Number.grasses","Space","Space",
                "Space","Number.grazing","Space","Number.standing",
                "Number.walking","Space","Number.drinking","Space","Space",
                "Space","Number.resting","Space","Space","Whitesheet.Filename")]


####### CONVERT NA TO "" #######
for (coln in colnames(new_df)) {
  column <- new_df[,coln]
  column[is.na(column)]<-""
  new_df[,coln] <- column
}



####### ORDER CSV #######
new_df <- new_df[order(new_df$Photos.begin),]


####### WRITE OUT #######
write.csv(new_df,paste0("~/Desktop/MPALA/Whitesheets/BACKUP/TransformedWhitesheets_",
                        today(),".csv"), row.names=FALSE)
write.csv(new_df,"~/Desktop/MPALA/Whitesheets/TransformedWhitesheets.csv", row.names=FALSE)


if (FALSE) {
  source("~/Desktop/MPALA/Whitesheets/Whitesheet\ Processing/TransformAbbreviations.R")
}


