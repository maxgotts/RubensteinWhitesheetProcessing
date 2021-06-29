rm(list=ls())

library(dplyr)
df <- read.csv("~/Desktop/MPALA/Whitesheets/ConvertedWhitesheets.csv")
df_zebra <- filter(df, Species %in% c("GZ","PZ"))
df_pack <- filter(df, !(Species %in% c("GZ","PZ")), !is.na(Total.animals))
# write.csv(df_zebra,"~/Desktop/MPALA/Whitesheets/Gotts_ConvertedWhitesheets_Zebra.csv",row.names=FALSE)
write.csv(df_cattle,"~/Desktop/MPALA/Whitesheets/Gotts_ConvertedWhitesheets_Cattle.csv",row.names=FALSE)

if (FALSE) {
  source('~/Desktop/MPALA/Whitesheets/Whitesheet Processing/ZebraCattleSplit.R')
}