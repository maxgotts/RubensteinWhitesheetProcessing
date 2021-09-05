rm(list=ls())

source('~/Desktop/MPALA/mpala.R')
df <- get_df()

df <- filter(df, Species%in%c(zebra.abbr,cattle.abbr), !(Identifier%in%blacklist))
df$Species <- df$QuickSpecies

columns <- c("Identifier")
ss <- as.data.frame(matrix(NA,1,length(columns)))
colnames(ss) <- columns
ss <- filter(ss, !is.na(Identifier))

df$Size <- NA
for (species in unique(df$Species)) {
  df$Size[df$Species==species] <- find_replace((df$Total.animals[df$Species==species]>=quantile(df$Total.animals[df$Species==species],.4,na.rm=T)[[1]])+ 
                                               (df$Total.animals[df$Species==species]>=quantile(df$Total.animals[df$Species==species],.7,na.rm=T)[[1]]),
                                               data.frame(x=c(NA,0:2),y=c(NA,"Small","Medium","Big")))
}


df$SplitVariable <- mdy(df$Date)-min(mdy(df$Date))+1
df$SplitVariable <- floor((df$SplitVariable+1)/2)
df.sp <- split(df, df$SplitVariable)


percentages_weighted <- function(df.day.sp,category,cat,weighted) {
  if (!weighted) return(nrow(subset(df.day.sp,df.day.sp[,category]==cat))/nrow(df.day.sp))
  else if (weighted) sum(subset(df.day.sp,df.day.sp[,category]==cat)$Total.animals,na.rm=T)/sum(df.day.sp$Total.animals,na.rm=T)
}

mean_sd_weighted <- function(df.day.sp,variable,weighted) {
  if (!weighted) {
    return(c(
      nan.return(mean(df.day.sp[,variable],na.rm=T),NA),
      na.return(sd(df.day.sp[,variable],na.rm=T),NA)
    ))
  } else if (weighted) {
    return(c(
      nan.return(mean(df.day.sp[,variable]*df.day.sp$Total.animals,na.rm=T),NA),
      na.return(sd(df.day.sp[,variable]*df.day.sp$Total.animals,na.rm=T),NA)
    ))
  }
}
mean_sd_names <- function(variable_name) return(paste0(c("Mean.","Sd."),variable_name))
min_med_max_weighted <- function(df.day.sp,variable,weighted) {
  if (!weighted) {
    return(c(
      inf.return(min(df.day.sp[,variable],na.rm=T),NA),
      na.return(median(df.day.sp[,variable],na.rm=T),NA),
      inf.return(max(df.day.sp[,variable],na.rm=T),NA)
    ))
  } else if (weighted) {
    return(c(
      inf.return(max(df.day.sp[,variable]*df.day.sp$Total.animals,na.rm=T),NA),
      na.return(median(df.day.sp[,variable]*df.day.sp$Total.animals,na.rm=T),NA),
      inf.return(min(df.day.sp[,variable]*df.day.sp$Total.animals,na.rm=T),NA)
    ))
  }
}
min_med_max_names <- function(variable_name) return(paste0(c("Min.","Med.","Max."),variable_name))
quartile_weighted <- function(df.day.sp,variable,weighted) return(quantile(df.day.sp[,variable], 0.75, na.rm=T)[[1]])
quartile_names <- function(variable_name) return(paste0(c("Upper."),variable_name))
all_weighted <- function(df.day.sp,variable,weighted)
  return(c(
    mean_sd_weighted(df.day.sp,variable,weighted),
    min_med_max_weighted(df.day.sp,variable,weighted),
    quartile_weighted(df.day.sp,variable,weighted))
  )
all_names <- function(variable_name)
  return(c(
    mean_sd_names(variable_name),
    min_med_max_names(variable_name),
    quartile_names(variable_name))
  )

fill_in_rows <- function(ss,df.day,species,weight_percent=F,weight_variable=F,split.by=c()) {
  df.day.sp <- filter(df.day, Species==species)
  if (species%in%zebra.abbr) df.day.of_species <- filter(df.day, Species%in%zebra.abbr)
  else if (species%in%cattle.abbr) df.day.of_species <- filter(df.day, Species%in%cattle.abbr)
  cattle.numbers <- sum(filter(df.day, Species=="Cattle")$Total.animals,na.rm=T)
  if (nrow(filter(df.day.sp, !is.na(Total.animals))) > 0) {
    ss[nrow(ss)+1,] <- rep(NA,times=ncol(ss))
    ss[nrow(ss),"Date"] <- df.day.sp$Date[1]
    for (split.by.var in c(split.by,"Species")) ss[nrow(ss),split.by.var] <- unique(df.day.sp[,split.by.var])
    
    ss[nrow(ss),"Total.sitings"] <- sum(df.day.sp$Total.animals, na.rm=T)
    ss[nrow(ss),"Total.herds"] <- nrow(df.day.of_species)
    ss[nrow(ss),all_names("herd.size")] <- all_weighted(df.day.sp,"Total.animals",F)
    ss[nrow(ss),all_names("distance.from.herd")] <- all_weighted(df.day.sp,"Distance.from.herd",weight_variable)
    
    ss[nrow(ss),all_names("distance.from.opp.herd")] <- all_weighted(df.day.sp,"Distance.from.herd.opp.sp",weight_variable)
    ss[nrow(ss),all_names("distance.from.mob")] <- all_weighted(df.day.sp,"Distance.from.mob",weight_variable)
    ss[nrow(ss),all_names("cattle.density")] <- all_weighted(df.day.sp,"Cattle.density",weight_variable)
  
    ss[nrow(ss),all_names("distance.to.water")] <- all_weighted(df.day.sp,"Distance.to.water",weight_variable)
    ss[nrow(ss),all_names("midday.distance.to.water")] <- all_weighted(df.day.sp %>% filter(abs(Time-1200)<=100),"Distance.to.water",weight_variable)
    
    ss[nrow(ss),all_names("NDVI")] <- all_weighted(df.day.sp,"NDVI",weight_variable)
    ss[nrow(ss),all_names("EVI")] <- all_weighted(df.day.sp,"EVI",weight_variable)
    
    ss[nrow(ss),all_names("X.cover")] <- all_weighted(df.day.sp,"X.Cover",weight_variable)
    ss[nrow(ss),all_names("X.green")] <- all_weighted(df.day.sp,"X.Green",weight_variable)
    ss[nrow(ss),mean_sd_names("grass.height")] <- mean_sd_weighted(df.day.sp,"Average.Height",weight_variable)
    ss[nrow(ss),mean_sd_names("density")] <- mean_sd_weighted(df.day.sp,"Approximate.density",weight_variable)
    
    ss[nrow(ss),"Total.cattle"] <- cattle.numbers
    
    # densities <- c()
    # for (row in 1:nrow(df.day.sp)) {
    #   densities[row] <- df.day.of_species %>% mutate("Distance" = sqrt((GPS.x - df.day.sp$GPS.x[row])^2 + (GPS.y - df.day.sp$GPS.y[row])^2)) %>%
    #     filter(Distance <= 1000) %>% dplyr::select("Total.animals") %>% sum(na.rm=T)
    # }
    # densities <- densities/pi
    # ss[nrow(ss),"Mean.density"] <- mean(densities)
    # ss[nrow(ss),"Sd.density"] <- na.return(sd(densities),0)
    
    
    
    for (cat in unique(df$Activity)) {
      if (is.na(cat)) next
      if (cat == "") next
      ss[nrow(ss),paste0("X.",cat)] <- percentages_weighted(df.day.sp,"Activity",cat,weight_percent)
    }
    for (cat in unique(df$Bush.type)) {
      if (is.na(cat)) next
      if (cat == "") next
      ss[nrow(ss),paste0("X.",cat)] <- percentages_weighted(df.day.sp,"Bush.type",cat,weight_percent)
    }
    for (cat in unique(df$Primary.habitat)) {
      if (is.na(cat)) next
      if (cat == "") next
      ss[nrow(ss),paste0("X.",cat)] <- percentages_weighted(df.day.sp,"Primary.habitat",cat,weight_percent)
    }
    for (cat in unique(df$Grass.height)) {
      if (is.na(cat)) next
      if (cat == "") next
      ss[nrow(ss),paste0("X.",cat)] <- percentages_weighted(df.day.sp,"Grass.height",cat,weight_percent)
    }
    ss[nrow(ss),"X.Combined"] <- percentages_weighted(df.day.sp,"Multispecies",1,weight_percent)
  }
  return(ss)
}

for (day in names(df.sp)) {
  df.day <- df.sp[[day]]
  for (species in unique(df$Species)) {
    # for (size in unique(df$Size)) {
    #   df.day <- filter(df.day, Size==size)
      ss <- fill_in_rows(ss, df.day, species,)# split.by=c("Size")) 
    # }
  }
}

for (species in unique(df$Species))
  ss[ss$Species==species,"Identifier"] <- paste0(find_replace(species,data.frame(x=c("PZ","GZ","Cattle","MC","CKC","CC"),y=c("PZ","GZ","CT","MC","CKC","CC"))),1:nrow(ss[ss$Species==species,]))

ss$Date <- mdy(ss$Date)

ss$Exp.1 <- "Not in experiment"
ss[is.before(ss$Date,"2021-06-14"),"Exp.1"] <- "Before"
ss[is.strictly.after(ss$Date,"2021-06-14") & is.strictly.before(ss$Date,"2021-06-19"), "Exp.1"] <- "During"
ss[is.after(ss$Date,"2021-06-19") & is.before(ss$Date,"2021-07-05"),"Exp.1"] <- "After"


ss$Exp.2 <- "Not in experiment"
ss[is.before(ymd(ss$Date),ymd("2021-06-16")),"Exp.2"] <- "Before"
ss[is.after(ymd(ss$Date),ymd("2021-06-17")),"Exp.2"] <- "After"




ss$Date <- time_length(interval("2021-06-03",ss$Date),"day")+1
colnames(ss)[2] <- "Days"

ss$Species <- factor(ss$Species,levels=c(zebra.abbr,cattle.abbr))


## Add Random.NDVI
library(reshape2)
vi <- get_vi()

vi$ID <- paste0(vi$Latitude,":",vi$Longitude)
vi.sample <- melt(vi %>% dplyr::select(3,5,7,9,11) %>% sample_n(1000))
colnames(vi.sample) <- c("ID","Date","NDVI")
vi.sample$Date <- ymd(paste0("2021-",gsub("_","-",substr(vi.sample$Date,6,10))))
vi.sample$Days <- days_since_start(vi.sample$Date)
vi.sample$Species <- "Random"
vi.lm <- lm(NDVI ~ Days, vi.sample)
ss$Random.NDVI <- vi.lm$coefficients['(Intercept)']+vi.lm$coefficients['Days']*ss$Days


cat("Writing...\n")
write.csv(ss,"/Users/maxgotts/Desktop/MPALA/Whitesheets/StatisticsWhitesheets.csv",row.names=FALSE)

if (FALSE) {
  source('~/Desktop/MPALA/Whitesheets/SummaryStatistics.R')
}

