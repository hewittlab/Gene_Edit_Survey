library(jsonlite) # Import .json file
library(stringr) # Trim whitespace 
library(XML) # Bubble plot map
library(httr) # Bubble plot map
library(maps) # Bubble plot map
library(ggplot2) # Plots
library(plyr) # Various
library(car) # Recode variables
library(doBy) # Group summary stats

# PREPARE JSON FILE FOR IMPORT INTO R

#Edit .json file in text editor before importing - Remove 'Results' and outer curly brackets. Replicate:
# [
# { "name":"Amy" , "grade1": 35 , "grade2": 41 , "grade3": 53},
# { "name":"Bob" , "grade1": 44 , "grade2": 37 , "grade3": 28},
# ]

# CHANGE WORKING DIRECTORY HERE

setwd('~/Dropbox/Research Projects/2015/Gene Edit Survey')
data <- fromJSON('Answers.json')

#----------------------------------------------------------------------------------------

# NEED TO MERGE WE CHAT DATA HERE

#----------------------------------------------------------------------------------------

# RENAME AND REORDER VARIABLES

data<-rename(data, c("question_1"="sex", "question_2"="YOB", "question_3"="country", "question_4"="ethnicity", "question_5"="religion", "question_6"="religion_type", "question_7"="edu_level", "question_8"="worked_health", "question_9"="worked_health_type", "question_10"="heard_about", "question_11"="wealth", "question_12"="genetic_cond", "question_13"="genetic_cond_affected", "question_14"="genetic_cond_type", "question_15"="kids_cure_life", "question_16"="kids_cure_debil", "question_17"="embr_prev_life", "question_18"="embr_prev_debil", "question_19"="edit_for_nondis", "question_20"="deter_phys_appear", "question_21"="deter_intell", "question_22"="deter_strength", "question_23"="other_traits_alter","question_24"="gen_mod_food"))

# Generate age variable
data$YOB <- as.numeric(data$YOB)
data$age <- 2015-data$YOB

# Reorder
data <- data[c("client", "objectId", "createdAt", "updatedAt", "ip", "language", "country", "sex", "YOB", "age", "ethnicity", "wealth", "edu_level", "worked_health", "worked_health_type", "heard_about", "genetic_cond", "genetic_cond_affected", "genetic_cond_type", "kids_cure_life", "kids_cure_debil", "embr_prev_life", "embr_prev_debil", "edit_for_nondis", "deter_phys_appear", "deter_intell", "deter_strength", "other_traits_alter", "gen_mod_food", "religion", "religion_type", "question_25", "question_100", "question_101", "question_102" )]

#----------------------------------------------------------------------------------------

# RECODE VARIABLES AS NOMINAL/ORDINAL

# SEX
# Trim Whitespace
data$sex <- str_trim(data$sex, side = "both")
# Make as Factor
data$sex <- as.factor(data$sex)
levels(data$sex)
# Recode
data$sex <- recode(data$sex,'c("Male", "Homme", "ذكر", "男", "Männlich", "पुरुषों", "男性", "Masculino", "Мужской", "Erkek") = "M"; c("Female", "Femme", "أنثى", "女", "Weiblich", "मिहला", "女性", "Feminino", "Женский", "Femenino", "Kadın") = "F"')
levels(data$sex)

# ETHNICITY
# Trim Whitespace
data$ethnicity <- str_trim(data$ethnicity, side = "both")
# Make as Factor
data$ethnicity <- as.factor(data$ethnicity)
levels(data$ethnicity)
# Recode
data$ethnicity <- recode(data$ethnicity,'c("Mixed Race", "Karışık (Melez)", "Raza mixta", "Смешанная раса", "Raça mista", "Gemischte Rasse", "Métis", "混血", "عرقية مختلطة", "मिश्र जाति") = 1; c("African/African American", "Afroamericano/Africano", "Africana/afro-americana", "Africain/Afro-américain", "Afro/Afro-Amerikanisch", "Африканец/Афроамериканец", "Afrikalı", "非裔美籍", "アフリカ人／アフリカ系アメリカ人", "أفريقى/ أمريكى أفريقى", "अफ्रीकी / अफ्रीकी अमेरिकी") = 2; c("Asian Indian", "Indio asiático", "Asiatische Inder", "Asiático Índico", "Indien d’Asie", "印度裔", "भारतीय/एशियाई", "هندى أسيوى", "Индиец", "Hint Asyalı", "アジア系インド人") = 3')
                 


#----------------------------------------------------------------------------------------

# FUNCTION TO GEOLOCATE FROM IP ADDRESS

freegeoip <- function(ip, format = ifelse(length(ip)==1,'list','dataframe'))
{
  if (1 == length(ip))
  {
    # a single IP address
    require(rjson)
    url <- paste(c("http://freegeoip.net/json/", ip), collapse='')
    ret <- fromJSON(readLines(url, warn=FALSE))
    if (format == 'dataframe')
      ret <- data.frame(t(unlist(ret)))
    return(ret)
  } else {
    ret <- data.frame()
    for (i in 1:length(ip))
    {
      r <- freegeoip(ip[i], format="dataframe")
      ret <- rbind(ret, r)
    }
    return(ret)
  }
}  

#----------------------------------------------------------------------------------------

# CREATE VECTOR OF IP ADDRESSES, TRIM PRECEDING WHITESPACE AND REMOVE NA'S

ipvec <- data$ip
ipvec<-str_trim(ipvec, side = "left")
sum(is.na(ipvec))
ipvec <- ipvec[!is.na(ipvec)]

#----------------------------------------------------------------------------------------

# GENERATE GEOLOCATION DATA

geodata <- freegeoip(ipvec)
geodata

#----------------------------------------------------------------------------------------

# BUBBLE PLOT OF IP DATA

geodata$longitude <- as.numeric(levels(geodata$longitude))[geodata$longitude]
geodata$latitude <- as.numeric(levels(geodata$latitude))[geodata$latitude]

# Recode - Convert blanks to NAs
geodata$time_zone[geodata$time_zone == ""] <- NA
geodata$country_name[geodata$country_name == ""] <- NA

# Remove rows (based on country_name = NA)
geodata_cut <-geodata[!is.na(geodata[,3]),] 

# Replace missing time_zone data with country_name
my.na <- is.na(geodata_cut$time_zone)
geodata_cut$country_name <- as.character(geodata_cut$country_name)
geodata_cut$time_zone <- as.character(geodata_cut$time_zone)
geodata_cut$time_zone[my.na] <- geodata_cut$country_name[my.na]

# Summarise - Create data frame of region, its frequency, and mean long and lat
mapdata <- summaryBy(longitude + latitude ~ time_zone, data = geodata_cut, FUN = function(x) { c(m = mean(x), N = length(x)) } )

# Create Bubble Plot
map <- map_data('world')
str(map)
ggplot() +
geom_polygon(data = map, aes(long, lat, group=group), fill="grey50") +
geom_point(data = mapdata, aes(x=longitude.m, y=latitude.m, map_id = time_zone, size = latitude.N), col="red")
ggsave(file="Bubble_Plot.eps", width=12, height=6)

#----------------------------------------------------------------------------------------

# POPULATION PYRAMID

ggplot(data=data,aes(x=age,fill=sex)) + 
  geom_bar(subset=.(sex=="F")) + 
  geom_bar(subset=.(sex=="M"),aes(y=..count..*(-1))) + 
  scale_fill_grey(start = 0.1, end = 0.4, na.value = "grey50") +
  scale_x_continuous(breaks=seq(0,100,10),labels=abs(seq(0,100,10))) +
  scale_y_continuous(breaks=seq(-120,120,20),labels=abs(seq(-120,120,20))) + 
  coord_flip()
ggsave(file="Pyramid_Plot.eps", width=12, height=10)



