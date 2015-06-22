# MARK SYSTEM TIME
t_start<-Sys.time()

library(jsonlite) # Import .json file
library(stringr) # Trim whitespace 
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
data$sex <- recode(data$sex,
'c("Male", "Homme", "ذكر", "男", "Männlich", "पुरुषों", "男性", "Masculino", "Мужской", "Erkek") = "M";
c("Female", "Femme", "أنثى", "女", "Weiblich", "मिहला", "女性", "Feminino", "Женский", "Femenino", "Kadın") = "F"')
levels(data$sex)

# ETHNICITY
# Trim Whitespace
data$ethnicity <- str_trim(data$ethnicity, side = "both")
# Make as Factor
data$ethnicity <- as.factor(data$ethnicity)
levels(data$ethnicity)
# Recode
data$ethnicity <- recode(data$ethnicity,
'c("Mixed Race", "Karışık (Melez)", "Raza mixta", "Смешанная раса", "Raça mista", "Gemischte Rasse", "Métis", "混血", "عرقية مختلطة", "मिश्र जाति") = 1; 

c("African/African American", "Afroamericano/Africano", "Africana/afro-americana", "Africain/Afro-américain", "Afro/Afro-Amerikanisch", "Африканец/Афроамериканец", "Afrikalı", "非裔美籍 ", "アフリカ人／アフリカ系アメリカ人 ", "أفريقى/ أمريكى أفريقى", "अफ्रीकी / अफ्रीकी अमेरिकी") = 2; 

c("Asian Indian", "Indio asiático", "Asiatische Inder", "Asiático Índico", "Indien d’Asie", "印度裔 ", "भारतीय/एशियाई", "هندى أسيوى", "Индиец", "Hint Asyalı", "アジア系インド人 ") = 3; 

c("Caucasian (European)", "قوقازى (أوروبى)", "白种人（欧洲）", "Caucasien (Européen)", "Kaukasic/Europäer", "श्वेतजाति  (यूरोपीय)", "白人（ヨーロッパ系）", "Caucasiano (Europeu)", "Европеоид (Европа)", "Caucásico (europeo)", "Avrupalı") = 4; 

c("Caucasian (Middle East)", "قوقازى (شرق أوسطى)", "白种人（中东）", "Caucasien (Moyen-Orient)", "श्वेतजाति  (मध्यपूर्व)", "白人（中東系)", "Caucasiano (Oriente Médio)", "Европеоид (Ближний Восток)", "Caucásico (medio oriente)", "Orta Doğulu") = 5; 

c("Hispanic, Latino or Spanish", "هسبانى أو لاتينى أو اسبانى", "西班牙，拉美裔 ", "Hispanique, Latino", "Hispanic, Latino oder Spanish", "हिस्पैनिकलातीनोयास्पेनिश", "ヒスパニック、ラテン系またはスペイン人", "Hispânico, latino ou espanhol", "Латиноамериканец или испанец", "Hispano, latinoamericano o español", "Hispanik, Latin veya İspanyol") = 6; 

c("Indigenous Australian", "أسترالى أصلى", "澳大利亚土著人 ", "Indigène australien", "Einheimischer Australier", "मूलऑस्ट्रेलियाई", "原住民オーストラリア人", "Indígena Australiano", "Австралийский абориген", "Indígena australiano", "Avusturalya Yerlisi") = 7;

c("Native American", "أمريكى أصلى", "美国人 ", "Indien d’Amérique", "Einheimischer Amerikaner (Indianer)", "मूलअमेरिकी", "アメリカ先住民 ", "Americano Nativo", "Коренной американец", "Nativo americano", "Amerika Yerlisi") = 8;

c("North East Asian (Mongol, Tibetan, Korean, Japanese, etc)", "شمال شرق آسيا (منغولى، تبتى، كورى، يابانى، إلخ)", "东北亚裔（蒙古，西藏，韩国，日本等）", "Asiatique du nord-est (Mongol, Tibétaine, Coréen, Japonais, etc...)", "Nordöstliche Asiaten (mongolisch, tibetisch, koreanisch, japanisch etc.)", "उत्तर-पूर्वएशियाई (मंगोल, तिब्बती, कोरियाई, जापानी, आदि)", "北東アジア（モンゴル人、チベット族、韓国人、日本人、など）", "Norte Leste Asiático (mongol, tibetano, coreano, japonês, etc.)", "Северо-восточный азиат (монгол, тибетец, кореец, японец и т.п.)", "Del noreste asiático (mongol, tibetano, coreano, japonés, etc.)", "Kuzey Doğu Asyalı (Moğol, Tibet, Koreli, Japon, vs)") = 9;

c("Pacific (Polynesian, micronesian, etc)", "المحيط الهادى (بولونيزى، ميكرونيزى، إلخ)", "太平洋(波利尼西亚, 密克罗尼西亚等) ", "Pacifique (Polynésien, Micronésien, etc...)", "Pazifische (Polynesisch etc.)", "प्रशांत (पॉलिनीषियन, माइक्रोनेशियन, आदि)", "大西洋（ポリネシア人、ミクロネシア人、など）", "Pacífico (etc. da Micronésia, Polinésia,)", "Тихоокеанец (полинезиец, микронезиец и т.п.)", "Del Pacífico (polinesio, micronesio, etc.)", "Pasifik (Polinezyalı, Mikronezyalı, vs)") = 10;

c("South East Asian (Chinese, Thai, Malay, Filipino, etc)", "جنوب شرق آسيا (صينى، تايلاندى، ماليزى، فليبينى، إلخ)", "东南亚裔（中国，泰国，马来，菲律宾等）", "Asiatique du sud-est -Chinois, Thaï, Malaisienne, Philippine, etc...)", "Südöstliche Asiaten (chinesisch, thai,  malaysisch, Filipino etc.)", "दक्षिणपूर्वएशियाई (चीनी, थाई, मलय, फिलिपिनो,आदि)", "南東アジア（中国人、タイ人、マレーシア人、フィリピン人、など）", "Do Sudeste Asiático (chinês, tailandês, malaio, filipino, etc)", "Юго-восточный азиат (китаец, таец, малаец, филиппинец и т.п.)", "Del sudeste asiático (chino, tailandés, malayo, filipino, etc.)", "Güney Doğu Asyalı (Çinli, Tay, Malezyalı, Filipinli, vs)") = 11')

levels(data$ethnicity) 



              


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


# MARK SYSTEM TIME AGAIN
# Subtract from t_start and print difference
t_end<-Sys.time()
t_dur=t_end-t_start
print(t_dur)
