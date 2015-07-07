# Need to have run the 1.Data_prep.R script and have the resulting data in this workspace

# MARK SYSTEM TIME
t_start<-Sys.time()

library(maps) # Bubble plot map
library(ggplot2) # Plots

#GEOFUNC----------------------------------------------------------------------------------------

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

#IPVEC----------------------------------------------------------------------------------------

# CREATE VECTOR OF IP ADDRESSES, TRIM PRECEDING WHITESPACE AND REMOVE NA'S

ipvec <- all$ip
ipvec<-str_trim(ipvec, side = "left")
sum(is.na(ipvec))
ipvec <- ipvec[!is.na(ipvec)]

#GEODATA----------------------------------------------------------------------------------------

# GENERATE GEOLOCATION DATA

geodata <- freegeoip(ipvec)

#BUBBLE----------------------------------------------------------------------------------------

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
ggsave(file="Figures symlink/Bubble_Plot.eps", width=12, height=6)

#----------------------------------------------------------------------------------------



# MARK SYSTEM TIME AGAIN
# Subtract from t_start and print difference
t_end<-Sys.time()
t_dur=t_end-t_start
print(t_dur)

