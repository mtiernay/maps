# This .R file generates maps and .gifs for Africa, the Great Lakes Region, and West Africa
# See https://files.nyu.edu/mrt265/public/maps.html

library(maptools)  
library(animation)
library(ggmap)


setwd("/Users/tiernay/Desktop/spatial")

ucdp <- readShapeSpatial("/Users/tiernay/Desktop/spatial/ucdp-ged-points-v-1-1-csv/UCDP_points_R.shp", proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
ucdp2 <- as.data.frame(ucdp)
ucdp3 <- fortify(ucdp2, region="ISONumber")
ucdp3$long <- ucdp3$coords.x1
ucdp3$lat <- ucdp3$coords.x2


#### Play with dates
# This gets up 'january', etc.
months <- month.name
#Grab the month
ucdp3$month <- substr(ucdp3$Date_start,7,8)
ucdp3$month <- months[as.numeric(ucdp3$month)]
# subset of months (for use in testing only)
months2 <- months[1:2]
months1 <- months[1:1]
months6 <- months[1:6]
months_test <- months[10:11]
# Vector of active years
years <- 1989:2010
# Day of the battle
ucdp3$day <- as.numeric(substr(ucdp3$Date_start,10,11))
days <- 1:31






##########################################################
# Create maps for all of Africa


# This first set of maps are static maps for testing
africa_latlon <- c(lon = 19, lat = 2) 

# Get coordinates of the map for later use
africa_temp <- get_map(location=africa_latlon, zoom = 3)
bb <- attr(africa_temp, 'bb')
mbbox <- c(left = bb$ll.lon, bottom = bb$ll.lat, right = bb$ur.lon, top = bb$ur.lat)
lat_range <- c(mbbox[2],mbbox[4])
lon_range <- c(mbbox[1],mbbox[3])

# Create the Africa Baselayer
Africa_ggmap <- ggmap(get_map(location=africa_latlon, zoom = 3, scale = 2, maptype="hybrid"))

# Initial Plot of Africa
Africa_ggmap+
  geom_jitter(data = ucdp3,  aes(x = long, y = lat, group = Dyad_ID), colour = "red", size = log(ucdp3$Best_est+1), alpha =.1)





  
# Restrict conflict data to be in map coordinates
ucdp4 <- subset(ucdp3, 
            mbbox[1] <= long & long <= mbbox[3] &
            mbbox[2] <= lat  & lat  <= mbbox[4])  




#####################
# Create a .gif for all of Africa
saveMovie({
  for (i in 1989:2010) {
    for(j in months){

      ucdp_sub <- subset(ucdp3, Year == i & month == j)  
      
      print(Africa_ggmap+
              geom_jitter(data = ucdp_sub,  aes(x = long, y = lat, group = Dyad_ID), colour = "red", size = log(ucdp_sub$Best_est+1), alpha =.25)+
                    ggtitle(paste("Conflict in Africa",j,i, sep = " "))+
                    theme(legend.position = "none")
      )
    }
  }
}, movie.name = "africa_points_by_month_hybrid.gif", interval = .25, filename = "ucdp" , outdir = getwd(), loop = 0)
















###############################################
# Regional Maps
# Great Lakes

africa_latlon <- c(lon = 29.3, lat = -2.75) 
africa_map <- get_map(africa_latlon, zoom=8, scale = 2)
bb <- attr(africa_map, 'bb')
mbbox <- c(left = bb$ll.lon, bottom = bb$ll.lat, right = bb$ur.lon, top = bb$ur.lat)

# Restrict conflict data to be in map coordinates
ucdp4 <- subset(ucdp3, 
                mbbox[1] <= long & long <= mbbox[3] &
                mbbox[2] <= lat  & lat  <= mbbox[4])

# Create base map
Great.Lakes <- ggmap(africa_map)
theme_set(theme_bw(16))




# Generate Variables to determine the size and density of the conflict dots 
ucdp4$deaths <- ifelse(ucdp4$Best_est > 0, sqrt(ucdp4$Best_est), 0)
ucdp4$deaths.percentage <- pmin(pmax(ucdp4$deaths/max(ucdp4$deaths),.25),.75)

saveMovie({
  for (i in 1990:2010) {
    for(j in months){
      ucdp_sub <- subset(ucdp4, Year == i & month == j)  
        if(nrow(ucdp_sub) <= 0) { # If any of the months have missing values, saveMovie removes the layer for EVERY month
          print(Great.Lakes+
                  ggtitle(paste("Great Lakes Conflict",j,i, sep = " "))+
                  theme(legend.position = "none")
          )
        }
      if(nrow(ucdp_sub) > 0) {
      print(Great.Lakes+
              geom_jitter(data = ucdp_sub,  aes(x = long, y = lat, group = Dyad_ID), colour = "red", size = ucdp_sub$deaths, alpha =ucdp_sub$deaths.percentage)+
              ggtitle(paste("Great Lakes Conflict",j,i, sep = " "))+
              theme(legend.position = "none")
      )
      }
      
    }
  }
}, movie.name = "great_lakes_by_month.gif", interval = .25, filename = "ucdp" , outdir = getwd(), loop = 0)











###############################################
# West Africa

africa_latlon <- c(lon = -10.2, lat = 7.7) 
africa_map <- get_map(africa_latlon, zoom=7, scale=2)
bb <- attr(africa_map, 'bb')
mbbox <- c(left = bb$ll.lon, bottom = bb$ll.lat, right = bb$ur.lon, top = bb$ur.lat)

# Restrict conflict data to be in map coordinates
ucdp4 <- subset(ucdp3, 
                mbbox[1] <= long & long <= mbbox[3] &
                  mbbox[2] <= lat  & lat  <= mbbox[4])

# Create base map
West.Africa <- ggmap(africa_map)
theme_set(theme_bw(16))


# Generate Variables to determine the size and density of the conflict dots 
ucdp4$deaths <- ifelse(ucdp4$Best_est > 0, sqrt(ucdp4$Best_est), 0)
ucdp4$deaths.percentage <- pmin(pmax(ucdp4$deaths/max(ucdp4$deaths),.25),.75)



saveMovie({
  for (i in 1990:2005) {
    for(j in months){
        
        ucdp_sub <- subset(ucdp4, Year == i & month == j)  
        if(nrow(ucdp_sub) <= 0) { # If any of the months have missing values, saveMovie removes the layer for EVERY month
          print(West.Africa+
                  ggtitle(paste("Conflict in West Africa",j,i, sep = " "))+
                  theme(legend.position = "none")
          )
        }
        if(nrow(ucdp_sub) > 0) {
          print(West.Africa+
                  geom_jitter(data = ucdp_sub,  aes(x = long, y = lat, group = Dyad_ID), colour = "red", size = ucdp_sub$deaths, alpha =ucdp_sub$deaths.percentage)+
                  ggtitle(paste("Conflict in West Africa",j,i, sep = " "))+
                  theme(legend.position = "none")
          )
        
      }
    }
  }
}, movie.name = "west_africa_by_month.gif", interval = .25, filename = "ucdp" , outdir = getwd(), loop = 0)


