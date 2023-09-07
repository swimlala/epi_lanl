## download bbs data
library(tidyverse)

path <- "2020Release_Nor"


unzipBBS <- function(path) {
  
  print("Unzipping folder")
  folderpath <- paste0(path, ".zip")
  unzip(folderpath,
        exdir = path)
  
  
  print("Unzipping 50-StopData.zip")
  unzip(paste0(path, "/50-StopData.zip"), overwrite = T,
        exdir = path)
  
  base <- paste0(path, "/50-StopData/1997ToPresent_SurveyWide")
  
  filesZip <- list.files(base, pattern = ".zip")
  
  print("Unzipping files")
  pb <- txtProgressBar(min = 0, max = 10, initial = 0, style = 3)
  
  for (i in 1:length(filesZip)) {
    #print(paste0("Unzipping file ", i))
    unzip(paste0(base, "/", filesZip[i]), 
          exdir = base,
          overwrite = T)
    setTxtProgressBar(pb,i)
    
  }
  
  print("Unzipping migrants")
  
  unzip(paste0(path, "/MigrantNonBreeder.zip"), overwrite = T,
        exdir = path)
  unzip(paste0(path, "/MigrantNonBreeder/Migrants.zip"), overwrite = T,
        exdir = path)
  
  print("Unzipping other data")
  unzip(paste0(path, "/Routes.zip"), overwrite = T,
        exdir = path)
  unzip(paste0(path, "/Weather.zip"), overwrite = T,
        exdir = path)
  
}

readBBS <- function(path, migrant = F) {
  base <- paste0(path, "/50-StopData/1997ToPresent_SurveyWide")
  
  files <- list.files(base, pattern = ".csv")
  
  print("Reading in breeding birds...")
  pb <- txtProgressBar(min = 0, max = 10, initial = 0, style = 3)
  
  data <- read.csv(paste0(base, "/", files[1]))
  colnames(data) <- tolower(colnames(data))
  setTxtProgressBar(pb,1)
  
  for (j in 2:length(files)) {
    tmp <- read.csv(paste0(base, "/", files[j]))
    colnames(tmp) <- tolower(colnames(tmp))
    setTxtProgressBar(pb,j)
    
    data <- rbind(data, tmp)
  }
  
  if (migrant == T) {
    print("Reading in migrants...")
    # unzip(paste0(path, "MigrantNonBreeder.zip"), overwrite = T)
    # unzip(paste0(path, "MigrantNonBreeder/Migrants.zip"), overwrite = T)
    # 
    mig <- read.csv(paste0(path, "/Migrants.csv"))
    colnames(mig) <- tolower(colnames(mig))
    
    data <- rbind(data, mig)
  }
  
  return(data)
}

summarizeBBS <- function(raw) {
  raw$count <- rowSums(raw[,8:57])
  raw$id <- paste0(raw$countrynum, "_", raw$statenum, "_", raw$route)
  raw1 <- raw[,c("id", "year", "aou", "count")]
  
  return(raw1)
}

aouToSp <- function(raw, path) {
  spList <- read.fwf(file = paste0(path, "/SpeciesList.txt"),
                     widths = c(7, 5, 50, 50, 50, 
                                50, 50, 50, 50),
                     skip = 14,
                     col.names = c("Seq", "aou", "English",
                                   "French", "Spanish",
                                   "Order", "Family", "Genus", "Species"),
                     strip.white = T)
  
  spList$gs <- paste(spList$Genus, spList$Species, sep = " ")
  spList <- spList[,c("aou", "gs")]
  
  bbsSp <- merge(raw, spList, by = "aou")
  bbsSp <- bbsSp[,c("id", "year", "gs", "count")]
  colnames(bbsSp) <- c("id", "year", "species", "count")
  
  return(bbsSp)
}


unzipBBS(path)
bbs <- readBBS(path = path)

bbs_raw <- readBBS(path = path, 
               migrant = T)

# subet to only ontario
bbs_ont <- bbs_raw %>% subset(countrynum == 124 & statenum == 68)
bbs_ont <- summarizeBBS(bbs_ont)
bbs_all <- summarizeBBS(bbs)
bbs <- summarizeBBS(bbs)

bbs <- aouToSp(bbs, path = path)

locs <- read.csv(paste0(path, "/routes.csv")) %>%
  mutate(id = paste0(CountryNum, "_", StateNum, "_", Route)) %>%
  select(id, Latitude, Longitude)

bbs_ont2 <- bbs_ont %>%
  inner_join(locs, by = "id") %>%
  inner_join(weather, by = c("id", "year")) #%>%
  filter(RunType == 1,
         year > 1999)

weather <- read.csv(paste0(path, "/weather.csv")) %>%
  mutate(id = paste0(CountryNum, "_", StateNum, "_", Route),
         year = Year) %>%
  select(id, year, ObsN, RunType)


bbs <- bbs %>%
  inner_join(locs, by = "id") %>%
  inner_join(weather, by = c("id", "year")) %>%
  filter(RunType == 1,
         year > 2009)


# american robin 07610


save(bbs, file = "bbs.RData")
save(bbs_ont2, file = "bbs_ont.RData")


bbs_ont_map <- map_lower %>% 
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = 'black') + 
  geom_point(data = bbs_subset, aes(x = Longitude, y = Latitude, size = count), color = "blue")

ggsave("bbs_ont_map_test2.pdf")


## look at spatial correlation
library(gstat)

bbs_ont2_sub <- bbs_ont2 %>% as.data.frame() %>%  subset( year == 2010 & Longitude < -74 & Longitude > -83 & Latitude > 41 & Latitude < 47) %>% 
  group_by(Longitude, Latitude, year) %>% summarise(count = sum(count, na.rm = T))
coordinates(bbs_ont2_sub) <- ~Longitude+Latitude
bbs.vc <- variogram(count ~ 1, data = bbs_ont2_sub, cloud = TRUE)
plot(bbs.vc, ylab=bquote(gamma), xlab=c("h (separation distance in m)"))

bbs.v <- gstat::variogram(count ~ 1, data = bbs_ont2_sub)
v0 <- variogram(count~1, bbs_ont2_sub, cutoff = 1, width = 1)
plot(v0, ylab=bquote(gamma), xlab=c("h (separation distance in m)"))

v.m <- fit.variogram(bbs.v, vgm(1, "Exp", 0.5, 1))
plot(bbs.v, ylab=bquote(gamma), xlab=c("h (separation distance in m)"))

library("automap")

out <- autofitVariogram(count ~ 1, input_data = bbs_ont2_sub)

coordinates(bbs_ont2_sub) <- ~Longitude + Latitude 
pointsSP2 <- SpatialPointsDataFrame(bbs_ont2_sub, data = data.frame(bbs_ont2_sub), proj4string = CRS(proj4string(map)))
krig_auto <- autoKrige(count ~1, input_data = pointsSP, new_data = map_lower)
krig_auto1 <- autoKrige(count ~1, input_data = pointsSP, new_data = map)
gridsp <- SpatialPoints(grid)
krig_auto <- autoKrige(count ~1, input_data = bbs_ont2_sub, new_data = grid)

krig_auto <- autoKrige(count ~1, input_data = bbs_ont2_sub)
krig_df <- data.frame(krig_auto$krige_output)
coordinates(map_lower) <- ~long + lat

# try and plot a krige map
bbs_ont_krig <- map_lower %>% 
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = 'black') + 
  geom_point(data = krig_df, aes(x = long, y = lat, color = var1.pred))
ggsave(bbs_ont_krig, file = "bbs_krig_attempt1.pdf")


map_sf <- st_as_sf(map)
grid <- map_sf %>% 
  st_make_grid(cellsize = .1, what = "centers") %>% # grid of points
  st_intersection(map_sf) 

 ggplot() + 
 geom_sf(data = map_sf) + 
 geom_sf(data = grid)
 
 map_lower %>% ggplot() + 
   geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = 'black') + 
   geom_sf(data = grid)
 
 shp <- spTransform(map, CRSobj = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
 cs <- c(1, 1)*6000
 grdpts <- makegrid(shp, cellsize = cs)
 
 spgrd <- SpatialPoints(grdpts, proj4string = CRS(proj4string(shp)))
 
 spgrdWithin <- SpatialPixels(spgrd[shp,])
 plot(spgrdWithin, add = T)
 plot(shp)
 
 test_df<- as.data.frame(st_coordinates(grid))
 
 names(test_df) <- c("long", "lat")
 coordinates(test_df) <- ~ long + lat
 proj4string(test_df) <-CRS("+proj=utm +zone=10 +datum=WGS84")
 
 krig_auto <- autoKrige(count ~1, input_data = bbs_ont2_sub, new_data = test_df)
 
 krig_df <- data.frame(krig_auto$krige_output)
 #coordinates(map_lower) <- ~long + lat
 
 # try and plot a krige map
 bbs_ont_krig <- map_lower %>% 
   ggplot() + 
   geom_blank() + 
   geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = 'black') + 
   geom_point(data = krig_df, aes(x = long, y = lat, color = var1.pred))
 ggsave(bbs_ont_krig, file = "bbs_krig_attempt3.pdf")
 
 
 library(fields)
 bbs_10 <- bbs_ont2_su
 fit <- Krig(bbs_ont2_sub@coords, bbs_ont2_sub$count, Range=1)   
 surface( fit, type="C")
 
 # find distance between points
 dist_points <- bbs_ont2_sub %>%
   mutate(
     dist = geosphere::distHaversine(cbind(homelong, homelat), cbind(worklong, worklat))
   )
 