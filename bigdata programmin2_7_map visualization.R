#install.packages("ggmap")
#install.packages("ggplot2")
#install.packages("raster")
#install.packages("rgeos")
#install.packages("maptools")
#install.packages("rgdal")

library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
.libPaths()

setwd('C:/Rworks/project/map/2021')
map <- shapefile('TL_SCCO_SIG2.shp',fileEncoding="euc-kr")
map <- spTransform(korea,CRS("+proj=longlat"))

df_map = fortify(map)
df_map2 = map@data

df_map2 <- (df_map2[order(df_map2$SIG_CD),])
map@data <- df_map2

df_map2$SIG_KOR_NM <- iconv(df_map2$SIG_KOR_NM, from = "CP949", to = "UTF-8", sub = NA, mark = TRUE, toRaw = FALSE)

data3 <- read.csv('C:/Rworks/project/cancer(eng)3.csv',
                  header=T, fileEncoding="euc-kr")

cancer1 <- data.frame(data3)
cancer2 <- subset(cancer1, sex == '계')
cancer2[, "id"] = (1:nrow(df_map2))

merge_result <- merge(df_map,cancer2,by='id')
head(merge_result)
#그래프
ggplot() + geom_polygon(data = merge_result, aes(x=long, y=lat, group=group, fill = standariztion13))
