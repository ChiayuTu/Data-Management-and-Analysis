# clear the decks
rm(list = ls(all = TRUE))

# set up your target directory, which needs to be created first
SaveFile <- "C:/Users/tulou/Desktop/PSU_Classes/G_324/Assignment_Two"
library(tidyverse)
library(maps)
library(mapdata)


#Part One - Download the map data
website <- "https://earthquake.usgs.gov/fdsnws/event/1/query"
qryFormat <- "format=csv"
qryStime <- "starttime=2018-01-01"
qryEndtime <- "endtime=2018-12-31"
qryMinlat <- "minlatitude=41"
qryMaxLat <- "maxlatitude=51"
qryMinLong <- "minlongitude=-129"
qryMaxLong <- "maxlongitude=-116"
qryMinMag <- "minmagnitude="
qryMaxMag <- "maxmagnitude="
qryMinDepth <- "mindepth="
qryMaxDepth <- "maxdepth="
qryLimit <- "limit="

#paste(): Concatenate vectors after converting to character.
#sep: a character string to separate the terms.
qryStr <- paste(qryFormat, qryStime, qryEndtime, qryMinlat, qryMaxLat, qryMinLong, qryMaxLong, sep = "&")

webQuery <- paste(website, qryStr, sep = "?")

targetFile <- "EarthquakeFile.csv"

#download.file: the function can be used to download a file from internet
#quiet: If TRUE, suppress(??????,??????) status messages (if any), and the progress bar.
download.file(webQuery, targetFile, quiet = TRUE)
#eqDF <- read.csv(targetFile, header = TRUE)
#header: a logical value indicating whether the file contains the names of the variables
#as its first line. If missing, the value is determined from the file format: header is 
#set to TRUE if and only if the first row contains one fewer field than the number of 
#columns.
eqDF <- as_tibble(read.csv(targetFile, header = TRUE))
view(eqDF)

#ggplot(eqDF, aes(x = longitude, y = latitude)) +
# geom_point();


#Part Two - create maps lat/long
#mapStates <- map_data("state")
mapStates <- map_data("state")
mapUSA <- subset(mapStates, region %in% c("oregon", "washington"))
#view(mapUSA)

#Part Three - Filter data and play with multivariate plots
filteredDFfA <- filter(eqDF, mag>0, type == "earthquake", 
                      magType %in% c("mb", "md", "mh", "ml", "mw", "mwr"))

#Question One
QuestionOne <- ggplot() +
  geom_polygon(data = mapUSA, fill = "wheat3", aes(long, lat, group = group)) +
  geom_point(data = filteredDFfA, aes(longitude, latitude, color = depth, size = mag)) +
  scale_color_gradient(low = "green", 
                       high = "red") +
  coord_quickmap() +
  scale_size(range = c(1, 3)) +
  theme_bw() + 
  #facet_wrap(~magType) +
  labs(caption = "Figure One: Spatial Distribution of Earthquakes in the Pacific Northwest",
       x = "Longitude (°)",
       y = "Latitude  (°)",
       color = "Depth",
       size = "Magniture") +
  theme(axis.title = element_text(family = "Times", size = 10),
        legend.title = element_text(family = "Times", size = 10),
        legend.position = "bottom",
        plot.caption = element_text(family = "Times", size = 20, hjust = 0))

  #facet_grid(~magType)
#QuestionOne

#ggsave(file = paste(SaveFile, "A2_EQ-dept.png", sep = ""), 
#      width = 4, height = 5, units = 'in')

#Question Two
filteredDFfB <- filter(eqDF, mag>4, type == "earthquake", 
                      magType %in% c("mb", "md", "mh", "ml", "mw", "mwr"))
#magType %in% c("mb", "md", "mh", "ml", "mw", "mwr", "mww")

QuestionTwo <- ggplot() +
  geom_polygon(data = mapUSA, fill = "wheat3", aes(long, lat, group = group)) +
  geom_point(data = filteredDFfB, aes(longitude, latitude, color = depth, size = mag)) +
  scale_color_gradient(low = "green", 
                       high = "red") +
  coord_quickmap() +
  scale_size(range = c(1, 3)) +
  theme_bw() +
  labs(caption = "Figure Two: More Intense Earthquake",
       x = "Longitude (°)",
       y = "Latitude  (°)",
       color = "Depth",
       size = "Magniture") +
  theme(axis.title = element_text(family = "Times", size = 10),
        legend.title = element_text(family = "Times", size = 10),
        legend.position = "bottom",
        plot.caption = element_text(family = "Times", size = 20, hjust = 0))

#QuestionTwo


#Question Three
#ggplot to creat a cross-section chart
QuestionThree <- ggplot() +
  geom_point(data = filteredDFfA, 
             aes(x = longitude, y = depth, color = mag)) +
  scale_color_gradient(low = "green", 
                       high = "red") +
  scale_y_reverse() +
  labs(caption = "Figure Three: E-W cross-section",
       x = "Longitude (°)",
       y = "Depth (km)",
       color = "Magniture") +
  theme(axis.title = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = "bottom",
        plot.caption = element_text(size = 20, hjust = 0))

#QuestionThree

#Question Four
qryFormatS <- "format=csv"
qryStimeS <- "starttime=2018-01-01"
qryEndtimeS <- "endtime=2018-12-31"
qryMinlatS <- "minlatitude=-60"
qryMaxLatS <- "maxlatitude=0"
qryMinLongS <- "minlongitude=-81"
qryMaxLongS <- "maxlongitude=-62"
qryMinMagS <- "minmagnitude="
qryMaxMagS <- "maxmagnitude="
qryMinDepthS <- "mindepth="
qryMaxDepthS <- "maxdepth="
qryLimitS <- "limit="

#paste(): Concatenate vectors after converting to character.
#sep: a character string to separate the terms.
qryStrS <- paste(qryFormatS, 
                 qryStimeS, 
                 qryEndtimeS, 
                 qryMinlatS, 
                 qryMaxLatS, 
                 qryMinLongS, 
                 qryMaxLongS, 
                 sep = "&")

webQueryS <- paste(website, qryStrS, sep = "?")

targetFileS <- "EarthquakeFileChile.csv"

download.file(webQueryS, targetFileS, quiet = TRUE)

eqDFA <- as_tibble(read.csv(targetFileS, header = TRUE))
view(eqDFA)

mapData <- map_data("world")
SouthAmerica <- subset(mapData, region %in% c("Chile", "Argentina", "peru"))

filtereqDFS <- filter(eqDFA, mag > 0, type == "earthquake", 
                      magType %in% c("mb", "md", "mh", "ml", "mw", "mwr"))

#Use ggplot to create a grap for SouthAmerica
QuestionFourFigure <- ggplot() +
  geom_polygon(data = SouthAmerica, 
               fill = "wheat3", 
               aes(long, lat, group = group)) +
  geom_point(data = filtereqDFS, 
             aes(longitude, latitude, color = depth, size = mag)) +
  scale_color_gradient(low = "green", 
                       high = "red") +
  coord_quickmap() +
  scale_size(range = c(1, 3)) +
  labs(caption = "Figure Four: South America Coastal Region",
       x = "Longitude (°)",
       y = "Latitude  (°)",
       color = "Depth",
       size = "Magniture") +
  theme(axis.title = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = "bottom",
        plot.caption = element_text(size = 20, hjust = 0))

filtereqDFSA <- filter(eqDFA, mag > 4, type == "earthquake", 
                      magType %in% c("mb", "md", "mh", "ml", "mw", "mwr"))

QuestionFour <- ggplot() +
  geom_point(data = filtereqDFSA, 
             aes(x = longitude, y = depth, color = mag)) +
  scale_color_gradient(low = "green", 
                       high = "red") +
  scale_y_reverse() +
  labs(caption = "Figure Four: cross-section South America",
       x = "Longitude (°)",
       y = "Depth (km)",
       color = "Magniture") +
  theme(axis.title = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = "bottom",
        plot.caption = element_text(size = 20, hjust = 0))



QuestionOne
QuestionTwo
QuestionThree
QuestionFourFigure 
QuestionFour










