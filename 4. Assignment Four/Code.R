# Assignment 4
# clear the environment
rm(list = ls(all = TRUE))

# Set up my work space
setwd("C:/Users/tulou/Desktop/PSU_Classes/G_324/Assignment_Four")
# Run these libraries
library(tidyverse)
library(googlesheets4)
library(ggtern)
library(png)
library(ggplot2)

# authenticates your google account
gs4_auth()

# Link to googlesheet
myURL <- "https://docs.google.com/spreadsheets/d/1wyD7IWT0rouZI_Gw8eh4rHIO70dWdIvnEEZ_LOWIxWU/edit#gid=531586939"

#Elements Table
ElementTableOne <- read_sheet(myURL, range = "ICP_INAAdata!A:AB", .name_repair = "unique") %>% 
  select(SampleID, Ba, Ce, Dy, Eu, Hf, La, Lu, Nb, Nd, Pb, Pr, Rb, Sm, Sr, Ta, Tb, Th, U, Y, Yb, Zr)
view(ElementTableOne)

ElementTable <- pivot_longer(ElementTableOne, -c('SampleID'), names_to = 'element', values_to = 'values')
view(ElementTable)

#Samples Table, include SampleID, Unit, Age, Latitude, Longitude
SampleTable <- read_sheet(myURL, range = "Samples!A:F", .name_repair = "unique") %>% 
  select(SampleID, Unit, Age, Latitude, Longitude)
view(SampleTable)

SamplesTable <- inner_join(SampleTable, ElementTable)
view(SamplesTable)

#Mantle Elements Table
MantleTable <- read_sheet(myURL, range = "Constants!G2:H26", .name_repair = "unique")
view(MantleTable)

METable <- full_join(SamplesTable, MantleTable)
view(METable)

#Average of Mantle Elements 
AveMETable <- function(num){
  if(num <= 13){
    METable %>% 
    as_tibble() %>% 
    rowwise() %>% 
    mutate(AveMentle = values / val) %>% 
    mutate(Age = round(Age, 0)) %>% 
    filter(, Age < num)
  }else{
    METable %>% 
    as_tibble() %>% 
    rowwise() %>% 
    mutate(AveMentle = values / val) %>% 
    mutate(Age = round(Age, 0)) %>%
    filter(, Age > num)
  }
}

#plot my data on the graph
MantleGraph <- function(name){
  ggplot(name, mapping = aes(x = element, y = val, group = element)) +
  ylim(0, 1000) +
  geom_point(, mapping = aes(x = element, y = AveMentle, group = Unit, fill = Unit)) +
  geom_line(aes(x= element, y = AveMentle, group = Unit, color = Unit)) +
  theme_bw() + 
  theme_showarrows() +
  weight_percent() +
  facet_wrap(~Age) + 
  theme(legend.position = "bottom")
}
MantleGraph(AveMETable(13))
MantleGraph(AveMETable(14))

#CI Chondrite Elements Table 
CITable <- read_sheet(myURL, range = "Constants!J2:K26", .name_repair = "unique")
view(CITable)

CIETable <- full_join(SamplesTable, CITable)
view(CIETable)

#Average of CI Chondrite Elements
AveCIETable <- CIETable %>% 
  as_tibble() %>% 
  rowwise() %>% 
  mutate(AveCI = val / values) %>% 
  mutate(Age = round(Age, 0))
view(AveCIETable)

#plot my data on the graph(CI Chondrite)
CIGraph <- ggplot(AveCIETable, mapping = aes(element, val, group = element)) +
  ylim(0.00001, 1) +
  geom_point(, mapping = aes(x = element, y = AveCI)) +
  geom_line(, mapping = aes(x = element, y = AveCI, group = Unit, color = Unit)) +
  theme_bw() + 
  theme_showarrows() +
  weight_percent() +
  facet_wrap(~Age) + 
  theme(legend.position = "bottom")

CIGraph


