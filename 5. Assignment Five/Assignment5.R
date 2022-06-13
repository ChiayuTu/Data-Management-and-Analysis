#this code is to clear the terminal.
rm(list = ls(all = 1))

#this code is my file save path which I have given the name of 'savefile'.
savefile =  "C:/Users/tulou/Desktop/PSU_Classes/G_324/Assignment_Five"


#this code is to load tidyverse and googlesheets4, make sure googlesheets4 is loaded
#otherwise the google authenticate function gs4__auth()
#also will allow us to plot data on the map.
library(tidyverse)
library(googlesheets4)  
library(png)
library(dplyr)
library(readxl)
library(janitor)
library(ggrepel)


#install.packages(c('dplyr', 'readxl')

#this code is to login to my google account. 

gs4_auth("tuchiayu@pdx.edu")

#this code is to establish that the object Puffmaster is my url for my google sheet of interest.
puffmaster <- "https://docs.google.com/spreadsheets/d/1sup9_iIEcPW5ulxAtzm9BIDD8LWTANsln0sUKrM7l5s"


smoke <- read_sheet(puffmaster, range = 'Master Sheet (corrected)!A3:CD45', .name_repair=unique)
#view(smoke)

dirt <- read_sheet(puffmaster, range= ('Soil_Info!A1:F6'), .name_repair = unique)
#view(dirt)

THC <- select(smoke, PlantID, PlanterPosition, Strain, 7, 8, 10)
# THC <- select(SoilData, PlantID, PlanterPosition, Strain, 7, 8, 10)
#view(THC)

SoilData <- smoke %>%
  #select(Soil, PlantID, PlanterPosition, "pH (soil)":"Mg_T/Mg_s (plant/soil)") %>%
  rename("WetWeight" = "WetWeight(harvested)",
         "Total_THT" = "TotalTHC(plant)",
         "Total_Cannabinoids" = "TotalCannabinoids(plant)",
         "Moisture_Percent" = "Moisture%(plant)",
         "Total_Terpenes" = "TotalTerpenes(plant)",
         "Total_Nitrogen" = "TotalNitrogen(plant)",
         "Total_Carbon" ="Total Carbon  (plant)",
         "C/NRatio" = "C/NRatio (plant)",
         "P_T" = "P_T  (plant)",
         "P2O5_T" = "P2O5_T (plant)",
         "K_T" = "K_T(plant)",
         "K2O_T" = "K2O_T (plant)",
         "Ca_T" = "Ca_T (plant)",
         "Mg_T" = "Mg_T (plant)",
         "Na_T" = "Na_T (plant)",
         "S_T" = "S_T (plant)",
         "Cu_T" = "Cu_T (plant)",
         "Zn_T" = "Zn_T (plant)",
         "Mn_T" = "Mn_T (plant)",
         "Fe_T" = "Fe_T (plant)",
         "B_T" = "B_T (plant)",
         "Mo_T" = "Mo_T (plant)",
         "Tissue_Nitrate_Nitrogen" = "Tissue Nitrate Nitrogen (plant)",
         "pH" = "pH (soil)",
         "Soluble_Salts" = "Soluble Salts (ppm) (soil)",
         "Chloride(Cl)" = "Chloride (Cl) (ppm) (soil)",
         "Bicarbonate(HCO3)" = "Bicarbonate (HCO3) (ppm) (soil)",
         "Sulfur" = "Sulfur (ppm) (soil)",
         "Phosphorus" = "Phosphorus (ppm) (soil)",
         "Calcium" = "Calcium (ppm) (soil)",
         "Magnesium" = "Magnesium (ppm) (soil)",
         "Potassium" = "Potassium (ppm) (soil)",
         "Sodium" = "Sodium (ppm) (soil)",
         "Calcium_Percent" = "Calcium (%) (soil)",
         "Magnesium_Percent" = "Magnesium (%) (soil)",
         "Potassium_Percent" = "Potassium (%) (soil)",
         "Sodium_Percent" = "Sodium (%) (soil)",
         "Boron" = "Boron (ppm) (soil)",
         "Iron" = "Iron (ppm) (soil)",
         "Manganese" = "Manganese (ppm) (soil)",
         "Copper" = "Copper (ppm) (soil)",
         "Zinc" = "Zinc (ppm) (soil)",
         "Aluminum" = "Aluminum (ppm) (soil)",
         "Ca/Mg" = "Ca/Mg (soil)",
         "Ca/B" = "Ca/B (soil)",
         "Ca/Mn" = "Ca/Mn (soil)",
         "Mg/Mn" = "Mg/Mn (soil)",
         "Mg_T/Mg_s" = "Mg_T/Mg_s (plant/soil)",
         "Camphene" = "Camphene (plant)",
         "3_Carene" = "3-Carene (plant)",
         "Endo_Fenchyl Alcohol" = "Endo-Fenchyl Alcohol (plant)",
         "Eucalyptol" = "Eucalyptol (plant)",
         "Fenchone" = "Fenchone (plant)",
         "Limonene" = "Limonene (plant)",
         "Linalool" = "Linalool (plant)",
         "p_Mentha_l_5-Diene" = "p-Mentha-l, 5-Diene (plant)",
         "Beta_Myrcene" = "Beta-Myrcene (plant)",
         "Ocimene" = "Ocimene (plant)",
         "Alpha_Pinene" = "Alpha-Pinene (plant)",
         "Beta_Pinene" = "Beta-Pinene (plant)",
         "Sabinene" = "Sabinene (plant)",
         "Sabinene_hydrate" = "Sabinene hydrate (plant)",
         "Gamma_Terpinene" = "Gamma-Terpinene (plant)",
         "Alpha_Terpinene" = "Alpha-Terpinene (plant)",
         "Terpineol" = "Terpineol (plant)",
         "Terpinolene" = "Terpinolene (plant)",
         "A_Terineol" = "A-Terineol (plant)",
         "Borneol" = "Borneol (plant)",
         "Ocimene_isomer_ll" = "Ocimene isomer ll (plant)",
         "Ocimene_isomer_l" = "Ocimene isomer l (plant)",
         "Alpha-Bisabolol" = "Alpha-Bisabolol (plant)",
         "Beta_Caryophyllene" = "Beta-Caryophyllene (plant)",
         "Caryophyllene_Oxide" = "Caryophyllene Oxide (plant)",
         "Alpha_Humulene" = "Alpha-Humulene (plant)",
         "Trans_Nerolidol" = "Trans-Nerolidol (plant)",
         "Valencene" = "Valencene (plant)",
         "Cis_Nerolidol" = "Cis-Nerolidol (plant)")
# view(SoilData)

#Planter Diagram Table
PlanterDiagram <- read_sheet(puffmaster, range = "Planter Diagram!B20:C28", .name_repair = "unique")
PlanterDiagram <- PlanterDiagram[-c(6),] %>% 
  rename("Farm" = "...2")
#view(PlanterDiagram)

PlantTerpeneData <- smoke %>% 
  select(PlantID, PlanterPosition, "Camphene (plant)":"Cis-Nerolidol (plant)") %>% 
  clean_names()
#view(PlantTerpeneData)
# get column names
# colnames(smoke)
# colnames(SoilData)
# colnames(PlantTerpeneData)
# rlang::last_error()

#Join PlanterDiagram table and dirt table 
SoilInfo <- full_join(PlanterDiagram, dirt)
#view(SoilInfo)

# FullTable <- left_join(smoke, SoilInfo, by = c("Soil" = "Raised Beds")) %>% 
#   relocate(c("Farm":"Soil Series"), .after = PlanterPosition)
FullTable <- left_join(SoilData, SoilInfo, by = c("Soil" = "Raised Beds")) %>%
  relocate(c("Farm":"Soil Series"), .after = PlanterPosition) %>% 
  rename("Soil_Order" = "Soil Order",
         "Soil_Subgroup" = "Soil Subgroup",
         "Soil_Series" = "Soil Series")
view(FullTable)
colnames(FullTable)


#Loop 
VariableList <- colnames(FullTable[12:88])
#view(VariableList)
nVariables <- length(VariableList)
ngraphs <- 0

for (i in 1:(nVariables-1)) {
  for (j in (i+1):nVariables) {
    ngraphs <- ngraphs + 1
    subText <- paste(ngraphs, "--", VariableList[i], "vs", VariableList[j])
    
    plot (
      FullTable %>%
        ggplot(aes(x = get(VariableList[i]), y = get(VariableList[j]))) +
        geom_point(aes(color = "TotalTHC(plant)"), show.legend = FALSE) +
        labs(x = VariableList[i], 
             y = VariableList[j], 
             subtitle = subText)
    )
  }
}

#test plot ton the graph
test1 <- lm(formula = Moisture_Percent~Total_Terpenes,
            data = FullTable)
summary(test1) 

testOne <- plot(test1$residuals)

#X axis is SoilOrder, Y axis is the value of TotalTerpenes, group by Soil Subgroup
ggplot(FullTable, aes(x = Soil_Subgroup, y = Total_Terpenes)) +
  geom_point(aes(color = Soil_Order)) 
  
#Terpense Table
TerpenseTable <- FullTable %>% 
  select("Soil":"PlanterPosition",
         "Soil_Order",
         "Soil_Subgroup",
         "Total_Terpenes",
         "Gamma_Terpinene":"Terpinolene")

TerpenseTable <- pivot_longer(TerpenseTable, -c("Soil":"Total_Terpenes"), 
                              names_to = "Terpense_Name", 
                              values_to = "Terpense_Value")
view(TerpenseTable)

ggplot(TerpenseTable, aes(x = Total_Terpenes, y = Terpense_Value)) +
  geom_point(aes(color = Terpense_Name)) +
  geom_line(aes(group = Terpense_Name, color = Terpense_Name)) + 
  facet_wrap(~Soil_Subgroup)
#Function shows plot and linear regression

#Create a new table, which include Soil to Farm, Soil_Order
#Soil_Subgroup, Total_Terpenes, and Camphene to Cri_Nerolidol
Test2 <- FullTable %>% 
  select("Soil":"Farm",
         "Soil_Order",
         "Soil_Subgroup",
         "Total_Terpenes",
         "Camphene":"Cis_Nerolidol")
view(Test2)

Test3 <- FullTable %>%
  select("Soil":"Farm",
         "Soil_Order",
         "Soil_Subgroup",
         "Total_Terpenes",
         "Alpha_Pinene",
         "Beta_Caryophyllene",
         "Beta_Myrcene",
         "Beta_Pinene",
         "Camphene",
         "Cis_Nerolidol",
         "Gamma_Terpinene",
         "Limonene",
         "Linalool",
         "Ocimene",
         "Ocimene_isomer_ll",
         "Terpinolene")

#Pivot longer the table
pivotTest2 <- pivot_longer(Test2, -c("Soil":"Total_Terpenes"), 
                           names_to = "Elevents",
                           values_to = "Values")  

pivotTest3 <- pivotTest2 %>% 
  filter(Values < 0.002)

# pivotTest4 <- pivotTest2 %>% 
#   filter(Values >= 0.002)
pivotTest5 <- pivot_longer(Test3, -c("Soil":"Total_Terpenes"),
                           names_to = "Elevents",
                           values_to = "Values")

view(pivotTest3)
#Plot
ggplot(pivotTest3, aes(x = Total_Terpenes, y = Values)) +
  geom_point(aes(color = Elevents)) +
  geom_line(aes(group = Elevents, color = Elevents)) + 
  facet_wrap(~Soil_Subgroup)

ggplot(pivotTest4, aes(x = Total_Terpenes, y = Values)) +
  geom_point(aes(color = Elevents)) +
  geom_line(aes(group = Elevents, color = Elevents)) + 
  facet_wrap(~Soil_Subgroup)

ggplot(pivotTest5, aes(x = Total_Terpenes, y = Values)) +
  geom_point(aes(color = Elevents)) +
  geom_line(aes(group = Elevents, color = Elevents)) + 
  facet_wrap(~Soil_Subgroup)

#Terpenes
AllTerpenesTable <- FullTable %>% 
  select("Soil":"Farm",
         "Soil_Order",
         "Soil_Subgroup",
         "Total_Terpenes",
         "Camphene":"Terpinolene")
# view(AllTerpenesTable)

AllTerpenesTable <- pivot_longer(AllTerpenesTable,
                                 -c("Soil":"Total_Terpenes"),
                                 names_to = "Terpenes",
                                 values_to = "Values")
view(AllTerpenesTable)

Plot_AllTerpenes <- ggplot(AllTerpenesTable, aes(x = Total_Terpenes, y = Values)) +
  geom_point(aes(color = Terpenes)) +
  geom_line(aes(group = Terpenes, color = Terpenes)) +
  geom_label_repel(data = filter(AllTerpenesTable, Total_Terpenes == max(AllTerpenesTable$Total_Terpenes)),
                   aes(label = Terpenes)) +
  labs(x = "Total Terpene", 
       y = "Individual Terpene",
       title = "Total Terpene vs individual terpene values") +
  #geom_text(aes(label = Terpenes, size = NULL)) +
  facet_wrap(~Soil_Subgroup)
Plot_AllTerpenes

#Each Soil group
Mollisols_Terpenes <- AllTerpenesTable %>% 
  filter(Soil_Order == "Mollisols")
view(Mollisols_Terpenes)

#Add title: Total Terpene vs individual terpene values for Mollisols
Plot_Mollisols <- ggplot(Mollisols_Terpenes, aes(x = Total_Terpenes, y = Values)) +
  geom_point(aes(color = Terpenes)) +
  geom_line(aes(group = Terpenes, color = Terpenes)) + 
  geom_label_repel(data = filter(Mollisols_Terpenes, Total_Terpenes == max(Mollisols_Terpenes$Total_Terpenes)),
                   aes(label = Terpenes)) +
  labs(x = "Total Terpene", 
       y = "Individual Terpene",
       title = "Total Terpene vs individual terpene values for Mollisols")
  #geom_text(aes(label = Terpenes, size = NULL)) +
  #facet_wrap(~Soil_Subgroup)
Plot_Mollisols


Alfisols_Terpenes <- AllTerpenesTable %>% 
  filter(Soil_Order == "Alfisols")
view(Alfisols_Terpenes)

Plot_Alfisols <- ggplot(Alfisols_Terpenes, aes(x = Total_Terpenes, y = Values)) +
  geom_point(aes(color = Terpenes)) +
  geom_line(aes(group = Terpenes, color = Terpenes)) +
  geom_label_repel(data = filter(Alfisols_Terpenes, Total_Terpenes == max(Alfisols_Terpenes$Total_Terpenes)),
                   aes(label = Terpenes)) +
  labs(x = "Total Terpene", 
       y = "Individual Terpene",
       title = "Total Terpene vs individual terpene values for Alfisols") +
  #geom_text(aes(label = Terpenes, size = NULL)) +
  facet_wrap(~Soil_Subgroup)
Plot_Alfisols


Ultisols_Terpenes <- AllTerpenesTable %>% 
  filter(Soil_Order == "Ultisols")

Plot_Ultisols <- ggplot(Ultisols_Terpenes, aes(x = Total_Terpenes, y = Values)) +
  geom_point(aes(color = Terpenes)) +
  geom_line(aes(group = Terpenes, color = Terpenes)) +
  geom_label_repel(data = filter(Ultisols_Terpenes, Total_Terpenes == max(Ultisols_Terpenes$Total_Terpenes)),
                   aes(label = Terpenes)) +
  labs(x = "Total Terpene", 
       y = "Individual Terpene",
       title = "Total Terpene vs individual terpene values for Ultisols") +
  #geom_text(aes(label = Terpenes, size = NULL)) +
  facet_wrap(~Soil_Subgroup)
Plot_Ultisols
