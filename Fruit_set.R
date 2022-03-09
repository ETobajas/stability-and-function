
getwd()
setwd("C:/Rfuncion")
list.files()

library(readxl)
library(haven)
library(dplyr)
library(reshape2)
library(tidyr)



#### Fruit set ####

excel_fruit <- read_excel("BeeFunMaster.xlsx",sheet = "Fruit set")

#combine genus and species columns
excel_fruit $ sp_plant <- paste (excel_fruit$Plant_genus, excel_fruit$Plant_species, sep = " ")


# remove "El hongo", not sampled in 2015
excel_fruit <- excel_fruit[!(excel_fruit$Site_ID == "El hongo"),]
excel_fruit <- excel_fruit[!(excel_fruit$Site_ID == "El Hongo"),]


data.frame(table(excel_fruit$Site_ID))

# modified "urbanizacion" by "Urbanizaciones"
excel_fruit$Site_ID[excel_fruit$Site_ID=="Urbanizacion"] <- "Urbanizaciones"

# 16 site and 4 years
levels(factor(excel_fruit$Site_ID))
levels(factor(excel_fruit$Year)) 


# remove "Lavandula NA"
excel_fruit <- excel_fruit[!(excel_fruit$sp_plant == "Lavandula NA"),]

levels(factor(excel_fruit$sp_plant))

# frequency of species plant per site
plant_site<-data.frame(table(excel_fruit$Site_ID, excel_fruit$sp_plant))
plant_site

# Replace NA in "Fruit_Yes" and "Fruit_No" columns
excel_fruit <- mutate_at(excel_fruit, c("Fruit_Yes", "Fruit_No"), ~replace(., is.na(.), 0))


# add column Fruit_Total (Fruit_Yes + Fruit_No)
excel_fruit$Fruit_Total = rowSums (excel_fruit[ , 9:10])

# Fruit proportion (Fruit yes / Fruit total)
excel_fruit<-excel_fruit %>% mutate(Fruit_proportion = Fruit_Yes/Fruit_Total)


# mean fruit proportion of species plants per site and year
Fruit_set<-aggregate(Fruit_proportion ~ Year+sp_plant+Site_ID, data = excel_fruit, FUN = mean)


# Export dataframe to csv and excell 
write.csv(Fruit_set, "my_fruitset.csv") 

install.packages("writexl")
library(writexl)
write_xlsx(Fruit_set, "C:/Rfuncion/my_fruitset.xlsx")
