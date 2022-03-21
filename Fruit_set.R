
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
excel_fruit $ Plant_sp <- paste (excel_fruit$Plant_genus, excel_fruit$Plant_species, sep = " ")


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
excel_fruit <- excel_fruit[!(excel_fruit$Plant_sp == "Lavandula NA"),]

# Remove Astragalus lusitanicus in fruit set (no present in focal)
excel_fruit <- excel_fruit[!(excel_fruit$Plant_sp == "Astragalus lusitanicus"),]

levels(factor(excel_fruit$Plant_sp))

# frequency of species plant per site
plant_site<-data.frame(table(excel_fruit$Site_ID, excel_fruit$Plant_sp))
plant_site

# Replace NA in "Fruit_Yes" and "Fruit_No" columns
excel_fruit <- mutate_at(excel_fruit, c("Fruit_Yes", "Fruit_No"), ~replace(., is.na(.), 0))


# add column Fruit_Total (Fruit_Yes + Fruit_No)
excel_fruit$Fruit_Total = rowSums (excel_fruit[ , 9:10])

# Fruit proportion (Fruit yes / Fruit total)
excel_fruit<-excel_fruit %>% mutate(Fruit_proportion = Fruit_Yes/Fruit_Total)


# mean fruit proportion of species plants per site and year
Fruit_set<-aggregate(Fruit_proportion ~ Year+Plant_sp+Site_ID, data = excel_fruit, FUN = mean)



# Export dataframe to csv and excell 
write.csv(Fruit_set, "my_fruitset.csv") 

install.packages("writexl")
library(writexl)
write_xlsx(Fruit_set, "C:/Rfuncion/my_fruitset.xlsx")



###############

### Focal ###

excel_focal <- read_excel("BeeFunMaster.xlsx",sheet = "Focal")

# remove years (2021,2020,2019)
levels(factor(excel_focal$Year))

excel_focal <- excel_focal[!(excel_focal$Year == "2021"),]
excel_focal <- excel_focal[!(excel_focal$Year == "2020"),]
excel_focal <- excel_focal[!(excel_focal$Year == "2019"),]

levels(factor(excel_focal$Year))

# Sites #
levels(factor(excel_focal$Site_ID))

# remove "El hongo", not sampled in 2015
excel_focal <- excel_focal[!(excel_focal$Site_ID == "El hongo"),]
excel_focal <- excel_focal[!(excel_focal$Site_ID == "El Hongo"),]

# modified Site_ID
excel_focal$Site_ID[excel_focal$Site_ID=="El Pozo"] <- "El pozo"
excel_focal$Site_ID[excel_focal$Site_ID=="La_cunya"] <- "La Cuña"
excel_focal$Site_ID[excel_focal$Site_ID=="Pinar del Cuervo"] <- "Pino del Cuervo"
excel_focal$Site_ID[excel_focal$Site_ID=="Pino del cuervo"] <- "Pino del Cuervo"
excel_focal$Site_ID[excel_focal$Site_ID=="VIllamanrique este"] <- "Villamanrique este"
excel_focal$Site_ID[excel_focal$Site_ID=="La_rocina"] <- "La Rocina"

levels(factor(excel_focal$Site_ID))


# Orden # 

# Remove NA in Orden column
excel_focal<-excel_focal%>% drop_na (Orden)

# Remove Arachnida, Aranea, Hemiptera, Neuroptera
levels(factor(excel_focal$Orden))

excel_focal <- excel_focal[!(excel_focal$Orden == "Arachnida"),]
excel_focal <- excel_focal[!(excel_focal$Orden == "Araneae"),]
excel_focal <- excel_focal[!(excel_focal$Orden == "Hemiptera"),]
excel_focal <- excel_focal[!(excel_focal$Orden == "Neuroptera"),]

levels(factor(excel_focal$Orden))

# Remove Lepidoptera
excel_focal <- excel_focal[!(excel_focal$Orden == "Lepidoptera"),]
levels(factor(excel_focal$Orden))


# remove wasp, kleptoparasite, ants (hymenoptera: bees and megascolia) 
levels(factor(excel_focal$Pollinator_genus))

w<-c("Bembix","Cerceris","Chrysididae","Chrysis","Chrysura","Coelioxys", "Colpa","Corynis",
     "Dolichovespula","Eodymerus","Eumenes","Gorytes","Hormiga","Ichneumonidae","Lestica",
     "Macrophya","Megalodontes","Oxybelus","Pemphredon","Philantus","Podalonia","Polistes",
     "Sphecodes","Tachysphex","Thyreus","Vespula")


excel_focal = excel_focal %>% filter(!Pollinator_genus %in% w)

# remove vanessa - (lepidoptera not hymenoptera)
excel_focal <- excel_focal[!(excel_focal$Pollinator_genus == "Vanessa"),]


#checking Hymenoptera
hym= excel_focal %>% filter(Orden %in% "Hymenoptera")
levels(factor(hym$Pollinator_genus))


# correct species/families within orden (excel_focal)
excel_focal$Orden = ifelse(excel_focal$Pollinator_genus == "Bombilidae", "Diptera", excel_focal$Orden)
excel_focal$Orden = ifelse(excel_focal$Pollinator_genus == "Parageron", "Diptera", excel_focal$Orden)
excel_focal$Orden = ifelse(excel_focal$Pollinator_genus == "Heliotaurus", "Coleoptera", excel_focal$Orden)

excel_focal$Pollinator_genus[excel_focal$Pollinator_genus=="Xilocopa"] <- "Xylocopa"


#checking Diptera
dip= excel_focal %>% filter(Orden %in% "Diptera")
levels(factor(dip$Pollinator_genus))


# remove Callophrys - (lepidoptera not diptera)
excel_focal <- excel_focal[!(excel_focal$Pollinator_genus == "Callophrys"),]

# correct species/families within orders (excel_focal)
excel_focal$Orden = ifelse(excel_focal$Pollinator_genus == "Xylocopa", "Hymenoptera", excel_focal$Orden)
excel_focal$Orden = ifelse(excel_focal$Pollinator_genus == "Psylothrix", "Coleoptera", excel_focal$Orden)

excel_focal$Pollinator_genus[excel_focal$Pollinator_genus=="Bombilidae"] <- "Bombyliidae"
excel_focal$Pollinator_genus[excel_focal$Pollinator_genus=="Bombillidae"] <- "Bombyliidae"
excel_focal$Pollinator_genus[excel_focal$Pollinator_genus=="Bombilius"] <- "Bombylius"
excel_focal$Pollinator_genus[excel_focal$Pollinator_genus=="Rhyncomya"] <- "Rhyncomyia"
excel_focal$Pollinator_genus[excel_focal$Pollinator_genus=="Sirfidae"] <- "Syrphidae"
excel_focal$Pollinator_genus[excel_focal$Pollinator_genus=="Sirphidae"] <- "Syrphidae"


# remove all that is not Bombilidos, sirfidos and rhyncomyia (Diptera)
d<-c("Asilidae","Asilido","Bibio","Bibionido", "Dasypogon","Dilophus","Diptero",
     "Empis","Lucilia","Miltogramma","Mosca","Musca","Myopa","Sarcophaga","Stenorrhina",
     "Stomorhina","Stratyiomis")

excel_focal = excel_focal %>% filter(!Pollinator_genus %in% d)


#checking Coleoptera
cole= excel_focal %>% filter(Orden %in% "Coleoptera")
levels(factor(cole$Pollinator_genus))

# correct species/families within orders (excel_focal)
excel_focal$Orden = ifelse(excel_focal$Pollinator_genus == "Lasioglossum", "Hymenoptera", excel_focal$Orden)

excel_focal$Pollinator_genus[excel_focal$Pollinator_genus=="Psylothrix"] <- "Psilothrix"
excel_focal$Pollinator_genus[excel_focal$Pollinator_genus=="Chasmaptoterus"] <- "Chasmatopterus"
excel_focal$Pollinator_genus[excel_focal$Pollinator_genus=="Chryptocephalus"] <- "Cryptocephalus"

# remove Riphiphoridae - (parasite)
excel_focal <- excel_focal[!(excel_focal$Pollinator_genus == "Riphiphoridae"),]


levels(factor(excel_focal$Pollinator_genus))




# Plant species #

levels(factor(excel_focal$Plant_genus))

#combine Plant_genus and Plant_species columns
excel_focal $ Plant_sp <- paste (excel_focal$Plant_genus, excel_focal$Plant_species, sep = " ")

# Plant species in focal #
levels(factor(excel_focal$Plant_sp))

#select plant species in focal present in fruit set
levels(factor(Fruit_set$Plant_sp))

t <- Fruit_set$Plant_sp
excel_focal = excel_focal %>% filter(Plant_sp %in% t)

levels(factor(excel_focal$Plant_sp)) #same plants in focal and fruit set


# Pollinators ##
## pollinator frequency (Total) per species plants, site and year
Pol_frequency<-aggregate(Frequency ~ Year+Plant_sp+Site_ID, data = excel_focal, FUN = sum)

#filtered rows from Pol_frequency based on the presence of matches in  Fruit_set
pol_frequency2<-merge(x=Pol_frequency,y=Fruit_set,by=c("Site_ID", "Plant_sp", "Year"),all.x=F, all.y=F)
levels(factor(pol_frequency2$Plant_sp))


# Hymenoptera frequency
Hym<-aggregate(Frequency ~ Year+Plant_sp+Site_ID, excel_focal[excel_focal$Orden %in% c("Hymenoptera"),], sum)
names(Hym)[names(Hym) == "Frequency"] <- "Hym_freq"

pol_frq<-merge(x=Hym,y=pol_frequency2,by=c("Site_ID", "Plant_sp", "Year"),all.x=F, all.y=T)


# Diptera frequency
dip<-aggregate(Frequency ~ Year+Plant_sp+Site_ID, excel_focal[excel_focal$Orden %in% c("Diptera"),], sum)
names(dip)[names(dip) == "Frequency"] <- "Dip_freq"

pol_frq2<-merge(x=dip,y=pol_frq,by=c("Site_ID", "Plant_sp", "Year"),all.x=F, all.y=T)


# Coleoptera frequency
coleop<-aggregate(Frequency ~ Year+Plant_sp+Site_ID, excel_focal[excel_focal$Orden %in% c("Coleoptera"),], sum)
names(coleop)[names(coleop) == "Frequency"] <- "Coleop_freq"

pol_frq3<-merge(x=coleop,y=pol_frq2,by=c("Site_ID", "Plant_sp", "Year"),all.x=F, all.y=T)


# Lepidoptera frequency
lepi<-aggregate(Frequency ~ Year+Plant_sp+Site_ID, excel_focal[excel_focal$Orden %in% c("Lepidoptera"),], sum)
names(lepi)[names(lepi) == "Frequency"] <- "Lepi_freq"

pol_frq4<-merge(x=lepi,y=pol_frq3,by=c("Site_ID", "Plant_sp", "Year"),all.x=F, all.y=T)



# Export dataframe to csv and excell 
write.csv(pol_frq4, "Frequency-Fruit.csv") 
write_xlsx(pol_frq4, "C:/Rfuncion/Frequency-Fruit.xlsx")
