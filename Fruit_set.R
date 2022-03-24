
getwd()
setwd("C:/Rfuncion")
list.files()

library(readxl)
library(haven)
library(dplyr)
library(reshape2)
library(tidyr)
library(writexl)



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
plant_site<-data.frame(table( excel_fruit$Plant_sp,excel_fruit$Site_ID))
plant_site

# Replace NA in "Fruit_Yes" and "Fruit_No" columns
excel_fruit <- mutate_at(excel_fruit, c("Fruit_Yes", "Fruit_No"), ~replace(., is.na(.), 0))


# add column Fruit_Total (Fruit_Yes + Fruit_No)
excel_fruit$Fruit_Total = rowSums (excel_fruit[ , 9:10])

# Fruit proportion (Fruit yes / Fruit total)
excel_fruit<-excel_fruit %>% mutate(Fruit_proportion = Fruit_Yes/Fruit_Total)


# new column plant_id (I have added a new column to standardize id plant)

# 2015 and 2016: the same plant species has a Plant ID A1,A2,A3, B1, B2, etc.
# I suppose that all A are from the same individual A and all B are from 
# the same individual B (probably different branches).

# 2017 and 2018:the same plant species has a Plant ID with the initials of the species
# + 1, 2, 3 or 4 in some cases and there is no repetition per plot, 
# I suppose that the initial with 1 is an individual, the initial with 2 is another individual
# (the whole individual has been counted without separating branches), 
# I establish A for the individuals that have the number 1, B for those that have the number 2, etc.


excel_fruit ['plant_id'] = ""

# para 2015-2016
excel_fruit$plant_id = ifelse(excel_fruit$Plant_ID=="A1"|excel_fruit$Plant_ID== "A2"|excel_fruit$Plant_ID== "A3", "A", excel_fruit$plant_id)
excel_fruit$plant_id = ifelse(excel_fruit$Plant_ID=="B1" |excel_fruit$Plant_ID== "B2"|excel_fruit$Plant_ID== "B3", "B", excel_fruit$plant_id)
excel_fruit$plant_id = ifelse(excel_fruit$Plant_ID=="C1" |excel_fruit$Plant_ID== "C2"|excel_fruit$Plant_ID== "C3", "C", excel_fruit$plant_id)
excel_fruit$plant_id = ifelse(excel_fruit$Plant_ID=="D1" |excel_fruit$Plant_ID== "D2"|excel_fruit$Plant_ID== "D3", "D", excel_fruit$plant_id)

# para 2017-2018
excel_fruit$plant_id = ifelse(excel_fruit$Plant_ID=="AF1" |excel_fruit$Plant_ID== "CC1"|excel_fruit$Plant_ID== "CL1"|
                                excel_fruit$Plant_ID=="CLI1"|excel_fruit$Plant_ID== "CM1"|excel_fruit$Plant_ID== "CS1"|
                                excel_fruit$Plant_ID=="EC1" |excel_fruit$Plant_ID== "HC1"|excel_fruit$Plant_ID== "HH1"|
                                excel_fruit$Plant_ID=="LP1" |excel_fruit$Plant_ID== "LS1"|excel_fruit$Plant_ID== "RO1"|
                                excel_fruit$Plant_ID=="TF1"|excel_fruit$Plant_ID== "CLIA","A", excel_fruit$plant_id)

excel_fruit$plant_id = ifelse(excel_fruit$Plant_ID=="AF2" |excel_fruit$Plant_ID== "CC2"|excel_fruit$Plant_ID== "CL2"|
                                excel_fruit$Plant_ID=="CLI2"|excel_fruit$Plant_ID== "CM2"|excel_fruit$Plant_ID== "CS2"|
                                excel_fruit$Plant_ID=="EC2" |excel_fruit$Plant_ID== "HC2"|excel_fruit$Plant_ID== "HH2"|
                                excel_fruit$Plant_ID=="LP2" |excel_fruit$Plant_ID== "LS2"|excel_fruit$Plant_ID== "RO2"|
                                excel_fruit$Plant_ID=="TF2"|excel_fruit$Plant_ID== "CLIB","B", excel_fruit$plant_id)


excel_fruit$plant_id = ifelse(excel_fruit$Plant_ID=="AF3" |excel_fruit$Plant_ID== "CC3"|excel_fruit$Plant_ID== "CL3"|
                                excel_fruit$Plant_ID=="CLI3"|excel_fruit$Plant_ID== "CM3"|excel_fruit$Plant_ID== "CS3"|
                                excel_fruit$Plant_ID=="EC3" |excel_fruit$Plant_ID== "HC3"|excel_fruit$Plant_ID== "HH3"|
                                excel_fruit$Plant_ID=="LP3" |excel_fruit$Plant_ID== "LS3"|excel_fruit$Plant_ID== "RO3"|
                                excel_fruit$Plant_ID=="TF3","C", excel_fruit$plant_id)

excel_fruit$plant_id = ifelse(excel_fruit$Plant_ID=="AF4" |excel_fruit$Plant_ID== "CC4"|excel_fruit$Plant_ID== "CL4"|
                                excel_fruit$Plant_ID=="CLI4"|excel_fruit$Plant_ID== "CM4"|excel_fruit$Plant_ID== "CS4"|
                                excel_fruit$Plant_ID== "HC4"|excel_fruit$Plant_ID== "HH4"| excel_fruit$Plant_ID=="LP4"|
                                excel_fruit$Plant_ID== "LS4"|excel_fruit$Plant_ID=="TF4","D", excel_fruit$plant_id)


levels(factor(excel_fruit$plant_id))

# calculated the fruit proportion (mean) per year, site, plant species and plant individual
Fruit_set<-aggregate(Fruit_proportion ~ Year+Site_ID+Plant_sp+plant_id, data = excel_fruit, FUN = mean)

#order the rows
Fruit_set<-arrange(Fruit_set,Year,Site_ID, Plant_sp)
Fruit_set<-arrange(Fruit_set,Site_ID, Plant_sp)


# Export dataframe to csv and excell 
write.csv(Fruit_set, "my_fruitset.csv") 

write_xlsx(Fruit_set, "C:/Rfuncion/my_fruitset.xlsx")


##########################

### Seed set ###

excel_seed <- read_excel("BeeFunMaster.xlsx",sheet = "Seed Set")
str(excel_seed)

# Site #

levels(factor(excel_seed$Site_ID))

# remove "El hongo", not sampled in 2015
excel_seed <- excel_seed[!(excel_seed$Site_ID == "El hongo"),]
excel_seed <- excel_seed[!(excel_seed$Site_ID == "El Hongo"),]

# modified Site ID
excel_seed$Site_ID[excel_seed$Site_ID=="Urbanización"] <- "Urbanizaciones"
excel_seed$Site_ID[excel_seed$Site_ID=="El Pozo"] <- "El pozo"

# 16 site and 4 years
levels(factor(excel_seed$Site_ID))
levels(factor(excel_seed$Year))


# Plant species #

#combine genus and species columns
excel_seed $ Plant_sp <- paste (excel_seed$Plant_genus, excel_seed$Plant_species, sep = " ")

levels(factor(excel_seed$Plant_sp))

# Remove Astragalus lusitanicus in seed set (no present in focal)
excel_seed <- excel_seed[!(excel_seed$Plant_sp == "Astragalus lusitanicus"),]


# Seed number #

# now column seed number is chr no num
# change NA by 0
excel_seed$seed_number[excel_seed$seed_number=="NA"] <- 0

# assign value of seed column when seed number column is "all"
excel_seed<-excel_seed %>% mutate(seed_number = ifelse(seed_number =="all", Seeds, seed_number))

# change seed number as numeric
excel_seed$seed_number<-as.numeric(excel_seed$seed_number)

str(excel_seed)

# NA = 0 in seed number
excel_seed <- mutate_at(excel_seed,"seed_number", ~replace(., is.na(.), 0))


# Seed weight #

#remove line 1133 value of the seed weight is 2145
excel_seed<-excel_seed[-1133,]

# NA = 0 in seed weight
excel_seed <- mutate_at(excel_seed,"Seed_weight", ~replace(., is.na(.), 0))

#Plant ID #

levels(factor(excel_seed$Plant_ID))

# new column plant_id (I have added a new column to standardize id plant)
# I use the same rule that in fruit

excel_seed ['plant_id'] = ""

# para 2015-2016
excel_seed$plant_id = ifelse(excel_seed$Plant_ID=="A1"|excel_seed$Plant_ID== "A2"|excel_seed$Plant_ID== "A3", "A", excel_seed$plant_id)
excel_seed$plant_id = ifelse(excel_seed$Plant_ID=="B1" |excel_seed$Plant_ID== "B2"|excel_seed$Plant_ID== "B3", "B", excel_seed$plant_id)
excel_seed$plant_id = ifelse(excel_seed$Plant_ID=="C1" |excel_seed$Plant_ID== "C2"|excel_seed$Plant_ID== "C3", "C", excel_seed$plant_id)
excel_seed$plant_id = ifelse(excel_seed$Plant_ID=="D1" |excel_seed$Plant_ID== "D2", "D", excel_seed$plant_id)

# para 2017-2018
excel_seed$plant_id = ifelse(excel_seed$Plant_ID=="AF1" |excel_seed$Plant_ID== "CC1"|excel_seed$Plant_ID== "CL1"|
                                     excel_seed$Plant_ID=="CLI1"|excel_seed$Plant_ID== "CM1"|excel_seed$Plant_ID== "CS1"|
                                     excel_seed$Plant_ID=="EC1" |excel_seed$Plant_ID== "HC1"|excel_seed$Plant_ID== "HH1"|
                                     excel_seed$Plant_ID=="LP1" |excel_seed$Plant_ID== "LS1"|excel_seed$Plant_ID== "RO1"|
                                     excel_seed$Plant_ID=="TF1"|excel_seed$Plant_ID== "HH1a"|excel_seed$Plant_ID== "HH1b"|
                                     excel_seed$Plant_ID=="ROA","A", excel_seed$plant_id)

excel_seed$plant_id = ifelse(excel_seed$Plant_ID=="AF2" |excel_seed$Plant_ID== "CC2"|excel_seed$Plant_ID== "CL2"|
                                     excel_seed$Plant_ID=="CLI2"|excel_seed$Plant_ID== "CM2"|excel_seed$Plant_ID== "CS2"|
                                     excel_seed$Plant_ID== "HC2"|excel_seed$Plant_ID== "HH2"|excel_seed$Plant_ID== "HH2a"|
                                     excel_seed$Plant_ID=="LP2" |excel_seed$Plant_ID== "LS2"|excel_seed$Plant_ID== "RO2"|
                                     excel_seed$Plant_ID=="TF2"|excel_seed$Plant_ID== "ROB","B", excel_seed$plant_id)


excel_seed$plant_id = ifelse(excel_seed$Plant_ID=="AF3" |excel_seed$Plant_ID== "CC3"|excel_seed$Plant_ID== "CL3"|
                                     excel_seed$Plant_ID=="CLI3"|excel_seed$Plant_ID== "CM3"|excel_seed$Plant_ID== "CS3"|
                                     excel_seed$Plant_ID== "HH3"|excel_seed$Plant_ID== "HH3a"|excel_seed$Plant_ID== "HH3b"|
                                     excel_seed$Plant_ID=="LP3" |excel_seed$Plant_ID== "LS3"|excel_seed$Plant_ID== "RO3"|
                                     excel_seed$Plant_ID=="TF3"|excel_seed$Plant_ID== "HH3c"|excel_seed$Plant_ID== "ROC","C", excel_seed$plant_id)

excel_seed$plant_id = ifelse(excel_seed$Plant_ID=="AF4" |excel_seed$Plant_ID== "CC4"|excel_seed$Plant_ID== "CL4"|
                                     excel_seed$Plant_ID=="CLI4"|excel_seed$Plant_ID== "CM4"|excel_seed$Plant_ID== "CS4"|
                                     excel_seed$Plant_ID== "HH4a"|excel_seed$Plant_ID== "HH4"| excel_seed$Plant_ID=="LP4"|
                                     excel_seed$Plant_ID== "HH4b"|excel_seed$Plant_ID=="TF4"|excel_seed$Plant_ID=="HH4c","D", excel_seed$plant_id)

levels(factor(excel_seed$plant_id))

#Halium commutatum Bonares 2016-Plant_ID empty. 
#checking fruit there is only one individual of the species at that site in 2016- I assign A
excel_seed <- mutate_at(excel_seed,"plant_id", ~replace(., is.na(.), "A"))

#remove line 1296: not Plant_ID, neither fruit
excel_seed<-excel_seed[-1296,]


# calculated the seed number(mean) per year, site, plant species and plant individual
seed_num<-aggregate(seed_number ~ Year+Site_ID+Plant_sp+plant_id, data = excel_seed, FUN = mean)

# calculated the seed number(mean) per year, site, plant species and plant individual
seed_we<-aggregate(Seed_weight ~ Year+Site_ID+Plant_sp+plant_id, data = excel_seed, FUN = mean)

seed_set<-merge(x = seed_num, y = seed_we)

#order the rows
seed_set<-arrange(seed_set,Year,Site_ID, Plant_sp)
seed_set<-arrange(seed_set,Site_ID, Plant_sp)

# joined fruit set and seed set
reprod_success<-merge(x=Fruit_set,y=seed_set,by=c("Year","Site_ID", "Plant_sp","plant_id" ),all.x=T, all.y=F)

reprod_success <- mutate_at(reprod_succes,"seed_number", ~replace(., is.na(.), 0))
reprod_success <- mutate_at(reprod_succes,"Seed_weight", ~replace(., is.na(.), 0))

#order the rows
reprod_success<-arrange(reprod_succes,Year,Site_ID, Plant_sp)
reprod_success<-arrange(reprod_succes,Site_ID, Plant_sp)


write_xlsx(reprod_success, "C:/Rfuncion/reproductice_success.xlsx")


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
excel_focal$Pollinator_genus[excel_focal$Pollinator_genus=="Psilotrix"] <- "Psilothrix"
excel_focal$Pollinator_genus[excel_focal$Pollinator_genus=="Chasmaptoterus"] <- "Chasmatopterus"
excel_focal$Pollinator_genus[excel_focal$Pollinator_genus=="Chryptocephalus"] <- "Cryptocephalus"

# remove Riphiphoridae - (parasite)
excel_focal <- excel_focal[!(excel_focal$Pollinator_genus == "Riphiphoridae"),]


levels(factor(excel_focal$Pollinator_genus))

# remove lines (in beefun have orden but not genus and species, R sets all columns with NA)
excel_focal<-excel_focal[c(-695,-824,-825,-1111,-1479),]



#####
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
