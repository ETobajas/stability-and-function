
getwd()
setwd("C:/Users/estef/git/stability-and-function")
list.files()

library(readxl)
library(haven)
library(dplyr)
#install.packages("reshape2")
library(reshape2)
library(tidyr)
#install.packages("writexl")
library(writexl)
#install.packages("BiodiversityR")
#library (BiodiversityR)




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

# remove "Cotito de Santa Teresa", not data in 2016
# remove "Las mulas", not data in 2018
excel_fruit <- excel_fruit[!(excel_fruit$Site_ID == "Cotito de Santa Teresa"),]
excel_fruit <- excel_fruit[!(excel_fruit$Site_ID == "Las mulas"),]


# 14 site and 4 years
levels(factor(excel_fruit$Site_ID))
levels(factor(excel_fruit$Year)) 


# remove "Lavandula NA"
excel_fruit <- excel_fruit[!(excel_fruit$Plant_sp == "Lavandula NA"),]

# Remove Astragalus lusitanicus in fruit set (no present in focal)
excel_fruit <- excel_fruit[!(excel_fruit$Plant_sp == "Astragalus lusitanicus"),]

levels(factor(excel_fruit$Plant_sp))

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
# Replace NA in "Fruit_Yes" and "Fruit_No" columns
excel_fruit <- mutate_at(excel_fruit, c("Fruit_Yes", "Fruit_No"), ~replace(., is.na(.), 0))

# Cuando Fruit preyed es mayor que fruit yes, incluir el valor de Fruit preyed en fruit yes
excel_fruit <- mutate_at(excel_fruit, c("Fruit_preyed"), ~replace(., is.na(.), 0))
index <- excel_fruit$Fruit_Yes<excel_fruit$Fruit_preyed

excel_fruit$Fruit_Yes[index] <- (excel_fruit$Fruit_preyed[index] ) 




# sum fruit yes and fruit no per individual plant 
#(2015 and 2016 fruit is counted by branch, 2017-2018 no)
#thus each individual plant in 2015-2016 has a number of fruit yes and fruit no (same that 2017-2018)
Fruit1<-aggregate(Fruit_Yes ~ Year+Site_ID+Plant_sp+plant_id, data = excel_fruit, FUN = sum)
Fruit2<-aggregate(Fruit_No ~ Year+Site_ID+Plant_sp+plant_id, data = excel_fruit, FUN = sum)
# sumar fruit preyed por individuo y especie, año y sitio
Fruit3<-aggregate(Fruit_preyed ~ Year+Site_ID+Plant_sp+plant_id, data = excel_fruit, FUN = sum)

#unir datos
Fruit<-merge(x=Fruit1,y=Fruit2,by=c("Year","Site_ID", "Plant_sp","plant_id" ),all.x=T, all.y=T)
Fruit<-merge(x=Fruit,y=Fruit3,by=c("Year","Site_ID", "Plant_sp","plant_id" ),all.x=T, all.y=T)


# add column Fruit_Total (Fruit_Yes + Fruit_No)
Fruit$Fruit_Total = rowSums (Fruit[ , 5:6])

# remove fruit yes and fruit no = 0.
# se eliminan 5 observaciones (cuando no se tenia en cuenta fruit preyed se eliminaban 27)
Fruit<-Fruit[!apply(Fruit[,5:8] == 0, 1, all), ]


# Fruit proportion (Fruit yes / Fruit total)
# fruit proportion per individual plant and species plant in site and year
Fruit<-Fruit %>% mutate(Fruit_proportion = Fruit_Yes/Fruit_Total)


#order the rows
Fruit<-arrange(Fruit,Year,Site_ID, Plant_sp)
Fruit<-arrange(Fruit,Site_ID, Plant_sp)


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

# remove "Cotito de Santa Teresa", not data in 2016
# remove "Las mulas", not data in 2018
excel_seed <- excel_seed[!(excel_seed$Site_ID == "Cotito de Santa Teresa"),]
excel_seed <- excel_seed[!(excel_seed$Site_ID == "Las mulas"),]

# 14 site and 4 years
levels(factor(excel_seed$Site_ID))
levels(factor(excel_seed$Year))


# Plant species #

#combine genus and species columns
excel_seed $ Plant_sp <- paste (excel_seed$Plant_genus, excel_seed$Plant_species, sep = " ")

levels(factor(excel_seed$Plant_sp))

# Remove Astragalus lusitanicus in seed set (no present in focal)
excel_seed <- excel_seed[!(excel_seed$Plant_sp == "Astragalus lusitanicus"),]

#remove line 1014 value of the seed weight is 2145
excel_seed<-excel_seed[-1014,]

#Halium commutatum Bonares 2016-Plant_ID empty. 
#checking fruit there is only one individual of the species at that site in 2016- I assign A
excel_seed <- mutate_at(excel_seed,"Plant_ID", ~replace(., is.na(.), "A1"))


# seed weight mean
# obtener peso medio de la semilla para aquellos frutos en los que se ha medido mas de una semilla
seed<-aggregate(Seed_weight ~ Year+Site_ID+Plant_sp+Plant_ID+Fruit_number+Seeds, data = excel_seed, FUN = mean)

#no elimino los ceros porque puede haber dato de semilla en otro fruto del mismo individuo
# ejem. 2015-C.crispus-A1-a-0
#                     -A1-b-30

#Plant ID #

# new column plant_id (I have added a new column to standardize id plant)
# I use the same rule that in fruit

seed ['plant_id'] = ""
seed$plant_id = ifelse(seed$Plant_ID=="A1"|seed$Plant_ID== "A2"|seed$Plant_ID== "A3", "A", seed$plant_id)
seed$plant_id = ifelse(seed$Plant_ID=="B1" |seed$Plant_ID== "B2"|seed$Plant_ID== "B3", "B", seed$plant_id)
seed$plant_id = ifelse(seed$Plant_ID=="C1" |seed$Plant_ID== "C2"|seed$Plant_ID== "C3", "C", seed$plant_id)
seed$plant_id = ifelse(seed$Plant_ID=="D1" |seed$Plant_ID== "D2", "D", seed$plant_id)

# para 2017-2018
seed$plant_id = ifelse(seed$Plant_ID=="AF1" |seed$Plant_ID== "CC1"|seed$Plant_ID== "CL1"|
                         seed$Plant_ID=="CLI1"|seed$Plant_ID== "CM1"|seed$Plant_ID== "CS1"|
                         seed$Plant_ID=="EC1" |seed$Plant_ID== "HC1"|seed$Plant_ID== "HH1"|
                         seed$Plant_ID=="LP1" |seed$Plant_ID== "LS1"|seed$Plant_ID== "RO1"|
                         seed$Plant_ID=="TF1"|seed$Plant_ID== "HH1a"|seed$Plant_ID== "HH1b"|
                         seed$Plant_ID=="ROA","A", seed$plant_id)

seed$plant_id = ifelse(seed$Plant_ID=="AF2" |seed$Plant_ID== "CC2"|seed$Plant_ID== "CL2"|
                         seed$Plant_ID=="CLI2"|seed$Plant_ID== "CM2"|seed$Plant_ID== "CS2"|
                         seed$Plant_ID== "HC2"|seed$Plant_ID== "HH2"|seed$Plant_ID== "HH2a"|
                         seed$Plant_ID=="LP2" |seed$Plant_ID== "LS2"|seed$Plant_ID== "RO2"|
                         seed$Plant_ID=="TF2"|seed$Plant_ID== "ROB","B", seed$plant_id)


seed$plant_id = ifelse(seed$Plant_ID=="AF3" |seed$Plant_ID== "CC3"|seed$Plant_ID== "CL3"|
                         seed$Plant_ID=="CLI3"|seed$Plant_ID== "CM3"|seed$Plant_ID== "CS3"|
                         seed$Plant_ID== "HH3"|seed$Plant_ID== "HH3a"|seed$Plant_ID== "HH3b"|
                         seed$Plant_ID=="LP3" |seed$Plant_ID== "LS3"|seed$Plant_ID== "RO3"|
                         seed$Plant_ID=="TF3"|seed$Plant_ID== "HH3c"|seed$Plant_ID== "ROC","C", seed$plant_id)

seed$plant_id = ifelse(seed$Plant_ID=="AF4" |seed$Plant_ID== "CC4"|seed$Plant_ID== "CL4"|
                         seed$Plant_ID=="CLI4"|seed$Plant_ID== "CM4"|seed$Plant_ID== "CS4"|
                         seed$Plant_ID== "HH4a"|seed$Plant_ID== "HH4"| seed$Plant_ID=="LP4"|
                         seed$Plant_ID== "HH4b"|seed$Plant_ID=="TF4"|seed$Plant_ID=="HH4c","D", seed$plant_id)

levels(factor(seed$plant_id))


#seed number mean per individual and species plant in site and year
# seed number mean per fruit in plant individual per plant species
seed_numb<-aggregate(Seeds ~ Year+Site_ID+Plant_sp+plant_id, data = seed, FUN = mean)

#seed weight mean per individual and species plant in site and year
# seed weight mean per fruit in plant individual per plant species
seed_weight<-aggregate(Seed_weight ~ Year+Site_ID+Plant_sp+plant_id, data = seed, FUN = mean)


seed_set<-merge(x=seed_numb,y=seed_weight,by=c("Year","Site_ID", "Plant_sp","plant_id" ),all.x=T, all.y=T)

# join fruit and seed (hay observaciones que tienen fruto pero no semilla). Se eliminan aquellas que aunque tienen semilla no tienen fruto
reprod_success<-merge(x=Fruit,y=seed_set,by=c("Year","Site_ID", "Plant_sp","plant_id" ),all.x=T, all.y=F)
# join fruit and seed only plant id coinciding
reprod_success0<-merge(x=Fruit,y=seed_set,by=c("Year","Site_ID", "Plant_sp","plant_id" ),all.x=F, all.y=F)


#sum(is.na(reprod_success$Seeds))

# dejando solo lo que coincide: 442 observaciones 
# dejando todos los datos: 488 observaciones



#order the rows
reprod_success<-arrange(reprod_success,Year,Site_ID, Plant_sp)
reprod_success<-arrange(reprod_success,Site_ID, Plant_sp)

#order the rows
reprod_success0<-arrange(reprod_success0,Year,Site_ID, Plant_sp)
reprod_success0<-arrange(reprod_success0,Site_ID, Plant_sp)


##################

# frequency of species plant per site
#levels(factor(reprod_success$Plant_sp))

#plant_site<-data.frame(table( reprod_success$Site_ID, reprod_success$Plant_sp))
#plant_site

#filter(plant_site, Freq >0)

# plant species at least in 4 sites

#pl<-c("Asphodelus fistulosus","Cistus crispus","Cistus ladanifer","Cistus salviifolius",
#      "Halimium commutatum","Halimium halimifolium","Lavandula pedunculata","Lavandula stoechas",
#      "Rosmarinus officinalis")


#reprod_success1 = reprod_success %>% filter(Plant_sp %in% pl)


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

# remove "Cotito de Santa Teresa", not data (fruit, seed) in 2016
# remove "Las mulas", not data (fruit, seed) in 2018
excel_focal <- excel_focal[!(excel_focal$Site_ID == "Cotito de Santa Teresa"),]
excel_focal <- excel_focal[!(excel_focal$Site_ID == "Las mulas"),]

levels(factor(excel_focal$Site_ID))


# Orden # 

# Remove NA in Orden column (no pollinator)
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
Dipt= excel_focal %>% filter(Orden %in% "Diptera")
levels(factor(dipt$Pollinator_genus))


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

# remove Riphiphoridae - (parasite) and Anthrenus
excel_focal <- excel_focal[!(excel_focal$Pollinator_genus == "Riphiphoridae"),]
excel_focal <- excel_focal[!(excel_focal$Pollinator_genus == "Anthrenus"),]

levels(factor(excel_focal$Pollinator_genus))

# remove lines (in beefun have orden but not genus and species, R sets all columns with NA)
excel_focal<-excel_focal[c(-739,-740,-1285),]


# Plant species #

#combine Plant_genus and Plant_species columns
excel_focal $ Plant_sp <- paste (excel_focal$Plant_genus, excel_focal$Plant_species, sep = " ")

# Plant species in focal #
levels(factor(excel_focal$Plant_sp))

#select plant species in focal present in reproductive success (todas:18 especies)
levels(factor(reprod_success$Plant_sp)) 

plant<-c("Anchusa azurea","Asphodelus fistulosus","Cistus crispus","Cistus ladanifer","Cistus libanotis",
      "Cistus monspeliensis","Cistus salviifolius","Erica ciliaris","Halimium commutatum","Halimium halimifolium",
      "Lavandula pedunculata","Lavandula stoechas","Phlomis purpurea","Retama sphaerocarpa","Rosmarinus officinalis",
      "Spartium junceum","Teucrium fruticans","Ulex australis")

excel_focal = excel_focal %>% filter(Plant_sp %in% plant)


levels(factor(excel_focal$Plant_sp)) #same plants in focal and fruit/seed


# Remove NA in Plant indivual (plants outside the transect)
excel_focal<-excel_focal%>% drop_na (Plant_individual)


levels(factor(excel_focal$Plant_individual))

# one plant individual =NA (probably mistake)-> A
excel_focal$Plant_individual[excel_focal$Plant_individual=="NA"] <- "A"

# Asphodelus fistulosus=ABC -> AFC
excel_focal$Plant_individual[excel_focal$Plant_individual=="ABC"] <- "AFC"


# new column plant_id (I have added a new column to standardize id plant like A, B, C)

excel_focal ['plant_id'] = ""

# para 2015-2016
excel_focal$plant_id = ifelse(excel_focal$Plant_individual=="A", "A", excel_focal$plant_id)
excel_focal$plant_id = ifelse(excel_focal$Plant_individual=="B", "B", excel_focal$plant_id)
excel_focal$plant_id = ifelse(excel_focal$Plant_individual=="C", "C", excel_focal$plant_id)

# para 2017-2018
excel_focal$plant_id = ifelse(excel_focal$Plant_individual=="AFA"|excel_focal$Plant_individual== "CAA" |excel_focal$Plant_individual== "CCA"|excel_focal$Plant_individual== "CLA"|
                                excel_focal$Plant_individual=="CLIA"|excel_focal$Plant_individual=="CMA"|excel_focal$Plant_individual=="CSA"|excel_focal$Plant_individual== "HCA"|
                                excel_focal$Plant_individual== "HHA"|excel_focal$Plant_individual=="LPA" |excel_focal$Plant_individual== "LSA"|excel_focal$Plant_individual== "ROA"|
                                 excel_focal$Plant_individual== "ECA"|excel_focal$Plant_individual== "TFA"  ,"A", excel_focal$plant_id)

excel_focal$plant_id = ifelse(excel_focal$Plant_individual=="AFB"|excel_focal$Plant_individual== "CAB" |excel_focal$Plant_individual== "CCB"|excel_focal$Plant_individual== "CLB"|
                                excel_focal$Plant_individual=="CLIB"|excel_focal$Plant_individual=="CMB"| excel_focal$Plant_individual=="CSB"|excel_focal$Plant_individual== "HCB"|
                                excel_focal$Plant_individual== "HHB"|excel_focal$Plant_individual== "HC3"|excel_focal$Plant_individual=="LPB" |excel_focal$Plant_individual== "LSB"|
                                 excel_focal$Plant_individual== "ECB"|excel_focal$Plant_individual== "ROB"|excel_focal$Plant_individual== "TFB","B", excel_focal$plant_id)

excel_focal$plant_id = ifelse(excel_focal$Plant_individual=="AFC"|excel_focal$Plant_individual== "CCC"|excel_focal$Plant_individual== "CLC"|
                                excel_focal$Plant_individual=="CLIC"|excel_focal$Plant_individual=="CMC"|excel_focal$Plant_individual=="CSC"|excel_focal$Plant_individual== "HCC"|
                                excel_focal$Plant_individual== "HHC"|excel_focal$Plant_individual=="LPC" |excel_focal$Plant_individual== "LSC"|excel_focal$Plant_individual== "ROC"|
                                 excel_focal$Plant_individual== "ECC"|excel_focal$Plant_individual== "TFC" ,"C", excel_focal$plant_id)

excel_focal$plant_id = ifelse(excel_focal$Plant_individual=="AFD"|excel_focal$Plant_individual== "CCD"|excel_focal$Plant_individual== "CLD"|excel_focal$Plant_individual== "CLID"|
                                      excel_focal$Plant_individual=="CSD"|excel_focal$Plant_individual== "HCD"|excel_focal$Plant_individual== "HHD"|
                                      excel_focal$Plant_individual=="LPD" |excel_focal$Plant_individual== "LSD"|excel_focal$Plant_individual== "ROD"|excel_focal$Plant_individual== "TFD"
                              ,"D", excel_focal$plant_id)

excel_focal$plant_id = ifelse(excel_focal$Plant_individual=="AFE" |excel_focal$Plant_individual== "CCE"|
                                      excel_focal$Plant_individual=="CSE"|excel_focal$Plant_individual== "HCE"|
                                      excel_focal$Plant_individual=="LPE" |excel_focal$Plant_individual== "ROE"
                              ,"E", excel_focal$plant_id)



# Flower abundance
str(excel_focal)
colnames(excel_focal)[28] <- "flower_abundance"

# change flower_abundance as numeric
excel_focal$flower_abundance<-as.numeric(excel_focal$flower_abundance)

#sum(is.na(excel_focal$flower_abundance))
#excel_focal <- excel_focal[!is.na(excel_focal$flower_abundance),]

flower_abun<-aggregate(flower_abundance ~ Year+Site_ID+Plant_sp+plant_id, data = excel_focal, FUN = mean)


# pollinator frequency ##

## pollinator frequency (Total (sum)) per plant individual, species plants, site and year
Pol_frequency<-aggregate(Frequency ~ Year+Site_ID+Plant_sp+plant_id, data = excel_focal, FUN = sum)


#link flower_abun and Pol_frequency (solo plant id coincidentes)
visitation<-merge(x=flower_abun,y=Pol_frequency,by=c("Year","Site_ID", "Plant_sp","plant_id" ),all.x=F, all.y=F)

sum(is.na(visitation$flower_abundance))
sum(is.na(visitation$Frequency))

#######
#trial<-merge(x=reprod_success,y=visitation,by=c("Year","Site_ID", "Plant_sp","plant_id" ),all.x=T, all.y=F)
#sum(is.na(trial$Frequency))

#write_xlsx(visitation, "C:/Users/estef/git/stability-and-function/Data/visitation.xlsx")
#write_xlsx(reprod_success, "C:/Users/estef/git/stability-and-function/Data/plantas.xlsx")

####

# frequency/ flower abundance
visitation<-visitation %>% mutate(visitatio_rate = Frequency/flower_abundance)


#####
#link visitation and reproductive success1 (plants at least in 4 sites)
#to<-merge(x=visitation,y=reprod_success1,by=c("Year","Site_ID", "Plant_sp","plant_id" ),all.x=T, all.y=T)

# solo unido las observaciones que tienen polinizador y exito reproductor
#to_fa<-merge(x=visitation,y=reprod_success1,by=c("Year","Site_ID", "Plant_sp","plant_id" ),all.x=F, all.y=F)
###

# Replace NA in "Frequency","Fruit_proportion", "seed_number" and "Seed_weight columns
#to <- mutate_at(to, c("Frequency", "Fruit_proportion","seed_number", "Seed_weight"), ~replace(., is.na(.), 0))


#to<-arrange(to,Year,Site_ID, Plant_sp)
#to<-arrange(to,Site_ID, Plant_sp)
######

# Hymenoptera frequency
Hym<-aggregate(Frequency ~ Year+Site_ID+Plant_sp+plant_id, excel_focal[excel_focal$Orden %in% c("Hymenoptera"),], sum)
names(Hym)[names(Hym) == "Frequency"] <- "Hym_freq"

#link flower_abun and Hymenoptera
Hym_visitation<-merge(x=flower_abun,y=Hym,by=c("Year","Site_ID", "Plant_sp","plant_id" ),all.x=T, all.y=T)
Hym_visitation<-Hym_visitation %>% mutate(Hym_visit = Hym_freq/flower_abundance)

Hym_visitation <- select(Hym_visitation, -flower_abundance)

visitation<-merge(x=Hym_visitation,y=visitation,by=c("Year","Site_ID", "Plant_sp","plant_id"),all.x=F, all.y=T)

# Replace NA in "Hym_freq" column
#to <- mutate_at(to, c("Hym_freq"), ~replace(., is.na(.), 0))


# Diptera frequency
dip<-aggregate(Frequency ~ Year+Site_ID+Plant_sp+plant_id, excel_focal[excel_focal$Orden %in% c("Diptera"),], sum)
names(dip)[names(dip) == "Frequency"] <- "Dip_freq"

#link flower_abun and diptera
Dip_visitation<-merge(x=flower_abun,y=dip,by=c("Year","Site_ID", "Plant_sp","plant_id" ),all.x=T, all.y=T)
Dip_visitation<-Dip_visitation %>% mutate(Dip_visit = Dip_freq/flower_abundance)

Dip_visitation <- select(Dip_visitation, -flower_abundance)
visitation<-merge(x=Dip_visitation,y=visitation,by=c("Year","Site_ID", "Plant_sp","plant_id"),all.x=F, all.y=T)


# Coleoptera frequency
coleop<-aggregate(Frequency ~ Year+Site_ID+Plant_sp+plant_id, excel_focal[excel_focal$Orden %in% c("Coleoptera"),], sum)
names(coleop)[names(coleop) == "Frequency"] <- "Coleop_freq"

#link flower_abun and coleoptera
Cole_visitation<-merge(x=flower_abun,y=coleop,by=c("Year","Site_ID", "Plant_sp","plant_id" ),all.x=T, all.y=T)
Cole_visitation<-Cole_visitation %>% mutate(Coleop_visit = Coleop_freq/flower_abundance)

Cole_visitation <- select(Cole_visitation, -flower_abundance)
visitation<-merge(x=Cole_visitation,y=visitation,by=c("Year","Site_ID", "Plant_sp","plant_id"),all.x=F, all.y=T)

visitation<-arrange(visitation,Year,Site_ID, Plant_sp)
visitation<-arrange(visitation,Site_ID, Plant_sp)


visitation <- mutate_all(visitation, ~replace(., is.na(.), 0))


############
## species richness ##

#combine Pollinator_genus and Pollinator_species columns
excel_focal $ Pollinator_sp <- paste (excel_focal$Pollinator_genus, excel_focal$Pollinator_species, sep = " ")

# correct pollinator species (excel_focal)
levels(factor(excel_focal$Pollinator_sp))

excel_focal$Pollinator_sp[excel_focal$Pollinator_sp=="Andrena hispanica"] <- "Andrena hispania"
excel_focal$Pollinator_sp[excel_focal$Pollinator_sp=="Andrena nigroaenaea"] <- "Andrena nigroaenea"
excel_focal$Pollinator_sp[excel_focal$Pollinator_sp=="Andrena ovatula ?"] <- "Andrena ovatula"
excel_focal$Pollinator_sp[excel_focal$Pollinator_sp=="Andrena sp"] <- "Andrena sp."
excel_focal$Pollinator_sp[excel_focal$Pollinator_sp=="Anthaxia sp"] <- "Anthaxia sp."
excel_focal$Pollinator_sp[excel_focal$Pollinator_sp=="Anthophora aetivalis"] <- "Anthophora aestivalis"
excel_focal$Pollinator_sp[excel_focal$Pollinator_sp=="Anthophora sp"] <- "Anthophora sp."
excel_focal$Pollinator_sp[excel_focal$Pollinator_sp=="Anthrenus sp"] <- "Anthrenus sp."
excel_focal$Pollinator_sp[excel_focal$Pollinator_sp=="Apis melifera"] <- "Apis mellifera"
excel_focal$Pollinator_sp[excel_focal$Pollinator_sp=="Bombylius sp"] <- "Bombylius sp."
excel_focal$Pollinator_sp[excel_focal$Pollinator_sp=="Ceratina cuburbitina"] <- "Ceratina cucurbitina"
excel_focal$Pollinator_sp[excel_focal$Pollinator_sp=="Ceratina sp"] <- "Ceratina sp."
excel_focal$Pollinator_sp[excel_focal$Pollinator_sp=="Dasypoda crasiccornis"] <- "Dasypoda crassicornis"
excel_focal$Pollinator_sp[excel_focal$Pollinator_sp=="Dasypoda sp"] <- "Dasypoda sp."
excel_focal$Pollinator_sp[excel_focal$Pollinator_sp=="Eucera elongulata"] <- "Eucera elongatula"
excel_focal$Pollinator_sp[excel_focal$Pollinator_sp=="Eucera sp"] <- "Eucera sp."
excel_focal$Pollinator_sp[excel_focal$Pollinator_sp=="Heliotaurus roficolis"] <- "Heliotaurus ruficollis"
excel_focal$Pollinator_sp[excel_focal$Pollinator_sp=="Heliotaurus rufficolis"] <- "Heliotaurus ruficollis"
excel_focal$Pollinator_sp[excel_focal$Pollinator_sp=="Lasioglossum albocintum"] <- "Lasioglossum albocinctum"
excel_focal$Pollinator_sp[excel_focal$Pollinator_sp=="Lasioglossum sp"] <- "Lasioglossum sp."
excel_focal$Pollinator_sp[excel_focal$Pollinator_sp=="Malachius sp"] <- "Malachius sp."
excel_focal$Pollinator_sp[excel_focal$Pollinator_sp=="Merodon sp"] <- "Merodon sp."
excel_focal$Pollinator_sp[excel_focal$Pollinator_sp=="Parageron sp"] <- "Parageron sp."
excel_focal$Pollinator_sp[excel_focal$Pollinator_sp=="Psilothrix viridicoeruleus"] <- "Psilothrix viridicoerulea"
excel_focal$Pollinator_sp[excel_focal$Pollinator_sp=="Syrphidae NA"] <- "Syrphidae sp."
excel_focal$Pollinator_sp[excel_focal$Pollinator_sp=="Bombyliidae NA"] <- "Bombyliidae"
excel_focal$Pollinator_sp[excel_focal$Pollinator_sp=="Coleoptero NA"] <- "Coleoptero"
excel_focal$Pollinator_sp[excel_focal$Pollinator_sp=="Dasypoda NA"] <- "Dasypoda sp."


# calculating richness
focal<-data.frame(excel_focal[,c(1,5,29,30,31)])

focal1 <- dcast(focal, formula = Site_ID + Year +Plant_sp+plant_id ~ Pollinator_sp)

focal1$richness<-specnumber(focal1[ , 5:145])


r<-data.frame(focal1[,c(1,2,3,4,146)])

visitation<-merge(x=r,y=visitation,by=c("Year","Site_ID", "Plant_sp","plant_id"),all.x=F, all.y=T)



#order the rows
visitation<-arrange(visitation,Year,Site_ID, Plant_sp)
visitation<-arrange(visitation,Site_ID, Plant_sp)


################
### species richness contributing to 80% of total ###

plant_polli<-data.frame(excel_focal[,c(29,31)])

plant_polli1 <- dcast(plant_polli, formula = Pollinator_sp ~ Plant_sp)

# calculated the abundance like number of times that one pollinator species interaction 
# with any flower species (count the interaction frequency, not sum)
plant_polli1$abun<-specnumber(plant_polli1[ , 2:18])

# ordered according to abundance (normal over 1), in descending order
# Total abundance
sum(plant_polli1$abun) #382

plant_polli1$abun2<-plant_polli1$abun*(1/382)

plant_polli1 <- arrange(plant_polli1, -abun2)

# sum up to 0.8 (culumate sum)
plant_polli1$abu_acum<-cumsum(plant_polli1$abun2) 

# si tenemos en cuenta todos los individuos que tienen abundancia 2 
# acumulan el 84% 
# si queremos hasta el 80% algunos individuos con abundancia 2 quedan dentro y otros fuera
# ¿por que son mas importantes unos que otros?

# calculating species richness contributing to 80% of total
plant_polli1<-plant_polli1[-c(82:141),]

pol08 <- plant_polli1$Pollinator_sp

excel_focal1 = excel_focal %>% filter(Pollinator_sp %in% pol08)
levels(factor(excel_focal1$Pollinator_sp))


focal08<-data.frame(excel_focal1[,c(1,5,29,30,31)])

focal1_08 <- dcast(focal08, formula = Site_ID + Year +Plant_sp+plant_id ~ Pollinator_sp)

focal1_08$richness08<-specnumber(focal1_08[ , 5:85])

r08<-data.frame(focal1_08[,c(1,2,3,4,86)])

visitation<-merge(x=r08,y=visitation,by=c("Year","Site_ID", "Plant_sp","plant_id"),all.x=F, all.y=T)

#order the rows
visitation<-arrange(visitation,Year,Site_ID, Plant_sp)
visitation<-arrange(visitation,Site_ID, Plant_sp)

#to <- mutate_at(to,"richness08", ~replace(., is.na(.), 0))

# join visitation and reproductive success
#solo individuos coincidentes
total<-merge(x=reprod_success,y=visitation,by=c("Year","Site_ID", "Plant_sp","plant_id"),all.x=F, all.y=F)
# dejando todos los individuos 
total1<-merge(x=reprod_success,y=visitation,by=c("Year","Site_ID", "Plant_sp","plant_id"),all.x=T, all.y=T)

#order the rows
total<-arrange(total,Year,Site_ID, Plant_sp)
total<-arrange(total,Site_ID, Plant_sp)


write_xlsx(total, "C:/Users/estef/git/stability-and-function/Data/Pollinator-Plant-new.xlsx")
write.csv(total, "C:/Users/estef/git/stability-and-function/Data/Pollinator-Plant-new.csv")
