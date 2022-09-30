
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

fruit <- read.csv("Data_BeeFun/master_fruitset.csv")


levels(factor(fruit$Site_ID))

# modified  Site_ID
fruit$Site_ID[fruit$Site_ID=="Convento de la Luz"] <- "Convento_de_la_luz"
fruit$Site_ID[fruit$Site_ID=="Cotito de Santa Teresa"] <- "Cotito_de_santa_teresa"
fruit$Site_ID[fruit$Site_ID=="El pozo"] <- "El_pozo"
fruit$Site_ID[fruit$Site_ID=="La Rocina"] <- "La_rocina"
fruit$Site_ID[fruit$Site_ID=="Pinares de Hinojos"] <- "Pinares_de_hinojos"
fruit$Site_ID[fruit$Site_ID=="Pino del Cuervo"] <- "Pino_del_cuervo"
fruit$Site_ID[fruit$Site_ID=="Villamanrique este"] <- "Villamanrique_este"
fruit$Site_ID[fruit$Site_ID=="Villamanrique sur"] <- "Villamanrique_sur"
fruit$Site_ID[fruit$Site_ID=="La Cuña"] <- "La_cunya"

levels(factor(fruit$Site_ID))

d<-data.frame(table(fruit$Site_ID, fruit$Year))


# remove Sites
fruit <- fruit[!(fruit$Site_ID == "Cotito_de_santa_teresa"),] #no hay datos para 2016
fruit <- fruit[!(fruit$Site_ID == "Las_mulas"),]  #no hay datos para 2018, 2019
fruit <- fruit[!(fruit$Site_ID == "El_hongo"),]   # no hay datos para 2015, 2019
fruit <- fruit[!(fruit$Site_ID == "El_pinar"),]   # solo datos para 2015


# 13 site and 5 years
levels(factor(fruit$Site_ID))
levels(factor(fruit$Year)) 

# Plant species (19 species + Lavandula sp)
levels(factor(fruit$Plant_gen_sp))


# new column plant_id (I have added a new column to standardize id plant)

# 2015 and 2016: the same plant species has a Plant ID A1,A2,A3, B1, B2, etc.
# I suppose that all A are from the same individual A and all B are from 
# the same individual B (probably different branches).

# 2017 and 2018:the same plant species has a Plant ID with the initials of the species
# + 1, 2, 3 or 4 in some cases and there is no repetition per plot, 
# I suppose that the initial with 1 is an individual, the initial with 2 is another individual
# (the whole individual has been counted without separating branches), 
# I establish A for the individuals that have the number 1, B for those that have the number 2, etc.


fruit ['plant_id'] = ""

# para 2015-2016
fruit$plant_id = ifelse(fruit$Plant_ID=="A1"|fruit$Plant_ID== "A2"|fruit$Plant_ID== "A3", "A", fruit$plant_id)
fruit$plant_id = ifelse(fruit$Plant_ID=="B1" |fruit$Plant_ID== "B2"|fruit$Plant_ID== "B3", "B", fruit$plant_id)
fruit$plant_id = ifelse(fruit$Plant_ID=="C1" |fruit$Plant_ID== "C2"|fruit$Plant_ID== "C3", "C", fruit$plant_id)
fruit$plant_id = ifelse(fruit$Plant_ID=="D1" |fruit$Plant_ID== "D2"|fruit$Plant_ID== "D3", "D", fruit$plant_id)

# para 2017-2018
fruit$plant_id = ifelse(fruit$Plant_ID=="AF1" |fruit$Plant_ID== "CC1"|fruit$Plant_ID== "CL1"|fruit$Plant_ID== "EB1"|
                                fruit$Plant_ID=="CLI1"|fruit$Plant_ID== "CM1"|fruit$Plant_ID== "CS1"|
                                fruit$Plant_ID=="EC1" |fruit$Plant_ID== "HC1"|fruit$Plant_ID== "HH1"|
                                fruit$Plant_ID=="LP1" |fruit$Plant_ID== "LS1"|fruit$Plant_ID== "RO1"|
                                fruit$Plant_ID=="TF1"|fruit$Plant_ID== "CLIA","A", fruit$plant_id)

fruit$plant_id = ifelse(fruit$Plant_ID=="AF2" |fruit$Plant_ID== "CC2"|fruit$Plant_ID== "CL2"|fruit$Plant_ID== "EB2"|
                                fruit$Plant_ID=="CLI2"|fruit$Plant_ID== "CM2"|fruit$Plant_ID== "CS2"|
                                fruit$Plant_ID=="EC2" |fruit$Plant_ID== "HC2"|fruit$Plant_ID== "HH2"|
                                fruit$Plant_ID=="LP2" |fruit$Plant_ID== "LS2"|fruit$Plant_ID== "RO2"|
                                fruit$Plant_ID=="TF2"|fruit$Plant_ID== "CLIB","B", fruit$plant_id)


fruit$plant_id = ifelse(fruit$Plant_ID=="AF3" |fruit$Plant_ID== "CC3"|fruit$Plant_ID== "CL3"|
                                fruit$Plant_ID=="CLI3"|fruit$Plant_ID== "CM3"|fruit$Plant_ID== "CS3"|
                                fruit$Plant_ID=="EC3" |fruit$Plant_ID== "HC3"|fruit$Plant_ID== "HH3"|
                                fruit$Plant_ID=="LP3" |fruit$Plant_ID== "LS3"|fruit$Plant_ID== "RO3"|
                                fruit$Plant_ID=="TF3","C", fruit$plant_id)

fruit$plant_id = ifelse(fruit$Plant_ID=="AF4" |fruit$Plant_ID== "CC4"|fruit$Plant_ID== "CL4"|
                                fruit$Plant_ID=="CLI4"|fruit$Plant_ID== "CM4"|fruit$Plant_ID== "CS4"|
                                fruit$Plant_ID== "HC4"|fruit$Plant_ID== "HH4"| fruit$Plant_ID=="LP4"|
                                fruit$Plant_ID== "LS4"|fruit$Plant_ID=="TF4","D", fruit$plant_id)



levels(factor(fruit$plant_id))



# sum fruit yes,fruit no, fruit preyed and fruit dispersed per individual plant 
#(2015 and 2016 fruit is counted by branch, 2017-2018 no)

fruit_yes<-aggregate(Fruit_Yes ~ Year+Site_ID+Plant_gen_sp+plant_id, data = fruit,FUN = sum)
fruit_no<-aggregate(Fruit_No ~ Year+Site_ID+Plant_gen_sp+plant_id, data = fruit, FUN = sum)
fruit_preyed<-aggregate(Fruit_preyed ~ Year+Site_ID+Plant_gen_sp+plant_id, data = fruit, FUN = sum)
fruit_dispersed<-aggregate(Fruit_dispersed ~ Year+Site_ID+Plant_gen_sp+plant_id, data = fruit, FUN = sum)


#unir datos
fruit_set<-merge(x=fruit_yes,y=fruit_no,by=c("Year","Site_ID", "Plant_gen_sp","plant_id" ),all.x=T, all.y=T)
fruit_set<-merge(x=fruit_set,y=fruit_preyed,by=c("Year","Site_ID", "Plant_gen_sp","plant_id" ),all.x=T, all.y=T)
fruit_set<-merge(x=fruit_set,y=fruit_dispersed,by=c("Year","Site_ID", "Plant_gen_sp","plant_id" ),all.x=T, all.y=T)

# fruit formada= Fruit yes + fruit preyed + fruit dispersed
fruit_set$fruit_formado = rowSums (fruit_set[ , c(5,7,8)])

# add column Fruit_Total (Fruit_formado + Fruit_No)
fruit_set$fruit_total = rowSums (fruit_set[ , c(6,9)])


# Fruit proportion (Fruit formados / Fruit total)
# fruit proportion per individual plant and species plant in site and year
fruit_set<-fruit_set %>% mutate(fruit_proportion = fruit_formado/fruit_total)


# Remove 4 observation all variables = 0
fruit_set<-fruit_set[!apply(fruit_set[,5:10] == 0, 1, all), ]

#order the rows
fruit_set<-arrange(fruit_set,Year,Site_ID, Plant_gen_sp)
fruit_set<-arrange(fruit_set,Site_ID, Plant_gen_sp)


##########################

### Seed set ###

seed <- read.csv("Data_BeeFun/master_seedset.csv")


# Site #

levels(factor(seed$Site_ID))

# modified  Site_ID
seed$Site_ID[seed$Site_ID=="Convento de la Luz"] <- "Convento_de_la_luz"
seed$Site_ID[seed$Site_ID=="Cotito de Santa Teresa"] <- "Cotito_de_santa_teresa"
seed$Site_ID[seed$Site_ID=="El Pozo"] <- "El_pozo"
seed$Site_ID[seed$Site_ID=="La Rocina"] <- "La_rocina"
seed$Site_ID[seed$Site_ID=="Pinares de Hinojos"] <- "Pinares_de_hinojos"
seed$Site_ID[seed$Site_ID=="Pino del Cuervo"] <- "Pino_del_cuervo"
seed$Site_ID[seed$Site_ID=="Villamanrique este"] <- "Villamanrique_este"
seed$Site_ID[seed$Site_ID=="Villamanrique sur"] <- "Villamanrique_sur"
seed$Site_ID[seed$Site_ID=="La Cuña"] <- "La_cunya"

levels(factor(seed$Site_ID))

d<-data.frame(table(seed$Site_ID, seed$Year))

# remove Sites
seed <- seed[!(seed$Site_ID == "Cotito_de_santa_teresa"),] #no hay datos para 2016
seed <- seed[!(seed$Site_ID == "Las_mulas"),]  #no hay datos para 2018, 2019
seed <- seed[!(seed$Site_ID == "El_hongo"),]   # no hay datos para 2015, 2019
seed <- seed[!(seed$Site_ID == "El_pinar"),]   # solo datos para 2015


# 13 site and 5 years
levels(factor(seed$Site_ID))
levels(factor(seed$Year))


# Plant species #

levels(factor(seed$Plant_gen_sp))


#Halium calycinum Bonares 2016-Plant_ID empty. 
#checking fruit there is only one individual of the species at that site in 2016- I assign A
seed <- mutate_at(seed,"Plant_ID", ~replace(., is.na(.), "A1"))


# seed weight mean
# obtener peso medio de la semilla para aquellos frutos en los que se ha medido mas de una semilla
seed1<-aggregate(Seed_weight ~ Year+Site_ID+Plant_gen_sp+Plant_ID+Fruit_number+Seeds, data = seed, FUN = mean)

#no elimino los ceros porque puede haber dato de semilla en otro fruto del mismo individuo
# ejem. 2015-C.crispus-A1-a-0
#                     -A1-b-30

#Plant ID #

# new column plant_id (I have added a new column to standardize id plant)
# I use the same rule that in fruit

seed1 ['plant_id'] = ""
seed1$plant_id = ifelse(seed1$Plant_ID=="A1"|seed1$Plant_ID== "A2"|seed1$Plant_ID== "A3", "A", seed1$plant_id)
seed1$plant_id = ifelse(seed1$Plant_ID=="B1" |seed1$Plant_ID== "B2"|seed1$Plant_ID== "B3", "B", seed1$plant_id)
seed1$plant_id = ifelse(seed1$Plant_ID=="C1" |seed1$Plant_ID== "C2"|seed1$Plant_ID== "C3", "C", seed1$plant_id)
seed1$plant_id = ifelse(seed1$Plant_ID=="D1" |seed1$Plant_ID== "D2", "D", seed1$plant_id)

# para 2017-2018
seed1$plant_id = ifelse(seed1$Plant_ID=="AF1" |seed1$Plant_ID== "CC1"|seed1$Plant_ID== "CL1"|
                         seed1$Plant_ID=="CLI1"|seed1$Plant_ID== "CM1"|seed1$Plant_ID== "CS1"|seed1$Plant_ID== "EB1"|
                         seed1$Plant_ID=="EC1" |seed1$Plant_ID== "HC1"|seed1$Plant_ID== "HH1"|
                         seed1$Plant_ID=="LP1" |seed1$Plant_ID== "LS1"|seed1$Plant_ID== "RO1"|
                         seed1$Plant_ID=="TF1"|seed1$Plant_ID== "HH1a"|seed1$Plant_ID== "HH1b"|
                         seed1$Plant_ID=="ROA","A", seed1$plant_id)

seed1$plant_id = ifelse(seed1$Plant_ID=="AF2" |seed1$Plant_ID== "CC2"|seed1$Plant_ID== "CL2"|
                         seed1$Plant_ID=="CLI2"|seed1$Plant_ID== "CM2"|seed1$Plant_ID== "CS2"|seed1$Plant_ID== "EB2"|
                         seed1$Plant_ID== "HC2"|seed1$Plant_ID== "HH2"|seed1$Plant_ID== "HH2a"|
                         seed1$Plant_ID=="LP2" |seed1$Plant_ID== "LS2"|seed1$Plant_ID== "RO2"|
                         seed1$Plant_ID=="TF2"|seed1$Plant_ID== "ROB","B", seed1$plant_id)


seed1$plant_id = ifelse(seed1$Plant_ID=="AF3" |seed1$Plant_ID== "CC3"|seed1$Plant_ID== "CL3"|
                         seed1$Plant_ID=="CLI3"|seed1$Plant_ID== "CM3"|seed1$Plant_ID== "CS3"|seed1$Plant_ID== "HC3"|
                         seed1$Plant_ID== "HH3"|seed1$Plant_ID== "HH3a"|seed1$Plant_ID== "HH3b"|
                         seed1$Plant_ID=="LP3" |seed1$Plant_ID== "LS3"|seed1$Plant_ID== "RO3"|
                         seed1$Plant_ID=="TF3"|seed1$Plant_ID== "HH3c"|seed1$Plant_ID== "ROC","C", seed1$plant_id)

seed1$plant_id = ifelse(seed1$Plant_ID=="AF4" |seed1$Plant_ID== "CC4"|seed1$Plant_ID== "CL4"|
                         seed1$Plant_ID=="CLI4"|seed1$Plant_ID== "CM4"|seed1$Plant_ID== "CS4"|
                         seed1$Plant_ID== "HH4a"|seed1$Plant_ID== "HH4"| seed1$Plant_ID=="LP4"|
                         seed1$Plant_ID== "HH4b"|seed1$Plant_ID=="TF4"|seed1$Plant_ID=="HH4c","D", seed1$plant_id)

levels(factor(seed1$plant_id))


#seed number mean per individual and species plant in site and year
# seed number mean per fruit in plant individual per plant species
seed_numb<-aggregate(Seeds ~ Year+Site_ID+Plant_gen_sp+plant_id, data = seed1, FUN = mean)

#seed weight mean per individual and species plant in site and year
# seed weight mean per fruit in plant individual per plant species
seed_weight<-aggregate(Seed_weight ~ Year+Site_ID+Plant_gen_sp+plant_id, data = seed1, FUN = mean)


seed_set<-merge(x=seed_numb,y=seed_weight,by=c("Year","Site_ID", "Plant_gen_sp","plant_id" ),all.x=T, all.y=T)


# join fruit and seed only plant id coinciding
reprod_success<-merge(x=fruit_set,y=seed_set,by=c("Year","Site_ID", "Plant_gen_sp","plant_id" ),all.x=F, all.y=F)

# join fruit and seed (hay observaciones que tienen fruto pero no semilla). Se eliminan aquellas que aunque tienen semilla no tienen fruto
reprod_success0<-merge(x=fruit_set,y=seed_set,by=c("Year","Site_ID", "Plant_gen_sp","plant_id" ),all.x=T, all.y=F)


# individuos que tienen datos de fruit y seed = 573 observaciones 
# individuos con datos de fruto pero no de semilla = 634 observaciones

sum(is.na(reprod_success0$Seeds)) # 67 observaciones sin seed
#algunas de estas observaciones son 0 porque el fruit total es fruit no (21 observ)
# en otros casos hay proporcion de fruit pero no dato de seed

#order the rows
reprod_success<-arrange(reprod_success,Year,Site_ID, Plant_gen_sp)
reprod_success<-arrange(reprod_success,Site_ID, Plant_gen_sp)

#order the rows
reprod_success0<-arrange(reprod_success0,Year,Site_ID, Plant_gen_sp)
reprod_success0<-arrange(reprod_success0,Site_ID, Plant_gen_sp)


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

focal <- read.csv("Data_BeeFun/master_focals.csv")

# remove years (2021,2020)
levels(factor(focal$Year))

focal <- focal[!(focal$Year == "2021"),]
focal <- focal[!(focal$Year == "2020"),]

levels(factor(focal$Year))

# Sites #
levels(factor(focal$Site_ID))

# remove Sites (the same that fruit and seed)
focal <- focal[!(focal$Site_ID == "Cotito_de_santa_teresa"),] 
focal <- focal[!(focal$Site_ID == "Las_mulas"),]  
focal <- focal[!(focal$Site_ID == "El_hongo"),]   
focal <- focal[!(focal$Site_ID == "El_pinar"),]   


levels(factor(focal$Site_ID))

# remove observation out of focal
focal<-focal[focal$Out != "out", ] 

# Orden #
levels(factor(focal$Orden))
sum(is.na(focal$Orden)) #(hay 1400 observ con NA en orden)

#añadir informacion en columna orden cuando es NA (hay sp de polinizador y frecuencia)
focal$Orden = ifelse(focal$Pollinator_genus == "Xylocopa", "Hymenoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Vespula", "Hymenoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Vanessa", "Lepidoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Synema", "Araneae", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Psilothrix", "Coleoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Tropinota", "Coleoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Sphaerophoria", "Diptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Rhodanthidium", "Hymenoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Usia", "Diptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Thomisus", "Araneae", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Systoechus", "Diptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Rhyncomyia", "Diptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Protaetia", "Coleoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Oxythyrea", "Coleoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Nomada", "Hymenoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Osmia", "Hymenoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Polistes", "Hymenoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Sphecodes", "Hymenoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Scaeva", "Diptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Pieris", "Lepidoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Parageron", "Diptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Apis", "Hymenoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Lasioglossum", "Hymenoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Andrena", "Hymenoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Anthophora", "Hymenoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Bombus", "Hymenoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Halictus", "Hymenoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Flavipanurgus", "Hymenoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Ceratina", "Hymenoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Dasypoda", "Hymenoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Eucera", "Hymenoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Heliotaurus", "Coleoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Episyrphus", "Diptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Chalicodoma", "Hymenoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Anthidiellum", "Hymenoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Chasmatopterus", "Coleoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Chrysolina", "Coleoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Empis", "Diptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Myathropa", "Diptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Messor", "Hymenoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Megachile", "Hymenoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Macroglossum", "Lepidoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Hoplitis", "Hymenoptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Helophilus", "Diptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Eristalis", "Diptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Eristalinus", "Diptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Dischistus", "Diptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Dilophus", "Diptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Chrysotoxum", "Diptera", focal$Orden)
focal$Orden = ifelse(focal$Pollinator_genus == "Chrysis", "Hymenoptera", focal$Orden)


# Remove Arachnida, Aranea,Lepidoptera
levels(factor(focal$Orden))
focal<-subset(focal, Orden != 'Arachnida' | is.na(Orden))
focal<-subset(focal, Orden != 'Araneae' | is.na(Orden))
focal<-subset(focal, Orden != 'Lepidoptera' | is.na(Orden))


levels(factor(focal$Orden))


# Hymenoptera
# remove wasp, kleptoparasite, ants (hymenoptera: bees and megascolia) 
hym= focal %>% filter(Orden %in% "Hymenoptera")
levels(factor(hym$Pollinator_genus))

hyme<-c("Bembix","Chrysididae","Chrysis","Formiciidae","Megalodontes","Pemphredon","Philantus",
     "Polistes","Sphecodes","Tachysphex","Thyreus","Vespula")


focal = focal %>% filter(!Pollinator_genus %in% hyme)


#Diptera
Dipt= focal %>% filter(Orden %in% "Diptera")
levels(factor(Dipt$Pollinator_genus))

# remove all that is not Bombilidos, sirfidos and rhyncomyia (Diptera)
dip<-c("Bibio","Bibionidae", "Dasypogon","Dilophus","Empis","Lucilia","Sarcophaga",
     "Stomorhina","Stratyiomis")

focal = focal %>% filter(!Pollinator_genus %in% dip)


#Coleoptera
cole= focal %>% filter(Orden %in% "Coleoptera")
levels(factor(cole$Pollinator_genus))

# remove Anthrenus
focal<-subset(focal, Pollinator_genus != 'Anthrenus' | is.na(Orden))

sum(is.na(focal$Orden)) # 702 obser con pollinator sp NANA


# Plant species #

# Plant species in focal #
levels(factor(focal$Plant_gen_sp))

#select plant species in focal present in reproductive success (todas:18 especies)
# reproductive success with data of fruit and seed
levels(factor(reprod_success$Plant_gen_sp)) 

plant<-c("Anchusa azurea","Asphodelus fistulosus","Cistus crispus","Cistus ladanifer","Cistus libanotis",
      "Cistus monspeliensis","Cistus salviifolius","Erophaca baetica","Halimium calycinum",
      "Halimium halimifolium","Lavandula pedunculata","Lavandula stoechas","Phlomis purpurea","Retama sphaerocarpa",
      "Salvia rosmarinus","Spartium junceum","Teucrium fruticans","Ulex australis")

focal = focal %>% filter(Plant_gen_sp %in% plant)


levels(factor(focal$Plant_gen_sp)) # Retama sphaerocarpa no observations in focal

#Plant individual
levels(factor(focal$Plant_individual))

# Asphodelus fistulosus=ABC -> AFC
focal$Plant_individual[focal$Plant_individual=="ABC"] <- "AFC"


# new column plant_id (I have added a new column to standardize id plant like A, B, C)

focal ['plant_id'] = ""

# para 2015-2016
focal$plant_id = ifelse(focal$Plant_individual=="A", "A", focal$plant_id)
focal$plant_id = ifelse(focal$Plant_individual=="B", "B", focal$plant_id)
focal$plant_id = ifelse(focal$Plant_individual=="C", "C", focal$plant_id)
focal$plant_id = ifelse(focal$Plant_individual=="D", "D", focal$plant_id)
focal$plant_id = ifelse(focal$Plant_individual=="E", "E", focal$plant_id)

# para 2017-2018
focal$plant_id = ifelse(focal$Plant_individual=="AFA"|focal$Plant_individual== "CCA"|focal$Plant_individual== "CLA"|
                           focal$Plant_individual=="CLIA"|focal$Plant_individual=="CMA"|focal$Plant_individual=="CSA"|
                           focal$Plant_individual== "ECA"|focal$Plant_individual== "ELA"|focal$Plant_individual== "HCA"|
                           focal$Plant_individual== "HHA"|focal$Plant_individual=="LPA" |focal$Plant_individual== "LSA"|
                           focal$Plant_individual== "ROA"|focal$Plant_individual== "TFA"  ,"A", focal$plant_id)

focal$plant_id = ifelse(focal$Plant_individual=="AFB"|focal$Plant_individual== "CCB"|focal$Plant_individual== "CLB"|
                           focal$Plant_individual=="CLIB"|focal$Plant_individual=="CMB"| focal$Plant_individual=="CSB"|
                           focal$Plant_individual== "ECB"|focal$Plant_individual== "ELB"|focal$Plant_individual== "HCB"|
                           focal$Plant_individual== "HHB"|focal$Plant_individual== "HC3"|focal$Plant_individual=="LPB" |
                           focal$Plant_individual== "LSB"|focal$Plant_individual== "ROB"|focal$Plant_individual== "TFB","B", focal$plant_id)

focal$plant_id = ifelse(focal$Plant_individual=="AFC"|focal$Plant_individual== "CCC"|focal$Plant_individual== "CLC"|
                           focal$Plant_individual=="CLIC"|focal$Plant_individual=="CMC"|focal$Plant_individual=="CSC"|
                           focal$Plant_individual== "ECC"|focal$Plant_individual== "HCC"|focal$Plant_individual== "HHC"|
                           focal$Plant_individual=="LPC" |focal$Plant_individual== "LSC"|focal$Plant_individual== "ROC"|
                           focal$Plant_individual== "TFC" ,"C", focal$plant_id)


focal$plant_id = ifelse(focal$Plant_individual=="AFD"|focal$Plant_individual== "CCD"|focal$Plant_individual== "CLD"|
                           focal$Plant_individual== "CLID"|focal$Plant_individual=="CSD"|focal$Plant_individual== "HCD"|
                           focal$Plant_individual== "ELD"|focal$Plant_individual== "HHD"|focal$Plant_individual=="LPD" |
                           focal$Plant_individual== "ROD"|focal$Plant_individual== "TFD","D", focal$plant_id)

focal$plant_id = ifelse(focal$Plant_individual=="AFE" |focal$Plant_individual== "CCE"|
                           focal$Plant_individual=="CSE"|focal$Plant_individual== "ELE"|focal$Plant_individual== "HCE"|
                           focal$Plant_individual=="LPE" |focal$Plant_individual== "ROE"
                              ,"E", focal$plant_id)


levels(factor(focal$plant_id))

write.csv(focal, "C:/Users/estef/git/stability-and-function/Data/focal.csv")


# Flower abundance
#calcular media de flores por individuo y sp
flower_abun<-aggregate(Flower_abundance ~ Year+Site_ID+Plant_gen_sp+plant_id, data = focal, FUN = mean)


# pollinator frequency ##
## pollinator frequency (Total (sum)) per plant individual, species plants, site and year
Pol_frequency<-aggregate(Frequency ~ Year+Site_ID+Plant_gen_sp+plant_id, data = focal, FUN = sum)


#link flower_abun and Pol_frequency (dejando individuos que tienen flores pero no visitas)
visitation<-merge(x=flower_abun,y=Pol_frequency,by=c("Year","Site_ID", "Plant_gen_sp","plant_id" ),all.x=T, all.y=F)

sum(is.na(visitation$Frequency)) # 45 observ (plant id) no tienen visitas polinizador


visitation<-visitation%>%
   filter(!Flower_abundance==0) # eliminar 4 observaciones con flower abundance=0

# frequency/ flower abundance
visitation<-visitation %>% mutate(visitatio_rate = Frequency/Flower_abundance)


# Apis mellifera frequency (solo A.mellifera)
Apis<-aggregate(Frequency ~ Year+Site_ID+Plant_gen_sp+plant_id, focal[focal$Pollinator_gen_sp %in% c("Apis mellifera"),], sum)
names(Apis)[names(Apis) == "Frequency"] <- "Apis_freq"

# A. mellifera visitation rate
visitation<-merge(x=visitation,y=Apis,by=c("Year","Site_ID", "Plant_gen_sp","plant_id" ),all.x=T, all.y=F)
visitation<-visitation %>% mutate(Apis_visit_rate = Apis_freq/Flower_abundance)


# pollinator sin apis mellifera

# pollinator frequency (sin A. mellifera)
no_apis<-aggregate(Frequency ~ Year+Site_ID+Plant_gen_sp+plant_id, focal[!focal$Pollinator_gen_sp %in% c("Apis mellifera"),], sum)
names(no_apis)[names(no_apis) == "Frequency"] <- "no_apis_freq"

# visitation rate of pollinator (sin A. mellifera)
visitation<-merge(x=visitation,y=no_apis,by=c("Year","Site_ID", "Plant_gen_sp","plant_id" ),all.x=T, all.y=F)
visitation<-visitation %>% mutate(No_Apis_visit_rate = no_apis_freq/Flower_abundance)


# Hymenoptera frequency
Hym<-aggregate(Frequency ~ Year+Site_ID+Plant_gen_sp+plant_id, focal[focal$Orden %in% c("Hymenoptera"),], sum)
names(Hym)[names(Hym) == "Frequency"] <- "Hym_freq"

# hymenoptera visitation
visitation<-merge(x=visitation,y=Hym,by=c("Year","Site_ID", "Plant_gen_sp","plant_id" ),all.x=T, all.y=F)
visitation<-visitation %>% mutate(Hym_visit = Hym_freq/Flower_abundance)


# hymenoptera sin A. mellifera




# Diptera frequency
dip<-aggregate(Frequency ~ Year+Site_ID+Plant_gen_sp+plant_id, focal[focal$Orden %in% c("Diptera"),], sum)
names(dip)[names(dip) == "Frequency"] <- "Dip_freq"

#diptera visitation
visitation<-merge(x=visitation,y=dip,by=c("Year","Site_ID", "Plant_gen_sp","plant_id" ),all.x=T, all.y=F)
visitation<-visitation %>% mutate(dip_visit = Dip_freq/Flower_abundance)


# Coleoptera frequency
coleop<-aggregate(Frequency ~ Year+Site_ID+Plant_gen_sp+plant_id,focal[focal$Orden %in% c("Coleoptera"),], sum)
names(coleop)[names(coleop) == "Frequency"] <- "Coleop_freq"

#link flower_abun and coleoptera
visitation<-merge(x=visitation,y=coleop,by=c("Year","Site_ID", "Plant_gen_sp","plant_id" ),all.x=T, all.y=F)
visitation<-visitation %>% mutate(colep_visit = Coleop_freq/Flower_abundance)



visitation<-arrange(visitation,Year,Site_ID, Plant_gen_sp)
visitation<-arrange(visitation,Site_ID, Plant_gen_sp)


#visitation <- mutate_all(visitation, ~replace(., is.na(.), 0))


############
## species richness ##

# calculating richness
focal_0 <- focal[!(focal$Pollinator_gen_sp == "NA NA"),] #eliminar polinizadores NA NA

focal_1<-data.frame(focal_0[,c(1,2,6,29,30)])

focal1 <- dcast(focal_1, formula = Site_ID + Year +Plant_gen_sp+plant_id ~ Pollinator_gen_sp)

focal1$richness<-specnumber(focal1[ , 5:143])


r<-data.frame(focal1[,c(1,2,3,4,144)])

visitation<-merge(x=r,y=visitation,by=c("Year","Site_ID", "Plant_gen_sp","plant_id"),all.x=F, all.y=T)



#order the rows
visitation<-arrange(visitation,Year,Site_ID, Plant_gen_sp)
visitation<-arrange(visitation,Site_ID, Plant_gen_sp)


################
### species richness contributing to 90% of total ###

focal_0 <- focal[!(focal$Pollinator_gen_sp == "NA NA"),] #eliminar polinizadores NA NA

plant_polli<-data.frame(focal_0[,c(1,29)])


# suma de numero de interaciones que ha tenido una especie de planta y polinizador (NO la frecuencia)
plant_polli1 <- dcast(plant_polli, formula = Pollinator_gen_sp ~ Plant_gen_sp)

# calculated the abundance like number of times that one pollinator species interaction 
# with any flower species (count the interaction frequency, not sum)
plant_polli1$abun<-specnumber(plant_polli1[ , 2:18])

# ordered according to abundance (normal over 1), in descending order
# Total abundance
sum(plant_polli1$abun) #451

plant_polli1$abun2<-plant_polli1$abun*(1/451)

plant_polli1 <- arrange(plant_polli1, -abun2)

# sum up to 0.9 (culumate sum)
plant_polli1$abu_acum<-cumsum(plant_polli1$abun2) 

# si tenemos en cuenta todos los individuos que tienen abundancia 2 
# acumulan el 90% 
# si queremos hasta el 80% algunos individuos con abundancia 2 quedan dentro y otros fuera
# ¿por que son mas importantes unos que otros?

# calculating species richness contributing to 90% of total
plant_polli1<-plant_polli1[-c(95:139),]

pol09 <- plant_polli1$Pollinator_gen_sp

excel_focal1 = focal %>% filter(Pollinator_gen_sp %in% pol09)
levels(factor(excel_focal1$Pollinator_gen_sp))


focal09<-data.frame(excel_focal1[,c(1,2,6,29,30)])

focal1_09 <- dcast(focal09, formula = Site_ID + Year +Plant_gen_sp+plant_id ~ Pollinator_gen_sp)

focal1_09$richness09<-specnumber(focal1_09[ , 5:98])

r09<-data.frame(focal1_09[,c(1,2,3,4,99)])

visitation<-merge(x=r09,y=visitation,by=c("Year","Site_ID", "Plant_gen_sp","plant_id"),all.x=F, all.y=T)


#order the rows
visitation<-arrange(visitation,Year,Site_ID, Plant_gen_sp)
visitation<-arrange(visitation,Site_ID, Plant_gen_sp)

#to <- mutate_at(to,"richness08", ~replace(., is.na(.), 0))

# join visitation and reproductive success
#solo individuos coincidentes
total<-merge(x=reprod_success,y=visitation,by=c("Year","Site_ID", "Plant_gen_sp","plant_id"),all.x=F, all.y=F)
# eliminando cuando no hay fruto pero si polinizador 
total1<-merge(x=reprod_success,y=visitation,by=c("Year","Site_ID", "Plant_gen_sp","plant_id"),all.x=T, all.y=F)

#order the rows
total<-arrange(total,Year,Site_ID, Plant_gen_sp)
total<-arrange(total,Site_ID, Plant_gen_sp)


write_xlsx(total, "C:/Users/estef/git/stability-and-function/Data/Pollinator_Plant.xlsx")
write.csv(total, "C:/Users/estef/git/stability-and-function/Data/Pollinator_Plant.csv")
write.csv(total1, "C:/Users/estef/git/stability-and-function/Data/Pollinator_Plant_fruityes.csv")


