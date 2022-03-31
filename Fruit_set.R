
getwd()
setwd("C:/Users/estef/git/stability-and-function")
list.files()

library(readxl)
library(haven)
library(dplyr)
library(reshape2)
library(tidyr)
library(writexl)
library (BiodiversityR)



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

# remove when fruit yes=0 and fruit no=0 (Total is 0)
excel_fruit<-excel_fruit[!apply(excel_fruit[,9:10] == 0, 1, all), ]

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
# assign value of seed column when seed number column is "all"
excel_seed<-excel_seed %>% mutate(seed_number = ifelse(seed_number =="all", Seeds, seed_number))
excel_seed<-excel_seed %>% mutate(seed_number = ifelse(seed_number =="All", Seeds, seed_number))

# change seed number as numeric
excel_seed$seed_number<-as.numeric(excel_seed$seed_number)

str(excel_seed)

# NA = not fruit or fruit preyed (remove)
excel_seed<-excel_seed%>% drop_na (seed_number | Seed_weight)


# Seed weight #

#remove line 1133 value of the seed weight is 2145
excel_seed<-excel_seed[-1133,]

# seed weight= 0 -> NOtes:preyed (remove)
excel_seed<-excel_seed[!apply(excel_seed[,10] == 0, 1, all), ]


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


# calculated the seed number(mean) per year, site, plant species and plant individual
# mean seed number per fruit in plant individual per plant species
seed_num<-aggregate(seed_number ~ Year+Site_ID+Plant_sp+plant_id, data = excel_seed, FUN = mean)

# calculated the seed weight(mean) per year, site, plant species and plant individual
# mean seed weight per fruit in plant individual per plant species
seed_we<-aggregate(Seed_weight ~ Year+Site_ID+Plant_sp+plant_id, data = excel_seed, FUN = mean)

seed_set<-merge(x = seed_num, y = seed_we)

#order the rows
seed_set<-arrange(seed_set,Year,Site_ID, Plant_sp)
seed_set<-arrange(seed_set,Site_ID, Plant_sp)

# joined fruit set and seed set
reprod_success<-merge(x=Fruit_set,y=seed_set,by=c("Year","Site_ID", "Plant_sp","plant_id" ),all.x=T, all.y=F)

reprod_success <- mutate_at(reprod_success,"seed_number", ~replace(., is.na(.), 0))
reprod_success <- mutate_at(reprod_success,"Seed_weight", ~replace(., is.na(.), 0))

#order the rows
reprod_success<-arrange(reprod_success,Year,Site_ID, Plant_sp)
reprod_success<-arrange(reprod_success,Site_ID, Plant_sp)


##################

# frequency of species plant per site
levels(factor(reprod_success$Plant_sp))

plant_site<-data.frame(table( reprod_success$Site_ID, reprod_success$Plant_sp))
plant_site

filter(plant_site, Freq >0)

# plant species at least in 4 sites

pl<-c("Asphodelus fistulosus","Cistus crispus","Cistus ladanifer","Cistus salviifolius",
      "Halimium commutatum","Halimium halimifolium","Lavandula pedunculata","Lavandula stoechas",
      "Rosmarinus officinalis")


reprod_success1 = reprod_success %>% filter(Plant_sp %in% pl)


# remove 0 in fruit, seed number and seed weight
reprod_success1<-reprod_success1[!apply(reprod_success1[,5:7] == 0, 1, all), ]


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

# remove Riphiphoridae - (parasite)
excel_focal <- excel_focal[!(excel_focal$Pollinator_genus == "Riphiphoridae"),]


levels(factor(excel_focal$Pollinator_genus))

# remove lines (in beefun have orden but not genus and species, R sets all columns with NA)
excel_focal<-excel_focal[c(-695,-824,-825,-1111,-1479),]


# Plant species #

#combine Plant_genus and Plant_species columns
excel_focal $ Plant_sp <- paste (excel_focal$Plant_genus, excel_focal$Plant_species, sep = " ")

# Plant species in focal #
levels(factor(excel_focal$Plant_sp))

#select plant species in focal present in reproductive success1
excel_focal = excel_focal %>% filter(Plant_sp %in% pl)

levels(factor(excel_focal$Plant_sp)) #same plants in focal and fruit set


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
excel_focal$plant_id = ifelse(excel_focal$Plant_individual=="AFA" |excel_focal$Plant_individual== "CCA"|excel_focal$Plant_individual== "CLA"|
                                      excel_focal$Plant_individual=="CSA"|excel_focal$Plant_individual== "HCA"|excel_focal$Plant_individual== "HHA"|
                                      excel_focal$Plant_individual=="LPA" |excel_focal$Plant_individual== "LSA"|excel_focal$Plant_individual== "ROA"
                                     ,"A", excel_focal$plant_id)

excel_focal$plant_id = ifelse(excel_focal$Plant_individual=="AFB" |excel_focal$Plant_individual== "CCB"|excel_focal$Plant_individual== "CLB"|
                                      excel_focal$Plant_individual=="CSB"|excel_focal$Plant_individual== "HCB"|excel_focal$Plant_individual== "HHB"|excel_focal$Plant_individual== "HC3"|
                                      excel_focal$Plant_individual=="LPB" |excel_focal$Plant_individual== "LSB"|excel_focal$Plant_individual== "ROB"
                              ,"B", excel_focal$plant_id)

excel_focal$plant_id = ifelse(excel_focal$Plant_individual=="AFC" |excel_focal$Plant_individual== "CCC"|excel_focal$Plant_individual== "CLC"|
                                      excel_focal$Plant_individual=="CSC"|excel_focal$Plant_individual== "HCC"|excel_focal$Plant_individual== "HHC"|
                                      excel_focal$Plant_individual=="LPC" |excel_focal$Plant_individual== "LSC"|excel_focal$Plant_individual== "ROC"
                              ,"C", excel_focal$plant_id)

excel_focal$plant_id = ifelse(excel_focal$Plant_individual=="AFD" |excel_focal$Plant_individual== "CCD"|excel_focal$Plant_individual== "CLD"|
                                      excel_focal$Plant_individual=="CSD"|excel_focal$Plant_individual== "HCD"|excel_focal$Plant_individual== "HHD"|
                                      excel_focal$Plant_individual=="LPD" |excel_focal$Plant_individual== "LSD"|excel_focal$Plant_individual== "ROD"
                              ,"D", excel_focal$plant_id)

excel_focal$plant_id = ifelse(excel_focal$Plant_individual=="AFE" |excel_focal$Plant_individual== "CCE"|
                                      excel_focal$Plant_individual=="CSE"|excel_focal$Plant_individual== "HCE"|
                                      excel_focal$Plant_individual=="LPE" |excel_focal$Plant_individual== "ROE"
                              ,"E", excel_focal$plant_id)



# Pollinators ##

# remove NA in frequency
excel_focal<-excel_focal%>% drop_na (Frequency)

## pollinator frequency (Total (sum)) per plant individual, species plants, site and year
Pol_frequency<-aggregate(Frequency ~ Year+Site_ID+Plant_sp+plant_id, data = excel_focal, FUN = sum)

#order the rows
Pol_frequency<-arrange(Pol_frequency,Year,Site_ID, Plant_sp)
Pol_frequency<-arrange(Pol_frequency,Site_ID, Plant_sp)


#link Pol_frequency and reproductive success1 (plants at least in 4 sites)
to<-merge(x=Pol_frequency,y=reprod_success1,by=c("Year","Site_ID", "Plant_sp","plant_id" ),all.x=F, all.y=F)

to<-arrange(to,Year,Site_ID, Plant_sp)
to<-arrange(to,Site_ID, Plant_sp)


# Hymenoptera frequency
Hym<-aggregate(Frequency ~ Year+Site_ID+Plant_sp+plant_id, excel_focal[excel_focal$Orden %in% c("Hymenoptera"),], sum)
names(Hym)[names(Hym) == "Frequency"] <- "Hym_freq"

to<-merge(x=Hym,y=to,by=c("Year","Site_ID", "Plant_sp","plant_id"),all.x=F, all.y=T)


# Diptera frequency
dip<-aggregate(Frequency ~ Year+Site_ID+Plant_sp+plant_id, excel_focal[excel_focal$Orden %in% c("Diptera"),], sum)
names(dip)[names(dip) == "Frequency"] <- "Dip_freq"

to<-merge(x=dip,y=to,by=c("Year","Site_ID", "Plant_sp","plant_id"),all.x=F, all.y=T)


# Coleoptera frequency
coleop<-aggregate(Frequency ~ Year+Site_ID+Plant_sp+plant_id, excel_focal[excel_focal$Orden %in% c("Coleoptera"),], sum)
names(coleop)[names(coleop) == "Frequency"] <- "Coleop_freq"

to<-merge(x=coleop,y=to,by=c("Year","Site_ID", "Plant_sp","plant_id"),all.x=F, all.y=T)

to<-arrange(to,Year,Site_ID, Plant_sp)
to<-arrange(to,Site_ID, Plant_sp)


to <- mutate_all(to, ~replace(., is.na(.), 0))


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

focal1$richness<-specnumber(focal1[ , 5:129])


r<-data.frame(focal1[,c(1,2,3,4,130)])

to<-merge(x=r,y=to,by=c("Year","Site_ID", "Plant_sp","plant_id"),all.x=F, all.y=T)

#order the rows
to<-arrange(to,Year,Site_ID, Plant_sp)
to<-arrange(to,Site_ID, Plant_sp)


################
### species richness contributing to 80% of total ###

plant_polli<-data.frame(excel_focal[,c(29,31)])

plant_polli1 <- dcast(plant_polli, formula = Pollinator_sp ~ Plant_sp)

# calculated the abundance like number of times that one pollinator species interaction 
# with any flower species (count the interaction frequency, not sum)
plant_polli1$abun<-specnumber(plant_polli1[ , 2:10])

# ordered according to abundance (normal over 1), in descending order
# Total abundance
sum(plant_polli1$abun) #304

plant_polli1$abun2<-plant_polli1$abun*(1/304)

plant_polli1 <- arrange(plant_polli1, -abun2)

# sum up to 0.8 (culumate sum)
plant_polli1$abu_acum<-cumsum(plant_polli1$abun2) # hasta fila 66 = 0.80592105

# calculating species richness contributing to 80% of total
plant_polli1<-plant_polli1[-c(67:125),]

pol08 <- plant_polli1$Pollinator_sp

excel_focal1 = excel_focal %>% filter(Pollinator_sp %in% pol08)
levels(factor(excel_focal1$Pollinator_sp))


focal08<-data.frame(excel_focal1[,c(1,5,29,30,31)])

focal1_08 <- dcast(focal08, formula = Site_ID + Year +Plant_sp+plant_id ~ Pollinator_sp)

focal1_08$richness08<-specnumber(focal1_08[ , 5:70])

r08<-data.frame(focal1_08[,c(1,2,3,4,71)])

to<-merge(x=r08,y=to,by=c("Year","Site_ID", "Plant_sp","plant_id"),all.x=F, all.y=T)

#order the rows
to<-arrange(to,Year,Site_ID, Plant_sp)
to<-arrange(to,Site_ID, Plant_sp)

to <- mutate_at(to,"richness08", ~replace(., is.na(.), 0))



write_xlsx(to, "C:/Users/estef/git/stability-and-function/Frequency-Fruit.xlsx")
write.csv(to, "C:/Users/estef/git/stability-and-function/Frequency-Fruit.csv")
