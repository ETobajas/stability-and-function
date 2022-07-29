library(tidyverse)
#install.packages("broom.mixed")
library(broom.mixed)
library(broom)
#install.packages("lmerTest")
library(lmerTest)
library(dplyr)
library(reshape2)


Pollinator_Plant<- read.csv("Data/Pollinator_Plant.csv") #400 observaciones
str(Pollinator_Plant)
Pollinator_Plant<-Pollinator_Plant[,-1]


#Species out
spp_out <- Pollinator_Plant %>%
  group_by(Plant_gen_sp) %>%
  summarise(Unique_Id = n_distinct(Site_ID)) %>%
  filter(Unique_Id == 1) %>%
  select(Plant_gen_sp) %>% pull(Plant_gen_sp)


focal<- read.csv("Data/focal.csv") 
focal<-focal[,-1]

#Data with species in more than 1 site
focal = focal %>% filter(!Plant_gen_sp %in% spp_out)

focal_0 <- focal[!(focal$Pollinator_gen_sp == "NA NA"),] #eliminar polinizadores NA NA


levels(factor(focal_0$Plant_gen_sp))

focal_0<-subset(focal_0, Plant_gen_sp != 'Ulex australis') #remove ulex


plant_polli<-data.frame(focal_0[,c(1,2,6,29)])

plant_polli1 <- dcast(plant_polli, formula = Site_ID + Year +Plant_gen_sp ~ Pollinator_gen_sp)


#variance ratio
#(Var over the Sum of abundances of all species over time/ Sum of the variance of each species over time)

var_ratio <- function(x){
  out <- (var(rowSums(x))) / (sum(apply(x, MARGIN = 2, FUN = var))) 
  out                               
}

asy=plant_polli1%>% 
  group_by(Plant_gen_sp,Site_ID)%>% 
  do(var_ratio=var_ratio(.[4:139]))

# Loreau & Mazancourt syncrony index
#sum of the tempotral covariances of all species / sum of sqrt of the variances squared
syncLM <- function(x){
  out <- (sum(cov(x))) / (sum(sqrt(diag(cov(x))))^2)
  out
}

asy_2=plant_polli1%>% 
  group_by(Plant_gen_sp,Site_ID)%>% 
  do(syncLM=syncLM(.[4:139]))


#Gross et al.
av_sync <- function(x, w = TRUE){
  S <- ncol(x) #num sp
  total <- rowSums(x)
  p <- colSums(x)/sum(colSums(x)) #rel abund
  cors <- c()
  for(i in 1:ncol(x)) cors[i] <- cor(x[,i],total-x[,i])
  if(w){
    out <- sum(p * cors) #a--b, a-c, ...
  }else{
    out <- (1/S) * sum(cors) #a--b, a-c, ...
  }
  out
}

asy_3=plant_polli1%>% 
  group_by(Plant_gen_sp,Site_ID)%>% 
  do(av_sync=av_sync(.[4:139]))
