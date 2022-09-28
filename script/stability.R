
library(tidyverse)
#install.packages("broom.mixed")
library(broom.mixed)
library(broom)
#install.packages("lmerTest")
library(lmerTest)
library(dplyr)
library(reshape2)
library(tidyr)
library(ggplot2)
#install.packages("ggeffects")
library(ggeffects)
library(patchwork)
library(ggcorrplot)
library(ggpubr)
#install.packages("qqplotr")
library(qqplotr)

Pollinator_Plant<- read.csv("Data/Pollinator_Plant.csv") 
str(Pollinator_Plant)
Pollinator_Plant<-Pollinator_Plant[,-1]


#Species out
spp_out <- Pollinator_Plant %>%
  group_by(Plant_gen_sp) %>%
  summarise(Unique_Id = n_distinct(Site_ID)) %>%
  filter(Unique_Id == 1) %>%
  select(Plant_gen_sp) %>% pull(Plant_gen_sp)


#Data with species in more than 1 site
Pollinator_Plant = Pollinator_Plant %>% filter(!Plant_gen_sp %in% spp_out)

# replace NAs by 0
Pollinator_Plant <- mutate_all(Pollinator_Plant, ~replace(., is.na(.), 0))


# datos sin plant_id (mean de las variables porque hay plantas con 2 indv, otras 3, etc)
#nos quedamos con sitio - año - plant species
pol_plant2=Pollinator_Plant %>%
  group_by(Site_ID,Year, Plant_gen_sp) %>%
  summarise_if(is.numeric, mean)

levels(factor(pol_plant2$Plant_gen_sp)) #12 plant species 


pol_plant2$Year <- factor(pol_plant2$Year)

# plot fruit proportion and year for each plant species and site 
plots3 <- pol_plant2 %>%
  group_by(Plant_gen_sp) %>%
  group_map(~ ggplot(.) + aes(x=Year, y=fruit_proportion, col =factor(Site_ID), shape=factor(Site_ID)) + 
              scale_shape_manual(values = c(15,16,17,18,6,7,8,9,10,11,12,13,14))+ geom_point(size = 3) + ggtitle(.y[[1]]))

plots3[[1]]+plots3[[2]]+plots3[[3]]+plots3[[4]]
plots3[[5]]+plots3[[6]]+plots3[[7]]+plots3[[8]] 
plots3[[9]]+plots3[[10]]+plots3[[11]]+plots3[[12]]


# plot seed number and year for each plant species and site 
plots3_seed <- pol_plant2 %>%
  group_by(Plant_gen_sp) %>%
  group_map(~ ggplot(.) + aes(x=Year, y=Seeds, col =factor(Site_ID), shape=factor(Site_ID)) + 
              scale_shape_manual(values = c(15,16,17,18,6,7,8,9,10,11,12,13,14))+ geom_point(size = 3) + ggtitle(.y[[1]]))

plots3_seed[[1]]+plots3_seed[[2]]+plots3_seed[[3]]+plots3_seed[[4]]
plots3_seed[[5]]+plots3_seed[[6]]+plots3_seed[[7]]+plots3_seed[[8]] 
plots3_seed[[9]]+plots3_seed[[10]]+plots3_seed[[11]]+plots3_seed[[12]]


# plot seed number and year for each plant species and site 
plots3_visitation_rate <- pol_plant2 %>%
  group_by(Plant_gen_sp) %>%
  group_map(~ ggplot(.) + aes(x=Year, y=visitatio_rate, col =factor(Site_ID), shape=factor(Site_ID)) + 
  scale_shape_manual(values = c(15,16,17,18,6,7,8,9,10,11,12,13,14))+ geom_point(size = 3) + ggtitle(.y[[1]]))

plots3_visitation_rate[[1]]+plots3_visitation_rate[[2]]+plots3_visitation_rate[[3]]+plots3_visitation_rate[[4]]
plots3_visitation_rate[[5]]+plots3_visitation_rate[[6]]+plots3_visitation_rate[[7]]+plots3_visitation_rate[[8]] 
plots3_visitation_rate[[9]]+plots3_visitation_rate[[10]]+plots3_visitation_rate[[11]]+plots3_visitation_rate[[12]]

## Stability ##

# fruit proportion stability for each plant species per site #
# con colum plant_id

#cv_1Fruit<-Pollinator_Plant %>%
#  group_by(Plant_gen_sp,Site_ID) %>%
#  summarise_at(vars(fruit_proportion),cv_1 <- function(x, ...) {
#    ( mean(x, ...)/sd(x, ...) )
#  })%>%
#  ungroup()


# datos por año y sitio para cada sp de planta (sin plant_id)
cv_1Fruit2<-pol_plant2 %>%
  group_by(Plant_gen_sp,Site_ID) %>%
  summarise_at(vars(fruit_proportion),cv_1 <- function(x, ...) {
    ( mean(x, ...)/sd(x, ...) )
  })%>%
  ungroup()


# seed number stability for each plant species per site #

#cv_1Seed<-Pollinator_Plant %>%
#  group_by(Plant_gen_sp,Site_ID) %>%
#  summarise_at(vars(Seeds),cv_1 <- function(x, ...) {
#    ( mean(x, ...)/sd(x, ...) )
#  })%>%
#  ungroup()

# datos por año y sitio para cada sp de planta (sin plant_id)
cv_1Seed2<-pol_plant2 %>%
  group_by(Plant_gen_sp,Site_ID) %>%
  summarise_at(vars(Seeds),cv_1 <- function(x, ...) {
    ( mean(x, ...)/sd(x, ...) )
  })%>%
  ungroup()

#visitation rate stability for each plant species per site (all pollinators) #

#cv_1pollinator<-Pollinator_Plant %>%
#  group_by(Plant_gen_sp,Site_ID) %>%
#  summarise_at(vars(visitatio_rate),cv_1 <- function(x, ...) {
#    ( mean(x, ...)/sd(x, ...) )
#  })%>%
#  ungroup()

# datos por año y sitio para cada sp de planta (sin plant_id)
cv_1pollinator2<-pol_plant2 %>%
  group_by(Plant_gen_sp,Site_ID) %>%
  summarise_at(vars(visitatio_rate),cv_1 <- function(x, ...) {
    ( mean(x, ...)/sd(x, ...) )
  })%>%
  ungroup()


## Richness ##

#  pollinator richness for each plant species per site (Total)
S<-aggregate(richness ~ Plant_gen_sp+Site_ID, data = Pollinator_Plant,FUN = sum)

#suma de la riqueza media por año para cada plant sp y sitio
S2<-aggregate(richness ~ Plant_gen_sp+Site_ID, data = pol_plant2,FUN = sum)



# join tables con nuevos calculos (sin plant_id)
full2=full_join(cv_1Fruit2, cv_1Seed2, by = c('Plant_gen_sp', 'Site_ID'))%>%
  full_join (., cv_1pollinator2, by=c('Plant_gen_sp', 'Site_ID')) %>%
  full_join (., S, by=c('Plant_gen_sp', 'Site_ID'))%>%
  full_join (., S2, by=c('Plant_gen_sp', 'Site_ID'))%>%
  rename(cv_1_fruit=fruit_proportion, cv_1_seed=Seeds, cv_1_visitation=visitatio_rate, S_total=richness.x, S_mean=richness.y)


# correlacion total

#levels(factor(full$Site_ID))
#tr<-full %>% select(cv_1_fruit, cv_1_seed,cv_1_visitation)

## Replace Inf and -Inf with NA
#tr[is.na(tr) | tr == "Inf"] <- 0 
#tr[is.na(tr) | tr == "-Inf"] <- 0 

#tr <- mutate_all(tr, ~replace(., is.na(.), 0))

#cor(tr)

# para la nueva tabla (full2= sin plant_id)

tr2<-full2 %>% select(cv_1_fruit, cv_1_seed,cv_1_visitation,S_total, S_mean)

## Replace Inf and -Inf with NA
tr2[is.na(tr2) | tr2 == "Inf"] <- 0 
tr2[is.na(tr2) | tr2 == "-Inf"] <- 0 

tr2 <- mutate_all(tr2, ~replace(., is.na(.), 0))

cor(tr2)



## Synchrony ###

focal<- read.csv("Data/focal.csv") 
focal<-focal[,-1]


focal_0 <- focal[!(focal$Pollinator_gen_sp == "NA NA"),] #eliminar polinizadores NA NA

#Data with species in more than 1 site
focal_0 = focal_0 %>% filter(!Plant_gen_sp %in% spp_out)


levels(factor(focal_0$Plant_gen_sp))
levels(factor(full$Plant_gen_sp))

focal_0<-subset(focal_0, Plant_gen_sp != 'Ulex australis') #remove ulex
plant_polli<-data.frame(focal_0[,c(1,2,6,29,30)])


df.result <- left_join(Pollinator_Plant, plant_polli, c('Plant_gen_sp', 'Site_ID','Year','plant_id'))

df.result1<-data.frame(df.result[,c(1,2,3,4,25)])
df.result2 <- dcast(df.result, formula = Site_ID + Year +plant_id+Plant_gen_sp ~ Pollinator_gen_sp)

# sin plant_id
prueba=df.result2 %>%
  group_by(Site_ID,Year, Plant_gen_sp) %>%
  summarise_if(is.numeric, sum) 


# log variance ratio (Lepš et al., 2018)
#log (Var over the Sum of abundances of all species over time/ Sum of the variance of each species over time)
# Positive values of log var ratio signify synchronization, negative values indicate compensatory dynamics
log_VR <- function(x){
  out <- log((var(rowSums(x))) / (sum(apply(x, MARGIN = 2, FUN = var)))) 
  out                               
}


# un dato por sp planta, sitio y año (sin plant_id)
syn_2=prueba%>% 
  group_by(Plant_gen_sp,Site_ID)%>% 
  do(log_VR=log_VR(.[4:127]))

# Loreau & Mazancourt syncrony index
#sum of the tempotral covariances of all species / sum of sqrt of the variances squared
syncLM <- function(x){
  out <- (sum(cov(x))) / (sum(sqrt(diag(cov(x))))^2)
  out
}


# un dato por sp planta, sitio y año (sin plant_id)
syn_LM2=prueba%>% 
  group_by(Plant_gen_sp,Site_ID)%>% 
  do(syncLM=syncLM(.[4:127]))



#Gross et al. 2014

### IMPORTANTE: se obtiene todo NA, consultar que puede estar pasando

#av_sync <- function(x, w = TRUE){
#  S <- ncol(x) #num sp
#  total <- rowSums(x)
#  p <- colSums(x)/sum(colSums(x)) #rel abund
#  cors <- c()
#  for(i in 1:ncol(x)) cors[i] <- cor(x[,i],total-x[,i])
# if(w){
#    out <- sum(p * cors) #a--b, a-c, ...
#  }else{
#    out <- (1/S) * sum(cors) #a--b, a-c, ...
#  }
#  out
#}

#syn_G2=prueba%>% 
#  group_by(Plant_gen_sp,Site_ID)%>% 
#  do(av_sync=av_sync(.[4:127]))



# join tables con nuevos calculos (sin plant_id)
full2=full_join(full2, syn_2, by = c('Plant_gen_sp', 'Site_ID'))%>%
  full_join (., syn_LM2, by=c('Plant_gen_sp', 'Site_ID'))

head(full2)
full2$log_VR <- unlist(full2$log_VR)
full2$syncLM <- unlist(full2$syncLM)

#write.csv(full2, "C:/Users/estef/git/stability-and-function/Data/Stability.csv")


#correlation por plant species

# correlacion con nuevos calculos (full2 = sin plant_id)
b<-full2 %>% select(Plant_gen_sp,cv_1_fruit, cv_1_seed,cv_1_visitation,S_total,S_mean,log_VR,syncLM)

## Replace Inf and -Inf with NA
b[is.na(b) | b == "Inf"] <- NA 
b[is.na(b) | b == "-Inf"] <- NA 

b <- mutate_all(b, ~replace(., is.na(.), 0))



df_with_plotsb <- b %>%
  group_by(Plant_gen_sp) %>%
  nest() %>%
  mutate(plot = map(data, function(.x) {
    .x  %>%
      cor() %>%
      ggcorrplot::ggcorrplot(show.diag = F, type="lower",lab=TRUE)
  }))

plots1 <- map2(df_with_plotsb$plot, df_with_plotsb$Plant_gen_sp, ~(.x + labs(title = .y)))




plots1[[1]] + plots1[[2]] + plots1[[3]] + plots1[[4]]+plots1[[5]]+plots1[[6]]+plots1[[7]]+
  plots1[[8]]+plots1[[9]]+plots1[[10]]+plots1[[11]]+plots1[[12]]

# tabla correlacion 
b_cor <- b %>%
  group_by(Plant_gen_sp) %>%
  do(cormat = cor(select(., -matches("Plant_gen_sp")))) 

b_cor[[2]]

list_1 <- b_cor[[2]]

b_cor[[1]]

v <- b_cor[[1]]
str(v)
names(list_1) <- v

str(list_1)
data2 <- do.call(rbind.data.frame, list_1)

corre<-data2 %>%
  mutate(newcolum = rownames(data2))%>%
  separate(newcolum, into=c('species', 'rows'), extra = "merge", sep = "\\.")%>%
  select(species, rows,everything())  %>%
  remove_rownames() %>%
  mutate(species = replace(species, duplicated(species), ""))



# 4 plant species are in 2-3 site (A.fistulosus, C.libanotis, c.monspeliensis, T.fructicans)

# eliminar plant species en new data
sp_out2=full2 %>%
  group_by(Plant_gen_sp) %>%
  summarise(Unique_Id = n_distinct(Site_ID)) %>%
  filter(Unique_Id <= 3) %>%
  select(Plant_gen_sp) %>% pull(Plant_gen_sp)

#Data with species in more than 3 sites
full_2 = full2 %>% filter(!Plant_gen_sp %in% sp_out2)

# Salvia rosmarinus solo un dato por año(estabilidad y sincronia =NA). Eliminar

full_2<-subset(full_2, Plant_gen_sp != 'Salvia rosmarinus')

## Replace Inf and -Inf with NA
full_2[is.na(full_2) | full_2 == "Inf"] <- NA 
full_2[is.na(full_2) | full_2 == "-Inf"] <- NA 


# Relationship between fruit proportion stability and visitation rate stability
# new data (full_2)

plots_sta_fruit <- full_2 %>%
  group_by(Plant_gen_sp) %>%
  group_map(~ ggplot(.) + aes(x=cv_1_visitation, y=cv_1_fruit) + 
              geom_point() + ggtitle(.y[[1]]))

plots_sta_fruit[[1]]+plots_sta_fruit[[2]]+plots_sta_fruit[[3]]+plots_sta_fruit[[4]]
plots_sta_fruit[[5]]+plots_sta_fruit[[6]]+plots_sta_fruit[[7]]

#model
sta_fruit<-full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_fruit~cv_1_visitation,data))) %>%
  summarize(tidy(mod))%>%
  ungroup()


#goodness of fit measures, p-values for hypothesis tests on residuals, or model convergence information.
sta_fruit_glance<-full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_fruit~cv_1_visitation,data))) %>%
  summarize(glance(mod))%>%
  ungroup()

#checking residuals
sta_fruit_resid<-full_2 %>%
  filter(!Plant_gen_sp=="Halimium calycinum") %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lm(cv_1_fruit~cv_1_visitation,data)))%>%
  mutate(plots = list(performance::check_model(model))) %>%
  ungroup()

plot(sta_fruit_resid$plots[[1]]) # Cistus crispus
plot(sta_fruit_resid$plots[[2]]) # Cistus ladanifer
plot(sta_fruit_resid$plots[[3]]) # Cistus salviifolius
plot(sta_fruit_resid$plots[[4]]) # Halimium halimifolium
plot(sta_fruit_resid$plots[[5]]) # Lavandula pedunculata
plot(sta_fruit_resid$plots[[6]]) # Lavandula stoechas


# model plot (fruit proportion stability and visitation rate stability)
sta_fruit.1 <- full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_fruit~cv_1_visitation,data))) %>%
  mutate(mod1 = list(ggeffects::ggpredict(mod, terms = "cv_1_visitation"))) %>%
  mutate(plots = list(ggplot(mod1, aes(x, predicted)) + geom_line() +
                        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)))



sta_fruit.1$plots[[1]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  cv_1_visitation, y = cv_1_fruit))+ 
  labs(x = "visitation rate stability",y="fruit proportion stability",subtitle = "C.crispus",cex.sub=0.2)+
  theme_classic()

sta_fruit.1$plots[[2]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  cv_1_visitation, y = cv_1_fruit))+ 
  labs(x = "visitation rate stability",y="fruit proportion stability",subtitle = "C.ladanifer")+  theme_classic()

sta_fruit.1$plots[[3]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  cv_1_visitation, y = cv_1_fruit))+ 
  labs(x = "visitation rate stability",y="fruit proportion stability",subtitle = "C.salviifolius")+  theme_classic()

## H. calycinum solo un dato para cv_1_fruit
sta_fruit.1$plots[[4]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  cv_1_visitation, y = cv_1_fruit))+ 
  labs(x = "visitation rate stability",y="fruit proportion stability",subtitle = "H.calycinum")+  theme_classic()

sta_fruit.1$plots[[5]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x =  cv_1_visitation, y = cv_1_fruit))+ 
  labs(x = "visitation rate stability",y="fruit proportion stability",subtitle = "H.halimifolium")+  theme_classic()

sta_fruit.1$plots[[6]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  cv_1_visitation, y = cv_1_fruit))+ 
  labs(x = "visitation rate stability",y="fruit proportion stability",subtitle = "L.pedunculata")+  theme_classic()

sta_fruit.1$plots[[7]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Lavandula stoechas"), aes(x =  cv_1_visitation, y = cv_1_fruit))+ 
  labs(x = "visitation rate stability",y="fruit proportion stability",subtitle = "L.stoechas")+  theme_classic()


# Relationship between seed number stability and visitation rate stability

plots_sta_seed <- full_2 %>%
  group_by(Plant_gen_sp) %>%
  group_map(~ ggplot(.) + aes(x=cv_1_visitation, y=cv_1_seed) + 
              geom_point() + ggtitle(.y[[1]]))

plots_sta_seed[[1]]+plots_sta_seed[[2]]+plots_sta_seed[[3]]+plots_sta_seed[[4]]
plots_sta_seed[[5]]+plots_sta_seed[[6]]+plots_sta_seed[[7]]


#model
sta_seed<-full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_seed~cv_1_visitation,data))) %>%
  summarize(tidy(mod))%>%
  ungroup()

#goodness of fit measures, p-values for hypothesis tests on residuals, or model convergence information.
sta_seed_glance<-full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_seed~cv_1_visitation,data))) %>%
  summarize(glance(mod))%>%
  ungroup()

#checking residuals
sta_seed_resid<-full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lm(cv_1_seed~cv_1_visitation,data)))%>%
  mutate(plots = list(performance::check_model(model))) %>%
  ungroup()

plot(sta_seed_resid$plots[[1]]) # Cistus crispus
plot(sta_seed_resid$plots[[2]]) # Cistus ladanifer
plot(sta_seed_resid$plots[[3]]) # Cistus salviifolius
plot(sta_seed_resid$plots[[4]]) # Halimium calycinum
plot(sta_seed_resid$plots[[5]]) # Halimium halimifolium
plot(sta_seed_resid$plots[[6]]) # Lavandula pedunculata
plot(sta_seed_resid$plots[[7]]) # Lavandula stoechas

# model plot (seed number stability and visitation rate stability)
sta_seed.1 <- full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_seed~cv_1_visitation,data))) %>%
  mutate(mod1 = list(ggeffects::ggpredict(mod, terms = "cv_1_visitation"))) %>%
  mutate(plots = list(ggplot(mod1, aes(x, predicted)) + geom_line() +
                        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)))


sta_seed.1$plots[[1]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  cv_1_visitation, y = cv_1_seed))+ 
  labs(x = "visitation rate stability",y="seed number stability",subtitle = "C.crispus")+theme_classic()

sta_seed.1$plots[[2]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  cv_1_visitation, y = cv_1_seed))+ 
  labs(x = "visitation rate stability",y="seed number stability",subtitle = "C.ladanifer")+  theme_classic()

sta_seed.1$plots[[3]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  cv_1_visitation, y = cv_1_seed))+ 
  labs(x = "visitation rate stability",y="seed number stability",subtitle = "C.salviifolius")+  theme_classic()

sta_seed.1$plots[[4]] + geom_point(data = full1 %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  cv_1_visitation, y = cv_1_seed))+ 
  labs(x = "visitation rate stability",y="seed number stability",subtitle = "H.calycinum")+  theme_classic()

sta_seed.1$plots[[5]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x =  cv_1_visitation, y = cv_1_seed))+ 
  labs(x = "visitation rate stability",y="seed number stability",subtitle = "H.halimifolium")+  theme_classic()

sta_seed.1$plots[[6]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  cv_1_visitation, y = cv_1_seed))+ 
  labs(x = "visitation rate stability",y="seed number stability",subtitle = "L.pedunculata")+  theme_classic()

sta_seed.1$plots[[7]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Lavandula stoechas"), aes(x =  cv_1_visitation, y = cv_1_seed))+ 
  labs(x = "visitation rate stability",y="seed number stability",subtitle = "L.stoechas")+  theme_classic()


######

#relationship between richness-synchrony with different synchrony indice.

# new data (full_2)
# total richness ~ logVR

plots_rich <- full_2 %>%
  group_by(Plant_gen_sp) %>%
  group_map(~ ggplot(.) + aes(x=log_VR, y=S_total) + 
              geom_point() + ggtitle(.y[[1]]))

plots_rich[[1]]+plots_rich[[2]]+plots_rich[[3]]+plots_rich[[4]]
plots_rich[[5]]+plots_rich[[6]]+plots_rich[[7]]

#model
rich<-full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(S_total~log_VR,data))) %>%
  summarize(tidy(mod))%>%
  ungroup()

rich_glance<-full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(S_total~log_VR,data))) %>%
  summarize(glance(mod))%>%
  ungroup()


#checking residuals
rich_resid<-full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lm(S_total~log_VR,data)))%>%
  mutate(plots = list(performance::check_model(model))) %>%
  ungroup()

plot(rich_resid$plots[[1]]) # Cistus crispus
plot(rich_resid$plots[[2]]) # Cistus ladanifer
plot(rich_resid$plots[[3]]) # Cistus salviifolius
plot(rich_resid$plots[[4]]) # Halimium calycinum
plot(rich_resid$plots[[5]]) # Halimium halimifolium
plot(rich_resid$plots[[6]]) # Lavandula pedunculata
plot(rich_resid$plots[[7]]) # Lavandula stoechas

# model plot
rich.1 <- full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(S_total~log_VR,data))) %>%
  mutate(mod1 = list(ggeffects::ggpredict(mod, terms = "log_VR"))) %>%
  mutate(plots = list(ggplot(mod1, aes(x, predicted)) + geom_line() +
                        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))


rich.1$plots[[1]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  log_VR, y = S_total))+ 
  labs(x = "log_VR",y="total richness",subtitle = "C.crispus")+theme_classic()

rich.1$plots[[2]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  log_VR, y = S_total))+ 
  labs(x = "log_VR",y="total richness",subtitle = "C.ladanifer")+  theme_classic()

rich.1$plots[[3]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  log_VR, y = S_total))+ 
  labs(x = "log_VR",y="total richness",subtitle = "C.salviifolius")+  theme_classic()

rich.1$plots[[4]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  log_VR, y = S_total))+ 
  labs(x = "log_VR",y="total richness",subtitle = "H.calycinum")+  theme_classic()

rich.1$plots[[5]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x =  log_VR, y = S_total))+ 
  labs(x = "log_VR",y="total richness",subtitle = "H.halimifolium")+  theme_classic()

rich.1$plots[[6]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  log_VR, y = S_total))+ 
  labs(x = "log_VR",y="total richness",subtitle = "L.pedunculata")+  theme_classic()

rich.1$plots[[7]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Lavandula stoechas"), aes(x =  log_VR, y = S_total))+ 
  labs(x = "log_VR",y="total richness",subtitle = "L.stoechas")+  theme_classic()


# total richnes ~ L& M index
rich2<-full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(S_total~syncLM,data))) %>%
  summarize(tidy(mod))%>%
  ungroup()


rich2_glance<-full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(S_total~syncLM,data))) %>%
  summarize(glance(mod))%>%
  ungroup()

rich.2 <- full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(S_total~syncLM,data))) %>%
  mutate(mod1 = list(ggeffects::ggpredict(mod, terms = "syncLM"))) %>%
  mutate(plots = list(ggplot(mod1, aes(x, predicted)) + geom_line() +
                        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))


rich.2$plots[[1]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  syncLM, y = S_total))+ 
  labs(x = "syncLM",y="total richness",subtitle = "C.crispus")+theme_classic()

rich.2$plots[[2]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  syncLM, y = S_total))+ 
  labs(x = "syncLM",y="total richness",subtitle = "C.ladanifer")+  theme_classic()

rich.2$plots[[3]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  syncLM, y = S_total))+ 
  labs(x = "syncLM",y="total richness",subtitle = "C.salviifolius")+  theme_classic()

rich.2$plots[[4]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  syncLM, y = S_total))+ 
  labs(x = "syncLM",y="total richness",subtitle = "H.calycinum")+  theme_classic()

rich.2$plots[[5]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x =  syncLM, y = S_total))+ 
  labs(x = "syncLM",y="total richness",subtitle = "H.halimifolium")+  theme_classic()

rich.2$plots[[6]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  syncLM, y = S_total))+ 
  labs(x = "syncLM",y="total richness",subtitle = "L.pedunculata")+  theme_classic()

rich.2$plots[[7]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Lavandula stoechas"), aes(x =  syncLM, y = S_total))+ 
  labs(x = "syncLM",y="total richness",subtitle = "L.stoechas")+  theme_classic()


# mean richness ~ logVR
rich3<-full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(S_mean~log_VR,data))) %>%
  summarize(tidy(mod))%>%
  ungroup()

rich3_glance<-full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(S_mean~log_VR,data))) %>%
  summarize(glance(mod))%>%
  ungroup()

rich.3 <- full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(S_mean~log_VR,data))) %>%
  mutate(mod1 = list(ggeffects::ggpredict(mod, terms = "log_VR"))) %>%
  mutate(plots = list(ggplot(mod1, aes(x, predicted)) + geom_line() +
                        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))


rich.3$plots[[1]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  log_VR, y = S_mean))+ 
  labs(x = "log_VR",y="S_mean",subtitle = "C.crispus")+theme_classic()

rich.3$plots[[2]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  log_VR, y = S_mean))+ 
  labs(x = "log_VR",y="S_mean",subtitle = "C.ladanifer")+  theme_classic()

rich.3$plots[[3]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  log_VR, y = S_mean))+ 
  labs(x = "log_VR",y="S_mean",subtitle = "C.salviifolius")+  theme_classic()

rich.3$plots[[4]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  log_VR, y = S_mean))+ 
  labs(x = "log_VR",y="S_mean",subtitle = "H.calycinum")+  theme_classic()

rich.3$plots[[5]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x =  log_VR, y = S_mean))+ 
  labs(x = "log_VR",y="S_mean",subtitle = "H.halimifolium")+  theme_classic()

rich.3$plots[[6]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  log_VR, y = S_mean))+ 
  labs(x = "log_VR",y="S_mean",subtitle = "L.pedunculata")+  theme_classic()

rich.3$plots[[7]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Lavandula stoechas"), aes(x =  log_VR, y = S_mean))+ 
  labs(x = "log_VR",y="S_mean",subtitle = "L.stoechas")+  theme_classic()



# mean richness ~ L& M index
rich4<-full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(S_mean~syncLM,data))) %>%
  summarize(tidy(mod))%>%
  ungroup()

rich4_glance<-full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(S_mean~syncLM,data))) %>%
  summarize(glance(mod))%>%
  ungroup()

rich.4 <- full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(S_mean~syncLM,data))) %>%
  mutate(mod1 = list(ggeffects::ggpredict(mod, terms = "syncLM"))) %>%
  mutate(plots = list(ggplot(mod1, aes(x, predicted)) + geom_line() +
                        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))


rich.4$plots[[1]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  syncLM, y = S_mean))+ 
  labs(x = "syncLM",y="S_mean",subtitle = "C.crispus")+theme_classic()

rich.4$plots[[2]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  syncLM, y = S_mean))+ 
  labs(x = "syncLM",y="S_mean",subtitle = "C.ladanifer")+  theme_classic()

rich.4$plots[[3]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  syncLM, y = S_mean))+ 
  labs(x = "syncLM",y="S_mean",subtitle = "C.salviifolius")+  theme_classic()

rich.4$plots[[4]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =syncLM, y = S_mean))+ 
  labs(x = "syncLM",y="S_mean",subtitle = "H.calycinum")+  theme_classic()

rich.4$plots[[5]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x = syncLM, y = S_mean))+ 
  labs(x = "syncLM",y="S_mean",subtitle = "H.halimifolium")+  theme_classic()

rich.4$plots[[6]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x = syncLM, y = S_mean))+ 
  labs(x = "syncLM",y="S_mean",subtitle = "L.pedunculata")+  theme_classic()

rich.4$plots[[7]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Lavandula stoechas"), aes(x =  syncLM, y = S_mean))+ 
  labs(x = "syncLM",y="S_mean",subtitle = "L.stoechas")+  theme_classic()



######
# Stability of visitation rate is affected by richness and synchrony

#model 1: total richness + log VR
sta_polli<-full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~S_total+log_VR,data))) %>%
  summarize(tidy(mod))%>%
  ungroup()

#goodness of fit measures, p-values for hypothesis tests on residuals, or model convergence information.
sta_polli_glance<-full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~S_total+log_VR,data))) %>%
  summarize(glance(mod))%>%
  ungroup()

# summarize(augment(mod)) para obtener fitted, resid,std.resid, stc


# model plot (cv_1_visitation~richness)
sta_polli2 <- full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~S_total+log_VR,data))) %>%
  mutate(mod1 = list(ggeffects::ggpredict(mod, terms = "S_total"))) %>%
  mutate(plots = list(ggplot(mod1, aes(x, predicted)) + geom_line() +
                        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))



sta_polli2$plots[[1]] + geom_point(data = na.omit(full_2 %>% filter(Plant_gen_sp == "Cistus crispus")), aes(x =  S_total, y = cv_1_visitation))+ 
  labs(x = "S_total",y="visitation rate stability",subtitle = "C.crispus",cex.sub=0.2)+
  theme_classic()

sta_polli2$plots[[2]] + geom_point(data = na.omit(full_2 %>% filter(Plant_gen_sp == "Cistus ladanifer")), aes(x =  S_total, y = cv_1_visitation))+ 
  labs(x = "S_total",y="visitation rate stability",subtitle = "C.ladanifer")+  theme_classic()

sta_polli2$plots[[3]] + geom_point(data = na.omit(full_2 %>% filter(Plant_gen_sp == "Cistus salviifolius")), aes(x =  S_total, y = cv_1_visitation))+ 
  labs(x = "S_total",y="visitation rate stability",subtitle = "C.salviifolius")+  theme_classic()

sta_polli2$plots[[4]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  S_total, y = cv_1_visitation))+ 
  labs(x = "S_total",y="visitation rate stability",subtitle = "H.calycinum")+  theme_classic()

sta_polli2$plots[[5]] + geom_point(data = na.omit(full_2 %>% filter(Plant_gen_sp == "Halimium halimifolium")), aes(x =  S_total, y = cv_1_visitation))+ 
  labs(x = "S_total",y="visitation rate stability",subtitle = "H.halimifolium")+  theme_classic()

sta_polli2$plots[[6]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  S_total, y = cv_1_visitation))+ 
  labs(x = "S_total",y="visitation rate stability",subtitle = "L.pedunculata")+  theme_classic()

sta_polli2$plots[[7]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Lavandula stoechas"), aes(x =  S_total, y = cv_1_visitation))+ 
  labs(x = "S_total",y="visitation rate stability",subtitle = "L.stoechas")+  theme_classic()



# model plot (cv_1_visitation~log_VR)
sta_polli2.1 <- full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~S_total+log_VR,data))) %>%
  mutate(mod1 = list(ggeffects::ggpredict(mod, terms = "log_VR"))) %>%
  mutate(plots = list(ggplot(mod1, aes(x, predicted)) + geom_line() +
                        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)))


sta_polli2.1$plots[[1]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  log_VR, y = cv_1_visitation))+ 
  labs(x = "log VR",y="visitation rate stability",subtitle = "C.crispus",cex.sub=0.2)+
  theme_classic()

sta_polli2.1$plots[[2]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  log_VR, y = cv_1_visitation))+ 
  labs(x = "log VR",y="visitation rate stability",subtitle = "C.ladanifer")+  theme_classic()

sta_polli2.1$plots[[3]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  log_VR, y = cv_1_visitation))+ 
  labs(x = "log VR",y="visitation rate stability",subtitle = "C.salviifolius")+  theme_classic()

sta_polli2.1$plots[[4]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  log_VR, y = cv_1_visitation))+ 
  labs(x = "log VR",y="visitation rate stability",subtitle = "H.calycinum")+  theme_classic()

sta_polli2.1$plots[[5]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x =  log_VR, y = cv_1_visitation))+ 
  labs(x = "log VR",y="visitation rate stability",subtitle = "H.halimifolium")+  theme_classic()

sta_polli2.1$plots[[6]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  log_VR, y = cv_1_visitation))+ 
  labs(x = "log VR",y="visitation rate stability",subtitle = "L.pedunculata")+  theme_classic()

sta_polli2.1$plots[[7]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Lavandula stoechas"), aes(x =  log_VR, y = cv_1_visitation))+ 
  labs(x = "log VR",y="visitation rate stability",subtitle = "L.stoechas")+  theme_classic()



#model 2: total richness + Lorea and Mazancourt index
sta_polli3<-full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~S_total+syncLM,data))) %>%
  summarize(tidy(mod))%>%
  ungroup()

#goodness of fit measures
sta_polli_glance3<-full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~S_total+syncLM,data))) %>%
  summarize(glance(mod))%>%
  ungroup()


# model2 plot (cv_1_visitation~total richness)
sta_polli3.1 <- full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~S_total+syncLM,data))) %>%
  mutate(mod1 = list(ggeffects::ggpredict(mod, terms = "S_total"))) %>%
  mutate(plots = list(ggplot(mod1, aes(x, predicted)) + geom_line() +
                        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)))



sta_polli3.1$plots[[1]] + geom_point(data = na.omit(full_2 %>% filter(Plant_gen_sp == "Cistus crispus")), aes(x =  S_total, y = cv_1_visitation))+ 
  labs(x = "S_total",y="visitation rate stability",subtitle = "C.crispus",cex.sub=0.2)+
  theme_classic()

sta_polli3.1$plots[[2]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  S_total, y = cv_1_visitation))+ 
  labs(x = "S_total",y="visitation rate stability",subtitle = "C.ladanifer")+  theme_classic()

sta_polli3.1$plots[[3]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  S_total, y = cv_1_visitation))+ 
  labs(x = "S_total",y="visitation rate stability",subtitle = "C.salviifolius")+  theme_classic()

sta_polli3.1$plots[[4]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  S_total, y = cv_1_visitation))+ 
  labs(x = "S_total",y="visitation rate stability",subtitle = "H.calycinum")+  theme_classic()

sta_polli3.1$plots[[5]] + geom_point(data = na.omit(full_2 %>% filter(Plant_gen_sp == "Halimium halimifolium")), aes(x =  S_total, y = cv_1_visitation))+ 
  labs(x = "S_total",y="visitation rate stability",subtitle = "H.halimifolium")+  theme_classic()

sta_polli3.1$plots[[6]] + geom_point(data = na.omit(full_2 %>% filter(Plant_gen_sp == "Lavandula pedunculata")), aes(x =  S_total, y = cv_1_visitation))+ 
  labs(x = "S_total",y="visitation rate stability",subtitle = "L.pedunculata")+  theme_classic()

sta_polli3.1$plots[[7]] + geom_point(data = na.omit(full_2 %>% filter(Plant_gen_sp == "Lavandula stoechas")), aes(x =  S_total, y = cv_1_visitation))+ 
  labs(x = "S_total",y="visitation rate stability",subtitle = "L.stoechas")+  theme_classic()


# model2 plot (cv_1_visitation~syncLM)
sta_polli3.2 <- full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~S_total+syncLM,data))) %>%
  mutate(mod1 = list(ggeffects::ggpredict(mod, terms = "syncLM"))) %>%
  mutate(plots = list(ggplot(mod1, aes(x, predicted)) + geom_line() +
                        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)))



sta_polli3.2$plots[[1]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  syncLM, y = cv_1_visitation))+ 
  labs(x = "Lorea and Mazancourt index",y="visitation rate stability",subtitle = "C.crispus",cex.sub=0.2)+
  theme_classic()

sta_polli3.2$plots[[2]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  syncLM, y = cv_1_visitation))+ 
  labs(x = "Lorea and Mazancourt index",y="visitation rate stability",subtitle = "C.ladanifer")+  theme_classic()

sta_polli3.2$plots[[3]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  syncLM, y = cv_1_visitation))+ 
  labs(x = "Lorea and Mazancourt index",y="visitation rate stability",subtitle = "C.salviifolius")+  theme_classic()

sta_polli3.2$plots[[4]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  syncLM, y = cv_1_visitation))+ 
  labs(x = "Lorea and Mazancourt index",y="visitation rate stability",subtitle = "H.calycinum")+  theme_classic()

sta_polli3.2$plots[[5]] + geom_point(data = na.omit(full_2 %>% filter(Plant_gen_sp == "Halimium halimifolium")), aes(x =  syncLM, y = cv_1_visitation))+ 
  labs(x = "Lorea and Mazancourt index",y="visitation rate stability",subtitle = "H.halimifolium")+  theme_classic()

sta_polli3.2$plots[[6]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  syncLM, y = cv_1_visitation))+ 
  labs(x = "Lorea and Mazancourt index",y="visitation rate stability",subtitle = "L.pedunculata")+  theme_classic()

sta_polli3.2$plots[[7]] + geom_point(data = na.omit(full_2 %>% filter(Plant_gen_sp == "Lavandula stoechas")), aes(x =  syncLM, y = cv_1_visitation))+ 
  labs(x = "Lorea and Mazancourt index",y="visitation rate stability",subtitle = "L.stoechas")+  theme_classic()



#model 3: mean richness + log VR
sta_polli_Smean<-full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~S_mean+log_VR,data))) %>%
  summarize(tidy(mod))%>%
  ungroup()


#goodness of fit measures
sta_polli_glance_mod3<-full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~S_mean+log_VR,data))) %>%
  summarize(glance(mod))%>%
  ungroup()

# model3 plot (cv_1_visitation~S_mean)
sta_polli_Smean_plot <- full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~S_mean+log_VR,data))) %>%
  mutate(mod = list(ggeffects::ggpredict(mod, terms = "S_mean"))) %>%
  mutate(plots = list(ggplot(mod, aes(x, predicted)) + geom_line() +
                        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))



sta_polli_Smean_plot$plots[[1]] + geom_point(data = na.omit(full_2 %>% filter(Plant_gen_sp == "Cistus crispus")), aes(x =  S_mean, y = cv_1_visitation))+ 
  labs(x = "mean richness",y="visitation rate stability",subtitle = "C.crispus",cex.sub=0.2)+
  theme_classic()

sta_polli_Smean_plot$plots[[2]] + geom_point(data = na.omit(full_2 %>% filter(Plant_gen_sp == "Cistus ladanifer")), aes(x =  S_mean, y = cv_1_visitation))+ 
  labs(x = "mean richness",y="visitation rate stability",subtitle = "C.ladanifer")+  theme_classic()

sta_polli_Smean_plot$plots[[3]] + geom_point(data = na.omit(full_2 %>% filter(Plant_gen_sp == "Cistus salviifolius")), aes(x =  S_mean, y = cv_1_visitation))+ 
  labs(x = "mean richness",y="visitation rate stability",subtitle = "C.salviifolius")+  theme_classic()

sta_polli_Smean_plot$plots[[4]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  S_mean, y = cv_1_visitation))+ 
  labs(x = "mean richness",y="visitation rate stability",subtitle = "H.calycinum")+  theme_classic()

sta_polli_Smean_plot$plots[[5]] + geom_point(data = na.omit(full_2 %>% filter(Plant_gen_sp == "Halimium halimifolium")), aes(x =  S_mean, y = cv_1_visitation))+ 
  labs(x = "mean richness",y="visitation rate stability",subtitle = "H.halimifolium")+  theme_classic()

sta_polli_Smean_plot$plots[[6]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  S_mean, y = cv_1_visitation))+ 
  labs(x = "mean richness",y="visitation rate stability",subtitle = "L.pedunculata")+  theme_classic()

sta_polli_Smean_plot$plots[[7]] + geom_point(data = na.omit(full_2 %>% filter(Plant_gen_sp == "Lavandula stoechas")), aes(x =  S_mean, y = cv_1_visitation))+ 
  labs(x = "mean richness",y="visitation rate stability",subtitle = "L.stoechas")+  theme_classic()


# model3 plot (cv_1_visitation~log_VR)
sta_polli_Smean_plot2 <- full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~S_mean+log_VR,data))) %>%
  mutate(mod = list(ggeffects::ggpredict(mod, terms = "log_VR"))) %>%
  mutate(plots = list(ggplot(mod, aes(x, predicted)) + geom_line() +
                        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))



sta_polli_Smean_plot2$plots[[1]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  log_VR, y = cv_1_visitation))+ 
  labs(x = "log_VR",y="visitation rate stability",subtitle = "C.crispus",cex.sub=0.2)+
  theme_classic()

sta_polli_Smean_plot2$plots[[2]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  log_VR, y = cv_1_visitation))+ 
  labs(x = "log_VR",y="visitation rate stability",subtitle = "C.ladanifer")+  theme_classic()

sta_polli_Smean_plot2$plots[[3]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  log_VR, y = cv_1_visitation))+ 
  labs(x = "log_VR",y="visitation rate stability",subtitle = "C.salviifolius")+  theme_classic()

sta_polli_Smean_plot2$plots[[4]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  log_VR, y = cv_1_visitation))+ 
  labs(x = "log_VR",y="visitation rate stability",subtitle = "H.calycinum")+  theme_classic()

sta_polli_Smean_plot2$plots[[5]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x =  log_VR, y = cv_1_visitation))+ 
  labs(x = "log_VR",y="visitation rate stability",subtitle = "H.halimifolium")+  theme_classic()

sta_polli_Smean_plot2$plots[[6]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  log_VR, y = cv_1_visitation))+ 
  labs(x = "log_VR",y="visitation rate stability",subtitle = "L.pedunculata")+  theme_classic()

sta_polli_Smean_plot2$plots[[7]] + geom_point(data = na.omit(full_2 %>% filter(Plant_gen_sp == "Lavandula stoechas")), aes(x =  log_VR, y = cv_1_visitation))+ 
  labs(x = "log_VR",y="visitation rate stability",subtitle = "L.stoechas")+  theme_classic()



#model 4: mean richness +L & M index
sta_polli_Smean2<-full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~S_mean+syncLM,data))) %>%
  summarize(tidy(mod))%>%
  ungroup()


#goodness of fit measures
sta_polli_glance_mod4<-full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~S_mean+syncLM,data))) %>%
  summarize(glance(mod))%>%
  ungroup()

# model4 plot (cv_1_visitation~S_mean)
sta_polli_Smean_plot4 <- full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~S_mean+syncLM,data))) %>%
  mutate(mod = list(ggeffects::ggpredict(mod, terms = "S_mean"))) %>%
  mutate(plots = list(ggplot(mod, aes(x, predicted)) + geom_line() +
                        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))



sta_polli_Smean_plot4$plots[[1]] + geom_point(data = na.omit(full_2 %>% filter(Plant_gen_sp == "Cistus crispus")), aes(x =  S_mean, y = cv_1_visitation))+ 
  labs(x = "mean richness",y="visitation rate stability",subtitle = "C.crispus",cex.sub=0.2)+
  theme_classic()

sta_polli_Smean_plot4$plots[[2]] + geom_point(data = na.omit(full_2 %>% filter(Plant_gen_sp == "Cistus ladanifer")), aes(x =  S_mean, y = cv_1_visitation))+ 
  labs(x = "mean richness",y="visitation rate stability",subtitle = "C.ladanifer")+  theme_classic()

sta_polli_Smean_plot4$plots[[3]] + geom_point(data = na.omit(full_2 %>% filter(Plant_gen_sp == "Cistus salviifolius")), aes(x =  S_mean, y = cv_1_visitation))+ 
  labs(x = "mean richness",y="visitation rate stability",subtitle = "C.salviifolius")+  theme_classic()

sta_polli_Smean_plot4$plots[[4]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  S_mean, y = cv_1_visitation))+ 
  labs(x = "mean richness",y="visitation rate stability",subtitle = "H.calycinum")+  theme_classic()

sta_polli_Smean_plot4$plots[[5]] + geom_point(data = na.omit(full_2 %>% filter(Plant_gen_sp == "Halimium halimifolium")), aes(x =  S_mean, y = cv_1_visitation))+ 
  labs(x = "mean richness",y="visitation rate stability",subtitle = "H.halimifolium")+  theme_classic()

sta_polli_Smean_plot4$plots[[6]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  S_mean, y = cv_1_visitation))+ 
  labs(x = "mean richness",y="visitation rate stability",subtitle = "L.pedunculata")+  theme_classic()

sta_polli_Smean_plot4$plots[[7]] + geom_point(data = na.omit(full_2 %>% filter(Plant_gen_sp == "Lavandula stoechas")), aes(x =  S_mean, y = cv_1_visitation))+ 
  labs(x = "mean richness",y="visitation rate stability",subtitle = "L.stoechas")+  theme_classic()


# model4 plot (cv_1_visitation~log_VR)
sta_polli_Smean_plot4.2 <- full_2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~S_mean+syncLM,data))) %>%
  mutate(mod = list(ggeffects::ggpredict(mod, terms = "syncLM"))) %>%
  mutate(plots = list(ggplot(mod, aes(x, predicted)) + geom_line() +
                        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))



sta_polli_Smean_plot4.2$plots[[1]] + geom_point(data = na.omit(full_2 %>% filter(Plant_gen_sp == "Cistus crispus")), aes(x =  syncLM, y = cv_1_visitation))+ 
  labs(x = "syncLM",y="visitation rate stability",subtitle = "C.crispus",cex.sub=0.2)+
  theme_classic()

sta_polli_Smean_plot4.2$plots[[2]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  syncLM, y = cv_1_visitation))+ 
  labs(x = "syncLM",y="visitation rate stability",subtitle = "C.ladanifer")+  theme_classic()

sta_polli_Smean_plot4.2$plots[[3]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  syncLM, y = cv_1_visitation))+ 
  labs(x = "syncLM",y="visitation rate stability",subtitle = "C.salviifolius")+  theme_classic()

sta_polli_Smean_plot4.2$plots[[4]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  syncLM, y = cv_1_visitation))+ 
  labs(x = "syncLM",y="visitation rate stability",subtitle = "H.calycinum")+  theme_classic()

sta_polli_Smean_plot4.2$plots[[5]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x =  syncLM, y = cv_1_visitation))+ 
  labs(x = "syncLM",y="visitation rate stability",subtitle = "H.halimifolium")+  theme_classic()

sta_polli_Smean_plot4.2$plots[[6]] + geom_point(data = full_2 %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  syncLM, y = cv_1_visitation))+ 
  labs(x = "syncLM",y="visitation rate stability",subtitle = "L.pedunculata")+  theme_classic()

sta_polli_Smean_plot4.2$plots[[7]] + geom_point(data = na.omit(full_2 %>% filter(Plant_gen_sp == "Lavandula stoechas")), aes(x =  syncLM, y = cv_1_visitation))+ 
  labs(x = "syncLM",y="visitation rate stability",subtitle = "L.stoechas")+  theme_classic()






