
library(tidyverse)
library(broom.mixed)
library(broom)
library(lmerTest)
library(dplyr)
library(patchwork)
library(ggcorrplot)
library(ggpubr)
library (lme4)
library(sjPlot)
library(car)
library(effects)




Pollinator_Plant<- read.csv("Data/Pollinator_Plant_Apis_separada.csv") #400 observaciones
Pollinator_Plant<-Pollinator_Plant[,-1]
Pollinator_Plant$Year= as.factor(Pollinator_Plant$Year)
str(Pollinator_Plant)

#Species out (plant species only in one site)
spp_out <- Pollinator_Plant %>%
  group_by(Plant_gen_sp) %>%
  summarise(Unique_Id = n_distinct(Site_ID)) %>%
  filter(Unique_Id == 1) %>%
  select(Plant_gen_sp) %>% pull(Plant_gen_sp)


#Data with species in more than 1 site
Pollinator_Plant = Pollinator_Plant %>% filter(!Plant_gen_sp %in% spp_out)

# replace NA by 0
Pollinator_Plant <- mutate_all(Pollinator_Plant, ~replace(., is.na(.), 0))


#we analyze whether the fruit proportion is affected by richness, 
#year and its interaction for all data 
#with plant species nested within site as random effect

mod1_to= glmer(cbind(fruit_formado, Fruit_No) ~ richness * Year + (1|Site_ID/Plant_gen_sp),family=binomial,data=Pollinator_Plant)
summary(mod1_to)
vif(mod1_to)

plot_model(mod1_to, type = "pred")
plot_model(mod1_to, type = "int")


#getting effects for the two variables
ee <- as.data.frame(Effect(c("richness","Year"),mod = mod1_to))

#plot
ggplot(ee, aes(richness,fit, group=Year, color = Year, fill=Year))+
  geom_line(size=0.8)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2,colour=NA)+
  geom_point(data=Pollinator_Plant, aes(richness, fruit_proportion))+
  labs(x = "richness",y="fruit proportion")



# and visitation rate, year and its interaction
mod2_to= glmer(cbind(fruit_formado, Fruit_No) ~ visitatio_rate * Year + (1|Site_ID/Plant_gen_sp),family=binomial,data=Pollinator_Plant)
summary(mod2_to)

plot_model(mod2_to, type = "pred")
plot_model(mod2_to, type = "int")

#getting effects for the two variables
ee_1 <- as.data.frame(Effect(c("visitatio_rate","Year"),mod = mod2_to))

#plot
ggplot(ee_1, aes(visitatio_rate,fit, group=Year, color = Year, fill=Year))+
  geom_line(size=0.8)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2,colour=NA)+
  geom_point(data=Pollinator_Plant, aes(visitatio_rate, fruit_proportion))+
  labs(x = "visitation rate",y="fruit proportion")

#we also analyze whether the seed number is affected by richness, 
#year and its interaction for all data 
#with plant species nested within site as random effect

# #scale seed number variable
Pollinator_Plant=Pollinator_Plant %>% group_by(Plant_gen_sp)%>% 
  mutate(seed_sc=scale(Seeds, center = T, scale = T)) %>%
  ungroup()

#model
mod3_tot= lmer(seed_sc ~richness * Year + (1|Site_ID/Plant_gen_sp),data=Pollinator_Plant)
summary(mod3_tot)


plot_model(mod3_tot, type = "pred")
plot_model(mod3_tot, type = "int", show.data = T)

#getting effects for the two variables
ee_2 <- as.data.frame(Effect(c("richness","Year"),mod = mod3_tot))

#plot
ggplot(ee_2, aes(richness,fit, group=Year, color = Year, fill=Year))+
  geom_line(size=0.8)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2,colour=NA)+
  geom_point(data=Pollinator_Plant, aes(richness, seed_sc))+
  labs(x = "richness",y="seed number")

# and visitation rate, year and its interaction
#model
mod4_tot= lmer(seed_sc ~visitatio_rate * Year + (1|Site_ID/Plant_gen_sp),data=Pollinator_Plant)
summary(mod4_tot)


plot_model(mod4_tot, type = "pred")
plot_model(mod4_tot, type = "int")

#getting effects for the two variables
ee_3 <- as.data.frame(Effect(c("visitatio_rate","Year"),mod = mod4_tot))

#plot
ggplot(ee_3, aes(visitatio_rate,fit, group=Year, color = Year, fill=Year))+
  geom_line(size=0.8)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2,colour=NA)+
  geom_point(data=Pollinator_Plant, aes(visitatio_rate, seed_sc))+
  labs(x = "visitation rate",y="seed number")



# we analyse the fruit proportion and the seed number per year separately

### 2015 ###----
year2015=Pollinator_Plant %>%
  filter(Year== "2015")

levels(factor(year2015$Site_ID)) # 13 sites
levels(factor(year2015$Plant_gen_sp)) # 12 plant species 


## fruit proportion

# general model (plant species nested within site as random effect)

# model: Richness and visitation rate
mod1_2015= glmer(cbind(fruit_formado, Fruit_No) ~ richness + visitatio_rate + (1|Site_ID/Plant_gen_sp),family=binomial,data=year2015)
summary(mod1_2015)

vif(mod1_2015) #check for correlations between variables included in model


##extract estimates and st errors
coefs.mod1_2015 <- data.frame(coef(summary(mod1_2015)))
Tablemod1_2015<-round(coefs.mod1_2015[,1:4],digits=3)

# plot model 
#extract p-values
text=Tablemod1_2015  %>%
  select(Pr...z..)


effe_mod2015 <- effect("richness", mod1_2015, se = TRUE)
x.modelo2015<- data.frame(effe_mod2015)
p1=ggplot(x.modelo2015, aes(richness, fit)) + geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data = year2015, aes(x =  richness, y = fruit_proportion, color=Plant_gen_sp), size=3)+
  labs(x = "richness",y="fruit proportion",subtitle = "2015")+
  annotate("text", x = Inf, y = -Inf, label=sprintf("p = %5.3f", as.numeric(text$Pr...z..[2])), size= 4, hjust=1.5, vjust=-2)



effe2_mod2015 <- effect("visitatio_rate", mod1_2015, se = TRUE)
x2.modelo2015<- data.frame(effe2_mod2015)
p1.2=ggplot(x2.modelo2015, aes(visitatio_rate, fit)) + geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data = year2015, aes(x =  visitatio_rate, y = fruit_proportion, color=Plant_gen_sp), size=3)+
  labs(x = "visitation rate",y="fruit proportion",subtitle = "2015")+
  annotate("text", x = Inf, y = -Inf, label=sprintf("p = %5.3f", as.numeric(text$Pr...z..[3])), size= 4, hjust=1.5, vjust=-2)



## seed number 

# #scale seed number variable
year2015=year2015 %>% group_by(Plant_gen_sp)%>% mutate(seed_sc=scale(Seeds, center = T, scale = T)) %>%
  ungroup()

#model
mod2_2015= lmer(seed_sc ~richness + visitatio_rate + (1|Site_ID/Plant_gen_sp),data=year2015)
summary(mod2_2015)

vif(mod2_2015) #check for correlations between variables included in model

##extract estimates and st errors
coefs.mod2_2015 <- data.frame(coef(summary(mod2_2015)))
Tablemod2_2015<-round(coefs.mod2_2015[,1:5],digits=3)

#plot model
#extract p-values
text=Tablemod2_2015  %>%
  select(Pr...t..)
  
 
effe_mod2_2015 <- effect("richness", mod2_2015, se = TRUE)
x.modelo2_2015<- data.frame(effe_mod2_2015)
p2=ggplot(x.modelo2_2015, aes(richness, fit)) + geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data = year2015, aes(x =  richness, y = seed_sc, color=Plant_gen_sp), size=3)+
  labs(x = "richness",y="seed number",subtitle = "2015")+
  annotate("text", x = Inf, y = -Inf, label=sprintf("p = %5.3f", as.numeric(text$Pr...t..[2])), size= 4, hjust=1.5, vjust=-2)


effe2_mod2_2015 <- effect("visitatio_rate", mod2_2015, se = TRUE)
x2.modelo2_2015<- data.frame(effe2_mod2_2015)
p2.2=ggplot(x2.modelo2_2015, aes(visitatio_rate, fit)) + geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data = year2015, aes(x =  visitatio_rate, y = seed_sc, color=Plant_gen_sp), size=3)+
  labs(x = "visitation rate",y="seed number",subtitle = "2015")+
  annotate("text", x = Inf, y = -Inf, label=sprintf("p = %5.3f", as.numeric(text$Pr...t..[3])), size= 4, hjust=1.5, vjust=-2)



# join plots 

jpeg('2015.jpg', width = 15, height = 7, units = 'in', res = 1200)

plot_2015=p1+ p1.2+p2 + p2.2+plot_layout(ncol = 2)& theme(legend.position = "bottom")
plot_2015+ plot_layout(guides = "collect")

dev.off()



### 2016 ###----
year2016=Pollinator_Plant %>%
  filter(Year== "2016")

levels(factor(year2016$Site_ID)) # 12 sites
levels(factor(year2016$Plant_gen_sp)) # 11 plant species


## fruit proportion

# general model (plant species nested within site as random effect)

# model: Richness and visitation rate
mod1_2016= glmer(cbind(fruit_formado, Fruit_No) ~ richness + visitatio_rate + (1|Site_ID/Plant_gen_sp),family=binomial,data=year2016)
summary(mod1_2016)

vif(mod1_2016) #check for correlations between variables included in model
r.squaredGLMM(mod1_2016)

##extract estimates and st errors
coefs.mod1_2016 <- data.frame(coef(summary(mod1_2016)))
Tablemod1_2016<-round(coefs.mod1_2016[,1:4],digits=3)

# plot model 
#extract p-values
text=Tablemod1_2016  %>%
  select(Pr...z..)

effe_mod2016 <- effect("richness", mod1_2016, se = TRUE)
x.modelo2016<- data.frame(effe_mod2016)
p1_16=ggplot(x.modelo2016, aes(richness, fit)) + geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data = year2016, aes(x =  richness, y = fruit_proportion, color=Plant_gen_sp), size=3)+
  labs(x = "richness",y="fruit proportion",subtitle = "2016")+
  annotate("text", x = Inf, y = -Inf, label=sprintf("p = %5.3f", as.numeric(text$Pr...z..[2])), size= 4, hjust=1.5, vjust=-2)


effe2_mod2016 <- effect("visitatio_rate", mod1_2016, se = TRUE)
x2.modelo2016<- data.frame(effe2_mod2016)
p1.2_16=ggplot(x2.modelo2016, aes(visitatio_rate, fit)) + geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data = year2016, aes(x =  visitatio_rate, y = fruit_proportion, color=Plant_gen_sp), size=3)+
  labs(x = "visitation rate",y="fruit proportion",subtitle = "2016")+
  annotate("text", x = Inf, y = -Inf, label=sprintf("p = %5.3f", as.numeric(text$Pr...z..[3])), size= 4, hjust=1.5, vjust=-2)


## seed number 

# #scale seed number variable
year2016=year2016 %>% group_by(Plant_gen_sp)%>% 
  mutate(seed_sc=scale(Seeds, center = T, scale = T)) %>%
  ungroup()

#model
mod2_2016= lmer(seed_sc ~richness + visitatio_rate + (1|Site_ID/Plant_gen_sp),data=year2016)
summary(mod2_2016)

vif(mod2_2016) #check for correlations between variables included in model
r.squaredGLMM(mod2_2016)

##extract estimates and st errors
coefs.mod2_2016 <- data.frame(coef(summary(mod2_2016)))
Tablemod2_2016<-round(coefs.mod2_2016[,1:5],digits=3)

#plot model
text=Tablemod2_2016  %>%
  select(Pr...t..)

effe_mod2_2016 <- effect("richness", mod2_2016, se = TRUE)
x.modelo2_2016<- data.frame(effe_mod2_2016)
p2_16=ggplot(x.modelo2_2016, aes(richness, fit)) + geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data = year2016, aes(x =  richness, y = seed_sc, color=Plant_gen_sp), size=3)+
  labs(x = "richness",y="seed number",subtitle = "2016")+
  annotate("text", x = Inf, y = -Inf, label=sprintf("p = %5.3f", as.numeric(text$Pr...t..[2])), size= 4, hjust=1.5, vjust=-2)



effe2_mod2_2016 <- effect("visitatio_rate", mod2_2016, se = TRUE)
x2.modelo2_2016<- data.frame(effe2_mod2_2016)
p2.2_16=ggplot(x2.modelo2_2016, aes(visitatio_rate, fit)) + geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data = year2016, aes(x =  visitatio_rate, y = seed_sc, color=Plant_gen_sp), size=3)+
  labs(x = "visitation rate",y="seed number",subtitle = "2016")+
  annotate("text", x = Inf, y = -Inf, label=sprintf("p = %5.3f", as.numeric(text$Pr...t..[3])), size= 4, hjust=1.5, vjust=-2)


# join plots 

jpeg('2016.jpg', width = 15, height = 7, units = 'in', res = 1200)

plot_2016=p1_16+ p1.2_16+p2_16 + p2.2_16+plot_layout(ncol = 2)& theme(legend.position = "bottom")
plot_2016+ plot_layout(guides = "collect")

dev.off()




### 2017 ###----
year2017=Pollinator_Plant %>%
  filter(Year== "2017")

levels(factor(year2017$Site_ID)) # 11 sites
levels(factor(year2017$Plant_gen_sp)) # 11 plant species


## fruit proportion

# general model (plant species nested within site as random effect)

# model: Richness and visitation rate
mod1_2017= glmer(cbind(fruit_formado, Fruit_No) ~ richness + visitatio_rate + (1|Site_ID/Plant_gen_sp),family=binomial,data=year2017)
summary(mod1_2017)

vif(mod1_2017) #check for correlations between variables included in model
r.squaredGLMM(mod1_2017)

##extract estimates and st errors
coefs.mod1_2017 <- data.frame(coef(summary(mod1_2017)))
Tablemod1_2017<-round(coefs.mod1_2017[,1:4],digits=3)

# plot model 
#extract p-values
text=Tablemod1_2017  %>%
  select(Pr...z..)

effe_mod2017 <- effect("richness", mod1_2017, se = TRUE)
x.modelo2017<- data.frame(effe_mod2017)
p1_17=ggplot(x.modelo2017, aes(richness, fit)) + geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data = year2017, aes(x =  richness, y = fruit_proportion, color=Plant_gen_sp), size=3)+
  labs(x = "richness",y="fruit proportion",subtitle = "2017")+
  annotate("text", x = Inf, y = -Inf, label=sprintf("p = %5.3f", as.numeric(text$Pr...z..[2])), size= 4, hjust=1.5, vjust=-2)


effe2_mod2017 <- effect("visitatio_rate", mod1_2017, se = TRUE)
x2.modelo2017<- data.frame(effe2_mod2017)
p1.2_17=ggplot(x2.modelo2017, aes(visitatio_rate, fit)) + geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data = year2017, aes(x =  visitatio_rate, y = fruit_proportion, color=Plant_gen_sp), size=3)+
  labs(x = "visitation rate",y="fruit proportion",subtitle = "2017")+
  annotate("text", x = Inf, y = -Inf, label=sprintf("p = %5.3f", as.numeric(text$Pr...z..[3])), size= 4, hjust=1.5, vjust=-2)



## seed number 

# #scale seed number variable
year2017=year2017 %>% group_by(Plant_gen_sp)%>% 
  mutate(seed_sc=scale(Seeds, center = T, scale = T)) %>%
  ungroup()

#model
mod2_2017= lmer(seed_sc ~richness + visitatio_rate + (1|Site_ID/Plant_gen_sp),data=year2017)
summary(mod2_2017)

vif(mod2_2017) #check for correlations between variables included in model
r.squaredGLMM(mod2_2017)


##extract estimates and st errors
coefs.mod2_2017 <- data.frame(coef(summary(mod2_2017)))
Tablemod2_2017<-round(coefs.mod2_2017[,1:5],digits=3)

#plot model
text=Tablemod2_2017  %>%
  select(Pr...t..)

effe_mod2_2017 <- effect("richness", mod2_2017, se = TRUE)
x.modelo2_2017<- data.frame(effe_mod2_2017)
p2_17=ggplot(x.modelo2_2017, aes(richness, fit)) + geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data = year2017, aes(x =  richness, y = seed_sc, color=Plant_gen_sp), size=3)+
  labs(x = "richness",y="seed number",subtitle = "2017")+
  annotate("text", x = Inf, y = -Inf, label=sprintf("p = %5.3f", as.numeric(text$Pr...t..[2])), size= 4, hjust=1.5, vjust=-2)



effe2_mod2_2017 <- effect("visitatio_rate", mod2_2017, se = TRUE)
x2.modelo2_2017<- data.frame(effe2_mod2_2017)
p2.2_17=ggplot(x2.modelo2_2017, aes(visitatio_rate, fit)) + geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data = year2017, aes(x =  visitatio_rate, y = seed_sc, color=Plant_gen_sp), size=3)+
  labs(x = "visitation rate",y="seed number",subtitle = "2017")+
  annotate("text", x = Inf, y = -Inf, label=sprintf("p = %5.3f", as.numeric(text$Pr...t..[3])), size= 4, hjust=1.5, vjust=-2)


# join plots 

jpeg('2017.jpg', width = 15, height = 7, units = 'in', res = 1200)

plot_2017=p1_17+ p1.2_17+p2_17 + p2.2_17+plot_layout(ncol = 2)& theme(legend.position = "bottom")
plot_2017+ plot_layout(guides = "collect")

dev.off()



### 2018 ###----
year2018=Pollinator_Plant %>%
  filter(Year== "2018")

levels(factor(year2018$Site_ID)) # 13 sites
levels(factor(year2018$Plant_gen_sp)) # 10 plant species


## fruit proportion

# general model (plant species nested within site as random effect)

# model: Richness and visitation rate
mod1_2018= glmer(cbind(fruit_formado, Fruit_No) ~ richness + visitatio_rate + (1|Site_ID/Plant_gen_sp),family=binomial,data=year2018)
summary(mod1_2018)

vif(mod1_2018) #check for correlations between variables included in model
tab_model(mod1_2018)
performance::model_performance(mod1_2018)
DHARMa::simulateResiduals(mod1_2018, plot = TRUE, re.form = NULL)
r.squaredGLMM(mod1_2018)


##extract estimates and st errors
coefs.mod1_2018 <- data.frame(coef(summary(mod1_2018)))
Tablemod1_2018<-round(coefs.mod1_2018[,1:4],digits=3)

# plot model 
#extract p-values
text=Tablemod1_2018  %>%
  select(Pr...z..)

text$Pr...z.. = format.pval(text$Pr...z..)


effe_mod2018 <- effect("richness", mod1_2018, se = TRUE)
x.modelo2018<- data.frame(effe_mod2018)
p1_18=ggplot(x.modelo2018, aes(richness, fit)) + geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data = year2018, aes(x =  richness, y = fruit_proportion, color=Plant_gen_sp), size=3)+
  labs(x = "richness",y="fruit proportion",subtitle = "2018")+
  annotate("text", x = Inf, y = -Inf, label=sprintf("p = < 0.001"), size= 4, hjust=1.5, vjust=-2)


effe2_mod2018 <- effect("visitatio_rate", mod1_2018, se = TRUE)
x2.modelo2018<- data.frame(effe2_mod2018)
p1.2_18=ggplot(x2.modelo2018, aes(visitatio_rate, fit)) + geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data = year2018, aes(x =  visitatio_rate, y = fruit_proportion, color=Plant_gen_sp), size=3)+
  labs(x = "visitation rate",y="fruit proportion",subtitle = "2018")+
  annotate("text", x = Inf, y = -Inf, label=sprintf("p = < 0.001"), size= 4, hjust=1.5, vjust=-2)



## seed number 

# #scale seed number variable
year2018=year2018 %>% group_by(Plant_gen_sp)%>% 
  mutate(seed_sc=scale(Seeds, center = T, scale = T)) %>%
  ungroup()

#model
mod2_2018= lmer(seed_sc ~richness + visitatio_rate + (1|Site_ID/Plant_gen_sp),data=year2018)
summary(mod2_2018)

vif(mod2_2018) #check for correlations between variables included in model
r.squaredGLMM(mod2_2018)


##extract estimates and st errors
coefs.mod2_2018 <- data.frame(coef(summary(mod2_2018)))
Tablemod2_2018<-round(coefs.mod2_2018[,1:5],digits=3)

#plot model
text=Tablemod2_2018  %>%
  select(Pr...t..)

effe_mod2_2018 <- effect("richness", mod2_2018, se = TRUE)
x.modelo2_2018<- data.frame(effe_mod2_2018)
p2_18=ggplot(x.modelo2_2018, aes(richness, fit)) + geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data = year2018, aes(x =  richness, y = seed_sc, color=Plant_gen_sp), size=3)+
  labs(x = "richness",y="seed number",subtitle = "2018")+
  annotate("text", x = Inf, y = -Inf, label=sprintf("p = %5.3f", as.numeric(text$Pr...t..[2])), size= 4, hjust=1.5, vjust=-2)



effe2_mod2_2018 <- effect("visitatio_rate", mod2_2018, se = TRUE)
x2.modelo2_2018<- data.frame(effe2_mod2_2018)
p2.2_18=ggplot(x2.modelo2_2018, aes(visitatio_rate, fit)) + geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data = year2018, aes(x =  visitatio_rate, y = seed_sc, color=Plant_gen_sp), size=3)+
  labs(x = "visitation rate",y="seed number",subtitle = "2018")+
  annotate("text", x = Inf, y = -Inf, label=sprintf("p = %5.3f", as.numeric(text$Pr...t..[3])), size= 4, hjust=1.5, vjust=-2)


# join plots 

jpeg('2018.jpg', width = 15, height = 7, units = 'in', res = 1200)

plot_2018=p1_18+ p1.2_18+p2_18 + p2.2_18+plot_layout(ncol = 2)& theme(legend.position = "bottom")
plot_2018+ plot_layout(guides = "collect")

dev.off()


### 2019 ###----
year2019=Pollinator_Plant %>%
  filter(Year== "2019")

levels(factor(year2019$Site_ID)) # 13 sites
levels(factor(year2019$Plant_gen_sp)) # 10 plant species


## fruit proportion

# general model (plant species nested within site as random effect)

# model: Richness and visitation rate
mod1_2019= glmer(cbind(fruit_formado, Fruit_No) ~ richness + visitatio_rate + (1|Site_ID/Plant_gen_sp),family=binomial,data=year2019)
summary(mod1_2019)

vif(mod1_2019) #check for correlations between variables included in model
r.squaredGLMM(mod1_2019)


##extract estimates and st errors
coefs.mod1_2019 <- data.frame(coef(summary(mod1_2019)))
Tablemod1_2019<-round(coefs.mod1_2019[,1:4],digits=3)

# plot model 
#extract p-values
text=Tablemod1_2019  %>%
  select(Pr...z..)



effe_mod2019 <- effect("richness", mod1_2019, se = TRUE)
x.modelo2019<- data.frame(effe_mod2019)
p1_19=ggplot(x.modelo2019, aes(richness, fit)) + geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data = year2019, aes(x =  richness, y = fruit_proportion, color=Plant_gen_sp), size=3)+
  labs(x = "richness",y="fruit proportion",subtitle = "2019")+
  annotate("text", x = Inf, y = -Inf, label=sprintf("p = %5.3f", as.numeric(text$Pr...z..[2])), size= 4, hjust=1.5, vjust=-2)


effe2_mod2019 <- effect("visitatio_rate", mod1_2019, se = TRUE)
x2.modelo2019<- data.frame(effe2_mod2019)
p1.2_19=ggplot(x2.modelo2019, aes(visitatio_rate, fit)) + geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data = year2019, aes(x =  visitatio_rate, y = fruit_proportion, color=Plant_gen_sp), size=3)+
  labs(x = "visitation rate",y="fruit proportion",subtitle = "2019")+
  annotate("text", x = Inf, y = -Inf, label=sprintf("p = %5.3f", as.numeric(text$Pr...z..[3])), size= 4, hjust=1.5, vjust=-2)


## seed number 

# #scale seed number variable
year2019=year2019 %>% group_by(Plant_gen_sp)%>% 
  mutate(seed_sc=scale(Seeds, center = T, scale = T)) %>%
  ungroup()

#model
mod2_2019= lmer(seed_sc ~richness + visitatio_rate + (1|Site_ID/Plant_gen_sp),data=year2019)
summary(mod2_2019)

vif(mod2_2019) #check for correlations between variables included in model
r.squaredGLMM(mod2_2019)


##extract estimates and st errors
coefs.mod2_2019 <- data.frame(coef(summary(mod2_2019)))
Tablemod2_2019<-round(coefs.mod2_2018[,1:5],digits=3)

#plot model
text=Tablemod2_2019  %>%
  select(Pr...t..)

effe_mod2_2019 <- effect("richness", mod2_2019, se = TRUE)
x.modelo2_2019<- data.frame(effe_mod2_2019)
p2_19=ggplot(x.modelo2_2019, aes(richness, fit)) + geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data = year2019, aes(x =  richness, y = seed_sc, color=Plant_gen_sp), size=3)+
  labs(x = "richness",y="seed number",subtitle = "2019")+
  annotate("text", x = Inf, y = -Inf, label=sprintf("p = %5.3f", as.numeric(text$Pr...t..[2])), size= 4, hjust=1.5, vjust=-2)



effe2_mod2_2019 <- effect("visitatio_rate", mod2_2019, se = TRUE)
x2.modelo2_2019<- data.frame(effe2_mod2_2019)
p2.2_19=ggplot(x2.modelo2_2019, aes(visitatio_rate, fit)) + geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data = year2019, aes(x =  visitatio_rate, y = seed_sc, color=Plant_gen_sp), size=3)+
  labs(x = "visitation rate",y="seed number",subtitle = "2019")+
  annotate("text", x = Inf, y = -Inf, label=sprintf("p = %5.3f", as.numeric(text$Pr...t..[3])), size= 4, hjust=1.5, vjust=-2)


# join plots 

jpeg('2019.jpg', width = 15, height = 7, units = 'in', res = 1200)

plot_2019=p1_19+ p1.2_19+p2_19 + p2.2_19+plot_layout(ncol = 2)& theme(legend.position = "bottom")
plot_2019+ plot_layout(guides = "collect")

dev.off()
