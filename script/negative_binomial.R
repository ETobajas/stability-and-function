
################################
# stability of visitation rate #
################################
# checking models

# stability data 
data_plant<- read.csv("Data/Stability.csv")
data_plant<-data_plant[,-1]


# delete plant species 
sp_out2=data_plant %>%
  group_by(Plant_gen_sp) %>%
  summarise(Unique_Id = n_distinct(Site_ID)) %>%
  filter(Unique_Id <= 4) %>%
  dplyr::select(Plant_gen_sp) %>% pull(Plant_gen_sp)

#Data with species in at least 5 sites
data_plant = data_plant %>% filter(!Plant_gen_sp %in% sp_out2)

levels(factor(data_plant$Plant_gen_sp))


data_plant=data_plant %>%
  filter(!Plant_gen_sp=="Salvia rosmarinus") %>%
  filter(!Plant_gen_sp=="Halimium calycinum")%>%
  filter(!Plant_gen_sp=="Lavandula stoechas")


## Replace Inf and -Inf with NA
data_plant[is.na(data_plant) | data_plant == "Inf"] <- NA 
data_plant[is.na(data_plant) | data_plant == "-Inf"] <- NA 

# We can express asynchrony as 1 - φ, 
#where φ is Loreau and de Mazancourt’s (2008)
data_plant$asyn_LM= 1-data_plant$syncLM



# stability ~ richness
# negative binomial
glm.nb1_sta= glmer.nb (cv_1_visitation ~ S_total + (1|Site_ID) + (1|Plant_gen_sp), data = data_plant)
summary(glm.nb1_sta)


simulationOutput <- simulateResiduals(fittedModel = glm.nb1_sta, plot = F)
plot(simulationOutput)


#getting effects for richness
effe2_mod_sta <-data.frame( effect("S_total", glm.nb1_sta, se = TRUE))
#plot model 
p2=ggplot(effe2_mod_sta, aes(S_total, fit)) + geom_line(size=1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 3, alpha=0.1, colour = "black")+
  labs(color='Plant species')+
  geom_point(data = data_plant, aes(x =  S_total, y = cv_1_visitation, color=Plant_gen_sp), size=3)+
  theme_classic ()+theme(panel.border = element_rect(colour = "black", fill=NA))+
  labs(x = "Richness of pollinator",y="")




# stability ~ asynchrony
# negative binomial
glm.nb2_sta= glmer.nb (cv_1_visitation ~ asyn_LM + (1|Site_ID) + (1|Plant_gen_sp), data = data_plant)
summary(glm.nb2_sta)

simulationOutput <- simulateResiduals(fittedModel = glm.nb2_sta, plot = F)
plot(simulationOutput)



#getting effects for asyncLM 
effe_mod_sta <-data.frame( effect("asyn_LM", glm.nb2_sta, se = TRUE))
#plot model 
p1=ggplot(effe_mod_sta, aes(asyn_LM, fit)) + geom_line(size=1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 3,alpha=0.1, colour = "black")+
  labs(color='Plant species')+
  geom_point(data = data_plant, aes(x =  asyn_LM, y = cv_1_visitation, color=Plant_gen_sp), size=3)+
  theme_classic ()+theme(panel.border = element_rect(colour = "black", fill=NA))+
  labs(x = "Asynchrony of pollinator",y="Stability of visitation rate")


#########################################3
# out c.crispus el pozo = valor de estabilidad en tasa de visita muy alto
# ver si cambia el modelo

data_sin_out=data_plant %>%
  filter(!(Site_ID=="El_pozo" & Plant_gen_sp=="Cistus crispus")) 


glm.nb1_out= glmer.nb (cv_1_visitation ~ S_total + (1|Site_ID) + (1|Plant_gen_sp), data = data_sin_out)
summary(glm.nb1_out)

glm.nb2_out= glmer.nb (cv_1_visitation ~ asyn_LM + (1|Site_ID) + (1|Plant_gen_sp), data = data_sin_out)
summary(glm.nb2_out)


#getting effects for asyncLM 
effe_mod_nb2_out <-data.frame( effect("asyn_LM", glm.nb2_out, se = TRUE))
#plot model 
ggplot(effe_mod_nb2_out, aes(asyn_LM, fit)) + geom_line(size=1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 3,alpha=0.1, colour = "black")+
  labs(color='Plant species')+
  geom_point(data = data_sin_out, aes(x =  asyn_LM, y = cv_1_visitation, color=Plant_gen_sp), size=3)+
  theme_classic ()+theme(panel.border = element_rect(colour = "black", fill=NA))+
  labs(x = "Asynchrony of pollinator",y="Stability of visitation rate")




##################
glm_bi= glm(asyn_LM ~ S_total  , data = data_plant, family=binomial)
summary(glm_bi)
simulationOutput <- simulateResiduals(fittedModel = glm_bi, plot = F)
plot(simulationOutput)
ranef(glm_bi)


mod_rich= lmer (asyn_LM ~ S_total +  (1|Site_ID),data = data_plant)
ranef(mod_rich)
summary(mod_rich)
simulationOutput <- simulateResiduals(fittedModel = mod_rich, plot = F)
plot(simulationOutput)
check_model(mod_rich)
