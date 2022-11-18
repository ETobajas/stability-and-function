
library(tidyverse)
library(lme4)
library(effects)
library(patchwork)


# staility of visitation rate and richness and asynchrony
# plant species present at least in five sites
# C.crispu, C. ladanifer, C. salviifolius, H.halimifolium and L. pedunculata

# stability data 
data_plant<- read.csv("Data/Stability.csv")
data_plant<-data_plant[,-1]


# eliminar plant species 
sp_out2=data_plant %>%
  group_by(Plant_gen_sp) %>%
  summarise(Unique_Id = n_distinct(Site_ID)) %>%
  filter(Unique_Id <= 4) %>%
  select(Plant_gen_sp) %>% pull(Plant_gen_sp)

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


#model 
mod1_sta= lmer (cv_1_visitation ~  asyn_LM + (1|Site_ID) + (1|Plant_gen_sp), data = data_plant)



#getting effects for asynchrony
effe_mod_sta <-data.frame( effect("asyn_LM", mod1_sta, se = TRUE))


#plot model of asynchrony
p1=ggplot(effe_mod_sta, aes(asyn_LM, fit)) + geom_line(size=1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 3,alpha=0.2, colour = "black")+
  scale_y_continuous(limits = c(-2.5, 9.5))+labs(color='Plant species')+
  geom_point(data = data_plant, aes(x =  asyn_LM, y = cv_1_visitation, color=Plant_gen_sp), alpha=0.5,size=3)+
  scale_colour_brewer(palette = "Set1")+
   theme_bw ()+
  labs(x = "Asynchrony of pollinator",y="Stability of visitation rate")+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+ theme(legend.text = element_text(face="italic"))



#model richness
mod1.2_sta= lmer (cv_1_visitation ~ S_total + (1|Site_ID) + (1|Plant_gen_sp), data = data_plant)


#getting effects for richness
effe2_mod_sta <-data.frame( effect("S_total", mod1.2_sta, se = TRUE))

#plot model of richness
p2=ggplot(effe2_mod_sta, aes(S_total, fit)) + geom_line(size=1,linetype = "dashed")+
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 3, alpha=0.1, colour = "black")+
  scale_y_continuous(limits = c(-2.5, 9.5))+  labs(color='Plant species')+
  geom_point(data = data_plant, aes(x =  S_total, y = cv_1_visitation, color=Plant_gen_sp), alpha=0.5,size=3)+
  scale_colour_brewer(palette = "Set1")+
  theme_bw ()+
  labs(x = "Richness of pollinator",y="Stability of visitation rate") +
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+ theme(legend.text = element_text(face="italic"))
  


# relationship between richness and asynchrony
mod_rich_asyn= lmer (asyn_LM ~ S_total +  (1|Site_ID) + (1|Plant_gen_sp), data = data_plant)

#getting effects for richness
effe_rich_mod_sta <-data.frame( effect("S_total", mod_rich_asyn, se = TRUE))
#plot model 
p1.2=ggplot(effe_rich_mod_sta, aes(S_total, fit)) + geom_line(size=1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 3, alpha=0.2, colour = "black")+
  labs(color='Plant species')+
  coord_cartesian(ylim=c(0,1))+
  geom_point(data = data_plant, aes(x =  S_total, y = asyn_LM, color=Plant_gen_sp), alpha=0.5,size=3)+
  scale_colour_brewer(palette = "Set1")+
  theme_bw ()+
  labs(x = "Richness of pollinator",y="Asynchrony of pollinator")+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+ theme(legend.text = element_text(face="italic"))
  


#join plots
p_cv_visit=p1+p2+ p1.2+
  plot_annotation(tag_levels = "A")& 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(hjust = -0.1, size=15))& 
  theme(legend.position = "bottom",legend.background = element_blank(),legend.box.background = element_rect(colour = "black"))
p_cv_visit+ plot_layout(guides = "collect")
