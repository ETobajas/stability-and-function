
library(tidyverse)
library(lme4)
library(effects)
library(patchwork)


# stability data for five plant species present at least in five sites
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


#model fruit stability ~ visitation rate stability
mod2_sta= lmer(cv_1_fruit ~ cv_1_visitation + (1|Plant_gen_sp)+(1|Site_ID), data = data_plant)

#getting effects 
effe3_mod_sta <-data.frame( effect("cv_1_visitation", mod2_sta, se = TRUE))


#plot model 
p_mod2=ggplot(effe3_mod_sta, aes(cv_1_visitation, fit)) + geom_line(size=1)+
  geom_ribbon(aes(ymin = lower, ymax = upper),linetype = 3, alpha=0.2, colour = "black")+
  geom_point(data = data_plant, aes(x =  cv_1_visitation, y = cv_1_fruit, color=Plant_gen_sp),alpha=0.5, size=3)+
  labs(color='Plant species')+
  scale_colour_brewer(palette = "Set1")+
  labs(y = "Stability of fruit proportion",x="Stability of visitation rate")+
  theme_bw ()+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+ theme(legend.text = element_text(face="italic"))





# model seed stability ~ visitation rate stability
mod3_sta= lmer(cv_1_seed ~ cv_1_visitation + (1|Plant_gen_sp)+(1|Site_ID), data = data_plant)


#getting effects 
effe4_mod_sta <-data.frame( effect("cv_1_visitation", mod3_sta, se = TRUE))


#plot model 
p_mod3=ggplot(effe4_mod_sta, aes(cv_1_visitation, fit)) + geom_line(size=1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 3, alpha=0.2, colour = "black")+
  labs(color='Plant species')+
  geom_point(data = data_plant, aes(x =  cv_1_visitation, y = cv_1_seed, color=Plant_gen_sp), alpha=0.5,size=3)+
  scale_colour_brewer(palette = "Set1")+
  labs(y = "Stability of seed number",x="Stability of visitation rate")+
  theme_bw ()+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+ theme(legend.text = element_text(face="italic"))



#join plots
p_plant=p_mod2+p_mod3+ 
  plot_annotation(tag_levels = "A")& 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(hjust = -0.3, vjust = 0.2,size=15))& 
  theme(legend.position = "bottom",legend.background = element_blank(),legend.box.background = element_rect(colour = "black"))
p_plant+ plot_layout(guides = "collect")

