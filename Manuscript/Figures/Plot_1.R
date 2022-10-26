
library(tidyverse)
library(lme4)
library(effects)
library(patchwork)



# stability data for five plant species present at least in five sites
# C.crispu, C. ladanifer, C. salviifolius, H.halimifolium and L. pedunculata

data_plant<- read.csv("Data/Stability_fivespecies.csv")
data_plant<-data_plant[,-1]


# Replace inf with NA
data_plant[sapply(data_plant, is.infinite)] <- NA

# We can express asynchrony as 1 - φ, 
#where φ is Loreau and de Mazancourt’s (2008)
data_plant$asynchrony= 1-data_plant$syncLM


#model 
mod1_sta= lmer (cv_1_visitation ~ S_total + asynchrony + (1|Site_ID) + (1|Plant_gen_sp), data = data_plant)



#getting effects for asynchrony
effe_mod_sta <-data.frame( effect("asynchrony", mod1_sta, se = TRUE))


#plot model of asynchrony
p1=ggplot(effe_mod_sta, aes(asynchrony, fit)) + geom_line(size=1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 3,alpha=0.1, colour = "black")+
  scale_y_continuous(limits = c(-0.14, 6))+ labs(color='Plant species')+
  geom_point(data = data_plant, aes(x =  asynchrony, y = cv_1_visitation, color=Plant_gen_sp), size=3)+
  theme_classic ()+theme(panel.border = element_rect(colour = "black", fill=NA))+
  labs(x = "Asynchrony of pollinator",y="Stability of visitation rate")+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+ theme(legend.text = element_text(face="italic"))



#getting effects for richness
effe2_mod_sta <-data.frame( effect("S_total", mod1_sta, se = TRUE))



#plot model of richness
p2=ggplot(effe2_mod_sta, aes(S_total, fit)) + geom_line(size=1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 3, alpha=0.1, colour = "black")+
  scale_y_continuous(limits = c(-0.14, 6))+  labs(color='Plant species')+
  geom_point(data = data_plant, aes(x =  S_total, y = cv_1_visitation, color=Plant_gen_sp), size=3)+
  theme_classic ()+theme(panel.border = element_rect(colour = "black", fill=NA))+
  labs(x = "Richness of pollinator",y="") +
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+ theme(legend.text = element_text(face="italic"))
  



#join plots
p_cv_visit=p1+p2+ 
  plot_annotation(tag_levels = "A")& 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(hjust = -0.5, size=15))& 
  theme(legend.position = "bottom",legend.background = element_blank(),legend.box.background = element_rect(colour = "black"))
p_cv_visit+ plot_layout(guides = "collect")
