

# pollinator richness per year
ggplot(data = Pollinator_Plant, aes(x = Year, y = S_total)) + 
    geom_boxplot ()


ave_S <- Pollinator_Plant%>% 
  group_by(Year) %>% 
  summarize(mean_S = mean(S_total, na.rm=T),
            standarderror = sd(S_total, na.rm = TRUE)/sqrt(n()))




# visitation rate per year
ggplot(data = Pollinator_Plant, aes(x = Year, y = visitatio_rate)) + 
  geom_boxplot ()


ave_VR <- Pollinator_Plant%>% 
  group_by(Year) %>% 
  summarize(mean_VR = mean(visitatio_rate, na.rm=T),
            standarderror = sd(visitatio_rate, na.rm = TRUE)/sqrt(n()))


# FOCAL

focal<- read.csv("Data/focal.csv") 
focal<-focal[,-1]


focal <- focal[!(focal$Pollinator_gen_sp == "NA NA"),] #eliminar polinizadores NA NA

#Data with species in more than 1 site
focal = focal %>% filter(!Plant_gen_sp %in% spp_out)

focal$Pollinator_gen_sp <- sub(" ", "_", focal$Pollinator_gen_sp )

levels(factor(focal$Plant_gen_sp))

focal<-subset(focal, Plant_gen_sp != 'Ulex australis') #remove ulex


# temperature mean per year during sampling
focal$Year <- as.factor(focal$Year)

ave_tem <- focal%>% 
  group_by(Year) %>% 
  summarize(mean_tem = mean(Temperature, na.rm=T),
            standarderror = sd(Temperature, na.rm = TRUE)/sqrt(n()))



ggplot(data = focal, aes(x = Year, y = Temperature)) + 
  geom_boxplot ()



# humidity mean per year during sampling
ave_hu <- focal%>% 
  group_by(Year) %>% 
  summarize(mean_hu= mean(Humidity, na.rm=T),
            standarderror = sd(Humidity, na.rm = TRUE)/sqrt(n()))


ggplot(data = focal, aes(x = Year, y = Humidity)) + 
  geom_boxplot ()





