library(tidyverse)


# Datos estacion meteorologia Almonte
met<- read_delim("Data/Almonte.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
str(met)

# separate column date
met=met %>% separate(FECHA, c("dia", "mes", "año"), sep = "/")

#change data types
met$Hu10Precip <- as.numeric(met$Hu10Precip)
met$Hu10Rad <- as.numeric(met$Hu10Rad)
met$Hu10HumMed <- as.numeric(met$Hu10HumMed)
met$Hu10TMed <- as.numeric(met$Hu10TMed)

str(met)


#remove june, july and august. 

met=met %>% filter(!mes %in% c("06","07", "08"))


# new column
#data for 2015 is from sept 2014 to may 2015. 
#the same for the rest year
met=met %>%
  mutate(new_year = case_when(mes %in% c(01,02,03,04,05) | año %in% c(19) ~ 2019,
                              mes%in% c("09","10","11","12") & año %in% c(18) ~ 2019,
                              mes %in% c(01,02,03,04,05) | año %in% c(18) ~ 2018,
                              mes%in% c("09","10","11","12") & año %in% c(17) ~ 2018,
                              mes %in% c(01,02,03,04,05) | año %in% c(17) ~ 2017,
                              mes%in% c("09","10","11","12") & año %in% c(16) ~ 2017,
                              mes %in% c(01,02,03,04,05) | año %in% c(16) ~ 2016,
                              mes%in% c("09","10","11","12") & año %in% c(15) ~ 2016,
                              mes %in% c(01,02,03,04,05) | año %in% c(15) ~ 2015,
                              mes%in% c("09","10","11","12") & año %in% c(14) ~ 2015,
                              TRUE~0))


met$new_year <- as.factor(met$new_year)


#box plot precipitacion 
ggplot(data = met, aes(x = new_year, y =  Hu10Precip)) + 
  geom_boxplot ()


# plot precipitacion media mensual por años 
met %>%
  group_by(mes, new_year) %>%
  summarise(mean_precip = mean(Hu10Precip, na.rm = TRUE)) %>%
  mutate(mes = factor(mes,c("09","10","11","12","01","02","03", "04", "05"))) %>%
   ggplot(aes(x=mes,
           y = mean_precip,group = 1)) +
  geom_point()+
  geom_line()+
  facet_grid(new_year~.)



#box plot radiacion 
ggplot(data = met, aes(x = new_year, y =  Hu10Rad)) + 
  geom_boxplot ()


# plot radiacion media mensual por años 
met %>%
  group_by(mes, new_year) %>%
  summarise(mean_rad = mean(Hu10Rad, na.rm = TRUE)) %>%
  mutate(mes = factor(mes,c("09","10","11","12","01","02","03", "04", "05"))) %>%
  ggplot(aes(x=mes,
             y = mean_rad,group = 1)) +
  geom_point()+
  geom_line()+
  facet_grid(new_year~.)


#box plot humedad media
ggplot(data = met, aes(x = new_year, y =  Hu10HumMed)) + 
  geom_boxplot ()


# plot humedad media mensual por años 
met %>%
  group_by(mes, new_year) %>%
  summarise(mean_hum = mean(Hu10HumMed, na.rm = TRUE)) %>%
  mutate(mes = factor(mes,c("09","10","11","12","01","02","03", "04", "05"))) %>%
  ggplot(aes(x=mes,
             y = mean_hum,group = 1)) +
  geom_point()+
  geom_line()+
  facet_grid(new_year~.)


#box plot temperatura media
ggplot(data = met, aes(x = new_year, y =  Hu10TMed)) + 
  geom_boxplot ()


# plot temperatura media mensual por años 
met %>%
  group_by(mes, new_year) %>%
  summarise(mean_temp = mean(Hu10TMed, na.rm = TRUE)) %>%
  mutate(mes = factor(mes,c("09","10","11","12","01","02","03", "04", "05"))) %>%
  ggplot(aes(x=mes,
             y = mean_temp,group = 1)) +
  geom_point()+
  geom_line()+
  facet_grid(new_year~.)
