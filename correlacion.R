
getwd()

library(readr)
library(dplyr)
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
library(ggplot2)
library(nlme)
library(lme4)
#install.packages("AICcmodavg")
library(AICcmodavg)
library(MASS)
library(psych)
#install.packages("corrplot")
library(corrplot)
#install.packages("DHARMa")
library(DHARMa)


total <- read.csv("Data/Frequency-Fruit.csv")
View(total)
str(total)
total<-total[,-1]

total$Site_ID[total$Site_ID=="La Cu\xf1a"] <- "La cuna" #problemas con la ñ

total$Year= factor(total$Year)

# removed lines with all=0 (5 obser)
total<-total[!apply(total[,5:16] == 0, 1, all), ]

#total <- total[!(total$Frequency == 0),]



par(mfrow=c(3,3))
boxplot(total$Fruit_proportion)
boxplot(Fruit_proportion~Year, data = total)
boxplot(Fruit_proportion~Site_ID, data = total)
boxplot(total$seed_number)
boxplot(seed_number~Year, data = total)
boxplot(seed_number~Site_ID, data = total)
boxplot(total$Seed_weight)
boxplot(Seed_weight~Year, data = total)
boxplot(Seed_weight~Site_ID, data = total)
dev.off()


require(car)
#scatterplot(Fruit_proportion ~ Frequency, reg.line = lm, smooth = F, data=total)
scatterplot(Fruit_proportion ~ Frequency|Plant_sp, reg.line = lm, smooth = F, data=total)


require("lattice")
xyplot(Fruit_proportion ~ Frequency, data = total, type = c("p", "r"))
xyplot(Fruit_proportion ~ Frequency|Plant_sp, data = total, type = c("p", "r"))
xyplot(Fruit_proportion ~ richness|Plant_sp, data = total, type = c("p", "r"))

xyplot(seed_number ~ Frequency, data = total, type = c("p", "r"))
xyplot(seed_number ~ Frequency|Plant_sp, data = total, type = c("p", "r"))
xyplot(seed_number ~ richness|Plant_sp, data = total, type = c("p", "r"))

xyplot(Seed_weight ~ Frequency|Plant_sp, data = total, type = c("p", "r"))
xyplot(Seed_weight ~ richness|Plant_sp, data = total, type = c("p", "r"))

# CORRELATION
total1<-total[,5:16]
plot(total1)

total1 %>% cor(method="spearman") %>% round(digits=2) -> total_cor
corrplot(total_cor, type="upper",addCoef.col = "black",insig="blank", tl.col="black",tl.srt=45)

chart.Correlation(total1,method = "spearman",exact=FALSE)


##################################
#- per plant species

#-Rosmarinus officinalis (114 observations)
r.m = total %>% filter(Plant_sp %in% "Rosmarinus officinalis")
r.m
str(r.m)
levels(factor(r.m$Year))
levels(factor(r.m$Site_ID))

par(mfrow=c(1,3))
plot(r.m$Fruit_proportion,col=r.m$Year,pch = 19)
plot(r.m$seed_number,col=r.m$Year,pch = 19)
plot(r.m$Seed_weight,col=r.m$Year,pch = 19)

r.m$Site_ID = as.factor(r.m$Site_ID)
par(mfrow=c(1,3))
plot(r.m$Fruit_proportion,col=r.m$Site_ID,pch = 19)
plot(r.m$seed_number,col=r.m$Site_ID,pch = 19)
plot(r.m$Seed_weight,col=r.m$Site_ID,pch = 19)

par(mfrow=c(3,3))
boxplot(r.m$Fruit_proportion)
boxplot(Fruit_proportion~Year, data = r.m)
boxplot(Fruit_proportion~Site_ID, data = r.m)
boxplot(r.m$seed_number)
boxplot(seed_number~Year, data = r.m)
boxplot(seed_number~Site_ID, data = r.m)
boxplot(r.m$Seed_weight)
boxplot(Seed_weight~Year, data = r.m)
boxplot(Seed_weight~Site_ID, data = r.m)

dev.off()

table(r.m$Fruit_proportion)
table(r.m$seed_number)
table(r.m$ Seed_weight)

#correlacion con corrplot
r.m1<-r.m[,5:16]
r.m1 %>% cor(method="spearman") %>% round(digits=2) -> r.m_cor
corrplot(r.m_cor, type="upper",addCoef.col = "black",insig="blank", tl.col="black",tl.srt=45)

chart.Correlation(r.m1,method = "spearman",exact=FALSE)

#-Cistus salviifolius (131 observations)
c.s = total %>% filter(Plant_sp %in% "Cistus salviifolius")
c.s
str(c.s)
levels(factor(c.s$Year))
levels(factor(c.s$Site_ID))

par(mfrow=c(1,3))
plot(c.s$Fruit_proportion,col=c.s$Year,pch = 19)
plot(c.s$seed_number,col=c.s$Year,pch = 19)
plot(c.s$Seed_weight,col=c.s$Year,pch = 19)

par(mfrow=c(1,3))
c.s$Site_ID = as.factor(c.s$Site_ID)
plot(c.s$Fruit_proportion,col=c.s$Site_ID,pch = 19)
plot(c.s$seed_number,col=c.s$Site_ID,pch = 19)
plot(c.s$Seed_weight,col=c.s$Site_ID,pch = 19)

par(mfrow=c(3,3))
boxplot(c.s$Fruit_proportion)
boxplot(Fruit_proportion~Year, data = c.s)
boxplot(Fruit_proportion~Site_ID, data = c.s)
boxplot(c.s$seed_number)
boxplot(seed_number~Year, data = c.s)
boxplot(seed_number~Site_ID, data = c.s)
boxplot(c.s$Seed_weight)
boxplot(Seed_weight~Year, data = c.s)
boxplot(Seed_weight~Site_ID, data = c.s)

dev.off()


table( c.s$Fruit_proportion,c.s$Frequency)
table( c.s$seed_number)
table( c.s$ Seed_weight)

#correlacion con corrplot
c.s1<-c.s[,5:16]
c.s1 %>% cor(method="spearman") %>% round(digits=2) -> c.s_cor
corrplot(c.s_cor, type="upper",addCoef.col = "black",insig="blank", tl.col="black",tl.srt=45)

chart.Correlation(c.s1,method = "spearman",exact=FALSE)

#-Cistus crispus (106 observations)
c.c = total %>% filter(Plant_sp %in% "Cistus crispus")
c.c
str(c.c)
levels(factor(c.c$Year))
levels(factor(c.c$Site_ID))

par(mfrow=c(1,3))
plot(c.c$Fruit_proportion,col=c.c$Year,pch = 19)
plot(c.c$seed_number,col=c.c$Year,pch = 19)
plot(c.c$Seed_weight,col=c.c$Year,pch = 19)

c.c$Site_ID = as.factor(c.c$Site_ID)
par(mfrow=c(1,3))
plot(c.c$Fruit_proportion,col=c.c$Site_ID,pch = 19)
plot(c.c$seed_number,col=c.c$Site_ID,pch = 19)
plot(c.c$Seed_weight,col=c.c$Site_ID,pch = 19)

par(mfrow=c(3,3))
boxplot(c.c$Fruit_proportion)
boxplot(Fruit_proportion~Year, data = c.c)
boxplot(Fruit_proportion~Site_ID, data = c.c)
boxplot(c.c$seed_number)
boxplot(seed_number~Year, data = c.c)
boxplot(seed_number~Site_ID, data = c.c)
boxplot(c.c$Seed_weight)
boxplot(Seed_weight~Year, data = c.c)
boxplot(Seed_weight~Site_ID, data = c.c)

dev.off()


table( c.c$Fruit_proportion)
table( c.c$seed_number)
table( c.c$ Seed_weight)

#correlacion con corrplot
c.c1<-c.c[,5:16]
c.c1 %>% cor(method="spearman") %>% round(digits=2) -> c.c_cor
corrplot(c.c_cor, type="upper",addCoef.col = "black",insig="blank", tl.col="black",tl.srt=45)

chart.Correlation(c.c1,method = "spearman",exact=FALSE)

#-Cistus ladanifer (73 observations)
c.l = total %>% filter(Plant_sp %in% "Cistus ladanifer")
c.l
str(c.l)
levels(factor(c.l$Year))
levels(factor(c.l$Site_ID))

par(mfrow=c(1,3))
plot(c.l$Fruit_proportion,col=c.l$Year,pch = 19)
plot(c.l$seed_number,col=c.l$Year,pch = 19)
plot(c.l$Seed_weight,col=c.l$Year,pch = 19)

par(mfrow=c(1,3))
c.l$Site_ID = as.factor(c.l$Site_ID)
plot(c.l$Fruit_proportion,col=c.l$Site_ID,pch = 19)
plot(c.l$seed_number,col=c.l$Site_ID,pch = 19)
plot(c.l$Seed_weight,col=c.l$Site_ID,pch = 19)

par(mfrow=c(3,3))
boxplot(c.l$Fruit_proportion)
boxplot(Fruit_proportion~Year, data = c.l)
boxplot(Fruit_proportion~Site_ID, data = c.l)
boxplot(c.l$seed_number)
boxplot(seed_number~Year, data = c.l)
boxplot(seed_number~Site_ID, data = c.l)
boxplot(c.l$Seed_weight)
boxplot(Seed_weight~Year, data = c.l)
boxplot(Seed_weight~Site_ID, data = c.l)

dev.off()



table(c.l$Fruit_proportion)
table(c.l$seed_number)
table(c.l$ Seed_weight)

#correlacion con corrplot
c.l1<-c.l[,5:16]
c.l1 %>% cor(method="spearman") %>% round(digits=2) -> c.l_cor
corrplot(c.l_cor, type="upper",addCoef.col = "black",insig="blank", tl.col="black",tl.srt=45)

chart.Correlation(c.l1,method = "spearman",exact=FALSE)

#-Halimium halimifolium (75 observations)
h.h = total %>% filter(Plant_sp %in% "Halimium halimifolium")
h.h
str(h.h)
levels(factor(h.h$Year))
levels(factor(h.h$Site_ID))

par(mfrow=c(1,3))
plot(h.h$Fruit_proportion,col=h.h$Year,pch = 19)
plot(h.h$seed_number,col=h.h$Year,pch = 19)
plot(h.h$Seed_weight,col=h.h$Year,pch = 19)


h.h$Site_ID = as.factor(h.h$Site_ID)
par(mfrow=c(1,3))
plot(h.h$Fruit_proportion,col=h.h$Site_ID,pch = 19)
plot(h.h$seed_number,col=h.h$Site_ID,pch = 19)
plot(h.h$Seed_weight,col=h.h$Site_ID,pch = 19)

par(mfrow=c(3,3))
boxplot(h.h$Fruit_proportion)
boxplot(Fruit_proportion~Year, data = h.h)
boxplot(Fruit_proportion~Site_ID, data = h.h)
boxplot(h.h$seed_number)
boxplot(seed_number~Year, data = h.h)
boxplot(seed_number~Site_ID, data = h.h)
boxplot(h.h$Seed_weight)
boxplot(Seed_weight~Year, data = h.h)
boxplot(Seed_weight~Site_ID, data = h.h)

dev.off()

table(h.h$Fruit_proportion)
table(h.h$seed_number)
table(h.h$ Seed_weight)


#correlacion con corrplot
h.h1<-h.h[,5:16]
h.h1 %>% cor(method="spearman") %>% round(digits=2) -> h.h_cor
corrplot(h.h_cor, type="upper",addCoef.col = "black",insig="blank", tl.col="black",tl.srt=45)

chart.Correlation(h.h1,method = "spearman",exact=FALSE)

#-Halimium commutatum (53 oberv)
h.c = total %>% filter(Plant_sp %in% "Halimium commutatum")
h.c
str(h.c)
levels(factor(h.c$Year))
levels(factor(h.c$Site_ID))

par(mfrow=c(1,3))
plot(h.c$Fruit_proportion,col=h.c$Year,pch = 19)
plot(h.c$seed_number,col=h.c$Year,pch = 19)
plot(h.c$Seed_weight,col=h.c$Year,pch = 19)


h.c$Site_ID = as.factor(h.c$Site_ID)
par(mfrow=c(1,3))
plot(h.c$Fruit_proportion,col=h.c$Site_ID,pch = 19)
plot(h.c$seed_number,col=h.c$Site_ID,pch = 19)
plot(h.c$Seed_weight,col=h.c$Site_ID,pch = 19)

par(mfrow=c(3,3))
boxplot(h.c$Fruit_proportion)
boxplot(Fruit_proportion~Year, data = h.c)
boxplot(Fruit_proportion~Site_ID, data = h.c)
boxplot(h.c$seed_number)
boxplot(seed_number~Year, data = h.c)
boxplot(seed_number~Site_ID, data = h.c)
boxplot(h.c$Seed_weight)
boxplot(Seed_weight~Year, data = h.c)
boxplot(Seed_weight~Site_ID, data = h.c)

dev.off()

table(h.c$Fruit_proportion)
table(h.c$seed_number)
table(h.c$ Seed_weight)


#correlacion con corrplot
h.c1<-h.c[,5:16]
h.c1 %>% cor(method="spearman") %>% round(digits=2) -> h.c_cor
corrplot(h.c_cor, type="upper",addCoef.col = "black",insig="blank", tl.col="black",tl.srt=45)

chart.Correlation(h.c1,method = "spearman",exact=FALSE)


#- Lavandula pedunculata (62 obse)
l.p = total %>% filter(Plant_sp %in% "Lavandula pedunculata")
l.p
str(l.p)
levels(factor(l.p$Year))
levels(factor(l.p$Site_ID))

par(mfrow=c(1,3))
plot(l.p$Fruit_proportion,col=l.p$Year,pch = 19)
plot(l.p$seed_number,col=l.p$Year,pch = 19)
plot(l.p$Seed_weight,col=l.p$Year,pch = 19)


l.p$Site_ID = as.factor(l.p$Site_ID)
par(mfrow=c(1,3))
plot(l.p$Fruit_proportion,col=l.p$Site_ID,pch = 19)
plot(l.p$seed_number,col=l.p$Site_ID,pch = 19)
plot(l.p$Seed_weight,col=l.p$Site_ID,pch = 19)

par(mfrow=c(3,3))
boxplot(l.p$Fruit_proportion)
boxplot(Fruit_proportion~Year, data = l.p)
boxplot(Fruit_proportion~Site_ID, data = l.p)
boxplot(l.p$seed_number)
boxplot(seed_number~Year, data = l.p)
boxplot(seed_number~Site_ID, data = l.p)
boxplot(l.p$Seed_weight)
boxplot(Seed_weight~Year, data = l.p)
boxplot(Seed_weight~Site_ID, data = l.p)

dev.off()

table(l.p$Fruit_proportion)
table(l.p$seed_number)
table(l.p$ Seed_weight)


#correlacion con corrplot
l.p1<-l.p[,5:16]
l.p1 %>% cor(method="spearman") %>% round(digits=2) -> l.p_cor
corrplot(l.p_cor, type="upper",addCoef.col = "black",insig="blank", tl.col="black",tl.srt=45)

chart.Correlation(l.p1,method = "spearman",exact=FALSE)


# Lavandula stoechas (64 obs)
l.s = total %>% filter(Plant_sp %in% "Lavandula stoechas")
l.s
str(l.s)
levels(factor(l.s$Year))
levels(factor(l.s$Site_ID))

par(mfrow=c(1,3))
plot(l.s$Fruit_proportion,col=l.s$Year,pch = 19)
plot(l.s$seed_number,col=l.s$Year,pch = 19)
plot(l.s$Seed_weight,col=l.s$Year,pch = 19)


l.s$Site_ID = as.factor(l.s$Site_ID)
par(mfrow=c(1,3))
plot(l.s$Fruit_proportion,col=l.s$Site_ID,pch = 19)
plot(l.s$seed_number,col=l.s$Site_ID,pch = 19)
plot(l.s$Seed_weight,col=l.s$Site_ID,pch = 19)

par(mfrow=c(3,3))
boxplot(l.s$Fruit_proportion)
boxplot(Fruit_proportion~Year, data = l.s)
boxplot(Fruit_proportion~Site_ID, data = l.s)
boxplot(l.s$seed_number)
boxplot(seed_number~Year, data = l.s)
boxplot(seed_number~Site_ID, data = l.s)
boxplot(l.s$Seed_weight)
boxplot(Seed_weight~Year, data = l.s)
boxplot(Seed_weight~Site_ID, data = l.s)

dev.off()

table(l.s$Fruit_proportion)
table(l.s$seed_number)
table(l.s$ Seed_weight)


#correlacion con corrplot
l.s1<-l.s[,5:16]
l.s1 %>% cor(method="spearman") %>% round(digits=2) -> l.s_cor
corrplot(l.s_cor, type="upper",addCoef.col = "black",insig="blank", tl.col="black",tl.srt=45)

chart.Correlation(l.s1,method = "spearman",exact=FALSE)

#- Asphodelus fistulosus(28 obse)
a.f = total %>% filter(Plant_sp %in% "Asphodelus fistulosus")
a.f
str(a.f)
levels(factor(a.f$Year))
levels(factor(a.f$Site_ID))

par(mfrow=c(1,3))
plot(a.f$Fruit_proportion,col=a.f$Year,pch = 19)
plot(a.f$seed_number,col=a.f$Year,pch = 19)
plot(a.f$Seed_weight,col=a.f$Year,pch = 19)


a.f$Site_ID = as.factor(a.f$Site_ID)
par(mfrow=c(1,3))
plot(a.f$Fruit_proportion,col=a.f$Site_ID,pch = 19)
plot(a.f$seed_number,col=a.f$Site_ID,pch = 19)
plot(a.f$Seed_weight,col=a.f$Site_ID,pch = 19)

par(mfrow=c(3,3))
boxplot(a.f$Fruit_proportion)
boxplot(Fruit_proportion~Year, data = a.f)
boxplot(Fruit_proportion~Site_ID, data = a.f)
boxplot(a.f$seed_number)
boxplot(seed_number~Year, data = a.f)
boxplot(seed_number~Site_ID, data = a.f)
boxplot(a.f$Seed_weight)
boxplot(Seed_weight~Year, data = a.f)
boxplot(Seed_weight~Site_ID, data = a.f)

dev.off()

table(a.f$Fruit_proportion)
table(a.f$seed_number)
table(a.f$ Seed_weight)


#correlacion con corrplot
a.f1<-a.f[,5:16]
a.f1 %>% cor(method="spearman") %>% round(digits=2) -> a.f_cor
corrplot(a.f_cor, type="upper",addCoef.col = "black",insig="blank", tl.col="black",tl.srt=45)

chart.Correlation(a.f1,method = "spearman",exact=FALSE)


#######################
##  Cistus crispus  ##

str(c.c)
attach(c.c)

## Fruit propotion 

par(mfrow=c(2,3))
plot(Frequency,Fruit_proportion,xlab="Pollinator frequency",ylab="Fruit proportion")
plot(richness,Fruit_proportion,xlab="Richness",ylab="Fruit proportion")
plot(richness08,Fruit_proportion,xlab="Richness-80%",ylab="Fruit proportion")
plot(Hym_freq,Fruit_proportion,xlab="Hymenoptera frequency",ylab="Fruit proportion")
plot(Dip_freq,Fruit_proportion,xlab="Diptera frequency",ylab="Fruit proportion")
plot(Coleop_freq,Fruit_proportion,xlab="Coleoptera frequency",ylab="Fruit proportion")

detach(c.c)


# Variables correlacionadas, probar modelos con una variable
# random=Site_ID
scatterplot(Fruit_proportion ~ Frequency | Site_ID, reg.line = lm, smooth = F, data=c.c)
scatterplot(Fruit_proportion ~ richness | Site_ID, reg.line = lm, smooth = F, data=c.c)

#frequency
model1_c.c <- glmer(cbind(Fruit_Yes, Fruit_No) ~ Frequency + (1|Site_ID), data = c.c, family=binomial)
summary(model1_c.c)

simulationOutput <- simulateResiduals(fittedModel = model1_c.c)
plot(simulationOutput)


#richness
model2_c.c <- glmer(cbind(Fruit_Yes, Fruit_No) ~ richness + (1|Site_ID), data = c.c, family=binomial)
summary(model2_c.c)

simulationOutput2 <- simulateResiduals(fittedModel = model2_c.c)
plot(simulationOutput2)


#frequency hymenopter, diptera, coleoptera
model3_c.c <- glmer(cbind(Fruit_Yes, Fruit_No) ~ Hym_freq+Dip_freq+ Coleop_freq+ (1|Site_ID), data = c.c, family=binomial)
summary(model3_c.c)

simulationOutput3 <- simulateResiduals(fittedModel = model3_c.c)
plot(simulationOutput3)

#richness 80%
model4_c.c <- glmer(cbind(Fruit_Yes, Fruit_No) ~ richness08 + (1|Site_ID), data = c.c, family=binomial)
summary(model4_c.c)

simulationOutput4 <- simulateResiduals(fittedModel = model4_c.c)
plot(simulationOutput4)

#+++++++++++
## random= Year ##
scatterplot(Fruit_proportion ~ Frequency | Year, reg.line = lm, smooth = F, data=c.c)
scatterplot(Fruit_proportion ~ richness | Year, reg.line = lm, smooth = F, data=c.c)

#frequency
model1.1_c.c <- glmer(cbind(Fruit_Yes, Fruit_No) ~ Frequency + (1|Year), data = c.c, family=binomial)
summary(model1.1_c.c)

simulationOutputY <- simulateResiduals(fittedModel = model1.1_c.c)
plot(simulationOutputY)

#richness
model2.1_c.c <- glmer(cbind(Fruit_Yes, Fruit_No) ~ richness + (1|Year), data = c.c, family=binomial)
summary(model2.1_c.c)

simulationOutput2Y <- simulateResiduals(fittedModel = model2.1_c.c)
plot(simulationOutput2Y)

#frequency hymenopter, diptera, coleoptera
model3.1_c.c <- glmer(cbind(Fruit_Yes, Fruit_No) ~ Hym_freq+Dip_freq+ Coleop_freq+ (1|Year), data = c.c, family=binomial)
summary(model3.1_c.c)

simulationOutput3Y <- simulateResiduals(fittedModel = model3.1_c.c)
plot(simulationOutput3Y)

#richness 80%
model4.1_c.c <- glmer(cbind(Fruit_Yes, Fruit_No) ~ richness08 + (1|Year), data = c.c, family=binomial)
summary(model4.1_c.c)

simulationOutput4Y <- simulateResiduals(fittedModel = model4.1_c.c)
plot(simulationOutput4Y)

#++++++++++++
# sin random factor
mod_c.c<- glm(cbind(Fruit_Yes, Fruit_No) ~ Frequency, family=binomial, data = c.c)
summary(mod_c.c)

simulationOutputmod <- simulateResiduals(fittedModel = mod_c.c)
#Error in simulateResiduals(fittedModel = mod_c.c)
plot(mod_c.c)


mod2_c.c <- glm(cbind(Fruit_Yes, Fruit_No) ~ richness, data = c.c, family=binomial)
summary(mod2_c.c)

simulationOutputmod2 <- simulateResiduals(fittedModel = mod2_c.c)
#Error in simulateResiduals(fittedModel = mod2_c.c) 
plot(mod2_c.c)


mod3_c.c <- glm(cbind(Fruit_Yes, Fruit_No) ~ Hym_freq+Dip_freq+ Coleop_freq, data = c.c, family=binomial)
summary(mod3_c.c)
plot(mod3_c.c)


mod4_c.c <- glm(cbind(Fruit_Yes, Fruit_No) ~ richness08, data = c.c, family=binomial)
summary(mod4_c.c)
plot(mod4_c.c)



## Seed number ##
attach(c.c)

par(mfrow=c(2,3))
plot(Frequency,seed_number,xlab="Pollinator frequency",ylab="Seed number")
plot(richness,seed_number,xlab="Richness",ylab="Seed number")
plot(richness08,seed_number,xlab="Richness-80%",ylab="Seed number")
plot(Hym_freq,seed_number,xlab="Hymenoptera frequency",ylab="Seed number")
plot(Dip_freq,seed_number,xlab="Diptera frequency",ylab="Seed number")
plot(Coleop_freq,seed_number,xlab="Coleoptera frequency",ylab="Seed number")

detach(c.c)

# Variables correlacionadas, probar modelos con una variable
# random=Site_ID
#frequency
model1seed_c.c <- lme(seed_number ~ Frequency,random= ~ 1|Site_ID,data = c.c )
summary(model1seed_c.c)

plot(model1seed_c.c)
resi_seed <- residuals(model1seed_c.c)
qqnorm(resi_seed)
qqline(resi_seed, lty=3, col="red")

#richness
model2seed_c.c <- lme(seed_number ~ richness,random= ~ 1|Site_ID,data = c.c )
summary(model2seed_c.c)

plot(model2seed_c.c)
resi_seed2 <- residuals(model2seed_c.c)
qqnorm(resi_seed2)
qqline(resi_seed2, lty=3, col="red")

#frequency hymenopter, diptera, coleoptera
model3seed_c.c <- lme(seed_number ~ Hym_freq+Dip_freq+Coleop_freq,random= ~ 1|Site_ID,data = c.c )
summary(model3seed_c.c)

plot(model3seed_c.c)
resi_seed3 <- residuals(model3seed_c.c)
qqnorm(resi_seed3)
qqline(resi_seed3, lty=3, col="red")

model3.1seed_c.c <- lme(seed_number ~ Hym_freq,random= ~ 1|Site_ID,data = c.c )
summary(model3.1seed_c.c)

plot(model3.1seed_c.c)
resi_seed3.1 <- residuals(model3.1seed_c.c)
qqnorm(resi_seed3.1)
qqline(resi_seed3.1, lty=3, col="red")

#richness 80%
model4seed_c.c <- lme(seed_number ~ richness08,random= ~ 1|Site_ID,data = c.c )
summary(model4seed_c.c)

plot(model4seed_c.c)
resi_seed4 <- residuals(model4seed_c.c)
qqnorm(resi_seed4)
qqline(resi_seed4, lty=3, col="red")



# probar transformacion
model1s.1_c.c <- lme((seed_number)^2 ~ Frequency, random= ~ 1|Site_ID, data=c.c)
summary(model1s.1_c.c)
plot(model1s.1_c.c)

resi3 <- residuals(model1s.1_c.c)
qqnorm(resi3)
qqline(resi3, lty=3, col="red")

#+++++++++
# random= Year
#frequency
model1.1seed_c.c <- lme(seed_number ~ Frequency,random= ~ 1|Year,data = c.c )
summary(model1.1seed_c.c)

plot(model1.1seed_c.c)
resi_seed1 <- residuals(model1.1seed_c.c)
qqnorm(resi_seed1)
qqline(resi_seed1, lty=3, col="red")

#richness
model2.1seed_c.c <- lme(seed_number ~ richness,random= ~ 1|Year,data = c.c )
summary(model2.1seed_c.c)

plot(model2.1seed_c.c)
resi_seed2.1 <- residuals(model2.1seed_c.c)
qqnorm(resi_seed2.1)
qqline(resi_seed2.1, lty=3, col="red")

#frequency hymenopter, diptera, coleoptera
model3.1seed_c.c <- lme(seed_number ~ Hym_freq+Dip_freq+Coleop_freq,random= ~ 1|Year,data = c.c )
summary(model3.1seed_c.c)

plot(model3.1seed_c.c)
resi_seed3.1 <- residuals(model3.1seed_c.c)
qqnorm(resi_seed3.1)
qqline(resi_seed3.1, lty=3, col="red")


#richness 80%
model4.1seed_c.c <- lme(seed_number ~ richness08,random= ~ 1|Year,data = c.c )
summary(model4.1seed_c.c)

plot(model4.1seed_c.c)
resi_seed4.1 <- residuals(model4.1seed_c.c)
qqnorm(resi_seed4.1)
qqline(resi_seed4.1, lty=3, col="red")

#+++++++++
# sin random factor
#frequency
model1.2seed_c.c <- lm(seed_number ~ Frequency,data = c.c )
summary(model1.2seed_c.c)
AIC(model1.2seed_c.c)

plot(model1.2seed_c.c)

#richness
model2.2seed_c.c <- lm(seed_number ~ richness,data = c.c )
summary(model2.2seed_c.c)
plot(model2.2seed_c.c)

#frequency hymenopter, diptera, coleoptera
model3.2seed_c.c <- lm(seed_number ~ Hym_freq+Dip_freq+Coleop_freq,data = c.c )
summary(model3.2seed_c.c)
AIC(model3.2seed_c.c)
plot(model3.2seed_c.c)

#richness 80%
model4.2seed_c.c <- lm(seed_number ~ richness08,data = c.c )
summary(model4.2seed_c.c)
plot(model4.2seed_c.c)


## Seed weight ##
attach(c.c)

par(mfrow=c(2,3))
plot(Frequency,Seed_weight,xlab="Pollinator frequency",ylab="Seed_weight")
plot(richness,Seed_weight,xlab="Richness",ylab="Seed_weight")
plot(richness08,Seed_weight,xlab="Richness-80%",ylab="Seed_weight")
plot(Hym_freq,Seed_weight,xlab="Hymenoptera frequency",ylab="Seed_weight")
plot(Dip_freq,Seed_weight,xlab="Diptera frequency",ylab="Seed_weight")
plot(Coleop_freq,Seed_weight,xlab="Coleoptera frequency",ylab="Seed_weight")

detach(c.c)

# Variables correlacionadas, probar modelos con una variable
# random=Site_ID
#frequency
model1weigth_c.c <- lme(Seed_weight ~ Frequency,random= ~ 1|Site_ID,data = c.c )
summary(model1weigth_c.c)

plot(model1weigth_c.c)
resi_seed_w <- residuals(model1weigth_c.c)
qqnorm(resi_seed_w)
qqline(resi_seed_w, lty=3, col="red")

#richness
model2weigth_c.c <- lme(Seed_weight ~ richness,random= ~ 1|Site_ID,data = c.c )
summary(model2weigth_c.c)

plot(model2weigth_c.c)
resi_seed2_w <- residuals(model2weigth_c.c)
qqnorm(resi_seed2_w)
qqline(resi_seed2_w, lty=3, col="red")

#frequency hymenopter, diptera, coleoptera
model3weigth_c.c <- lme(Seed_weight ~ Hym_freq+Dip_freq+Coleop_freq,random= ~ 1|Site_ID,data = c.c )
summary(model3weigth_c.c)

plot(model3weigth_c.c)
resi_seed3_w <- residuals(model3weigth_c.c)
qqnorm(resi_seed3_w)
qqline(resi_seed3_w, lty=3, col="red")

#richness 80%
model4weigth_c.c <- lme(Seed_weight ~ richness08,random= ~ 1|Site_ID,data = c.c )
summary(model4weigth_c.c)

plot(model4weigth_c.c)
resi_seed4_w <- residuals(model4weigth_c.c)
qqnorm(resi_seed4_w)
qqline(resi_seed4_w, lty=3, col="red")

#++++++++++++
# random= Year
#frequency
model1.1weight_c.c <- lme(Seed_weight ~ Frequency,random= ~ 1|Year,data = c.c )
summary(model1.1weight_c.c)

plot(model1.1weight_c.c)
resi_seed1.1_w <- residuals(model1.1weight_c.c)
qqnorm(resi_seed1.1_w)
qqline(resi_seed1.1_w, lty=3, col="red")

#richness
model2.1weight_c.c <- lme(Seed_weight ~ richness,random= ~ 1|Year,data = c.c )
summary(model2.1weight_c.c)

plot(model2.1weight_c.c)
resi_seed2.1_w <- residuals(model2.1weight_c.c)
qqnorm(resi_seed2.1_w)
qqline(resi_seed2.1_w, lty=3, col="red")

#frequency hymenopter, diptera, coleoptera
model3.1weight_c.c <- lme(Seed_weight ~ Hym_freq+Dip_freq+Coleop_freq,random= ~ 1|Year,data = c.c )
summary(model3.1weight_c.c)

plot(model3.1weight_c.c)
resi_seed3.1_w <- residuals(model3.1weight_c.c)
qqnorm(resi_seed3.1_w)
qqline(resi_seed3.1_w, lty=3, col="red")


#richness 80%
model4.1weight_c.c <- lme(Seed_weight ~ richness08,random= ~ 1|Year,data = c.c )
summary(model4.1weight_c.c)

plot(model4.1weight_c.c)
resi_seed4.1_w <- residuals(model4.1weight_c.c)
qqnorm(resi_seed4.1_w)
qqline(resi_seed4.1_w, lty=3, col="red")


#++++++++++++
# sin random factor
#frequency
model1.2weight_c.c <- lm(Seed_weight ~ Frequency,data = c.c )
summary(model1.2weight_c.c)
AIC(model1.2weight_c.c)

plot(model1.2weight_c.c)

#richness
model2.2weight_c.c <- lm(Seed_weight ~ richness,data = c.c )
summary(model2.2weight_c.c)
plot(model2.2weight_c.c)

#frequency hymenopter, diptera, coleoptera
model3.2weight_c.c <- lm(Seed_weight ~ Hym_freq+Dip_freq+Coleop_freq,data = c.c )
summary(model3.2weight_c.c)
AIC(model3.2weight_c.c)
plot(model3.2weight_c.c)

#richness 80%
model4.2weight_c.c <- lm(Seed_weight ~ richness08,data = c.c )
summary(model4.2weight_c.c)
plot(model4.2weight_c.c)


#######################
##  Cistus salviifolius  ##

attach(c.s)

## Fruit propotion 

par(mfrow=c(2,3))
plot(Frequency,Fruit_proportion,xlab="Pollinator frequency",ylab="Fruit proportion")
plot(richness,Fruit_proportion,xlab="Richness",ylab="Fruit proportion")
plot(richness08,Fruit_proportion,xlab="Richness-80%",ylab="Fruit proportion")
plot(Hym_freq,Fruit_proportion,xlab="Hymenoptera frequency",ylab="Fruit proportion")
plot(Dip_freq,Fruit_proportion,xlab="Diptera frequency",ylab="Fruit proportion")
plot(Coleop_freq,Fruit_proportion,xlab="Coleoptera frequency",ylab="Fruit proportion")

detach(c.s)

str(c.s)

scatterplot(Fruit_proportion ~ Frequency, reg.line = lm, smooth = F, data=c.s)
scatterplot(Fruit_proportion ~ Frequency | Site_ID, reg.line = lm, smooth = F, data=c.s)
scatterplot(Fruit_proportion ~ Frequency | Year, reg.line = lm, smooth = F, data=c.s)

xyplot(Fruit_proportion ~ Frequency|Site_ID, data = c.s, type = c("p", "r"))
xyplot(Fruit_proportion ~ Frequency|Year, data = c.s, type = c("p", "r"))

levels(factor(c.s$Site_ID))

c.s <- c.s[!(c.s$Site_ID == "El pinar"),] #1 observ fruit=0 y seed=0
c.s <- c.s[!(c.s$Site_ID == "Niebla"),] 
#c.s <- c.s[!(c.s$Site_ID == "Pino del Cuervo"),]

# Variables correlacionadas, probar modelos con una variable
# random=Site_ID
#frequency
model1_c.s <- glmer(cbind(Fruit_Yes, Fruit_No) ~ Frequency + (1|Site_ID), data = c.s, family=binomial)
ranef(model1_c.s)
VarCorr(model1_c.s)
isSingular(model1_c.s)
rePCA(model1_c.s)


summary(model1_c.s)


simulationOutput <- simulateResiduals(fittedModel = model1_c.s)
plot(simulationOutput)

model1.1_c.s <- glm(cbind(Fruit_Yes, Fruit_No) ~ Frequency, data = c.s, family=binomial)
summary(model1.1_c.s)
plot(model1.1_c.s)

#richness
model2_c.s <- glmer(cbind(Fruit_Yes, Fruit_No) ~ richness + (1|Site_ID), data = c.s, family=binomial)
VarCorr(model2_c.s)
isSingular(model2_c.s)
summary(model2_c.s)

simulationOutput2 <- simulateResiduals(fittedModel = model2_c.s)
plot(simulationOutput2)

model2.1_c.s <- glm(cbind(Fruit_Yes, Fruit_No) ~ richness, data = c.s, family=binomial)
summary(model2.1_c.s)
plot(model2.1_c.s)

#frequency hymenopter, diptera, coleoptera
model3_c.s <- glmer(cbind(Fruit_Yes, Fruit_No) ~ Hym_freq+Dip_freq+ Coleop_freq+ (1|Site_ID), data = c.s, family=binomial)
summary(model3_c.s)

simulationOutput3 <- simulateResiduals(fittedModel = model3_c.s)
plot(simulationOutput3)

model3.1_c.s <- glm(cbind(Fruit_Yes, Fruit_No) ~ Hym_freq+Dip_freq+ Coleop_freq, data = c.s, family=binomial)
summary(model3.1_c.s)
plot(model3.1_c.s)

#richness 80%
model4_c.s <- glmer(cbind(Fruit_Yes, Fruit_No) ~ richness08 + (1|Site_ID), data = c.s, family=binomial)
summary(model4_c.s)

simulationOutput4 <- simulateResiduals(fittedModel = model4_c.s)
plot(simulationOutput4)


model4.1_c.s <- glm(cbind(Fruit_Yes, Fruit_No) ~ richness08, data = c.s, family=binomial)
summary(model4.1_c.s)
plot(model4.1_c.s)

#+++++++++
# random= Year
#frequency
mod1.1_c.s <- glmer(cbind(Fruit_Yes, Fruit_No) ~ Frequency + (1|Year), data = c.s, family=binomial)
summary(mod1.1_c.s)

simulationOutputY <- simulateResiduals(fittedModel = mod1.1_c.s)
plot(simulationOutputY)

#richness
mod2.1_c.s <- glmer(cbind(Fruit_Yes, Fruit_No) ~ richness + (1|Year), data = c.s, family=binomial)
summary(mod2.1_c.s)

simulationOutput2Y <- simulateResiduals(fittedModel = mod2.1_c.s)
plot(simulationOutput2Y)

#frequency hymenopter, diptera, coleoptera
mod3.1_c.s <- glmer(cbind(Fruit_Yes, Fruit_No) ~ Hym_freq+Dip_freq+ Coleop_freq+ (1|Year), data = c.s, family=binomial)
summary(mod3.1_c.s)

simulationOutput3Y <- simulateResiduals(fittedModel = mod3.1_c.s)
plot(simulationOutput3Y)

#richness 80%
mod4.1_c.s <- glmer(cbind(Fruit_Yes, Fruit_No) ~ richness08 + (1|Year), data = c.s, family=binomial)
summary(mod4.1_c.s)

simulationOutput4Y <- simulateResiduals(fittedModel = mod4.1_c.s)
plot(simulationOutput4Y)



## Seed number ##

attach(c.s)
par(mfrow=c(2,3))
plot(Frequency,seed_number,xlab="Pollinator frequency",ylab="Seed number")
plot(richness,seed_number,xlab="Richness",ylab="Seed number")
plot(richness08,seed_number,xlab="Richness-80%",ylab="Seed number")
plot(Hym_freq,seed_number,xlab="Hymenoptera frequency",ylab="Seed number")
plot(Dip_freq,seed_number,xlab="Diptera frequency",ylab="Seed number")
plot(Coleop_freq,seed_number,xlab="Coleoptera frequency",ylab="Seed number")

detach(c.s)

# Variables correlacionadas, probar modelos con una variable
# random=Site_ID
#frequency
model1seed_c.s <- lme(seed_number ~ Frequency,random= ~ 1|Site_ID,data = c.s )
summary(model1seed_c.s)

plot(model1seed_c.s)
resi_seed <- residuals(model1seed_c.s)
qqnorm(resi_seed)
qqline(resi_seed, lty=3, col="red")

#richness
model2seed_c.s <- lme(seed_number ~ richness,random= ~ 1|Site_ID,data = c.s )
summary(model2seed_c.s)

plot(model2seed_c.s)
resi_seed2 <- residuals(model2seed_c.s)
qqnorm(resi_seed2)
qqline(resi_seed2, lty=3, col="red")

#frequency hymenopter, diptera, coleoptera
model3seed_c.s <- lme(seed_number ~ Hym_freq+Dip_freq+Coleop_freq,random= ~ 1|Site_ID,data = c.s )
summary(model3seed_c.s)

plot(model3seed_c.s)
resi_seed3 <- residuals(model3seed_c.s)
qqnorm(resi_seed3)
qqline(resi_seed3, lty=3, col="red")

#richness 80%
model4seed_c.s <- lme(seed_number ~ richness08,random= ~ 1|Site_ID,data = c.s )
summary(model4seed_c.s)

plot(model4seed_c.s)
resi_seed4 <- residuals(model4seed_c.s)
qqnorm(resi_seed4)
qqline(resi_seed4, lty=3, col="red")

#++++++++++++++
# random= Year
#frequency
model1.1seed_c.s <- lme(seed_number ~ Frequency,random= ~ 1|Year,data = c.s )
summary(model1.1seed_c.s)

plot(model1.1seed_c.s)
resi_seed1 <- residuals(model1.1seed_c.s)
qqnorm(resi_seed1)
qqline(resi_seed1, lty=3, col="red")

#richness
model2.1seed_c.s <- lme(seed_number ~ richness,random= ~ 1|Year,data = c.s )
summary(model2.1seed_c.s)

plot(model2.1seed_c.s)
resi_seed2.1 <- residuals(model2.1seed_c.s)
qqnorm(resi_seed2.1)
qqline(resi_seed2.1, lty=3, col="red")

#frequency hymenopter, diptera, coleoptera
model3.1seed_c.s <- lme(seed_number ~ Hym_freq+Dip_freq+Coleop_freq,random= ~ 1|Year,data = c.s )
summary(model3.1seed_c.s)

plot(model3.1seed_c.s)
resi_seed3.1 <- residuals(model3.1seed_c.s)
qqnorm(resi_seed3.1)
qqline(resi_seed3.1, lty=3, col="red")

#richness 80%
model4.1seed_c.s <- lme(seed_number ~ richness08,random= ~ 1|Year,data = c.s)
summary(model4.1seed_c.s)

plot(model4.1seed_c.s)
resi_seed4.1 <- residuals(model4.1seed_c.s)
qqnorm(resi_seed4.1)
qqline(resi_seed4.1, lty=3, col="red")

#+++++++++++
# sin random factor
#frequency
model1.2seed_c.s <- lm(seed_number ~ Frequency,data = c.s )
summary(model1.2seed_c.s)

plot(model1.2seed_c.s)

#richness
model2.2seed_c.s <- lm(seed_number ~ richness,data = c.s)
summary(model2.2seed_c.s)
plot(model2.2seed_c.s)

#frequency hymenopter, diptera, coleoptera
model3.2seed_c.s <- lm(seed_number ~ Hym_freq+Dip_freq+Coleop_freq,data = c.s)
summary(model3.2seed_c.s)
plot(model3.2seed_c.s)

#richness 80%
model4.2seed_c.s <- lm(seed_number ~ richness08,data = c.s)
summary(model4.2seed_c.s)
plot(model4.2seed_c.s)



## Seed weight ##

attach(c.s)
par(mfrow=c(2,3))
plot(Frequency,Seed_weight,xlab="Pollinator frequency",ylab="Seed_weight")
plot(richness,Seed_weight,xlab="Richness",ylab="Seed_weight")
plot(richness08,Seed_weight,xlab="Richness-80%",ylab="Seed_weight")
plot(Hym_freq,Seed_weight,xlab="Hymenoptera frequency",ylab="Seed_weight")
plot(Dip_freq,Seed_weight,xlab="Diptera frequency",ylab="Seed_weight")
plot(Coleop_freq,Seed_weight,xlab="Coleoptera frequency",ylab="Seed_weight")

detach(c.s)

# random=Site_ID
#frequency
model1weigth_c.s <- lme(Seed_weight ~ Frequency,random= ~ 1|Site_ID,data = c.s)
summary(model1weigth_c.s)

plot(model1weigth_c.s)
resi_seed_w <- residuals(model1weigth_c.s)
qqnorm(resi_seed_w)
qqline(resi_seed_w, lty=3, col="red")

#richness
model2weigth_c.s <- lme(Seed_weight ~ richness,random= ~ 1|Site_ID,data = c.s )
summary(model2weigth_c.s)

plot(model2weigth_c.s)
resi_seed2_w <- residuals(model2weigth_c.s)
qqnorm(resi_seed2_w)
qqline(resi_seed2_w, lty=3, col="red")

#frequency hymenopter, diptera, coleoptera
model3weigth_c.s <- lme(Seed_weight ~ Hym_freq+Dip_freq+Coleop_freq,random= ~ 1|Site_ID,data = c.s)
summary(model3weigth_c.s)

plot(model3weigth_c.s)
resi_seed3_w <- residuals(model3weigth_c.s)
qqnorm(resi_seed3_w)
qqline(resi_seed3_w, lty=3, col="red")

#richness 80%
model4weigth_c.s <- lme(Seed_weight ~ richness08,random= ~ 1|Site_ID,data = c.s)
summary(model4weigth_c.s)

plot(model4weigth_c.s)
resi_seed4_w <- residuals(model4weigth_c.s)
qqnorm(resi_seed4_w)
qqline(resi_seed4_w, lty=3, col="red")

#++++++++++++
# random= Year
#frequency
model1.1weight_c.s <- lme(Seed_weight ~ Frequency,random= ~ 1|Year,data = c.s)
summary(model1.1weight_c.s)

plot(model1.1weight_c.s)
resi_seed1.1_w <- residuals(model1.1weight_c.s)
qqnorm(resi_seed1.1_w)
qqline(resi_seed1.1_w, lty=3, col="red")

#richness
model2.1weight_c.s <- lme(Seed_weight ~ richness,random= ~ 1|Year,data = c.s)
summary(model2.1weight_c.s)

plot(model2.1weight_c.s)
resi_seed2.1_w <- residuals(model2.1weight_c.s)
qqnorm(resi_seed2.1_w)
qqline(resi_seed2.1_w, lty=3, col="red")

#frequency hymenopter, diptera, coleoptera
model3.1weight_c.s <- lme(Seed_weight ~ Hym_freq+Dip_freq+Coleop_freq,random= ~ 1|Year,data = c.s )
summary(model3.1weight_c.s)

plot(model3.1weight_c.s)
resi_seed3.1_w <- residuals(model3.1weight_c.s)
qqnorm(resi_seed3.1_w)
qqline(resi_seed3.1_w, lty=3, col="red")

#richness 80%
model4.1weight_c.s <- lme(Seed_weight ~ richness08,random= ~ 1|Year,data = c.s)
summary(model4.1weight_c.s)

plot(model4.1weight_c.s)
resi_seed4.1_w <- residuals(model4.1weight_c.s)
qqnorm(resi_seed4.1_w)
qqline(resi_seed4.1_w, lty=3, col="red")


#++++++++++++++++
# sin random factor
#frequency
model1.2weight_c.s <- lm(Seed_weight ~ Frequency,data = c.s)
summary(model1.2weight_c.s)

plot(model1.2weight_c.s)

#richness
model2.2weight_c.s <- lm(Seed_weight ~ richness,data = c.s)
summary(model2.2weight_c.s)
plot(model2.2weight_c.s)

#frequency hymenopter, diptera, coleoptera
model3.2weight_c.s <- lm(Seed_weight ~ Hym_freq+Dip_freq+Coleop_freq,data = c.s)
summary(model3.2weight_c.s)

plot(model3.2weight_c.s)

#richness 80%
model4.2weight_c.s <- lm(Seed_weight ~ richness08,data = c.s)
summary(model4.2weight_c.s)
plot(model4.2weight_c.s)



#######################
##  Cistus ladanifer  ##


## Fruit propotion 
attach(c.l)
par(mfrow=c(2,3))
plot(Frequency,Fruit_proportion,xlab="Pollinator frequency",ylab="Fruit proportion")
plot(richness,Fruit_proportion,xlab="Richness",ylab="Fruit proportion")
plot(richness08,Fruit_proportion,xlab="Richness-80%",ylab="Fruit proportion")
plot(Hym_freq,Fruit_proportion,xlab="Hymenoptera frequency",ylab="Fruit proportion")
plot(Dip_freq,Fruit_proportion,xlab="Diptera frequency",ylab="Fruit proportion")
plot(Coleop_freq,Fruit_proportion,xlab="Coleoptera frequency",ylab="Fruit proportion")

detach(c.l)

# Variables correlacionadas, probar modelos con una variable
# random=Site_ID
#frequency
xyplot(Fruit_proportion ~ Frequency | Site_ID, data = c.l, type = c("p", "r"))

model1_c.l <- glmer(cbind(Fruit_Yes, Fruit_No) ~ Frequency + (1|Site_ID), data = c.l, family=binomial)
summary(model1_c.l)

simulationOutput <- simulateResiduals(fittedModel = model1_c.l)
plot(simulationOutput)


#richness
model2_c.l <- glmer(cbind(Fruit_Yes, Fruit_No) ~ richness + (1|Site_ID), data = c.l, family=binomial)

#Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
# Model failed to converge with max|grad| = 0.0170295 (tol = 0.002, component 1)

xyplot(Fruit_proportion ~ richness | Site_ID, data = c.l, type = c("p", "r"))


#install.packages("optimx")
library(optimx)
aa <- allFit(model2_c.l)
ss <- summary(aa)
ss$msgs

model2_c.l_n <- glmer(cbind(Fruit_Yes, Fruit_No) ~ scale(richness) + (1|Site_ID), data = c.l, family=binomial)
# the same warning message


#frequency hymenopter, diptera, coleoptera
model3_c.l <- glmer(cbind(Fruit_Yes, Fruit_No) ~ Hym_freq+Dip_freq+ Coleop_freq+ (1|Site_ID), data = c.l, family=binomial)
summary(model3_c.l)

simulationOutput3 <- simulateResiduals(fittedModel = model3_c.l)
plot(simulationOutput3)

#richness 80%
model4_c.l <- glmer(cbind(Fruit_Yes, Fruit_No) ~ richness08 + (1|Site_ID), data = c.l, family=binomial)
#same warning that richness

#++++++++++
# random= Year
#frequency
model1.1_c.l <- glmer(cbind(Fruit_Yes, Fruit_No) ~ Frequency + (1|Year), data = c.l, family=binomial)
summary(model1.1_c.l)

simulationOutputY <- simulateResiduals(fittedModel = model1.1_c.l)
plot(simulationOutputY)

#richness
model2.1_c.l <- glmer(cbind(Fruit_Yes, Fruit_No) ~ richness + (1|Year), data = c.l, family=binomial)
summary(model2.1_c.l)

simulationOutput2Y <- simulateResiduals(fittedModel = model2.1_c.l)
plot(simulationOutput2Y)

#frequency hymenopter, diptera, coleoptera
model3.1_c.l <- glmer(cbind(Fruit_Yes, Fruit_No) ~ Hym_freq+Dip_freq+ Coleop_freq+ (1|Year), data = c.l, family=binomial)
summary(model3.1_c.l)

simulationOutput3Y <- simulateResiduals(fittedModel = model3.1_c.l)
plot(simulationOutput3Y)

#richness 80%
model4.1_c.l <- glmer(cbind(Fruit_Yes, Fruit_No) ~ richness08 + (1|Year), data = c.l, family=binomial)
summary(model4.1_c.l)

simulationOutput4Y <- simulateResiduals(fittedModel = model4.1_c.l)
plot(simulationOutput4Y)

#+++++++++++
# sin random factor
mod_c.l<- glm(cbind(Fruit_Yes, Fruit_No) ~ Frequency, family=binomial, data = c.l)
summary(mod_c.l)
AIC(mod_c.l)

plot(mod_c.l)


mod2_c.l <- glm(cbind(Fruit_Yes, Fruit_No) ~ richness, data = c.l, family=binomial)
summary(mod2_c.l)

plot(mod2_c.l)


mod3_c.l <- glm(cbind(Fruit_Yes, Fruit_No) ~ Hym_freq+Dip_freq+ Coleop_freq, data = c.l, family=binomial)
summary(mod3_c.l)
AIC(mod3_c.l)
plot(mod3_c.l)


mod4_c.l <- glm(cbind(Fruit_Yes, Fruit_No) ~ richness08, data = c.l, family=binomial)
summary(mod4_c.l)
plot(mod4_c.l)


## Seed number ##

attach(c.l)
par(mfrow=c(2,3))
plot(Frequency,seed_number,xlab="Pollinator frequency",ylab="Seed number")
plot(richness,seed_number,xlab="Richness",ylab="Seed number")
plot(richness08,seed_number,xlab="Richness-80%",ylab="Seed number")
plot(Hym_freq,seed_number,xlab="Hymenoptera frequency",ylab="Seed number")
plot(Dip_freq,seed_number,xlab="Diptera frequency",ylab="Seed number")
plot(Coleop_freq,seed_number,xlab="Coleoptera frequency",ylab="Seed number")

detach(c.l)

# Variables correlacionadas, probar modelos con una variable
# random=Site_ID
#frequency
model1seed_c.l <- lme(seed_number ~ Frequency,random= ~ 1|Site_ID,data = c.l )
summary(model1seed_c.l)

plot(model1seed_c.l)
resi_seed <- residuals(model1seed_c.l)
qqnorm(resi_seed)
qqline(resi_seed, lty=3, col="red")

#richness
model2seed_c.l <- lme(seed_number ~ richness,random= ~ 1|Site_ID,data = c.l )
summary(model2seed_c.l)

plot(model2seed_c.l)
resi_seed2 <- residuals(model2seed_c.l)
qqnorm(resi_seed2)
qqline(resi_seed2, lty=3, col="red")

#frequency hymenopter, diptera, coleoptera
model3seed_c.l <- lme(seed_number ~ Hym_freq+Dip_freq+Coleop_freq,random= ~ 1|Site_ID,data = c.l )
summary(model3seed_c.l)

plot(model3seed_c.l)
resi_seed3 <- residuals(model3seed_c.l)
qqnorm(resi_seed3)
qqline(resi_seed3, lty=3, col="red")

#richness 80%
model4seed_c.l <- lme(seed_number ~ richness08,random= ~ 1|Site_ID,data = c.l)
summary(model4seed_c.l)

plot(model4seed_c.l)
resi_seed4 <- residuals(model4seed_c.l)
qqnorm(resi_seed4)
qqline(resi_seed4, lty=3, col="red")

#++++++++
# random= Year
#frequency
model1.1seed_c.l <- lme(seed_number ~ Frequency,random= ~ 1|Year,data = c.l)
summary(model1.1seed_c.l)

plot(model1.1seed_c.l)
resi_seed1 <- residuals(model1.1seed_c.l)
qqnorm(resi_seed1)
qqline(resi_seed1, lty=3, col="red")

#richness
model2.1seed_c.l <- lme(seed_number ~ richness,random= ~ 1|Year,data = c.l)
summary(model2.1seed_c.l)

plot(model2.1seed_c.l)
resi_seed2.1 <- residuals(model2.1seed_c.l)
qqnorm(resi_seed2.1)
qqline(resi_seed2.1, lty=3, col="red")

#frequency hymenopter, diptera, coleoptera
model3.1seed_c.l <- lme(seed_number ~ Hym_freq+Dip_freq+Coleop_freq,random= ~ 1|Year,data = c.l )
summary(model3.1seed_c.l)

plot(model3.1seed_c.l)
resi_seed3.1 <- residuals(model3.1seed_c.l)
qqnorm(resi_seed3.1)
qqline(resi_seed3.1, lty=3, col="red")

#richness 80%
model4.1seed_c.l <- lme(seed_number ~ richness08,random= ~ 1|Year,data = c.l)
summary(model4.1seed_c.l)

plot(model4.1seed_c.l)
resi_seed4.1 <- residuals(model4.1seed_c.l)
qqnorm(resi_seed4.1)
qqline(resi_seed4.1, lty=3, col="red")

#++++++++++++
# sin random factor
#frequency
model1.2seed_c.l <- lm(seed_number ~ Frequency,data = c.l)
summary(model1.2seed_c.l)

plot(model1.2seed_c.l)

#richness
model2.2seed_c.l <- lm(seed_number ~ richness,data = c.l)
summary(model2.2seed_c.l)
plot(model2.2seed_c.l)

#frequency hymenopter, diptera, coleoptera
model3.2seed_c.l <- lm(seed_number ~ Hym_freq+Dip_freq+Coleop_freq,data = c.l)
summary(model3.2seed_c.l)
plot(model3.2seed_c.l)

#richness 80%
model4.2seed_c.l <- lm(seed_number ~ richness08,data = c.l)
summary(model4.2seed_c.l)
plot(model4.2seed_c.l)



## Seed weight ##

attach(c.l)
par(mfrow=c(2,3))
plot(Frequency,Seed_weight,xlab="Pollinator frequency",ylab="Seed_weight")
plot(richness,Seed_weight,xlab="Richness",ylab="Seed_weight")
plot(richness08,Seed_weight,xlab="Richness-80%",ylab="Seed_weight")
plot(Hym_freq,Seed_weight,xlab="Hymenoptera frequency",ylab="Seed_weight")
plot(Dip_freq,Seed_weight,xlab="Diptera frequency",ylab="Seed_weight")
plot(Coleop_freq,Seed_weight,xlab="Coleoptera frequency",ylab="Seed_weight")

detach(c.l)

# Variables correlacionadas, probar modelos con una variable
# random=Site_ID
#frequency
model1weigth_c.l <- lme(Seed_weight ~ Frequency,random= ~ 1|Site_ID,data = c.l)
summary(model1weigth_c.l)

plot(model1weigth_c.l)
resi_seed_w <- residuals(model1weigth_c.l)
qqnorm(resi_seed_w)
qqline(resi_seed_w, lty=3, col="red")

#richness
model2weigth_c.l <- lme(Seed_weight ~ richness,random= ~ 1|Site_ID,data = c.l )
summary(model2weigth_c.l)

plot(model2weigth_c.l)
resi_seed2_w <- residuals(model2weigth_c.l)
qqnorm(resi_seed2_w)
qqline(resi_seed2_w, lty=3, col="red")

#frequency hymenopter, diptera, coleoptera
model3weigth_c.l <- lme(Seed_weight ~ Hym_freq+Dip_freq+Coleop_freq,random= ~ 1|Site_ID,data = c.l)
summary(model3weigth_c.l)

plot(model3weigth_c.l)
resi_seed3_w <- residuals(model3weigth_c.l)
qqnorm(resi_seed3_w)
qqline(resi_seed3_w, lty=3, col="red")

#richness 80%
model4weigth_c.l <- lme(Seed_weight ~ richness08,random= ~ 1|Site_ID,data = c.l)
summary(model4weigth_c.l)

plot(model4weigth_c.l)
resi_seed4_w <- residuals(model4weigth_c.l)
qqnorm(resi_seed4_w)
qqline(resi_seed4_w, lty=3, col="red")

#++++++
# random= Year
#frequency
model1.1weight_c.l <- lme(Seed_weight ~ Frequency,random= ~ 1|Year,data = c.l)
summary(model1.1weight_c.l)

plot(model1.1weight_c.l)
resi_seed1.1_w <- residuals(model1.1weight_c.l)
qqnorm(resi_seed1.1_w)
qqline(resi_seed1.1_w, lty=3, col="red")

#richness
model2.1weight_c.l <- lme(Seed_weight ~ richness,random= ~ 1|Year,data = c.l)
summary(model2.1weight_c.l)

plot(model2.1weight_c.l)
resi_seed2.1_w <- residuals(model2.1weight_c.l)
qqnorm(resi_seed2.1_w)
qqline(resi_seed2.1_w, lty=3, col="red")

#frequency hymenopter, diptera, coleoptera
model3.1weight_c.l <- lme(Seed_weight ~ Hym_freq+Dip_freq+Coleop_freq,random= ~ 1|Year,data = c.l )
summary(model3.1weight_c.l)

plot(model3.1weight_c.l)
resi_seed3.1_w <- residuals(model3.1weight_c.l)
qqnorm(resi_seed3.1_w)
qqline(resi_seed3.1_w, lty=3, col="red")

#richness 80%
model4.1weight_c.l <- lme(Seed_weight ~ richness08,random= ~ 1|Year,data = c.l)
summary(model4.1weight_c.l)

plot(model4.1weight_c.l)
resi_seed4.1_w <- residuals(model4.1weight_c.l)
qqnorm(resi_seed4.1_w)
qqline(resi_seed4.1_w, lty=3, col="red")


#++++++++++++
# sin random factor
#frequency
model1.2weight_c.l <- lm(Seed_weight ~ Frequency,data = c.l)
summary(model1.2weight_c.l)

plot(model1.2weight_c.l)

#richness
model2.2weight_c.l <- lm(Seed_weight ~ richness,data = c.l)
summary(model2.2weight_c.l)
plot(model2.2weight_c.l)

#frequency hymenopter, diptera, coleoptera
model3.2weight_c.l <- lm(Seed_weight ~ Hym_freq+Dip_freq+Coleop_freq,data = c.l)
summary(model3.2weight_c.l)

plot(model3.2weight_c.l)

#richness 80%
model4.2weight_c.l <- lm(Seed_weight ~ richness08,data = c.l)
summary(model4.2weight_c.l)
plot(model4.2weight_c.l)



############################
## Halimium halimifolium  ##

## Fruit propotion 
attach(h.h)
par(mfrow=c(2,3))
plot(Frequency,Fruit_proportion,xlab="Pollinator frequency",ylab="Fruit proportion")
plot(richness,Fruit_proportion,xlab="Richness",ylab="Fruit proportion")
plot(richness08,Fruit_proportion,xlab="Richness-80%",ylab="Fruit proportion")
plot(Hym_freq,Fruit_proportion,xlab="Hymenoptera frequency",ylab="Fruit proportion")
plot(Dip_freq,Fruit_proportion,xlab="Diptera frequency",ylab="Fruit proportion")
plot(Coleop_freq,Fruit_proportion,xlab="Coleoptera frequency",ylab="Fruit proportion")

detach(h.h)

# Variables correlacionadas, probar modelos con una variable
# random=Site_ID
#frequency
xyplot(Fruit_proportion ~ Frequency, data = h.h, type = c("p", "r"))
xyplot(Fruit_proportion ~ Frequency | Site_ID, data = h.h, type = c("p", "r"))
xyplot(Fruit_proportion ~ richness | Site_ID, data = h.h, type = c("p", "r"))

model1_h.h <- glmer(cbind(Fruit_Yes, Fruit_No) ~ Frequency + (1|Site_ID), data = h.h, family=binomial)
summary(model1_h.h)

simulationOutput <- simulateResiduals(fittedModel = model1_h.h)
plot(simulationOutput)


#richness
model2_h.h <- glmer(cbind(Fruit_Yes, Fruit_No) ~ richness + (1|Site_ID), data = h.h, family=binomial)
summary(model2_h.h)

simulationOutput2 <- simulateResiduals(fittedModel = model2_h.h)
plot(simulationOutput2)

#frequency hymenopter, diptera, coleoptera
model3_h.h <- glmer(cbind(Fruit_Yes, Fruit_No) ~ Hym_freq+Dip_freq+ Coleop_freq+ (1|Site_ID), data = h.h, family=binomial)
summary(model3_h.h)

simulationOutput3 <- simulateResiduals(fittedModel = model3_h.h)
plot(simulationOutput3)

#richness 80%
model4_h.h <- glmer(cbind(Fruit_Yes, Fruit_No) ~ richness08 + (1|Site_ID), data = h.h, family=binomial)
summary(model4_h.h)

simulationOutput4 <- simulateResiduals(fittedModel = model4_h.h)
plot(simulationOutput4)

#++++++++++
# random= Year
#frequency
model1.1_h.h <- glmer(cbind(Fruit_Yes, Fruit_No) ~ Frequency + (1|Year), data = h.h, family=binomial)
summary(model1.1_h.h)

simulationOutputY <- simulateResiduals(fittedModel = model1.1_h.h)
plot(simulationOutputY)

#richness
model2.1_h.h <- glmer(cbind(Fruit_Yes, Fruit_No) ~ richness + (1|Year), data = h.h, family=binomial)
summary(model2.1_h.h)

simulationOutput2Y <- simulateResiduals(fittedModel = model2.1_h.h)
plot(simulationOutput2Y)

#frequency hymenopter, diptera, coleoptera
model3.1_h.h<- glmer(cbind(Fruit_Yes, Fruit_No) ~ Hym_freq+Dip_freq+ Coleop_freq+ (1|Year), data = h.h, family=binomial)
summary(model3.1_h.h)

simulationOutput3Y <- simulateResiduals(fittedModel = model3.1_h.h)
plot(simulationOutput3Y)

#richness 80%
model4.1_h.h <- glmer(cbind(Fruit_Yes, Fruit_No) ~ richness08 + (1|Year), data = h.h, family=binomial)
summary(model4.1_h.h)

simulationOutput4Y <- simulateResiduals(fittedModel = model4.1_h.h)
plot(simulationOutput4Y)

#+++++++++++
# sin random factor
mod_h.h<- glm(cbind(Fruit_Yes, Fruit_No) ~ Frequency, family=binomial, data = h.h)
summary(mod_h.h)
plot(mod_h.h)


mod2_h.h <- glm(cbind(Fruit_Yes, Fruit_No) ~ richness, data = h.h, family=binomial)
summary(mod2_h.h)
plot(mod2_h.h)


mod3_h.h <- glm(cbind(Fruit_Yes, Fruit_No) ~ Hym_freq+Dip_freq+ Coleop_freq, data = h.h, family=binomial)
summary(mod3_h.h)
plot(mod3_h.h)


mod4_h.h <- glm(cbind(Fruit_Yes, Fruit_No) ~ richness08, data = h.h, family=binomial)
summary(mod4_h.h)
plot(mod4_h.h)


## Seed number ##

attach(h.h)
par(mfrow=c(2,3))
plot(Frequency,seed_number,xlab="Pollinator frequency",ylab="Seed number")
plot(richness,seed_number,xlab="Richness",ylab="Seed number")
plot(richness08,seed_number,xlab="Richness-80%",ylab="Seed number")
plot(Hym_freq,seed_number,xlab="Hymenoptera frequency",ylab="Seed number")
plot(Dip_freq,seed_number,xlab="Diptera frequency",ylab="Seed number")
plot(Coleop_freq,seed_number,xlab="Coleoptera frequency",ylab="Seed number")
detach(h.h)

xyplot(seed_number ~ Frequency, data = h.h, type = c("p", "r"))
xyplot(seed_number ~ Frequency | Site_ID, data = h.h, type = c("p", "r"))
xyplot(seed_number ~ richness | Site_ID, data = h.h, type = c("p", "r"))


# Variables correlacionadas, probar modelos con una variable
# random=Site_ID
#frequency
model1seed_h.h <- lme(seed_number ~ Frequency,random= ~ 1|Site_ID,data = h.h )
summary(model1seed_h.h)

plot(model1seed_h.h)
resi_seed <- residuals(model1seed_h.h)
qqnorm(resi_seed)
qqline(resi_seed, lty=3, col="red")

#richness
model2seed_h.h <- lme(seed_number ~ richness,random= ~ 1|Site_ID,data = h.h )
summary(model2seed_h.h)

plot(model2seed_h.h)
resi_seed2 <- residuals(model2seed_h.h)
qqnorm(resi_seed2)
qqline(resi_seed2, lty=3, col="red")

#frequency hymenopter, diptera, coleoptera
model3seed_h.h <- lme(seed_number ~ Hym_freq+Dip_freq+Coleop_freq,random= ~ 1|Site_ID,data = h.h )
summary(model3seed_h.h)

plot(model3seed_h.h)
resi_seed3 <- residuals(model3seed_h.h)
qqnorm(resi_seed3)
qqline(resi_seed3, lty=3, col="red")

#richness 80%
model4seed_h.h <- lme(seed_number ~ richness08,random= ~ 1|Site_ID,data = h.h)
summary(model4seed_h.h)

plot(model4seed_h.h)
resi_seed4 <- residuals(model4seed_h.h)
qqnorm(resi_seed4)
qqline(resi_seed4, lty=3, col="red")

#++++++++
# random= Year
#frequency
model1.1seed_h.h <- lme(seed_number ~ Frequency,random= ~ 1|Year,data = h.h)
summary(model1.1seed_h.h)

plot(model1.1seed_h.h)
resi_seed1 <- residuals(model1.1seed_h.h)
qqnorm(resi_seed1)
qqline(resi_seed1, lty=3, col="red")

#richness
model2.1seed_h.h <- lme(seed_number ~ richness,random= ~ 1|Year,data = h.h)
summary(model2.1seed_h.h)

plot(model2.1seed_h.h)
resi_seed2.1 <- residuals(model2.1seed_h.h)
qqnorm(resi_seed2.1)
qqline(resi_seed2.1, lty=3, col="red")

#frequency hymenopter, diptera, coleoptera
model3.1seed_h.h <- lme(seed_number ~ Hym_freq+Dip_freq+Coleop_freq,random= ~ 1|Year,data = h.h )
summary(model3.1seed_h.h)

plot(model3.1seed_h.h)
resi_seed3.1 <- residuals(model3.1seed_h.h)
qqnorm(resi_seed3.1)
qqline(resi_seed3.1, lty=3, col="red")

#richness 80%
model4.1seed_h.h <- lme(seed_number ~ richness08,random= ~ 1|Year,data = h.h)
summary(model4.1seed_h.h)

plot(model4.1seed_h.h)
resi_seed4.1 <- residuals(model4.1seed_h.h)
qqnorm(resi_seed4.1)
qqline(resi_seed4.1, lty=3, col="red")

#++++++++++++
# sin random factor
#frequency
model1.2seed_h.h <- lm(seed_number ~ Frequency,data = h.h)
summary(model1.2seed_h.h)

plot(model1.2seed_h.h)

#richness
model2.2seed_h.h <- lm(seed_number ~ richness,data = h.h)
summary(model2.2seed_h.h)
plot(model2.2seed_h.h)

#frequency hymenopter, diptera, coleoptera
model3.2seed_h.h <- lm(seed_number ~ Hym_freq+Dip_freq+Coleop_freq,data = h.h)
summary(model3.2seed_h.h)
plot(model3.2seed_h.h)

#richness 80%
model4.2seed_h.h <- lm(seed_number ~ richness08,data = h.h)
summary(model4.2seed_h.h)
plot(model4.2seed_h.h)


## Seed weight ##

attach(h.h)
par(mfrow=c(2,3))
plot(Frequency,Seed_weight,xlab="Pollinator frequency",ylab="Seed_weight")
plot(richness,Seed_weight,xlab="Richness",ylab="Seed_weight")
plot(richness08,Seed_weight,xlab="Richness-80%",ylab="Seed_weight")
plot(Hym_freq,Seed_weight,xlab="Hymenoptera frequency",ylab="Seed_weight")
plot(Dip_freq,Seed_weight,xlab="Diptera frequency",ylab="Seed_weight")
plot(Coleop_freq,Seed_weight,xlab="Coleoptera frequency",ylab="Seed_weight")
detach(h.h)

# random=Site_ID
#frequency
model1weigth_h.h <- lme(Seed_weight ~ Frequency,random= ~ 1|Site_ID,data = h.h)
summary(model1weigth_h.h)

plot(model1weigth_h.h)
resi_seed_w <- residuals(model1weigth_h.h)
qqnorm(resi_seed_w)
qqline(resi_seed_w, lty=3, col="red")

#richness
model2weigth_h.h <- lme(Seed_weight ~ richness,random= ~ 1|Site_ID,data = h.h )
summary(model2weigth_h.h)

plot(model2weigth_h.h)
resi_seed2_w <- residuals(model2weigth_h.h)
qqnorm(resi_seed2_w)
qqline(resi_seed2_w, lty=3, col="red")

#frequency hymenopter, diptera, coleoptera
model3weigth_h.h <- lme(Seed_weight ~ Hym_freq+Dip_freq+Coleop_freq,random= ~ 1|Site_ID,data = h.h)
summary(model3weigth_h.h)

plot(model3weigth_h.h)
resi_seed3_w <- residuals(model3weigth_h.h)
qqnorm(resi_seed3_w)
qqline(resi_seed3_w, lty=3, col="red")

#richness 80%
model4weigth_h.h <- lme(Seed_weight ~ richness08,random= ~ 1|Site_ID,data = h.h)
summary(model4weigth_h.h)

plot(model4weigth_h.h)
resi_seed4_w <- residuals(model4weigth_h.h)
qqnorm(resi_seed4_w)
qqline(resi_seed4_w, lty=3, col="red")

#++++++
# random= Year
#frequency
model1.1weight_h.h <- lme(Seed_weight ~ Frequency,random= ~ 1|Year,data = h.h)
summary(model1.1weight_h.h)

plot(model1.1weight_h.h)
resi_seed1.1_w <- residuals(model1.1weight_h.h)
qqnorm(resi_seed1.1_w)
qqline(resi_seed1.1_w, lty=3, col="red")

#richness
model2.1weight_h.h <- lme(Seed_weight ~ richness,random= ~ 1|Year,data = h.h)
summary(model2.1weight_h.h)

plot(model2.1weight_h.h)
resi_seed2.1_w <- residuals(model2.1weight_h.h)
qqnorm(resi_seed2.1_w)
qqline(resi_seed2.1_w, lty=3, col="red")

#frequency hymenopter, diptera, coleoptera
model3.1weight_h.h <- lme(Seed_weight ~ Hym_freq+Dip_freq+Coleop_freq,random= ~ 1|Year,data = h.h )
summary(model3.1weight_h.h)

plot(model3.1weight_h.h)
resi_seed3.1_w <- residuals(model3.1weight_h.h)
qqnorm(resi_seed3.1_w)
qqline(resi_seed3.1_w, lty=3, col="red")

#richness 80%
model4.1weight_h.h <- lme(Seed_weight ~ richness08,random= ~ 1|Year,data = h.h)
summary(model4.1weight_h.h)

plot(model4.1weight_h.h)
resi_seed4.1_w <- residuals(model4.1weight_h.h)
qqnorm(resi_seed4.1_w)
qqline(resi_seed4.1_w, lty=3, col="red")


#++++++++++++
# sin random factor
#frequency
model1.2weight_h.h <- lm(Seed_weight ~ Frequency,data = h.h)
summary(model1.2weight_h.h)

plot(model1.2weight_h.h)

#richness
model2.2weight_h.h <- lm(Seed_weight ~ richness,data = h.h)
summary(model2.2weight_h.h)
plot(model2.2weight_h.h)

#frequency hymenopter, diptera, coleoptera
model3.2weight_h.h <- lm(Seed_weight ~ Hym_freq+Dip_freq+Coleop_freq,data = h.h)
summary(model3.2weight_h.h)

plot(model3.2weight_h.h)

#richness 80%
model4.2weight_h.h <- lm(Seed_weight ~ richness08,data = h.h)
summary(model4.2weight_h.h)
plot(model4.2weight_h.h)


############################
## Lavandula pedunculatta ##

## Fruit propotion 

attach(l.p)
par(mfrow=c(2,3))
plot(Frequency,Fruit_proportion,xlab="Pollinator frequency",ylab="Fruit proportion")
plot(richness,Fruit_proportion,xlab="Richness",ylab="Fruit proportion")
plot(richness08,Fruit_proportion,xlab="Richness-80%",ylab="Fruit proportion")
plot(Hym_freq,Fruit_proportion,xlab="Hymenoptera frequency",ylab="Fruit proportion")
plot(Dip_freq,Fruit_proportion,xlab="Diptera frequency",ylab="Fruit proportion")
plot(Coleop_freq,Fruit_proportion,xlab="Coleoptera frequency",ylab="Fruit proportion")

detach(l.p)

# Variables correlacionadas, probar modelos con una variable
# random=Site_ID
#frequency
xyplot(Fruit_proportion ~ Frequency, data = l.p, type = c("p", "r"))
xyplot(Fruit_proportion ~ Frequency | Site_ID, data = l.p, type = c("p", "r"))
xyplot(Fruit_proportion ~ richness | Site_ID, data = l.p, type = c("p", "r"))

model1_l.p <- glmer(cbind(Fruit_Yes, Fruit_No) ~ Frequency + (1|Site_ID), data = l.p, family=binomial)
summary(model1_l.p)

simulationOutput <- simulateResiduals(fittedModel = model1_l.p)
plot(simulationOutput)


#richness
model2_l.p <- glmer(cbind(Fruit_Yes, Fruit_No) ~ richness + (1|Site_ID), data = l.p, family=binomial)
summary(model2_l.p)

simulationOutput2 <- simulateResiduals(fittedModel = model2_l.p)
plot(simulationOutput2)

#frequency hymenopter, diptera, coleoptera
model3_l.p <- glmer(cbind(Fruit_Yes, Fruit_No) ~ Hym_freq+Dip_freq+ Coleop_freq+ (1|Site_ID), data = l.p, family=binomial)
summary(model3_l.p)

simulationOutput3 <- simulateResiduals(fittedModel = model3_l.p)
plot(simulationOutput3)

#richness 80%
model4_l.p<- glmer(cbind(Fruit_Yes, Fruit_No) ~ richness08 + (1|Site_ID), data = l.p, family=binomial)
summary(model4_l.p)

simulationOutput4 <- simulateResiduals(fittedModel = model4_l.p)
plot(simulationOutput4)

#++++++++++
# random= Year
#frequency
model1.1_l.p <- glmer(cbind(Fruit_Yes, Fruit_No) ~ Frequency + (1|Year), data = l.p, family=binomial)
summary(model1.1_l.p)

simulationOutputY <- simulateResiduals(fittedModel = model1.1_l.p)
plot(simulationOutputY)

#richness
model2.1_l.p <- glmer(cbind(Fruit_Yes, Fruit_No) ~ richness + (1|Year), data = l.p, family=binomial)
summary(model2.1_l.p)

simulationOutput2Y <- simulateResiduals(fittedModel = model2.1_l.p)
plot(simulationOutput2Y)

#frequency hymenopter, diptera, coleoptera
model3.1_l.p<- glmer(cbind(Fruit_Yes, Fruit_No) ~ Hym_freq+Dip_freq+ Coleop_freq+ (1|Year), data = l.p, family=binomial)
summary(model3.1_l.p)

simulationOutput3Y <- simulateResiduals(fittedModel = model3.1_l.p)
plot(simulationOutput3Y)

#richness 80%
model4.1_l.p <- glmer(cbind(Fruit_Yes, Fruit_No) ~ richness08 + (1|Year), data = l.p, family=binomial)
summary(model4.1_l.p)

simulationOutput4Y <- simulateResiduals(fittedModel = model4.1_l.p)
plot(simulationOutput4Y)

#+++++++++++
# sin random factor
mod_l.p <- glm(cbind(Fruit_Yes, Fruit_No) ~ Frequency, family=binomial, data = l.p)
summary(mod_l.p)
plot(mod_l.p)


mod2_l.p <- glm(cbind(Fruit_Yes, Fruit_No) ~ richness, data = l.p, family=binomial)
summary(mod2_l.p)
plot(mod2_l.p)


mod3_l.p <- glm(cbind(Fruit_Yes, Fruit_No) ~ Hym_freq+Dip_freq+ Coleop_freq, data = l.p, family=binomial)
summary(mod3_l.p)
plot(mod3_l.p)


mod4_l.p <- glm(cbind(Fruit_Yes, Fruit_No) ~ richness08, data = l.p, family=binomial)
summary(mod4_l.p)
plot(mod4_l.p)


## Seed number ##

attach(l.p)
par(mfrow=c(2,3))
plot(Frequency,seed_number,xlab="Pollinator frequency",ylab="Seed number")
plot(richness,seed_number,xlab="Richness",ylab="Seed number")
plot(richness08,seed_number,xlab="Richness-80%",ylab="Seed number")
plot(Hym_freq,seed_number,xlab="Hymenoptera frequency",ylab="Seed number")
plot(Dip_freq,seed_number,xlab="Diptera frequency",ylab="Seed number")
plot(Coleop_freq,seed_number,xlab="Coleoptera frequency",ylab="Seed number")
detach(l.p)

xyplot(seed_number ~ Frequency, data = l.p, type = c("p", "r"))
xyplot(seed_number ~ Frequency | Site_ID, data = l.p, type = c("p", "r"))
xyplot(seed_number ~ richness | Site_ID, data = l.p, type = c("p", "r"))


# Variables correlacionadas, probar modelos con una variable
# random=Site_ID
#frequency
model1seed_l.p <- lme(seed_number ~ Frequency,random= ~ 1|Site_ID,data = l.p )
summary(model1seed_l.p)

plot(model1seed_l.p)
resi_seed <- residuals(model1seed_l.p)
qqnorm(resi_seed)
qqline(resi_seed, lty=3, col="red")

#richness
model2seed_l.p <- lme(seed_number ~ richness,random= ~ 1|Site_ID,data = l.p )
summary(model2seed_l.p)

plot(model2seed_l.p)
resi_seed2 <- residuals(model2seed_l.p)
qqnorm(resi_seed2)
qqline(resi_seed2, lty=3, col="red")

#frequency hymenopter, diptera, coleoptera
model3seed_l.p <- lme(seed_number ~ Hym_freq+Dip_freq+Coleop_freq,random= ~ 1|Site_ID,data = l.p )
summary(model3seed_l.p)

plot(model3seed_l.p)
resi_seed3 <- residuals(model3seed_l.p)
qqnorm(resi_seed3)
qqline(resi_seed3, lty=3, col="red")

#richness 80%
model4seed_l.p <- lme(seed_number ~ richness08,random= ~ 1|Site_ID,data = l.p)
summary(model4seed_l.p)

plot(model4seed_l.p)
resi_seed4 <- residuals(model4seed_l.p)
qqnorm(resi_seed4)
qqline(resi_seed4, lty=3, col="red")

#++++++++
# random= Year
#frequency
model1.1seed_l.p <- lme(seed_number ~ Frequency,random= ~ 1|Year,data = l.p)
summary(model1.1seed_l.p)

plot(model1.1seed_l.p)
resi_seed1 <- residuals(model1.1seed_l.p)
qqnorm(resi_seed1)
qqline(resi_seed1, lty=3, col="red")

#richness
model2.1seed_l.p <- lme(seed_number ~ richness,random= ~ 1|Year,data = l.p)
summary(model2.1seed_l.p)

plot(model2.1seed_l.p)
resi_seed2.1 <- residuals(model2.1seed_l.p)
qqnorm(resi_seed2.1)
qqline(resi_seed2.1, lty=3, col="red")

#frequency hymenopter, diptera, coleoptera
model3.1seed_l.p <- lme(seed_number ~ Hym_freq+Dip_freq+Coleop_freq,random= ~ 1|Year,data = l.p )
summary(model3.1seed_l.p)

plot(model3.1seed_l.p)
resi_seed3.1 <- residuals(model3.1seed_l.p)
qqnorm(resi_seed3.1)
qqline(resi_seed3.1, lty=3, col="red")

#richness 80%
model4.1seed_l.p <- lme(seed_number ~ richness08,random= ~ 1|Year,data = l.p)
summary(model4.1seed_l.p)

plot(model4.1seed_l.p)
resi_seed4.1 <- residuals(model4.1seed_l.p)
qqnorm(resi_seed4.1)
qqline(resi_seed4.1, lty=3, col="red")

#++++++++++++
# sin random factor
#frequency
model1.2seed_l.p <- lm(seed_number ~ Frequency,data = l.p)
summary(model1.2seed_l.p)

plot(model1.2seed_l.p)

#richness
model2.2seed_l.p <- lm(seed_number ~ richness,data = l.p)
summary(model2.2seed_l.p)
plot(model2.2seed_l.p)

#frequency hymenopter, diptera, coleoptera
model3.2seed_l.p <- lm(seed_number ~ Hym_freq+Dip_freq+Coleop_freq,data = l.p)
summary(model3.2seed_l.p)
plot(model3.2seed_l.p)

#richness 80%
model4.2seed_l.p <- lm(seed_number ~ richness08,data = l.p)
summary(model4.2seed_l.p)
plot(model4.2seed_l.p)


## Seed weight ##

attach(l.p)
par(mfrow=c(2,3))
plot(Frequency,Seed_weight,xlab="Pollinator frequency",ylab="Seed_weight")
plot(richness,Seed_weight,xlab="Richness",ylab="Seed_weight")
plot(richness08,Seed_weight,xlab="Richness-80%",ylab="Seed_weight")
plot(Hym_freq,Seed_weight,xlab="Hymenoptera frequency",ylab="Seed_weight")
plot(Dip_freq,Seed_weight,xlab="Diptera frequency",ylab="Seed_weight")
plot(Coleop_freq,Seed_weight,xlab="Coleoptera frequency",ylab="Seed_weight")
detach(l.p)

# random=Site_ID
#frequency
model1weigth_l.p <- lme(Seed_weight ~ Frequency,random= ~ 1|Site_ID,data = l.p)
summary(model1weigth_l.p)

plot(model1weigth_l.p)
resi_seed_w <- residuals(model1weigth_l.p)
qqnorm(resi_seed_w)
qqline(resi_seed_w, lty=3, col="red")

#richness
model2weigth_l.p <- lme(Seed_weight ~ richness,random= ~ 1|Site_ID,data = l.p )
summary(model2weigth_l.p)

plot(model2weigth_l.p)
resi_seed2_w <- residuals(model2weigth_l.p)
qqnorm(resi_seed2_w)
qqline(resi_seed2_w, lty=3, col="red")

#frequency hymenopter, diptera, coleoptera
model3weigth_l.p <- lme(Seed_weight ~ Hym_freq+Dip_freq+Coleop_freq,random= ~ 1|Site_ID,data = l.p)
summary(model3weigth_l.p)

plot(model3weigth_l.p)
resi_seed3_w <- residuals(model3weigth_l.p)
qqnorm(resi_seed3_w)
qqline(resi_seed3_w, lty=3, col="red")

#richness 80%
model4weigth_l.p <- lme(Seed_weight ~ richness08,random= ~ 1|Site_ID,data = l.p)
summary(model4weigth_l.p)

plot(model4weigth_l.p)
resi_seed4_w <- residuals(model4weigth_l.p)
qqnorm(resi_seed4_w)
qqline(resi_seed4_w, lty=3, col="red")

#++++++
# random= Year
#frequency
model1.1weight_l.p <- lme(Seed_weight ~ Frequency,random= ~ 1|Year,data = l.p)
summary(model1.1weight_l.p)

plot(model1.1weight_l.p)
resi_seed1.1_w <- residuals(model1.1weight_l.p)
qqnorm(resi_seed1.1_w)
qqline(resi_seed1.1_w, lty=3, col="red")

#richness
model2.1weight_l.p <- lme(Seed_weight ~ richness,random= ~ 1|Year,data = l.p)
summary(model2.1weight_l.p)

plot(model2.1weight_l.p)
resi_seed2.1_w <- residuals(model2.1weight_l.p)
qqnorm(resi_seed2.1_w)
qqline(resi_seed2.1_w, lty=3, col="red")

#frequency hymenopter, diptera, coleoptera
model3.1weight_l.p <- lme(Seed_weight ~ Hym_freq+Dip_freq+Coleop_freq,random= ~ 1|Year,data = l.p )
summary(model3.1weight_l.p)

plot(model3.1weight_l.p)
resi_seed3.1_w <- residuals(model3.1weight_l.p)
qqnorm(resi_seed3.1_w)
qqline(resi_seed3.1_w, lty=3, col="red")

#richness 80%
model4.1weight_l.p <- lme(Seed_weight ~ richness08,random= ~ 1|Year,data = l.p)
summary(model4.1weight_l.p)

plot(model4.1weight_l.p)
resi_seed4.1_w <- residuals(model4.1weight_l.p)
qqnorm(resi_seed4.1_w)
qqline(resi_seed4.1_w, lty=3, col="red")


#++++++++++++
# sin random factor
#frequency
model1.2weight_l.p <- lm(Seed_weight ~ Frequency,data = l.p)
summary(model1.2weight_l.p)

plot(model1.2weight_l.p)

#richness
model2.2weight_l.p <- lm(Seed_weight ~ richness,data = l.p)
summary(model2.2weight_l.p)
plot(model2.2weight_l.p)

#frequency hymenopter, diptera, coleoptera
model3.2weight_l.p <- lm(Seed_weight ~ Hym_freq+Dip_freq+Coleop_freq,data = l.p)
summary(model3.2weight_l.p)

plot(model3.2weight_l.p)

#richness 80%
model4.2weight_l.p <- lm(Seed_weight ~ richness08,data = l.p)
summary(model4.2weight_l.p)
plot(model4.2weight_l.p)


############################
## Lavandula stoechas ##


## Fruit propotion 

attach(l.s)
par(mfrow=c(2,3))
plot(Frequency,Fruit_proportion,xlab="Pollinator frequency",ylab="Fruit proportion")
plot(richness,Fruit_proportion,xlab="Richness",ylab="Fruit proportion")
plot(richness08,Fruit_proportion,xlab="Richness-80%",ylab="Fruit proportion")
plot(Hym_freq,Fruit_proportion,xlab="Hymenoptera frequency",ylab="Fruit proportion")
plot(Dip_freq,Fruit_proportion,xlab="Diptera frequency",ylab="Fruit proportion")
plot(Coleop_freq,Fruit_proportion,xlab="Coleoptera frequency",ylab="Fruit proportion")

detach(l.s)

# Variables correlacionadas, probar modelos con una variable
# random=Site_ID
#frequency
xyplot(Fruit_proportion ~ Frequency, data = l.s, type = c("p", "r"))
xyplot(Fruit_proportion ~ Frequency | Site_ID, data = l.s, type = c("p", "r"))
xyplot(Fruit_proportion ~ richness | Site_ID, data = l.s, type = c("p", "r"))

model1_l.s <- glmer(cbind(Fruit_Yes, Fruit_No) ~ Frequency + (1|Site_ID), data = l.s, family=binomial)
summary(model1_l.s)

simulationOutput <- simulateResiduals(fittedModel = model1_l.s)
plot(simulationOutput)


#richness
model2_l.s <- glmer(cbind(Fruit_Yes, Fruit_No) ~ richness + (1|Site_ID), data = l.s, family=binomial)
summary(model2_l.s)

simulationOutput2 <- simulateResiduals(fittedModel = model2_l.s)
plot(simulationOutput2)

#frequency hymenopter, diptera, coleoptera 
#coleoptero todo ceros-el analisis da error- sacarlos del analisis
model3_l.s <- glmer(cbind(Fruit_Yes, Fruit_No) ~ Hym_freq+Dip_freq+ (1|Site_ID), data = l.s, family=binomial)
summary(model3_l.s)

simulationOutput3 <- simulateResiduals(fittedModel = model3_l.s)
plot(simulationOutput3)

#richness 80%
model4_l.s<- glmer(cbind(Fruit_Yes, Fruit_No) ~ richness08 + (1|Site_ID), data = l.s, family=binomial)
summary(model4_l.s)

simulationOutput4 <- simulateResiduals(fittedModel = model4_l.s)
plot(simulationOutput4)

#++++++++++
# random= Year
xyplot(Fruit_proportion ~ Frequency | Year, data = l.s, type = c("p", "r"))

#frequency
model1.1_l.s <- glmer(cbind(Fruit_Yes, Fruit_No) ~ Frequency + (1|Year), data = l.s, family=binomial)
summary(model1.1_l.s)

simulationOutputY <- simulateResiduals(fittedModel = model1.1_l.s)
plot(simulationOutputY)

#richness
model2.1_l.s <- glmer(cbind(Fruit_Yes, Fruit_No) ~ richness + (1|Year), data = l.s, family=binomial)
summary(model2.1_l.s)

simulationOutput2Y <- simulateResiduals(fittedModel = model2.1_l.s)
plot(simulationOutput2Y)

#frequency hymenopter, diptera, coleoptera
model3.1_l.s<- glmer(cbind(Fruit_Yes, Fruit_No) ~ Hym_freq+Dip_freq+ (1|Year), data = l.s, family=binomial)
summary(model3.1_l.s)

simulationOutput3Y <- simulateResiduals(fittedModel = model3.1_l.s)
plot(simulationOutput3Y)

#richness 80%
model4.1_l.s <- glmer(cbind(Fruit_Yes, Fruit_No) ~ richness08 + (1|Year), data = l.s, family=binomial)
summary(model4.1_l.s)

simulationOutput4Y <- simulateResiduals(fittedModel = model4.1_l.s)
plot(simulationOutput4Y)

#+++++++++++
# sin random factor
mod_l.s <- glm(cbind(Fruit_Yes, Fruit_No) ~ Frequency, family=binomial, data = l.s)
summary(mod_l.s)
plot(mod_l.s)


mod2_l.s <- glm(cbind(Fruit_Yes, Fruit_No) ~ richness, data = l.s, family=binomial)
summary(mod2_l.s)
plot(mod2_l.s)


mod3_l.s <- glm(cbind(Fruit_Yes, Fruit_No) ~ Hym_freq+Dip_freq, data = l.s, family=binomial)
summary(mod3_l.s)
plot(mod3_l.s)


mod4_l.s <- glm(cbind(Fruit_Yes, Fruit_No) ~ richness08, data = l.s, family=binomial)
summary(mod4_l.s)
plot(mod4_l.s)


## Seed number ##

attach(l.s)
par(mfrow=c(2,3))
plot(Frequency,seed_number,xlab="Pollinator frequency",ylab="Seed number")
plot(richness,seed_number,xlab="Richness",ylab="Seed number")
plot(richness08,seed_number,xlab="Richness-80%",ylab="Seed number")
plot(Hym_freq,seed_number,xlab="Hymenoptera frequency",ylab="Seed number")
plot(Dip_freq,seed_number,xlab="Diptera frequency",ylab="Seed number")
plot(Coleop_freq,seed_number,xlab="Coleoptera frequency",ylab="Seed number")
detach(l.s)

xyplot(seed_number ~ Frequency, data = l.s, type = c("p", "r"))
xyplot(seed_number ~ Frequency | Site_ID, data = l.s, type = c("p", "r"))
xyplot(seed_number ~ richness | Site_ID, data = l.s, type = c("p", "r"))


# Variables correlacionadas, probar modelos con una variable
# random=Site_ID
#frequency
model1seed_l.s <- lme(seed_number ~ Frequency,random= ~ 1|Site_ID,data = l.s )
summary(model1seed_l.s)

plot(model1seed_l.s)
resi_seed <- residuals(model1seed_l.s)
qqnorm(resi_seed)
qqline(resi_seed, lty=3, col="red")

#richness
model2seed_l.s <- lme(seed_number ~ richness,random= ~ 1|Site_ID,data = l.s )
summary(model2seed_l.s)

plot(model2seed_l.s)
resi_seed2 <- residuals(model2seed_l.s)
qqnorm(resi_seed2)
qqline(resi_seed2, lty=3, col="red")

#frequency hymenopter, diptera, coleoptera
model3seed_l.s <- lme(seed_number ~ Hym_freq+Dip_freq,random= ~ 1|Site_ID,data = l.s )
summary(model3seed_l.s)

plot(model3seed_l.s)
resi_seed3 <- residuals(model3seed_l.s)
qqnorm(resi_seed3)
qqline(resi_seed3, lty=3, col="red")

#richness 80%
model4seed_l.s <- lme(seed_number ~ richness08,random= ~ 1|Site_ID,data = l.s)
summary(model4seed_l.s)

plot(model4seed_l.s)
resi_seed4 <- residuals(model4seed_l.s)
qqnorm(resi_seed4)
qqline(resi_seed4, lty=3, col="red")

#++++++++
# random= Year
#frequency
model1.1seed_l.s <- lme(seed_number ~ Frequency,random= ~ 1|Year,data = l.s)
summary(model1.1seed_l.s)

plot(model1.1seed_l.s)
resi_seed1 <- residuals(model1.1seed_l.s)
qqnorm(resi_seed1)
qqline(resi_seed1, lty=3, col="red")

#richness
model2.1seed_l.s <- lme(seed_number ~ richness,random= ~ 1|Year,data = l.s)
summary(model2.1seed_l.s)

plot(model2.1seed_l.s)
resi_seed2.1 <- residuals(model2.1seed_l.s)
qqnorm(resi_seed2.1)
qqline(resi_seed2.1, lty=3, col="red")

#frequency hymenopter, diptera, coleoptera
model3.1seed_l.s <- lme(seed_number ~ Hym_freq+Dip_freq,random= ~ 1|Year,data = l.s )
summary(model3.1seed_l.s)

plot(model3.1seed_l.s)
resi_seed3.1 <- residuals(model3.1seed_l.s)
qqnorm(resi_seed3.1)
qqline(resi_seed3.1, lty=3, col="red")

#richness 80%
model4.1seed_l.s <- lme(seed_number ~ richness08,random= ~ 1|Year,data = l.s)
summary(model4.1seed_l.s)

plot(model4.1seed_l.s)
resi_seed4.1 <- residuals(model4.1seed_l.s)
qqnorm(resi_seed4.1)
qqline(resi_seed4.1, lty=3, col="red")

#++++++++++++
# sin random factor
#frequency
model1.2seed_l.s <- lm(seed_number ~ Frequency,data = l.s)
summary(model1.2seed_l.s)

plot(model1.2seed_l.s)

#richness
model2.2seed_l.s <- lm(seed_number ~ richness,data = l.s)
summary(model2.2seed_l.s)
plot(model2.2seed_l.s)

#frequency hymenopter, diptera, coleoptera
model3.2seed_l.s <- lm(seed_number ~ Hym_freq+Dip_freq,data = l.s)
summary(model3.2seed_l.s)
plot(model3.2seed_l.s)

#richness 80%
model4.2seed_l.s <- lm(seed_number ~ richness08,data = l.s)
summary(model4.2seed_l.s)
plot(model4.2seed_l.s)


## Seed weight ##

attach(l.s)
par(mfrow=c(2,3))
plot(Frequency,Seed_weight,xlab="Pollinator frequency",ylab="Seed_weight")
plot(richness,Seed_weight,xlab="Richness",ylab="Seed_weight")
plot(richness08,Seed_weight,xlab="Richness-80%",ylab="Seed_weight")
plot(Hym_freq,Seed_weight,xlab="Hymenoptera frequency",ylab="Seed_weight")
plot(Dip_freq,Seed_weight,xlab="Diptera frequency",ylab="Seed_weight")
plot(Coleop_freq,Seed_weight,xlab="Coleoptera frequency",ylab="Seed_weight")
detach(l.s)

# random=Site_ID
#frequency
model1weigth_l.s <- lme(Seed_weight ~ Frequency,random= ~ 1|Site_ID,data = l.s)
summary(model1weigth_l.s)

plot(model1weigth_l.s)
resi_seed_w <- residuals(model1weigth_l.s)
qqnorm(resi_seed_w)
qqline(resi_seed_w, lty=3, col="red")

#richness
model2weigth_l.s <- lme(Seed_weight ~ richness,random= ~ 1|Site_ID,data = l.s )
summary(model2weigth_l.s)

plot(model2weigth_l.s)
resi_seed2_w <- residuals(model2weigth_l.s)
qqnorm(resi_seed2_w)
qqline(resi_seed2_w, lty=3, col="red")

#frequency hymenopter, diptera, coleoptera
model3weigth_l.s <- lme(Seed_weight ~ Hym_freq+Dip_freq,random= ~ 1|Site_ID,data = l.s)
summary(model3weigth_l.s)

plot(model3weigth_l.s)
resi_seed3_w <- residuals(model3weigth_l.s)
qqnorm(resi_seed3_w)
qqline(resi_seed3_w, lty=3, col="red")

#richness 80%
model4weigth_l.s <- lme(Seed_weight ~ richness08,random= ~ 1|Site_ID,data = l.s)
summary(model4weigth_l.s)

plot(model4weigth_l.s)
resi_seed4_w <- residuals(model4weigth_l.s)
qqnorm(resi_seed4_w)
qqline(resi_seed4_w, lty=3, col="red")

#++++++
# random= Year
#frequency
model1.1weight_l.s <- lme(Seed_weight ~ Frequency,random= ~ 1|Year,data = l.s)
summary(model1.1weight_l.s)

plot(model1.1weight_l.s)
resi_seed1.1_w <- residuals(model1.1weight_l.s)
qqnorm(resi_seed1.1_w)
qqline(resi_seed1.1_w, lty=3, col="red")

#richness
model2.1weight_l.s <- lme(Seed_weight ~ richness,random= ~ 1|Year,data = l.s)
summary(model2.1weight_l.s)

plot(model2.1weight_l.s)
resi_seed2.1_w <- residuals(model2.1weight_l.s)
qqnorm(resi_seed2.1_w)
qqline(resi_seed2.1_w, lty=3, col="red")

#frequency hymenopter, diptera, coleoptera
model3.1weight_l.s <- lme(Seed_weight ~ Hym_freq+Dip_freq,random= ~ 1|Year,data = l.s)
summary(model3.1weight_l.s)

plot(model3.1weight_l.s)
resi_seed3.1_w <- residuals(model3.1weight_l.s)
qqnorm(resi_seed3.1_w)
qqline(resi_seed3.1_w, lty=3, col="red")

#richness 80%
model4.1weight_l.s <- lme(Seed_weight ~ richness08,random= ~ 1|Year,data = l.s)
summary(model4.1weight_l.s)

plot(model4.1weight_l.s)
resi_seed4.1_w <- residuals(model4.1weight_l.s)
qqnorm(resi_seed4.1_w)
qqline(resi_seed4.1_w, lty=3, col="red")


#++++++++++++
# sin random factor
#frequency
model1.2weight_l.s <- lm(Seed_weight ~ Frequency,data = l.s)
summary(model1.2weight_l.s)

plot(model1.2weight_l.s)

#richness
model2.2weight_l.s <- lm(Seed_weight ~ richness,data = l.s)
summary(model2.2weight_l.s)
plot(model2.2weight_l.s)

#frequency hymenopter, diptera, coleoptera
model3.2weight_l.s <- lm(Seed_weight ~ Hym_freq+Dip_freq,data = l.s)
summary(model3.2weight_l.s)

plot(model3.2weight_l.s)

#richness 80%
model4.2weight_l.s <- lm(Seed_weight ~ richness08,data = l.s)
summary(model4.2weight_l.s)
plot(model4.2weight_l.s)



#####################
##      Total      ##
#####################


# Fruit proportion #

xyplot(Fruit_proportion ~ Frequency, data = total, type = c("p", "r"))
xyplot(Fruit_proportion ~ Frequency|Plant_sp, data = total, type = c("p", "r"))
xyplot(Fruit_proportion ~ richness, data = total, type = c("p", "r"))
xyplot(Fruit_proportion ~ richness|Plant_sp, data = total, type = c("p", "r"))
xyplot(Fruit_proportion ~ Hym_freq+Dip_freq+Coleop_freq, data = total, type = c("p", "r"))
xyplot(Fruit_proportion ~ Hym_freq+Dip_freq+Coleop_freq|Plant_sp, data = total, type = c("p", "r"))


model1_total <- glmer(cbind(Fruit_Yes, Fruit_No) ~ Frequency + (1|Plant_sp), data = total, family=binomial)
summary(model1_total)
model1.1_total <- glmer(cbind(Fruit_Yes, Fruit_No) ~ scale(Frequency) + (1|Plant_sp), data = total, family=binomial)
summary(model1.1_total)
simulationOutput <- simulateResiduals(fittedModel = model1.1_total)
plot(simulationOutput)
testOutliers(simulationOutput,type ="bootstrap")


model2_total <- glmer(cbind(Fruit_Yes, Fruit_No) ~ richness + (1|Plant_sp), data = total, family=binomial)
summary(model2_total)
simulationOutput <- simulateResiduals(fittedModel = model2_total)
plot(simulationOutput)


model3_total <- glmer(cbind(Fruit_Yes, Fruit_No) ~ Hym_freq+Dip_freq+ Coleop_freq+ (1|Plant_sp), data = total, family=binomial)
summary(model3_total)

model3.1_total <- glmer(cbind(Fruit_Yes, Fruit_No) ~ scale(Hym_freq)+Dip_freq+ Coleop_freq+ (1|Plant_sp), data = total, family=binomial)
summary(model3.1_total)
simulationOutput <- simulateResiduals(fittedModel = model3.1_total)
plot(simulationOutput)


model4_total <- glmer(cbind(Fruit_Yes, Fruit_No) ~ richness08+ (1|Plant_sp), data = total, family=binomial)
summary(model4_total)
simulationOutput <- simulateResiduals(fittedModel = model4_total)
plot(simulationOutput)


# seed_number #

model1seed_total <- lme(seed_number ~ Frequency,random= ~ 1|Plant_sp,data = total )
summary(model1seed_total)
plot(model1seed_total)
resi_seed <- residuals(model1seed_total)
qqnorm(resi_seed)
qqline(resi_seed, lty=3, col="red")


model2seed_total <- lme(seed_number ~ richness,random= ~ 1|Plant_sp,data = total )
summary(model2seed_total)
plot(model2seed_total)
resi_seed <- residuals(model2seed_total)
qqnorm(resi_seed)
qqline(resi_seed, lty=3, col="red")


model3seed_total <- lme(seed_number ~ Hym_freq+Dip_freq+Coleop_freq,random= ~ 1|Plant_sp,data = total )
summary(model3seed_total)
plot(model3seed_total)
resi_seed <- residuals(model3seed_total)
qqnorm(resi_seed)
qqline(resi_seed, lty=3, col="red")


model4seed_total <- lme(seed_number ~ richness08,random= ~ 1|Plant_sp,data = total )
summary(model4seed_total)
plot(model4seed_total)
resi_seed <- residuals(model4seed_total)
qqnorm(resi_seed)
qqline(resi_seed, lty=3, col="red")


# seed_weight #

model1weight_total <- lme(Seed_weight ~ Frequency,random= ~ 1|Plant_sp,data = total )
summary(model1weight_total)
plot(model1weight_total)
resi_seed <- residuals(model1weight_total)
qqnorm(resi_seed)
qqline(resi_seed, lty=3, col="red")


model2weight_total <- lme(Seed_weight ~ richness,random= ~ 1|Plant_sp,data = total )
summary(model2weight_total)
plot(model2weight_total)
resi_seed <- residuals(model2weight_total)
qqnorm(resi_seed)
qqline(resi_seed, lty=3, col="red")


model3weight_total <- lme(Seed_weight ~ Hym_freq+Dip_freq+Coleop_freq,random= ~ 1|Plant_sp,data = total )
summary(model3weight_total)
plot(model3weight_total)
resi_seed <- residuals(model3weight_total)
qqnorm(resi_seed)
qqline(resi_seed, lty=3, col="red")


model4weight_total <- lme(Seed_weight ~ richness08,random= ~ 1|Plant_sp,data = total )
summary(model4weight_total)
plot(model4weight_total)
resi_seed <- residuals(model4weight_total)
qqnorm(resi_seed)
qqline(resi_seed, lty=3, col="red")
