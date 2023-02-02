
library(ggplot2)
library(patchwork)


#First dataset RED
s1 <- seq(from=0, to=100, by=0.5)
l1 = log(s1, base = 6)
d1 = data.frame(l1, s1)


#Second dataset BLUE
s2 <- seq(from=0, to=100, by=0.5)
l2 = log(s2, base = 8)
d2 = data.frame(l2, s2)

#Third dataset GREEN
s3 <- seq(from=0, to=100, by=0.5)
l3 = log(s3, base = 12)
d3 = data.frame(l3, s3)



to=ggplot(d1, aes(s1, l1)) +
  geom_line(color = "#e41a1c", linewidth=1.3) +
  geom_line(data =  d2, aes(s2,l2), color = "#377eb8",linewidth=1.3) +
  geom_line(data =  d3, aes(s3,l3), color = "#4daf4a",linewidth=1.3) +
  ylab("Reproductive success")+
  labs(x = "") +
  theme_classic()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  scale_x_continuous(breaks=seq(from= 0, to= 100, by = 20), labels=c("V1","V2","V3", "V4","V5","V6"))+
  theme(text = element_text(size=15,face="bold"))+
  theme(axis.ticks.x=element_blank())+
  annotate("rect", xmin = c(0, 60), xmax = c(20,80), ymin = c(0.4, 1.35), 
           ymax = c(1.75, 2.7), linetype = 1, fill="gray88", alpha=0.3, colour = "black") +
  annotate("text", x = c(98, 98,98), y = c(2.67, 2.31,1.95), label = c("R3", "R2", "R1"), size=4)+
  annotate("text", x = c(1.25, 61.3), y = c(1.66,2.62), label = c("A", "B"), size=6)+
  theme(axis.line.x = element_line(arrow = arrow()))+
  theme(axis.line.y = element_line(arrow = arrow()))



##################################
x <- seq(0, 41, by = 0.1)
y1 <- sin(x)
y1.2 <- cos(x+4.3)

da1 <- data.frame( x, y1)
da1.2 <- data.frame( x, y1.2)

R_1=ggplot(da1, aes(x, y1))+
  geom_line( size=1) +
  geom_line(data =  da1.2, aes(x, y1.2), color = "#e41a1c",size=1)+
  theme_classic() +
  xlab("Time") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  scale_y_continuous(breaks=seq(from= -1, to= 1, by = 2), labels=c("R1","R3"))+
  theme(text = element_text(size=10, face="bold"))+
  theme(axis.title.y = element_blank()) +
  annotate("text", x = 42, y = 0.4, label = "Rep", size=3,color="#e41a1c")+
  annotate("text", x = 42, y = 0, label = "R", size=4)+ 
  theme(aspect.ratio=0.5)



###################################

x2 <- seq(0, 41, by = 0.1)
y2 <- sin(x2)
y3 = sin(x2/2.5)

da2 <- data.frame(x2, y2)
da2.1 = data.frame(x2,y3)

v_1=ggplot(da2, aes(x2, y2))+
  geom_line( size=1) +
  geom_line(data =  da2.1, aes(x2,y3), color = "#e41a1c",size=1)+
  theme_classic() +
  xlab("Time")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ 
  scale_y_continuous(breaks=seq(from= -1, to= 1, by = 2), labels=c("V1","V2"))+
  theme(text = element_text(size=10, face="bold"))+
  theme(axis.title.y = element_blank())+
  annotate("text", x = 42, y = -0.5, label = "Rep", size=3, color="#e41a1c")+
  annotate("text", x = 41.5, y = 0, label = "V", size=4) + 
  theme(aspect.ratio=0.5)



po1=R_1/v_1& theme(plot.background = element_rect(color = "black", size = 1))

######################
x3 <- seq(0, 57, by = 0.1)
y3 <- sin(x3)
y3.1 <- cos(x3+4.3)

da3 <- data.frame(x3, y3)
da3.1 <- data.frame(x3, y3.1)

v_2=ggplot(da3, aes(x3, y3))+
  geom_line( size=1) +
  geom_line(data =  da3.1, aes(x3,y3.1), color = "#e41a1c",size=1)+
  theme_classic() +
  xlab("Time") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  scale_y_continuous(breaks=seq(from= -1, to= 1, by = 2), labels=c("V4","V5"))+
  theme(text = element_text(size=10, face="bold"))+
  theme(axis.title.y = element_blank()) +
  annotate("text", x = 58.5, y = 0, label = "Rep", size=3,color="#e41a1c")+
  annotate("text", x = 58, y = 0.3, label = "V", size=4)+ 
  theme(aspect.ratio=0.5)




########################################
x4 <- seq(0, 57, by = 0.1)
y4 <- sin(x4)
y5 = sin(x4/2.5)


da4 <- data.frame(x4, y4)
da5 = data.frame(x4,y5)

R_2=ggplot(da4, aes(x4, y4))+
  geom_line( size=1) +
  geom_line(data =  da5, aes(x4,y5), color = "#e41a1c",size=1)+
  theme_classic() +
  xlab("Time")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ 
  scale_y_continuous(breaks=seq(from= -1, to= 1, by = 2), labels=c("R1","R3"))+
  theme(text = element_text(size=10, face="bold"))+
  theme(axis.title.y = element_blank())+
  annotate("text", x = 58.5, y = -0.6, label = "Rep", size=3, color="#e41a1c")+
  annotate("text", x = 58, y = 0.4, label = "R", size=4)+ 
  theme(aspect.ratio=0.5)


po2=v_2/R_2& theme(plot.background = element_rect(color = "black", size = 1))

########################################

design <- "
  AA#
  BC#
"

to+po1+v_2+R_2+ plot_layout(heights  = c(3,1,1),design = design)

to / ((v_1 / R_1)|(v_2 / R_2))

to/ (po1 |po2)
 
to - (po1 | po2) + plot_layout(ncol=1)
