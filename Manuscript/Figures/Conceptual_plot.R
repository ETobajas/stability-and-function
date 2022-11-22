
library(ggplot2)


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
  geom_line(color = "#e41a1c", size=1.3) +
  geom_line(data =  d2, aes(s2,l2), color = "#377eb8",size=1.3) +
  geom_line(data =  d3, aes(s3,l3), color = "#4daf4a",size=1.3) +
  ylab("Reproductive success")+
  labs(x = "") +
  theme_classic()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  scale_x_continuous(breaks=seq(from= 0, to= 100, by = 50), labels=c("V1","V2","V3"))+
  theme(text = element_text(size=15,face="bold"))+
  theme(axis.ticks.x=element_blank())+
  annotate("rect", xmin = c(6, 60), xmax = c(15,80), ymin = c(0.6, 1.5), 
           ymax = c(1.6, 2.6), linetype = 1, fill="gray88", alpha=0.2, colour = "black") +
  annotate("text", x = c(98, 98,98), y = c(2.67, 2.31,1.95), label = c("R1", "R2", "R3"), size=4)+
  theme(axis.line.x = element_line(arrow = arrow()))+
  theme(axis.line.y = element_line(arrow = arrow()))



##################################
x <- seq(0, 40, by = 0.1)
y1 <- sin(x)

da1 <- data.frame( x, y1)


R_1=ggplot(da1, aes(x, y1))+
  geom_line( size=1) +
  geom_hline(yintercept=0, color= "#e41a1c", size=1) +
  theme_classic() +
  xlab("Time") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  scale_y_continuous(breaks=seq(from= -1, to= 1, by = 2), labels=c("R3","R1"))+
  theme(text = element_text(size=10, face="bold"))+
  theme(axis.title.y = element_blank()) +
  annotate("text", x = 42, y = 0.15, label = "Rep", size=3,color="#e41a1c")+
  annotate("text", x = 42, y = 0.8, label = "R", size=4)



###################################

x2 <- seq(0, 40, by = 1)
set.seed(4)
x3=runif(x2, min = 0, max = 40)
y2 <- sin(x2 *x3)
y3 = cos(x2 *x3)

da2 <- data.frame(x2, y2)
da3 = data.frame(x2,y3)

v_1=ggplot(da2, aes(x2, y2))+
  geom_line( size=1) +
  geom_line(data =  da3, aes(x2,y3), color = "#e41a1c",size=1)+
  theme_classic() +
  xlab("Time")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ 
  scale_y_continuous(breaks=seq(from= -1, to= 1, by = 2), labels=c("V1","V3"))+
  theme(text = element_text(size=10, face="bold"))+
  theme(axis.title.y = element_blank())+
  annotate("text", x = 42, y = 0.8, label = "Rep", size=3, color="#e41a1c")+
  annotate("text", x = 41.5, y = -0.4, label = "V", size=4)




######################
x3 <- seq(0, 50, by = 0.1)
y3 <- sin(x3)

da3 <- data.frame(x3, y3)


v_2=ggplot(da3, aes(x3, y3))+
  geom_line( size=1) +
  geom_hline(yintercept=0, color= "#e41a1c", size=1) +
  theme_classic() +
  xlab("Time") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  scale_y_continuous(breaks=seq(from= -1, to= 1, by = 2), labels=c("V1","V3"))+
  theme(text = element_text(size=10, face="bold"))+
  theme(axis.title.y = element_blank()) +
  annotate("text", x = 52, y = 0.15, label = "Rep", size=3,color="#e41a1c")+
  annotate("text", x = 52, y = -0.3, label = "V", size=4)



########################################
x4 <- seq(0, 45, by = 1)
set.seed(4)
x5=runif(x4, min = 0, max = 40)
y4 <- sin(x4 *x5)
y5 = cos(x4 *x4)

da4 <- data.frame(x4, y4)
da5 = data.frame(x4,y5)

R_2=ggplot(da4, aes(x4, y4))+
  geom_line( size=1) +
  geom_line(data =  da5, aes(x4,y5), color = "#e41a1c",size=1)+
  theme_classic() +
  xlab("Time")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ 
  scale_y_continuous(breaks=seq(from= -1, to= 1, by = 2), labels=c("R3","R1"))+
  theme(text = element_text(size=10, face="bold"))+
  theme(axis.title.y = element_blank())+
  annotate("text", x = 47, y = -0.2, label = "Rep", size=3, color="#e41a1c")+
  annotate("text", x = 47, y = 0.8, label = "R", size=4)


########################################




design= c(area(1,1,1,2),
          area(2,1))


(to + (R_2 /v_2) + plot_layout( widths = c(3, 1)))/ (((v_1 + plot_spacer())/ (R_1 + plot_spacer()))) + plot_layout(design = design)

