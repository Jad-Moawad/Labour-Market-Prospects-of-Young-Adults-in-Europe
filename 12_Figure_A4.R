#Install the packages below in case they are not installed. 
#install.packages(ggplot2)
#install.packages(ggpubr)

#Extract the packages
library(ggplot2)
library(ggpubr)

#Clear the data environment
rm(list = ls())


#France

#Import the merged data set that is constructed in "0_setting_data"
load("directory_path_file/m.Rda")

#Keep only working people and choosing the country of interest 
m= m %>% filter(mainact==1&cntry=="France") 

g1= ggplot(m, aes(x=income)) + 
  geom_density(color="black", fill="gray")+
  #scale_x_continuous(limits = c(-1, 13), breaks = seq(0, 12, by = 4)) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_continuous(limits = c(0, 50000), breaks = seq(0, 50000, by = 20000))+
  ggtitle("Earnings")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

g2= ggplot(m, aes(x=ln_wage)) + 
  geom_density(color="black", fill="gray")+
  #scale_x_continuous(limits = c(-1, 13), breaks = seq(0, 12, by = 4)) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 3))+
  ggtitle("Log earnings")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

figure1 <- ggarrange(g1, g2, ncol=2, nrow=1)
figure1= annotate_figure(figure1, top = text_grob("France", color = "Black", face = "bold", size = 15))

#Germany

#Import the merged data set that is constructed in "0_setting_data"
load("directory_path_file/m.Rda")

#Keep only working people and choosing the country of interest 
m= m %>% filter(mainact==1&cntry=="Germany") 

g1= ggplot(m, aes(x=income)) + 
  geom_density(color="black", fill="gray")+
  xlab(NULL) +
  ylab(NULL) +
  scale_x_continuous(limits = c(0, 50000), breaks = seq(0, 50000, by = 20000))+
  ggtitle("Earnings")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

g2= ggplot(m, aes(x=ln_wage)) + 
  geom_density(color="black", fill="gray")+
  scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 3))+
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Log earnings")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

figure2 <- ggarrange(g1, g2, ncol=2, nrow=1)
figure2= annotate_figure(figure2, top = text_grob("Germany", color = "Black", face = "bold", size = 15))

#Italy

#Import the merged data set that is constructed in "0_setting_data"
load("directory_path_file/m.Rda")

#Keep only working people and choosing the country of interest 
m= m %>% filter(mainact==1&cntry=="Italy") 

g1= ggplot(m, aes(x=income)) + 
  geom_density(color="black", fill="gray")+
  xlab(NULL) +
  ylab(NULL) +
  scale_x_continuous(limits = c(0, 50000), breaks = seq(0, 50000, by = 20000))+
  ggtitle("Earnings")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

g2= ggplot(m, aes(x=ln_wage)) + 
  geom_density(color="black", fill="gray")+
  xlab(NULL) +
  ylab(NULL) +
  scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 3))+
  ggtitle("Log earnings")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

figure3 <- ggarrange(g1, g2, ncol=2, nrow=1)
figure3= annotate_figure(figure3, top = text_grob("Italy", color = "Black", face = "bold", size = 15))


#Poland

#Import the merged data set that is constructed in "0_setting_data"
load("directory_path_file/m.Rda")

#Keep only working people and choosing the country of interest 
m= m %>% filter(mainact==1&cntry=="Poland") 

g1= ggplot(m, aes(x=income)) + 
  geom_density(color="black", fill="gray")+
  xlab(NULL) +
  ylab(NULL) +
  scale_x_continuous(limits = c(0, 50000), breaks = seq(0, 50000, by = 20000))+
  ggtitle("Earnings")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

g2= ggplot(m, aes(x=ln_wage)) + 
  geom_density(color="black", fill="gray")+
  xlab(NULL) +
  ylab(NULL) +
  scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 3))+
  ggtitle("Log earnings")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

figure4 <- ggarrange(g1, g2, ncol=2, nrow=1)
figure4= annotate_figure(figure4, top = text_grob("Poland", color = "Black", face = "bold", size = 15))

#Spain

#Import the merged data set that is constructed in "0_setting_data"
load("directory_path_file/m.Rda")


#Keep only working people and choosing the country of interest 
m= m %>% filter(mainact==1&cntry=="Spain") 

g1= ggplot(m, aes(x=income)) + 
  geom_density(color="black", fill="gray")+
  xlab(NULL) +
  ylab(NULL) +
  scale_x_continuous(limits = c(0, 50000), breaks = seq(0, 50000, by = 20000))+
  ggtitle("Earnings")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

g2= ggplot(m, aes(x=ln_wage)) + 
  geom_density(color="black", fill="gray")+
  xlab(NULL) +
  ylab(NULL) +
  scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 3))+
  ggtitle("Log earnings")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

figure5 <- ggarrange(g1, g2, ncol=2, nrow=1)
figure5= annotate_figure(figure5, top = text_grob("Spain", color = "Black", face = "bold", size = 15))


#United Kingdom

#Import the merged data set that is constructed in "0_setting_data"
load("directory_path_file/m.Rda")

#Keep only working people and choosing the country of interest 
m= m %>% filter(mainact==1&cntry=="United Kingdom") 

g1= ggplot(m, aes(x=income)) + 
  geom_density(color="black", fill="gray")+
  xlab(NULL) +
  ylab(NULL) +
  scale_x_continuous(limits = c(0, 50000), breaks = seq(0, 50000, by = 20000))+
  ggtitle("Earnings")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

g2= ggplot(m, aes(x=ln_wage)) + 
  geom_density(color="black", fill="gray")+
  xlab(NULL) +
  ylab(NULL) +
  scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 3))+
  ggtitle("Log earnings")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

figure6 <- ggarrange(g1, g2, ncol=2, nrow=1)
figure6= annotate_figure(figure6, top = text_grob("United Kingdom", color = "Black", face = "bold", size = 15))


#Merge all countries in one figure
figure <- ggarrange(figure1, figure2, figure3, figure4, figure5, figure6, ncol=2, nrow=3)
figure= annotate_figure(figure, left = text_grob("Density", color = "Black", face = "bold", size = 18, rot = 90))

#Save the figure
ggsave(file="Figure_A4.pdf", width = 28, height = 20, dpi= 800, paper="USr", figure)




