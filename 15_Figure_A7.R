#Install the packages below in case they are not installed. 
#install.packages(ggplot2)
#install.packages(ggpubr)
#install.packages(readxl)
#install.packages(tidyverse)

#Extract the packages
library(ggplot2)
library(ggpubr)
library(readxl)
library(tidyverse)

#Clear the data environment
rm(list = ls())

#Import the excel sheet "income_inequality".
inequality <- readxl::read_excel("directory_path_file/income_inequality.xlsx")

#Data is given a short name for brevity reasons
d = inequality

#Import and arrange the data
ine <- gather(d, key = year, value = i_inequality, `2005`:`2011`)
ine = arrange(ine, cntry, year)
ine$year = as.numeric(ine$year)

df=ine  
d_de <- filter(df, cntry == 'Germany'& (year==2005 | year==2011))
d_es <- filter(df, cntry == 'Spain'& (year==2005 | year==2011))
d_fr <- filter(df, cntry == 'France' & (year==2005 | year==2011))
d_uk <- filter(df, cntry == 'United Kingdom'& (year==2005 | year==2011))
d_it <- filter(df, cntry == 'Italy'& (year==2005 | year==2011))
d_po <- filter(df, cntry == 'Poland'& (year==2005 | year==2011))

#Plot the figures separately by country
g1= ggplot(df, aes(year, i_inequality), color=cntry)+
  geom_line(aes(group=cntry, color=cntry), size=1.5)+
  geom_point(data=d_fr, size=3.5)+
  geom_line(data = df[df$cntry == 'France',], aes(group=cntry, color=cntry), size=2.2)+
  scale_x_continuous(limits = c(2004.5, 2011.5), breaks = seq(2005, 2011, by = 1)) +
  geom_text(data = slice_max(d_fr, i_inequality, n = 1), aes(year, i_inequality, label = round(i_inequality, 3)), hjust = 1.3, size=5) +
  geom_text(data = slice_min(d_fr, i_inequality, n = 1), aes(year, i_inequality, label = round(i_inequality, 3)), hjust = -0.35, size=5) +
  ylab(NULL)+xlab(NULL)+ggtitle("France")+
  scale_color_manual(values=c('black','snow3','snow3','snow3','snow3','snow3')) + 
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size=15))



g2= ggplot(df, aes(year, i_inequality), color=cntry)+
  geom_line(data=df,aes(group=cntry, color=cntry), size=1.5)+
  geom_line(data = df[df$cntry == 'Germany',], aes(group=cntry, color=cntry), size=2.2)+
  scale_x_continuous(limits = c(2004.5, 2011.5), breaks = seq(2005, 2011, by = 1)) +
  geom_point(data=d_de, size=3.5)+
  geom_text(data = slice_max(d_de, i_inequality, n = 1), aes(year, i_inequality, label = round(i_inequality, 3)), hjust = 1.3, size=5) +
  geom_text(data = slice_min(d_de, i_inequality, n = 1), aes(year, i_inequality, label = round(i_inequality, 3)), hjust = -0.35, size=5) +
  ylab(NULL)+xlab(NULL)+ggtitle("Germany")+
  scale_color_manual(values=c('snow3','black','snow3','snow3','snow3','snow3')) + 
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size=15))

g3= ggplot(df, aes(year, i_inequality), color=cntry)+
  geom_line(data=df,aes(group=cntry, color=cntry), size=1.5)+
  geom_line(data = df[df$cntry == 'Poland',], aes(group=cntry, color=cntry), size=2.2)+
  scale_x_continuous(limits = c(2004.5, 2011.5), breaks = seq(2005, 2011, by = 1)) +
  geom_point(data=d_po, size=3.5)+
  geom_text(data = slice_max(d_po, i_inequality, n = 1), aes(year, i_inequality, label = round(i_inequality, 3)), hjust = 1.3, size=5) +
  geom_text(data = slice_min(d_po, i_inequality, n = 1), aes(year, i_inequality, label = round(i_inequality, 3)), hjust = -0.35, size=5) +
  ylab(NULL)+xlab(NULL)+ggtitle("Poland")+
  scale_color_manual(values=c('snow3','snow3','snow3','black','snow3','snow3')) + 
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size=15))


g4= ggplot(df, aes(year, i_inequality), color=cntry)+
  geom_line(data=df,aes(group=cntry, color=cntry), size=1.5)+
  geom_line(data = df[df$cntry == 'United Kingdom',], aes(group=cntry, color=cntry), size=2.2)+
  scale_x_continuous(limits = c(2004.5, 2011.5), breaks = seq(2005, 2011, by = 1)) +
  geom_point(data=d_uk, size=3.5)+
  geom_text(data = slice_max(d_uk, i_inequality, n = 1), aes(year, i_inequality, label = round(i_inequality, 3)), hjust = -.35, size=5) +
  geom_text(data = slice_min(d_uk, i_inequality, n = 1), aes(year, i_inequality, label = round(i_inequality, 3)), hjust = 1.3, size=5) +
  ylab(NULL)+xlab(NULL)+ggtitle("United Kingdom")+
  scale_color_manual(values=c('snow3','snow3','snow3','snow3','snow3','black')) + 
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size=15),
        axis.text.x = element_text(color="black", size=15)
  )

g5= ggplot(df, aes(year, i_inequality), color=cntry)+
  geom_line(data=df,aes(group=cntry, color=cntry), size=1.5)+
  geom_line(data = df[df$cntry == 'Spain',], aes(group=cntry, color=cntry), size=2.2)+
  scale_x_continuous(limits = c(2004.5, 2011.5), breaks = seq(2005, 2011, by = 1)) +
  geom_point(data=d_es, size=3.5)+
  geom_text(data = slice_max(d_es, i_inequality, n = 1), aes(year, i_inequality, label = round(i_inequality, 3)), hjust = 1.3, size=5) +
  geom_text(data = slice_min(d_es, i_inequality, n = 1), aes(year, i_inequality, label = round(i_inequality, 3)), hjust = -0.35, size=5) +
  ylab(NULL)+xlab(NULL)+ggtitle("Spain")+
  scale_color_manual(values=c('snow3','snow3','snow3','snow3','black','snow3')) + 
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))

g6 = ggplot(df, aes(year, i_inequality), color=cntry)+
  geom_line(data=df,aes(group=cntry, color=cntry), size=1.5)+
  geom_line(data = df[df$cntry == 'Italy',], aes(group=cntry, color=cntry), size=2.2)+
  scale_x_continuous(limits = c(2004.5, 2011.5), breaks = seq(2005, 2011, by = 1)) +
  geom_point(data=d_it, size=3.5)+
  geom_text(data = slice_max(d_it, i_inequality, n = 1), aes(year, i_inequality, label = round(i_inequality, 3)), hjust = 1.3, size=5) +
  geom_text(data = slice_min(d_it, i_inequality, n = 1), aes(year, i_inequality, label = round(i_inequality, 3)), hjust = -0.35, size=5) +
  ylab(NULL)+xlab(NULL)+ggtitle("Italy")+
  scale_color_manual(values=c('snow3','snow3','black','snow3','snow3','snow3')) + 
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size=15))

#Gather all figures
figure2 <- ggarrange(g3, g2, g1, g6, g5,g4, ncol=2, nrow=3)
ggsave("Figure_A7.pdf", scale=1.35, width = 10, height = 7, dpi= 2300,limitsize = FALSE,units = c("in"))
