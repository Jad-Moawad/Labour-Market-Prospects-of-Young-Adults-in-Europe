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

#Import the excel sheet "OECD_macro_unemployment_rate".
unemployment <- readxl::read_excel("directory_path_file/OECD_macro_unemployment_rate.xlsx")
unemp <- gather(unemployment, key = year, value = unemp, `2002`:`2014`)

unemp$cntry = as.factor(unemp$cntry)

g1 <- filter(unemp, cntry=="Italy" | cntry=="Poland" | cntry=="Spain"| cntry=="France" | cntry=="Germany" | cntry=="United Kingdom") 
g1 <- filter(g1, year<2015) 
g1 = arrange(g1, cntry, year)

g1$year = as.numeric(g1$year)

df=g1  

d_de <- filter(df, cntry == 'Germany'& (year==2002 | year==2014))
d_es <- filter(df, cntry == 'Spain'& (year==2002 | year==2014))
d_fr <- filter(df, cntry == 'France' & (year==2002 | year==2014))
d_uk <- filter(df, cntry == 'United Kingdom'& (year==2002 | year==2014))
d_it <- filter(df, cntry == 'Italy'& (year==2002 | year==2014))
d_po <- filter(df, cntry == 'Poland'& (year==2002 | year==2014))


g1= ggplot(df, aes(year, unemp), color=cntry)+
  geom_line(aes(group=cntry, color=cntry), size=1.5)+
  geom_point(data=d_fr, size=3.5)+
  geom_line(data = df[df$cntry == 'France',], aes(group=cntry, color=cntry), size=2.2)+
  geom_text(data = slice_max(d_fr, unemp, n = 1), aes(year, unemp, label = round(unemp, 1)), hjust = -0.4, size=5) +
  geom_text(data = slice_min(d_fr, unemp, n = 1), aes(year, unemp, label = round(unemp, 1)), hjust = 1.5, size=5) +
  ylab(NULL)+xlab(NULL)+ggtitle("France")+
  scale_x_continuous(limits = c(2001.5, 2015), breaks = seq(2002, 2014, by = 2)) +
  scale_color_manual(values=c('black','snow3','snow3','snow3','snow3','snow3')) + 
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size=15))

g2= ggplot(df, aes(year, unemp), color=cntry)+
  geom_line(data=df,aes(group=cntry, color=cntry), size=1.5)+
  geom_line(data = df[df$cntry == 'Germany',], aes(group=cntry, color=cntry), size=2.2)+
  geom_point(data=d_de, size=3.5)+
  geom_text(data = slice_max(d_de, unemp, n = 1), aes(year, unemp, label = round(unemp, 1)), hjust = 1.5, size=5) +
  geom_text(data = slice_min(d_de, unemp, n = 1), aes(year, unemp, label = round(unemp, 1)), hjust = -1.1, size=5) +
  ylab(NULL)+xlab(NULL)+ggtitle("Germany")+
  scale_x_continuous(limits = c(2001.5, 2015), breaks = seq(2002, 2014, by = 2)) +
  scale_color_manual(values=c('snow3','black','snow3','snow3','snow3','snow3')) + 
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size=15))

g3=ggplot(df, aes(year, unemp), color=cntry)+
  geom_line(data=df,aes(group=cntry, color=cntry), size=1.5)+
  geom_line(data = df[df$cntry == 'Poland',], aes(group=cntry, color=cntry), size=2.2)+
  geom_point(data=d_po, size=3.5)+
  geom_text(data = slice_max(d_po, unemp, n = 1), aes(year, unemp, label = round(unemp, 1)), hjust = 1.5, size=5) +
  geom_text(data = slice_min(d_po, unemp, n = 1), aes(year, unemp, label = round(unemp, 1)), hjust = -1.1, size=5) +
  ylab(NULL)+xlab(NULL)+ggtitle("Poland")+
  scale_x_continuous(limits = c(2001.5, 2015), breaks = seq(2002, 2014, by = 2)) +
  scale_color_manual(values=c('snow3','snow3','snow3','black','snow3','snow3')) + 
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size=15))

g4= ggplot(df, aes(year, unemp), color=cntry)+
  geom_line(data=df,aes(group=cntry, color=cntry), size=1.5)+
  geom_line(data = df[df$cntry == 'United Kingdom',], aes(group=cntry, color=cntry), size=2.2)+
  geom_point(data=d_uk, size=3.5)+
  geom_text(data = slice_max(d_uk, unemp, n = 1), aes(year, unemp, label = round(unemp, 1)), hjust = -0.4, size=5) +
  geom_text(data = slice_min(d_uk, unemp, n = 1), aes(year, unemp, label = round(unemp, 1)), hjust = 1.5, size=5) +
  ylab(NULL)+xlab(NULL)+ggtitle("United Kingdom")+
  scale_x_continuous(limits = c(2001.5, 2015), breaks = seq(2002, 2014, by = 2)) +
  scale_color_manual(values=c('snow3','snow3','snow3','snow3','snow3','black')) + 
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size=15),
        axis.text.x = element_text(color="black", size=15))

g5= ggplot(df, aes(year, unemp), color=cntry)+
  geom_line(data=df,aes(group=cntry, color=cntry), size=1.5)+
  geom_line(data = df[df$cntry == 'Spain',], aes(group=cntry, color=cntry), size=2.2)+
  geom_point(data=d_es, size=3.5)+
  geom_text(data = slice_max(d_es, unemp, n = 1), aes(year, unemp, label = round(unemp, 1)), hjust = -0.4, size=5) +
  geom_text(data = slice_min(d_es, unemp, n = 1), aes(year, unemp, label = round(unemp, 1)), hjust = 1.3, size=5) +
  ylab(NULL)+xlab(NULL)+ggtitle("Spain")+
  scale_x_continuous(limits = c(2001.5, 2015), breaks = seq(2002, 2014, by = 2)) +
  scale_color_manual(values=c('snow3','snow3','snow3','snow3','black','snow3')) + 
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(color="black", size=15),
        plot.title = element_text(hjust = 0.5, size=15))

g6= ggplot(df, aes(year, unemp), color=cntry)+
  geom_line(data=df,aes(group=cntry, color=cntry), size=1.5)+
  geom_line(data = df[df$cntry == 'Italy',], aes(group=cntry, color=cntry), size=2.2)+
  geom_point(data=d_it, size=3.5)+
  geom_text(data = slice_max(d_it, unemp, n = 1), aes(year, unemp, label = round(unemp, 1)), hjust = -0.4, size=5) +
  geom_text(data = slice_min(d_it, unemp, n = 1), aes(year, unemp, label = round(unemp, 1)), hjust = 1.5, size=5) +
  ylab(NULL)+xlab(NULL)+ggtitle("Italy")+
  scale_x_continuous(limits = c(2001.5, 2015), breaks = seq(2002, 2014, by = 2)) +
  scale_color_manual(values=c('snow3','snow3','black','snow3','snow3','snow3')) + 
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size=15))

#Gather all figures
figure1 <- ggarrange(g3, g2, g1, g6, g5,g4, ncol=2, nrow=3)
ggsave("Figure_1.pdf", scale=1.35, width = 10, height = 7, dpi= 2300,limitsize = FALSE,units = c("in"))

