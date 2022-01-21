#Install the packages below in case they are not installed. 
#install.packages(forcats)
#install.packages(ggplot2)
#install.packages(dplyr)
#install.packages(ggpubr)

#Extract the packages
library(forcats)
library(ggplot2)
library(dplyr)
library(ggpubr)

#Clear the data environment
rm(list = ls())

#Import the merged data set that is constructed in "0_setting_data"
load("directory_path_file/m.Rda")

#Recode the labels so they can fit in the figure
m= m %>%  dplyr::mutate(parents_educ = fct_recode(parents_educ,    
                                                  "L" = "Low social origin",
                                                  "M" = "Middle social origin",
                                                  "H" = "High social origin"))

#Reorder the labels from low social origin to high social origin
m =  m %>% mutate(parents_educ = fct_relevel(parents_educ,"L","M","H")) 


#Prepare the data 
#Keep people working full-time and part-time. See the data section. 
m= m %>% filter(mainact==1) 

m2 = m %>%
  group_by(crisis,cntry,parents_educ) %>%
  summarise(inc = round(mean(income,na.rm = T),3))%>%
  ungroup()  

m2 <- na.omit(m2)

m2$diff = (m2$inc[m2$crisis == "Post-recession"] - m2$inc)/m2$inc
m2$diff2 =round(m2$diff *100,0)


#Plot the figure
my_cap <- expression(paste(bold(" L:")," Low",bold(" M:")," Middle",bold(" H:")," High"))

m2 %>% filter(crisis=="Prior-recession")%>%
  ggplot(aes(x=parents_educ, y=diff, fill=crisis)) +
  geom_bar(stat = "identity", position = position_dodge())+
  geom_text(aes(y = diff + ifelse(diff>=0,0.005, -.03),label = diff2), vjust =  -0.01, colour = "black",position = position_dodge(.9), size =6)+
  facet_wrap(~cntry, nrow=1, strip.position = "bottom") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme_classic()+ ylab(NULL) + xlab(NULL)+
  labs(caption  = my_cap,fill='Year')+
  scale_fill_manual(values=c("black", "gray50"))+
  theme( 
    axis.title.x = element_text(color="Black", size=15, face="bold"),
    axis.title.y = element_text(color="Black", size=15, face="bold", hjust = 0.5),
    plot.title = element_text(hjust = 0.5, color="Black", size=10, face="bold"),
    legend.text = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(face="bold", color="black", size=15),
    axis.text.y = element_text(face="bold", color="black", size=25),
    strip.text = element_text(face= "bold", size=20),
    strip.background = element_rect(colour = "white", fill = "white"),
    plot.caption=element_text(hjust = 0, size=20),
    strip.placement = "outside",
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank())

#Save the figure
ggsave("Figure_3.pdf", scale=1.35, width = 12, height = 7, dpi= 2300,limitsize = FALSE,units = c("in"))

