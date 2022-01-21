#Install the packages below in case they are not installed. 
#install.packages(forcats)
#install.packages(ggplot2)
#install.packages(dplyr)
#install.packages(tidyr)
#install.packages(broom)
#install.packages(ggpubr)

#Extract the packages
library(forcats)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggpubr)

#Clear the data environment
rm(list = ls())

#Import the merged data set that is constructed in "0_setting_data"
load("directory_path_file/m.Rda")

################################################# Left panel: outcome employment

#Run regression seperately for each country
a <-  m %>%
  group_by(cntry) %>%
  do(model1 = tidy(lm(mainact  ~ parents_educ + gndr + age + education*parents_educ + education* crisis + parents_educ*crisis + data, data = .),  conf.int=TRUE)) %>%   
  gather(model_name, model, -cntry) %>%                        ## make it long format
  unnest() 

#Prepare the data to plot it. We filter the interaction of interest, that is parental_education*crisis.
b = filter(a,term=="parents_educLow social origin:crisisPost-recession")
b$elabel =round(b$estimate*100)

#Fix the country order of appearance
b =  b %>% mutate(cntry = fct_relevel(cntry,"United Kingdom", "Spain", "Poland", "Italy", "Germany", "France")) 

#Plot the left panel
g1= ggplot(data= b,mapping = aes(estimate,cntry)) + 
  geom_point(size=4,shape=15) +
  geom_label(label=b$elabel,nudge_y = 0.13, nudge_x=-0.001 ,label.size = NA) +
  geom_linerange(mapping=aes(xmin=conf.low , xmax=conf.high, y=cntry), width=0.1, size=.5, color="black") + 
  geom_vline(xintercept=0, color = "black") + ggtitle("Employment")+
  scale_x_continuous(limits = c(-0.2, 0.22), breaks = seq(-0.2, 0.4, by = 0.1),labels = scales::percent_format(accuracy = 1))+
  theme_classic()+ ylab(NULL)+xlab(NULL)+
  theme( 
    axis.title.x = element_text(color="Black", size=15, face="bold"),
    axis.title.y = element_text(color="Black", size=15, face="bold", hjust = 0.5),
    plot.title = element_text(hjust = 0.5, color="Black", size=20, face="bold"),
    legend.text = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(face="bold", color="black", size=18),
    axis.text.y = element_text(face="bold", color="black", size=25),
    strip.text = element_text(face= "bold", size=20),
    strip.background = element_rect(colour = "white", fill = "white"),
    plot.caption=element_text(hjust = 0, size=20),
    strip.placement = "outside",
    axis.ticks.x = element_blank())


################################################# Right panel: outcome earnings

#Keep working people (Full-time and part-time)
m= m %>% filter(mainact==1) 

#Run regression seperately for each country
z <-  m %>%
  group_by(cntry) %>%
  do(model1 = tidy(lm(ln_wage  ~ parents_educ + gndr + age + education*parents_educ + education*crisis + parents_educ*crisis , data = .),  conf.int=TRUE)) %>%   ## Same as question
  gather(model_name, model, -cntry) %>%                        ## make it long format
  unnest() 

#Prepare the data to plot it. We filter the interaction of interest, that is parental_education*crisis.
q= z %>% filter(term=="parents_educLow social origin:crisisPost-recession")
q$elabel =round(q$estimate,2)

#Fix the country order of appearance
q =  q %>% mutate(cntry = fct_relevel(cntry,"United Kingdom", "Spain", "Poland", "Italy", "Germany", "France")) 

#Plot the right panel
g2= ggplot(data= q,mapping = aes(estimate,cntry)) + 
  geom_point(size=4) +
  geom_label(label=q$elabel,nudge_y = 0.13, nudge_x=-0.001 ,label.size = NA) +
  geom_linerange(mapping=aes(xmin=conf.low , xmax=conf.high, y=cntry), width=0.1, size=.5, color="black") + 
  geom_vline(xintercept=0, color = "black") + ggtitle("Log earnings")+
  theme_classic()+ ylab(NULL)+xlab(NULL)+
  theme( 
    axis.title.x = element_text(color="Black", size=15, face="bold"),
    axis.title.y = element_text(color="Black", size=15, face="bold", hjust = 0.5),
    plot.title = element_text(hjust = 0.5, color="Black", size=20, face="bold"),
    legend.text = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(face="bold", color="black", size=18),
    axis.text.y = element_text(face="bold", color="black", size=25),
    strip.text = element_text(face= "bold", size=20),
    strip.background = element_rect(colour = "white", fill = "white"),
    plot.caption=element_text(hjust = 0, size=20),
    strip.placement = "outside",
    axis.ticks.x = element_blank())

#Combine the left and right panel 
figure4 <- ggarrange(g1, g2, ncol=2, nrow=1)

#Save the figure
ggsave("Figure_4.pdf", scale=1.35, width = 12, height = 7, dpi= 2300,limitsize = FALSE,units = c("in"))

