#Install the packages below in case they are not installed. 
#install.packages(ggeffects)
#install.packages(dplyr)
#install.packages(ggpubr)
#install.packages(ggplot2)

#Extract the packages
library(ggeffects)
library(ggpubr)
library(dplyr)
library(ggplot2)

#Clear the data environment
rm(list = ls())

#Import the merged data set that is constructed in "0_setting_data"
load("directory_path_file/m.Rda")


#We run the predictive probabilities for each country separately. We combine all the figures together at the end.

#################################### France ####################################
#Filter country
m=filter(m, cntry=="France")
#Save the regression
mod= glm(mainact  ~ parents_educ + gndr + age + education*parents_educ + education* crisis + parents_educ*crisis + data, data = m, family = "binomial")
#Run the predictive probabilities
df <- ggpredict(mod, terms = c("parents_educ","crisis"))
#Filter the data to keep low origin young adults relative to high origin
df <- df[ df$x != "Middle social origin", , drop=F]

#Function for y-axis decimals
scale <- function(x) sprintf("%.2f", x)

#Prepare the labels to plot them on figure
df = df %>% select(x,group, predicted,std.error,conf.high,conf.low)
t1= filter(df, x=="High social origin" & group=="Prior-recession")
t2= filter(df, x=="High social origin" & group=="Post-recession")
t3= filter(df, x=="Low social origin" & group=="Prior-recession")
t4= filter(df, x=="Low social origin" & group=="Post-recession")

#Plot the figure
g1= ggplot(df, aes(group, predicted, group = x, shape= x, linetype=x, color=x)) + 
  geom_point(position=position_dodge(width=0.15),size=2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),width = 0.1,
                linetype = "solid",position=position_dodge(width=0.15))+
  geom_text(data = t1, aes(group, predicted, label = round(predicted, 2)), hjust = 1.6, size=4,show.legend = FALSE) +
  geom_text(data = t2, aes(group, predicted, label = round(predicted, 2)), hjust = 1.8, size=4,show.legend  = FALSE) +
  geom_text(data = t3, aes(group, predicted, label = round(predicted, 2)), hjust = -0.6, size=4,show.legend  = FALSE) +
  geom_text(data = t4, aes(group, predicted, label = round(predicted, 2)), hjust = -0.6, size=4,show.legend  = FALSE) +
  scale_color_manual(values=c('black','snow4')) +
  scale_y_continuous(labels=scale)+
  theme_classic()+
  labs(group= "Social origin", shape= "Social origin", linetype= "Social origin",color= "Social origin") +
  xlab(NULL)+ ylab(NULL) + ggtitle("France") +
  theme( 
    axis.title.x = element_text(color="Black", size=28, face="bold"),
    axis.title.y = element_text(color="Black", size=16, face="bold", hjust = 0.9),
    plot.title = element_text(hjust = 0.5, color="Black", size=28, face="bold"),
    legend.title = element_blank(),
    legend.text = element_blank(),
    legend.position="none",
    axis.text.x = element_text(face="bold", color="black", size=14),
    axis.text.y = element_text(face="bold", color="black", size=14),
    strip.text = element_text(face= "bold", size=20)
  )


#################################### Germany ####################################

#Import the merged data set that is constructed in "0_setting_data"
load("directory_path_file/m.Rda")

#Filter country
m=filter(m, cntry=="Germany")

#Save the regression
mod= glm(mainact  ~ parents_educ + gndr + age + education*parents_educ + education* crisis + parents_educ*crisis + data, data = m, family = "binomial")

#Run the predictive probabilities
df <- ggpredict(mod, terms = c("parents_educ","crisis"))

#Filter the data to keep low origin young adults relative to high origin
df <- df[ df$x != "Middle social origin", , drop=F]

#Prepare the labels to plot them on figure
df = df %>% select(x,group, predicted,std.error,conf.high,conf.low)
t1= filter(df, x=="High social origin" & group=="Prior-recession")
t2= filter(df, x=="High social origin" & group=="Post-recession")
t3= filter(df, x=="Low social origin" & group=="Prior-recession")
t4= filter(df, x=="Low social origin" & group=="Post-recession")

#Plot the figure
g2=ggplot(df, aes(group, predicted, group = x, shape= x, linetype=x, color=x)) + 
  geom_point(position=position_dodge(width=0.15),size=2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),width = 0.1,
                linetype = "solid",position=position_dodge(width=0.15))+
  geom_text(data = t1, aes(group, predicted, label = round(predicted, 2)), hjust = 1.6, size=4,show.legend = FALSE) +
  geom_text(data = t2, aes(group, predicted, label = round(predicted, 2)), hjust = 1.6, size=4,show.legend  = FALSE) +
  geom_text(data = t3, aes(group, predicted, label = round(predicted, 2)), hjust = -0.6, size=4,show.legend  = FALSE) +
  geom_text(data = t4, aes(group, predicted, label = round(predicted, 2)), hjust = -0.6, size=4,show.legend  = FALSE) +
  scale_color_manual(values=c('black','snow4')) +
  scale_y_continuous(labels=scale)+
  theme_classic()+
  labs(group= "Social origin", shape= "Social origin", linetype= "Social origin",color= "Social origin") +
  xlab(NULL)+ ylab(NULL) + ggtitle("Germany") +
  theme( 
    axis.title.x = element_text(color="Black", size=28, face="bold"),
    axis.title.y = element_text(color="Black", size=16, face="bold", hjust = 0.9),
    plot.title = element_text(hjust = 0.5, color="Black", size=28, face="bold"),
    legend.title = element_blank(),
    legend.text = element_blank(),
    legend.position="none",
    axis.text.x = element_text(face="bold", color="black", size=14),
    axis.text.y = element_text(face="bold", color="black", size=14),
    strip.text = element_text(face= "bold", size=20)
  )


#################################### Italy ####################################
#Import the merged data set that is constructed in "0_setting_data"
load("directory_path_file/m.Rda")


#Filter country
m=filter(m, cntry=="Italy")

#Save the regression
mod= glm(mainact  ~ parents_educ + gndr + age + education*parents_educ + education* crisis + parents_educ*crisis + data, data = m, family = "binomial")

#Run the predictive probabilities
df <- ggpredict(mod, terms = c("parents_educ","crisis"))

#Filter the data to keep low origin young adults relative to high origin
df <- df[ df$x != "Middle social origin", , drop=F]


#Prepare the labels to plot them on figure
df = df %>% select(x,group, predicted,std.error,conf.high,conf.low)
t1= filter(df, x=="High social origin" & group=="Prior-recession")
t2= filter(df, x=="High social origin" & group=="Post-recession")
t3= filter(df, x=="Low social origin" & group=="Prior-recession")
t4= filter(df, x=="Low social origin" & group=="Post-recession")

#Plot the figure
g3= ggplot(df, aes(group, predicted, group = x, shape= x, linetype=x, color=x)) + 
  geom_point(position=position_dodge(width=0.15),size=2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),width = 0.1,
                linetype = "solid",position=position_dodge(width=0.15))+
  geom_text(data = t1, aes(group, predicted, label = round(predicted, 2)), hjust = 1.6, size=4,show.legend = FALSE) +
  geom_text(data = t2, aes(group, predicted, label = round(predicted, 2)), hjust = 1.6, size=4,show.legend  = FALSE) +
  geom_text(data = t3, aes(group, predicted, label = round(predicted, 2)), hjust = -0.6, size=4,show.legend  = FALSE) +
  geom_text(data = t4, aes(group, predicted, label = round(predicted, 2)), hjust = -0.6, size=4,show.legend  = FALSE) +
  scale_color_manual(values=c('black','snow4')) +
  scale_y_continuous(labels=scale)+
  theme_classic()+
  labs(group= "Social origin", shape= "Social origin", linetype= "Social origin",color= "Social origin") +
  xlab(NULL)+ ylab(NULL) + ggtitle("Italy") +
  theme( 
    axis.title.x = element_text(color="Black", size=28, face="bold"),
    axis.title.y = element_text(color="Black", size=16, face="bold", hjust = 0.9),
    plot.title = element_text(hjust = 0.5, color="Black", size=28, face="bold"),
    legend.title = element_blank(),
    legend.text = element_blank(),
    legend.position="none",
    axis.text.x = element_text(face="bold", color="black", size=14),
    axis.text.y = element_text(face="bold", color="black", size=14),
    strip.text = element_text(face= "bold", size=20)
  )

#################################### Poland ####################################
#Import the merged data set that is constructed in "0_setting_data"
load("directory_path_file/m.Rda")


#Filter country
m=filter(m, cntry=="Poland")

#Save the regression
mod= glm(mainact  ~ parents_educ + gndr + age + education*parents_educ + education* crisis + parents_educ*crisis + data, data = m, family = "binomial")

#Run the predictive probabilities
df <- ggpredict(mod, terms = c("parents_educ","crisis"))

#Filter the data to keep low origin young adults relative to high origin
df <- df[ df$x != "Middle social origin", , drop=F]

#Prepare the labels to plot them on figure
df = df %>% select(x,group, predicted,std.error,conf.high,conf.low)
t1= filter(df, x=="High social origin" & group=="Prior-recession")
t2= filter(df, x=="High social origin" & group=="Post-recession")
t3= filter(df, x=="Low social origin" & group=="Prior-recession")
t4= filter(df, x=="Low social origin" & group=="Post-recession")

#Plot the figure
g4= ggplot(df, aes(group, predicted, group = x, shape= x, linetype=x, color=x)) + 
  geom_point(position=position_dodge(width=0.15),size=2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),width = 0.1,
                linetype = "solid",position=position_dodge(width=0.15))+
  geom_text(data = t1, aes(group, predicted, label = round(predicted, 2)), hjust = 1.6, size=4,show.legend = FALSE) +
  geom_text(data = t2, aes(group, predicted, label = round(predicted, 2)), hjust = 1.6, size=4,show.legend  = FALSE) +
  geom_text(data = t3, aes(group, predicted, label = round(predicted, 2)), hjust = -0.6, size=4,show.legend  = FALSE) +
  geom_text(data = t4, aes(group, predicted, label = round(predicted, 2)), hjust = -0.6, size=4,show.legend  = FALSE) +
  scale_color_manual(values=c('black','snow4')) +
  scale_y_continuous(labels=scale)+
  theme_classic()+
  labs(group= "Social origin", shape= "Social origin", linetype= "Social origin",color= "Social origin") +
  xlab(NULL)+ ylab(NULL) + ggtitle("Poland") +
  theme( 
    axis.title.x = element_text(color="Black", size=28, face="bold"),
    axis.title.y = element_text(color="Black", size=16, face="bold", hjust = 0.9),
    plot.title = element_text(hjust = 0.5, color="Black", size=28, face="bold"),
    legend.title = element_blank(),
    legend.text = element_blank(),
    legend.position="none",
    axis.text.x = element_text(face="bold", color="black", size=14),
    axis.text.y = element_text(face="bold", color="black", size=14),
    strip.text = element_text(face= "bold", size=20)
  )

#################################### Spain ####################################
#Import the merged data set that is constructed in "0_setting_data"
load("directory_path_file/m.Rda")


#Filter country
m=filter(m, cntry=="Spain")

#Save the regression
mod= glm(mainact  ~ parents_educ + gndr + age + education*parents_educ + education* crisis + parents_educ*crisis + data, data = m, family = "binomial")

#Run the predictive probabilities
df <- ggpredict(mod, terms = c("parents_educ","crisis"))

#Filter the data to keep low origin young adults relative to high origin
df <- df[ df$x != "Middle social origin", , drop=F]

#Prepare the labels to plot them on figure
df = df %>% select(x,group, predicted,std.error,conf.high,conf.low)
t1= filter(df, x=="High social origin" & group=="Prior-recession")
t2= filter(df, x=="High social origin" & group=="Post-recession")
t3= filter(df, x=="Low social origin" & group=="Prior-recession")
t4= filter(df, x=="Low social origin" & group=="Post-recession")

#Plot the figure
g5= ggplot(df, aes(group, predicted, group = x, shape= x, linetype=x, color=x)) + 
  geom_point(position=position_dodge(width=0.15),size=2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),width = 0.1,
                linetype = "solid",position=position_dodge(width=0.15))+
  geom_text(data = t1, aes(group, predicted, label = round(predicted, 2)), hjust = 1.5, size=4,show.legend = FALSE) +
  geom_text(data = t2, aes(group, predicted, label = round(predicted, 2)), hjust = 1.5, size=4,show.legend  = FALSE) +
  geom_text(data = t3, aes(group, predicted, label = round(predicted, 2)), hjust = -0.5, size=4,show.legend  = FALSE) +
  geom_text(data = t4, aes(group, predicted, label = round(predicted, 2)), hjust = -0.5, size=4,show.legend  = FALSE) +
  scale_color_manual(values=c('black','snow4')) +
  scale_y_continuous(labels=scale)+
  theme_classic()+
  labs(group= "Social origin", shape= "Social origin", linetype= "Social origin",color= "Social origin") +
  xlab(NULL)+ ylab(NULL) + ggtitle("Spain") +
  theme( 
    axis.title.x = element_text(color="Black", size=28, face="bold"),
    axis.title.y = element_text(color="Black", size=16, face="bold", hjust = 0.9),
    plot.title = element_text(hjust = 0.5, color="Black", size=28, face="bold"),
    legend.title = element_blank(),
    legend.text = element_text(color = "Black", size = 13),
    legend.position="bottom",
    axis.text.x = element_text(face="bold", color="black", size=14),
    axis.text.y = element_text(face="bold", color="black", size=14),
    strip.text = element_text(face= "bold", size=20)
  )

#################################### United Kingdom ############################
#Import the merged data set that is constructed in "0_setting_data"
load("directory_path_file/m.Rda")


#Filter country
m=filter(m, cntry=="United Kingdom")

#Save the regression
mod= glm(mainact  ~ parents_educ + gndr + age + education*parents_educ + education* crisis + parents_educ*crisis + data, data = m, family = "binomial")

#Run the predictive probabilities
df <- ggpredict(mod, terms = c("parents_educ","crisis"))

#Filter the data to keep low origin young adults relative to high origin
df <- df[ df$x != "Middle social origin", , drop=F]

#Prepare the labels to plot them on figure
df = df %>% select(x,group, predicted,std.error,conf.high,conf.low)
t1= filter(df, x=="High social origin" & group=="Prior-recession")
t2= filter(df, x=="High social origin" & group=="Post-recession")
t3= filter(df, x=="Low social origin" & group=="Prior-recession")
t4= filter(df, x=="Low social origin" & group=="Post-recession")

#Plot the figure
g6= ggplot(df, aes(group, predicted, group = x, shape= x, linetype=x, color=x)) + 
  geom_point(position=position_dodge(width=0.15),size=2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),width = 0.1,
                linetype = "solid",position=position_dodge(width=0.15))+
  geom_text(data = t1, aes(group, predicted, label = round(predicted, 2)), hjust = 1.6, size=4,show.legend = FALSE) +
  geom_text(data = t2, aes(group, predicted, label = round(predicted, 2)), hjust = 1.6, size=4,show.legend  = FALSE) +
  geom_text(data = t3, aes(group, predicted, label = round(predicted, 2)), hjust = -0.6, size=4,show.legend  = FALSE) +
  geom_text(data = t4, aes(group, predicted, label = round(predicted, 2)), hjust = -0.6, size=4,show.legend  = FALSE) +
  scale_color_manual(values=c('black','snow4')) +
  scale_y_continuous(labels=scale)+
  theme_classic()+
  labs(group= "Social origin", shape= "Social origin", linetype= "Social origin",color= "Social origin") +
  xlab(NULL)+ ylab(NULL) + ggtitle("United Kingdom") +
  theme( 
    axis.title.x = element_text(color="Black", size=28, face="bold"),
    axis.title.y = element_text(color="Black", size=16, face="bold", hjust = 0.9),
    plot.title = element_text(hjust = 0.5, color="Black", size=28, face="bold"),
    legend.title = element_text(color = "Black", size = 16, face="bold"),
    legend.text = element_text(color = "Black", size = 13),
    legend.position="bottom",
    axis.text.x = element_text(face="bold", color="black", size=14),
    axis.text.y = element_text(face="bold", color="black", size=14),
    strip.text = element_text(face= "bold", size=20)
  )

#Combine all countries together
figure1 <- ggarrange(g1, g2, g3, g4, g5,g6, ncol=2, nrow=3)
figure1= annotate_figure(figure1, left = text_grob("Employment",color = "black", face = "bold", size = 30, rot = 90))

#Save the figure
ggsave("Figure_A1.pdf", scale=1.35, width = 14, height = 7, dpi= 2300,limitsize = FALSE,units = c("in"))
