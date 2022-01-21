#Install the packages below in case they are not installed. 
#install.packages(dplyr)
#install.packages(forcats)
#install.packages(modelsummary)

#Extract the packages
library(dplyr)
library(forcats)
library(modelsummary)

#Clear the data environment
rm(list = ls())

#Import the merged data set that is constructed in "0_setting_data"
load("directory_path_file/m.Rda")


#Keep the ESS data
ess = filter(m, data==1)

#Relevel parental education from bottom to top
ess =  ess %>% mutate(parents_educ = fct_relevel(parents_educ,"Low social origin", "Middle social origin", "High social origin"))

#Relevel education from bottom to top
ess =  ess %>% mutate(education = fct_relevel(education,"Lower secondary and below ", "Upper secondary ", "Post-secondary and tertiary "))

#Relevel countries in order of appearance
ess =  ess %>% mutate(cntry = fct_relevel(cntry,"France", "Germany", "Italy", "Poland", "Spain", "United Kingdom"))

#Label the period (or crisis variable)
ess$crisis <- NA
ess$crisis[ess$year<2008] = 0
ess$crisis[ess$year>2007] = 1
ess$crisis <- factor(ess$crisis,
                     levels = c(0,1),
                     labels = c("2002-07", "2008-14"))

#Label the employed variable
ess$mainact <- factor(ess$mainact,
                      levels = c(0,1),
                      labels = c("Non-employed", "Employed"))

#Remove missing data from the descriptive table
ess <- ess[!is.na(ess$gndr), ]
ess <- ess[!is.na(ess$parents_educ), ]
ess <- ess[!is.na(ess$education), ]
ess <- ess[!is.na(ess$age), ]
ess <- ess[!is.na(ess$crisis), ]
ess <- ess[!is.na(ess$mainact), ]

ess$crisis = as.factor(ess$crisis)

#Select the variables of interest
ess <- ess %>%
  select(Country =cntry,
         Gender =gndr,
         `Parental education` = parents_educ,
         `Main activity`= mainact,
         Education = education,
         Crisis= crisis)

datasummary_balance(~Country,
                    data = ess, dinm= F)

datasummary_balance(~Country,
                    data = ess, dinm= F,output = 'table_descriptives_ess.docx')


###################################### EU-SILC ################################
#Keep the EU-SILC data
eu_silc = filter(m, data==0)

#Relevel parental education from bottom to top
eu_silc =  eu_silc %>% mutate(parents_educ = fct_relevel(parents_educ,"Low social origin", "Middle social origin", "High social origin"))

#Relevel education from bottom to top
eu_silc =  eu_silc %>% mutate(education = fct_relevel(education,"Lower secondary and below ", "Upper secondary ", "Post-secondary and tertiary "))

#Relevel countries in order of appearance
eu_silc =  eu_silc %>% mutate(cntry = fct_relevel(cntry,"France", "Germany", "Italy", "Poland", "Spain", "United Kingdom"))

#Label the period (or crisis variable)
eu_silc$crisis <- NA
eu_silc$crisis[eu_silc$year<2008] = 0
eu_silc$crisis[eu_silc$year>2007] = 1
eu_silc$crisis <- factor(eu_silc$crisis,
                         levels = c(0,1),
                         labels = c("2005-06", "2011-12"))

#Label the employed variable
eu_silc$mainact <- factor(eu_silc$mainact,
                          levels = c(0,1),
                          labels = c("Non-employed", "Employed"))



#Remove missing data from the descriptive tables
eu_silc <- eu_silc[!is.na(eu_silc$gndr), ]
eu_silc <- eu_silc[!is.na(eu_silc$parents_educ), ]
eu_silc <- eu_silc[!is.na(eu_silc$education), ]
eu_silc <- eu_silc[!is.na(eu_silc$age), ]
eu_silc <- eu_silc[!is.na(eu_silc$crisis), ]
eu_silc <- eu_silc[!is.na(eu_silc$mainact), ]

eu_silc$crisis = as.factor(eu_silc$crisis)

#Select the variables of interest
eu_silc <- eu_silc  %>%
  select(Country =cntry,
         `Income (log)`=ln_wage,
         Gender =gndr,
         `Parental education` = parents_educ,
         `Main activity`= mainact,
         Education = education,
         Crisis= crisis)


datasummary_balance(~Country,
                    data = eu_silc, dinm= F)

datasummary_balance(~Country,
                    data = eu_silc, dinm= F,output = 'table_descriptives_eu_silc.docx')

