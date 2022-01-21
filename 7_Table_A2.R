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

#Select only the variables of interest
c1 <- m %>% dplyr::select(c("mainact", "parents_educ", "gndr", "education", "age", 
                            "parents_educ", "crisis", "data", "cntry"))
c1= as.data.frame(c1)

#Run a separate regression model for each country
ols_1 <- c1 %>%
  filter(cntry=="France") 
ols_1 =lm(mainact ~ parents_educ + gndr + age + education*parents_educ + education* crisis + parents_educ*crisis + data, data=ols_1)

ols_2 <- c1 %>%
  filter(cntry=="Germany") 
ols_2 =lm(mainact ~ parents_educ + gndr + age + education*parents_educ + education* crisis + parents_educ*crisis + data, data=ols_2)

ols_3 <- c1 %>%
  filter(cntry=="Italy") 
ols_3 =lm(mainact ~ parents_educ + gndr + age + education*parents_educ + education* crisis + parents_educ*crisis + data, data=ols_3)

ols_4 <- c1 %>%
  filter(cntry=="Poland") 
ols_4 =lm(mainact ~ parents_educ + gndr + age + education*parents_educ + education* crisis + parents_educ*crisis + data, data=ols_4)

ols_5 <- c1 %>%
  filter(cntry=="Spain") 
ols_5 =lm(mainact ~ parents_educ + gndr + age + education*parents_educ + education* crisis + parents_educ*crisis + data, data=ols_5)

ols_6 <- c1 %>%
  filter(cntry=="United Kingdom") 
ols_6 =lm(mainact ~ parents_educ + gndr + age + education*parents_educ + education* crisis + parents_educ*crisis + data, data=ols_6)


# Combine the results
results<-list("France"= ols_1,
              "Germany"= ols_2,
              "Italy"= ols_3,
              "Poland"= ols_4,
              "Spain"= ols_5,
              "United Kingdom"= ols_6)

#Label the variables in the table
cm <- c('(Intercept)' = 'Intercept',
        'gndrMale' = 'Sex',
        'parents_educLow social origin' = 'Parental - Below upper-secondary', 
        'parents_educMiddle social origin' = 'Parental - Upper-secondary',
        'educationLower secondary and below ' = 'Below upper-secondary', 
        'educationUpper secondary ' = 'Upper-secondary', 
        'age' = 'Age', 
        'crisisPost-recession' = 'Crisis (1 = 2008-14)', 
        'data' = 'Data (1 = ESS)',
        'parents_educLow social origin:educationLower secondary and below '= 'Low social origin x lower secondary and below',
        'parents_educMiddle social origin:educationLower secondary and below ' = 'Middle social origin x lower secondary and below',
        'parents_educLow social origin:educationUpper secondary ' = 'Low social origin x upper secondary',
        'parents_educMiddle social origin:educationUpper secondary ' = 'Middle social origin x upper secondary',
        'educationLower secondary and below :crisisPost-recession' = 'Below upper-secondary x crisis', 
        'educationUpper secondary :crisisPost-recession' = 'Upper-secondary x crisis', 
        'parents_educLow social origin:crisisPost-recession' = 'Parental - Below upper-secondary x crisis',
        'parents_educMiddle social origin:crisisPost-recession' = 'Parental - Upper-secondary x crisis')


#Create extra rows above certain variables
rows <- tribble(~term, ~France, ~Germany, ~Italy,~Poland, ~Spain, ~UK,
                'Parental education' ,'' , '', '', '', '', '', 
                'Respondent education' ,'' , '', '', '', '', '', 
                'Parental education x respondent education' ,'' , '', '', '', '', '',
                'Respondent education x crisis' ,'' , '', '', '', '', '',
                'Parental education x crisis' ,'' , '', '', '', '', '',
                '' ,'' , '', '', '', '', '',)


attr(rows, 'position') <- c(5,10,21,30,35)


# Output Table
modelsummary::modelsummary(results, statistic = 'std.error',
                           fmt= '%.3f',coef_map=cm, gof_omit = 'F|R2|AIC|BIC|Log', 
                           output = 'gt', add_rows = rows)

# Set a working directory where you would like to find the table output
# setwd("/directory_path_file")
modelsummary::modelsummary(results, stars = TRUE, statistic = 'std.error',
                           fmt= '%.3f',coef_map=cm,  gof_omit = 'F|R2|AIC|BIC|Log',
                           output = 'table_A2.docx', add_rows = rows)

