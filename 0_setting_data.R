# Replication package for "Labour Market Prospects of Young Adults in Europe: Differential Effects of Social Origin During the Great Recession" 
# European Societies. Author: Jad Moawad
# January 2022
# See readme.rtf for further instructions
# This file compiles data from the European Union Statistics on Income and Living Conditions (EU-SILC) and European Social Survey (ESS)


#Install the packages below in case they are not installed. 
#install.packages(dplyr)
#install.packages(forcats)

#Extract the packages
library(dplyr)
library(forcats)

################################################################################
######################### Import all data sets #################################
################################################################################


#Clear the data environment
rm(list = ls())

#Import the ESS data
ess = haven::read_dta("directory_path_file/ESS.dta")
#Import the EU-SILC data in 2005 from the personal data file (P)
eu_silc05 = haven::read_dta("directory_path_file/eu_silc05.dta")
#Import the EU-SILC data in 2011 from the personal data file (P)
eu_silc11 = haven::read_dta("directory_path_file/eu_silc11.dta")
#Import the excel sheet "CPI_deflator".
CPI_deflator <- readxl::read_excel("directory_path_file/CPI_deflator.xlsx")

ess= rename(ess, age = agea,
          educ= eisced,
          emp= uempla,
          class= isco08,
          feduc= edulvlfa,
          meduc= edulvlma,
          foccup1a= occf14 ,
          foccup1b= occf14a,
          foccup2= occf14b,
          moccup1a= occm14 ,
          moccup1b= occm14a,
          moccup2= occm14b,
          f.status = emprf14,
          m.status =emprm14,
          mainact =mnactic,
          workhours= wkhtot)


#################################### Year
#Create a combined year variable from different modules as the name of this 
#variable was different across some modules.
ess= ess %>%
  mutate(year = coalesce(inwyr, inwyys))
ess$year = as.integer(ess$year)

#################################### Age
ess$age = as.numeric(ess$age)

#################################### Country 
ess$cntry <- gsub(",","",ess$cntry)
ess$cntry = as.numeric(as.factor(ess$cntry))
ess$cntry <- factor(ess$cntry,
                  levels = c(1,2,3,4,5,6),
                  labels = c("Germany", "Spain", "France","United Kingdom","Italy", "Poland"))

#################################### Gender
ess$gndr <- gsub(",","",ess$gndr)
ess$gndr = as.numeric(as.factor(ess$gndr))
ess$gndr[ess$gndr==2] = 0

ess$gndr = factor(ess$gndr,
                levels = c(0,1),
                labels = c("Female", "Male"))

#################################### Work hours
ess$workhours = as.numeric(ess$workhours)

#################################### Main activity- Harmonize employment variable
#Categories are labelled as follows: 1= Paid work, 2= education, 3=active unemployed, 4=inactive unemployed, 
#5= permanently sick, 6=retired, 7=military service, 8=housework, 9=other

ess$employed <- NA
ess$employed[ess$mainact==1] = 0
ess$employed[ess$mainact>1] = 1
ess$employed <- factor(ess$employed,
                     levels = c(0,1),
                     labels = c("Employed", "Other"))

#################################### Parental education
#1= Less than lower secondary education, 2= Lower secondary education completed, 3= Upper secondary education completed, 4= Post-secondary non-tertiary education, 5= Tertiary education completed 

#Fathers' education
ess$feduc =as.integer(ess$feduc)
ess= ess %>% naniar::replace_with_na(replace = list(feduc = 55)) #This category is "other", we replace it with missing as it has no information on father's education

ess$feduc2 <- NA
ess$feduc2[ess$feduc == 1 | ess$feduc == 2] <- 1
ess$feduc2[ess$feduc == 3 | ess$feduc == 4] <- 2
ess$feduc2[ess$feduc == 5] <- 3
ess$feduc2= haven::as_factor(ess$feduc2)

#Mothers' education
ess$meduc =as.integer(ess$meduc)
ess= ess %>% naniar::replace_with_na(replace = list(meduc = 55))

ess$meduc2 <- NA
ess$meduc2[ess$meduc == 1 | ess$meduc == 2] <- 1
ess$meduc2[ess$meduc == 3 | ess$meduc == 4] <- 2
ess$meduc2[ess$meduc == 5] <- 3

ess$feduc2 = as.integer(ess$feduc2)
ess$meduc2 = as.integer(ess$meduc2)

#Taking the highest education between both parents
ess$parents_educ = NA
ess$parents_educ =  pmax(ess$feduc2, ess$meduc2, na.rm=T)


ess$parents_educ <- factor(ess$parents_educ,
                         levels = c(1,2,3),
                         labels = c("Low social origin", "Middle social origin", "High social origin"))


######################################### Parental occupation
# The coding of parental occupation has changed throughout different ESS modules. Therefore we create a harmonized version. 

#Merging parental occupational variables that were named differently throughout the modules.
#Parental occupation before 2008
#1= Traditional professional occupations, 2= Modern professional occupations, 3= Clerical and intermediate occupations, 4= Senior manager or administrators, 5=Technical and craft occupations, farmer, 6=Semi-routine/manual/service occupations, 7=Routine manual and service occupations, 8=Middle or junior managers
ess= ess %>%
  mutate(foccup1 = coalesce(foccup1a, foccup1b),
         moccup1 = coalesce(moccup1a, moccup1b))

#Father's occupation before 2008
ess$foccup_b08 <- NA
ess$foccup_b08[ess$foccup1 == 1 | ess$foccup1 == 4 | ess$foccup1 == 8] <- 3
ess$foccup_b08[ess$foccup1 == 2 | ess$foccup1 == 3] <- 2
ess$foccup_b08[ess$foccup1 >= 5 & ess$foccup1 <= 7] <- 1
ess$foccup_b08= haven::as_factor(ess$foccup_b08)
ess$foccup_b08 <- factor(ess$foccup_b08,
                       levels = c(1,2,3),
                       labels = c("Working class", "Lower-middle class", "Upper-middle class"))

#Mother's occupation before 2008
ess$moccup_b08 <- NA
ess$moccup_b08[ess$moccup1 == 1 | ess$moccup1 == 4 | ess$moccup1 == 8] <- 3
ess$moccup_b08[ess$moccup1 == 2 | ess$moccup1 == 3] <- 2
ess$moccup_b08[ess$moccup1 >= 5 & ess$moccup1 <= 7] <- 1
ess$moccup_b08= haven::as_factor(ess$moccup_b08)
ess$moccup_b08 <- factor(ess$moccup_b08,
                       levels = c(1,2,3),
                       labels = c("Working class", "Lower-middle class", "Upper-middle class"))

#Father's occupation after 2008
#1= Professional and technical occupations, 2=Higher administrator occupations, 3=Clerical occupations, 4= Sales occupations, 5= Service occupations, 6=Skilled worker, 7= Semi-skilled worker, 8=Unskilled worker, 9=Farm worker 
ess$foccup_a08 <- NA
ess$foccup_a08[ess$foccup2 == 1 | ess$foccup2 == 2] <- 3
ess$foccup_a08[ess$foccup2 >= 3 & ess$foccup2 <= 6] <- 2
ess$foccup_a08[ess$foccup2 >= 7] <- 1
ess$foccup_a08= haven::as_factor(ess$foccup_a08)
ess$foccup_a08 <- factor(ess$foccup_a08,
                       levels = c(1,2,3),
                       labels = c("Working class", "Lower-middle class", "Upper-middle class"))
#Mother's occupation after 2008
ess$moccup_a08 <- NA
ess$moccup_a08[ess$moccup2 == 1 | ess$moccup2 == 2] <- 3
ess$moccup_a08[ess$moccup2 >= 3 & ess$moccup2 <= 6] <- 2
ess$moccup_a08[ess$moccup2 >= 7] <- 1
ess$moccup_a08= haven::as_factor(ess$moccup_a08)
ess$moccup_a08 <- factor(ess$moccup_a08,
                       levels = c(1,2,3),
                       labels = c("Working class", "Lower-middle class", "Upper-middle class"))

ess= ess %>%
  mutate(foccup = coalesce(foccup_b08, foccup_a08),
         moccup = coalesce(moccup_b08, moccup_a08))

ess$foccup = as.integer(ess$foccup)
ess$moccup = as.integer(ess$moccup)

#Taking highest social class between both parents
ess$parents_occup = NA
ess$parents_occup =  pmax(ess$foccup, ess$moccup, na.rm=T)
ess$parents_occup <- factor(ess$parents_occup,
                          levels = c(1,2,3),
                          labels = c("Working class", "Lower-middle class", "Upper-middle class"))


#################################### Respondent Education
#1= Less than lower secondary education,2= Lower secondary education completed, 3= Upper secondary education completed,4= Post-secondary non-tertiary education, 5=Tertiary education completed 
ess$edulvla = as.integer(ess$edulvla)

ess$education <- NA
ess$education[ess$edulvla == 1 | ess$edulvla == 2 ] = 1
ess$education[ess$edulvla == 3] = 2
ess$education[ess$edulvla == 4 | ess$edulvla == 5] = 3

ess$education <- factor(ess$education,
                      levels = c(1,2,3),
                      labels = c("Lower secondary and below ", "Upper secondary ", "Post-secondary and tertiary "))



#################################### Filter years between 2002 and 2014  
ess= filter(ess, year>=2002 & year<=2014)

#################################### Period (or Crisis)    
ess$crisis <- NA
ess$crisis[ess$year<=2007] = 0
ess$crisis[ess$year>=2008] = 1

#################################### Data source variable: ESS =1
ess$data <- 1


#################################### Keep necessary variables
keeps <- c("education","employed","cntry","workhours",
           "age","parents_educ","crisis","year","data","parents_occup",
           "mainact","gndr")
ess= ess[ , keeps, drop = FALSE]



################################################################################
######################### EU-SILC 2005 #########################################
################################################################################

eu_silc05= rename(eu_silc05, mainact = PL030,
                  inty= PB110,
                  birth = PB140,
                  gndr =PB150,
                  cntry = PB020,
                  workhours = PL060, 
                  education=PE040,
                  feduc = PM040,
                  meduc = PM050,
                  foccup = PM070,
                  moccup = PM090,
                  incomeg= PY010G,
                  incomen = PY010N)


################################################ Year
eu_silc05$year = as.numeric(eu_silc05$inty)

############################################### Age  
eu_silc05$birth = as.integer(eu_silc05$birth)
eu_silc05$inty = as.integer(eu_silc05$inty)
eu_silc05$age = eu_silc05$inty - eu_silc05$birth

################################################ Country
eu_silc05= filter(eu_silc05, cntry=="FR"|cntry=="DE"|cntry=="IT"|cntry=="PL"|cntry=="ES"|cntry=="UK")
eu_silc05$cntry= haven::as_factor(eu_silc05$cntry)
eu_silc05$cntry <- gsub(",","",eu_silc05$cntry)
eu_silc05$cntry = as.numeric(as.factor(eu_silc05$cntry))
eu_silc05$cntry <- factor(eu_silc05$cntry,
                          levels = c(1, 2, 3,4,5,6),
                          labels = c("Germany", "Spain", "France","Italy","Poland", "United Kingdom"))

############################################### Gender
eu_silc05$gndr <- gsub(",","",eu_silc05$gndr)
eu_silc05$gndr = as.numeric(as.factor(eu_silc05$gndr))
eu_silc05$gndr[eu_silc05$gndr==2] = 0
eu_silc05$gndr = factor(eu_silc05$gndr,
                        levels = c(0,1),
                        labels = c("Female", "Male"))

#################################### Work hours
eu_silc05$workhours = as.numeric(eu_silc05$workhours)

############################################### Main activity 
# 1=full time, 2=part-time, 3= Unemployed, 4=Student, 5=Retirement, 6= Permanently disabled, 7= Military service, 8=Housework, 9= Other
eu_silc05$mainact = as.integer(eu_silc05$mainact)

eu_silc05$employed <- NA
eu_silc05$employed[eu_silc05$mainact<=3] = 0
eu_silc05$employed[eu_silc05$mainact>=3] = 1
eu_silc05$employed <- factor(eu_silc05$employed,
                             levels = c(0,1),
                             labels = c("Employed", "Other"))

################################################ Parental education
#0= Less than primary education, 1= Primary education, 2= Lower secondary education, 3=Upper secondary education, 4= Post-secondary non -tertiary education, 5= 1st and 2nd stage of tertiary education

#Father's education
eu_silc05$feduc =as.integer(eu_silc05$feduc)

eu_silc05$feduc2 <- NA
eu_silc05$feduc2[eu_silc05$feduc <=2] <- 1
eu_silc05$feduc2[eu_silc05$feduc == 3 | eu_silc05$feduc == 4] <- 2
eu_silc05$feduc2[eu_silc05$feduc == 5] <- 3
eu_silc05$feduc2= haven::as_factor(eu_silc05$feduc2)
eu_silc05$feduc2 <- factor(eu_silc05$feduc2,
                           levels = c(1,2,3),
                           labels = c("Low social origin", "Middle social origin", "High social origin"))

#Mother's education
eu_silc05$meduc2 <- NA
eu_silc05$meduc2[eu_silc05$meduc <=2] <- 1
eu_silc05$meduc2[eu_silc05$meduc == 3 | eu_silc05$meduc == 4] <- 2
eu_silc05$meduc2[eu_silc05$meduc == 5] <- 3
eu_silc05$meduc2= haven::as_factor(eu_silc05$meduc2)
eu_silc05$meduc2 <- factor(eu_silc05$meduc2,
                           levels = c(1,2,3),
                           labels = c("Low social origin", "Middle social origin", "High social origin"))

eu_silc05$feduc2 = as.integer(eu_silc05$feduc2)
eu_silc05$meduc2 = as.integer(eu_silc05$meduc2)

#Taking the highest education between both parents
eu_silc05$parents_educ = NA
eu_silc05$parents_educ =  pmax(eu_silc05$feduc2, eu_silc05$meduc2, na.rm=T)
eu_silc05$parents_educ <- factor(eu_silc05$parents_educ,
                                 levels = c(1,2,3),
                                 labels = c("Low social origin", "Middle social origin", "High social origin"))

################################################ Parental occupation
#ISCO 88-2 digits.
#Category "Armed forces"is not included in the analysis.

#Father's occupation
eu_silc05$foccup =as.integer(eu_silc05$foccup)
eu_silc05$foccup2 <- NA
eu_silc05$foccup2[eu_silc05$foccup >80] <- 1
eu_silc05$foccup2[eu_silc05$foccup >36 & eu_silc05$foccup<80] <- 2
eu_silc05$foccup2[eu_silc05$foccup >1 & eu_silc05$foccup<36] <- 3
eu_silc05$foccup2= haven::as_factor(eu_silc05$foccup2)
eu_silc05$foccup2 <- factor(eu_silc05$foccup2,
                            levels = c(1,2,3),
                            labels = c("Working class", "Lower-middle class", "Upper-middle class"))

#Mother's occupation
eu_silc05$moccup =as.integer(eu_silc05$moccup)
eu_silc05$moccup2 <- NA
eu_silc05$moccup2[eu_silc05$moccup >80] <- 1
eu_silc05$moccup2[eu_silc05$moccup >36 & eu_silc05$moccup<80] <- 2
eu_silc05$moccup2[eu_silc05$moccup >1 & eu_silc05$moccup<36] <- 3
eu_silc05$moccup2= haven::as_factor(eu_silc05$moccup2)
eu_silc05$moccup2 <- factor(eu_silc05$moccup2,
                            levels = c(1,2,3),
                            labels = c("Working class", "Lower-middle class", "Upper-middle class"))

eu_silc05$foccup = as.integer(eu_silc05$foccup2)
eu_silc05$moccup = as.integer(eu_silc05$moccup2)

#Taking the highest social class between both parents
eu_silc05$parents_occup = NA
eu_silc05$parents_occup =  pmax(eu_silc05$foccup, eu_silc05$moccup, na.rm=T)
eu_silc05$parents_occup <- factor(eu_silc05$parents_occup,
                                  levels = c(1,2,3),
                                  labels = c("Working class", "Lower-middle class", "Upper-middle class"))


################################################ Respondent education
# 0= pre-primary education, 1= primary education, 2= lower secondary education, 3=(upper) secondary education , 4=post-secondary non-tertiary education, 5= 1st & 2nd stage of tertiary education

eu_silc05$education =as.integer(eu_silc05$education)

eu_silc05$education2 <- NA
eu_silc05$education2[eu_silc05$education <3] <- 1
eu_silc05$education2[eu_silc05$education ==3] <- 2
eu_silc05$education2[eu_silc05$education >3] <- 3
eu_silc05$education2= haven::as_factor(eu_silc05$education2)
eu_silc05$education2 <- factor(eu_silc05$education2,
                               levels = c(1,2,3),
                               labels = c("Lower secondary and below ", "Upper secondary ", "Post-secondary and tertiary "))

eu_silc05$education = eu_silc05$education2

################################################ Period (or Crisis) 
eu_silc05$crisis = 0

################################################ Data source EU-SILC =1
eu_silc05$data= 0

################################################ Income
#Replace gross with net income, when the former is missing. See footnote 2 in the manuscript.
eu_silc05$income= ifelse(is.na(eu_silc05$incomeg), eu_silc05$incomen, eu_silc05$incomeg)

#################################### drop unnecessary variables in 2005
keeps <- c("education", "cntry","workhours","age","parents_educ","crisis","year","data","parents_occup", "employed","gndr", "income")
eu_silc05= eu_silc05[ , keeps, drop = FALSE]



################################################################################
######################### EU-SILC 2011 #########################################
################################################################################

eu_silc11= rename(eu_silc11, 
                  mainact = PL031,
                  inty= PB110,
                  birth = PB140,
                  gndr =PB150,
                  cntry = PB020,
                  workhours = PL060, 
                  educ=PE040,
                  feduc = PT110,
                  meduc = PT120,
                  foccup = PT150,
                  moccup = PT180,
                  incomeg= PY010G,
                  incomen = PY010N)

################################################ Year
eu_silc11$year = as.numeric(eu_silc11$inty)

############################################### Age
eu_silc11$birth = as.integer(eu_silc11$birth)
eu_silc11$inty = as.integer(eu_silc11$inty)
eu_silc11$age = eu_silc11$inty - eu_silc11$birth

################################################ Country
eu_silc11= filter(eu_silc11, cntry=="FR"|cntry=="DE"|cntry=="IT"|cntry=="PL"|cntry=="ES"|cntry=="UK")
eu_silc11$cntry= haven::as_factor(eu_silc11$cntry)
eu_silc11$cntry <- gsub(",","",eu_silc11$cntry)
eu_silc11$cntry = as.numeric(as.factor(eu_silc11$cntry))
eu_silc11$cntry <- factor(eu_silc11$cntry,
                          levels = c(1, 2, 3,4,5,6),
                          labels = c("Germany", "Spain", "France","Italy","Poland", "United Kingdom"))

################################################ Gender
eu_silc11$gndr <- gsub(",","",eu_silc11$gndr)
eu_silc11$gndr = as.numeric(as.factor(eu_silc11$gndr))
eu_silc11$gndr[eu_silc11$gndr==2] = 0
eu_silc11$gndr = factor(eu_silc11$gndr,
                        levels = c(0,1),
                        labels = c("Female", "Male"))

################################################ Work hours
eu_silc11$workhours = as.numeric(eu_silc11$workhours)

############################################### Main activity 
#1= full time, 2= part time, 3=self-employed full time, 4=self employed part time, 5= unemployed, 6= student, 7=retirement, 8=permanently disabled, 9=military, 10=housework, 11= other
eu_silc11$mainact = as.integer(eu_silc11$mainact)
eu_silc11$employed <- NA
eu_silc11$employed[eu_silc11$mainact<=4] = 0
eu_silc11$employed[eu_silc11$mainact>=5] = 1
eu_silc11$employed <- factor(eu_silc11$employed,
                             levels = c(0,1),
                             labels = c("Employed", "Other"))

############################################### Parental education

#Father's education
eu_silc11$feduc =as.integer(eu_silc11$feduc)
eu_silc11$feduc2 <- NA
eu_silc11$feduc2[eu_silc11$feduc ==0 | eu_silc11$feduc ==1] <- 1
eu_silc11$feduc2[eu_silc11$feduc == 2] <- 2
eu_silc11$feduc2[eu_silc11$feduc == 3] <- 3
eu_silc11$feduc2= haven::as_factor(eu_silc11$feduc2)
eu_silc11$feduc2 <- factor(eu_silc11$feduc2,
                           levels = c(1,2,3),
                           labels = c("Low social origin", "Middle social origin", "High social origin"))

#Mother's education
eu_silc11$meduc2 <- NA
eu_silc11$meduc2[eu_silc11$meduc ==0 | eu_silc11$meduc ==1] <- 1
eu_silc11$meduc2[eu_silc11$meduc == 2] <- 2
eu_silc11$meduc2[eu_silc11$meduc == 3] <- 3
eu_silc11$meduc2= haven::as_factor(eu_silc11$meduc2)
eu_silc11$meduc2 <- factor(eu_silc11$meduc2,
                           levels = c(1,2,3),
                           labels = c("Low social origin", "Middle social origin", "High social origin"))

eu_silc11$feduc2 = as.integer(eu_silc11$feduc2)
eu_silc11$meduc2 = as.integer(eu_silc11$meduc2)

#Taking the highest education between both parents
eu_silc11$parents_educ = NA
eu_silc11$parents_educ =  pmax(eu_silc11$feduc2, eu_silc11$meduc2, na.rm=T)
eu_silc11$parents_educ <- factor(eu_silc11$parents_educ,
                                 levels = c(1,2,3),
                                 labels = c("Low social origin", "Middle social origin", "High social origin"))

############################################### Parental occupation
#ISCO 08-1 digit.
#Category "Armed forces"is not included in the analysis.

#Father's occupation
eu_silc11$foccup =as.integer(eu_silc11$foccup)
eu_silc11$foccup2 <- NA
eu_silc11$foccup2[eu_silc11$foccup >=8] <- 1
eu_silc11$foccup2[eu_silc11$foccup >=3 & eu_silc11$foccup<=7] <- 2
eu_silc11$foccup2[eu_silc11$foccup ==1 | eu_silc11$foccup ==2 ] <- 3
eu_silc11$foccup2= haven::as_factor(eu_silc11$foccup2)
eu_silc11$foccup2 <- factor(eu_silc11$foccup2,
                            levels = c(1,2,3),
                            labels = c("Working class", "Lower-middle class", "Upper-middle class"))

#Mother's occupation
eu_silc11$moccup =as.integer(eu_silc11$moccup)
eu_silc11$moccup2 <- NA
eu_silc11$moccup2[eu_silc11$moccup >=8] <- 1
eu_silc11$moccup2[eu_silc11$moccup >=3 & eu_silc11$moccup<=7] <- 2
eu_silc11$moccup2[eu_silc11$moccup ==1 | eu_silc11$moccup==2] <- 3
eu_silc11$moccup2= haven::as_factor(eu_silc11$moccup2)
eu_silc11$moccup2 <- factor(eu_silc11$moccup2,
                            levels = c(1,2,3),
                            labels = c("Working class", "Lower-middle class", "Upper-middle class"))

eu_silc11$foccup = as.integer(eu_silc11$foccup2)
eu_silc11$moccup = as.integer(eu_silc11$moccup2)

#Taking the highest social class between both parents
eu_silc11$parents_occup = NA
eu_silc11$parents_occup =  pmax(eu_silc11$foccup, eu_silc11$moccup, na.rm=T)
eu_silc11$parents_occup <- factor(eu_silc11$parents_occup,
                                  levels = c(1,2,3),
                                  labels = c("Working class", "Lower-middle class", "Upper-middle class"))

################################################ Respondent education 
# 0= pre-primary education, 1= primary education, 2= lower secondary education, 3=(upper) secondary education , 4=post-secondary non-tertiary education, 5= 1st & 2nd stage of tertiary education

eu_silc11$educ =as.integer(eu_silc11$educ)
eu_silc11$education <- NA
eu_silc11$education[eu_silc11$educ <3] <- 1
eu_silc11$education[eu_silc11$educ ==3] <- 2
eu_silc11$education[eu_silc11$educ >3] <- 3
eu_silc11$education= haven::as_factor(eu_silc11$education)
eu_silc11$education <- factor(eu_silc11$education,
                              levels = c(1,2,3),
                              labels = c("Lower secondary and below ", "Upper secondary ", "Post-secondary and tertiary "))

################################################ Period (or Crisis) 
eu_silc11$crisis = 1

################################################ Data source
eu_silc11$data= 0

################################################ Income
#Put missing gross earnings for Spain and Italy to harmonize with 2005 module. See footnote 2 in the manuscript.
eu_silc11$incomeg[eu_silc11$cntry =="Spain"] <- NA
eu_silc11$incomeg[eu_silc11$cntry =="Italy"] <- NA
eu_silc11$income= ifelse(is.na(eu_silc11$incomeg), eu_silc11$incomen, eu_silc11$incomeg)

#################################### drop unnecessary variables in 2011
keeps <- c("education", "cntry","workhours","age","parents_educ","crisis","year","data","parents_occup", "employed", "gndr", "income")
eu_silc11= eu_silc11[ , keeps, drop = FALSE]


################################################################################
######################### Merge all data sets ##################################
################################################################################


#Merge EU-SILC 2005 and 20011
eu_silc= bind_rows(eu_silc05, eu_silc11)
eu_silc <- merge(eu_silc, CPI_deflator, by=c("cntry","year")) 

#Merge EU-SILC and ESS
m= bind_rows(eu_silc, ess)

#Filter the age range
m= filter(m, age>24 & age<35)

################################################ Recode and label some variables for final analyses
m$mainact = as.numeric(m$employed)
m$mainact[m$mainact ==1] <- 1
m$mainact[m$mainact ==2] <- 0

m$crisis <- factor(m$crisis,
                   levels = c(0,1),
                   labels = c("Prior-recession", "Post-recession"))

################################################ Income
#correct for inflation
m$income = m$income/m$cpi05
#We create zero earnings equal to 1 in order to log them later
m$income_imputed <- ifelse(m$income<1,1,m$income)
#Log income
m$ln_wage = log(m$income_imputed)

#Changing the base category
m$education <- relevel(m$education, 3)
m$parents_educ <- relevel( m$parents_educ, 3)
m$parents_occup <- relevel( m$parents_occup, 3)

m =  m %>% mutate(parents_educ = fct_relevel(parents_educ,"High social origin","Low social origin","Middle social origin")) 
m = m %>% mutate(cntry = fct_relevel(cntry,"France", "Germany", "Italy", "Poland", "Spain", "United Kingdom")) 

#Save the merged data sets in order to use it later for running the models and plotting figures
#save(m, file= "directory_path_file/m.Rda")

