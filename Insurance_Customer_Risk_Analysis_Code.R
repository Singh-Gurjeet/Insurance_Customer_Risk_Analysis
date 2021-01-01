#------------------------------------------------------------------
# Auto Insurance Customer Risk Analysis
# Singh, Gurjeet
#------------------------------------------------------------------

library(readr)
library(car)
library(fBasics)
library(ggplot2)
library(corrplot)
library(plyr)
library(gmodels)
library(MASS)
library(gridExtra)
library(pROC)

options(scipen = 999)
#-------------------------------------------------------------------------------
## 1 - DATA EXPLORATION
#-------------------------------------------------------------------------------

logit_insurance <- read_csv("Insurance_Data.csv")
#View(logit_insurance)

colnames(logit_insurance)[1] <- "INDEX"

View(logit_insurance)
#Understand the stats and summary
str(logit_insurance)
summary(logit_insurance)


#Getting the Frequency of the categorical variables
char_Freq <- lapply(logit_insurance[sapply(logit_insurance,is.character)], FUN = count)
char_stats <- ldply(char_Freq, data.frame)
names(char_stats) <- c("Variable Name", "Value", "Frequency")

#reviewing the stats
View(t(basicStats(logit_insurance[sapply(logit_insurance,is.numeric)])))
View(char_stats)


#----------------------------------------
##clean missing values with median values
#----------------------------------------

summary(logit_insurance)
##clean missing values with median values

logit_insurance$IMP_AGE <-ifelse(is.na(logit_insurance$AGE), 
                                 45,
                                 logit_insurance$AGE)
logit_insurance$M_AGE <- ifelse(is.na(logit_insurance$AGE), 
                                1, 0)

logit_insurance$IMP_YOJ <-ifelse(is.na(logit_insurance$YOJ), 
                                 11,
                                 logit_insurance$YOJ)
logit_insurance$M_YOJ <- ifelse(is.na(logit_insurance$YOJ), 
                                1, 0)

logit_insurance$IMP_INCOME <- ifelse(is.na(logit_insurance$INCOME) & is.na(logit_insurance$JOB), 54000, 
                              ifelse(is.na(logit_insurance$INCOME) & (logit_insurance$JOB == "Doctor"), 128000, 
                              ifelse(is.na(logit_insurance$INCOME) & (logit_insurance$JOB == "Lawyer"), 88000, 
                              ifelse(is.na(logit_insurance$INCOME) & (logit_insurance$JOB == "Manager"), 87000, 
                              ifelse(is.na(logit_insurance$INCOME) & (logit_insurance$JOB == "Professional"), 76000,
                              ifelse(is.na(logit_insurance$INCOME) & (logit_insurance$JOB == "Clerical"), 33000,
                              ifelse(is.na(logit_insurance$INCOME) & (logit_insurance$JOB == "z_Blue Collar"), 58000,
                              ifelse(is.na(logit_insurance$INCOME) & (logit_insurance$JOB == "Home Maker"), 12000,
                              ifelse(is.na(logit_insurance$INCOME) & (logit_insurance$JOB == "Student"), 6300,
                                     logit_insurance$INCOME)))))))))


logit_insurance$M_INCOME <- ifelse(is.na(logit_insurance$INCOME), 
                                   1, 0)

logit_insurance$IMP_HOME_VAL <-ifelse(is.na(logit_insurance$HOME_VAL), 
                                      162000,
                                      logit_insurance$HOME_VAL)
logit_insurance$M_HOME_VAL <- ifelse(is.na(logit_insurance$HOME_VAL), 
                                     1, 0)



logit_insurance$IMP_CAR_AGE <-     ifelse(is.na(logit_insurance$CAR_AGE), 8,
                                          ifelse(logit_insurance$CAR_AGE < 0, 0,
                                          logit_insurance$CAR_AGE))

logit_insurance$M_CAR_AGE <- ifelse(is.na(logit_insurance$CAR_AGE), 1,
                                    ifelse(logit_insurance$CAR_AGE < 0, 1,
                                           0))


logit_insurance$IMP_JOB <-  ifelse(is.na(logit_insurance$JOB) & logit_insurance$IMP_INCOME > 150000, "Doctor", 
                            ifelse(is.na(logit_insurance$JOB) & logit_insurance$IMP_INCOME > 100000, "Lawyer",
                            ifelse(is.na(logit_insurance$JOB) & logit_insurance$IMP_INCOME > 85000, "Manager",
                            ifelse(is.na(logit_insurance$JOB) & logit_insurance$IMP_INCOME > 75000, "Professional",
                            ifelse(is.na(logit_insurance$JOB) & logit_insurance$IMP_INCOME > 60000, "z_Blue Collar",
                            ifelse(is.na(logit_insurance$JOB) & logit_insurance$IMP_INCOME > 35000, "Clerical",
                            ifelse(is.na(logit_insurance$JOB) & logit_insurance$IMP_INCOME >= 12000, "Home Maker",
                            ifelse(is.na(logit_insurance$JOB) & logit_insurance$IMP_INCOME < 12000, "Student",
                            logit_insurance$JOB))))))))


logit_insurance$M_JOB <- ifelse(is.na(logit_insurance$JOB), 
                                1, 0)

#Print After fixing the values
char_Freq2 <- lapply(logit_insurance[sapply(logit_insurance,is.character)], FUN = count)
char_stats2 <- ldply(char_Freq2, data.frame)
names(char_stats2) <- c("Variable Name", "Value", "Frequency")

options(scipen = 999)
View(t(basicStats(logit_insurance[sapply(logit_insurance,is.numeric)])))
View(char_stats2)


logit_insurance[which(logit_insurance$IMP_JOB == "Doctor" & logit_insurance$IMP_INCOME ==0),
                c("IMP_JOB", "IMP_INCOME", "IMP_YOJ", "M_YOJ")]
logit_insurance[which(logit_insurance$IMP_JOB == "Lawyer" & logit_insurance$IMP_INCOME ==0),
                c("IMP_JOB", "IMP_INCOME", "IMP_YOJ", "M_YOJ")]
logit_insurance[which(logit_insurance$IMP_JOB == "Manager" & logit_insurance$IMP_INCOME ==0),
                c("IMP_JOB", "IMP_INCOME", "IMP_YOJ", "M_YOJ")]
logit_insurance[which(logit_insurance$IMP_JOB == "Professional" & logit_insurance$IMP_INCOME ==0), 
                c("IMP_JOB", "IMP_INCOME", "IMP_YOJ", "M_YOJ")]
logit_insurance[which(logit_insurance$IMP_JOB == "z_Blue Collar" & logit_insurance$IMP_INCOME ==0),
                c("IMP_JOB", "IMP_INCOME", "IMP_YOJ", "M_YOJ")]
logit_insurance[which(logit_insurance$IMP_JOB == "Home Maker" & logit_insurance$IMP_INCOME ==0), 
                c("IMP_JOB", "IMP_INCOME", "IMP_YOJ", "M_YOJ")]
logit_insurance[which(logit_insurance$IMP_JOB == "Student" & logit_insurance$IMP_INCOME ==0), 
                c("IMP_JOB", "IMP_INCOME", "IMP_YOJ", "M_YOJ")]


#----------------------------------------
##Histograms - Exploration
#----------------------------------------

##---------------
## Numerical Value by TARGET_FLAG
#----------------


#Predictable - yes use it
plot1 <-  ggplot(logit_insurance,mapping = aes(x=KIDSDRIV, y = (..density..) ,fill=TARGET_FLAG))+
    geom_histogram(colour="black") + 
    facet_grid(~TARGET_FLAG, labeller = label_both )+theme_bw() +
    labs(title = "Histogram of KIDSDRIV by TARGET_FLAG", x = "KIDSDRIV", y = "Density") +
    scale_y_continuous(breaks = seq(0, 10,by = 0.5))


#Predictable - yes use it
plot2 <-  ggplot(logit_insurance,mapping = aes(x=IMP_AGE, y = (..density..) ,fill=TARGET_FLAG))+
      geom_histogram(colour="black") + 
      facet_grid(~TARGET_FLAG, labeller = label_both )+theme_bw() +
      labs(title = "Histogram of IMP_AGE by TARGET_FLAG", x = "IMP_AGE", y = "Density") +
      scale_y_continuous(breaks = seq(0, 1,by = 0.005))

#Predictable - yes use it
plot3 <-  ggplot(logit_insurance,mapping = aes(x=HOMEKIDS, y = (..density..) ,fill=TARGET_FLAG))+
  geom_histogram(colour="black") + 
  facet_grid(~TARGET_FLAG, labeller = label_both )+theme_bw() +
  labs(title = "Histogram of HOMEKIDS by TARGET_FLAG", x = "HOMEKIDS", y = "Density") +
  scale_y_continuous(breaks = seq(0, 6,by = 0.5))


#Predictable - NO
plot4 <-  ggplot(logit_insurance,mapping = aes(x=IMP_YOJ, y = (..density..) ,fill=TARGET_FLAG))+
  geom_histogram(colour="black") + 
  facet_grid(~TARGET_FLAG, labeller = label_both )+theme_bw() +
  labs(title = "Histogram of IMP_YOJ by TARGET_FLAG", x = "IMP_YOJ", y = "Density") +
  scale_y_continuous(breaks = seq(0, 6,by = 0.02))

grid.arrange(plot1, plot2,plot3, plot4, nrow = 2)

#Predictable - Could be but those people might be just renters
plot5 <-  ggplot(logit_insurance,mapping = aes(x=IMP_HOME_VAL, y = (..density..) * 100000 ,fill=TARGET_FLAG))+
  geom_histogram(colour="black") + 
  facet_grid(~TARGET_FLAG, labeller = label_both )+theme_bw() +
  labs(title = "Histogram of IMP_HOME_VAL by TARGET_FLAG", x = "IMP_HOME_VAL", y = "Density X 100,000") +
  scale_y_continuous(breaks = seq(0, 2,by = 0.1))

#Predictable - yes use it
plot6 <-  ggplot(logit_insurance,mapping = aes(x=IMP_INCOME, y = (..density..) * 100000 ,fill=TARGET_FLAG))+
  geom_histogram(colour="black") + 
  facet_grid(~TARGET_FLAG, labeller = label_both )+theme_bw() +
  labs(title = "Histogram of IMP_INCOME by TARGET_FLAG", x = "IMP_INCOME", y = "Density X 100,000") +
  scale_y_continuous(breaks = seq(0, 2,by = 0.1))

#Predictable - yes use it
plot7 <-  ggplot(logit_insurance,mapping = aes(x=TRAVTIME, y = (..density..) * 100 ,fill=TARGET_FLAG))+
  geom_histogram(colour="black") + 
  facet_grid(~TARGET_FLAG, labeller = label_both )+theme_bw() +
  labs(title = "Histogram of TRAVTIME by TARGET_FLAG", x = "TRAVTIME", y = "Density * 100") +
  scale_y_continuous(breaks = seq(0, 5,by = 0.5)) 


#Predictable - yes use it
plot8 <-  ggplot(logit_insurance,mapping = aes(x=BLUEBOOK, y = (..density..)  * 100000 ,fill=TARGET_FLAG))+
  geom_histogram(colour="black") + 
  facet_grid(~TARGET_FLAG, labeller = label_both )+theme_bw() +
  labs(title = "Histogram of BLUEBOOK by TARGET_FLAG", x = "BLUEBOOK", y = "Density X 100,000") +
  scale_y_continuous(breaks = seq(0, 10,by = 0.5)) 

grid.arrange(plot5, plot6, plot7, plot8, nrow = 2)

#Predictable - yes use it
plot9 <-  ggplot(logit_insurance,mapping = aes(x=TIF, y = (..density..) * 10 ,fill=TARGET_FLAG))+
  geom_histogram(colour="black") + 
  facet_grid(~TARGET_FLAG, labeller = label_both )+theme_bw() +
  labs(title = "Histogram of TIF by TARGET_FLAG", x = "TIF", y = "Density X 10") +
  scale_y_continuous(breaks = seq(0, 5,by = 0.5)) 


#Predictable - yes use it
plot10 <-  ggplot(logit_insurance,mapping = aes(x=OLDCLAIM, y = (..density..) * 1000 ,fill=TARGET_FLAG))+
  geom_histogram(colour="black") + 
  facet_grid(~TARGET_FLAG, labeller = label_both )+theme_bw() +
  labs(title = "Histogram of OLDCLAIM by TARGET_FLAG", x = "OLDCLAIM", y = "Density X 1000") +
  scale_y_continuous(breaks = seq(0, 1,by = 0.05)) 

#Predictable - yes use it
plot11 <-  ggplot(logit_insurance,mapping = aes(x=CLM_FREQ, y = (..density..)  ,fill=TARGET_FLAG))+
  geom_histogram(colour="black") + 
  facet_grid(~TARGET_FLAG, labeller = label_both )+theme_bw() +
  labs(title = "Histogram of CLM_FREQ by TARGET_FLAG", x = "CLM_FREQ", y = "Density") +
  scale_y_continuous(breaks = seq(0, 10,by = 0.5)) 

#Predictable - yes use it
plot12 <-  ggplot(logit_insurance,mapping = aes(x=MVR_PTS, y = (..density..)  ,fill=TARGET_FLAG))+
  geom_histogram(colour="black") + 
  facet_grid(~TARGET_FLAG, labeller = label_both )+theme_bw() +
  labs(title = "Histogram of MVR_PTS by TARGET_FLAG", x = "MVR_PTS", y = "Density") +
  scale_y_continuous(breaks = seq(0, 2,by = 0.1)) 

grid.arrange(plot9, plot10, plot11, plot12, nrow = 2)

#Predictable - yes use it
plot13 <-  ggplot(logit_insurance,mapping = aes(x=IMP_CAR_AGE, y = (..density..)  ,fill=TARGET_FLAG))+
  geom_histogram(colour="black") + 
  facet_grid(~TARGET_FLAG, labeller = label_both )+theme_bw() +
  labs(title = "Histogram of IMP_CAR_AGE by TARGET_FLAG", x = "IMP_CAR_AGE", y = "Density") +
  scale_y_continuous(breaks = seq(0, 1,by = 0.05)) 


##---------------
## Categorical Value
#----------------

# Predictable - % higher for single parent
plot14 <-  ggplot(logit_insurance,mapping = aes(x=TARGET_FLAG, y = (..density..),  group = PARENT1,fill=PARENT1))+
  geom_histogram(colour="black") + 
  facet_grid(~PARENT1, labeller = label_both )+theme_bw() +
  labs(title = "Histogram of TARGET_FLAG by PARENT1", x = "TARGET_FLAG", y = "Density") +
  scale_y_continuous(breaks = seq(0, 30,by =5)) 

# Predictable - % higher for Not married people
plot15 <-  ggplot(logit_insurance,mapping = aes(x=TARGET_FLAG, y = (..density..),  group = MSTATUS,fill=MSTATUS))+
  geom_histogram(colour="black") + 
  facet_grid(~MSTATUS, labeller = label_both )+theme_bw() +
  labs(title = "Histogram of TARGET_FLAG by MSTATUS", x = "TARGET_FLAG", y = "Density") +
  scale_y_continuous(breaks = seq(0, 30,by =5)) 

# Predictable - Could be - CounterIntitutive - Female at higher risk
plot16 <-  ggplot(logit_insurance,mapping = aes(x=TARGET_FLAG, y = (..density..),  group = SEX,fill=SEX))+
  geom_histogram(colour="black") + 
  facet_grid(~SEX, labeller = label_both )+theme_bw() +
  labs(title = "Histogram of TARGET_FLAG by SEX", x = "TARGET_FLAG", y = "Density") +
  scale_y_continuous(breaks = seq(0, 30,by =5)) 

grid.arrange(plot13, plot14, plot15, plot16, nrow = 2)

# Predictable - % is higher. Use Indicator variable Degree vs Non-degree
plot17 <-  ggplot(logit_insurance,mapping = aes(x=TARGET_FLAG, y = (..density..),  group = EDUCATION,fill=EDUCATION))+
  geom_histogram(colour="black") + 
  facet_grid(~EDUCATION )+theme_bw() +
  #facet_grid(~EDUCATION, labeller = label_both )+theme_bw() +
  labs(title = "Histogram of TARGET_FLAG by EDUCATION", x = "TARGET_FLAG", y = "Density") +
  scale_y_continuous(breaks = seq(0, 100,by =5)) + 
  scale_x_continuous(breaks = seq(0, 1,by = 0.5))



# Predictable - % is higher. Use Indicator variable White collar vs Blue collar
plot18 <-  ggplot(logit_insurance,mapping = aes(x=TARGET_FLAG, y = (..density..),  group = IMP_JOB,fill=IMP_JOB))+
  geom_histogram(colour="black") + 
  facet_grid(~IMP_JOB )+theme_bw() +
  labs(title = "Histogram of TARGET_FLAG by IMP_JOB", x = "TARGET_FLAG", y = "Density") +
  scale_y_continuous(breaks = seq(0, 30,by =5))  + 
  scale_x_continuous(breaks = seq(0, 1,by = 0.5))



# Predictable - % is higher for commercial
plot19 <-  ggplot(logit_insurance,mapping = aes(x=TARGET_FLAG, y = (..density..),  group = CAR_USE,fill=CAR_USE))+
  geom_histogram(colour="black") + 
  facet_grid(~CAR_USE)+theme_bw() +
  labs(title = "Histogram of TARGET_FLAG by CAR_USE", x = "TARGET_FLAG", y = "Density") +
  scale_y_continuous(breaks = seq(0, 30,by =5)) + 
  scale_x_continuous(breaks = seq(0, 1,by = 0.5)) 


# Predictable - check with average Target flag = = 1 and determine.
plot20 <-  ggplot(logit_insurance,mapping = aes(x=TARGET_FLAG, y = (..density..),  group = CAR_TYPE,fill=CAR_TYPE))+
  geom_histogram(colour="black") + 
  facet_grid(~CAR_TYPE )+theme_bw() +
  labs(title = "Histogram of TARGET_FLAG by CAR_TYPE", x = "TARGET_FLAG", y = "Density") +
  scale_y_continuous(breaks = seq(0, 30,by =5)) + 
  scale_x_continuous(breaks = seq(0, 1,by = 0.5))

grid.arrange(plot17, plot18, plot19, plot20, nrow = 2)

# Predictable - NO
plot21 <-  ggplot(logit_insurance,mapping = aes(x=TARGET_FLAG, y = (..density..),  group = RED_CAR,fill=RED_CAR))+
  geom_histogram(colour="black") + 
  facet_grid(~RED_CAR )+theme_bw() +
  labs(title = "Histogram of TARGET_FLAG by RED_CAR", x = "TARGET_FLAG", y = "Density") +
  scale_y_continuous(breaks = seq(0, 30,by =2)) + 
  scale_x_continuous(breaks = seq(0, 1,by = 0.5))


# Predictable - YES
plot22 <-  ggplot(logit_insurance,mapping = aes(x=TARGET_FLAG, y = (..density..),  group = REVOKED,fill=REVOKED))+
  geom_histogram(colour="black") + 
  facet_grid(~REVOKED)+theme_bw() +
  labs(title = "Histogram of TARGET_FLAG by REVOKED", x = "TARGET_FLAG", y = "Density") +
  scale_y_continuous(breaks = seq(0, 30,by =5)) + 
  scale_x_continuous(breaks = seq(0, 1,by = 0.5))



# Predictable - YES - Urban city is higher for urban
plot23 <-  ggplot(logit_insurance,mapping = aes(x=TARGET_FLAG, y = (..density..),  group = URBANICITY,fill=URBANICITY))+
  geom_histogram(colour="black") + 
  facet_grid(~URBANICITY)+theme_bw() +
  labs(title = "Histogram of TARGET_FLAG by URBANICITY", x = "TARGET_FLAG", y = "Density") +
  scale_y_continuous(breaks = seq(0, 30,by =5)) + 
  scale_x_continuous(breaks = seq(0, 1,by = 0.5))

grid.arrange(plot21, plot22, plot23, nrow = 2)

#----------------------------------------
##Variable selection Exploration - DO After Fixing missing variables and outliers
#----------------------------------------

#This provides the total Accident versus non accident splits with prop
CrossTable('Total' = logit_insurance$JOB, logit_insurance$TARGET_FLAG)


CrossTable(logit_insurance$KIDSDRIV, logit_insurance$TARGET_FLAG, prop.r = TRUE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE, row.labels = TRUE,
           dnn = c("KIDSDRIV", "TARGET_FLAG"))


TRUECrossTable(logit_insurance$HOMEKIDS, logit_insurance$TARGET_FLAG, prop.r = TRUE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE, row.labels = TRUE,
           dnn = c("HOMEKIDS", "TARGET_FLAG"))


CrossTable(logit_insurance$PARENT1, logit_insurance$TARGET_FLAG, prop.r = TRUE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE, row.labels = TRUE,
           dnn = c("PARENT1", "TARGET_FLAG"))


CrossTable(logit_insurance$MSTATUS, logit_insurance$TARGET_FLAG, prop.r = TRUE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE, row.labels = TRUE,
           dnn = c("MSTATUS", "TARGET_FLAG"))


CrossTable(logit_insurance$SEX, logit_insurance$TARGET_FLAG, prop.r = TRUE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE, row.labels = TRUE,
           dnn = c("SEX", "TARGET_FLAG"))


CrossTable(logit_insurance$EDUCATION, logit_insurance$TARGET_FLAG, prop.r = TRUE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE, row.labels = TRUE,
           dnn = c("EDUCATION", "TARGET_FLAG"))


CrossTable(logit_insurance$JOB, logit_insurance$TARGET_FLAG, prop.r = TRUE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE, row.labels = TRUE,
           dnn = c("JOB", "TARGET_FLAG"))


CrossTable(logit_insurance$CAR_USE, logit_insurance$TARGET_FLAG, prop.r = TRUE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE, row.labels = TRUE,
           dnn = c("CAR_USE", "TARGET_FLAG"))


CrossTable(logit_insurance$CAR_TYPE, logit_insurance$TARGET_FLAG, prop.r = TRUE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE, row.labels = TRUE,
           dnn = c("CAR_TYPE", "TARGET_FLAG"))


CrossTable(logit_insurance$RED_CAR, logit_insurance$TARGET_FLAG, prop.r = TRUE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE, row.labels = TRUE,
           dnn = c("RED_CAR", "TARGET_FLAG"))


CrossTable(logit_insurance$REVOKED, logit_insurance$TARGET_FLAG, prop.r = TRUE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE, row.labels = TRUE,
           dnn = c("REVOKED", "TARGET_FLAG"))


CrossTable(logit_insurance$URBANICITY, logit_insurance$TARGET_FLAG, prop.r = TRUE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE, row.labels = TRUE,
           dnn = c("URBANICITY", "TARGET_FLAG"))


CrossTable(logit_insurance$IMP_AGE, logit_insurance$TARGET_FLAG, prop.r = TRUE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE, row.labels = TRUE,
           dnn = c("IMP_AGE", "TARGET_FLAG"))


CrossTable(logit_insurance$IMP_YOJ, logit_insurance$TARGET_FLAG, prop.r = TRUE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE, row.labels = TRUE,
           dnn = c("IMP_YOJ", "TARGET_FLAG"))


CrossTable(logit_insurance$IMP_CAR_AGE, logit_insurance$TARGET_FLAG, prop.r = TRUE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE, row.labels = TRUE,
           dnn = c("IMP_CAR_AGE", "TARGET_FLAG"))



#------------------------------------------------------------------------------
## 2 - DATA PREPARATION 
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
##Indicator Variables
#-------------------------------------------------------------------------------

logit_insurance$Single_Parent_Ind <- ifelse(logit_insurance$PARENT1 =='Yes',1,0);
logit_insurance$MSTATUS_Single_Ind <- ifelse(logit_insurance$MSTATUS =='Yes',0,1);
logit_insurance$SEX_Female_Ind <- ifelse(logit_insurance$SEX =='M',0,1);
logit_insurance$Ed_Non_Degree_Ind <- ifelse(logit_insurance$EDUCATION =='<High School' |
                                              logit_insurance$EDUCATION =='z_High School', 1,0)
logit_insurance$CU_Commercial_Ind <- ifelse(logit_insurance$CAR_USE =='Commercial',1,0);


logit_insurance$CT_Panel_Truck_Ind <- ifelse(logit_insurance$CAR_TYPE =='Panel Truck',1,0);
logit_insurance$CT_Pickup_Ind <- ifelse(logit_insurance$CAR_TYPE =='Pickup',1,0);
logit_insurance$CT_Sports_Car_Ind <- ifelse(logit_insurance$CAR_TYPE =='Sports Car',1,0);
logit_insurance$CT_Van_Ind <- ifelse(logit_insurance$CAR_TYPE =='Van',1,0);
logit_insurance$CT_SUV_Ind <- ifelse(logit_insurance$CAR_TYPE =='z_SUV',1,0);

logit_insurance$REVOKED_Ind <- ifelse(logit_insurance$REVOKED =='Yes',1,0);

logit_insurance$UC_HUU_Ind <- ifelse(logit_insurance$URBANICITY =='Highly Urban/ Urban',1,0);


logit_insurance$JOB_White_Collar_Ind <- ifelse(logit_insurance$IMP_JOB =='Clerical' |
                                              logit_insurance$IMP_JOB =='Doctor'|
                                                logit_insurance$IMP_JOB =='Lawyer' |
                                                logit_insurance$IMP_JOB =='Manager' |
                                                logit_insurance$IMP_JOB =='Professional', 1,0)
logit_insurance$JOB_Blue_Collar_Ind <- ifelse(logit_insurance$IMP_JOB =='z_Blue Collar',1,0);
logit_insurance$JOB_Student_Ind <- ifelse(logit_insurance$IMP_JOB =='Student',1,0);

logit_insurance$Home_Owner_else_Renter_Ind <- ifelse(logit_insurance$IMP_HOME_VAL != 0 ,1,0);



View(t(basicStats(logit_insurance[sapply(logit_insurance,is.numeric)])))

#--------------------------
##creating a drop list
#-------------------------------------------------------------------------------
names(logit_insurance)

# logit_insurance2 <- logit_insurance
# logit_insurance <- logit_insurance2

#creating a drop list to remove not required variables.
drop.list <- c('INDEX','TARGET_AMT','AGE', 'YOJ',
               'INCOME','PARENT1', 'HOME_VAL','MSTATUS','SEX','EDUCATION','JOB',
               'CAR_USE','BLUEBOOK','CAR_TYPE', 'RED_CAR','REVOKED','CAR_AGE','URBANICITY',
               'IMP_YOJ','M_YOJ','IMP_HOME_VAL', 'M_HOME_VAL', 'IMP_CAR_AGE', 'M_CAR_AGE',
                'IMP_JOB', 'M_JOB'
               )

#droping the variables
logit_insurance <- logit_insurance[,!(names(logit_insurance) %in% drop.list )]

names(logit_insurance)
summary(logit_insurance)


#------------------------------------------------------------------
#Requirement # Add a train/test flag to split the sample  
#------------------------------------------------------------------


logit_insurance$u <- runif(n=dim(logit_insurance)[1],min=0,max=1);

logit_insurance$train <- ifelse(logit_insurance$u<0.70,1,0);


# Save the R data frame as an .RData object
saveRDS(logit_insurance,file= );



# Check the counts on the train/test split
table(logit_insurance$train)

# Check the train/test split as a percentage of whole
table(logit_insurance$train)/dim(logit_insurance)[1]





#------------------------------------------------------------------------------
## 3- Build Models
#-------------------------------------------------------------------------------
# Read (or reload) the .RData object as an R data frame
logit_insurance <- readRDS(file= "logit_insurance_train.RData");

names(logit_insurance)

# Create train/test split;
train.df <- subset(logit_insurance, train==1);
test.df <- subset(logit_insurance, train==0);

cutoff <- 0.50

# #------------------------------
# ### ##Model_1_GLM - STEP AIC
# #------------------------------
# Define the upper model as the FULL model
upper.glm <- glm(TARGET_FLAG ~ . -train -u
                    ,family = binomial(link = "logit")
                    ,data = train.df )
summary(upper.glm)

# Define the lower model as the Intercept model
lower.glm <- glm(TARGET_FLAG ~ 1
                ,family = binomial(link = "logit")
                ,data = train.df );
summary(lower.glm)

# Need a SLR to initialize stepwise selection
start.glm <- glm(TARGET_FLAG ~ CLM_FREQ
                ,family = binomial(link = "logit")
                ,data = train.df );
summary(start.glm)

Model_1_GLM <- stepAIC(object=start.glm,scope=list(upper=formula(upper.glm),lower=~1),
                       direction=c('both'));
summary(Model_1_GLM)

par(mfrow=c(1,2))

## ROC Curve and AUC based on Train data set
Model_1_GLM_pred_train <- predict(Model_1_GLM, type = "response")
ROC_Model_1_GLM_train <- roc(train.df$TARGET_FLAG, Model_1_GLM_pred_train  )

##test data
Model_1_GLM_pred_test <- predict(Model_1_GLM, newdata = test.df, type = "response")
ROC_Model_1_GLM_test <- roc(test.df$TARGET_FLAG, Model_1_GLM_pred_test  )


#confusion Matrix work
class_pred_Model_1_train <- ifelse(Model_1_GLM_pred_train > cutoff,1,0)
class_pred_Model_1_test <- ifelse(Model_1_GLM_pred_test > cutoff,1,0)

Conf_Matrix_Model_1_train <- table(train.df$TARGET_FLAG,class_pred_Model_1_train)
Conf_Matrix_Model_1_test <- table(test.df$TARGET_FLAG,class_pred_Model_1_test)

#compute the classification accuraxy
acc_Model_1_train <- sum(diag(Conf_Matrix_Model_1_train))/nrow(train.df)
acc_Model_1_test <- sum(diag(Conf_Matrix_Model_1_test))/nrow(test.df)


## ROC Curve for Test and Train data set
plot.roc(ROC_Model_1_GLM_train, col = "blue", main = "ROC Curve using Train Data")
plot.roc(ROC_Model_1_GLM_test, col = "blue", main = "ROC Curve using Train Data")

## AUC value of Train and Test data
auc(ROC_Model_1_GLM_train)
auc(ROC_Model_1_GLM_test)

# #------------------------------
##Model_2_GLM
# #------------------------------
Model_2_GLM <-glm(TARGET_FLAG ~ CLM_FREQ + UC_HUU_Ind + IMP_INCOME + M_INCOME + 
                CU_Commercial_Ind + Single_Parent_Ind + REVOKED_Ind + HOMEKIDS + 
              MVR_PTS + TRAVTIME + Ed_Non_Degree_Ind +   MSTATUS_Single_Ind + 
                TIF + KIDSDRIV + CT_Sports_Car_Ind + CT_SUV_Ind + 
                CT_Pickup_Ind + CT_Van_Ind + OLDCLAIM + SEX_Female_Ind+ 
                Home_Owner_else_Renter_Ind
                ,family = binomial(link = "logit"),data = train.df )
summary(Model_2_GLM)
sort(vif(Model_2_GLM), decreasing = TRUE)


## ROC Curve and AUC based on Train data set
Model_2_GLM_pred_train <- predict(Model_2_GLM, type = "response")
ROC_Model_2_GLM_train <- roc(train.df$TARGET_FLAG, Model_2_GLM_pred_train  )

##test data
Model_2_GLM_pred_test <- predict(Model_2_GLM, newdata = test.df, type = "response")
ROC_Model_2_GLM_test <- roc(test.df$TARGET_FLAG, Model_2_GLM_pred_test  )



  #confusion Matrix work
  class_pred_Model_2_train <- ifelse(Model_2_GLM_pred_train > cutoff,1,0)
  class_pred_Model_2_test <- ifelse(Model_2_GLM_pred_test > cutoff,1,0)
  
  Conf_Matrix_Model_2_train <- table(train.df$TARGET_FLAG,class_pred_Model_2_train)
  Conf_Matrix_Model_2_test <- table(test.df$TARGET_FLAG,class_pred_Model_2_test)
  
  #compute the classification accuraxy
  acc_Model_2_train <- sum(diag(Conf_Matrix_Model_2_train))/nrow(train.df)
  acc_Model_2_test <- sum(diag(Conf_Matrix_Model_2_test))/nrow(test.df)





## ROC Curve for Test and Train data set
plot.roc(ROC_Model_2_GLM_train, col = "red", main = "ROC Curve using Train Data")
plot.roc(ROC_Model_2_GLM_test, col = "red", main = "ROC Curve using Train Data")

## AUC value of Train and Test data
auc(ROC_Model_2_GLM_train)
auc(ROC_Model_2_GLM_test)

# #------------------------------
##Model_3_GLM
# #------------------------------

Model_3_GLM <-glm(TARGET_FLAG ~ KIDSDRIV + TRAVTIME + TIF +  CT_Pickup_Ind + 
                CLM_FREQ + MVR_PTS + IMP_AGE + M_AGE + CT_Van_Ind + HOMEKIDS + 
                IMP_INCOME + M_INCOME + Home_Owner_else_Renter_Ind + Single_Parent_Ind + 
                MSTATUS_Single_Ind + Ed_Non_Degree_Ind + 
                CU_Commercial_Ind + CT_Sports_Car_Ind +  OLDCLAIM +
                 CT_SUV_Ind + REVOKED_Ind + UC_HUU_Ind #+ SEX_Female_Ind
              ,family = binomial(link = "logit"),data = train.df )
summary(Model_3_GLM)

names(logit_insurance)

## ROC Curve and AUC based on Train data set
Model_3_GLM_pred_train <- predict(Model_3_GLM, type = "response")
ROC_Model_3_GLM_train <- roc(train.df$TARGET_FLAG, Model_3_GLM_pred_train  )

##test data
Model_3_GLM_pred_test <- predict(Model_3_GLM, newdata = test.df, type = "response")
ROC_Model_3_GLM_test <- roc(test.df$TARGET_FLAG, Model_3_GLM_pred_test  )


  #confusion Matrix work
  class_pred_Model_3_train <- ifelse(Model_3_GLM_pred_train > cutoff,1,0)
  class_pred_Model_3_test <- ifelse(Model_3_GLM_pred_test > cutoff,1,0)
  
  Conf_Matrix_Model_3_train <- table(train.df$TARGET_FLAG,class_pred_Model_3_train)
  Conf_Matrix_Model_3_test <- table(test.df$TARGET_FLAG,class_pred_Model_3_test)
  
  #compute the classification accuraxy
  acc_Model_3_train <- sum(diag(Conf_Matrix_Model_3_train))/nrow(train.df)
  acc_Model_3_test <- sum(diag(Conf_Matrix_Model_3_test))/nrow(test.df)



## ROC Curve for Test and Train data set
plot.roc(ROC_Model_3_GLM_train, col = "dark green", main = "ROC Curve using Train Data")
plot.roc(ROC_Model_3_GLM_test, col = "dark green", main = "ROC Curve using Train Data")

## AUC value of Train and Test data
auc(ROC_Model_3_GLM_train)
auc(ROC_Model_3_GLM_test)


# #------------------------------
# ##Model_4_GLM
# #------------------------------

Model_4_GLM <-glm(TARGET_FLAG ~ CLM_FREQ + UC_HUU_Ind + IMP_INCOME + M_INCOME +  
                CU_Commercial_Ind + Single_Parent_Ind + REVOKED_Ind + MVR_PTS +
                TRAVTIME + Ed_Non_Degree_Ind + MSTATUS_Single_Ind + TIF + 
                KIDSDRIV +   CT_Sports_Car_Ind + CT_SUV_Ind + 
                CT_Pickup_Ind + CT_Van_Ind +
                 Home_Owner_else_Renter_Ind        
              ,family = binomial(link = "logit"),data = train.df )
summary(Model_4_GLM)

## ROC Curve and AUC based on Train data set
Model_4_GLM_pred_train <- predict(Model_4_GLM, type = "response")
ROC_Model_4_GLM_train <- roc(train.df$TARGET_FLAG, Model_4_GLM_pred_train  )

##test data
Model_4_GLM_pred_test <- predict(Model_4_GLM, newdata = test.df, type = "response")
ROC_Model_4_GLM_test <- roc(test.df$TARGET_FLAG, Model_4_GLM_pred_test  )



  #confusion Matrix work
  class_pred_Model_4_train <- ifelse(Model_4_GLM_pred_train > cutoff,1,0)
  class_pred_Model_4_test <- ifelse(Model_4_GLM_pred_test > cutoff,1,0)
  
  Conf_Matrix_Model_4_train <- table(train.df$TARGET_FLAG,class_pred_Model_4_train)
  Conf_Matrix_Model_4_test <- table(test.df$TARGET_FLAG,class_pred_Model_4_test)
  
  #compute the classification accuraxy
  acc_Model_4_train <- sum(diag(Conf_Matrix_Model_4_train))/nrow(train.df)
  acc_Model_4_test <- sum(diag(Conf_Matrix_Model_4_test))/nrow(test.df)
  


## ROC Curve for Test and Train data set
plot.roc(ROC_Model_4_GLM_train, col = "dark green", main = "ROC Curve using Train Data")
plot.roc(ROC_Model_4_GLM_test, col = "dark green", main = "ROC Curve using Train Data")

## AUC value of Train and Test data
auc(ROC_Model_4_GLM_train)
auc(ROC_Model_4_GLM_test)

# #------------------------------
# ##Model_5_GLM
# #------------------------------


Model_5_GLM <-glm(TARGET_FLAG ~ CLM_FREQ + UC_HUU_Ind +   
                CU_Commercial_Ind + Single_Parent_Ind + REVOKED_Ind + MVR_PTS +
                TRAVTIME + Ed_Non_Degree_Ind + MSTATUS_Single_Ind + TIF + 
                KIDSDRIV +   CT_Sports_Car_Ind + CT_SUV_Ind + 
                CT_Pickup_Ind + CT_Van_Ind +  
              OLDCLAIM + Home_Owner_else_Renter_Ind  
              ,family = binomial(link = "logit"),data = train.df )
summary(Model_5_GLM)

names(logit_insurance)

## ROC Curve and AUC based on Train data set
Model_5_GLM_pred_train <- predict(Model_5_GLM, type = "response")
ROC_Model_5_GLM_train <- roc(train.df$TARGET_FLAG, Model_5_GLM_pred_train  )

##test data
Model_5_GLM_pred_test <- predict(Model_5_GLM, newdata = test.df, type = "response")
ROC_Model_5_GLM_test <- roc(test.df$TARGET_FLAG, Model_5_GLM_pred_test  )


    #confusion Matrix work
    class_pred_Model_5_train <- ifelse(Model_5_GLM_pred_train > cutoff,1,0)
    class_pred_Model_5_test <- ifelse(Model_5_GLM_pred_test > cutoff,1,0)
    
    Conf_Matrix_Model_5_train <- table(train.df$TARGET_FLAG,class_pred_Model_5_train)
    Conf_Matrix_Model_5_test <- table(test.df$TARGET_FLAG,class_pred_Model_5_test)
    
    #compute the classification accuraxy
    acc_Model_5_train <- sum(diag(Conf_Matrix_Model_5_train))/nrow(train.df)
    acc_Model_5_test <- sum(diag(Conf_Matrix_Model_5_test))/nrow(test.df)
    


## ROC Curve for Test and Train data set
plot.roc(ROC_Model_5_GLM_train, col = "dark green", main = "ROC Curve using Train Data")
plot.roc(ROC_Model_5_GLM_test, col = "dark green", main = "ROC Curve using Train Data")

## AUC value of Train and Test data
auc(ROC_Model_5_GLM_train)
auc(ROC_Model_5_GLM_test)


# #-----------------------------------------------------------------------------
# ## 4- SELECT MODELS - Predictive Accuracy 
# #-----------------------------------------------------------------------------
# 
summary(Model_1_GLM)
summary(Model_2_GLM)
summary(Model_3_GLM)
summary(Model_4_GLM)
summary(Model_5_GLM)

AIC(Model_1_GLM)
AIC(Model_2_GLM)
AIC(Model_3_GLM)
AIC(Model_4_GLM)
AIC(Model_5_GLM)


## AUC value of Train and Test data
auc(ROC_Model_1_GLM_train)
auc(ROC_Model_1_GLM_test)

auc(ROC_Model_2_GLM_train)
auc(ROC_Model_2_GLM_test)

auc(ROC_Model_3_GLM_train)
auc(ROC_Model_3_GLM_test)

auc(ROC_Model_4_GLM_train)
auc(ROC_Model_4_GLM_test)


auc(ROC_Model_5_GLM_train)
auc(ROC_Model_5_GLM_test)




options(scipen = 999)
ks.test(Model_1_GLM_pred_train, train.df$TARGET_FLAG)$statistic
ks.test(Model_1_GLM_pred_test, test.df$TARGET_FLAG)$statistic

ks.test(Model_2_GLM_pred_train, train.df$TARGET_FLAG)$statistic
ks.test(Model_2_GLM_pred_test, test.df$TARGET_FLAG)$statistic

ks.test(Model_3_GLM_pred_train, train.df$TARGET_FLAG)$statistic
ks.test(Model_3_GLM_pred_test, test.df$TARGET_FLAG)$statistic

ks.test(Model_4_GLM_pred_train, train.df$TARGET_FLAG)$statistic
ks.test(Model_4_GLM_pred_test, test.df$TARGET_FLAG)$statistic


ks.test(Model_5_GLM_pred_train, train.df$TARGET_FLAG)$statistic
ks.test(Model_5_GLM_pred_test, test.df$TARGET_FLAG)$statistic


##Classification Accuracy test

acc_Model_1_train
acc_Model_1_test

acc_Model_2_train
acc_Model_2_test

acc_Model_3_train
acc_Model_3_test

acc_Model_4_train
acc_Model_4_test

acc_Model_5_train
acc_Model_5_test



1

plot.roc(ROC_step.Mod, col = "Yellow")
lines.roc(ROC_step.Mod, col = "Yellow")
auc(ROC_step.Mod)

lines.roc(ROC_model_3, col = "red")
auc(ROC_model_3)


lines.roc(ROC_model_4, col = "purple")
auc(ROC_model_4)

lines.roc(ROC_model_5, col = "blue")
auc(ROC_model_5)

lines.roc(ROC_model_5b, col = "orange")
auc(ROC_model_5b)
# 
# #------------------------------------------------------------------------------
# ## 3- Build Models
# #-------------------------------------------------------------------------------
# 
# #------------------------------
# ##Model_1_lm 
# #------------------------------
# names(MoneyBall)
# 
# ##Adjusted R-squared:     0.3151  
# Model_1_lm <- lm(TARGET_WINS ~ IMP_TEAM_BATTING_H  + 
#                             IMP_TEAM_BATTING_BB +
#                             IMP_TEAM_BASERUN_SB + M_TEAM_BASERUN_SB + 
#                             IMP_TEAM_FIELDING_E +
#                             IMP_TEAM_PITCHING_BB +
#                             IMP_TEAM_PITCHING_H
#                           ,data=MoneyBall)
# 
# summary(Model_1_lm)
# 
# #------------------------------
# ##Model_1_lm - Assessing the Goodness-Of-Fit in OLS Regression
# #------------------------------ 
# 
# # Validating the normality assumption:
# par(mfrow = c(1,2))
# #Creating 2 Q-Q plots to evaluate the distribution of 
# # SalePrice and L_SalePrice
# qqnorm(Model_1_lm$residuals, main = "Q-Q plot, Rediduals", 
#        xlab="Theoretical Quantiles", col = "black",
#        ylab="Standardized residuals",datax=FALSE)
# 
# qqline(Model_1_lm$residuals, datax=FALSE, distribution=qnorm,
#        probs=c(0.25,0.75),qtype=7, col = "red")
# 
# hist(Model_1_lm$residuals, breaks = "FD", col = "violet"); box();
# par(mfrow = c(1,1))
# 
# #Validating the homoscedasticity assumption (equal variance):
# residualPlots(Model_1_lm)
#     # par(mfrow = c(1,1))
#     # residualPlot(Model_1_lm)
# 
# #------------------------------
# ##Model_2_lm 
# #------------------------------
# names(MoneyBall)
# 
# ##Adjusted R-squared:     0.3148  
# Model_2_lm <- lm(TARGET_WINS ~ IMP_TEAM_BATTING_H  + 
#                              IMP_TEAM_BATTING_BB +
#                              IMP_TEAM_BASERUN_SB + M_TEAM_BASERUN_SB + 
#                              IMP_TEAM_FIELDING_E +
#                              IMP_TEAM_PITCHING_BB +
#                              IMP_TEAM_PITCHING_H +
#                              IMP_TEAM_PITCHING_HR
#                            ,data=MoneyBall)
# 
# summary(Model_2_lm)
# 
# #------------------------------
# ##Model_2_lm - Assessing the Goodness-Of-Fit in OLS Regression
# #------------------------------ 
# 
# # Validating the normality assumption:
# par(mfrow = c(1,2))
# #Creating 2 Q-Q plots to evaluate the distribution of 
# # SalePrice and L_SalePrice
# qqnorm(Model_2_lm$residuals, main = "Q-Q plot, Rediduals", 
#        xlab="Theoretical Quantiles", col = "black",
#        ylab="Standardized residuals",datax=FALSE)
# 
# qqline(Model_2_lm$residuals, datax=FALSE, distribution=qnorm,
#        probs=c(0.25,0.75),qtype=7, col = "red")
# 
# hist(Model_2_lm$residuals, breaks = "FD", col = "violet"); box();
# par(mfrow = c(1,1))
# 
# #Validating the homoscedasticity assumption (equal variance):
# residualPlots(Model_2_lm)
# par(mfrow = c(1,1))
# residualPlot(Model_2_lm)
# 
# 
# 
# #------------------------------
# ##Model_3_lm
# #------------------------------
# names(MoneyBall)
# 
# ##Adjusted R-squared:     0.3344  
# Model_3_lm <- lm(TARGET_WINS ~ IMP_TEAM_BATTING_H  + 
#                              IMP_TEAM_BATTING_BB +
#                              IMP_TEAM_BASERUN_SB + M_TEAM_BASERUN_SB + 
#                              IMP_TEAM_BASERUN_CS + M_TEAM_BASERUN_CS +
#                              IMP_TEAM_PITCHING_H +
#                              IMP_TEAM_PITCHING_BB +
#                              IMP_TEAM_FIELDING_E
#                            ,data=MoneyBall)
# 
# summary(Model_3_lm)
# 
# #------------------------------
# ##Model_3_lm - Assessing the Goodness-Of-Fit in OLS Regression
# #------------------------------ 
# 
# # Validating the normality assumption:
# par(mfrow = c(1,2))
# #Creating 2 Q-Q plots to evaluate the distribution of 
# # SalePrice and L_SalePrice
# qqnorm(Model_3_lm$residuals, main = "Q-Q plot, Rediduals", 
#        xlab="Theoretical Quantiles", col = "black",
#        ylab="Standardized residuals",datax=FALSE)
# 
# qqline(Model_3_lm$residuals, datax=FALSE, distribution=qnorm,
#        probs=c(0.25,0.75),qtype=7, col = "red")
# 
# hist(Model_3_lm$residuals, breaks = "FD", col = "violet"); box();
# par(mfrow = c(1,1))
# 
# #Validating the homoscedasticity assumption (equal variance):
# residualPlots(Model_3_lm)
#     # par(mfrow = c(1,1))
#     # residualPlot(Model_3_lm)
# 
# #-----------------------------------------------------------------------------
# ## 4- SELECT MODELS - Predictive Accuracy 
# #-----------------------------------------------------------------------------
# 
# #extract the model information from summary output
# Model_1_call <- Model_1_lm$call
# Model_2_call <- Model_2_lm$call
# Model_3_call <- Model_3_lm$call
# 
# #Printing the each model.
# Model_1_call
# 
# Model_2_call
# 
# Model_3_call
# 
# 
# # Compute the VIF values
# library(car)
# Model_1_lm.VIF <- as.matrix(sort(vif(Model_1_lm),decreasing=TRUE))
# Model_2_lm.VIF <- as.matrix(sort(vif(Model_2_lm),decreasing=TRUE))
# Model_3_lm.VIF <- as.matrix(sort(vif(Model_3_lm),decreasing=TRUE))
# 
# 
# colnames(Model_1_lm.VIF) <- "VIF_Values"
# colnames(Model_2_lm.VIF) <- "VIF_Values"
# colnames(Model_3_lm.VIF) <- "VIF_Values"
# 
# 
# View(Model_1_lm.VIF)
# View(Model_2_lm.VIF)
# View(Model_3_lm.VIF)
# 
# ##MSE
# mse.Model_1_lm <- mean(Model_1_lm$residuals^2)
# mse.Model_2_lm <- mean(Model_2_lm$residuals^2)
# mse.Model_3_lm <- mean(Model_3_lm$residuals^2)
# 
# 
# ##MAE
# mae.Model_1_lm <- mean(abs(Model_1_lm$residuals))
# mae.Model_2_lm <- mean(abs(Model_2_lm$residuals))
# mae.Model_3_lm <- mean(abs(Model_3_lm$residuals))
# 
# 
# 
# ##Creating a Table to include all the metrics
# rsqrd.Mat <- matrix(c(summary(Model_1_lm)$adj.r.squared, 
#                       summary(Model_2_lm)$adj.r.squared, 
#                       summary(Model_3_lm)$adj.r.squared), 
#                     ncol = 1)
# 
# rownames(rsqrd.Mat) <- c("Model_1_lm", "Model_2_lm", "Model_3_lm")
# colnames(rsqrd.Mat) <- "Adjusted_R_Squared"
# 
# 
# AIC.Mat <- matrix(c(AIC(Model_1_lm), 
#                     AIC(Model_2_lm), 
#                     AIC(Model_3_lm)), 
#                   ncol = 1)
# rownames(AIC.Mat) <- c("Model_1_lm", "Model_2_lm", "Model_3_lm")
# colnames(AIC.Mat) <- "AIC_Values"
# 
# 
# BIC.Mat <- matrix(c(BIC(Model_1_lm), 
#                     BIC(Model_2_lm), 
#                     BIC(Model_3_lm)), 
#                   ncol = 1)
# rownames(BIC.Mat) <-  c("Model_1_lm", "Model_2_lm", "Model_3_lm")
# colnames(BIC.Mat) <- "BIC_Values"
# 
# 
# MSE.Mat <- matrix(c(mse.Model_1_lm, 
#                     mse.Model_2_lm, 
#                     mse.Model_3_lm), 
#                   ncol = 1)
# rownames(MSE.Mat) <-  c("Model_1_lm", "Model_2_lm", "Model_3_lm")
# colnames(MSE.Mat) <- "MSE_Values"
# 
# MAE.Mat <- matrix(c(mae.Model_1_lm, 
#                     mae.Model_2_lm, 
#                     mae.Model_3_lm), 
#                   ncol = 1)
# rownames(MAE.Mat) <-  c("Model_1_lm", "Model_2_lm", "Model_3_lm")
# colnames(MAE.Mat) <- "MAE_Values"
# 
# 
# final.table  <- cbind( rsqrd.Mat, 
#                        AIC.Mat, 
#                        BIC.Mat, 
#                        MSE.Mat, 
#                        MAE.Mat)
# 
# View(final.table)







