# R-Project
# 11/10/2016
# Felix
# Descriptive statistics

library(ggplot2)

meanAge <- mean(sm[,'age']) #16.696

medianAge <- median(sm[,'age']) #17 

summary(sm[,'sex']) #F=208 #M=187

summary(sm[,'school']) #Gabriel Pereira = 349   #Mousinho da Silveira = 46

summary(sm[,'address']) #Rural = 77 #Urban = 307

summary(sm[,'famsize']) #Greater than 3 = 281 #Less than 3 = 114

summary(sm[,'Pstatus']) #A = 41 #T = 354

summary(sm[,'internet']) #no = 66 #yes=329

summary(sm[,'romantic']) #no=263 #yes=132

###

# Father's Job
summary(sm[,'Fjob'])
ggplot(data=sm) + geom_bar(aes((sm[,'Fjob']))) + labs(x="Father's Job")
# at_home   health  other  services  teacher 
#   20       18      217      111       29 

# Mother's Job
summary(sm[,'Mjob'])
ggplot(data=sm) + geom_bar(aes((sm[,'Mjob']))) + labs(x="Mother's Job")
# at_home   health  other  services  teacher 
# 59       34        141      103       58

# Student's Guardian
summary(sm[,'guardian'])
ggplot(data=sm) + geom_bar(aes(guardian)) + labs(x="Student's Guardian")
# father mother  other 
# 90    273     32 

# Amount of Workday Alchohol Consumption
ggplot(data=sm) + geom_bar(aes(sm[,'Dalc'])) + 
  labs(x='Workday Alcohol Consumption', y='Frequency') + 
  ggtitle('Amount of Workday Alchohol Consumption')


# Amount of Weekday Alchohol Consumption
ggplot(data=sm) + geom_bar(aes(sm[,'Walc'])) + 
  labs(x='Weekday Alcohol Consumption', y='Frequency') + 
  ggtitle('Amount of Weekday Alchohol Consumption')


###############################################################################################
