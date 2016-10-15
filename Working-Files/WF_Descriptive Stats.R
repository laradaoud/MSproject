# R-Project
# 11/10/2016
# Felix
# Descriptive statistics

#import student-mat.csv as sm
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

########################################################
# Blank Theme for Pie Charts below

blank_theme <- theme_minimal()+ theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )
########################################################
# Sex
summary(sm[,'sex']) 
#F=208 #M=187

df_gender <- data.frame(Gender=c("Female", "Male"), value=c(208, 187))
pie_gender <- ggplot(data=df_sex, aes(x="", y=value, fill=Gender)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + 
  scale_fill_brewer() + blank_theme + 
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), label = (value)), size=4) 


###
# Address
summary(sm[,'address']) 
#Rural = 77 #Urban = 307

df_address <- data.frame(Address=c("Rural", "Urban"), value=c(77, 307))
pie_address <- ggplot(data=df_address, aes(x="", y=value, fill=Address)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + 
  scale_fill_brewer() + blank_theme + 
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), label = (value)), size=4) 


###
#Study Time

studyTime <- table(sm$studytime)
#  1   2   3   4 
# 105 198  65  27 

df_studyTime <- data.frame(Study_Time=c('<2 hours', "2 to 5 hours", "5 to 10 hours", ">10 hours"), value=c(105, 198, 65, 27))
#weekly study time (numeric: 1 - <2 hours, 2 - 2 to 5 hours, 3 - 5 to 10 hours, or 4 - >10 hours)

bar_studytime <- ggplot(data=df_studyTime, aes(x="", y=value, fill=Study_Time)) + 
  geom_bar(width = 1, stat = "identity") + 
  scale_fill_brewer() + blank_theme + 
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), label = (value)), size=4) 


###
# Failures
# number of past class failures (numeric: n if 1<=n<3, else 4)

failures <- table(sm$failures)
# 0   1   2   3 
# 312  50  17  16 
df_failures <- data.frame(Failures_Times=c("0", "1", "2", "3"), value=c(312, 50, 17, 16))

pie_failures <- ggplot(data=df_failures, aes(x="", y=value, fill=Failures_Times)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + 
  scale_fill_brewer() + blank_theme + 
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), label = (value)), size=4) 


###
# Schools
summary(sm[,'school']) 
#Gabriel Pereira = 349   #Mousinho da Silveira = 46

df_school <- data.frame(Schools=c("Gabriel Pereira", "Mousinho da Silveira"), value=c(349,46))
bar_school <- ggplot(data=df_school, aes(x="", y=value, fill=Schools)) + 
  geom_bar(width = 1, stat = "identity") + 
  blank_theme + 
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), label = (value)), size=4) 


###
# Father's Job
summary(sm[,'Fjob'])
ggplot(data=sm) + geom_bar(aes((sm[,'Fjob']))) + labs(x="Father's Job")
# at_home   health  other  services  teacher 
#   20       18      217      111       29 

df_fjob <- data.frame(Group = c("Health", "Work at Home", "Teacher", "Servies", "Other"), value = c(18, 20, 29, 111, 217))
pie_fjob <- ggplot(data = df_fjob, aes(x="", y=value, fill=Group)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + 
  scale_fill_brewer() + blank_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), label = (value)), size=4) +
  ggtitle("Father's Job")


###
# Mother's Job
summary(sm[,'Mjob'])
ggplot(data=sm) + geom_bar(aes((sm[,'Mjob']))) + labs(x="Mother's Job")
# at_home   health  other  services  teacher 
# 59       34        141      103       58

df_mjob <- data.frame(Group = c("Health", "Teacher", "Work at Home", "Services", "Other"), value = c(34, 58, 59, 103, 141))
pie_mjob <- ggplot(data = df_mjob, aes(x="", y=value, fill=Group)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + 
  scale_fill_brewer() + blank_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), label = (value)), size=4) +
  ggtitle("Mother's Job")


###
# Student's Guardian
summary(sm[,'guardian'])
ggplot(data=sm) + geom_bar(aes(guardian)) + labs(x="Student's Guardian")
# father mother  other 
# 90    273     32 

df_guardian <- data.frame(group = c("Father", "Mother", "Other"), value = c(90, 273, 32))
pie_guardian <- ggplot(data = df_guardian, aes(x="", y=value, fill=group)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + 
  scale_fill_brewer() + blank_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), label = (value)), size=4) +
  ggtitle("Guardian")


###############################################################################################
#
pie_gender
pie_address
bar_studytime
pie_failures

bar_school
pie_fjob
pie_mjob
pie_guardian

###############################################################################################

# Amount of Workday Alchohol Consumption
ggplot(data=sm) + geom_bar(aes(sm[,'Dalc'])) + 
  labs(x='Workday Alcohol Consumption', y='Frequency') + 
  ggtitle('Amount of Workday Alchohol Consumption') 

# Amount of Weekday Alchohol Consumption
ggplot(data=sm, aes(x=Dalc), colour='2') +
  geom_bar(position = 'dodge') +
  labs(x='Weekday Alcohol Consumption', y='Frequency') + 
  ggtitle('Amount of Alchohol Consumption')

#below - trying something

df_alcohol <- data.frame(DrinkingAmount = c("1", "2", "3", "4", "5"), Dalc = c(276,75,26,9,9), Walc = c(151,85,80,51,28))

ggplot(data=df_alcohol) + geom_density(aes(x=DrinkingAmount, y=Dalc))
ggplot(data=df_alcohol) + geom_density(aes(x=DrinkingAmount, y=Walc))

table(sm$Dalc)
# 1   2   3   4   5 
# 276  75  26   9   9 

table(sm$Walc)
# 1   2   3   4   5 
# 151  85  80  51  28 

# students don't really drink that much more on weekends compare to weekdays
# just 19 more for very high, and 42 more for high 
# compare to; 125 for very low

###############################################################################################
# MATHS SCORE G1, G2, G3

mean(sm$G1) # 10.91
mean(sm$G2) # 10.73
mean(sm$G3) # 10.42
avgG <- mean( (sm$G1+sm$G2+sm$G3) / 3) # 10.68

df_mat <- data.frame(group = c("G1", "G2", "G3", "G_Avg"), value = c(10.91, 10.73, 10.42, 10.68))

df_mat1 <- data.frame(G1=sm$G1, G2=sm$G2, G3=sm$G3)

qplot(factor(G1), data=df_mat1, geom="bar", fill=factor(G2))


  


