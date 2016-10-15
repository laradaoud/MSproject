#Loading the data

sm <- read.csv("student-mat.csv", sep=';') 

#Data modification 
#As explain in the reports, we have changed the dataset in order to perform statistical analysis
sm <- student.mat
for (i in 16:24) {                                  
  sm[,i] <- as.character(sm[,i])
  for (j in 1:nrow(sm)) {
    if (sm[j,i] == "yes") sm[j,i] <- 1
    if (sm[j,i] == "no") sm[j,i] <- 0
  }
  sm[,i] <- as.numeric(sm[,i])
}


for (i in 6) {                                  
  sm[,i] <- as.character(sm[,i])
  for (j in 1:nrow(sm)) {
    if (sm[j,i] == "A") sm[j,i] <- 1
    if (sm[j,i] == "T") sm[j,i] <- 0
  }
  sm[,i] <- as.numeric(sm[,i])
}

for (i in 5) {                                  
  sm[,i] <- as.character(sm[,i])
  for (j in 1:nrow(sm)) {
    if (sm[j,i] == "GT3") sm[j,i] <- 1
    if (sm[j,i] == "LE3") sm[j,i] <- 0
  }
  sm[,i] <- as.numeric(sm[,i])
}


sm[,"Medu"] <- abs(sm[,"Medu"] - 6)
sm[,"Fedu"] <- abs(sm[,"Fedu"] - 6)
sm[,"famrel"] <- abs(sm[,"famrel"] - 6)
sm[,"famrel"] <- abs(sm[,"famrel"] - 6)

#---------------------------------------------------------------------

#General test 

t.test(sm[,"Walc"], sm[,"Dalc"])
library(ggplot2)
ggplot(data = sm) + geom_bar(aes(x=age))
ggplot(data = sm) + geom_bar(aes(x=sex))
ggplot(data = sm) + geom_bar(aes(x=Dalc))
ggplot(data = sm) + geom_bar(aes(x=Walc))
i <- ggplot(sm, aes(x = sm[,"age"], y = sm[,"sex"]))
i + geom_bin2d(binwidth = c(1,1)) #LABEL #Distribution of gender and age
i <- ggplot(sm, aes(x = sm[,"Dalc"], y = sm[,"Walc"]))
i + geom_bin2d(binwidth = c(1,1)) #LABEL #Distribution of Walc and Dalc

#Test and plot on gender and age

#age
f <- ggplot(data =sm, aes(x = sm[,"Dalc"], y = sm[,"age"]))
f + geom_smooth(method = lm) #LABEL
f <- ggplot(data =sm, aes(x = sm[,"Walc"], y = sm[,"age"]))
f + geom_density(method = lm)  #LABEL
f + geom_smooth(na.rm = TRUE)
i <- ggplot(sm, aes(x = sm[,"age"], y = sm[,"sex"]))
i + geom_bin2d(binwidth = c(1,1))
i <- ggplot(sm, aes(x = sm[,"absences"], y = sm[,"Walc"]))
i + geom_bin2d(binwidth = c(1,1))
cor.test(sm[,"age"],sm[,"Dalc"])
cor.test(sm[,"age"],sm[,"Walc"])

#gender

for (i in 2) {                                  
  sm[,i] <- as.character(sm[,i])
  for (j in 1:nrow(sm)) {
    if (sm[j,i] == "M") sm[j,i] <- 1
    if (sm[j,i] == "F") sm[j,i] <- 0
  }
  sm[,i] <- as.numeric(sm[,i])
}
cor.test(sm[,"sex"],sm[,"Walc"])
cor.test(sm[,"sex"],sm[,"Dalc"])


#Correlations tests 
#As inputs have to be numeric it requires less lines to split the code that way instead of doing block per parameters analysed (Failures, paid, Medu, ... )
#We could have done for loops here to calculate several correlations but it the results wasn't user friendly and doint it that way gave some useless correlations.

for (i in 32) {                                  
  sm[,i] <- as.character(sm[,i])
  for (j in 1:nrow(sm)) {
    if (sm[j,i] == "M") sm[j,i] <- 1
    if (sm[j,i] == "F") sm[j,i] <- 0
  }
  sm[,i] <- as.numeric(sm[,i])
}

sm <- as.numeric(sm)
sm1 <- subset(sm, sex == 1) #Create subsets with boys(1) or girls(0)
sm2 <- subset(sm, sex == 0)
#Romantic
#No correlation between being in relationships on alcohol consumption, nonetheless, we can see in the vizualisation plot that people in relationships tend to drink more during working daysp and single during week ends. This could be explained by assuming that people do not like to drink alone. Therefore, single people tend to drink when the go out, so particulary during week-end, while people in relationships will drink moderaly during dinners.   
cor.test(sm[,"romantic"], sm[,"Dalc"])
cor.test(sm[,"romantic"], sm[,"Walc"])
cor.test(sm[,"romantic"], sm[,"goout"])
#____Medu
cor.test(sm[,"Medu"], sm[,"Dalc"])
cor.test(sm[,"Medu"], sm[,"Walc"])
#____Fedu
cor.test(sm[,"Fedu"], sm[,"Dalc"])
cor.test(sm[,"Fedu"], sm[,"Walc"])
#___Travel Time
cor.test(sm[,"traveltime"], sm[,"Dalc"])
cor.test(sm[,"traveltime"], sm[,"Walc"])
#___famrel
cor.test(sm[,"famrel"], sm[,"Dalc"])
cor.test(sm[,"famrel"], sm[,"Walc"])
#___goout
cor.test(sm[,"goout"], sm[,"Dalc"])
cor.test(sm[,"goout"], sm[,"Walc"])
#___famsize
cor.test(sm[,"famsize"], sm[,"Dalc"])
cor.test(sm[,"famsize"], sm[,"Walc"])
#___internet
cor.test(sm[,"internet"], sm[,"Dalc"])
cor.test(sm[,"internet"], sm[,"Walc"])
#famrel 
cor.test(sm[,"famrel"], sm[,"Dalc"])
cor.test(sm[,"famrel"], sm[,"Walc"])
#___Grades
cor.test(sm[,"G1"], sm[,"Dalc"])
cor.test(sm[,"G1"], sm[,"Walc"])
cor.test(sm[,"G2"], sm[,"Dalc"])
cor.test(sm[,"G2"], sm[,"Walc"])
cor.test(sm[,"G3"], sm[,"Dalc"])
cor.test(sm[,"G3"], sm[,"Walc"])
cor.test(sm[,"G1"], sm[,"Dalc"])
cor.test(sm[,"G1"], sm[,"Walc"])
cor.test(sm[,"G2"], sm[,"Dalc"])
cor.test(sm[,"G3"], sm[,"goout"])
cor.test(sm[,"G3"], sm[,"failures"])
cor.test(sm[,"G3"], sm[,"absences"])
#EARLY SIGNS OF ACADEMIC FAILURES
#___abscences
##There is a postive correlation between the number of absenses and the alcohol consumption. This is particulary the case for high abscences frequency is correlated to high alcohol consumption. 
cor.test(sm[,"absences"], sm[,"Dalc"])
cor.test(sm[,"absences"], sm[,"Walc"])
cor.test(sm1[,"absences"], sm1[,"Dalc"])
cor.test(sm1[,"absences"], sm1[,"Walc"])
cor.test(sm2[,"absences"], sm2[,"Dalc"])
cor.test(sm2[,"absences"], sm2[,"Walc"])
#____Failures There is a hig
cor.test(sm[,"failures"], sm[,"Dalc"])
cor.test(sm[,"failures"], sm[,"Walc"])
cor.test(sm1[,"failures"], sm1[,"Dalc"])
cor.test(sm1[,"failures"], sm1[,"Walc"])
cor.test(sm2[,"failures"], sm2[,"Dalc"])
cor.test(sm2[,"failures"], sm2[,"Walc"])
#___goout
cor.test(sm[,"goout"], sm[,"Dalc"])
cor.test(sm[,"goout"], sm[,"Walc"])
cor.test(sm1[,"goout"], sm1[,"Dalc"])
cor.test(sm1[,"goout"], sm1[,"Walc"])
cor.test(sm2[,"goout"], sm2[,"Dalc"])
cor.test(sm2[,"goout"], sm2[,"Walc"])


sm <- student.mat

for (i in 1:32) {                                  
  sm[,i] <- as.factor(sm[,i])
}

layout(matrix(1:14, ncol = 7))
cdplot(sm[,"guardian"] ~ sm[,"Dalc"], data = sm, xlab = "Alcohol consumption during working days ", ylab = "Guardian", border = 1)
cdplot(sm[,"guardian"] ~ sm[,"Walc"], data = sm, xlab = "Alcohol consumption during Week-end ", ylab = "Guardian", border = 1)
cdplot(sm[,"Medu"] ~ sm[,"Dalc"], data = sm, xlab = "Alcohol consumption during working days ", ylab = "Father's education", border = 1)
cdplot(sm[,"Medu"] ~ sm[,"Walc"], data = sm, xlab = "Alcohol consumption during Week-end ", ylab = "Father's education", border = 1)
cdplot(sm[,"Fedu"] ~ sm[,"Dalc"], data = sm, xlab = "Alcohol consumption during working days ", ylab = "Mother's education", border = 1)
cdplot(sm[,"Fedu"] ~ sm[,"Walc"], data = sm, xlab = "Alcohol consumption during working Week-end ", ylab = "Mother's education", border = 1)
cdplot(sm[,"Pstatus"] ~ sm[,"Dalc"], data = sm, xlab = "Alcohol consumption during working days ")
cdplot(sm[,"Pstatus"] ~ sm[,"Walc"], data = sm, xlab = "Alcohol consumption during Week-end ")
cdplot(sm[,"famsize"] ~ sm[,"Dalc"], data = sm, xlab = "Alcohol consumption during working days ")
cdplot(sm[,"famsize"] ~ sm[,"Walc"], data = sm, xlab = "Alcohol consumption during working Week-end ")
cdplot(sm[,"Mjob"] ~ sm[,"Dalc"], data = sm, xlab = "Alcohol consumption during working days")
cdplot(sm[,"Mjob"] ~ sm[,"Walc"], data = sm, xlab = "Alcohol consumption during working Week-end")
cdplot(sm[,"famrel"] ~ sm[,"Dalc"], data = sm, xlab = "Alcohol consumption during working days")
cdplot(sm[,"famrel"] ~ sm[,"Walc"], data = sm, xlab = "Alcohol consumption during working Week-end ")

layout(matrix(1:10, ncol = 5))
cdplot(sm[,"romantic"] ~ sm[,"Dalc"], data = sm)
cdplot(sm[,"romantic"] ~ sm[,"Walc"], data = sm)
cdplot(sm[,"goout"] ~ sm[,"Dalc"], data = sm)
cdplot(sm[,"goout"] ~ sm[,"Walc"], data = sm)
cdplot(sm[,"internet"] ~ sm[,"Dalc"], data = sm)
cdplot(sm[,"internet"] ~ sm[,"Walc"], data = sm)
cdplot(sm[,"activities"] ~ sm[,"Dalc"], data = sm)
cdplot(sm[,"activities"] ~ sm[,"Walc"], data = sm)
cdplot(sm[,"freetime"] ~ sm[,"Dalc"], data = sm)
cdplot(sm[,"freetime"] ~ sm[,"Walc"], data = sm)

layout(matrix(1:10, ncol = 5))
cdplot(sm[,"failures"] ~ sm[,"Dalc"], data = sm)
cdplot(sm[,"failures"] ~ sm[,"Walc"], data = sm)
cdplot(sm[,"schoolsup"] ~ sm[,"Dalc"], data = sm)
cdplot(sm[,"schoolsup"] ~ sm[,"Walc"], data = sm)
#cdplot(sm[,"absences"] ~ sm[,"Dalc"], data = sm)
#cdplot(sm[,"absences"] ~ sm[,"Walc"], data = sm) Make no sens
cdplot(sm[,"studytime"] ~ sm[,"Dalc"], data = sm)
cdplot(sm[,"studytime"] ~ sm[,"Walc"], data = sm)
cdplot(sm[,"G3"] ~ sm[,"Dalc"], data = sm)
cdplot(sm[,"G3"] ~ sm[,"Walc"], data = sm)


#Some regression

RegFails <- lm (Walc ~ failures, data = sm)
summary (lm (Walc ~ failures, data = sm))
libraryr(ggplot2)
f <- ggplot(data =sm, aes(x = sm[,"Walc"], y = sm[,"failures"]))
f + geom_smooth(method = lm)
libraryr(ggplot2)
f <- ggplot(data =sm, aes(x = sm[,"Dalc"], y = sm[,"failures"]))
f + geom_smooth(method = lm)

sm1 <- subset(sm, Walc < 3 & Dalc <3)
sm2 <- subset(sm, Walc > 2 & Dalc > 2)

layout(matrix(1:2, ncol = 2))
cdplot(sm[,"failures"] ~ sm[,"Dalc"], data = sm)
cdplot(sm[,"failures"] ~ sm[,"Walc"], data = sm)

#Age

f <- ggplot(data =sm, aes(x = sm[,"Dalc"], y = sm[,"age"]))
f + geom_smooth(method = lm)
f <- ggplot(data =sm, aes(x = sm[,"Walc"], y = sm[,"age"]))
f + geom_smooth(method = lm)
f + geom_density(na.rm = TRUE)
i <- ggplot(sm, aes(x = sm[,"Fedu"], y = sm[,"Dalc"]))
i + geom_bin2d(binwidth = c(1,1))
cor.test(sm[,"age"],sm[,"Dalc"])
cor.test(sm[,"age"],sm[,"Walc"])


#Correlation test - famsize 


model <- glm(famsize ~ Dalc + Walc, family=binomial(link = 'logit'), data=sm)
summary(model)
i <- ggplot(sm, aes(x = sm[,"famsize"], y = sm[,"Walc"]))
i + geom_line()
sm[,"famsize"] <- as.factor(sm[,"famsize"])
layout(matrix(1:2, ncol = 2))
cdplot(sm[,"famsize"] ~ sm[,"Dalc"], data = sm)
cdplot(sm[,"famsize"] ~ sm[,"Walc"], data = sm)

cor.test(sm[,"age"],sm[,"Dalc"])



layout(matrix(1:2, ncol = 2))
sm[,"Medu"] <- as.numeric(sm[,"Medu"])
sm[,"Pedu"] <- as.numeric(sm[,"Pedu"])
plot(density(sm[,"Medu"] ), xlim = c(0, 5), main = "Density of Mother education with respect to arbitrary value", xlab = "Level of Mather's education")
plot(density(sm[,"Pedu"] ), xlim = c(0, 5), main = "Density of Father education with respect to arbitrary value", xlab = "Level of Father's education")

sm[,"Medu"] <- abs(sm[,"Medu"] - 6)
sm[,"Pedu"] <- abs(sm[,"Pedu"] - 6)
sm[,"Medu"] <- as.numeric(sm[,"Medu"])
sm[,"Pedu"] <- as.numeric(sm[,"Pedu"])
cor.test(sm[,"Medu"], sm[,"Dalc"])
cor.test(sm[,"Medu"], sm[,"Walc"])
cor.test(sm[,"Pedu"], sm[,"Dalc"])
cor.test(sm[,"Pedu"], sm[,"Walc"])


#Relationships with the family 

sm[,"famrel"] <- abs(sm[,"famrel"] - 6)
sm[,"famrel"] <- abs(sm[,"famrel"] - 6)
sm[,"famrel"] <- as.numeric(sm[,"famrel"])
sm[,"famrel"] <- as.numeric(sm[,"famrel"])
sm[,"Walc"] <- as.numeric(sm[,"Walc"])
sm[,"Dalc"] <- as.numeric(sm[,"Dalc"])
cor.test(sm[,"famrel"], sm[,"Dalc"])
cor.test(sm[,"famrel"], sm[,"Walc"])
cor.test(sm[,"famrel"], sm[,"Dalc"])
cor.test(sm[,"famrel"], sm[,"Walc"])

#Travel Time

cor.test(sm[,"traveltime"], sm[,"Dalc"])
cor.test(sm[,"traveltime"], sm[,"Walc"])

#GoOUT

sm[,"goout"] <- as.numeric(sm[,"goout"])
sm[,"Dalc"] <- as.numeric(sm[,"Dalc"])
sm[,"Walc"] <- as.numeric(sm[,"Walc"])
cor.test(sm[,"goout"], sm[,"Dalc"])
cor.test(sm[,"goout"], sm[,"Walc"]) #Stong correlation
RegGoout <- lm (goout ~ Walc, data = sm)
RegGoout
libraryr(ggplot2)
f <- ggplot(data =sm, aes(x = sm[,"goout"], y = sm[,"Walc"]))
f + geom_smooth(method = lm) #TBS
sm[,"goout"] <- as.factor(sm[,"goout"])
sm[,"Dalc"] <- as.factor(sm[,"Dalc"])
sm[,"Walc"] <- as.factor(sm[,"Walc"])
layout(matrix(1:2, ncol = 2))
cdplot(sm[,"goout"] ~ sm[,"Dalc"], data = sm)
cdplot(sm[,"goout"] ~ sm[,"Walc"], data = sm)

#Internet

sm[,"internet"] <- as.numeric(sm[,"internet"])
sm[,"Dalc"] <- as.numeric(sm[,"Dalc"])
sm[,"Walc"] <- as.numeric(sm[,"Walc"])
cor.test(sm[,"internet"], sm[,"Dalc"])
cor.test(sm[,"internet"], sm[,"Walc"])

#Activities

sm[,"activities"] <- as.numeric(sm[,"activities"])
sm[,"Dalc"] <- as.numeric(sm[,"Dalc"])
sm[,"Walc"] <- as.numeric(sm[,"Walc"])
cor.test(sm[,"activities"], sm[,"Dalc"])
cor.test(sm[,"activities"], sm[,"Walc"])

#Freetime

sm[,"freetime"] <- as.numeric(sm[,"freetime"])
sm[,"Dalc"] <- as.numeric(sm[,"Dalc"])
sm[,"Walc"] <- as.numeric(sm[,"Walc"])
cor.test(sm[,"freetime"], sm[,"Dalc"])
cor.test(sm[,"freetime"], sm[,"Walc"])



RegAbs <- lm (absences ~ Walc, data = sm)
RegAbs
library(ggplot2)
f <- ggplot(data =sm, aes(x = Walc, y = absences))
f + geom_smooth(method = lm) #TBS
sm[,"absences"] <- as.factor(sm[,"absences"])
sm[,"Dalc"] <- as.factor(sm[,"Dalc"])
sm[,"Walc"] <- as.factor(sm[,"Walc"])
layout(matrix(1:2, ncol = 2))
cdplot(sm[,"absences"] ~ sm[,"Dalc"], data = sm)
cdplot(sm[,"absences"] ~ sm[,"Walc"], data = sm)



#Mean tests

mean(sm[sm[, "Fedu"] == "5", "Dalc"]) - mean(sm[sm[, "Medu"] == "5", "Dalc"])
mean(sm[sm[, "guardian"] == "mother", "Dalc"]) - mean(sm[sm[, "guardian"] == "father", "Dalc"])

#Other measues

g <- ggplot(sm, aes(x = sm[,"Dalc"], y = sm[,"Fedu"]))
g + geom_violin(scale = "area")
g <- ggplot(sm, aes(x = sm[,"Dalc"], y = sm[,"age"]))
g + geom_violin(scale = "area")

library(ggplot2)
f <- ggplot(data =sm, aes(x = sm[,"goout"], y = sm[,"Walc"]))
f + geom_smooth(method = lm)








