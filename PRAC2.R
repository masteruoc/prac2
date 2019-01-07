### Import arxius i variables
rm(list=ls(all=TRUE))
library(dplyr)
library(titanic)
data("titanic_train")


### Valors buits i outlierts
colSums(is.na(titanic_train))  # falta edat 
colSums(titanic_train=='') # columna cabina te masses valors buits


titanic_train$Age[is.na(titanic_train$Age)] <- median(titanic_train$Age,na.rm=T) # apliquem la mediana perque els valors
                                                                                  #no es vegin afectats per als extrems
titanic_data <- titanic_train %>% select(-c(Cabin, PassengerId, Ticket, Name)) # Traiem Cabin (masses valors buits)
                                                                               # passengerId, Ticket i Name no son necesaries
apply(titanic_train,2, function(x) length(unique(x)))
cols<-c("Survived","Pclass","Sex","Embarked") # podem moure aquestes dades a factors
for (i in cols){
  titanic_train[,i] <- as.factor(titanic_train[,i])}

hist(titanic_data$Age, xlab = 'Edad', main = "Histogrma d'edat") # mirem outliers
hist(titanic_data$Fare, xlab = 'Tarifa', main = "Histogrma de la Tarifa") # mirem outliers
titanic_data$Fare[which(titanic_data$Fare > 300)] <- 300
hist(titanic_data$Fare, xlab = 'Tarifa', main = "Histogrma de la Tarifa") # mirem outliers

# Analisi de dades 

shapiro.test(titanic_data$Age)
shapiro.test(titanic_data$Fare) # no son variables normals


chisq.test(titanic_data$Survived, titanic_data$Sex)
chisq.test(table(titanic_data$Survived,titanic_data$Pclass))

fit <- glm(Survived ~ Age + Pclass + Sex + SibSp + Parch + Fare + Embarked,
           data = titanic_data, family = binomial(link = 'logit'))

summary(fit)
