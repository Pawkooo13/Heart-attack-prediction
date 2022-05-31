install.packages("tree")
library(tree)

data=read.csv2("C:/Users/pstrz/Downloads/heart.csv",header=TRUE,sep=',')
print(data)

data = data[c(1:300),]

#sprawdzenie normalizacji danych
shapiro.test(data$age)
shapiro.test(data$sex)
shapiro.test(data$cp)
shapiro.test(data$trtbps)
shapiro.test(data$chol)
shapiro.test(data$fbs)
shapiro.test(data$restecg)
shapiro.test(data$thalachh)
shapiro.test(data$exng)
shapiro.test(data$oldpeak)
shapiro.test(data$slp)
shapiro.test(data$caa)
shapiro.test(data$thall)

#podzial na zbior uczacy i testowy

dane = data[c(1:75,151:225),]
test = data[c(76:150,226:300),]

print(sum(dane$output))
print(sum(test$output))

#model jaki zastosujemy to model regresji logistycznej

model = glm(
  dane$output ~ dane$age + dane$sex + dane$cp + dane$trtbps +
  dane$chol + dane$fbs + dane$restecg + dane$thalachh + dane$exng +
  dane$oldpeak + dane$slp + dane$caa + dane$thall, family = 'binomial'
)
summary(model)

#budowa modelu z najbardziej istotnych parametrow

model2=glm(dane$output ~ dane$sex + dane$cp + dane$chol +
          dane$restecg + dane$thall,
          family = 'binomial'
)
summary(model2)
pred=predict(model2,test,type="response")
pred
x=ifelse(pred > 0.5, 1, 0)
sum(x==test$output)/nrow(test) #skutecznosc
1-sum(x==test$output)/nrow(test) #blad

#proba ulepszenia modelu usuwajac najmniej znaczacy parametr

model3=glm(dane$output ~ dane$cp +
           dane$restecg + dane$thall,
           family = 'binomial')
summary(model3)
pred2=predict(model3,test,type="response")
pred2
y=ifelse(pred2 > 0.5, 1, 0)
sum(y==test$output)/nrow(test) 
1-sum(y==test$output)/nrow(test) 
#brak poprawy modelu,
#ostatecznie mamy 76% dokladnosci 


drzewo=tree(dane$output~.,data=dane)
drzewo
plot(drzewo)
text(drzewo)
pred_tree=predict(drzewo,test)
pred_tree
z=ifelse(pred_tree > 0.5, 1, 0)
sum(z==test$output)/nrow(test)
1-sum(z==test$output)/nrow(test) 
# 76% dokladnosci


