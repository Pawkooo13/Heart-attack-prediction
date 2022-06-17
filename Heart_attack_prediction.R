install.packages("tree")
install.packages("ggplot2")      
install.packages("GGally")
library(tree)
library("ggplot2")      
library("GGally")
library(class)

data=read.csv2("C:/Users/pstrz/Downloads/heart.csv",header=TRUE,sep=',')
print(data)

data = data[c(1:300),]
data = data[c(1,2,3,4,5,6,7,8,9,11,12,13,14)]
print(data)

#sprawdzenie normalizacji danych
shapiro.test(data$age)
qqnorm(data$age)

shapiro.test(data$sex)
qqnorm(data$sex)

shapiro.test(data$cp)
qqnorm(data$cp)

shapiro.test(data$trtbps)
qqnorm(data$trtbps)
qqnorm(log(data$trtbps))

shapiro.test(data$chol)
hist(data$chol)
qqnorm(data$chol)
qqnorm(log(data$chol)) #ok

data$chol = log(data$chol)

shapiro.test(data$fbs)
hist(data$fbs)
qqnorm(data$fbs)

shapiro.test(data$restecg)
hist(data$restecg)
qqnorm(data$restecg)

shapiro.test(data$thalachh)
hist(data$thalachh)
qqnorm(data$thalachh) 

shapiro.test(data$exng)
hist(data$exng)
qqnorm(data$exng)

shapiro.test(data$slp)
hist(data$slp)
qqnorm(data$slp)

shapiro.test(data$caa)
hist(data$caa)
qqnorm(data$caa)

shapiro.test(data$thall)
hist(data$thall)
qqnorm(data$thall)


ggpairs(data)


#podzial na zbior uczacy i testowy

dane = data[c(1:75,151:225),]
test = data[c(76:150,226:300),]

print(sum(dane$output))
print(sum(test$output))

#model jaki zastosujemy to model regresji logistycznej
model = glm(dane$output~.,data=dane,family='binomial')
summary(model)
pred=predict(model,test,type='response')
pred
x=ifelse(pred>0.5,1,0)
sum(x==test$output)/nrow(test) #skutecznosc
1-sum(x==test$output)/nrow(test) #blad

#budowa modelu z najbardziej istotnych parametrow

model2=glm(dane$output ~ dane$sex + dane$cp + dane$chol +
             dane$restecg + dane$thall + dane$slp + dane$caa,
           family = 'binomial'
)
summary(model2)
pred2=predict(model2,test,type="response")
pred2
y=ifelse(pred2 > 0.5, 1, 0)
sum(y==test$output)/nrow(test) #skutecznosc
1-sum(y==test$output)/nrow(test) #blad
#mamy 79% dokladnosci

#proba ulepszenia modelu usuwajac najmniej znaczacy parametr

model3=glm(dane$output ~ dane$cp + dane$sex + 
             dane$restecg + dane$thall + dane$slp + dane$caa,
           family = 'binomial')
summary(model3)
pred3=predict(model3,test,type="response")
pred3
z=ifelse(pred3 > 0.5, 1, 0)
sum(z==test$output)/nrow(test) 
1-sum(z==test$output)/nrow(test) 
#mamy 79% dokladnosci 

model4=glm(dane$output~dane$cp+dane$restecg+dane$thall+dane$slp+dane$caa,data=dane,family='binomial')
summary(model4)
pred4=predict(model4,test,type="response")
pred4
w=ifelse(pred4 > 0.5, 1, 0)
sum(w==test$output)/nrow(test) 
1-sum(w==test$output)/nrow(test) 
#brak poprawy modelu,
#ostatecznie mamy 79% dokladnosci 

data=read.csv2("C:/Users/pstrz/Downloads/heart.csv",header=TRUE,sep=',')
data = data[c(1:300),]
data = data[c(1,2,3,4,5,6,7,8,9,11,12,13,14)]
dane = data[c(1:75,151:225),]
test = data[c(76:150,226:300),]


# DRZEWO DECYZYJNE 
drzewo=tree(dane$output~.,data=dane)
drzewo
plot(drzewo)
text(drzewo)
pred_tree=predict(drzewo,test)
pred_tree
v=ifelse(pred_tree > 0.5, 1, 0)
sum(v==test$output)/nrow(test)
1-sum(v==test$output)/nrow(test) 
# 75% dokladnosci


# MODEL KNN
accuracy_list = c()
for (i in 1:150){
  modelknn=knn(dane,test,cl=dane$output,k=i)
  accuracy = sum(modelknn==test$output)/nrow(test) #skutecznosc
  accuracy_list = c(accuracy_list[],accuracy)
}
print(max(accuracy_list))
print(accuracy_list)

# MODEL KNN PO WYBRANIU ISTOTNYCH DANYCH
accuracy_list = c()
dane_knn = dane[c(3,7,10,11,12,13)]
test_knn = test[c(3,7,10,11,12,13)]
for (i in 1:150){
  modelknn=knn(dane_knn,test_knn,cl=dane_knn$output,k=i)
  accuracy = sum(modelknn==test_knn$output)/nrow(test_knn) #skutecznosc
  accuracy_list = c(accuracy_list[],accuracy)
}
print(max(accuracy_list))
print(accuracy_list)
# 91% dokladnosci


# MODEL KNN PO NORMALIZACJI DANYCH
dane_knn2 = dane
test_knn2 = test
for (i in 1:12){
  dane_knn2[i] = dane_knn2[i]/max(dane_knn2[i])
  test_knn2[i] = test_knn2[i]/max(test_knn2[i])
}
print(dane_knn2)
print(test_knn2)
accuracy_list2 = c()
for (i in 1:150){
  modelknn2=knn(dane_knn2,test_knn2,cl=dane_knn2$output,k=i)
  accuracy2 = sum(modelknn2==test_knn2$output)/nrow(test_knn2) #skutecznosc
  accuracy_list2 = c(accuracy_list2[],accuracy2)
}
print(max(accuracy_list2))
print(accuracy_list2)
# 100% dokladnosci
