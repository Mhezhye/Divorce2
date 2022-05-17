# import dataset
setwd("C:/Users/Mhezhye/Desktop/UoS/ASDM/COURSEWORK/Task 1")
Divorce<-read.csv("divorce.csv", header=TRUE)

#Inspect data
names(Divorce)
head(Divorce)
tail(Divorce)
summary(Divorce)
str(Divorce)

#check dimensions and num of points
nrow(Divorce)
ncol(Divorce)
dim(Divorce)

#Convert Class to factor since we need factor variable for prediction
Divorce$ClassF<-as.factor(Divorce$Class)

#check if changes were effected
str(Divorce)

#set seed to reproduce result from same sample consistently
set.seed(1234)

#divide sample into 70% training and 30% validation for implementing tree
CD<-sample(2, nrow(Divorce), replace=TRUE, prob=c(0.7,0.3))
CD

Train<-Divorce[CD==1,]
Validate<- Divorce[CD==2,]

#check dimension of training and validation data
dim(Train)
dim(Validate)

#install package, ignore if you have it
install.packages("party")
library(party)

#Train tree using ctree() function inside party package
Divorce_Tree<-ctree(ClassF~Atr1+Atr2+Atr3+Atr4+Atr5+Atr6+Atr7+Atr8+Atr9+Atr10+Atr11+Atr12
                    +Atr13+Atr14+Atr15+Atr16+Atr17+Atr18+Atr19+Atr20+Atr21+Atr22+Atr23+Atr24
                    +Atr25+Atr26+Atr27+Atr28+Atr29+Atr30+Atr31+Atr32+Atr33+Atr34+Atr35+Atr36
                    +Atr37+Atr38+Atr39+Atr40+Atr41+Atr42+Atr43+Atr44+Atr44+Atr45+Atr46+Atr47
                    +Atr48+Atr49+Atr50+Atr51+Atr52+Atr53+Atr54, data=Train)
#Draw Tree
Divorce_Tree
print(Divorce_Tree)

#plot results
plot(Divorce_Tree)
plot(Divorce_Tree, type="simple")

#make predictions
predict(Divorce_Tree)
Variables<-table(predict(Divorce_Tree), Train$ClassF)
print(Variables)

#calculate classification accuracy and error on train data itself
sum(diag(Variables))/sum(Variables)
1-sum(diag(Variables))/sum(Variables)

Test_pre_data<- table(predict(Divorce_Tree, newdata=Validate), Validate$ClassF)
print(Test_pre_data)

#calculate classification accuracy and error on test data itself
sum(diag(Test_pre_data))/sum(Test_pre_data)
1-sum(diag(Test_pre_data))/sum(Test_pre_data)

