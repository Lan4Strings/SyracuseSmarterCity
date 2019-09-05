crime_data<-read.csv("crime2017-2018.csv")
census_data<-read.csv("temp_census_data.csv")
library(e1071)
library(caret)
library(randomForest)


mergeddata<-merge(census_data,crime_data,by="CTN",all.y=T)
mergeddata<-mergeddata[mergeddata$CODE_DEFINED!="Property",]
mergeddata$CODE_DEFINED<-factor(mergeddata$CODE_DEFINED)
dataformodel<-mergeddata[,c(-1:-5,-13:-15,-19:-30,-32,-33,-34)]

sam<-sample.int(dim(dataformodel)[1],size=dim(dataformodel)[1]/10)
test<-dataformodel[sam,]
trainval<-dataformodel[-sam,]
sam<-sample.int(dim(trainval)[1],size=dim(trainval)[1]/3)
validation<-trainval[sam,]
train<-trainval[-sam,]

backuptrain<-train


#baseline
baseline_train<-max(table(train$CODE_DEFINED))/dim(train)[1]
baseline_validation<-max(table(validation$CODE_DEFINED))/dim(validation)[1]
baseline_test<-max(table(test$CODE_DEFINED))/dim(test)[1]
baseline_train
baseline_validation
baseline_test

train<-backuptrain
#train<-train[,c(-6,-9,-8,-7,-10,-12)]
train<-train[,c(-6,-8:-10,-12)]

maxacc=0
for(ntree in 60){
  rfm<-randomForest(CODE_DEFINED~.,data=train,ntree=ntree*50)
  pre<-predict(rfm,validation)
  cm_rf<-confusionMatrix(validation$CODE_DEFINED,pre)
  if(cm_rf$overall[1]>maxacc){
    bestntree=ntree
    maxacc=cm_rf$overall[1]
  }
}
ntree
maxacc-baseline_validation
rfm<-randomForest(CODE_DEFINED~.,data=train,ntree=bestntree*50)

rfm$importance
pre<-predict(rfm,test)
cm_rf<-confusionMatrix(test$CODE_DEFINED,pre)
cm_rf
baseline_test

#dsad

svmm<-svm(CODE_DEFINED~.,data=train)
pre<-predict(svmm,validation)
cm_svm<-confusionMatrix(validation$CODE_DEFINED,pre)
cm_svm$overall[1]-baseline_test

#pre<-predict(svmm,test)
#cm_svm<-confusionMatrix(test$CODE_DEFINED,pre)








