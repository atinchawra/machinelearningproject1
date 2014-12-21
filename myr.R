library(caret)
set.seed(300)
setwd("D:/work/coursera/machinelearningproject1")
tr<-read.csv("pml-training.csv")

library(doParallel)
registerDoParallel(cores=detectCores(all.tests=TRUE))
                   
#sapply(names(tr),function(s) {print(class(tr[,s])) } )
#tt<-sapply(names(tr),function(s) if(grepl("belt",s)) {print(s) } )
#dim(subset(tr, new_window=="yes"))

tr<-subset(tr,new_window=="no")
tr<-subset(tr,subset=T,select=c(roll_belt,pitch_belt,yaw_belt,total_accel_belt,gyros_belt_x,gyros_belt_y,gyros_belt_z,accel_belt_x,accel_belt_y,accel_belt_z,magnet_belt_x,magnet_belt_y,magnet_belt_z,total_accel_arm,accel_arm_x,accel_arm_y,accel_arm_z,magnet_arm_x,magnet_arm_y,magnet_arm_z,roll_dumbbell,pitch_dumbbell,yaw_dumbbell,total_accel_dumbbell,gyros_dumbbell_x,gyros_dumbbell_y,gyros_dumbbell_z,accel_dumbbell_x,accel_dumbbell_y,accel_dumbbell_z,magnet_dumbbell_x,magnet_dumbbell_y,magnet_dumbbell_z,pitch_forearm,yaw_forearm,roll_forearm,gyros_forearm_x,gyros_forearm_y,gyros_forearm_z,classe))

#check if NA are present
#sapply(names(tr),function(c){any(is.na(tr[,c]))})

inTr<-createDataPartition(y=tr$classe,p = .3,list=F)

tr2<-tr[inTr,]

#predictors
pred<-tr2[,-dim(tr2)[2]]
#outcomes
out<-tr2[,dim(tr2)[2]]

#library(pROC)
#RocImp <- filterVarImp(x = pred, y = out)
#head(RocImp)

#are any predictors varying very little to be insignificant
nzv<-nearZeroVar(pred)
nzv

#detect correlated predictors, particularly highly correlated ones
#psCor<-cor(ps)
#highlyCor<-findCorrelation(psCor,cutoff = .75)
#ps2<-ps[,-highlyCor]#skip highly ones
#dim(ps2)

#pp<-preProcess(ps,method="pca")
#psPC<-predict(pp,ps)


fitControl<-trainControl(method="cv", number=10)
rfFit<-train(x=pred,y=as.factor(out),method="rf",trControl=fitControl,prox=T,allowParallel=T)
rfFit
rfFit$finalModel
plot(varImp(rfFit))



ts2<-tr[-inTr,]
#predictors
pred2<-ts2[,-dim(ts2)[2]]
8#psts3<-predict(pp,psts3)
#outcomes
out2<-ts2[,dim(ts2)[2]]
#length(osts)  
res<-predict(rfFit,pred2)
cm<-confusionMatrix(res,out2)


test<-read.csv("pml-testing.csv")
test<-subset(test,new_window=="no")
test<-subset(test,subset=T,select=c(roll_belt,pitch_belt,yaw_belt,total_accel_belt,gyros_belt_x,gyros_belt_y,gyros_belt_z,accel_belt_x,accel_belt_y,accel_belt_z,magnet_belt_x,magnet_belt_y,magnet_belt_z,total_accel_arm,accel_arm_x,accel_arm_y,accel_arm_z,magnet_arm_x,magnet_arm_y,magnet_arm_z,roll_dumbbell,pitch_dumbbell,yaw_dumbbell,total_accel_dumbbell,gyros_dumbbell_x,gyros_dumbbell_y,gyros_dumbbell_z,accel_dumbbell_x,accel_dumbbell_y,accel_dumbbell_z,magnet_dumbbell_x,magnet_dumbbell_y,magnet_dumbbell_z,pitch_forearm,yaw_forearm,roll_forearm,gyros_forearm_x,gyros_forearm_y,gyros_forearm_z))
#test<-predict(pp,test)
pred2<-predict(rfFit,newdata=test)
pred2

