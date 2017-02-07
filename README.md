# analyzethis
#reading the files
train_set<-read.csv("Training_Dataset.csv")
test_set<-read.csv("Final_Dataset.csv")
#converting dollar to numeric in both train set
y <-sub('\\$','',as.character(train_set$mvar_2))
train_set$mvar_2<-as.numeric(sub('\\,','',as.character(y)))
y <-sub('\\$','',as.character(train_set$mvar_3))
train_set$mvar_3<-as.numeric(sub('\\,','',as.character(y)))
y <-sub('\\$','',as.character(train_set$mvar_4))
train_set$mvar_4<-as.numeric(sub('\\,','',as.character(y)))
y <-sub('\\$','',as.character(train_set$mvar_5))
train_set$mvar_5<-as.numeric(sub('\\,','',as.character(y)))
y <-sub('\\$','',as.character(train_set$mvar_6))
train_set$mvar_6<-as.numeric(sub('\\,','',as.character(y)))
#converting dollar to numeric in both test set
y <-sub('\\$','',as.character(test_set$mvar_2))
test_set$mvar_2<-as.numeric(sub('\\,','',as.character(y)))
y <-sub('\\$','',as.character(test_set$mvar_3))
test_set$mvar_3<-as.numeric(sub('\\,','',as.character(y)))
y <-sub('\\$','',as.character(test_set$mvar_4))
test_set$mvar_4<-as.numeric(sub('\\,','',as.character(y)))
y <-sub('\\$','',as.character(test_set$mvar_5))
test_set$mvar_5<-as.numeric(sub('\\,','',as.character(y)))
y <-sub('\\$','',as.character(test_set$mvar_6))
test_set$mvar_6<-as.numeric(sub('\\,','',as.character(y)))
#getting rid of NA values
train_set[is.na(train_set)]<-0
test_set[is.na(test_set)]<-0
#using the gbm library
library(gbm)

#setting seed to replicate the output
set.seed(1)
gbmFit<-gbm(actual_vote~mvar_1+mvar_2+mvar_3+mvar_4+mvar_5+mvar_6+mvar_23+mvar_24+mvar_25+mvar_26+mvar_27+mvar_28+mvar_21+mvar_15+mvar_19+mvar_7+mvar_8+mvar_9+mvar_10+mvar_11,
                    data = train_set,
                    n.trees=100,
                    distribution = "multinomial",
                    shrinkage = .1,
                    interaction.depth =5,
                    n.minobsinnode = 21,
                    bag.fraction = .75,
                    train.fraction = 1.0,
                    keep.data = TRUE,
                    verbose = "CV",
                    n.cores =1,
                    var.monotone = NULL
                    )
                    
#predicting the output
my_prediction<-predict(gbmFit,newdata =test_set,n.trees=100,type="response")
my_prediction<-data.frame(my_prediction)

#constructing the output
output<-matrix(0,nrow=nrow(test_set),ncol=1)
#excluding ODYSSEY from final output
my_prediction$ODYSSEY.100<-NULL

#finding maximum probability to predict the output
for(i in 1:nrow(test_set)){
  output[i,1]<-which.max(my_prediction[i,])
}
for(i in 1:nrow(test_set)){
  if(output[i,1]==1)output[i,1]<-"CENTAUR"
  if(output[i,1]==2)output[i,1]<-"COSMOS"
  if(output[i,1]==3)output[i,1]<-"EBONY"
  if(output[i,1]==4)output[i,1]<-"TOKUGAWA"
}
#getting the final output in the required format
output<-data.frame(test_set$Citizen.ID,output)
write.table(output,file="solution_final.csv",sep=",",row.names = FALSE,col.names = F)
