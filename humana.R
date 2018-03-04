setwd('C:/Users/Arthi/Desktop/challenge/TAMU_FINAL_SUBSET V0.3')
data<-read.csv("TAMU_FINAL_SUBSET V0.3.csv")
data_1<-read.csv("TAMU_FINAL_SUBSET_1st.csv")
library(devtools)
# install_github("riv","tomasgreif")
# install.packages("woe")
# install.packages("rpart")
# library(woe)
# library(rpart)
install.packages("Information")
library(Information)
# info_val<-iv.mult(data,"gb",TRUE)
IV <- Information::create_infotables(data=data, y="X2016_Readmit", parallel = F)
# [1] "Variable POT_VISIT_65_Q01 was removed because it has only 1 unique level"
# [1] "Variable POT_VISIT_65_Q02 was removed because it has only 1 unique level"
# [1] "Variable POT_VISIT_65_Q03 was removed because it has only 1 unique level"
# [1] "Variable POT_VISIT_19_Q05 was removed because it has only 1 unique level"
# [1] "Variable POT_VISIT_09_Q01 was removed because it has only 1 unique level"
# [1] "Variable POT_VISIT_57_Q04 was removed because it has only 1 unique level"
# [1] "Variable POT_VISIT_57_Q07 was removed because it has only 1 unique level"
# [1] "Variable POT_VISIT_25_Q01 was removed because it has only 1 unique level"
# [1] "Variable POT_VISIT_09_Q06 was removed because it has only 1 unique level"
# [1] "Variable POT_VISIT_09_Q07 was removed because it has only 1 unique level"
# [1] "Variable POT_VISIT_55_Q07 was removed because it has only 1 unique level"
# [1] "Variable POT_VISIT_09_Q04 was removed because it has only 1 unique level"
# [1] "Variable POT_VISIT_17_Q06 was removed because it has only 1 unique level"
# [1] "Variable POT_VISIT_55_Q02 was removed because it has only 1 unique level"
IV_admit <- Information::create_infotables(data=data, y="X2016_Admit", parallel = F)
print(head(IV$Summary), row.names=FALSE)
print(IV$Tables$N_OPEN_REV_ACTS, row.names=FALSE)
# Plotting a single variable
Information::plot_infotables(IV, "N_OPEN_REV_ACTS")
# Plotting multiple variables
Information::plot_infotables(IV, IV$Summary$Variable[1:4], same_scale=TRUE)
is.binary("data$X2016_Admit")
subset<-data$X2016_Admit==8695
data<- subset(data,!(subset))
dataframe<-as.data.frame(IV)
IV_data<-as.data.frame(IV$Summary)
write.csv(IV_data,"Iv_data.csv")

head(data)
dim(data)
summary(data)

str(data)
colSums(is.na(data)) 
# lasso regression 

install.packages("glmnet")
library(glmnet)
#convert training data to matrix format
x <- model.matrix(data_1$X2016_Admit~.,data)
y<- data_$X2016_Admit
#perform grid search to find optimal value of lambda
#family= binomial => logistic regression, alpha=1 => lasso
# check docs to explore other type.measure options
grid=10^seq(10,-2, length =100)
cv.out <- cv.glmnet(x,y,alpha=2,family='binomial')

#plot result
plot(cv.out)

dim(coef(cv.out))
coef(cv.out)[,50]
cv.out$lambda.1se
#min value of lambda
lambda_min <- cv.out$lambda.min
#best value of lambda
lambda_1se <- cv.out$lambda.1se
#regression coefficients
coef(cv.out,s=lambda_1se)
predict (cv.out ,s=0.00674,type="coefficients")

for (i in data) {
  if (is.binary(data$i)) {
    data$i<-as.factor(data$i)
  }
}

for(i in data_1){
  y<-data$paste(y)
  print(is.binary(y))
  if (is.binary(data_1$(paste(i)))) {
    print("in if")
  data_1$i<-as.factor(data_1$i)
}
}
is.binary(data["X2016_Admit"])

for(i in data){
  dat <- dat[order(dat[,p]),]
}

for(col in data_1)
{as.factor(col)}

y="X2016_Readmit"
if (is.binary(data_1$X2016_Readmit)) {
  data_1$X2016_Readmit<-as.factor(data_1$X2016_Readmit)
}
y="X2016_Readmit"
y<-as.name(y)
y
data$y
if (is.binary(data_1$as.character(y))) {
  data_1[y]<-as.factor(data_1[y])
}
as.name("X2016_Admit")
is.binary(data_1$y)
colnames<-names(data_admit)
  
  sapply( df, function(x) if("factor" %in% class(x) ) { 
    max(as.numeric(as.character(x)))
  } else { max(x) } )
which(colnames(df)=="B")
IV$Tables

#variable clustering
trans<-t(data_num)
as.data.frame(trans)
utilities.df.norm <- sapply(trans, scale)
hc.complete =hclust(dist(x), method="complete ")
hclust()
hc.complete =hclust(dist(trans), method="single")
plot(hc.complete, hang = -1, cex=0.8, main="single linkage clustering")
clusters<- cutree(hc.complete,k= 10)
plot(clusters)
table(clusters)
df<-aggregate(trans,by=list(cluster=clusters),median)
f_data<-t(df)
f_data<-as.data.frame(f_data)
f_data<-f_data[-1,]
f_data$admit<-data_1$X2016_Admit
mytree <- rpart(f_data$admit~ ., data =f_data , method = "class")
mytree
install.packages("rattle")
install.packages("rpart.plot")
install.packages("RColorBrewer")
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(mytree)
train = sample (1:nrow(), nrow(Boston)/2) 

#Data based on IV
data_IV<-read.csv("TAMU_FINAL_SUBSET IV_002.csv")
data_admit<-data_IV[,-(2:4)]
library (MASS)
set.seed(1)
train_full = sample (1:nrow(data_admit), nrow(data_admit)*0.8) 
train = sample (1:nrow(f_data), nrow(f_data)*0.8) 

#Random forest
install.packages("randomForest")
library(randomForest)
set.seed(1) 

rf.boston=randomForest(f_data$admit~.,data=f_data,subset=train, mtry=3,importance=TRUE, type= "class")
yhat.rf_n = predict(rf.boston ,newdata=f_data[-train,])
yhat.rf<-as.numeric(yhat.rf)
f_data$admit<-as.integer(f_data$admit)
boston.test=f_data[-train ,"admit"]
mean((yhat.rf-boston.test)^2)
importance (rf.boston) 
varImpPlot (rf.boston)
plot(rf.boston)
plot(getTree(rf.boston))
#non - clustered data random forest
rf.full=randomForest(data_admit$X2016_Admit~.,data=data_admit,subset=train_full, mtry=17,importance=TRUE, type= "class")
#subset the variables --- remove 2016 rx variables
list<-grep("YR2016",names(data_admit))

for(i in names(data_IV)){
  if(grepl("YR2016",i))
  {
    data_IV[,i]<-NULL
  }
}
#now perform decision trees
library(rpart)
mytree <- rpart(data_admit$X2016_Admit~ ., data =data_admit , method = "class")



