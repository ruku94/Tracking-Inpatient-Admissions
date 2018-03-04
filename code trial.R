library(readr)
TAMU_FINAL_SUBSET_IV_0_1_ <- read_csv("D:/Humana Analytics/TAMU_FINAL_SUBSET1/TAMU_FINAL_SUBSET (IV_0.1).csv")
View(TAMU_FINAL_SUBSET_IV_0_1_)
TAMU_FINAL_SUBSET_IV_0_1_$`2016_Readmit`<-NULL
TAMU_FINAL_SUBSET_IV_0_1_$ADMISSIONS<-NULL
TAMU_FINAL_SUBSET_IV_0_1_$READMISSIONS<-NULL
install.packages("rpart")
admission_iv_01 <- TAMU_FINAL_SUBSET_IV_0_1_
mytree <- rpart(admission_iv_01$`2016_Admit` ~ ., data = admission_iv_01, method = "class")
mytree
install.packages("rattle")
install.packages("rpart.plot")
install.packages("RColorBrewer")
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(mytree)

TAMU_FINAL_SUBSET_IV_002 <- read_csv("D:/Humana Analytics/TAMU_FINAL_SUBSET1/TAMU_FINAL_SUBSET IV_002.csv")
View(TAMU_FINAL_SUBSET_IV_002)
TAMU_FINAL_SUBSET_IV_002$`2016_Readmit`<-NULL
TAMU_FINAL_SUBSET_IV_002$ADMISSIONS<-NULL
TAMU_FINAL_SUBSET_IV_002$READMISSIONS<-NULL
mytree2 <- rpart(admission_iv_01$`2016_Admit` ~ ., data = TAMU_FINAL_SUBSET_IV_002, method = "class")
fancyRpartPlot(mytree2)


#variable clustering
library("ClustOfVar")
clustinput <-TAMU_FINAL_SUBSET_IV_002
head(clustinput)
clustinput$`2016_Admit`<-NULL

# function to subset variables
varlist <- function (df=NULL,type=c("numeric","factor","character"), pattern="", exclude=NULL) {
  vars <- character(0)
  if (any(type %in% "numeric")) {
    vars <- c(vars,names(df)[sapply(df,is.numeric)])
  }
  if (any(type %in% "factor")) {
    vars <- c(vars,names(df)[sapply(df,is.factor)])
  }  
  if (any(type %in% "character")) {
    vars <- c(vars,names(df)[sapply(df,is.character)])
  }  
  vars[(!vars %in% exclude) & grepl(vars,pattern=pattern)]
}

#filter for factor variables
varlist(clustinput,type="factor")
V<- varlist(clustinput,pattern = "^FLAG") 
V

as.factor(clustinput$ORIG_REAS_ENTITLE_CD)
as.factor(clustinput$DUAL)
as.factor(clustinput$LIS)
as.factor(clustinput$MINOR_GEOGRAPHY)
as.factor(clustinput$CDC_2014)
as.factor(clustinput$CDC_EYE_GAP_2014)
as.factor(clustinput$CDC_HBAGOOD_GAP_2014)
as.factor(clustinput$CDC_LDL100_GAP_2014)
as.factor(clustinput$COL_2015)
as.factor(clustinput$Decile_struggle_Med_lang)
as.factor(clustinput$Est_BMI_decile)


clustinput[V] <- lapply(clustinput[V], factor)
lapply(clustinput, class)
clustinput
summary(clustinput)
subset_numeric <- c(vars,names(clustinput)[sapply(clustinput,is.numeric)])
subset_numeric
clustinput1 <-clustinput[subset_numeric]
clustinput1


X.quanti <- clustinput1[,250:300]
tree <- hclustvar(X.quanti)




