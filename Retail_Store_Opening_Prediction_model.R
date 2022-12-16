##Om Namah Shivay
##Om Sai Ram
####Om Ganapati Bappa Moriya
#############################################################################
#############################################################################
#############################################################################

##Fetching all Necessary Library

library(tidymodels)
library(tidyverse)
library(cowplot)
library(car)
library(visdat)
library(tidyr)
library(dplyr)
library(ROCit)
library(lubridate)
library(pROC)

####Now fetching the Train data set in R file
df1<-read.csv(r'(C:\Project\Project2\store_train.csv)')

####Now fetching the Test Data set in R file
df2<-read.csv(r'(C:\Project\Project2\store_test.csv)')

names(df1)
#### Checking for the data 
head(df1)

head(df2)

glimpse(df1)
####Part 1 solve#################################################

t1<-filter(df1,df1$Areaname=='Kennebec County')

table(df1$Areaname)

table(df1$state_alpha)

table(df1$store_Type)
df2<-df1 %>% group_by(store_Type) %>% 
  filter(Areaname=='Kennebec County, ME') %>% 
  summarise('Sum_Sales'=sum(c(sales0,sales1,sales2,sales3,sales4))) %>% 
  summarise()


df4<-df1 %>% rowwise() %>% 
  mutate('Total_sales'=sum(sales0,sales1,sales2,sales3,sales4)) %>% data.frame() 

df5<-df4 %>% group_by(store_Type) %>% 
  summarise('Var_Store_type'=var(Total_sales))

round(prop.table(table(df1$store_Type)),2)

un_area<-unique(df1$Areaname)
length(un_area)

boxplot(df4$Total_sales)
IQR(df4$Total_sales)
quantile(df4$Total_sales)
4968.75+(1.5*1546.75)
o5<-df4$Total_sales[df4$Total_sales>7288.875]
length(o5)

prop.table(table(df1$store_Type))

plot(df1$sales0)
plot(df1$sales1)


boxplot(df1$sales4)

df5<-df1 %>% group_by(store_Type) %>% 
  summarise('Response'=sum(store),'Count'=n()) %>% 
  mutate('Response_Rate'=(Response/Count)*100)%>%  data.frame()

round(df5$Response_Rate,2)

###############End#########################################################

###################Part###########################################

df1<-read.csv(r'(C:\Project\Project2\store_train.csv)')

df2<-read.csv(r'(C:\Project\Project2\store_test.csv)')

head(df1)
glimpse(df1)

table(df1$country)

table(df1$State)

table(df1$state_alpha)

tail(sapply(df1,is.na),20)

table(df1$countytownname)

table(df1$countyname)

fun_e<-function(x){
  
  x<-gsub('\\d.','',x)
  
  x<-substr(x,1,5)
  
  return(x)
  
}

#####Event Rate
table(df1$store)

prop.table(table(df1$store))

df1$store<-as.numeric(df1$store==1)

prop.table(table(df1$store))

names(df1)
####Transformation fit

###Data-Preparation from train data

dp_pipe<-recipe(store~.,data=df1) %>% 
  update_role(Areaname,countytownname,Id,new_role = 'Drop_Vars') %>% 
  update_role(state_alpha,store_Type,countyname,storecode,new_role = 'As_Dummies') %>% 
  step_rm(has_role('Drop_Vars')) %>% 
  step_mutate_at(storecode,fn=fun_e) %>% 
  step_mutate_at(country,fn=as.numeric) %>% 
  step_unknown(has_role('As_Dummies'),new_level="__missing__") %>%   step_other(has_role('As_Dummies'),threshold =0.004,other="__other__") %>% 
  step_dummy(has_role('As_Dummies')) %>%
  step_impute_median(all_numeric(), -all_outcomes())

####Preparing the Data

dp_pipe<-prep(dp_pipe)  

###Final Transformation from fit
##Baking the data into train and test data

train<-bake(dp_pipe,new_data = NULL)##Baking train data

test<-bake(dp_pipe,new_data = df2)## Baking test data

####checking for head of train and test data

head(train)

tail(train)

head(test)

vis_dat(train)

vis_dat(test)

####Checking for dimensions for train data

dim(train)

#####Creating 80% & 20% sample for the train and test sample data
set.seed(1)
s<-sample(1:nrow(train),0.8*nrow(train))

t1<-train[s,]

t2<-train[-s,]
####Creating a variable to check the VIF

check_for_vif<-lm(store~ . -store_Type_X__other__ -storecode_X__other__ -sales0 -countyname_X__other__ - State- sales2 -sales3 -state_alpha_ME -sales1 -state_alpha_NH -state_alpha_MA ,data = t1)

####Checking the VIF on the created variable

sort(vif(check_for_vif),decreasing = TRUE)[1:3]

####Checking for Alias to remove correlated features of the Linear Regression the model

alias(check_for_vif)

###Checking for summary on the Linear regression model

summary(check_for_vif)

#####Checking for step wise regression on the Linear Regression model 

#####Creating Logistic regression Model on t1 data

log_fit<-glm(store~ . -store_Type_X__other__ -storecode_X__other__ -sales0 -countyname_X__other__ - State- sales2 -sales3 -state_alpha_ME -sales1 -state_alpha_NH -state_alpha_MA ,data = t1,family='binomial')

log_fit=stats::step(log_fit)
###Running the scipen function for finding full decimal number

options(scipen = 999)

####Running the Logistic Regression Model
log_fit<-glm(store~ State + countyname_Cheshire.County + countyname_Coos.County + 
               countyname_Essex.County + countyname_New.Haven.County + countyname_Rutland.County + 
               countyname_Somerset.County + countyname_Washington.County + 
               countyname_Worcester.County + countyname_X__other__ + storecode_METRO + 
               state_alpha_IL + state_alpha_IN + state_alpha_LA + state_alpha_MS + 
               state_alpha_NM + state_alpha_NY + state_alpha_OK + state_alpha_PR + 
               state_alpha_SD + state_alpha_TN + state_alpha_TX + state_alpha_VA + 
               state_alpha_WV + store_Type_Supermarket.Type1, data = t1 ,family='binomial')

summary(log_fit)
####saving the log_fit file
saveRDS(log_fit,file='C:/data/mylogit1.RDS')

###Reading the log_fit file
readRDS(file='C:/data/mylogit1.RDS')
####score on test data
te.score<-predict(log_fit ,newdata = t2, type='response')
####score on train data
tr.score<-predict(log_fit, newdata = t1, type = 'response')

summary(log_fit)
####AUC ROC on test data
pROC::auc(pROC::roc(t2$store, te.score))
#####AUC ROC on train data
pROC::auc(pROC::roc(t1$store, tr.score))


###Checking with the features on the linear regression model 

check_for_vif<-lm(store~ State + countyname_Cheshire.County + countyname_Coos.County + 
                    countyname_Essex.County + countyname_New.Haven.County + countyname_Rutland.County + 
                    countyname_Somerset.County + countyname_Washington.County + 
                    countyname_Worcester.County + countyname_X__other__ + storecode_METRO + 
                    state_alpha_IL + state_alpha_IN + state_alpha_LA + state_alpha_MS + 
                    state_alpha_NM + state_alpha_NY + state_alpha_OK + state_alpha_PR + 
                    state_alpha_SD + state_alpha_TN + state_alpha_TX + state_alpha_VA + 
                    state_alpha_WV + store_Type_Supermarket.Type1, data = train)

####Checking for VIF for correlated features

sort(vif(check_for_vif),decreasing = TRUE)[1:3]

###Correlation test for cross verification

cor.test(train$countyname_Cheshire.County,train$store)

cor.test(train$state_alpha_SD,train$store)
####logistic regression final model on train data

log_fit_final<-glm(store~ State + countyname_Cheshire.County + countyname_Coos.County + 
                     countyname_Essex.County + countyname_New.Haven.County + countyname_Rutland.County + 
                     countyname_Somerset.County + countyname_Washington.County + 
                     countyname_Worcester.County + countyname_X__other__ + storecode_METRO + 
                     state_alpha_IL + state_alpha_IN + state_alpha_LA + state_alpha_MS + 
                     state_alpha_NM + state_alpha_NY + state_alpha_OK + state_alpha_PR + 
                     state_alpha_SD + state_alpha_TN + state_alpha_TX + state_alpha_VA + 
                     state_alpha_WV + store_Type_Supermarket.Type1, data = train ,family='binomial')

###Summary for logistic regression on train data
summary(log_fit_final)


###scoring on train data
train.score=predict(log_fit_final,newdata = train,type='response')

real<-train$store

m = measureit(score = round(train.score,3), class = real,
              measure = c("ACC", "SENS", "SPEC","PREC","FSCR"))

###Cutoff
cutoff_data =data.frame(Cutoff = m$Cutoff,
                        TP=m$TP,
                        TN=m$TN,
                        FP=m$FP,
                        FN=m$FN, 
                        Depth = m$Depth,
                        Accuracy = m$ACC,
                        Sensitivity = m$SENS,
                        Specificity = m$SPEC, 
                        F1 = m$FSCR) %>% 
  mutate(P=TP+FN,
         N=TN+FP,
         KS=(TP/P)-(FP/N)) %>% 
  select(-P,-N) %>% 
  na.omit() %>% 
  arrange(Cutoff)

###Cutoff graph
ggplot(cutoff_data,aes(x=Cutoff,y=KS))+geom_line()


cutoff_long=cutoff_data %>% 
  select(Cutoff,Accuracy:KS) %>% 
  gather(Measure,Value,Accuracy:KS)

ggplot(cutoff_long,aes(x=Cutoff,y=Value,color=Measure))+geom_line()

rocit = rocit(score = train.score, 
              class = real) 

kplot=ksplot(rocit)

my_cutoff=kplot$`KS Cutoff`

test.score= predict(log_fit_final,newdata = test,type='response')

write.csv(test.score,"Saibal_Chakraborty_P2_Part2.csv",row.names = F)
