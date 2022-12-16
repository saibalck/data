
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

####Now fetching the Train data set in R file

d1_h1<-read.csv(r'(C:\Project\Project 1\housing_train.csv)')

d1_h2<-read.csv(r'(C:\Project\Project 1\housing_test.csv)')

##finding the head of the data

head(d1_h1)

head(d1_h2)

setdiff(names(d1_h1),names(d1_h2))
##overall data visualization

#########Part-1-Solve#######################################################
glimpse(d1_h1)


var(d1_h1$Price)

sum(is.na(d1_h1$YearBuilt))

table(d1_h1$Type)

d1_p1<-d1_h1 %>% group_by(Type) %>% 
  summarise("Mean_Price"=mean(Price)) %>% data.frame()

s1<-d1_p1$Mean_Price[1]-d1_p1$Mean_Price[2]

unique(d1_h1$Postcode)

table(d1_h1$Postcode)

plot(d1_h1$Distance)

ggplot(data=d1_h1,aes(x=d1_h1$Price))+geom_histogram()

table(d1_h1$SellerG)
s_p<-d1_h1 %>% group_by(SellerG) %>% 
  summarise('Sum_P'=sum(Price)) %>%data.frame()

s_p[which.max(s_p$Sum_P),]

s_c<-d1_h1 %>% group_by(CouncilArea) %>% 
  summarise('Average_Price'=mean(Price)) %>% data.frame()

s_c[which.max(s_c$Average_Price),]

s_v<-d1_h1 %>% group_by(CouncilArea) %>% 
  summarise('Variance_Price'=var(Price)) %>% data.frame()

s_v[which.max(s_v$Variance_Price),]

###########################End-Part1###################################

##########################Part2########################################


fun_na<-function(x){
  x<-which(is.na(x))
  return(x)
  
}



func_na_median<-function(x){
  
  x[which(is.na(x))]<-median(x,na.rm = TRUE)
  
  return(x)
}


replace_quantile <- function(x, q=.75){
  qvalue <- quantile(x, probs=q)
  x[x > qvalue] <- median(x,na.rm = TRUE)
  return(x)
}

sapply(d1_h1,fun_na)

head(d1_h1$Address)

tail(d1_h1$Address)

######Data Processing for creating Modelling###########

df1<-d1_h1 %>% group_by(Rooms) %>% 
  summarise('Mean_BedRoom2'=mean(Bedroom2,na.rm=TRUE),
            'Mean_Bathroom'=mean(Bathroom,na.rm=TRUE),
            'Mean_Car'=mean(Car,na.rm=TRUE))

sum(is.na(d1_h1$Bedroom2))
df2<-d1_h1 %>% group_by(Rooms) %>% 
 summarise('NA1'=which(is.na(Bedroom2)))

tail(df2)

####data preparation for train data
dp_pipe=recipe(Price ~ .,data=d1_h1) %>% 
  update_role(Postcode,Address,CouncilArea, new_role = "drop_vars") %>% 
  update_role(Suburb,Type,SellerG,Method,
              new_role="to_dummies") %>% 
  
  step_rm(has_role("drop_vars")) %>% 
  step_unknown(has_role("to_dummies"),new_level="__missing__") %>% 
  step_other(has_role("to_dummies"),threshold =0.005,other="__other__") %>% 
  step_dummy(has_role("to_dummies")) %>% 
  step_impute_median(all_numeric(),-all_outcomes())


dp_pipe<-prep(dp_pipe)#####data preparation part

#####transforming the data+bake

train=bake(dp_pipe,new_data = NULL)#####Baking on Train data

test=bake(dp_pipe,new_data = d1_h2)#####Baking on Test data
head(test)
head(train)

vis_dat(train)

vis_dat(d1_h1)

vis_dat(test)

set.seed(2)
s=sample(1:nrow(train),0.8*nrow(train))

t1<-train[s,]###train data where i will build the model

t2<-train[-s,]###test data where i will validate the model

fit<-lm(Price~.-Type_X__other__- Suburb_X__other__,data = t1)####removing the correlated features which are found while running VIF

alias(fit)

sort(vif(fit),decreasing=TRUE)

options(scipen = 999)
##Checking the summary of the fit created

summary(fit)

####doing step wise regression

fit<- stats::step(fit)

#####Building the model with all the coefficients

fit<-lm(Price~Rooms + Distance + Bedroom2 + Bathroom + Car + Landsize + 
          BuildingArea + YearBuilt + Suburb_Airport.West + Suburb_Altona.North + 
          Suburb_Armadale + Suburb_Ascot.Vale + Suburb_Ashburton + 
          Suburb_Avondale.Heights + Suburb_Balwyn + Suburb_Balwyn.North + 
          Suburb_Bentleigh + Suburb_Bentleigh.East + Suburb_Brighton + 
          Suburb_Brighton.East + Suburb_Brunswick + Suburb_Brunswick.East + 
          Suburb_Brunswick.West + Suburb_Burwood + Suburb_Camberwell + 
          Suburb_Carnegie + Suburb_Coburg + Suburb_Coburg.North + Suburb_Collingwood + 
          Suburb_Doncaster + Suburb_Elwood + Suburb_Fawkner + Suburb_Footscray + 
          Suburb_Glen.Iris + Suburb_Glenroy + Suburb_Hadfield + Suburb_Hampton + 
          Suburb_Hawthorn.East + Suburb_Heidelberg.Heights + Suburb_Heidelberg.West + 
          Suburb_Keilor.East + Suburb_Kensington + Suburb_Kew + Suburb_Maidstone + 
          Suburb_Malvern + Suburb_Malvern.East + Suburb_Maribyrnong + 
          Suburb_Melbourne + Suburb_Moonee.Ponds + Suburb_Murrumbeena + 
          Suburb_Newport + Suburb_North.Melbourne + Suburb_Ormond + 
          Suburb_Pascoe.Vale + Suburb_Preston + Suburb_Reservoir + 
          Suburb_Richmond + Suburb_Rosanna + Suburb_South.Melbourne + 
          Suburb_St.Kilda + Suburb_Sunshine + Suburb_Sunshine.North + 
          Suburb_Sunshine.West + Suburb_Surrey.Hills + Suburb_Thornbury + 
          Suburb_Toorak + Suburb_West.Footscray + Suburb_Yarraville + 
          Type_t + Type_u + Method_S + Method_SP + Method_X__other__ + 
          SellerG_Buxton + SellerG_Fletchers + SellerG_Greg + SellerG_Jas + 
          SellerG_Jellis + SellerG_Kay + SellerG_Marshall + SellerG_McGrath + 
          SellerG_Miles + SellerG_RT + SellerG_Sweeney + SellerG_Williams + 
          SellerG_Woodards ,data=t1)

summary(fit)
###prediction on sample test data

t2.pred<-predict(fit,newdata = t2)

####prediction on sample train data

t1.pred<-predict(fit,newdata = t1)

##checking for Errors on test data

errors_test<-t2$Price-t2.pred

##checking for errors on train data

errors_train<-t1$Price-t1.pred

##checking for RMSE for sample test data

rmse_test<-errors**2 %>% mean() %>% sqrt()

###checking for RMSE for sample train data

rmse_train<-errors_train**2 %>% mean() %>% sqrt()

###mean absolute error for train data

mae_train<-mean(abs(errors_train))


###mean absolute error for test data

mae_test<-mean(abs(errors_test))


###Checking the mae_test result
(386700.7-380726)/380726

####Creating model on the entire Train data set

fit.final<-lm(Price~Rooms + Distance + Bedroom2 + Bathroom + Car + Landsize + 
                BuildingArea + YearBuilt + Suburb_Airport.West + Suburb_Altona.North + 
                Suburb_Armadale + Suburb_Ascot.Vale + Suburb_Ashburton + 
                Suburb_Avondale.Heights + Suburb_Balwyn + Suburb_Balwyn.North + 
                Suburb_Bentleigh + Suburb_Bentleigh.East + Suburb_Brighton + 
                Suburb_Brighton.East + Suburb_Brunswick + Suburb_Brunswick.East + 
                Suburb_Brunswick.West + Suburb_Burwood + Suburb_Camberwell + 
                Suburb_Carnegie + Suburb_Coburg + Suburb_Coburg.North + Suburb_Collingwood + 
                Suburb_Doncaster + Suburb_Elwood + Suburb_Fawkner + Suburb_Footscray + 
                Suburb_Glen.Iris + Suburb_Glenroy + Suburb_Hadfield + Suburb_Hampton + 
                Suburb_Hawthorn.East + Suburb_Heidelberg.Heights + Suburb_Heidelberg.West + 
                Suburb_Keilor.East + Suburb_Kensington + Suburb_Kew + Suburb_Maidstone + 
                Suburb_Malvern + Suburb_Malvern.East + Suburb_Maribyrnong + 
                Suburb_Melbourne + Suburb_Moonee.Ponds + Suburb_Murrumbeena + 
                Suburb_Newport + Suburb_North.Melbourne + Suburb_Ormond + 
                Suburb_Pascoe.Vale + Suburb_Preston + Suburb_Reservoir + 
                Suburb_Richmond + Suburb_Rosanna + Suburb_South.Melbourne + 
                Suburb_St.Kilda + Suburb_Sunshine + Suburb_Sunshine.North + 
                Suburb_Sunshine.West + Suburb_Surrey.Hills + Suburb_Thornbury + 
                Suburb_Toorak + Suburb_West.Footscray + Suburb_Yarraville + 
                Type_t + Type_u + Method_S + Method_SP + Method_X__other__ + 
                SellerG_Buxton + SellerG_Fletchers + SellerG_Greg + SellerG_Jas + 
                SellerG_Jellis + SellerG_Kay + SellerG_Marshall + SellerG_McGrath + 
                SellerG_Miles + SellerG_RT + SellerG_Sweeney + SellerG_Williams + 
                SellerG_Woodards ,data=train)

###checking with vif the correlation test

sort(vif(fit.final),decreasing=TRUE)

summary(fit.final)

####prediction on test data

test.pred<-predict(fit.final,newdata = test)


write.csv(test.pred,"Saibal_Chakraborty_P1_Part2(2).csv",row.names= FALSE)


