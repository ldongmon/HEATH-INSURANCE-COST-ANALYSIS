## PROCESS PHASE

## 1°Intall packages

install.packages("tidyverse")
install.packages("skimr")
install.packages("janitor")
library(tidyverse)
library(skimr)
library(janitor)
install.packages('ggplot2')
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("plotly")
library(plotly)
install.packages("cowplot")
library("cowplot")

## IMPORT DATASET
data=read.csv("HEATH_DATA.csv")
my_data=data

## preview datasets
View(data)
typeof(data)
colnames(data)
str(data)
nrow(data)
ncol(data)
summary(data)

## is there any na variables?

is.na(data)

factor_GENDER=factor(data$sex)
View(factor_GENDER)
class(factor_GENDER)


### analyse phase
 ##univariate 

P1=ggplot(my_data, aes(x=children)) +
  geom_boxplot()
P2=ggplot(my_data, aes(x=charges)) +
  geom_boxplot()
P3=ggplot(my_data, aes(x=age)) +
  geom_boxplot()
### now we are going to plot in by arranging it in one page

plot_grid(P1,P2,P3, labels = c(),nrow = 1, ncol = 3)

#ggplot(data)+
 # geom_bar(mapping = aes(x=bmi))

P4=ggplot(data, aes(bmi))+
  geom_histogram(fill="deepskyblue", color="red")


#ggplot(data)+
 # geom_bar(mapping = aes(x=bmi))+
  #geom_histogram(fill="deepskyblue", color="red")

P5=ggplot(data, aes(children))+
  geom_histogram(fill="blue", color="red")

P6=ggplot(data, aes(charges))+
  geom_histogram(fill="blue", color="red")

P7=ggplot(data, aes(age))+
  geom_histogram(fill="blue", color='red')

### now we are going to plot in by arranging it in one page
plot_grid(P4,P5,P6,P7, labels = c(),nrow = 2, ncol = 2)


### bivariate and multivariate analysis
## we wat to create the boxplot for each 3 numeric variable,
## but by the level of each nominal variable


### boxplot by the sex 

P8=ggplot(my_data, aes(x=sex, y=bmi, color=sex)) +
  geom_boxplot()


P9=ggplot(my_data, aes(x=sex, y=children, color=sex)) +
  geom_boxplot()


P10=ggplot(my_data, aes(x=sex, y=charges, color=sex)) +
  geom_boxplot()

### boxplot by smoker variable

P11=ggplot(my_data, aes(x=smoker, y=bmi, color=smoker)) +
  geom_boxplot()

P12=ggplot(my_data, aes(x=smoker, y=children, color=smoker)) +
  geom_boxplot()

P13=ggplot(my_data, aes(x=smoker, y=charges, color=smoker)) +
  geom_boxplot()

### now we are going to plot in by arranging it in one page
plot_grid(P8,P9,P10,P11,P12,P13, labels = c(),nrow = 3, ncol = 3)


## relationship between charges and bmi

### creation of scatter plot

P14=ggplot(data)+
  geom_point(mapping = aes(x=bmi, y=charges))

P15=ggplot(data)+
  geom_point(mapping = aes(x=bmi, y=charges, color=smoker))

P16=ggplot(data)+
  geom_point(mapping = aes(x=bmi, y=charges, color=sex))

P17=ggplot(data)+
  geom_point(mapping = aes(x=bmi, y=charges, color=region))

### now we are going to plot in by arranging it in one page
plot_grid(P14,P15,P16,P17, labels = c(),nrow = 2, ncol = 2)
############### line chart


P18=ggplot(data)+
  geom_smooth(mapping = aes(x=bmi, y=charges))

P19=ggplot(data)+
  geom_smooth(mapping = aes(x=bmi, y=charges, color=smoker))


P20=ggplot(data)+
  geom_smooth(mapping = aes(x=bmi, y=charges, color=sex))

P21=ggplot(data)+
  geom_smooth(mapping = aes(x=bmi, y=charges, color=region))

### now we are going to plot in by arranging it in one page
plot_grid(P18,P19,P20,P21, labels = c(),nrow = 2, ncol = 2)

### relationship between charges and smoker character

#############scatter plot

P22=ggplot(data)+
  geom_point(mapping = aes(x=age, y=charges))

P23=ggplot(data)+
  geom_point(mapping = aes(x=age, y=charges, color=sex))

P24=ggplot(data)+
  geom_point(mapping = aes(x=age, y=charges, color=smoker))

P25=ggplot(data)+
  geom_point(mapping = aes(x=age, y=charges, color=region))

### now we are going to plot in by arranging it in one page
plot_grid(P22,P23,P24,P25, labels = c(),nrow = 2, ncol = 2)
####line chart
P26=ggplot(data)+
  geom_smooth(mapping = aes(x=age, y=charges))

P27=ggplot(data)+
  geom_smooth(mapping = aes(x=age, y=charges, color=sex))

P28=ggplot(data)+
  geom_smooth(mapping = aes(x=age, y=charges, color=smoker))

P29=ggplot(data)+
  geom_smooth(mapping = aes(x=age, y=charges, color=region))

### now we are going to plot in by arranging it in one page
plot_grid(P26,P27,P28,P29, labels = c(),nrow = 2, ncol = 2)

### relationship between charges and children

### scatter plot

P30=ggplot(data)+
  geom_point(mapping = aes(x=children, y=charges))

P31=ggplot(data)+
  geom_point(mapping = aes(x=children, y=charges, color=smoker))

P32=ggplot(data)+
  geom_point(mapping = aes(x=children, y=charges, color=sex))

P33=ggplot(data)+
  geom_point(mapping = aes(x=children, y=charges, color=region))
### now we are going to plot in by arranging it in one page
plot_grid(P30,P31,P32,P33, labels = c(),nrow = 2, ncol = 2)

########bar chart



P34=ggplot(data, aes(charges))+
  geom_histogram(fill="blue", color="red")+
  facet_grid(~smoker)

P35=ggplot(data, aes(charges))+
  geom_histogram(fill="blue", color="red")+
  facet_grid(~sex)

### now we are going to plot in by arranging it in one page
plot_grid(P34,P35, labels = c(),nrow = 1, ncol = 2)


## create a heatmap
data_num=data %>%
  select(age, bmi, children, charges)
View(data_num)

### now we have to find the correlation matrix
mat_cor=cor(data_num)
View(mat_cor)
###now we can find the draw the heatmap of the correlation matrix
heatmap(mat_cor)

############corrplot
install.packages("Correlplot")
library(corrplot)
corrplot(mat_cor,type="upper", order="hclust", tl.col="black", tl.srt=45)

### regression
M_1=lm(charges~age+bmi+sex+children+smoker+region, data)

summary(M_1)
### delet region variable
M_2=lm(charges~age+bmi+sex+children+smoker, data)
summary(M_2)

#### let's perform machine leaning 

##Great! Let’s perform the split now. 70% 
#of the data is used for training, and the remaining 30% 
#is used for testing. Here’s the code:
my_data=data
set.seed(25)
n = nrow(my_data)
n
install.packages("caTools")
library(caTools)
set.seed(101) 


n = nrow(my_data)
my_split = sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(0.7, 0.3))


train_data = my_data[my_split, ]
test_data = my_data[!my_split, ]


nrow(train_data) 
nrow(test_data)



### can now process with the model training

m_T1=lm(charges ~ age+bmi+sex+children+smoker+region, data = train_data)
#### OR 
m_T1=lm(charges~., data = train_data) 
## to train with all the variable of the model
summary(m_T1)

### next we can make the residual plot or residual histogram
## we are expect to see something approximatly normally
## before it let's define the residual model

residual_model=as.data.frame(residuals(m_T1))
##PLOT residual histogram

ggplot(residual_model, aes(residuals(m_T1)))+
  geom_histogram(fill="blue", color='black')


## we can build a better model
## we are going to delete the region variable because 
### the p value for these variable are non significant
m_T2=lm(charges ~ age+bmi+sex+children+smoker, data = train_data)  

summary(m_T2)

### residual
residual_model=as.data.frame(residuals(m_T2))
##PLOT residual histogram

ggplot(residual_model, aes(residuals(m_T2)))+
  geom_histogram(fill="blue", color='black')

## WE can build the third model by delating the sex variable

m_T3=lm(charges ~ age+bmi+children+smoker, data = train_data)  
summary(m_T3)

### residual
residual_model=as.data.frame(residuals(m_T3))
##PLOT residual histogram

ggplot(residual_model, aes(residuals(m_T3)))+
  geom_histogram(fill="blue", color='black')

## WE can dinally make prediction on our third model
predict_=predict(m_T3,test_data)


## we can now create a dataframe of actual and predicted value
model_Predict_Actual=cbind(test_data$charges,predict_)

### adding a colname of this model
colnames(model_Predict_Actual)=c("Actual values", "Predicted values")

## transform it as a dataframe
model_Predict_Actual=as.data.frame(model_Predict_Actual)

is.data.frame(model_Predict_Actual)

View(model_Predict_Actual)

### To evaluate our model we are going to use the MSE and RMSE
MSE=mean(model_Predict_Actual$`Actual values`- model_Predict_Actual$`Predicted value`)^2

View(MSE)
RMSE

RMSE=sqrt(MSE)
View(RMSE)

## RMSE=233.7015 means that are on average wrong by 233.7015 unit of charges
