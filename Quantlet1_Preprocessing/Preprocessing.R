################################################################################
############################## SPL Preprocessing ###############################
################################################################################

################################### Packages ###################################
# install.packages("dplyr")
# install.packages("DMwR")
# install.packages("ISLR")
# install.packages("Rtsne")
# install.packages("Hmisc")
# install.packages("devtools")
# devtools::install_github("rstudio/httpuv")
# install.packages("klaR")
# install.packages("httpuv")
# install.packages("dendextend") 
# install.packages("dbscan")
# install.packages("factoextra")
################################### Libraries ##################################
library(DMwR) # Data mining with R
library(rpart) # Imputation by similarity
library(Hmisc) # Descriptive statistics
library(scales) # Rescaling of variables
library(dplyr) # for data cleaning
library(ISLR) # for college dataset
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization
library(MASS)
library(klaR) # for k-modes
library(httpuv) # for klaR
library(Rcpp) 
library(dendextend) # coloring branches in dendrogram
library(dbscan) # density based clustering
library(fpc)
library(factoextra)
############################## Load relevant data ##############################
if(!file.exists("rawdata.csv")){
  print("Please download the data file from the same folder, and put in working directory.")
}

# Clear environment
remove(list = ls())


# Rename data
data = read.csv("rawdata.csv", stringsAsFactors = FALSE)

# Overview of data structure
str(data)

# Drop irrelevant variables
for (i in c("delivery_date", "return", "item_size", "item_color", 
            "brand_id", "return", "item_id")){
  data[, c(i)] = NULL
}

############################## Create new variables ##############################
# Age variable (in years), based on variable "user_dob"
data$user_dob = as.Date(as.character(data$user_dob))

# Check for anomalous values 
describe(data$user_dob)
hist(data$user_dob, "years")
head(sort(table(data$user_dob), decreasing=TRUE))

# Create age variable
data$age = as.numeric(floor((as.Date(as.character("2013-03-31"), format="%Y-%m-%d")-
                                as.Date(as.character(data$user_dob), format="%Y-%m-%d"))/365.25))

# Set anomalous values to NAs
data$age[data$age<7 | data$age>85] = NA

## Impute anomalous values by rpart technique
anova_mod = rpart(age ~ ., data=data[!is.na(data$age), ], method="anova", na.action=na.omit)
age_pred = predict(anova_mod, data[is.na(data$age), ])
table(age_pred)
data$age = impute(data$age, age_pred)
data$age = floor(as.numeric(data$age))
rm(anova_mod,age_pred)

# Drop original variable
data$user_dob=NULL


# Gender variable

# Reduce data frame to men and women only
data = data[ which(data$user_title=='Mrs' 
                    | data$user_title=='Mr'), ]

colnames(data)[colnames(data)=="user_title"] = "gender"


# Customer loyalty variable (in months), based on variable "user_reg_date"

# Check for anomalous values
head(sort(table(data$user_reg_date),decreasing =TRUE)) 
#improbable number of Reg.dates on 2011-02-16, 28200 instances

# Set anomalous values to NAs
data$user_reg_date[data$user_reg_date=="2011-02-16"] = NA

# Create loyalty variable
data$loyaltydays = as.numeric(ceiling((as.Date(as.character("2013-04-01"), 
                                                format="%Y-%m-%d")- as.Date(as.character(data$user_reg_date), format="%Y-%m-%d"))))
data$loyalty.in.months = as.numeric(ceiling((data$loyaltydays)/(365/12)))

## Impute anomalous values by rpart
anova_mod = rpart(loyalty.in.months~ ., data=data[!is.na(data$loyalty.in.months), ], method="anova", na.action=na.omit)
loyalty.in.months_pred = predict(anova_mod, data[is.na(data$loyalty.in.months), ])
table(loyalty.in.months_pred)
data$loyalty.in.months = impute(data$loyalty.in.months, loyalty.in.months_pred)
data$loyalty.in.months = floor(as.numeric(data$loyalty.in.months))
rm(anova_mod, loyalty.in.months_pred)

# Drop irrelevant variables
data$user_reg_date = NULL 
data$loyaltydays = NULL

# Focus only on customers of loyalty more than 5 months
data = data[which(data$loyalty.in.months>=6), ]

# Create new variables based on customer ID (different length), i.e. separate of orginal data set (based on "order_item_id")

# Total amount ordered per customer, based on "item_price"

# Check for anomalous values 
describe(data$item_price) # no problems apparent

# total value variable
total.value = aggregate(data$item_price, by=list(data$user_id), sum)

# Rename columns
colnames(total.value)[colnames(total.value)=="Group.1"] = "ID"
colnames(total.value)[colnames(total.value)=="x"] = "total.value"

# Total number of orders per customer, based on appearance of "user_id"

# Check for anomalous values 
describe(data$user_id) # no problems apparent

# Total orders variable
total.orders=as.data.frame(table(data$user_id), decreasing = TRUE)

# Rename columns
colnames(total.orders)[colnames(total.orders)=="Var1"] = "ID"
colnames(total.orders)[colnames(total.orders)=="Freq"] = "total.orders"

# Change format to integer
total.orders$ID = as.integer(levels(total.orders$ID))[total.orders$ID]


# Recency of last order variable, based on "order_date"

# Check for anomalous values 
describe(data$order_date)

# Receny of any order (in months)
data$recency = as.numeric(ceiling((as.Date(as.character("2013-04-01"), 
                                            format="%Y-%m-%d")-as.Date(as.character(data$order_date), 
                                                                       format="%Y-%m-%d"))/(365/12)))

# Most recent order by customer (in months)
recency = aggregate(data$recency, by=list(data$user_id), min)
colnames(recency)[colnames(recency)=="Group.1"] = "ID"
colnames(recency)[colnames(recency)=="x"] = "recency.in.months"

# Drop irrelevant variables
for (i in c("recency", "item_price", "order_item_id", "order_date")){
  data[, c(i)] = NULL
}

# Change name "user_id" to "ID"
colnames(data)[colnames(data)=="user_id"] = "ID"

# Drop duplicates, i.e. reduce original dataframe to unique customer ID
data = unique(data[,1:5 ])

sort(data$ID, decreasing = TRUE)
# Merge customer ID based data frames to original data set
data = Reduce(function(x, y) merge(x, y, all=T, by=c("ID")), 
               list(total.value, total.orders, recency, data), accumulate=F)

# Remove irrelevant data from environment
rm(total.value, total.orders, recency)

# Final changes of variables
# Average number of orders per month
data$frequency.per.month = data$total.orders/12
data$total.orders = NULL

# Average amount ordered per month
data$monetary.per.month = data$total.value/12
data$total.value = NULL


# rfm 
# Cut them in 10 groups each
data$recency.rfm = 11-as.numeric(cut2(data$recency.in.months, g=10)) 
data$frequency.rfm = as.numeric(cut2(data$frequency.per.month, g=10))
data$monetary.rfm = as.numeric(cut2(data$monetary.per.month, g=10))

# Create rfm (numerical)
data$rfm = (1/3)*data$recency.rfm+(1/3)*data$frequency.rfm+(1/3)*data$monetary.rfm

# Drop rest of irrelevant variables
for (i in c("recency.rfm", "frequency.rfm", "monetary.rfm", "frequency.per.month", 
            "recency.in.months", "monetary.per.month")){
  data[, c(i)] = NULL
}


# Rename column
colnames(data)[colnames(data)=="user_state"] = "state"

# Reorder variables 
data = data[c("ID", "state", "gender", "age", "loyalty.in.months", "rfm")]

# Factorize variables
for (i in c("state", "gender")){
  data[, c(i)] = as.factor(data[, c(i)])
}

# Drop NAs in final data set
data = data[complete.cases(data), ]

############################### data reduction ################################
# Draw subsample, otherwise calculation process for distances takes too long
set.seed(1) # control sample
sample= data[sample(nrow(data), "1000"), ]
