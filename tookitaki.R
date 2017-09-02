#Clearing the global environment.
##Required libraries are loaded at their point of use.

rm(list = ls(all = TRUE))

#setting working directory.

setwd("G:/Prasat/study/Tookitaki/RBL_70_30")

#Loading all the train data.

raw_data_train = read.csv("raw_data_70.csv" , header = TRUE)
raw_acc_train = read.csv("raw_account_70.csv" , header = TRUE)
raw_enquiry_train = read.csv("raw_enquiry_70.csv" , header = TRUE)

##loading all the test data.

raw_data_test = read.csv("raw_data_30.csv" , header = TRUE)
raw_acc_test = read.csv("raw_account_30.csv" , header = TRUE)
raw_enquiry_test = read.csv("raw_enquiry_30.csv" , header = TRUE)

##First, i will work on the training dataset.
##A glimpse on training datasets.

library(dplyr)
glimpse(raw_acc_train)
glimpse(raw_data_train)
glimpse(raw_enquiry_train)


##Working on raw_enquiry_train
raw_enquiry_train$id = as.factor(raw_enquiry_train$id)
raw_enquiry_train$enq_purpose = as.factor(raw_enquiry_train$enq_purpose)
raw_enquiry_train$enq_amt = as.factor(raw_enquiry_train$enq_amt)

sum(is.na(raw_enquiry_train))
summary(raw_enquiry_train)
str(raw_enquiry_train)

##Imputation of values in the place of NA's

library(DMwR)
raw_enquiry_train = knnImputation(raw_enquiry_train, k = 9)
sum(is.na(raw_enquiry_train))

##Working on test set of enquiry data.

summary(raw_enquiry_test)

##Imputation of values.

raw_enquiry_test = knnImputation(raw_enquiry_test , k = 9)
sum(is.na(raw_enquiry_test))

###Working on raw_account_train.

summary(raw_acc_train)
glimpse(raw_acc_train)
sum(is.na(raw_acc_train))

##Removing unwanted variables

raw_acc_train$amt_past_due = NULL
raw_acc_train$suitfiledorwilfuldefaultorwritte = NULL
raw_acc_train$valueofcollateral = NULL
raw_acc_train$typeofcollateral = NULL
raw_acc_train$emiamount = NULL
raw_acc_train$writtenoffamountprincipal = NULL
raw_acc_train$writtenoffamounttotal = NULL
raw_acc_train$settlementamount = NULL
raw_acc_train$paymentfrequency = NULL
raw_acc_train$actualpaymentamount = NULL
raw_acc_train$dateofentryforerrorcode = NULL
raw_acc_train$errorcode = NULL
raw_acc_train$disputeremarkscode2 = NULL
raw_acc_train$disputeremarkscode1 = NULL
raw_acc_train$dateofentryforerrorordisputerema = NULL
raw_acc_train$cibilremarkscode = NULL
raw_acc_train$dateofentryforcibilremarkscode = NULL
raw_acc_train$writtenoffandsettled = NULL
raw_acc_train$amt_past_due = NULL
raw_acc_train$cashlimit = NULL
raw_acc_train$creditlimit = NULL

##Imputation of values.

raw_acc_train = knnImputation(raw_acc_train , k=20)
sum(is.na(raw_acc_train))

##working on raw_account of test data.

summary(raw_acc_test)
glimpse(raw_acc_test)

##removing the same in test data.

raw_acc_test$amt_past_due = NULL
raw_acc_test$suitfiledorwilfuldefaultorwritte = NULL
raw_acc_test$valueofcollateral = NULL
raw_acc_test$typeofcollateral = NULL
raw_acc_test$emiamount = NULL
raw_acc_test$writtenoffamountprincipal = NULL
raw_acc_test$writtenoffamounttotal = NULL
raw_acc_test$settlementamount = NULL
raw_acc_test$paymentfrequency = NULL
raw_acc_test$actualpaymentamount = NULL
raw_acc_test$dateofentryforerrorcode = NULL
raw_acc_test$errorcode = NULL
raw_acc_test$disputeremarkscode2 = NULL
raw_acc_test$disputeremarkscode1 = NULL
raw_acc_test$dateofentryforerrorordisputerema = NULL
raw_acc_test$cibilremarkscode = NULL
raw_acc_test$dateofentryforcibilremarkscode = NULL
raw_acc_test$writtenoffandsettled = NULL
raw_acc_test$amt_past_due = NULL
raw_acc_test$cashlimit = NULL
raw_acc_test$creditlimit = NULL

##Imputation of values.

raw_acc_train = knnImputation(raw_acc_test , k=20)
sum(is.na(raw_acc_test))
glimpse(raw_acc_test)

####Working on raw_data of training set.

glimpse(raw_data_train)
sum(is.na(raw_data_train))

##removing features 

raw_data_train$rbl_relationship_no = NULL
raw_data_train$app_account_type = NULL
raw_data_train$cin = NULL
raw_data_train$reject_reason_desc = NULL
raw_data_train$reject_reason_code = NULL
raw_data_train$company_type = NULL
raw_data_train$industry_type = NULL
raw_data_train$email = NULL
raw_data_train$office_email = NULL
raw_data_train$promo_code = NULL
raw_data_train$mob_verified = NULL
raw_data_train$entry_time = NULL
raw_data_train$override_fee_code = NULL
raw_data_train$override_months = NULL
raw_data_train$mktg_code = NULL
raw_data_train$app_go_green = NULL
raw_data_train$designation = NULL
raw_data_train$app_coupon_code = NULL
raw_data_train$existing_card_start_date = NULL
raw_data_train$app_existing_other_loan_cc = NULL
raw_data_train$edu_qualification = NULL
raw_data_train$app_mobile = NULL

##Imputation of values

sum(is.na(raw_data_train))
raw_data_train = knnImputation(raw_data_train , k = 20)

##Raw_data of test set.

glimpse(raw_data_test)

##removing features

raw_data_test$rbl_relationship_no = NULL
raw_data_test$app_account_type = NULL
raw_data_test$cin = NULL
raw_data_test$reject_reason_desc = NULL
raw_data_test$reject_reason_code = NULL
raw_data_test$company_type = NULL
raw_data_test$industry_type = NULL
raw_data_test$email = NULL
raw_data_test$office_email = NULL
raw_data_test$promo_code = NULL
raw_data_test$mob_verified = NULL
raw_data_test$entry_time = NULL
raw_data_test$override_fee_code = NULL
raw_data_test$override_months = NULL
raw_data_test$mktg_code = NULL
raw_data_test$app_go_green = NULL
raw_data_test$designation = NULL
raw_data_test$app_coupon_code = NULL
raw_data_test$existing_card_start_date = NULL
raw_data_test$app_existing_other_loan_cc = NULL
raw_data_test$edu_qualification = NULL
raw_data_test$app_mobile = NULL

##imputation of values

raw_data_test = knnImputation(raw_data_test , k = 20)


##fearture - raw_Data_train - gain
library(FSelector)
information.gain(bad_flag_worst6 ~ . , raw_data_train)


###model

library(randomForest)
set.seed(123)
rf <- randomForest(bad_flag_worst6 ~ ., data=raw_data_train, keep.forest=TRUE, ntree=30)

####

x = subset(raw_data_train , select = -bad_flag_worst6)
y = as.factor(raw_data_train$bad_flag_worst6)

a = subset(raw_data_test , select = -bad_flag_worst6)
b = as.factor(raw_data_test$bad_flag_worst6)


##ensembling

library(ada)
model=ada(x,y,iter=20,loss="logistic")

pred  =  predict(model, a)
table(pred, b)
