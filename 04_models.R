#==================================
# Models file
# Purpose: To build regression models.
#==================================


# ---------EDA----------
dloans_m[, (.N), by = gender]
dloans_m[, (.N), by = race]
dloans_m[, (.N), by = businessagedescription]
dloans_m[, (.N), by = fraud]


fraud_m[, (.N), by = race]
fraud_m[, (.N), by = businessagedescription]
fraud_m[, (.N), by = ethnicity]

# median
median(fraud_m$initialapprovalamount[!is.na(fraud_m$initialapprovalamount)])
median(train$initialapprovalamount[!is.na(train$initialapprovalamount)])

# mean
mean(fraud_m$initialapprovalamount[!is.na(fraud_m$initialapprovalamount)])
mean(train$initialapprovalamount[!is.na(train$initialapprovalamount)])

# mode
get_mode <- function(x){
  return(names(which.max(table(x))))
}
get_mode(train$initialapprovalamount)
get_mode(fraud_m$initialapprovalamount)

# skewness
library(moments)
skewness(fraud_m$initialapprovalamount[!is.na(fraud_m$initialapprovalamount)])
skewness(train$initialapprovalamount[!is.na(train$initialapprovalamount)])
jarque.test(fraud_m$initialapprovalamount[!is.na(fraud_m$initialapprovalamount)])
jarque.test(train$initialapprovalamount[!is.na(train$initialapprovalamount)])


# what are the top 10 loan servicers that issued the most fraudulent loans?
table_lender <- table(fraud_m$servicinglendername)
plot(sort(table_lender, decreasing=TRUE)[1:10], type = "h", ylab = "Number of fraudulent loans granted", cex.axis = .7)

# what types of 'businesses' do these borrowers take out fraud loans for?
fraud_business <- fraud_m %>% group_by(businesstype) %>% summarise(fraudloans = sum(indicted))
print(fraud_business)

# all businesses?
business <- train %>% group_by(businesstype) %>% summarise(loans = sum(fraud == 0))
business %>% print(n = Inf)

# what about the typical size of the loans the fraudsters take out?
ggplot(fraud_m, aes(x=initialapprovalamount, y = dateapproved, binwidth = 10)) + geom_histogram() + label



#boxplot of loans issued to fraudulent businesses, by business type
ggplot(subset(fraud_m, initialapprovalamount != 0), aes(x=businesstype,y=initialapprovalamount)) + geom_violin(color="black", fill="orange")+
                                          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
                                          scale_y_continuous() + labs(
                                            x="Business Type",
                                            y="Loan Approval Amount",
                                            title="Fraudulent Businesses: Loan Size by Business Type")

# boxplot of loans issued to ALL, by business type
ggplot(subset(train, initialapprovalamount != 0), aes(x=businesstype,y=initialapprovalamount)) + geom_violin(color="black", fill="orange")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_continuous() + labs(
    x="Business Type",
    y="Loan Approval Amount",
    title="Loan Size by Business Type")



# ----------------------------------------------------------
# --------- Logistic -------------
# ----------------------------------------------------------
# v1:
glm.fits <- glm(fraud ~ . - originatinglender - loanstatusdate - dateapproved - ethnicity - gender - veteran,
                data = train,
                family = binomial
)
# too big, didn't return result
# trying model with just factor variables (as many as I could fit)


# --------- just factor variables -------------
glm.fits <- glm(fraud ~ loanstatus + hubzoneindicator + ruralurbanindicator + initialapprovalamount + businesstype + businessagdescription,
                data = train, 
                family = binomial)

summary(glm.fits)
coef(glm.fits)
preds <- predict(glm.fits, train, type='response')
print(preds)
library(caret)
caret::confusionMatrix(as.factor(as.integer(preds>0.0005)), as.factor(train[,fraud]), positive='1')

library(pROC)
pROC::roc(preds, train[,fraud])


# residual sum of squares
deviance(glm.fits)


# skinnier?
glm.fac <- glm(fraud ~ lmiindicator + hubzoneindicator + ruralurbanindicator + businesstype + businessagedescription,
                data = train.2, 
                family = binomial)

summary(glm.fac)
preds.fac <- predict(glm.fac, train.2, type='response')


caret::confusionMatrix(as.factor(as.integer(preds.fac>0.0005)), as.factor(train.2[,fraud]), positive='1')

pROC::roc(preds.num, train.2[,fraud])
# --------- just numeric variables -------------
glm.num <- glm(fraud ~ initialapprovalamount + 
                  jobsreported + 
                  utilities_proceed +
                  payroll_proceed +
                  mortgage_interest_proceed +
                  rent_proceed +
                  health_care_proceed +
                  debt_interest_proceed +
                  businesstype,
                data = train, 
                family = binomial)


summary(glm.num)
coef(glm.num)
preds.num <- predict(glm.num, train.2, type='response')


caret::confusionMatrix(as.factor(as.integer(preds.num>0.0005)), as.factor(train.2[,fraud]), positive='1')


pROC::roc(preds.num, train.2[,fraud])



# compute AUC (area under the curve)
# find best cutoff
# confusion_matrix after setting up on a cutoff for the probability of fraud




# NOTE to myself: save, then restart only 03_models by picking up the dataset dloans_m
#Then read ROC, and continue to refine the model and test the confusionMatrix and ROC


# results <- resampleSummary()
# summary(results)
# dotplot(results)

# ROC repROC
# ROC resources: 
# Try LDA and QDA
# plot ROC
# get AUC


pairs(glm.fits)
# ----------------------------------------------------------
## --------- Lasso, Ridge -------------
# ----------------------------------------------------------

# lasso regression
lasso.model <- train(fraud ~ ., data = train, method = 'lasso', na.action = na.exclude)
# cannot get a result, kept getting the following errors:
# Error: vector memory exhausted (limit reached?)
# rsession(84168) MallocStackLogging: can't turn off malloc stack logging because it was not enabled.



#----- another train set -----
saveRDS(train, file = file.path(saveddataPath, "train.step.1.RDS"))
train.2 <- copy(train)[, ":="(originatinglender=NULL, servicinglendername=NULL, dateapproved=NULL, loanstatusdate=NULL)]
rm(train)



train.2[,c("veteran", "gender", "race", "ethnicity")] <- list(NULL)

train.2[train.2$v1 == 2423070, "lmiindicator"] <- "N"
train.2[train.2$v1 == 1391367, "lmiindicator"] <- "Y"
train.2[train.2$v1 == 4446790, "lmiindicator"] <- "Y"
levels(train.2$lmiindicator)[levels(train.2$lmiindicator) == ""] <- "N"
levels(train.2$businessagedescription)[levels(train.2$businessagedescription) == ""] <- "Unanswered"
levels(train.2$businesstype)[levels(train.2$businesstype) == ""] <- "Unknown"

feather::write_feather(train.2, file.path(saveddataPath, "train.2.feather"))

# To recall feather file
testSavedPath <- file.path(saveddataPath, "test.feather")
test.1 <- read_feather(testSavedPath)
test.1 <- data.table(test.1)
test.1[,c("dateapproved", "borrowerstatusdate","servicinglendername", "originatinglender", "loanstatusdate")] <- list(NULL)

test.1[test.1$v1 == 2423070, "lmiindicator"] <- "N"
test.1[test.1$v1 == 1391367, "lmiindicator"] <- "Y"
test.1[test.1$v1 == 4446790, "lmiindicator"] <- "Y"
levels(test.1$lmiindicator)[levels(test.1$lmiindicator) == ""] <- "N"
levels(test.1$businessagedescription)[levels(test.1$businessagedescription) == ""] <- "Unanswered"
levels(test.1$businesstype)[levels(test.1$businesstype) == ""] <- "Unknown"


# now a train and test with no NAs
train.2 <- train.2[complete.cases(train.2)]
test.1 <- test.1[complete.cases(test.1),]
# ----------------------------------------------------------
## --------- classification model -------------
# ----------------------------------------------------------
# KNN
# library(class)
# knn_test_pred <- knn(
#   train = train.2, 
#   test = test.1,
#   cl = train.2$fraud, 
#   k=5
# )

knn_tune <- train(
  fraud ~ ., 
  data = train.2, 
  method = "knn", 
  trControl = trainControl(method = "cv"), 
  tuneGrid = data.frame(k = c(3,5,7))
)
