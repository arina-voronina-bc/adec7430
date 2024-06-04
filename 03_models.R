
options(scipen=999)

library(magrittr)

# ---------Cleaning up merged loans data----------
# which fields have NA values and how many
sort(sapply(dloans_m, function(x) sum(length(which(is.na(x))))), decreasing = TRUE)

# reclassifying data type
sapply(dloans_m, class)
cols <- c("loanstatus", "ruralurbanindicator", "hubzoneindicator", 
          "lmiindicator", "businessagedescription", "businesstype", 
          "nonprofit", "ethnicity", "race", "gender", "veteran")
dloans_m %<>% mutate_at(cols, factor)



dloans_m[, (.N), by = loanstatus]
dloans_m[, (.N), by = ruralurbanindicator]
dloans_m[, (.N), by = hubzoneindicator]
dloans_m[, (.N), by = lmiindicator]
dloans_m[, (.N), by = businessagedescription]
dloans_m[, (.N), by = businesstype]
dloans_m[, (.N), by = nonprofit]
# some cleanup with the factors
dloans_m[dloans_m$uid == 2423070, "lmiindicator"] <- "N"
dloans_m[dloans_m$uid == 1391367, "lmiindicator"] <- "Y"
dloans_m[dloans_m$uid == 4446790, "lmiindicator"] <- "Y"
levels(dloans_m$nonprofit)[levels(dloans_m$nonprofit) == ""] <- "N"


dloans_m$initialapprovalamount <- as.numeric(dloans_m$initialapprovalamount)
dloans_m$jobsreported <- as.numeric(dloans_m$jobsreported)

# ---------EDA----------
dloans_m[, (.N), by = gender]
dloans_m[, (.N), by = race]
dloans_m[, (.N), by = businessagedescription]
dloans_m[, (.N), by = fraud]


fraud_m[, (.N), by = race]
fraud_m[, (.N), by = businessagedescription]
fraud_m[, (.N), by = ethnicity]


# what are the top 10 loan servicers that issued the most fraudulent loans?
table_lender <- table(fraud_m$servicinglendername)
plot(sort(table_lender, decreasing=TRUE)[1:10], type = "h", ylab = "Number of fraudulent loans granted", cex.axis = .7)

# what types of 'businesses' do these borrowers take out fraud loans for?
fraud_business <- fraud_m %>% group_by(businesstype) %>% summarise(fraudloans = sum(indicted))

# what about the typical size of the loans the fraudsters take out?
ggplot(fraud_m, aes(x=initialapprovalamount, y = dateapproved, binwidth = 10)) + geom_point()

# ggplot(fraud_m, aes(initialapprovalamount), na.rm = TRUE) + 
#   geom_histogram(bins = 315)




# --------- Logistic -------------
# v1: fraud ~ numeric variables
glm.fits <- glm(fraud ~ loanstatus + hubzoneindicator + ruralurbanindicator + initialapprovalamount + jobsreported,
                data = dloans_m,
                family = binomial
)


# compute AUC (area under the curve)
# find best cutoff
# confusion_matrix after setting up on a cutoff for the probability of fraud


# After some trial and error with running this, I was able to eliminate 
glm.fits <- glm(fraud ~ loanstatus + hubzoneindicator + ruralurbanindicator + initialapprovalamount + jobsreported,
  data = dloans_m,
  family = binomial
)

summary(glm.fits)
coef(glm.fits)
preds <- predict(glm.fits, dloans_m, type='response')
library(caret)
caret::confusionMatrix(as.factor(as.integer(preds>0.0005)), as.factor(dloans_m[,fraud]), positive='1')
library(pROC)
pROC::roc(preds, dloans_m[,fraud])
#I would save, then restart only 03_models by picking up the dataset dloans_m
#Then read ROC, and continue to refine the model and test the confusionMatrix and ROC

# ROC repROC# ROC resources: 
# Try LDA and QDA
# plot ROC
# get AUC
