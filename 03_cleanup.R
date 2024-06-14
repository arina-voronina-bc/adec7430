#==================================
# Cleanup file
# Purpose: Cleanup of merged data and breaking into train and test
#          Paring down dataset variables, cleaning up NA values, converting class types, etc
#==================================




options(scipen=999)


#===========================================
# --------- Cleaning up NA values ----------

# which fields have NA values and how many
sapply(dloans_m, function(x) sum(length(which(is.na(x)))))

# impute missing values of "proceeds" variables with zero - typically they add up to the loan amount, so it's safe to assume NAs = 0
dloans_m$utilities_proceed[is.na(dloans_m$utilities_proceed)] <- 0
dloans_m$payroll_proceed[is.na(dloans_m$payroll_proceed)] <- 0
dloans_m$mortgage_interest_proceed[is.na(dloans_m$mortgage_interest_proceed)] <- 0
dloans_m$rent_proceed[is.na(dloans_m$rent_proceed)] <- 0
dloans_m$health_care_proceed[is.na(dloans_m$health_care_proceed)] <- 0
dloans_m$debt_interest_proceed[is.na(dloans_m$debt_interest_proceed)] <- 0

#=============================================
# --------- Reclassifying Data Type ----------
dloans_m[, (.N), by = loanstatus]
dloans_m[, (.N), by = ruralurbanindicator]
dloans_m[, (.N), by = hubzoneindicator]
dloans_m[, (.N), by = lmiindicator]
dloans_m[, (.N), by = businessagedescription]
dloans_m[, (.N), by = businesstype]
dloans_m[, (.N), by = nonprofit]



sapply(dloans_m, class)
cols <- c("loanstatus", "ruralurbanindicator", "hubzoneindicator", 
          "lmiindicator", "businessagedescription", "businesstype", 
          "nonprofit", "ethnicity", "race", "gender", "veteran")
dloans_m <- dloans_m %>% mutate_at(cols, factor)

# there were 3 rows whose factor value's name was just "", so imputing (looked up specific loan online)
dloans_m[dloans_m$uid == 2423070, "lmiindicator"] <- "N"
dloans_m[dloans_m$uid == 1391367, "lmiindicator"] <- "Y"
dloans_m[dloans_m$uid == 4446790, "lmiindicator"] <- "Y"
levels(dloans_m$nonprofit)[levels(dloans_m$nonprofit) == ""] <- "N"

# convert a couple of vars to numeric
dloans_m$initialapprovalamount <- as.numeric(dloans_m$initialapprovalamount)
dloans_m$jobsreported <- as.numeric(dloans_m$jobsreported)


#=============================================
# ---------Making Train and Test Set----------


train <- dloans_m %>% dplyr::sample_frac(0.70)
train[,c("indictments", "uid", "amount_stolen", "borrower", "loannumber", "nonprofit", "forgivenessamount")] <- list(NULL)
test  <- dplyr::anti_join(dloans_m, train, by = 'uid')


# some cleanup of NA values
train$utilities_proceed[is.na(train$utilities_proceed)] <- 0
train$payroll_proceed[is.na(train$payroll_proceed)] <- 0
train$mortgage_interest_proceed[is.na(train$mortgage_interest_proceed)] <- 0
train$rent_proceed[is.na(train$rent_proceed)] <- 0
train$health_care_proceed[is.na(train$health_care_proceed)] <- 0
train$debt_interest_proceed[is.na(train$debt_interest_proceed)] <- 0
sort(sapply(train, function(x) sum(length(which(is.na(x))))), decreasing = TRUE)
#save train and test sets
feather::write_feather(train, file.path(saveddataPath, "train.feather"))
feather::write_feather(test, file.path(saveddataPath, "test.feather"))
