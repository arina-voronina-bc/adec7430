#========================================
# Purpose: match fraud within loans data
#========================================

#==== orient the script on the project ====
library(rstudioapi)
dirpath <- dirname(rstudioapi::documentPath())
# load _Setup
source(file.path(dirpath, "_Setup.R"))
#========================================

#load detailed loans data
savedloansfile <- file.path(saveddataPath, "detailed_loans.feather")
dloans <- fread(rawloansfile)
dloans <- data.table(dloans)
#then save again after cleaning
feather::write_feather(dloans, savedloansfile)

# load fraud
fraudSavedPath <- file.path(saveddataPath, "fraud.feather")
fraud <- feather::read_feather(fraudSavedPath)
fraud <- data.table(fraud)
fraud[, fuid := .I]

#====== clean-up considerations ========
# # example: Aarox, Inc; AACIIS Services; eros productions; booktech enterprises; tech 1 engineering 
# bnames <- tolower(loans$Borrower)
# bnames[grepl('aarox',bnames)]
# bnames[grepl('aaci',bnames)]
# loans[grepl('tianna hart',loans)]
#=======================================

# match
# first attempt direct merge
#make skinny version of *NEW loans data* (more explanatory columns) to merge
dloans_s <- subset(dloans, select = -c(undisbursedamount, franchisename, 
                                       servicinglenderstate, congressdistrict, 
                                       refinance_eidl_proceed, originatinglenderstate,
                                       sbaguarantypercentage, currentapprovalamount))
rm(dloans)
gc()


# save
feather::write_feather(dloans_s, file.path(saveddataPath, "dloans_s.feather"))

# dloans_s <- loans[, list(uid, borrower, InitialApprovalAmount, 
#                          LoanStatus, DateApproved, ServicingLenderName, 
#                          RuralUrbanIndicator, BorrowerState, 
#                          BusinessAgeDescription,)]


# now let's try to call a loan by business name - one we know committed fraud
# dloans_s[grepl('4health',dloans_s)]
#make skinny version of fraud
fraud_s <- subset(fraud, select = -c(summary, healthcare_billing, matched, shell, real_mis, false_id, secondary))
fraud_s <- fraud_s[!is.na(borrower)]
rm(fraud)
#loans_s[, business := gsub('.','', gsub(',','',gsub('-',' ',business, fixed = T), fixed = T), fixed = T)]

#======================================================================================
#==== Investigate matches - both one-to-many, and missings, to improve matching =======
#LMI: low and moderate income communities
# now to merge the two
fraud_m <- merge(fraud_s, dloans_s[, list(uid, 
                                          borrower, 
                                          dateapproved, 
                                          initialapprovalamount, 
                                          lmiindicator, 
                                          hubzoneindicator,
                                          ruralurbanindicator,
                                          businesstype,
                                          originatinglender,
                                          race,
                                          jobsreported,
                                          utilities_proceed,
                                          payroll_proceed,
                                          mortgage_interest_proceed,
                                          rent_proceed,
                                          health_care_proceed,
                                          debt_interest_proceed,
                                          businessagedescription,
                                          ethnicity,
                                          servicinglendername)], by = 'borrower', all.x = T)

# ----Merged fraud data cleanup----
cols <- c("ruralurbanindicator", "hubzoneindicator", "ethnicity", "race",
          "lmiindicator", "businessagedescription", "businesstype", "originatinglender", "servicinglendername")
fraud_m <- fraud_m %>% mutate_at(cols, factor)
fraud_m$initialapprovalamount <- as.numeric(fraud_m$initialapprovalamount)
fraud_m$amount_stolen <- as.numeric(fraud_m$amount_stolen)
# ----Looking for matches----
# find the ones matched when uid (a business name and how often it shows up) is not null (note - there may be multiple ones, so needs manual review)
# the ones non-matches are the ones with uid null
fraud_m[!is.na(uid)][, (.N), by = fuid]
# top example of one-to-many matches
fraud_m[fuid==467]
# these are borrower names which were not matched, even though they were entered manually to match with loans
# return here to clean them as time allows
fraud_m[is.na(uid)][,borrower]


#loans_s[grepl('zippy',borrower)]
#== changes identified here will be implemented in Fraud in Excel, loaded in 01_ETL, saved, loaded above, iterate to see improved matching
#=======================================================================================

# prepare a copy of fraud that only has: borrower (unique), number of indictments, min(amount_stolen) - 
# because of the excel copy paste mistake with 12000, 12001, 12002..

fraud_rm <- merge(fraud_s, fraud_m[,list(borrower,initialapprovalamount)], by = 'borrower', all.x = T)
fraud_rm <- fraud_rm[, list(indictments = .N, amount_stolen = min(amount_stolen)), by = borrower]


# what we really need is an indicator of fraud at the loan level
dloans_m <- merge(dloans_s, fraud_rm, by = 'borrower', all.x = T)
dloans_m <- data.table(dloans_m)
dloans_m[, fraud := as.integer(!is.na(indictments))]
dloans_m[is.na(amount_stolen), amount_stolen:=0]
dloans_m[is.na(amount_stolen), amount_stolen:=0]
rm(dloans_s)

gc()


#==== investigate one-to-many matches ====
dloans_m[, (.N), by = uid][V1>1, ][order(-V1)]
# look at one of the uids with multiple records (duplicated post-merge)
dloans_m[uid == 6243622] # there may be nothing to look at - great!
fraud_s[fuid %in% c(888, 893, 894)]
# CONCLUSION: no duplication, which is kind of incredible... perhaps unbelievable. warrants further scrutiny.
# TO DO: potentially add fraud rows to help additional matches (e.g. stephen bennett)
#===========================================
feather::write_feather(dloans_m, file.path(saveddataPath, "dloans_m.feather"))
feather::write_feather(fraud_s, file.path(saveddataPath, "fraud_s.feather"))
feather::write_feather(fraud_m, file.path(saveddataPath, "fraud_m.feather"))





