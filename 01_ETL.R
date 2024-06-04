

#========== read and save the data for faster reloading

library(rstudioapi)
library(readxl)
library(feather)
library(data.table)

#### Adding borrower industry, merging using loan number
# loansPath <- file.path(rawdataPath, "PPP details.csv")
# loansSavedPath <- file.path(saveddataPath, "ppp_details.feather")
# 
# loans <- fread(loansPath, nrows = Inf)
# loans <- data.table(loans)


#### detailed loans (more columns) ####
rawloansfile <- file.path(rawdataPath, "detailed_loans.csv")
savedloansfile <- file.path(saveddataPath, "detailed_loans.feather")
dloans <- fread(rawloansfile)
dloans <- data.table(dloans)
dloans[, uid := .I]   #to add unique ID

feather::write_feather(dloans, savedloansfile)



#### LOANS  - OLD LOANS DATA - Clean up column names and borrower names ####
# load loans
# loansSavedPath <- file.path(saveddataPath, "ppp_details.feather")
# loans <- feather::read_feather(loansSavedPath)
# loans <- data.table(loans)
# loans2 <- readRDS(file.path(saveddataPath, "ppp_details.rds"))




colnames(dloans) <- tolower(colnames(dloans))
colnames(dloans) <- gsub("\\(|\\)|\\*", "", colnames(dloans))    # Get rid of special characters in column names
#colnames(dloans) <- gsub(" ", "_", colnames(dloans))

names(dloans)[names(dloans) == "borrowername"] <- "borrower"
dloans$borrower <- tolower(dloans$borrower)
dloans$borrower <- gsub("\\. ", "", dloans$borrower)  # Get rid of weird formatting issues in borrower names like ". "
dloans$borrower <- gsub("\\: ", "", dloans$borrower)
dloans$borrower <- gsub("[:;,.]", "", dloans$borrower)
dloans$borrower <- gsub("\\?s", "s", dloans$borrower) # A lot of business names with CHARLIE?S instead of 'S
dloans$borrower <- gsub("\\'", "", dloans$borrower)
#loans$borrower <- gsub("\\?s", "\\'s", loans$borrower)

#== save as RDS file - sloooow but small file
# loansSavedPath <- file.path(saveddataPath, "ppp_details.rds")
# saveRDS(loans, loansSavedPath)
#=== save instead as Feather file - faster, 6x file size
# save again after creating loan column uid and cleaning
feather::write_feather(dloans,savedloansfile)



#==== fraud data =====
fraudPath <- file.path(rawdataPath, "fraud-tracker-v2.xlsx")
fraudSavedPath <- file.path(saveddataPath, "fraud.feather")
   
fraud <- data.table(readxl::read_xlsx(fraudPath) )
fraud[, fuid := .I]


fraud$borrower <- tolower(fraud$borrower)
fraud$borrower <- gsub("[']", "", fraud$borrower)
fraud$borrower <- gsub("[:;,.]", "", fraud$borrower)

# save again
feather::write_feather(fraud, fraudSavedPath)




#==== additional information files======
# for (i in c(1:12)){
#   print(paste0("working on ", i))
#   rawfile <- file.path(rawdataPath, paste0("public_up_to_150k_",i,".csv"))
#   savedfile <- file.path(saveddataPath, paste0("public_up_to_150k",i,'.feather'))
#   rawf <- fread(rawfile)
#   feather::write_feather(rawf, savedfile)
# }

# rawfile150plus <- file.path(rawdataPath, "public_150k_plus.csv")
# plusSavedPath <- file.path(saveddataPath, "public_150k_plus.feather")
# 
# rawfplus <- fread(rawfile150plus, nrows = Inf)
# loans <- data.table(rawfplus)

