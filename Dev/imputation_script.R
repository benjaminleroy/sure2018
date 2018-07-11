library(reshape2)
library(tidyverse)
library(mice)

sure_path <- "/Users/shiva/code/CMU/sure2018"
source(paste(sure_path, "/functions/mice_new.R", sep = ""))
source(paste(sure_path, "/functions/fill_in_nas.R", sep = ""))
source(paste(sure_path, "/functions/some_testing_functions.R", sep = ""))

#put name of data file here:
data_file <- "/merged_dataset.Rdata"
load(paste(sure_path, "/data" , data_file, sep = ""))


#some initial clean up of the dataset

cols_to_remove <- c("isForcedMilitary","isOrganRemoval","typeOfLabourIllicitActivities",
                    "typeOfLabourMiningOrDrilling","typeOfLabourTransportation",
                    "typeOfSexRemoteInteractiveServices")

#all the columns in cols_to_remove don't have a single one, i.e. these indicators
#are never true, so we removed them
big_merge_datasets[, cols_to_remove] <- NULL

#furthermore the variable isSexAndLabour has a 1 in 23 instances and in each of them
#both isForcedLabour and isSexualExploit have a 1 as well
big_merge_datasets[,"isSexAndLabour"] <- NULL

#mice doesn't like column names with spaces in them
colnames(big_merge_datasets) <- gsub(' ', '_', colnames(big_merge_datasets))

#parentheses makes R try to do a function call
colnames(big_merge_datasets)[22:23] <- c("Country_of_Exploit_2L", "Country_of_Citizenship_2L")


#filling in some of the NAs with zeros
for(i in list(30:46, 48:51, 52:60, 62:64, c(65, 67:70))){
  big_merge_datasets <- fill_in_nas(big_merge_datasets, i)
}



#building the blocks
imp_dataset_names<-colnames(big_merge_datasets)

controlgroup <-  grep("meansOfControl+",imp_dataset_names, value=TRUE)
#take out not specified
control_not_specified <- controlgroup[length(controlgroup)]
controlgroup <- controlgroup[-length(controlgroup)]

recruitergroup <- grep("recruiter+", imp_dataset_names, ignore.case = FALSE, value=TRUE)
#removing not specified
recruiter_not_specified <- recruitergroup[length(recruitergroup)]
recruitergroup <- recruitergroup[-length(recruitergroup)]
recruitergroup <- append(recruitergroup, "isAbduction")


labourgroup <- grep("typeOfLabour+",imp_dataset_names, value=TRUE)
#removing not specified 
labourgroup_not_specified <- labourgroup[length(labourgroup)]
labourgroup <- labourgroup[-length(labourgroup)]
labourgroup <- append(labourgroup, "isForcedLabour")

sexgroup<-grep("typeOfSex+", imp_dataset_names, ignore.case = FALSE, value=TRUE)
sexgroup<-append(sexgroup, c("isSexualExploit","isForcedMarriage"))

#country of citizenship proxy variables and removing the last one tier:
citz_group <- grep("Citz_+", imp_dataset_names, ignore.case = FALSE, value=TRUE)
citz_group <- citz_group[-length(citz_group)]

#country of exploit proxy variables and removing the last one tier:
exp_group <- grep("Exp_+", imp_dataset_names, ignore.case = FALSE, value=TRUE)
exp_group <- exp_group[-length(exp_group)]

#migration block
migration_group <- grep("Migration_+", imp_dataset_names, ignore.case = FALSE, value=TRUE)


block_list<-list(citz_group, exp_group, migration_group, "ageBroad", controlgroup, 
                 control_not_specified, recruitergroup, recruiter_not_specified,
                 labourgroup, labourgroup_not_specified, sexgroup, "isOtherExploit")



#Turning the indicator variables into factors
big_merge_datasets_factor <- big_merge_datasets

#converts year and ageBroad into ordered factors
big_merge_datasets_factor$Year <- as.ordered(big_merge_datasets$Year)
big_merge_datasets_factor$ageBroad <- as.ordered(big_merge_datasets$ageBroad)
big_merge_datasets_factor$ageBroad <- fct_relevel(big_merge_datasets$ageBroad, "0--8", "9--17", 
                                                  "18--20", "21--23", "24--26", "27--29", 
                                                  "30--38", "39--47", "48+")

#converts the columns with characters into unordered factors
factor_cols <- c("Datasource", "gender", "majorityStatus", "majorityEntry",  
                 "RecruiterRelationship",  
                 "meansOfControlDebtBondage", "meansOfControlTakesEarnings", 
                 "meansOfControlRestrictsFinancialAccess", "meansOfControlThreats", 
                 "meansOfControlPsychologicalAbuse", "meansOfControlPhysicalAbuse", 
                 "meansOfControlSexualAbuse", "meansOfControlFalsePromises", 
                 "meansOfControlPsychoactiveSubstances", "meansOfControlRestrictsMovement", 
                 "meansOfControlRestrictsMedicalCare", "meansOfControlExcessiveWorkingHours", 
                 "meansOfControlUsesChildren", "meansOfControlThreatOfLawEnforcement", 
                 "meansOfControlWithholdsNecessities", "meansOfControlWithholdsDocuments", 
                 "meansOfControlOther", "meansOfControlNotSpecified", "isForcedLabour", 
                 "isSexualExploit", "isOtherExploit","isForcedMarriage", 
                 "typeOfLabourAgriculture", "typeOfLabourAquafarming", "typeOfLabourBegging", 
                 "typeOfLabourConstruction", "typeOfLabourDomesticWork", 
                 "typeOfLabourHospitality", "typeOfLabourManufacturing","typeOfLabourPeddling", 
                 "typeOfLabourOther", "typeOfLabourNotSpecified", "typeOfSexProstitution", 
                 "typeOfSexPornography","typeOfSexPrivateSexualServices", 
                 "recruiterRelationIntimatePartner", "recruiterRelationFriend", 
                 "recruiterRelationFamily", "recruiterRelationOther", 
                 "recruiterRelationUnknown", "isAbduction")

for (column in factor_cols) {
  big_merge_datasets_factor[,column] <- as.factor(unlist(big_merge_datasets[,column]))
}



#Creating an ini object to pull the predictorMatrix and method vector
ini <- mice(big_merge_datasets_factor, blocks = block_list, maxit = 0)
pred_matrix <- ini$predictorMatrix
imp_method <- ini$method

#pmm results in mice trying to solve a computationally singular system and 
#switching to rf (random forest)
imp_method[1:4] <- "rf"

#Make sure relationships of interest aren't used in imputation
pred_matrix[1:3,22:71] <- 0
pred_matrix[-(1:3),-(22:71)] <- 0

for(i in 4:length(block_list)){
  pred_matrix[i, block_list[[i]]] <- 0
}

factor_run <- mice_new(big_merge_datasets_factor, method = imp_method, blocks = block_list, 
                     predictorMatrix = pred_matrix, seed = 1729,
                     m = 5, maxit = 10)

mice_new_factor_obj <- mice_new_cleanup(factor_run)