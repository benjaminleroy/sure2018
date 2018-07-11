library(reshape2)
library(tidyverse)
library(mice)


source(paste(sure_path, "/functions/mice_new.R", sep = ""))
source(paste(sure_path, "/functions/fill_in_nas.R", sep = ""))
source(paste(sure_path, "/functions/some_testing_functions.R", sep = ""))

#put name of data file here:
data_file <- "/big_merge_datasets.Rdata"
load(paste(sure_path, "/data" , data_file, sep = ""))


#some initial clean up of the dataset

#mice doesn't like column names with spaces in them
colnames(big_merge_datasets) <- gsub(' ', '_', colnames(big_merge_datasets))

#parentheses makes R try to do a function call
colnames(big_merge_datasets)[22:23] <- c("Country_of_Exploit_2L", "Country_of_Citizenship_2L")


#we won't be using these columns:
names_exclude <- c("majorityStatus", "majorityStatusAtExploit", "majorityEntry",
                   "Country_of_Exploit_2L",   
                   "Country_of_Citizenship_2L",
                   "Datasource",
                   "Country_of_Exploitation.y",
                   "Country_of_Exploitation.x",
                   "Country_Code.y",
                   "Country_of_Citizenship.y",
                   "Country_of_Citizenship.x",
                   "Country_Code",
                   "Year")

#these columns don't have a single one, i.e. these indicators are never true,
# so we removed them along with the names_exclude
cols_to_remove <- c("isForcedMilitary","isOrganRemoval","typeOfLabourIllicitActivities",
                    "typeOfLabourMiningOrDrilling","typeOfLabourTransportation",
                    "typeOfSexRemoteInteractiveServices", names_exclude)


big_merge_datasets[, cols_to_remove] <- NULL

#furthermore the variable isSexAndLabour has a 1 in 23 instances and in each of them
#both isForcedLabour and isSexualExploit have a 1 as well
big_merge_datasets[,"isSexAndLabour"] <- NULL




#filling in some of the NAs with zeros
for(i in list(17:33, 35:38, 39:47, 49:51, c(52, 54:57))){
  big_merge_datasets <- fill_in_nas(big_merge_datasets, i)
}


#converts year and ageBroad into ordered factors
for(i in c("Citz_Tier", "Exp_Tier", "ageBroad")){
  big_merge_datasets[,i] <- as.ordered(big_merge_datasets[,i])
}
big_merge_datasets$ageBroad <- fct_relevel(big_merge_datasets$ageBroad, "0--8", "9--17", 
                                                  "18--20", "21--23", "24--26", "27--29", 
                                                  "30--38", "39--47", "48+")

#converts the columns with characters into unordered factors
factor_cols <- c("gender", "RecruiterRelationship",  
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
  big_merge_datasets[,column] <- as.factor(unlist(big_merge_datasets[,column]))
}

trimmed_dataset <- big_merge_datasets

save(trimmed_dataset, file = paste(sure_path, "/data/trimmed_dataset.Rdata", sep = ""))

#building the blocks
imp_dataset_names<-colnames(trimmed_dataset)

controlgroup <-  grep("meansOfControl+",imp_dataset_names, value=TRUE)
#take out not specified
controlgroup <- controlgroup[-length(controlgroup)]

recruitergroup <- grep("recruiter+", imp_dataset_names, ignore.case = FALSE, value=TRUE)
#removing not specified
recruitergroup <- recruitergroup[-length(recruitergroup)]
recruitergroup <- append(recruitergroup, "isAbduction")


labourgroup <- grep("typeOfLabour+",imp_dataset_names, value=TRUE)
#removing not specified 
labourgroup <- labourgroup[-length(labourgroup)]


sexgroup<-grep("typeOfSex+", imp_dataset_names, ignore.case = FALSE, value=TRUE)


#country of citizenship proxy variables and removing the last one tier:
citz_group <- grep("Citz_+", imp_dataset_names, ignore.case = FALSE, value=TRUE)
citz_group <- citz_group[-length(citz_group)]

#country of exploit proxy variables and removing the last one tier:
exp_group <- grep("Exp_+", imp_dataset_names, ignore.case = FALSE, value=TRUE)
exp_group <- exp_group[-length(exp_group)]

#migration block
migration_group <- grep("Migration_+", imp_dataset_names, ignore.case = FALSE, value=TRUE)

#population block
population_group <- grep("Population_+", imp_dataset_names, ignore.case = FALSE, value=TRUE)


block_list<-list(citz_group, exp_group, "ageBroad", controlgroup, recruitergroup,
                 labourgroup, sexgroup)

save(block_list, file =  paste(sure_path, "/data/block_list.Rdata", sep = ""))
#Creating an ini object to pull the predictorMatrix and method vector
ini <- mice(trimmed_dataset, blocks = block_list, maxit = 0)
pred_matrix <- ini$predictorMatrix
imp_method <- ini$method



#if pmm or polr cause problems consider switching to rf (random forest)
#imp_method[1:3] <- "rf"

#Make sure relationships of interest aren't used in imputation
pred_matrix[, c("isForcedLabour", "isSexualExploit", "isOtherExploit", "isForcedMarriage")] <- 0

#excluding the names_exclude, population and migration columns
pred_matrix[,c(population_group, migration_group)] <- 0

for(i in 4:length(block_list)){
  pred_matrix[i, block_list[[i]]] <- 0
}
save(pred_matrix, file =  paste(sure_path, "/data/pred_matrix.Rdata", sep = ""))
save(imp_method, file =  paste(sure_path, "/data/imp_method.Rdata", sep = ""))