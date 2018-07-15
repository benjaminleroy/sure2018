sure_path <- getwd()
load(paste(sure_path, "/data/mice_new_obj_marginal.Rdata", sep = ""))
library(tidyverse)
source(paste(sure_path, "/functions/mice_glm_function.R", sep = ""))
source(paste(sure_path, "/functions/beta_functions.R", sep = ""))


### Building the regression formula
load(paste(sure_path, "/data/mice_new_obj.Rdata", sep = ""))
column_names <- colnames(mice_new_obj[['data_list']][[10]][[1]][['data']])
rm("mice_new_obj")
citz_features <- grep("Citz_+", column_names, value = TRUE) %>% paste(collapse = " + ")
exp_features <- grep("Exp_+", column_names, value = TRUE)[] %>% paste(collapse = " + ")
meansOfControl <- grep("meansOfControl+", column_names, value = TRUE)[-18] %>% paste(collapse = " + ")
migration <- grep("Migration_+", column_names, value = TRUE) %>% paste(collapse = " + ")
populationgroup <- grep("Population_+", column_names, value = TRUE) %>% paste(collapse = " + ")
RecruiterRelationship <- c(grep("recruiterRelation+", column_names, value = TRUE)[-5], 
                           "isAbduction") %>% paste(collapse = " + ")

imp_formula <- paste("isForcedLabour ~ gender + ageBroad", citz_features, exp_features,
                     meansOfControl, migration, populationgroup, RecruiterRelationship,
                    sep = " + ") %>% as.formula()

imp_formula_interaction <- paste("isForcedLabour ~ gender*ageBroad", citz_features, exp_features,
                     meansOfControl, migration, populationgroup, RecruiterRelationship,
                     sep = " + ") %>% as.formula()

working_mno <- mice_new_obj_marginal

working_mno <- bind_country_cols(working_mno, 10, 5, sure_path = sure_path)


#Fitting glm model
data_list <- mice_sep(working_mno, 10, 5, stratify = TRUE, 
                      strat_vars = c("gender", "ageBroad", "Country_of_Exploit_2L",
                                     "Country_of_Citizenship_2L"))

glm_list <- mno_regression(data_list[["data_corrected"]][["training"]], 
                           imp_formula_interaction, glm, family = binomial)


###Building a composite variable LabourOrSex from isForcedLabour and isSexualExploit
for(i in 1:5){
  holder_df <- data_list[["data_corrected"]][["training"]][[i]]
  
  holder_df$LabourOrSex <- NA
  
  holder_df[holder_df$isForcedLabour == 1 & !is.na(holder_df$isForcedLabour),"LabourOrSex"] <- 1
  holder_df[holder_df$isSexualExploit == 1 & !is.na(holder_df$isSexualExploit),"LabourOrSex"] <- 0
  
  data_list[["data_corrected"]][["training"]][[i]] <- holder_df
}

#Refitting the glm model with a new formula:
imp_formula_interaction2 <- paste("LabourOrSex ~ gender*ageBroad", citz_features, exp_features,
                                 meansOfControl, migration, populationgroup, RecruiterRelationship,
                                 sep = " + ") %>% as.formula()

glm_list2 <- mno_regression(data_list[["data_corrected"]][["training"]], 
                            imp_formula_interaction2, glm, family = binomial)
