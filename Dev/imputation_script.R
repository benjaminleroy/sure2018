#load everything
sure_path <- getwd()
library(reshape2)
library(tidyverse)
library(mice)

source(paste(sure_path, "/functions/mice_new.R", sep = ""))
source(paste(sure_path, "/functions/fill_in_nas.R", sep = ""))
source(paste(sure_path, "/functions/some_testing_functions.R", sep = ""))

load(paste(sure_path,"/data/imp_method.Rdata", sep = ""))
load(paste(sure_path,"/data/pred_matrix.Rdata", sep = ""))
load(paste(sure_path,"/data/block_list.Rdata", sep = ""))
load(paste(sure_path,"/data/trimmed_dataset.Rdata", sep = ""))

mice_new_run <- mice_new(trimmed_dataset, method = imp_method, blocks = block_list,
                     predictorMatrix = pred_matrix, seed = 1729,
                     m = 5, maxit = 10)

mice_new_obj <- mice_new_cleanup(mice_new_run)