library(mice)
library(tidyverse)
# functions not really in order - see the bottom for example of running the 
# only really import function (mice_new).
#
# runs smoothly (though I need to do a few more checks if I'm a good person)
# ~ 11:42 pm, 6 July 2018



# inner function that runs a full step of imputation on all desired variables
# allows for raw data and "corrected" data to be used
# randomizes order of block analysis
#
# Args:
# -----
# data = data frame (n x p) with NAs
# data_corrected = data frame (n x p) with similar structure to data, but some 
#   NAs are expected to be filled in
# block = list of blocks (same as in mice::mice)
# predictorMatrix = predictor matrix (same as in mice::mice)
# method = vector of methods for each block element
# 
# verbose = logical statement if report processing for each variable to impute
#
# Returns:
# --------
# out = a list that has same information as a mice::mids object, see names 
#     and mice::mice()
mice_problem_na <- function(data, data_corrected = correct_data(data),
                            blocks = mice::make.blocks(data),
                            predictorMatrix = mice::make.predictorMatrix(
                              data = data),
                            method = NULL, verbose = TRUE){
  
  pMat_and_methods <- get_methods_for_full_block(predMatrix = predictorMatrix,
                                                 data = data,
                                                 blocks = blocks,
                                                 block_methods = method)
  
  large_pMat <- pMat_and_methods[[2]]
  full_methods <- pMat_and_methods[[1]]
  
  num_vars <- nrow(large_pMat)
  var_names <- rownames(large_pMat)
  
  #storage_df <- data
  where <- is.na(data)
  nmis <- data %>% sapply(function(x) sum(is.na(x)))
  
  imp <- list()
  formulas <- list()
  visitSequence <- c()
  blots <- list()
  chainMean <- array(dim = c(length(unlist(blocks)),1,1))
  dimnames(chainMean) <- list(unlist(blocks),"1", "Chain 1")
  chainVar <- array(dim = c(length(unlist(blocks)),1,1))
  dimnames(chainVar) <- list(unlist(blocks),"1", "Chain 1")
  
  num_blocks <- length(blocks)
  block_order <- sample(num_blocks)
  for (b_idx in block_order) {
    block = blocks[[b_idx]]
    inner_order <- sample(length(block))
    
    finish_iter <- 1 # tells us when the block is done
    
    block_data <- data[, unlist(block)] %>% data.frame
    names(block_data) <- block
    block_data_corrected <- data_corrected[, unlist(block)] %>% data.frame
    names(block_data_corrected) <- block
    
    for (i_idx in inner_order) {
      y_name <- block[i_idx]
      x_names <- colnames(large_pMat)[large_pMat[y_name,] == 1]
      
      y <- data[,y_name]
      x <- data_corrected[,x_names]
      ry <- !is.na(y)
      keep <- remove_lindep(x = x,y = y,ry = ry)
      if (verbose) {
        cat(paste0(y_name,": dropped ",
                     as.character(round(mean(!keep)*100),1),"% \n"))
      }
      
      large_pMat[y_name,][large_pMat[y_name,] == 1][!keep] <- 0
      
      inner_df <- data_corrected
      inner_df[,y_name] <- data[,y_name]
      
      inner_block <- list(y_name)
      inner_pMat <- large_pMat[y_name,] %>% matrix %>% t
      colnames(inner_pMat) <- colnames(large_pMat)
      rownames(inner_pMat) <- y_name

      if (y_name == "majorityStatusAtExploit") {
        #browser()
      }
      
      single_mice_run <- try(mice(m = 1, maxit = 1, data = inner_df, 
                              predictorMatrix = inner_pMat,
                              blocks = inner_block,printFlag = FALSE,
                              method = full_methods[y_name]))
      
      if (class(single_mice_run) == "try-error" &
          full_methods[y_name] == "pmm") {
        cat("error in running pmm, switch to rf")
        single_mice_run <- mice(m = 1, maxit = 1, data = inner_df, 
                                    predictorMatrix = inner_pMat,
                                    blocks = inner_block,printFlag = FALSE,
                                    method = "rf")
      }
      imp[[y_name]] <- single_mice_run$imp[[y_name]]
      formulas[[y_name]] <- single_mice_run$formulas[[y_name]]
      visitSequence <- c(visitSequence, (1:length(var_names))[var_names == y_name])
      blots[[y_name]] <- list()
      chainMean[y_name,1,1] <- single_mice_run$chainMean[y_name,1,1]
      chainVar[y_name,1,1] <- single_mice_run$chainVar[y_name,1,1]
      

      
      # preparing to update data
      block_data[,y_name] <- mice::complete(single_mice_run)[,y_name]
      non_na_logic <- !is.na(block_data[,y_name])
      block_data_corrected[, y_name][non_na_logic] <- block_data[,y_name][non_na_logic]
      
      
      if (finish_iter == length(inner_order)) {
        # when block analysis is finished
        # update data and data_corrected
        data[,unlist(block)] <- block_data
        data_corrected[,unlist(block)] <- block_data_corrected
      }
      finish_iter <- finish_iter + 1
    }
  }
  
  out <- list(data = data,
              data_corrected = data_corrected,
              imp = imp,
              m = 1,
              where = where,
              blocks = var_names,
              call = "",
              nmis = nmis,
              method = full_methods,
              predictorMatrix = large_pMat,
              visitSequence = visitSequence,
              formulas = formulas,
              post = single_mice_run$post,
              blots = blots,
              seed = NA,
              iteration = 1,
              lastSeedValue = NA,
              chainMean = chainMean,
              chainVar = chainVar,
              loggedEvents = NULL,
              version = single_mice_run$version,
              date = single_mice_run$date)
  
  class(out) <- "mids"
  
  return(out)
}

# function of "correct" data, but filling in NA values
#
# Args:
# -----
# data = data frame (n x p) with NA values
# option = string, either "NAlevel" or "empirical" - see Details
# ... = other parameters (the seed = 1 to pass into the "empirical" method)
#
# Returns:
# --------
# corrected_data = data frame (n x p) with some if not all NA values removed 
#     (same basic matrix as data)
#
# Description:
# ------------
# For the `option` parameter, 
# "NAlevel": for factor variables we introduce a new "NA" level
# "emprical": draw from the marignal empirical distribution of non "NA" and fill
#     the NA values in
correct_data <- function(data, option = "NAlevel", ...){
  if (option == "NAlevel") {
    return(correct_data_NAlevel(data))
  } else if (option == "empirical") {
    return(correct_data_Marginalsample(data,...))
  } else{
    stop("option must be either 'NAlevel' or 'empirical'")
  }
} 


# inner function - 
# for factor variables we introduce a new "NA" level (see correct_data function)
#
# Args:
# -----
# data = data frame (n x p) with NA values
#
# Returns:
# --------
# corrected_data = data frame (n x p) with some if not all NA values removed 
#     (same basic matrix as data)
correct_data_NAlevel <- function(data){
  data_corrected <- data
  factor_logic <- sapply(data_corrected, function(x){
    any(class(x) %in% c("factor", "character"))
  })
  for (col_idx in (1:ncol(data_corrected))[factor_logic]) {
    col <- data_corrected[, col_idx]
    has_na <- sum(is.na(col)) > 0
    if (has_na) {
      col2 <- col %>% factor
      levels(col2) = c(levels(col2), "NA")
      col2[is.na(col)] <- "NA"
      col <- col2
    }
    data_corrected[,col_idx] <- col
  }
  return(data_corrected)
}

# inner function - 
# Draws from the marignal empirical distribution of non "NA" and fill
#      the NA values in
# Args:
# -----
# data = data frame (n x p) with NA values
# seed = integer value to be used in set.seed(seed)
#
# Returns:
# --------
# corrected_data = data frame (n x p) with some if not all NA values removed 
#     (same basic matrix as data)
correct_data_Marginalsample <- function(data, seed = 1){
  data_corrected <- data
  
  factor_logic <- sapply(data_corrected, function(x){
    any(class(x) %in% c("factor", "character", "integer"))
  })
  
  integer_logic <- sapply(data_corrected, function(x){
    any(class(x) %in% c("integer"))
  })
  
  # factor or character
  for (col_idx in (1:ncol(data_corrected))[factor_logic]) {
    col <- data_corrected[, col_idx]
    num_na <- sum(is.na(col)) 
    if (num_na > 0) {
      if (!integer_logic[col_idx]) { # only do for factor and character
        col2 <- col %>% factor
      }
      col2_prop <- table(col2)/sum(!is.na(col2))
      col2[is.na(col)] <- sample(names(col2_prop),
                                 size = num_na,
                                 replace = T, 
                                 prob = col2_prop)
      col <- col2
    }
    data_corrected[,col_idx] <- col
  }
  
  # continuous 
  for (col_idx in (1:ncol(data_corrected))[!factor_logic]) {
    col <- data_corrected[, col_idx]
    num_na <- sum(is.na(col)) 
    if (num_na > 0) {
      col2 <- col
      # sampled from empirical distribution
      col2[is.na(col)] <- sample(col2[!is.na(col2)],replace = T, size = num_na)
      col <- col2
    }
    data_corrected[,col_idx] <- col
  }
  
  
  
  return(data_corrected)
}

# tries to remove columns in formula that would cause linear dependence in
# y ~ x.
# This inner function similar to mice::remove.lindep
#
# Args: (same as mice::remove.lindep)
# -----
# x = data frame (n x p ) (note this comes before yx)
# y = imputation vector (n)
# ry = logical vector (n) if y is not missing
# eps = minimum threshold for variance of univariate x (rm.na)
# maxcor = max correlation allowed between y and x (rm.na)
# ... = extra parameters see mice::remove.lindep
#
# Returns:
# --------
# keep = logical vector of columns of x to keep
remove_lindep <- function(x, y, ry, eps = 1e-04, maxcor = 0.99, 
                          allow.na = TRUE, frame = 4, ...) {
  # returns a logical vector of length ncol(x)
  if (ncol(x) == 0)
    return(NULL)
  if (eps <= 0)
    stop("\n Argument 'eps' must be positive.")
  
  # Keep all predictors if we allow imputation of fully missing y
  if (allow.na && sum(ry) == 0) return(rep.int(TRUE, ncol(x)))
  
  xobs <- x[ry, , drop = FALSE]
  yobs <- as.numeric(y[ry])
  if (var(yobs) < eps) return(rep(FALSE, ncol(xobs)))
  xobs <- xobs %>% data.matrix
  keep <- unlist(apply(xobs, 2, var, na.rm = T) > eps)
  keep[is.na(keep)] <- FALSE
  highcor <- suppressWarnings(unlist(apply(xobs, 2, cor, yobs, use = "pairwise.complete.obs") < maxcor))
  highcor[is.na(highcor)] <- FALSE
  keep <- keep & highcor
  
  # no need to calculate correlations, so return
  k <- sum(keep)
  if (k <= 1L) return(keep)  # at most one TRUE
  # correlation between x's
  cx <- suppressWarnings(
    cor(xobs[, keep, drop = FALSE], use = "pairwise.complete.obs")
  )
  cx[is.na(cx)] <- 0
  eig <- eigen(cx, symmetric = TRUE)
  ncx <- cx
  while (eig$values[k]/eig$values[1] < eps) {
    j <- seq_len(k)[order(abs(eig$vectors[, k]), decreasing = TRUE)[1]]
    keep[keep][j] <- FALSE
    ncx <- cx[keep[keep], keep[keep], drop = FALSE]
    k <- k - 1
    eig <- eigen(ncx)
  }
  return(keep)
}

# converts block predictorMatrix to full predictor matrix
# 
# Args:
# -----
# predMatrix = predictorMatrix (from mice::mice) that is assumed to be in
#   block form (a x p)
# data = data frame (n x p) - just for column names
# blocks = list of blocks (from mice::mice), length a
#
# Returns:
# --------
# full predictor matrix (b x p) for all b variables to impute b >= a
convert_block_predMatrix <- function(predMatrix, data, blocks){
  
  num_var <- ncol(data)
  
  fullpMat <- matrix(0, ncol = num_var, nrow = num_var)
  colnames(fullpMat) <- names(data)
  rownames(fullpMat) <- names(data)
  
  b_idx <- 1
  for (block in blocks) {
    if (length(block) != 1) {
      for (variable in block) {
        fullpMat[variable,] = predMatrix[paste0("B",b_idx),]
      }
      b_idx <- b_idx + 1
    }else{
      fullpMat[block,] = predMatrix[block,]
    }
  }
  
  return(fullpMat)
}

# creates a vector with method for each variable from block based method imput
# and also returns a full predictor matrix (see convert_block_predMatrix)
#
# Args:
# -----
# predMatrix = predictorMatrix (from mice::mice) that is assumed to be in
#   block form (a x p)
# data = data frame (n x p) - just for column names
# blocks = list of blocks (from mice::mice), length a
# block_methods = vector of method strings, length a (see mice::mice for method 
# string options)
#
# Returns:
# --------
# list with:
# pMat: full predictor matrix (b x p) for all b variables to impute b >= a
# methods: full length methods vector (length b)
get_methods_for_full_block <- function(predMatrix, data, blocks, 
                                       block_methods = NULL){
  if (is.null(block_methods) | length(block_methods) == 1) {
    block_methods <- mice(data = data, predictorMatrix = predMatrix, 
                          blocks = blocks, maxit = 0, method = block_methods)$method
  }
  fullpMat <- convert_block_predMatrix(predMatrix = predMatrix,data = data, blocks = blocks)
  
  # just in case:
  names(block_methods) <- rownames(predMatrix)
  
  fullmethods <- rep("", nrow(fullpMat))
  names(fullmethods) <- rownames(fullpMat)
  
  b_idx <- 1
  for (block in blocks) {
    if (length(block) != 1) {
      for (variable in block) {
        fullmethods[variable] = block_methods[paste0("B",b_idx)]
      }
      b_idx <- b_idx + 1
    }else{
      fullmethods[block] = fullmethods[block]
    }
    
  }
  return(list(methods = fullmethods, pMat = fullpMat))
}



# preforms multiple imputation (for 1 iteration each)
# allows for raw data and "corrected" data to be used
# randomizes order of block analysis
#
# Args:
# -----
# data = data frame (n x p) with NAs
# data_corrected = data frame (n x p) with similar structure to data, but some 
#   NAs are expected to be filled in
# block = list of blocks (same as in mice::mice)
# predictorMatrix = predictor matrix (same as in mice::mice)
# method = vector of methods for each block element
# seed = for randomization 
# m = number of imputations occuring "concurrently"
# verbose = logical statement if report processing for each variable to impute
#
# Returns:
# --------
# out = a list of lists, each that has same information as a mice::mids object, 
# see names in said lists and mice::mice()
mice_multiple_imputations_one_iter2 <- function(data_list,
                                blocks = mice::make.blocks(data),
                                predictorMatrix = mice::make.predictorMatrix(
                                  data = data),
                                method = NULL,
                                seed = 1,
                                m = 5, verbose = FALSE){
  set.seed(seed)
  
  new_df_list <- list()
  for (iter in seq_len(m)) {
    new_df_list[[iter]] <- mice_problem_na(data = data_list[[iter]]$data,
                              data_corrected = data_list[[iter]]$data_corrected,
                              blocks = blocks,
                              predictorMatrix = predictorMatrix,
                              method = method,
                              verbose = verbose) 
    cat("\n")
  }
  return(new_df_list)
}


# mice (multiple imputation)
# allows for raw data and "corrected" data to be used
# randomizes order of block analysis
#
# Args:
# -----
# data = data frame (n x p) with NAs
# data_corrected = data frame (n x p) with similar structure to data, but some 
#   NAs are expected to be filled in
# block = list of blocks (same as in mice::mice)
# predictorMatrix = predictor matrix (same as in mice::mice)
# method = vector of methods for each block element
# seed = for randomization 
# m = number of imputations occuring "concurrently"
# maxit = maximum number of iterations
# verbose = logical statement if report processing for each variable to impute
#
# Returns:
# --------
# out = a list of list of lists, each that has same information as a mice::mids object, 
# see names in said lists and mice::mice()
# *Note: the nesting is 1) by iteration, 2) by multiple imputation
mice_new <- function(data, data_corrected = correct_data(data),
                     blocks = mice::make.blocks(data),
                     predictorMatrix = mice::make.predictorMatrix(
                       data = data),
                     method = NULL,
                     seed = 1,
                     m = 5, maxit = 5,
                     verbose = FALSE){
 if (ceiling(maxit) != floor(maxit) |
    maxit < 1) {
   stop("maxit needs to be positive real number")
 }
  
  initial_data_storage <- list()
  initial_data_storage[["0"]] <- lapply(1:maxit, function(x){
                                list(data = data,
                                     data_corrected = data_corrected)}) 
  cat("starting mice approach \n")
  for (i in 1:maxit) {
    cat(paste0("iteration: ", i, "\n"))
    initial_data_storage[[as.character(i)]] <- 
      mice_multiple_imputations_one_iter2(
        data_list = initial_data_storage[[as.character(i - 1)]],
        blocks = complete_blocks,
        predictorMatrix = predictorMatrix,
        method = method,
        seed = seed,
        m = m,
        verbose = verbose)
  }
  return(initial_data_storage)
}



### code example: 
# running2e <- mice_new(data = data_proxy,
#                      data_corrected = correct_data(data_proxy,
#                                                    option = "empirical"),
#                             blocks = complete_blocks,
#                             method = complete_methods,
#                             predictorMatrix = complete_pMat,
#                             seed = 4909,
#                             m = 2, maxit = 2, verbose = TRUE) 
