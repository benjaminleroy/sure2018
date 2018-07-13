
# creates a matrix of beta values for a list of glms
#
# Args:
# -----
# glm_list: list of glm models (n models, p = number of features, 
# should be same features in each model)
#
# Returns:
# --------
# matrix (n x p) of beta values of glm models (per row)
beta_matrix <- function(glm_list){
  n <- length(glm_list)
  p <- length(coef(glm_list[[1]]))
  out_mat <- matrix(NA, nrow = n, ncol = p)
  
  colnames(out_mat) <- names(coef(glm_list[[1]]))
  rownames(out_mat) <- 1:n
  
  for (i in seq_len(n)) {
    out_mat[i,] <- coef(glm_list[[i]])
  }
  return(out_mat)
}

# Visualize beta matrix through parallel coordinates
#
# Args:
# ----
# beta_mat <- matrix of beta values (needs column names)
#
# Returns:
# --------
# ggplot
vis_beta_mat <- function(beta_mat){
  beta_df <- data.frame(beta_mat) %>% 
    mutate(idx = 1:nrow(beta_mat))
  vis_df <- beta_df %>% reshape2::melt(id.vars = "idx")
  
  ggout <- ggplot(vis_df, aes(x = variable, y = value, color = factor(idx))) +
    geom_line(aes(group = factor(idx)))
  
  return(ggout)
}