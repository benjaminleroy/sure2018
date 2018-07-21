library(tidyverse)
library(ROCR)

# info list is a *named* list
# where each entry is a list with vectors: "prob" and a "labels"
create_roc_df <- function(info_list) {
	df <- data.frame(x = -1, y = -1, model = "me") %>%
		mutate(model = as.character(model))

	for (model in names(info_list)) {
		pred <- ROCR::prediction(
  					predictions = info_list[[model]]$prob,
    				labels = info_list[[model]]$labels)
		pref <- ROCR::performance(pred, measure = "tpr", x.measure = "fpr")
		df_inner <- data.frame(x = pref@x.values[[1]],
							   y = pref@y.values[[1]],
							   model = model)

		df <- rbind(df, df_inner)
	}
	df <- df[-1,]

	return(df)
}

# takes dataframe from create_roc_df from above
vis_roc_df <- function(roc_df){
	ggout <- ggplot(roc_df,aes(x = x,y = y, color = model)) + 
	geom_line() + 
	geom_path(data = data.frame(x = c(0,1), y = c(0,1)),
				 aes(x = x, y = y), linetype = "dashed",
				 color = "black") +
 	labs(x = "False Positive Rate", 
 		 y = "True Positive Rate")

 	return(ggout)
}
