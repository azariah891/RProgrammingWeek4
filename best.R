library(tidyverse)

best <- function(state, outcome) {
        ## Read outcome data
        outcome_df <- read.csv("outcome-of-care-measures.csv", header = TRUE)
        list_causes <- list("heart attack" = names(outcome_df[11]), 
                            "heart failure" = names(outcome_df[17]), 
                            "pneumonia" = names(outcome_df[23])
                            )
        
        ## Coerce to numeric since read as character
        for (i in list_causes) {
                outcome_df[, i] <- as.numeric(outcome_df[, i])
        }
        
        ## Check that state and outcome are valid
        if (!state %in% outcome_df$State) stop("invalid state")
        if (!outcome %in% names(list_causes)) stop ("invalid outcome")
  
        
        ## New filtered df with state and ordered 30-day death rate
        ## !!sym used in line 27 to handle data masking/ tidy evaluation
        ordered_df <- {
                outcome_df %>% 
                            filter(State == state) %>%
                            arrange(!!sym(list_causes[[outcome]]))
        }
      
        ## Return hospital name in that state with lowest 30-day death rate
        ordered_df[1,"Hospital.Name"]
        
        
}