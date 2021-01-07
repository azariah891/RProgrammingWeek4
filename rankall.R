library(tidyverse)

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcome_df <- read.csv("outcome-of-care-measures.csv", header = TRUE)
  list_causes <- list("heart attack" = names(outcome_df[11]), 
                      "heart failure" = names(outcome_df[17]), 
                      "pneumonia" = names(outcome_df[23])
  )
  list_state <- sort(unique(outcome_df$State))
  return_df <- data.frame(hospital = 1:54, state = list_state, row.names = list_state)
  
  ## Coerce to numeric since read as character
  for (i in list_causes) {
    outcome_df[, i] <- as.numeric(outcome_df[, i])
  }
  ## Check that  outcome is valid
  if (!outcome %in% names(list_causes)) stop ("invalid outcome")
  
  ## For each state, find the hospital of the given rank
  for (i in list_state) {
          ## New filtered df with state and ordered 30-day death rate
          ordered_df <- {
            outcome_df %>% 
              filter(State == i) %>%
              arrange(Hospital.Name) %>%
              arrange(!!sym(list_causes[[outcome]])) %>%
              filter(!is.na(!!sym(list_causes[[outcome]]))) %>%
              select(Hospital.Name, State)
          }
          
          ## update return_df
          return_df[i, 2] <- i
          
          if (num == "best") {
                  return_df[i, 1] <- ordered_df[1,"Hospital.Name"]
          }
          else if (num == "worst") {
                  return_df[i, 1] <- ordered_df[nrow(ordered_df),"Hospital.Name"]
          }
          else return_df[i, 1] <- ordered_df[num, "Hospital.Name"]
          
  }
  
  return_df
  
}