library(tidyverse)

rankhospital <- function(state, outcome, num = "best") {
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
        
        ##print(num)

        ## New filtered df with state and ordered 30-day death rate
        ordered_df <- {
          outcome_df %>% 
            filter(State == state) %>%
            arrange(Hospital.Name) %>%
            arrange(!!sym(list_causes[[outcome]])) %>%
            filter(!is.na(!!sym(list_causes[[outcome]])))
        }
        
        ##print(tail(select(ordered_df, "Hospital.Name", "State", !!sym(list_causes[[outcome]]))))
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        ##print(length(ordered_df))
        ##print(str(ordered_df))
        if (num == "best") ordered_df[1,"Hospital.Name"]
        else if (num == "worst") ordered_df[nrow(ordered_df),"Hospital.Name"]
        else ordered_df[num, "Hospital.Name"]
}