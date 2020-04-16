#N.B. MASS also has a select() function, so must call dplyr::select()

### table of contents ###
# set up
# forward stepwise selection
# sample call
#########################

### set up ###
library(MASS)
library(readr)
library(tidyverse)
library(ggplot2)
library(GGally)
library(lmtest)

# filePath = '/Users/stevenfriedman/Documents/5205FinalProject'
# setwd(filePath)

###TODO sort out full model, i.e. add log terms (& polynomials if required), drop various terms
# data <- as_tibble(read_csv('streeteasy_cleaned.csv'))
data <- as_tibble(read_csv('streeteasy_combined.csv'))
data <- data %>% dplyr::select(1:6,17:32)
 model <- lm(log(rent)~.-size_sqft + log(size_sqft) - borough,data)
 summary(model)
 
 model_revised <- lm(log(rent)~.-size_sqft + log(size_sqft) - borough -before_war,data)
 model_select <- do_forward_stepwise(model_revised, data, "log(rent)")
 
# Weighted linear regression
wts     <- 1/fitted( lm(abs(residuals(model_select))~fitted(model_select)) )^2
model_weighted    <- lm(log(rent) ~.-size_sqft + log(size_sqft) - borough -before_war,
                         data, weights=wts)
##############

### forward stepwise selection ###
#can use F values directly also
do_forward_stepwise = function(full, data, y_var, alpha_to_enter = 0.01, alpha_to_leave = 0.05){
  #start with no variables
  formula = as.formula(paste(y_var, 1, sep = " ~ "))
  reduced = lm(formula, data=data)
  vars = c()
  adjR2s = c()
  
  while(TRUE){
    added = FALSE
    dropped = FALSE

    # try to add one term
    if(length(vars) < length(full$coefficients) - 1){
      to_add = addterm(reduced, scope=full, test="F")
      #check vs threshold
      # if(max(to_add$`F Value`, na.rm = TRUE) >= F_add_threshold){
      if(min(to_add$`Pr(F)`, na.rm = TRUE) <= alpha_to_enter){
        #select based on F value because p values may be identical if rounded very small
        max_f = to_add[which(to_add$`F Value` == max(to_add$`F Value`, na.rm = TRUE)),]
        if(nrow(max_f) > 1){
          stop("cannot add multiple params in one step")
        }
        param_to_add = rownames(max_f)
        #update var list and model
        cat("adding", param_to_add, "\n")
        vars = append(vars, param_to_add)
        formula = as.formula(paste(y_var,paste(vars, collapse = " + "),sep = " ~ "))
        reduced = lm(formula, data=data)
        adjR2s = append(adjR2s, summary(reduced)$adj.r.squared)
        added = TRUE 
      }
    }
    
    #try to drop one term (procedure calls for dropping highest p val, not all that exceed)
    if(length(vars) > 0){
      to_drop = dropterm(reduced, test="F")
      #check vs threshold
      # if(min(to_drop$`F Value`, na.rm = TRUE) < F_drop_threshold){
      if(max(to_drop$`Pr(F)`, na.rm = TRUE) > alpha_to_leave){
        max_p = to_drop[which(to_drop$`Pr(F)` == max(to_drop$`Pr(F)`, na.rm = TRUE)),]
        param_to_drop = rownames(max_p)
        if(nrow(max_f) > 1){
          stop("Cannot drop multiple params in one step")
        }
        #update var list and model
        cat("dropping", param_to_drop, "\n")
        vars = vars[vars!=param_to_drop]
        formula = as.formula(paste(y_var,paste(vars, collapse = " + "),sep = " ~ "))
        reduced = lm(formula, data=data)
        adjR2s = append(adjR2s, summary(reduced)$adj.r.squared)
        dropped = TRUE 
      }
    }
    #break if stable
    if(!added && !dropped){
      break
    }
  }
  
  plot(x=1:length(adjR2s), y=adjR2s)
  #cat("final model:", "\n")
  return(reduced)
}
##################################

### sample call ###
final_model = do_forward_stepwise(model, data, "log(rent)")
summary(final_model)
anova(final_model)
###################



