# Setup directories
filePath = '/Users/stevenfriedman/Documents/5205FinalProject'
setwd(filePath)

# Load necessary packages
library('readr')
library('tidyverse')
library('ggplot2')
library(GGally)

# Read in data
streeteasy <- as_tibble(read_csv('streeteasy.csv'))
streeteasy_select <- streeteasy %>% select(rent, bedrooms, bathrooms, size_sqft, building_age_yrs, submarket, borough)
ggpairs(streeteasy_select, aes(colour=borough, alpha=0.4))
ggpairs(streeteasy, aes(colour=borough, alpha=0.4))

ggpairs(streeteasy[,-c(18)],aes(colour=borough, alpha=0.4))

# Boxplot: rent by neighborhood
boxplot(rent~borough, streeteasy_select)

# Boxplot: rent by neighborhood
boxplot(rent~submarket, streeteasy_select)

ggplot(streeteasy_select, mapping = aes(x=bedrooms, y = mean(rent)))+
  geom_col()+
  facet_wrap(~borough)


ggplot(streeteasy, aes(x=2016-building_age_yrs)) + geom_density()
cor(streeteasy$rent, streeteasy$building_age_yrs)

### table of contents ###
# a. residual plots
# b. residual QQ plot
# c. BP test 
# d. ANOVA
#########################

data = streeteasy_select
model = lm(rent~bedrooms+bathrooms+size_sqft+building_age_yrs, data=streeteasy_select)

### a. residual plots ###
print_residual_plots = function(data, model, useggplot = FALSE){
  alpha = 0.4
  if(useggplot){
    print(ggplot() +
      geom_point(alpha=alpha, aes_string(x=model$fitted.values, y=model$residuals, colour=factor(data$borough))) +
      xlab('fitted val') +
      ylab('residual'))
  }else{
    plot(x=model$fitted.values, y=model$residuals, ylab='residual', xlab='fitted val')
  }
  for(col in names(model$coefficients[-c(1)])){
    if(useggplot){
      print(ggplot() +
        geom_point(alpha=alpha, aes_string(x=unlist(data[,c(col)]), y=model$residuals, colour=factor(data$borough))) +
        xlab(col) +
        ylab('residual'))
    }else{
      plot(x=unlist(data[,c(col)]), y=model$residuals, ylab='residual', xlab=col)
    }
  }
}
######################

### b. residual QQ plot ###
print_qq_plot = function(model){
  residuals = rstandard(model)
  qqnorm(residuals)
  qqline(residuals)
  #ggplot() + stat_qq(aes(sample=residuals))
}
########################

### c. BP test ###
#for constancy of error variance
library(lmtest)
print_bp_results = function(model, alpha = 0.05){
  df = length(model$coefficients) - 1
  critical_val = qchisq(1-alpha, df)
  bp = bptest(model, studentize=FALSE)
  if(bp$statistic <= critical_val){
    interp = sprintf("BP statistic %f is less than critical value %f, so accept H0, that error variance is constant across all predictor variables", bp$statistic, critical_val)
  }else{
    interp = sprintf("BP statistic %f is greater than critical value %f, so reject H0 and conclude that there is nonconstancy of error variance with at least on predictor variable", bp$statistic, critical_val)
  }
  cat(
    sprintf("* Critical val is %f for %i degrees of freedom at %f significance level", critical_val, df, alpha),
    paste("*", interp),
    "* R library BP test results:",
    sep="\n"
  )
  bp
}
##################

### d. ANOVA ###
print_anova = function(model){
  anova(model)
}
#############

### model diagnostics sample calls ###
print_residual_plots(data, model)
print_residual_plots(data, model, useggplot = TRUE)
print_qq_plot(model)
print_bp_results(model)
print_anova(model)
######################################




### feature selection
library(MASS)
#can use F values directly
do_forward_stepwise = function(full, data, y_var, alpha_to_enter = 0.10, alpha_to_leave = 0.15){
  #start with no variables
  formula = as.formula(paste(y_var, 1, sep = " ~ "))
  reduced = lm(formula, data=data)
  vars = c()
  
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
        vars = append(vars, param_to_add)
        formula = as.formula(paste(y_var,paste(vars, collapse = " + "),sep = " ~ "))
        reduced = lm(formula, data=data)
        added = TRUE 
      }
    }
      
    #try to drop one term (procedure calls for dropping highest p val, not all that exceed)
    if(length(vars) > 0){
      to_drop = dropterm(reduced, test="F")
      #check vs threshold
      # if(min(to_drop$`F Value`, na.rm = TRUE) < F_drop_threshold){
      if(max(to_drop$`Pr(F)`, na.rm = TRUE) > alpha_to_leave){
        max_p = to_add[which(to_add$`Pr(F)` == max(to_add$`Pr(F)`, na.rm = TRUE)),]
        param_to_drop = rownames(max_p)
        if(nrow(max_f) > 1){
          stop("Cannot drop multiple params in one step")
        }
        #update var list and model
        vars = vars[vars!=param_to_drop]
        formula = as.formula(paste(y_var,paste(vars, collapse = " + "),sep = " ~ "))
        reduced = lm(formula, data=data)
        dropped = TRUE 
      }
    }
    #break if stable
    if(!added && !dropped){
      break
    }
  }
  print("final model:")
  summary(reduced)
}
do_forward_stepwise(model, data, "rent")

# y_var="rent"
# formula = as.formula(paste(y_var, 1, sep = " ~ "))
# reduced = lm(formula, data=data)
# vars = c()
# to_add = addterm(reduced, scope=full, test="F")
# # if(max(to_add$`F Value`, na.rm = TRUE) >= F_add_threshold){
#   max_f = to_add[which(to_add$`F Value` == max(to_add$`F Value`, na.rm = TRUE)),]
#   param_to_add = rownames(max_f)
#   #update var list and model
#   vars = append(vars, param_to_add)
#   formula = as.formula(paste(y_var,paste(vars, collapse = " + "),sep = " ~ "))
#   reduced = lm(formula, data=data)
# # }
# 
#   y_var="rent"
#   formula = as.formula(paste(y_var, 1, sep = " ~ "))
#   reduced = lm(formula, data=data)
#   vars = c()
#   to_add = addterm(reduced, scope=full, test="F")
#   # if(max(to_add$`F Value`, na.rm = TRUE) >= F_add_threshold){
#   max_f = to_add[which(to_add$`F Value` == min(to_add$`F Value`, na.rm = TRUE)),]
#   nrow(max_f)
#   param_to_add = rownames(max_f)
#   #update var list and model
#   vars = append(vars, param_to_add)
#   formula = as.formula(paste(y_var,paste(vars, collapse = " + "),sep = " ~ "))
#   reduced = lm(formula, data=data)
#   # }
  
  

library(nortest)
#anderson-darling normality test
ad.test(residuals)

