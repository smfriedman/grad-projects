### table of contents ###
# set up
# a. residual plots
# b. residual QQ plot
#     TODO formalize normality test
# c. BP test 
# d. ANOVA
# sample calls
#########################

### set up ###
library(readr)
library(tidyverse)
library(ggplot2)
library(GGally)
library(lmtest)
library(nortest)

# filePath = '/Users/stevenfriedman/Documents/5205FinalProject'
# setwd(filePath)

# data = as_tibble(read_csv('streeteasy_cleaned.csv'))
data <- as_tibble(read_csv('streeteasy_combined.csv'))
model = lm(log(rent)~bedrooms + min_to_subway + borough + bathrooms+floor + log(size_sqft) + submarket, data)

#in order to examin residuals above or below a threshold
# low_e = data %>% 
#         mutate(e = model$residuals, e_std = rstandard(model)) %>% 
#         filter(e_std < -1.5) %>% 
#         arrange(desc(e_std) )
# 
# high_e = data %>%      
#   mutate(e = model$residuals, e_std = rstandard(model)) %>% 
#   filter(e_std > 2) %>% 
#   arrange(desc(e_std) )


##############

### a. residual plots ###
print_residual_plots = function(mdl, useggplot = FALSE){
  alpha = 0.4
  if(useggplot){
    fig <- ggplot(mdl) +
            geom_point(aes(x=fitted_values, y=resids,color=borough), alpha=alpha)+
            labs(x='fitted values', y= 'residuals')+theme_light()
    
    ggsave('./Plots/residuals_vs_fittedvals.png',
           plot=fig,width=5,height=5)
  }else{
    plot(x=mdl$fitted.values, y=mdl$residuals, ylab='residual', xlab='fitted val')
  }
  n <- ncol(mdl)
  for(col in 2:(n-2)){
    if(useggplot){
      fig <- ggplot(mdl) +
              geom_point(aes_string(x=names(mdl)[col],y="resids",
                                    color="borough"), alpha=alpha)+
        labs(x=names(mdl)[col],y='residuals')+ theme_light()
      
      ggsave(paste0('./Plots/residuals_vs_',names(mdl)[col],'.png'),
             plot=fig, width=5, height=5)
    }else{
      plot(x=unlist(data[,c(col)]), y=mdl$residuals, ylab='residual', xlab=col)
    }
  }
}
######################

### b. residual QQ plot ###
print_qq_plot = function(model){
  residuals = rstandard(model)
  fig <- ggplot(tibble(resids=residuals), aes(sample=resids))+
    stat_qq()+
    geom_qq_line()+
    labs(x = "Expected", y = "Residuals",
         title="Residual QQ-Plot")
  ggsave('./Plots/residual_qqplot.png',
         plot=fig, width=5, height=5)
}
########################

### c. BP test ###
#for constancy of error variance
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
################

### model diagnostics sample calls ###
#works if borough is included
model_tibble = data%>%mutate(fitted_values = model$fitted.values, resids = model$residuals)
print_residual_plots(model_tibble, useggplot = TRUE)
# print_residual_plots(model_tibble)
print_qq_plot(model)
print_bp_results(model)
print_anova(model)
######################################

#test for normality
ad.test(rstandard(model))
ks.test(rstandard(model), "pnorm",mean=0,sd=1, alternative="two.sided")

