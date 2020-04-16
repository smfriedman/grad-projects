library(readr)
library(purrr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(mltools)
library(data.table)
library(GGally)

# Read-in data
streeteasy <- read_csv("./streeteasy.csv")

# Convert borough, submarket, neighborhood to factors
streeteasy <- streeteasy %>% mutate(borough = factor(borough),
                                    submarket = factor(submarket),
                                    neighborhood = factor(neighborhood))

# Add before_war binary variable (=1 if building_age_yrs >= 50, =0 otherwise)
streeteasy <- streeteasy %>% mutate(before_war = as.numeric(building_age_yrs >=50))

# Add extra terms
streeteasy <- streeteasy %>% mutate(amenity_ct = has_elevator + has_doorman + has_washer_dryer + has_dishwasher + has_gym + has_patio)

# Remove outliers (filter out all samples where min_to_subway >= 30)
streeteasy <- streeteasy %>% filter(min_to_subway<30)

# Remove erroneous data (filter out all samples with bathrooms = 0)
streeteasy <- streeteasy %>% filter(bathrooms > 0)

# Remove outlier (filter out top 5% of rent for each bedroom count)
streeteasy <- streeteasy %>% group_by(bedrooms) %>% filter(rent < quantile(rent, .95)) %>% ungroup()

# Remove trial from The Rockaways (only one sample)
streeteasy <- streeteasy %>% filter(submarket != "The Rockaways")
  
# Add region variable (based on neighborhood and submarket)
neighborhoods.to.relabel <- streeteasy %>% 
  group_by(neighborhood) %>%
  count() %>%
  filter(n < 20)

relabeled.neighborhoods <- streeteasy %>% 
  filter(neighborhood %in% neighborhoods.to.relabel$neighborhood) %>%
  mutate(region = paste0("Other", submarket))

unrelabeled.neighborhoods <- streeteasy %>% 
  filter(!(neighborhood %in% neighborhoods.to.relabel$neighborhood)) %>%
  mutate(region = neighborhood)

streeteasy_combined <- rbind(relabeled.neighborhoods, unrelabeled.neighborhoods)%>%
  group_by(region)%>%filter(n() > 20)%>%ungroup()

# Remove some unused variables for model
streeteasy_combined <- streeteasy_combined%>%
  dplyr::select(-c("rental_id", "building_id"))

# Remove whitespaces from region names and convert region to factor
streeteasy_combined <- streeteasy_combined %>%
    mutate(submarket = unlist(map(submarket, function(x) gsub(" ", "", x, fixed = TRUE))))%>%
    mutate(submarket=factor(submarket))

# One-hot encoding of categorical variables

streeteasy_onehot <- streeteasy_combined %>%as.data.table() %>% 
  one_hot(cols="submarket")%>% as_tibble()

# Save data
write_csv(streeteasy_combined, 'streeteasy_combined.csv')
write_csv(streeteasy_onehot, 'streeteasy_cleaned.csv')

## Descriptive statistics
streeteasy_summary <- streeteasy %>% group_by(borough)%>%
  summarize(rent_25 = quantile(rent, .25), mean_rent = mean(rent),
            rent_75 = quantile(rent, .75), mean_bdrm = mean(bedrooms),
            mean_bthrm = mean(bathrooms), sqft_25 = quantile(size_sqft, .25),
            mean_sqft = mean(size_sqft), sqft_75 = quantile(size_sqft, .75))
streeteasy_summary = streeteasy %>% summarize()
## Visualizations

## Pairwise plots
# streeteasy$log_rent = log(streeteasy$rent)
# streeteasy$log_bathrooms = log(streeteasy$bathrooms)
# streeteasy$log_size_sqft = log(streeteasy$size_sqft)
# streeteasy_select <- streeteasy %>% dplyr::select(log_rent, bedrooms, log_bathrooms, log_size_sqft, min_to_subway, floor, building_age_yrs, borough)
streeteasy_select <- streeteasy %>% dplyr::select(rent, bedrooms, bathrooms, size_sqft, min_to_subway, floor, borough)
ggpairs(streeteasy_select, aes(colour=borough, alpha=0.4))
# streeteasy_select <- streeteasy %>% dplyr::select(rent, bedrooms, bathrooms, size_sqft, min_to_subway, floor)
# ggpairs(streeteasy_select[,1:6], aes(alpha=0.4))

# Boxplot: rent by borough
boxplot(rent~borough, streeteasy_select)

# Boxplot: rent by submarket
boxplot(rent~submarket, streeteasy_select)

# Histogram of sqft
ggplot(data = streeteasy,mapping = aes(x=size_sqft))+
  geom_histogram()+
  theme_light()

## QQ plot of sqft
ggplot(data = streeteasy,mapping = aes(sample=size_sqft))+
  geom_qq()+
  geom_qq_line()+
  labs(y="Square footage", title = "QQ-Plot of Square footage")+
  theme_light()

# Histogram of mins to subway
ggplot(data = streeteasy_combined,mapping = aes(x=min_to_subway))+
  geom_histogram()+
  theme_light()
