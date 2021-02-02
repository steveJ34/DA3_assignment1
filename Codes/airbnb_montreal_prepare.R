#########################################################################################
# CH14B Predicting AirBnB apartment prices: selecting a regression model
# using the airbnb Montreal dataset
#########################################################################################



# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())


library(tidyverse)
library(stargazer)
library(Hmisc)

# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("/Users/steve_j/Documents/CEU /data_analysis/DA_3/assignment_1")

setwd("/Users/steve_j/Documents/CEU /data_analysis/DA_3/assignment_1")

# set data dir, load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")


# data used
source("set-data-directory.R") #data_dir must be first defined #
data_in <- paste(data_dir,"clean/", sep = "/")

use_case_dir <- "regression/"
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)

options(digits = 3)


#-------------------------------------------------------


# Import data
data <- read_csv(paste0(data_in,"airbnb_montreal_cleaned5.csv", sep = ""))


# keep if property type is Apartment, House or Townhouse
table(data$property_type)
data <- data %>%
  filter(property_type %in% c("Entire apartment", "Entire bungalow",                   
                              "Entire condominium", "Entire cottage",                    
                              "Entire guest suite", "Entire guesthouse" ,                
                              "Entire house" ,                     
                              "Entire loft", "Entire place",                      
                              "Entire serviced apartment", "Entire townhouse",                  
                              "Entire villa", "Private room in apartment",         
                              "Private room in bungalow",         
                              "Private room in condominium",      
                              "Private room in cottage","Private room in guest suite","Private room in guesthouse",            
                              "Private room in house", "Private room in loft",             
                              "Private room in serviced apartment",
                              "Private room in townhouse", "Private room in villa",            
                              "Room in aparthotel",       
                              "Room in serviced apartment",
                              "Shared room in apartment", "Shared room in condominium",      
                              "Shared room in guest suite", "Shared room in guesthouse",         
                              "Shared room in house","Shared room in loft", "Shared room in townhouse"))

# rename house types to House 
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Entire house", "House", data$property_type),
    f_property_type = factor(property_type))


data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Entire townhouse", "House", data$property_type),
    f_property_type = factor(property_type))


data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Entire bungalow", "House", data$property_type),
    f_property_type = factor(property_type))

data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Entire condominium", "House", data$property_type),
    f_property_type = factor(property_type))

data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Entire cottage", "House", data$property_type),
    f_property_type = factor(property_type))

data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Entire guesthouse", "House", data$property_type),
    f_property_type = factor(property_type))

data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Entire villa", "House", data$property_type),
    f_property_type = factor(property_type))

data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Private room in bungalow", "House", data$property_type),
    f_property_type = factor(property_type))

data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Private room in condominium", "House", data$property_type),
    f_property_type = factor(property_type))
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Private room in cottage", "House", data$property_type),
    f_property_type = factor(property_type))
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Private room in guesthouse", "House", data$property_type),
    f_property_type = factor(property_type))
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Private room in house", "House", data$property_type),
    f_property_type = factor(property_type))
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Private room in townhouse", "House", data$property_type),
    f_property_type = factor(property_type))
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Private room in villa", "House", data$property_type),
    f_property_type = factor(property_type))
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Shared room in condominium", "House", data$property_type),
    f_property_type = factor(property_type))
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Shared room in house", "House", data$property_type),
    f_property_type = factor(property_type))
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Shared room in guesthouse", "House", data$property_type),
    f_property_type = factor(property_type))
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Shared room in townhouse", "House", data$property_type),
    f_property_type = factor(property_type))


#rename apartment types to Apartment

data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Entire loft", "Apartment", data$property_type),
    f_property_type = factor(property_type))
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Entire apartment", "Apartment", data$property_type),
    f_property_type = factor(property_type))
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Entire guest suite", "Apartment", data$property_type),
    f_property_type = factor(property_type))
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Entire place", "Apartment", data$property_type),
    f_property_type = factor(property_type))
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Entire serviced apartment", "Apartment", data$property_type),
    f_property_type = factor(property_type))
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Private room in apartment", "Apartment", data$property_type),
    f_property_type = factor(property_type))
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Private room in serviced apartment", "Apartment", data$property_type),
    f_property_type = factor(property_type))
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Private room in guest suite", "Apartment", data$property_type),
    f_property_type = factor(property_type))
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Private room in loft", "Apartment", data$property_type),
    f_property_type = factor(property_type))
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Room in aparthotel", "Apartment", data$property_type),
    f_property_type = factor(property_type))
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Room in serviced apartment", "Apartment", data$property_type),
    f_property_type = factor(property_type))
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Shared room in guest suite", "Apartment", data$property_type),
    f_property_type = factor(property_type))
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Shared room in apartment", "Apartment", data$property_type),
    f_property_type = factor(property_type))
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Shared room in loft", "Apartment", data$property_type),
    f_property_type = factor(property_type))
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Entire loft", "Apartment", data$property_type),
    f_property_type = factor(property_type))
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Entire loft", "Apartment", data$property_type),
    f_property_type = factor(property_type))


#filtering hotel rooms from room type
table(data$room_type)
data <- data %>%
  filter(room_type %in% c("Entire home/apt", "Private room", "Shared room"))



#Room type as factor
data <- data %>%
  mutate(f_room_type = factor(room_type))

# Rename room type because it is too long
data$f_room_type2 <- factor(ifelse(data$f_room_type== "Entire home/apt", "Entire/Apt",
                                   ifelse(data$f_room_type== "Private room", "Private",
                                          ifelse(data$f_room_type== "Shared room", "Shared", "."))))


data <- data %>% mutate(Bathtub = ifelse(Bathtub==1 | `Bathtub]`==1 , "1", "0"))
data <- data %>% mutate(`Hangers` = ifelse(`Hangers`==1 | `Hangers]`==1 , "1", "0"))
data <- data %>% mutate(Heating = ifelse(Heating  ==1 | `Heating]`==1 , "1", "0"))
data <- data %>% mutate(Kitchen = ifelse(Kitchen==1 | `Kitchen]` ==1 , "1", "0"))
data <- data %>% mutate(`Hair dryer`= ifelse(`Hair dryer`==1 | `Hair dryer]` ==1 , "1", "0"))
data <- data %>% mutate(`Long term stays allowed`= ifelse(`Long term stays allowed`==1 | `Long term stays allowed]` ==1 , "1", "0"))
data <- data %>% mutate(`Luggage dropoff allowed`= ifelse(`Luggage dropoff allowed`==1 | `Luggage dropoff allowed]` ==1 , "1", "0"))
data <- data %>% mutate(`Microwave`= ifelse(`Microwave`==1 | `Microwave]` ==1 , "1", "0"))



#data <- data %>% mutate(bathrooms = gsub("[^0-9.]", "",as.numeric(bathrooms_text)))

#dropping redundant columns 
drops <- c("Bathtub]", "Hangers]", "Heating]", 
           "Kitchen]", "Hair dryer]", "Long term stays allowed]",
           "Luggage dropoff allowed]", "Microwave]","X", "X1")
data<-data[ , !(names(data) %in% drops)]



#extracting number of bathrooms and converting to numeric 
data$bathrooms <- substr(data$bathrooms_text, 1, 1)
data$bathrooms <- as.numeric(data$bathrooms)

# host neighbourhood as factor
table(data$host_neighbourhood)
# rename to Couch
data <- data %>%
  mutate(
    f_neighbourhood = factor(host_neighbourhood))

#---------------------------------------------------------------------------------------------------------------------------

## Create Numerical variables
data <- data %>%
  mutate(
    usd_price_day = price,
    p_host_response_rate = host_response_rate)
# rename cleaning_fee column
#data <- data %>%
#  rename(usd_cleaning_fee = cleaning_fee)
#-------------------------------------------------------------------

# add new numeric columns from certain columns
numericals <- c("accommodates","bathrooms","review_scores_rating","number_of_reviews",
                "reviews_per_month","minimum_nights","beds")
data <- data %>%
  mutate_at(vars(numericals), funs("n"=as.numeric))


# rename columns so they start with n_ as opposed to end with _n
nnames <- data %>%
  select(ends_with("_n")) %>%
  names()
nnames_i <- match(nnames, colnames(data))
colnames(data)[nnames_i] <- paste0("n_", numericals)


#create days since first review
data <- data %>%
  mutate(
    n_days_since = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                as.Date(first_review ,format="%Y-%m-%d")))
# create dummy vars
dummies <- names(data)[seq(54,100)]
data <- data %>%
  mutate_at(vars(dummies), funs("d"= (.)))
# rename columns
dnames <- data %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(data))
colnames(data)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))
# keep columns if contain d_, n_,f_, p_, usd_ and some others
data <- data %>%
  select(matches("^d_.*|^n_.*|^f_.*|^p_.*|^usd_.*"), price, id,
         neighbourhood_cleansed,room_type,property_type)

# with price info only
data <- data %>%
  drop_na(price)

write_csv(data, paste0(data_out, "airbnb_motreal_workfile2.csv"))



library(skimr)
##################################
# DESCRIBE


N=nrow(data) 
N
#N=13289


#
#####################
### look at price ###
#####################
summary(data$price)
describe(data$price)

data <- data %>%
  mutate(ln_price = log(price))

# Remove extreme values + missing from prices (this case: no missing values )
data <- data %>%
  filter(price <1000)

# Histograms
R_F14_h_lnprice <- ggplot(data, aes(ln_price)) +
  geom_histogram(binwidth = 0.15, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("Count") +
  xlab("Log price") +
  theme_bg()
R_F14_h_lnprice
ggsave(paste0(output, "R_F14_h_lnprice.png"), width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)
cairo_ps(filename = paste0(output, "R_F14_h_lnprice.eps"),
         width = mywidth_small, height = myheight_small, pointsize = 8,
         fallback_resolution = 1200)
print(R_F14_h_lnprice)
dev.off()

### log(price) seems to have a more or less normal distribution 



R_F14_h_price <- ggplot(data, aes(price)) +
  geom_histogram(binwidth = 25, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("count") +
  xlab("Price") +
  theme_bg()
R_F14_h_price
ggsave(paste0(output, "R_F14_h_price.png"), width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)
cairo_ps(filename = paste0(output, "R_F14_h_price.eps"),
         width = mywidth_small, height = myheight_small, pointsize = 12,
         fallback_resolution = 1200)
print(R_F14_h_price)
dev.off()

### With no log transformation the price is skewed heavily to the right 

### Make a decision about transformations 

################################################
# look at some cnts. key vars, functional form #
################################################

## n_accomodates: look at distribution

data %>%
  group_by(n_accommodates) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

# The mean price of accommodation that can fit 16 ppl is approx 400. How about making the cut at 600? 


R_14_s_n_accommodates <- ggplot(data = data, aes(x=n_accommodates, y=price)) +
  geom_point(size=1, colour=color[3], shape=16)+
  ylim(0,800)+
  xlim(0,15)+
  labs(x="Number of people accomodated",y="Price")+
  geom_smooth(method="lm", colour=color[1], se=FALSE)+
  theme_bg()
ggsave(paste0(output, "R_14_s_n_accommodates.png"), width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)
cairo_ps(filename = paste0(output, "R_14_s_n_accommodates.eps"),
         width = mywidth_small, height = myheight_small, pointsize = 12,
         fallback_resolution = 1200)
print(R_14_s_n_accommodates)
dev.off()

# Price of accomodation increases linearly with every additional person accommodated 


# Squares and further values to create
data <- data %>%
  mutate(n_accommodates2=n_accommodates^2, ln_accommodates=log(n_accommodates) ,
         ln_accommodates2=log(n_accommodates)^2)

# Regression 1: ln price and num of accommodates and squares
lm(ln_price ~ n_accommodates + n_accommodates2, data=data)
# Regression 2: ln price and log num of accommodates
lm(ln_price ~ ln_accommodates , data=data)
# Regression 3: ln price and num of accommodates
lm(ln_price ~ n_accommodates, data=data)

# The squared term in lm(formula = ln_price ~ n_accommodates + n_accommodates2, data = data) seems to be redundant. 
# The lm(formula = ln_price ~ ln_accommodates, data = data) makes the most sense if we want to use the log transformstions 
# Ths might not be useful for prediction lm(formula = ln_price ~ n_accommodates, data = data)


## Beds
data %>%
  group_by(n_beds) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())
# maybe best is to have log beds
data <- data %>%
  mutate(ln_beds = log(n_beds))

## bathrooms
ggplot(data, aes(n_bathrooms)) +
  geom_histogram(binwidth = 0.5, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("N of bathrooms") +
  theme_bg()

# Most accommodations have 1 bathroom. Thus making a pools would be a good idea 

# Pool accommodations with 0,1,2,10 bathrooms

data <- data %>%
  mutate(f_bathroom = cut(n_bathrooms, c(0,1,2,10), labels=c(0,1,2), right = F) )

data %>%
  group_by(f_bathroom) %>%
  summarise(mean_price = mean(price), n = n())

# When going from 1 to 2 bathrooms, the mean price doubles 

## Number of reviews
nreview_plot <- data %>%
  filter(n_number_of_reviews <100)

ggplot(nreview_plot, aes(n_number_of_reviews)) +
  geom_histogram(binwidth = 5, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("N of reviews") +
  theme_bg()

# number of reviews is heavily skewed to the right (most apartments have 0 reviews), thus using a log is agood idea 
data <- data %>%
  mutate(ln_number_of_reviews = log(n_number_of_reviews+1))

ggplot(data, aes(ln_number_of_reviews)) +
  geom_histogram(binwidth = 0.5, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("Log N of reviews") +
  theme_bg()
# The distribution would be normal, however there are still lots of zeros. Pooling the reviews would be a good idea 

# Pool num of reviews to 3 categories: none, 1-51 and >51
data <- data %>%
  mutate(f_number_of_reviews = cut(n_number_of_reviews, c(0,1,51,max(data$n_number_of_reviews)), labels=c(0,1,2), right = F))
data %>%
  group_by(f_number_of_reviews) %>%
  summarise(median_price = median(price) ,mean_price = mean(price) ,  n=n())

# It looks like the there is a non-linear relationship between the number of reviews and mean price. Perhaps some of the reviews are not favouralbe? 

### Regressions

# Regression 1: log-price and number of reviews
reg4<-lm(ln_price ~ f_number_of_reviews, data=data)
summary(reg4)
# The R2 sucks plus the error is almost 70% 

# Regression 2: log-price and log number of reviews
reg5<-lm(ln_price ~ ln_number_of_reviews, data=data)
summary(reg5)
# R2 sucks even more and the error is still big 






## Time since
# Create variables, measuring the time since: squared, cubic, logs
data <- data %>%
  mutate(
    ln_days_since = log(n_days_since),
    ln_days_since2 = log(n_days_since)^2,
    ln_days_since3 = log(n_days_since)^3 ,
    n_days_since2=n_days_since^2,
    n_days_since3=n_days_since^3)

# Check the effect
lndays_plot <- data %>%
  filter(data$price<=800, ln_days_since>2)

skimr::skim(data$n_number_of_reviews)
ggplot(data = data, aes(x=n_number_of_reviews , y=price)) +
  geom_point(size=1.5, colour=color[3], shape=4) +
  ylim(60,100)+
  xlim(0,20)+
  geom_smooth(method="loess", colour=color[1], se=F)+
  labs(x="Log number of days since first review",y="Log daily price")+
  theme_bg()
# It looks like there is non linear relationship between the daily price and the number of days that passed since the first review

#-Inf values
#lm(ln_price ~ ln_days_since + ln_days_since2 + ln_days_since3, data=data)

## review score effect
ggplot(data = data, aes(x=n_review_scores_rating , y=price)) +
  geom_point(size=1.5, colour=color[3], shape=4) +
  ylim(0,800)+
  xlim(20,100)+
  geom_smooth(method="loess", colour=color[1], se=F)+
  labs(x="Review score",y="Daily price (USD)")+
  theme_bg()
# There's an almost negative relationship between the review  scores and the daily price up until approximately 95 reviews 
# Let's see what happens if we do log transformations 

# Create log of review scores
data <- data %>%
  mutate(ln_review_scores_rating = log(n_review_scores_rating))
# Regression 1) ln price - num of review scores
lm(ln_price ~ n_review_scores_rating,data=data)

# Regression 2) ln price - log num of review scores
lm(ln_price ~ ln_review_scores_rating,data=data)
# every additional review affects the daily price by about 20%

#leave as is

## minimum nights
lm(ln_price ~ n_minimum_nights,data=data)

# The above doesn't really provide any meaningful, thus let's categorize the number of minimum nights 

# Pool and categorize the number of minimum nights: 1,2,3, 3+

data <- data %>%
  mutate(f_minimum_nights= cut(n_minimum_nights, c(1,2,3,max(data$n_minimum_nights)), labels=c(1,2,3), right = F))

lm(ln_price ~ f_minimum_nights,data=data)

###########################
## look at categoricals  ##
###########################

categoricals <- c("f_property_type", "f_room_type", "f_neighbourhood", "f_minimum_nights")

for (i in 1:length(categoricals)) {
  data %>%
    group_by(get(categoricals[i])) %>%
    summarise(mean_price = mean(price) ,  n=n()) %>%
    print
}

# The price mean is higher or houses 
# The lowest mean price is for private room (maybe because the capacity is smaller), followed by shared room (can accommodate at least 2), and the entire home has the highest mean price 



#####################################

# Change Infinite values with NaNs
for (j in 1:ncol(data) ) data.table::set(data, which(is.infinite(data[[j]])), j, NA)

write_csv(data, paste0(data_out, "airbnb_montreal_workfile_adj2.csv"))

#------------------------------------------------------------------------------------------------

