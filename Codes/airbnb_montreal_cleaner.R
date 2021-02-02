# Cleaning London airbnb file
# v.1.2. 2021-01-04 paths changed


# IN data from web
# out: airbnb_montreal_cleaned.csv

#setting working directory
rm(list=ls())

# CHANGE TO YOUR WORKING DIRECTORY
# setwd("")
setwd("/Users/steve_j/Documents/CEU /data_analysis/DA_3/assignment_1")
dir<-"data"

#location folders
data_in  <- paste0(dir,"/raw/")
data_out <- paste0(dir,"/clean/")

library(tidyverse)

# zero step
# not necessary
data<-read.csv(paste0(data_in,"listings.csv"))
drops <- c("host_thumbnail_url","host_picture_url","listing_url","thumbnail_url",
           "medium_url","picture_url","xl_picture_url","host_url","last_scraped",
           "description", "experiences_offered", "neighborhood_overview", "notes", 
           "transit", "access", "interaction", "house_rules", "host_about", "host_response_time", 
           "name", "summary", "space", "host_location", "host_name", "host_verifications", "scrape_id", 
           "bathrooms", "first_review, last_review", "host_id", "calendar_updated", 
           "neighbourhood_group_cleansed","license" )
data<-data[ , !(names(data) %in% drops)]


write.csv(data,file=paste0(data_in,"airbnb_montreal_listing1.csv"))


#####################################

# opening dataset
df<-read.csv(paste0(data_in,"airbnb_montreal_listing1.csv"),
             sep=",",header = TRUE, stringsAsFactors = FALSE)

#drop broken lines - where id is not a character of numbers
df$junk<-grepl("[[:alpha:]]", df$id)
df<-subset(df,df$junk==FALSE)
df<-df[1:ncol(df)-1]

#display the class and type of each columns
sapply(df, class)
sapply(df, typeof)

#####################
#formatting columns


#remove percentage signs
for (perc in c("host_response_rate","host_acceptance_rate")){
  df[[perc]]<-gsub("%","",as.character(df[[perc]]))
}

#remove dollar signs from price variables
for (pricevars in c("price")){
  df[[pricevars]]<-gsub("\\$","",as.character(df[[pricevars]]))
  df[[pricevars]]<-as.numeric(as.character(df[[pricevars]]))
}


#format binary variables
for (binary in c("host_is_superhost","host_has_profile_pic", "host_identity_verified", "has_availability", "instant_bookable")){
  df[[binary]][df[[binary]]=="f"] <- 0
  df[[binary]][df[[binary]]=="t"] <- 1
}

#amenities
df$amenities<-gsub("\\{","",df$amenities)
df$amenities<-gsub("\\}","",df$amenities)
df$amenities<-gsub('\\"',"",df$amenities)
df$amenities<-as.list(strsplit(df$amenities, ","))

#grouping amenities 
#Some amenities are grouped based on similarity e.g. fridges. Others are frouped to indicate a a qulity of the aaprtment e.g. aportments tht ahve cribs and maention baby and kids can be refffered ro as kid friendly 


df <- df %>% 
  mutate(refrigerator = ifelse(str_detect(amenities, "Refrigerator|refrigerator|fridge"), 1, 0)) %>% 
  mutate(tv = ifelse(str_detect(amenities, "TV|tv"), 1, 0)) %>% 
  mutate(air_conditioning = ifelse(str_detect(amenities, "Air conditioning|air conditioning"), 1, 0)) %>% 
  mutate(kid_friendly = ifelse(str_detect(amenities, "Baby|baby|Children|children|Crib|crib"), 1, 0)) %>% 
  mutate(oven = ifelse(str_detect(amenities, "Oven|oven"), 1, 0)) %>% 
  mutate(carbon_monoxide_alarm = ifelse(str_detect(amenities, "Carbon|carbon"), 1, 0)) %>% 
  mutate(wifi = ifelse(str_detect(amenities, "Wifi|wifi"), 1, 0)) %>% 
  mutate(air_conditioning = ifelse(str_detect(amenities, "Air|air|AC|ac"), 1, 0)) %>% 
  mutate(smoke_alarm = ifelse(str_detect(amenities, "Smoke alarm|smoke alarm"), 1, 0)) %>% 
  mutate(dedicated_workspace = ifelse(str_detect(amenities, "Dedicated workspace|dedicated workspace"), 1, 0)) %>% 
  mutate(fire_extinguisher = ifelse(str_detect(amenities, "Fire extinguisher|fire extinguisher"), 1, 0)) %>% 
  mutate(iron  = ifelse(str_detect(amenities, "Iron|iron"), 1, 0)) %>% 
  mutate(parking  = ifelse(str_detect(amenities, "Parking|parking|Garage|garage|carport|Carport"), 1, 0)) %>% 
  mutate(stove  = ifelse(str_detect(amenities, "Stove|stove"), 1, 0)) %>% 
  mutate(pool  = ifelse(str_detect(amenities, "Pool|pool"), 1, 0)) %>% 
  mutate(gym  = ifelse(str_detect(amenities, "Fitness|fitness|Gym|gym"), 1, 0)) %>% 
  mutate(pet_friendly  = ifelse(str_detect(amenities, "Pets|pets|Cat|cat|Dog|dog"), 1, 0)) %>% 
  mutate(bathroom_essentials = ifelse(str_detect(amenities, "Soap|soap|Shampoo|sahmpoo|Conditioner|conditioner|Toiletries|toiletries"), 1, 0)) %>% 
  mutate(sound_system  = ifelse(str_detect(amenities, "Sound|sound"), 1, 0)) %>% 
  mutate(dryer  = ifelse(str_detect(amenities, "Dryer|dryer"), 1, 0)) %>% 
  mutate(fireplace  = ifelse(str_detect(amenities, "Fireplace|fireplace"), 1, 0)) %>% 
  mutate(cofeeMachine  = ifelse(str_detect(amenities, "Coffee|coffee|Nespresso"), 1, 0)) %>% 
  mutate(laundry_services  = ifelse(str_detect(amenities, "Launry|laundry|Laundromat|laundromat"), 1, 0)) %>% 
  mutate(hot_tub  = ifelse(str_detect(amenities, "Hot tub|hot tub"), 1, 0)) %>% 
  mutate(balcony  = ifelse(str_detect(amenities, "Balcony|balcony|Patio|patio"), 1, 0)) %>% 
  mutate(linens  = ifelse(str_detect(amenities, "Pillow|Linens|linens|pillow|comforts|Sheets|sheets"), 1, 0)) %>% 
  mutate(breakfast_available  = ifelse(str_detect(amenities, "Breakfast|breakfast"), 1, 0)) %>% 
  mutate(staff  = ifelse(str_detect(amenities, "Staff|staff|Concierge|concierge|Room service|room service"), 1, 0)) %>% 
  mutate(clothing_storage  = ifelse(str_detect(amenities, "Closet|closet|Wardrobe|wardrobe|Clothing|clothing"), 1, 0)) %>% 
  mutate(dishes  = ifelse(str_detect(amenities, "Dishes|dishes"), 1, 0)) %>% 
  mutate(dining_table  = ifelse(str_detect(amenities, "Dining| dining|table|Table"), 1, 0))



#define levels and dummies 
levs <- levels(factor(unlist(df$amenities)))
df<-cbind(df,as.data.frame(do.call(rbind, lapply(lapply(df$amenities, factor, levs), table))))

drops <- c("amenities","translation missing: en.hosting_amenity_49",
           "translation missing: en.hosting_amenity_50")
df<-df[ , !(names(df) %in% drops)]


# drop columns that were already grouped or not relevant for the prediction (like'Essentials' or 'Board Games')
df <- df[ -c(85:135, 139: 199, 201:248, 250: 266, 271: 278, 281: 306, 309: 323, 331: 350, 353:402, 404: 408,  410:449, 451:559) ]




#write csv
write.csv(df,file=paste0(data_out,"airbnb_montreal_cleaned5.csv"))
