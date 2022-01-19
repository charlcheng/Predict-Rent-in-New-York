###Loading needed libraries
library("readxl")
library("fpp2")
library("xts")
library("forecast")
library(IRdisplay)
library(magrittr)
library(tidyverse)
library(scales)
library(gridExtra)
library(forecast)
library(tseries)
library(ggthemes)
library(tsibbledata)
library(pastecs)
library(Metrics)
library(plm)
library(ISLR)
library(caret)
library(caTools)
library(ISLR) 
library(dplyr)
library(randomForest)
library(ranger)
library(Rborist)
library(ngram)

#########Reading Analysis Data#################
kg_df <- read.csv("analysisData.csv")

#######Cleaning Data if needed###############
kg_df$host_acceptance_rate <- gsub("N/A", 52.58017, kg_df$host_acceptance_rate)  ##Convert NA into mean
kg_df$host_response_rate <- gsub("N/A", 66.51636, kg_df$host_response_rate)  ##Convert NA into mean
kg_df <- kg_df %>%
  mutate(host_response_rate = parse_number(host_response_rate)) %>%
  mutate(host_response_rate = host_response_rate/100) %>%
  mutate(host_acceptance_rate = parse_number(host_acceptance_rate)) %>%
  mutate(host_acceptance_rate = host_acceptance_rate/100) %>%
  mutate(zipcode = as.numeric(zipcode))  ###Convert to manageable data type
kg_df$zipcode[which(is.na(kg_df$zipcode))] = mean(kg_df$zipcode, na.rm = TRUE)
kg_df$host_acceptance_rate[which(is.na(kg_df$host_acceptance_rate))] = mean(kg_df$host_acceptance_rate, na.rm = TRUE)
kg_df$host_response_rate[which(is.na(kg_df$host_response_rate))] = mean(kg_df$host_response_rate, na.rm = TRUE)
#####Counting Words for features like summary, description, etc########
kg_df$name<-ifelse(kg_df$name=='',0,sapply(strsplit(kg_df$name,' '),length))
kg_df$summary<-ifelse(kg_df$summary=='',0,sapply(strsplit(kg_df$summary,' '),length)+1)
kg_df$space<-ifelse(kg_df$space=='',0,sapply(strsplit(kg_df$space,' '),length)+1)
kg_df$neighborhood_overview<-ifelse(kg_df$neighborhood_overview=='',0,sapply(strsplit(kg_df$neighborhood_overview,' '),length)+1)
kg_df$notes<-ifelse(kg_df$notes=='',0,sapply(strsplit(kg_df$notes,' '),length)+1)
kg_df$transit<-ifelse(kg_df$transit=='',0,sapply(strsplit(kg_df$transit,' '),length)+1)
kg_df$access<-ifelse(kg_df$access=='',0,sapply(strsplit(kg_df$access,' '),length)+1)
kg_df$interaction<-ifelse(kg_df$interaction=='',0,sapply(strsplit(kg_df$interaction,' '),length)+1)
kg_df$house_rules<-ifelse(kg_df$house_rules=='',0,sapply(strsplit(kg_df$house_rules,' '),length)+1)
kg_df$description<-ifelse(kg_df$description=='',0,sapply(strsplit(kg_df$description,' '),length)+1)
kg_df$host_about<-ifelse(kg_df$host_about=='',0,sapply(strsplit(kg_df$host_about,' '),length)+1)

#########################Converting city variable######################################################
kg_df$city[which(kg_df$city=="纽约"|kg_df$city=="纽约市")] = "nyc"
kg_df$city[which(kg_df$city=="纽约法拉盛"|kg_df$city=="布鲁克林")] = "ny"
kg_df$city<-tolower(kg_df$city)
kg_df$city<-ifelse(str_detect(kg_df$city,"long"),"lic",kg_df$city)
kg_df$city<-ifelse(str_detect(kg_df$city,"asto"),"asto",kg_df$city)
kg_df$city<-ifelse(str_detect(kg_df$city,"brook"),"bk",kg_df$city)
kg_df$city<-ifelse(str_detect(kg_df$city,"bron"),"bx",kg_df$city)
kg_df$city<-ifelse(str_detect(kg_df$city,"ny"),"ny",kg_df$city)
kg_df$city<-ifelse(str_detect(kg_df$city,"city"),"nyc",kg_df$city)
kg_df$city<-ifelse(str_detect(kg_df$city,"manha"),"mah",kg_df$city)
kg_df$city<-ifelse(str_detect(kg_df$city,"wood"),"wood",kg_df$city)
kg_df$city<-ifelse(str_detect(kg_df$city,"flus"),"fls",kg_df$city)
kg_df$city<-ifelse(str_detect(kg_df$city,"que"),"queens",kg_df$city)
kg_df$city<-ifelse(str_detect(kg_df$city,"new"),"ny",kg_df$city)
kg_df$city<-ifelse(str_detect(kg_df$city,"sou"),"sou",kg_df$city)
kg_df$city<-ifelse(str_detect(kg_df$city," "),"others",kg_df$city)
kg_df$city<-ifelse((kg_df$city=="nyc"|kg_df$city=="ny"|kg_df$city=="lic"|kg_df$city=="asto"|
                      kg_df$city=="bk"|kg_df$city=="bx"|kg_df$city=="mah"|kg_df$city=="wood"|
                      kg_df$city=="fls"|kg_df$city=="queens"|kg_df$city=="sou"),kg_df$city,"others")
kg_df$city<-as.factor(kg_df$city)
########Finding room age and other time variables########################

today = Sys.time()
kg_df$host_since <- as.Date(today) - as.Date(kg_df$host_since)
kg_df$host_since <- as.numeric(kg_df$host_since)

kg_df$first_review <- as.Date(today) - as.Date(kg_df$first_review)
kg_df$first_review <- as.numeric(kg_df$first_review)

kg_df$last_review <- as.Date(today) - as.Date(kg_df$last_review)
kg_df$last_review <- as.numeric(kg_df$last_review)

#######Checking if host in US###################
a<-str_extract(kg_df$host_location, "United States")
kg_df$host_location<-ifelse(a == "United States","t","f")
kg_df$host_location[is.na(kg_df$host_location)] = "f"
kg_df$host_location=as.factor(kg_df$host_location)

######Convert Features like Amenities and verification into numbers of items################
kg_df$amenities = gsub("\\.", "", kg_df$amenities) 
kg_df$amenities = kg_df$amenities %>% stringr::str_replace_all("\\s", "")
kg_df$amenities = noquote(kg_df$amenities)
kg_df$amenities<-nchar(gsub('[^,]+', '', gsub(',(?=,)|(^,|,$)', '',
                                                     gsub('(Null){1,}', '', kg_df$amenities), perl=TRUE)))+1L

kg_df$host_verifications = gsub("\\.", "", kg_df$host_verifications) 
kg_df$host_verifications = kg_df$host_verifications %>% stringr::str_replace_all("\\s", "")
kg_df$host_verifications = noquote(kg_df$host_verifications)
kg_df$host_verifications<-nchar(gsub('[^,]+', '', gsub(',(?=,)|(^,|,$)', '',
                                              gsub('(Null){1,}', '', kg_df$host_verifications), perl=TRUE)))+1L

######Converting Response time into levels, 1 as fastest and 4 as lowest#########
kg_df$host_response_time<-ifelse(kg_df$host_response_time%in%"within an hour",1,kg_df$host_response_time)
kg_df$host_response_time<-ifelse(kg_df$host_response_time%in%"within a few hours",2,kg_df$host_response_time)
kg_df$host_response_time<-ifelse(kg_df$host_response_time%in%"within a day",3,kg_df$host_response_time)
kg_df$host_response_time<-ifelse(kg_df$host_response_time%in%"a few days or more",4,kg_df$host_response_time)
kg_df$host_response_time<-ifelse(kg_df$host_response_time%in%"N/A",4,kg_df$host_response_time)
kg_df$host_response_time<-as.factor(kg_df$host_response_time)

kg_df$calendar_updated<-ifelse(str_detect(kg_df$calendar_updated,"months"),3,kg_df$calendar_updated)
kg_df$calendar_updated<-ifelse(str_detect(kg_df$calendar_updated,"day"),1,kg_df$calendar_updated)
kg_df$calendar_updated<-ifelse(str_detect(kg_df$calendar_updated,"week"),2,kg_df$calendar_updated)
kg_df$calendar_updated<-ifelse(str_detect(kg_df$calendar_updated,"nev"),4,kg_df$calendar_updated)
kg_df$calendar_updated<-as.factor(kg_df$calendar_updated)

#####################Working With neighborhood data by converting to levels###########################
###pricing each level of neighborhood
neighborhood = kg_df %>%
  group_by(neighbourhood_cleansed = neighbourhood_cleansed) %>%
  summarize(levels = n(),    
            price_level = mean(price)) %>%  
  arrange(desc(levels))
###Adding the new column for applying pricing segementation
kg_df = merge(kg_df, neighborhood, by = c("neighbourhood_cleansed", "neighbourhood_cleansed"))
kg_df$neighbourhood_cleansed = as.character(kg_df$neighbourhood_cleansed) 
kg_df = kg_df %>%
  mutate(new = ifelse(levels > 142, neighbourhood_cleansed, "other"))  
kg_df$neighbourhood_cleansed = as.factor(data$neighbourhood_cleansed)
kg_df$new = as.factor(kg_df$new)

########Checking Data Types#####################
kg_df$property_type = as.factor(kg_df$property_type)
kg_df$room_type = as.factor(kg_df$room_type)
kg_df$require_guest_phone_verification = as.factor(kg_df$require_guest_phone_verification)
kg_df$require_guest_profile_picture = as.factor(kg_df$require_guest_profile_picture)
kg_df$is_business_travel_ready = as.factor(kg_df$is_business_travel_ready)
kg_df$instant_bookable = as.factor(kg_df$instant_bookable)
kg_df$host_is_superhost = as.factor(kg_df$host_is_superhost)
kg_df$host_neighbourhood = as.factor(kg_df$host_neighbourhood)
kg_df$neighbourhood_group_cleansed = as.factor(kg_df$neighbourhood_group_cleansed)
kg_df$is_location_exact = as.factor(kg_df$is_location_exact)
kg_df$bed_type = as.factor(kg_df$bed_type)
kg_df$host_has_profile_pic = as.factor(kg_df$host_has_profile_pic)
kg_df$host_identity_verified = as.factor(kg_df$host_identity_verified)
kg_df$has_availability = as.factor(kg_df$has_availability)
kg_df$requires_license = as.factor(kg_df$requires_license)
kg_df$market = as.factor(kg_df$market)
kg_df$cancellation_policy = as.factor(kg_df$cancellation_policy)

########Fit numeric NA value with mean##########
for (i in 1:ncol(kg_df)) {
  if (is.numeric(kg_df[,i])) {
    kg_df[is.na(kg_df[,i]), i] = mean(kg_df[,i], na.rm = TRUE)
  }}

view(head(kg_df))


###############################Working with scoring data#################################
scoring<-read.csv("scoringData.csv")
       
scoring$host_acceptance_rate <- gsub("N/A", 52.58017, scoring$host_acceptance_rate)  ##Convert NA into mean
scoring$host_response_rate <- gsub("N/A", 66.51636, scoring$host_response_rate)  ##Convert NA into mean
scoring <- scoring %>%
  mutate(host_response_rate = parse_number(host_response_rate)) %>%
  mutate(host_response_rate = host_response_rate/100) %>%
  mutate(host_acceptance_rate = parse_number(host_acceptance_rate)) %>%
  mutate(host_acceptance_rate = host_acceptance_rate/100) %>%
  mutate(zipcode = as.numeric(zipcode))  ###Convert to manageable data type
scoring$zipcode[which(is.na(scoring$zipcode))] = mean(scoring$zipcode, na.rm = TRUE)
scoring$host_acceptance_rate[which(is.na(scoring$host_acceptance_rate))] = mean(scoring$host_acceptance_rate, na.rm = TRUE)
scoring$host_response_rate[which(is.na(scoring$host_response_rate))] = mean(scoring$host_response_rate, na.rm = TRUE)
#####Counting Words for features like summary, description, etc########
scoring$name<-ifelse(scoring$name=='',0,sapply(strsplit(scoring$name,' '),length))
scoring$summary<-ifelse(scoring$summary=='',0,sapply(strsplit(scoring$summary,' '),length)+1)
scoring$space<-ifelse(scoring$space=='',0,sapply(strsplit(scoring$space,' '),length)+1)
scoring$neighborhood_overview<-ifelse(scoring$neighborhood_overview=='',0,sapply(strsplit(scoring$neighborhood_overview,' '),length)+1)
scoring$notes<-ifelse(scoring$notes=='',0,sapply(strsplit(scoring$notes,' '),length)+1)
scoring$transit<-ifelse(scoring$transit=='',0,sapply(strsplit(scoring$transit,' '),length)+1)
scoring$access<-ifelse(scoring$access=='',0,sapply(strsplit(scoring$access,' '),length)+1)
scoring$interaction<-ifelse(scoring$interaction=='',0,sapply(strsplit(scoring$interaction,' '),length)+1)
scoring$house_rules<-ifelse(scoring$house_rules=='',0,sapply(strsplit(scoring$house_rules,' '),length)+1)
scoring$description<-ifelse(scoring$description=='',0,sapply(strsplit(scoring$description,' '),length)+1)
scoring$host_about<-ifelse(scoring$host_about=='',0,sapply(strsplit(scoring$host_about,' '),length)+1)
########Finding room age and other time variables########################

today = Sys.time()
scoring$host_since <- as.Date(today) - as.Date(scoring$host_since)
scoring$host_since <- as.numeric(scoring$host_since)

scoring$first_review <- as.Date(today) - as.Date(scoring$first_review)
scoring$first_review <- as.numeric(scoring$first_review)

scoring$last_review <- as.Date(today) - as.Date(scoring$last_review)
scoring$last_review <- as.numeric(scoring$last_review)

#######Checking if host in US###################
a<-str_extract(scoring$host_location, "United States")
scoring$host_location<-ifelse(a == "United States","t","f")
scoring$host_location[is.na(scoring$host_location)] = "f"
scoring$host_location=as.factor(scoring$host_location)

######Convert Features like Amenities and verification into numbers of items################
scoring$amenities = gsub("\\.", "", scoring$amenities) 
scoring$amenities = scoring$amenities %>% stringr::str_replace_all("\\s", "")
scoring$amenities = noquote(scoring$amenities)
scoring$amenities<-nchar(gsub('[^,]+', '', gsub(',(?=,)|(^,|,$)', '',
                                                gsub('(Null){1,}', '', scoring$amenities), perl=TRUE)))+1L

scoring$host_verifications = gsub("\\.", "", scoring$host_verifications) 
scoring$host_verifications = scoring$host_verifications %>% stringr::str_replace_all("\\s", "")
scoring$host_verifications = noquote(scoring$host_verifications)
scoring$host_verifications<-nchar(gsub('[^,]+', '', gsub(',(?=,)|(^,|,$)', '',
                                                         gsub('(Null){1,}', '', scoring$host_verifications), perl=TRUE)))+1L

######Converting Response time into levels, 1 as fastest and 4 as lowest#########
scoring$host_response_time<-ifelse(scoring$host_response_time%in%"within an hour",1,scoring$host_response_time)
scoring$host_response_time<-ifelse(scoring$host_response_time%in%"within a few hours",2,scoring$host_response_time)
scoring$host_response_time<-ifelse(scoring$host_response_time%in%"within a day",3,scoring$host_response_time)
scoring$host_response_time<-ifelse(scoring$host_response_time%in%"a few days or more",4,scoring$host_response_time)
scoring$host_response_time<-ifelse(scoring$host_response_time%in%"N/A",4,scoring$host_response_time)
scoring$host_response_time<-as.factor(scoring$host_response_time)

scoring$calendar_updated<-ifelse(str_detect(scoring$calendar_updated,"months"),3,scoring$calendar_updated)
scoring$calendar_updated<-ifelse(str_detect(scoring$calendar_updated,"day"),1,scoring$calendar_updated)
scoring$calendar_updated<-ifelse(str_detect(scoring$calendar_updated,"week"),2,scoring$calendar_updated)
scoring$calendar_updated<-ifelse(str_detect(scoring$calendar_updated,"nev"),4,scoring$calendar_updated)
scoring$calendar_updated<-as.factor(scoring$calendar_updated)

#####################Working With neighborhood data by converting to levels###########################
## create a data frame to use the neighbourhood_cleansed
scoring1 = scoring %>%
  group_by(neighbourhood_cleansed = neighbourhood_cleansed) %>%
  summarize(levels = n()) %>%
  arrange(desc(levels))
## merge the data frame and get the mean price and record counts
scoring = merge(scoring, scoring1, by = c("neighbourhood_cleansed", "neighbourhood_cleansed"))
scoring$neighbourhood_cleansed = as.character(scoring$neighbourhood_cleansed)
scoring = scoring %>%
  mutate(new = ifelse(levels > 32, neighbourhood_cleansed, "other"))
scoring$new = as.factor(scoring$new)
## create price_mean_c for score_data 
data2 = data.frame(neighbourhood_cleansed = neighborhood$neighbourhood_cleansed,
                   price_level = neighborhood$price_level)
scoring = merge(scoring, data2, by = c("neighbourhood_cleansed", "neighbourhood_cleansed"), all.x = TRUE) 
### remember to use all.x = TRUE here to keep scoring as a whole
## check the missing value

##############################Working with city########################################
#########################Converting city variable######################################################
scoring$city[which(scoring$city=="纽约"|scoring$city=="纽约市")] = "nyc"
scoring$city[which(scoring$city=="纽约法拉盛"|scoring$city=="布鲁克林")] = "ny"
scoring$city<-tolower(scoring$city)
scoring$city<-ifelse(str_detect(scoring$city,"long"),"lic",scoring$city)
scoring$city<-ifelse(str_detect(scoring$city,"asto"),"asto",scoring$city)
scoring$city<-ifelse(str_detect(scoring$city,"brook"),"bk",scoring$city)
scoring$city<-ifelse(str_detect(scoring$city,"bron"),"bx",scoring$city)
scoring$city<-ifelse(str_detect(scoring$city,"ny"),"ny",scoring$city)
scoring$city<-ifelse(str_detect(scoring$city,"city"),"nyc",scoring$city)
scoring$city<-ifelse(str_detect(scoring$city,"manha"),"mah",scoring$city)
scoring$city<-ifelse(str_detect(scoring$city,"wood"),"wood",scoring$city)
scoring$city<-ifelse(str_detect(scoring$city,"flus"),"fls",scoring$city)
scoring$city<-ifelse(str_detect(scoring$city,"que"),"queens",scoring$city)
scoring$city<-ifelse(str_detect(scoring$city,"new"),"ny",scoring$city)
scoring$city<-ifelse(str_detect(scoring$city,"sou"),"sou",scoring$city)
scoring$city<-ifelse(str_detect(scoring$city," "),"others",scoring$city)
scoring$city<-ifelse((scoring$city=="nyc"|scoring$city=="ny"|scoring$city=="lic"|scoring$city=="asto"|
                        scoring$city=="bk"|scoring$city=="bx"|scoring$city=="mah"|scoring$city=="wood"|
                        scoring$city=="fls"|scoring$city=="queens"|scoring$city=="sou"),scoring$city,"others")
scoring$city<-as.factor(scoring$city)


########Checking Data Types#####################
scoring$property_type = as.factor(scoring$property_type)
scoring$room_type = as.factor(scoring$room_type)
scoring$require_guest_phone_verification = as.factor(scoring$require_guest_phone_verification)
scoring$require_guest_profile_picture = as.factor(scoring$require_guest_profile_picture)
scoring$is_business_travel_ready = as.factor(scoring$is_business_travel_ready)
scoring$instant_bookable = as.factor(scoring$instant_bookable)
scoring$host_is_superhost = as.factor(scoring$host_is_superhost)
scoring$host_neighbourhood = as.factor(scoring$host_neighbourhood)
scoring$neighbourhood_group_cleansed = as.factor(scoring$neighbourhood_group_cleansed)
scoring$is_location_exact = as.factor(scoring$is_location_exact)
scoring$bed_type = as.factor(scoring$bed_type)
scoring$host_has_profile_pic = as.factor(scoring$host_has_profile_pic)
scoring$host_identity_verified = as.factor(scoring$host_identity_verified)
scoring$has_availability = as.factor(scoring$has_availability)
scoring$requires_license = as.factor(scoring$requires_license)
scoring$market = as.factor(scoring$market)
scoring$cancellation_policy = as.factor(scoring$cancellation_policy)
########Fit numeric NA value with mean##########
for (i in 1:ncol(scoring)) {
  if (is.numeric(scoring[,i])) {
    scoring[is.na(scoring[,i]), i] = mean(scoring[,i], na.rm = TRUE)
  }}


##############################Splitting Data Set###################################
set.seed(520)
split = createDataPartition(y = kg_df$price,
                            p = 0.7,
                            list = F,
                            groups = 100)
train_kg = kg_df[split,]
test_kg = kg_df[-split,]
nrow(train_kg) + nrow(test_kg) == nrow(kg_df)

str(train_kg)

lm <- lm(price ~ summary + space + description + neighborhood_overview + notes + transit + access
                     + interaction + house_rules + host_since + host_location + host_about + host_response_time + host_response_rate
                     + host_acceptance_rate + host_is_superhost + host_listings_count + host_total_listings_count + host_verifications
                     + host_has_profile_pic + host_identity_verified + neighbourhood_group_cleansed + is_location_exact + property_type 
                     + room_type + accommodates + bathrooms + bedrooms + beds + bed_type + amenities + security_deposit + cleaning_fee
                     + guests_included + extra_people + minimum_nights + maximum_nights + minimum_minimum_nights + maximum_minimum_nights
                     + minimum_maximum_nights + maximum_maximum_nights + minimum_nights_avg_ntm + maximum_nights_avg_ntm
                     + availability_30 + availability_60 + availability_90 + availability_365 + number_of_reviews_ltm + first_review + last_review
                     + review_scores_rating + review_scores_accuracy + review_scores_checkin + review_scores_communication + review_scores_location
                     + review_scores_value + review_scores_cleanliness + instant_bookable + require_guest_profile_picture + require_guest_phone_verification
                     + calculated_host_listings_count + calculated_host_listings_count_entire_homes + calculated_host_listings_count_private_rooms + calculated_host_listings_count_private_rooms
                     + calculated_host_listings_count_shared_rooms + reviews_per_month + market + calendar_updated + cancellation_policy + city
                     ,data = kg_df)
summary(lm)
predTree <- predict(tree, kg_df)
rmse(predTree, kg_df$price)
###########################modeling######################
###kg_df$calendar_updated = as.factor(kg_df$calendar_updated)
library(gbm)
set.seed(520)
  boost = gbm(price ~ summary + space + description + neighborhood_overview + notes + transit + access
              + interaction + house_rules + host_since + host_location + host_about + host_response_time + host_response_rate
              + host_acceptance_rate + host_is_superhost + host_listings_count + host_total_listings_count + host_verifications
              + host_has_profile_pic + host_identity_verified + neighbourhood_group_cleansed + is_location_exact + property_type 
              + room_type + accommodates + bathrooms + bedrooms + beds + bed_type + amenities + security_deposit + cleaning_fee
              + guests_included + extra_people + minimum_nights + maximum_nights + minimum_minimum_nights + maximum_minimum_nights
              + minimum_maximum_nights + maximum_maximum_nights + minimum_nights_avg_ntm + maximum_nights_avg_ntm
              + availability_30 + availability_60 + availability_90 + availability_365 + number_of_reviews_ltm + first_review + last_review
              + review_scores_rating + review_scores_accuracy + review_scores_checkin + review_scores_communication + review_scores_location
              + review_scores_value + review_scores_cleanliness + instant_bookable + require_guest_profile_picture + require_guest_phone_verification
              + calculated_host_listings_count + calculated_host_listings_count_entire_homes + calculated_host_listings_count_private_rooms + calculated_host_listings_count_private_rooms
              + calculated_host_listings_count_shared_rooms + reviews_per_month + market + new + calendar_updated + cancellation_policy + city
               ,data = kg_df, distribution = "gaussian", 
               n.trees = 30000,   
               interaction.depth = 5,
               shrinkage = 0.003,
               n.minobsinnode = 5)
summary(boost)
predBoost = predict(boost, scoring, n.trees = 30000)
summary(predBoost)
summary(train$price)
su<- read.csv("Submit3.csv")
summary(su$price)
summary(train$price)

###########################Submission!!!!#############################
submissionFile = data.frame(id = scoring$id, price = predBoost)
write.csv(submissionFile, 'SubmitFinal.csv',row.names = F)
