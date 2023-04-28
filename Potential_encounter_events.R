#### POTENTIAL ENCOUNTER RATES ########
## Author: Tiziana A. Gelmi Candusso  - tiziana.gelmicandusso@utoronto.ca
## uses raw output from labelled images using Timelapse

##### libraries ######
library(dplyr)
library(plyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(tidyverse)

#####  working folder ####
getwd()
setwd("C:/Users/tizge/Documents/Github/cameratrap_analysis")

####

##call dataset (raw timelapse export csv)
##this dataset already has only fully revised sites. 
data<-read.csv("data_clean_revsites_28072022.csv")%>%
  filter(DeleteFlag %in% c("FALSE", "false"))%>%
  filter(revised %in% c("TRUE", "true")) 



#set date + species name format
as.character(data$common_name) -> data$common_name
as.POSIXlt(data$DateTime, format="%Y-%m-%d %H:%M:%S") -> data$DateTime
#WHY? as.POSIXlt(paste(data$DateTime), format="%Y-%m-%d %H:%M:%S") -> data$Date

##filter out unrevised sites + species not of interest

species <- levels(factor(data$common_name))

preys <- c("dog", "cat", "chipmunk" , "mink", "deer","groundhog","oppossum","rabbit","raccoon","skunk","small rodent","squirrel","fox", "coyote")
actual_preys <-c("deer",
                 #"oppossum",
                 "rabbit","raccoon","skunk",
                 #"small rodent",
                 "squirrel", "cat")
mesopredators <-c("oppossum","raccoon","squirrel","fox", "coyote")

predators <- c("coyote", "fox")
prey_predator <- c(preys,predators)
sites <- levels(factor(data$site_name))
#prev_output_allsites2<-output_allsites
output_allsites <- data.frame()
output_allprey <- data.frame()
output_allpredators <- data.frame()
data$site_name <- factor(data$site_name)
data$common_name <- factor(data$common_name)

for (predator in predators){
  for (prey in preys){
    species <- c(predator,prey)
    data1 <- data %>%
      filter(common_name %in% species)
    for (site in sites){
      data2 <- data1 %>%
        filter(site_name == site) %>% 
        dplyr::arrange(DateTime)
      if(length(unique(data2$common_name))>1){
        #keep species names
        combn(data2$common_name, 2, simplify=TRUE) -> m              #takes ~4 min to run
        m[1,] -> Species_1
        m[2,] -> Species_2
        rm(m)
        # ##keep station name (referred to as site_name in this .csv)
        combn(as.character(data2$DateTime),2, simplify=TRUE) -> p    #takes ~6 min to run
        p[1,] -> Date_1
        p[2,] -> Date_2
        rm(p)
        interval(Date_1,Date_2)->c                                            #takes ~5 min to run
        as.duration(c)->d
        as.numeric(d)/3600-> time_interval    #time interval in hours
        rm(c)
        rm(d)
        combn(as.character(data2$File),2, simplify=TRUE) -> p    #takes ~6 min to run
        p[1,] -> Filename_1
        p[2,] -> Filename_2
        rm(p)
        combn(as.character(data2$site_name),2, simplify=TRUE) -> p    #takes ~6 min to run
        p[1,] -> site_1
        p[2,] -> site_2
        rm(p)
        output_dataset_loc <- data.frame(Species_1, Species_2, Date_1, Date_2, time_interval, Filename_1, Filename_2, site_1, site_2)
        output_dataset_loc$site_name <- site
        ##filter for events where species 1 and 2 are dissimilar and time interval is within 3 hours
        output_dataset_loc <- output_dataset_loc %>% 
          filter(Species_1 != Species_2) %>%
          filter(time_interval<12)
        if(nrow(output_dataset_loc)>=1){
          output_allsites <- rbind(output_allsites, output_dataset_loc)
        }
        #rm(output_dataset_loc)
       }
     }
    # if(nrow(output_allsites)>1){
    #   output_allprey <- rbind(output_allprey, output_allsites)
    }
    #rm(output_allsites)
  }
  # if(nrow(output_allprey)>1){
  #   output_allpredators <- rbind(output_allpredators, output_allprey)

  #rm(output_allprey)
#View(try)
saveRDS(output_allsites, 'preypredatorcombnoutput_allsites_Ttfix.rds')
#output_allsites <- readRDS('preypredatorcombnoutput_allsites.rds')

output_allpredators <-output_allsites
try<-output_allsites %>%
  unique() %>%
  filter(time_interval<0.08666667)
