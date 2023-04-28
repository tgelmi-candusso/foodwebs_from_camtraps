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

####THIS IS THE CODE I USED FOR THE MANUSCRIPT!!!! with the rds from above. 
species <- levels(output_allsites$Species_1)
mesopredators <- c("fox", "coyote")
actual_Potprey <- c("dog", "cat", "deer", "rabbit", "raccoon", "skunk", "squirrel", "oppossum")
fox_coy_dogs <- c("fox", "coyote", "dog")
#output_allpredators_2 
nrow(unique(output_allsites))
#output_allsites <- readRDS('preypredatorcombnoutput_allsites.rds')
output <- output_allsites %>% unique() 
#nrow(prev_output_allsites2%>% unique())
#output_allsites %>% filter(output_allsites$Species_1 == 'fox') %>% filter(time_interval<0.01)
#nrow(output_allsites%>% unique())
# output<- prev_output_allsites%>% unique()
out<-output %>% 
  mutate(species1 = ifelse((Species_1 %in% predators), 'predator',
                              ifelse((Species_1 %in% actual_Potprey), 'prey','NA')))%>%
  mutate(species2 = ifelse((Species_2 %in% predators), 'predator',
                              (ifelse((Species_2 %in% actual_Potprey), 'prey','NA'))))

# out<-out %>% 
#   mutate(fox_coy_dogs = ifelse(((Species_1 %in% fox_coy_dogs)&(Species_2 %in% fox_coy_dogs)), 'yes', 'no'))
out2<-out %>% filter(species1!=species2)
out_a<- out2 %>% filter (species1 == 'predator') %>%
  mutate(predator = Species_1) %>%
  mutate(prey = Species_2)

out_b<- out2 %>% filter (species1 == 'prey') %>%
  mutate(predator = Species_2) %>%
  mutate(prey = Species_1)

out_pp<- rbind(out_a, out_b)

out_pp1 <- out_pp %>%
  mutate(bird_feeder = ifelse(site_name == 'TUW25', 'yes', 'no')) %>%
  filter(time_interval <= 0.08666667) #%>% 
  #filter(prey!= "dog")

#only dogs, foxes and coyotes
out_c<- out %>% 
  mutate(pred_pred = ifelse(species1=='predator' & species2=='predator', 'yes', 'no'))
out_fcd<- out_c %>% filter(species1=='predator' & species2=='predator')

#trail<- c("TUW04", "TUW37b", "TUW04b", "TUW26", "TUW28", "TUW23", "TUW38b")

out_fcd2 <- out_fcd %>% 
  mutate(bird_feeder = ifelse(site_name == 'TUW25', 'yes', 'no')) %>%
  mutate(trail = ifelse(site_name %in% trail, 'yes', 'no')) %>%
  filter(time_interval <= 0.08666667) 
#no funciona este objeto
species
#View(out_fcd)
out_pp1$predator <- factor(out_pp1$predator, levels =c("fox","coyote", "dog", "cat", 'squirrel', 'rabbit', 'raccoon', 'skunk', 'oppossum', 'deer', "small rodent","mink","not listed","groundhog", "horse","empty", "chipmunk","bird"))
out_pp1$prey <- factor(out_pp1$prey, levels =c("dog", "cat", 'squirrel', 'rabbit', 'raccoon', 'skunk', 'oppossum', 'deer', "small rodent","mink","not listed","groundhog", "horse","empty", "chipmunk","bird", "coyote","fox"))
ggplot(out_pp1, aes(x=prey, y=time_interval,  shape=species1)) + 
  geom_jitter(aes(fill= bird_feeder), width = 0.20) + 
  facet_wrap(~predator, nrow =2) +
  scale_x_discrete(name = "species")+
  scale_y_continuous(name = "waiting times",breaks = c(0.016666667,0.0333333, 0.05, 0.066666667, 0.08333334), labels = c('1 min', '2 min', '3 min', '4 min', '5 min'))+
  scale_shape_manual(name = 'First species',
                     values=c("predator"=24,"prey"=21))+
  scale_fill_manual(name = 'Bird feeder', values = c("black", "white"))+
  ggtitle('Potential encounter events')+
  theme_bw() +
  theme(panel.grid.major.y = element_line(colour = "grey", linetype="dashed"), 
        panel.grid.minor.y = element_blank())
##how many of out_pp1 are foxes, coyotes etc - summary numbers for paper.
nrow(out_pp1 %>% filter(predator=='fox'& prey!='dog'))
nrow(out_pp1 %>% filter(predator=='coyote'& prey!='dog'))
nrow(out_pp1 %>% filter(species1=='predator'& prey!='dog'))
nrow(out_pp1 %>% filter(species1=='prey'& prey!='dog'))
nrow(out_pp1 %>% filter((predator=='fox' & site_name !="TUW25")))
nrow(out_pp1 %>% filter((species1=='predator' &predator=='coyote')))
supp.labs <- c("coyote")
names(supp.labs) <- c("predator")

###count those excluding dogs and excluding TUW25
nrow(out_pp1 %>% filter((predator=='fox' & site_name !="TUW25"& prey!='dog')))
nrow(out_pp1 %>% filter((predator=='coyote' & site_name !="TUW25"& prey!='dog')))
nrow(out_pp1 %>% filter((predator=='fox' & prey=='dog')))
nrow(out_pp1 %>% filter((predator=='coyote' & prey=='dog')))


ggplot(out_fcd2, aes(x=predator, y=time_interval, shape=Species_1)) + 
  geom_jitter(aes(fill= trail), width = 0.20) + 
  facet_wrap(~species1, nrow =1, labeller=labeller(species1 = supp.labs)) +
  #theme(panel.grid.major.y = element_line(colour = "grey",linetype="dashed",size=0.1))+
  scale_x_discrete(name = "species")+
  scale_y_continuous(name = "waiting times", limits = c(-0.000001,0.08666667), breaks = c(seq(0.016666667,0.08333334, by=0.016666667)), labels = c('1 min', '2 min', '3 min', '4 min', '5 min'))+
  scale_shape_manual(name = 'First species',
                     values=c(24, 21))+
  scale_fill_manual(name = 'Bird feeder', values = c("black", "white"))+
  ggtitle('Potential encounter events')+
  theme_bw() +
  theme(panel.grid.major.y = element_line(colour = "grey", linetype="dashed"), 
        panel.grid.minor.y = element_blank())
is.na(out_fcd2)
  ##when considering predator after prey
out_pp2 <- out_pp1 %>% select(predator, prey, bird_feeder, species1, species2)%>%
  filter(prey != 'dog')%>%
  filter(!(bird_feeder == 'yes')) %>%
  filter(species2 == 'predator')
out_pp2 <- out_pp2 %>% select(prey, predator, bird_feeder)
out_pp2$site<- "site_s"
colnames(out_pp2)<- c("prey", "predator", "bird_feeder", "site_s")
##create file for web
web5<- frame2webs(out_pp2[,c(1:2,4)], varnames = c("prey", "predator", "site_s"), type.out = "list", emptylist = TRUE)
plotweb(web5$site_s, arrow='down')
bipartite_D3(web5$site_s)
Bmod_pred <- computeModules(web5$site_s)
Bmodpred_prey<- plotModuleWeb(Bmod_pred)
networklevel(web5$site_s)
nestedcontribution(web5$site_s, nsimul = 99)

###when considering prey after predator
out_pp2 <- out_pp1 %>% select(predator, prey, bird_feeder, species1, species2)%>%
  filter(prey != 'dog')%>%
  filter(!(bird_feeder == 'yes')) %>%
  filter(species1 == 'predator')
out_pp2 <- out_pp2 %>% select(predator, prey, bird_feeder)
out_pp2$site<- "site_s"
colnames(out_pp2)<- c("predator","prey", "bird_feeder", "site_s")
##create file for web
web6<- frame2webs(out_pp2[,c(1:2,4)], varnames = c("prey", "predator", "site_s"), type.out = "list", emptylist = TRUE)
plotweb(web6$site_s)
bipartite_D3(web6$site_s)
Bmod_prey <- computeModules(web6$site_s)
Bmodprey_pred<- plotModuleWeb(Bmod_prey)
networklevel(web6$site_s)
nestedcontribution(web6$site_s, nsimul = 99)

out_pp2 <- out_pp1 %>% select(predator, prey, bird_feeder, species1, species2)%>%
  filter(prey != 'dog')%>%
  filter(!(bird_feeder == 'yes')) 
out_pp2 <- out_pp2 %>% select(predator, prey, bird_feeder)
out_pp2$site<- "site_s"
colnames(out_pp2)<- c("predator","prey", "bird_feeder", "site_s")
##create file for web
out_pp2$predator <- factor(out_pp2$predator, labels = c('fox (PEE)', 'coyote (PEE)'))
web7<- frame2webs(out_pp2[,c(1:2,4)], varnames = c("prey", "predator", "site_s"), type.out = "list", emptylist = TRUE)
plotweb(web7$site_s)
bipartite_D3(web7$site_s)
Bmod <- computeModules(web7$site_s)
plotModuleWeb(Bmod)
networklevel(web7$site_s)
nestedcontribution(web7$site_s, nsimul = 99)



par(mfrow=c(2,2))
plotModuleWeb(AAmod)
title(main = "Predation observations", line = -1,
      cex.main = 1,   font.main= 2, col.main= "black")
plotModuleWeb(Bmod)
title(main = "Bidirectional waiting time", line = -1,
      cex.main = 1,   font.main= 2, col.main= "black")
plotModuleWeb(Bmod_prey)
title(main = "Unidirectional:Predator after prey", line = -1,
      cex.main = 1,   font.main= 2, col.main= "black")
plotModuleWeb(Bmod_pred)
title(main = "Unidirectional:Prey after predator", line = -1,
      cex.main = 1,   font.main= 2, col.main= "black")

par(mfrow=c(2,2))
plotweb(web4$site_s)
title(main = "Predation observations", line = 0,
      cex.main = 1,   font.main= 2, col.main= "black")
plotweb(web7$site_s)
title(main = "Bidirectional waiting time", line = 0,
      cex.main = 1,   font.main= 2, col.main= "black")
plotweb(web5$site_s)
title(main = "Unidirectional:Predator after prey", line = 0,
      cex.main = 1,   font.main= 2, col.main= "black")
plotweb(web6$site_s)
title(main = "Unidirectional:Prey after predator", line = 0,
      cex.main = 1,   font.main= 2, col.main= "black")
par(new=T)
###there is a difference if we consider prey or predator first or second when we comput modularity!! 

##incidence matrixes
install.packages('incidentally')
library(incidentally)

#when we filter the combination of fox with the less probable preys, cat and raccoon
out_pp3 <- out_pp1 %>% select(predator, prey, bird_feeder)%>%
  filter(prey != 'dog')%>%
  filter(!(predator == 'fox' & prey == 'raccoon'))%>%
  filter(!(predator == 'fox' & prey == 'cat'))%>%
  filter(!(predator == 'fox' & prey == 'oppossum'))
out_pp3$site<- "site_s"
colnames(out_pp3)<- c("predator","prey", "bird_feeder", "site_s")
##create file for web
web6<- frame2webs(out_pp3[,c(1:2,4)], varnames = c("predator", "prey", "site_s"), type.out = "list", emptylist = TRUE)
plotweb(web6$site_s)
bipartite_D3(web6$site_s)
Bmod1 <- computeModules(web6$site_s)
plotModuleWeb(Bmod1)
