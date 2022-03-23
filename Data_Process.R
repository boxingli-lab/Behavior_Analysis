#!user/bin/env R
# Author: Yuxin Qin
# Created: 2021-12
# File: Data_Process.R
# Email: qinyx3@mail2.sysu.edu.cn
# Usage: This file is used to handle the behavior data and plot it.

#################
### libraries ###
#################
library(readxl)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(ggthemes)
library(scales)
library(plyr)

#################
### functions ###
#################
# This function is used to draw a data frame with the start and end frame of particular behavior.
#########
# Input
# data: The data frame with all the mice behavior information with each frame annotate.
# col: The column of a specific mice. 
#########
# Output
# s: The data frame with the start and end frame of every partibular behavior of the pointed mice.
rect_data <- function(data, col){
  xstart <- c()
  xend <- c()
  class <- c()
  for(i in 1:nrow(data)){
    if(i == 1){
      xstart <- c(xstart, 1)
      class <- c(class, as.character(data[i, col]))
    }else{
      if(data[i, col] == data[i-1, col]){#equal to last frame
        next
      }else{ # not equal to last frame
        xstart <- c(xstart, i)
        class <- c(class, as.character(data[i, col]))
        xend <- c(xend, i-1)
      }
    }
  }
  xend <- c(xend, nrow(data))
  s <-data.frame(start = xstart,
                         end = xend,
                         type = class)
  return(s)
}

# This function is used to draw a data frame with the start and end frame of particular behavior.
#########
# Input
# data: The data frame with all the mice behavior information with each frame annotate.
# col: The column number of the specific mice. 
# time_tag: The time of the behavior happen, which is the minute.
#########
# Output
# mice_data: The data frame with the mice name (mice) 
#            the sequence number of behavior happened (count) , 
#            the time last for the behavior with the unit of seconds (long), 
#            the time tag (time),  
#            the probability of the behavior happened in one minute (prob).
handle_data <- function(data, col, mice, time_tag){
  mice_data <- rect_data(data, col)
  mice_data$mice <- rep(mice, nrow(mice_data))
  mice_data$count <- seq(1,nrow(mice_data), 1)
  mice_data$long <- round((mice_data$end-mice_data$start)/15, 4)
  mice_data$time <- time_tag
  mice_data$prob <- round(mice_data$long/60, 4)
  return(mice_data)
}

#########################
### input information ###
#########################
# The section intended to divide
section <- 10

# Frames per section
per_section <- 9000/section

#######################################
### Handling data: behavior summary ###
#######################################
# Read in data
data <- read.csv('input_data/empathy_behavior_merge.csv')
colnames(data)[1] <- 'frames'

# Add time tag column
time_vec <- c()
for (i in 1:section) {
  add <- rep(i, per_section)
  time_vec <- c(time_vec, add)
}
data$time <- time_vec

# Handle data with each mice for time_tag 1
time_tag <- 1
time1 <- subset(data, time == time_tag)
female1 <- handle_data(time1, 2, 'female1', time_tag)
female2 <- handle_data(time1, 3, 'female2', time_tag)
female3 <- handle_data(time1, 4, 'female3', time_tag)
female4 <- handle_data(time1, 5, 'female4', time_tag)
female5 <- handle_data(time1, 6, 'female5', time_tag)
female6 <- handle_data(time1, 7, 'female6', time_tag)
female7 <- handle_data(time1, 8, 'female7', time_tag)
female8 <- handle_data(time1, 9, 'female8', time_tag)
female9 <- handle_data(time1, 10, 'female9', time_tag)
female10 <- handle_data(time1, 11, 'female10', time_tag)
male1 <- handle_data(time1, 12, 'male1', time_tag)
male2 <- handle_data(time1, 13, 'male2', time_tag)
male3 <- handle_data(time1, 14, 'male3', time_tag)
male4 <- handle_data(time1, 15, 'male4', time_tag)
male5 <- handle_data(time1, 16, 'male5', time_tag)
male6 <- handle_data(time1, 17, 'male6', time_tag)
male7 <- handle_data(time1, 18, 'male7', time_tag)
male8 <- handle_data(time1, 19, 'male8', time_tag)
male9 <- handle_data(time1, 20, 'male9', time_tag)
male10 <- handle_data(time1, 21, 'male10', time_tag)

# Join all the mice together
all_data <- female1 %>% bind_rows(female2)%>% 
  bind_rows(female3)%>% 
  bind_rows(female4)%>%
  bind_rows(female5)%>% 
  bind_rows(female6)%>%
  bind_rows(female7)%>% 
  bind_rows(female8)%>%
  bind_rows(female9)%>% 
  bind_rows(female10)%>%
  bind_rows(male1)%>%
  bind_rows(male2)%>%
  bind_rows(male3)%>%
  bind_rows(male4)%>%
  bind_rows(male5)%>%
  bind_rows(male6)%>%
  bind_rows(male7)%>%
  bind_rows(male8)%>%
  bind_rows(male9)%>%
  bind_rows(male10)

# Handle data with each mice for the rest of the time-tag
for(i in 2:section){
 time_tag <- i 
 time1 <- subset(data, time == time_tag)
 female1 <- handle_data(time1, 2, 'female1', time_tag)
 female2 <- handle_data(time1, 3, 'female2', time_tag)
 female3 <- handle_data(time1, 4, 'female3', time_tag)
 female4 <- handle_data(time1, 5, 'female4', time_tag)
 female5 <- handle_data(time1, 6, 'female5', time_tag)
 female6 <- handle_data(time1, 7, 'female6', time_tag)
 female7 <- handle_data(time1, 8, 'female7', time_tag)
 female8 <- handle_data(time1, 9, 'female8', time_tag)
 female9 <- handle_data(time1, 10, 'female9', time_tag)
 female10 <- handle_data(time1, 11, 'female10', time_tag)
 male1 <- handle_data(time1, 12, 'male1', time_tag)
 male2 <- handle_data(time1, 13, 'male2', time_tag)
 male3 <- handle_data(time1, 14, 'male3', time_tag)
 male4 <- handle_data(time1, 15, 'male4', time_tag)
 male5 <- handle_data(time1, 16, 'male5', time_tag)
 male6 <- handle_data(time1, 17, 'male6', time_tag)
 male7 <- handle_data(time1, 18, 'male7', time_tag)
 male8 <- handle_data(time1, 19, 'male8', time_tag)
 male9 <- handle_data(time1, 20, 'male9', time_tag)
 male10 <- handle_data(time1, 21, 'male10', time_tag)
 # join the data together
 time_data <- female1 %>% bind_rows(female2)%>% 
   bind_rows(female3)%>% 
   bind_rows(female4)%>%
   bind_rows(female5)%>% 
   bind_rows(female6)%>%
   bind_rows(female7)%>% 
   bind_rows(female8)%>%
   bind_rows(female9)%>% 
   bind_rows(female10)%>%
   bind_rows(male1)%>%
   bind_rows(male2)%>%
   bind_rows(male3)%>%
   bind_rows(male4)%>%
   bind_rows(male5)%>%
   bind_rows(male6)%>%
   bind_rows(male7)%>%
   bind_rows(male8)%>%
   bind_rows(male9)%>%
   bind_rows(male10)
 
 all_data <- bind_rows(all_data, time_data)
}

# Change data label
all_data$type <- gsub("N", "Sniffing mice (no pain)", all_data$type) 
all_data$type <- gsub("P", "Sniffing mice (pain)", all_data$type) 
all_data$type <- gsub("E", "Exploring", all_data$type) 
all_data$type <- gsub("G", "Grooming", all_data$type) 
all_data$type <- gsub("O", "Others", all_data$type) 

# Change to factor
all_data$type <- factor(all_data$type, levels = c("Sniffing mice (no pain)","Sniffing mice (pain)", "Exploring", "Grooming", "Others"))
all_data$mice <- factor(all_data$mice, levels = c('female1','male1', 'female2','male2','female3','male3','female4','male4', 'female5','male5', 'female6','male6','female7','male7','female8','male8','female9','male9','female10','male10'))

# Adjust position
all_data <- select(all_data, mice, time, count, type, long, prob, start, end, total)

# Save the data
write.csv(all_data, 'output_data/behavior_summary.csv')

#####################################################
### Handling data: feature data accumulative time ###
#####################################################
# Get all the mice name
mice_group <- unique(all_data$mice)

# Create the data frame with its column names 
feature_data <- as.data.frame(matrix(0, section*length(mice_group), 10))
colnames(feature_data) <- c('Nnum', 'Pnum', 'Enum', 'Gnum', 'Onum', 'Nprob', 'Pprob', 'Eprob', 'Gprob', 'Oprob')

# Calculate the features for each mice in each section
for(j in 1: length(mice_group)){
  female1 <- subset(all_data, mice == mice_group[j])
  for(i in 1:10){
    female1_time1 <- subset(female1, time <= i)
    c <- count(female1_time1$type)
    if(length(which(c$x =='Sniffing mice (no pain)')) > 0){
      feature_data[(i + (j-1) * 10), 1] <- c$freq[which(c$x =='Sniffing mice (no pain)')]
      feature_data[(i + (j-1) * 10), 6] <- sum(subset(female1_time1, type == 'Sniffing mice (no pain)')$prob)/sum(female1_time1$prob)
    }
    if(length(which(c$x =='Sniffing mice (pain)')) > 0){
      feature_data[(i + (j-1) * 10), 2] <- c$freq[which(c$x =='Sniffing mice (pain)')]
      feature_data[(i + (j-1) * 10), 7] <- sum(subset(female1_time1, type == 'Sniffing mice (pain)')$prob)/sum(female1_time1$prob)
    }
    if(length(which(c$x =='Exploring')) > 0){
      feature_data[(i + (j-1) * 10), 3] <- c$freq[which(c$x =='Exploring')]
      feature_data[(i + (j-1) * 10), 8] <- sum(subset(female1_time1, type == 'Exploring')$prob)/sum(female1_time1$prob)
    }
    if(length(which(c$x =='Grooming')) >0){
      feature_data[(i + (j-1) * 10), 4] <- c$freq[which(c$x =='Grooming')]
      feature_data[(i + (j-1) * 10), 9] <- sum(subset(female1_time1, type == 'Grooming')$prob)/sum(female1_time1$prob)
    }
    if(length(which(c$x =='Others')) > 0){
      feature_data[(i + (j-1) * 10), 5] <- c$freq[which(c$x =='Others')]
      feature_data[(i + (j-1) * 10), 10] <- sum(subset(female1_time1, type == 'Others')$prob)/sum(female1_time1$prob)
    }
  }
}

# Create row names with mice name and the section
col_vec <- c()
for(i in 1: length(mice_group)){
  for(j in 1:section){
    col_vec <- c(col_vec, paste0(mice_group[i], '_', j))
  }
}
rownames(feature_data) <- col_vec  

# Save the data
write.csv(feature_data, 'output_data/feature_data_accumulative_time.csv')

########################
### Plot the heatmap ###
########################
# Select the behavior events intended to present
data_t <- subset(all_data, type == "Sniffing mice (pain)" | type == "Grooming")

# Plot
p <- ggplot(data_t)+
  geom_rect(aes(xmin = start,xmax = end , ymin = 0 , ymax = 1 , fill = type))+ 
  facet_wrap(~mice, ncol = 2, strip.position="left")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())+
  scale_x_continuous(breaks=seq(0,9000,by=3000))+
  theme(legend.position = "bottom")+
  scale_fill_manual(values=c(hue_pal()(3)[1], hue_pal()(3)[2]))

# Save the plot
ggsave(paste0("Plots/specific_behavior.pdf"), p, width =8, height = 10)
