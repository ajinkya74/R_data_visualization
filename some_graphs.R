library(tidyverse)
source('hw.R')
library(usmap)
library(ggplot2)

all_data <- read.csv('shootingdata_n.csv',header = T)

#####by number of incidnets US map ######
all_heat<-all_data[, c('Location_ab', 'total_victims')]
all_mod<-aggregate(all_heat[,2], list(all_heat$Location_ab), sum)

names(all_mod) <- c("state", "values")

all_count<-aggregate(cbind(count = total_victims) ~ Location_ab, 
          data = all_heat, 
          FUN = function(x){NROW(x)})

p<-ggplot(data=all_count, aes(x=state, y=count)) +
  geom_bar(stat="identity")
p

colnames(all_count)<-c('state','count')

p2<-plot_usmap(data=all_count, values='count',lines='red',labels='True')+
  labs(title = "Mass shootings in USA from 1982 to 2019")+
  scale_fill_continuous(
    low = "yellow", high = "red", name = "Mass shooting", label = scales::comma
  ) + theme(legend.position = "right")
p2

############################################
data2<-all_data
data2$president <- factor(data2$president,
                          
                          levels = c("Reagan","Bush Sr", "Clinton", "Bush",
                                     
                                     "Obama", "Trump"))

############################
plot1 <- ggplot(data2, aes(x=president) ) +
  geom_bar() + 
  labs(x='President',
       y='Number of Shootings',
       title='Frequency of Mass Shootings\nUnder Presidential Terms') + hw
plot1

########################weapns obtained_legally#######
data2$weapons_obtained_legally <- as.character(data2$weapons_obtained_legally)
data2$weapons_obtained_legally[data2$weapons_obtained_legally == "Yes "] <- "Yes"
#categories <- unique(data2$weapons_obtained_legally) 
data3<-data2
data3$weapons_obtained_legally <- factor(data2$weapons_obtained_legally,
                          
                          levels = c("Yes","No","Unknown","TBD"))



plot2 <- ggplot(data3, aes(x=Location_ab, fill=weapons_obtained_legally) ) +
  geom_bar() + 
  labs(x='States',
       y='Number of Shootings',
       title='Frequency of Mass Shootings in states 1982-2019',
       fill='Weapons obtained legally') + 
  hw
plot2


###############################################
###########future use sql commands###################################
#library(sqldf)
#ll_data<-sqldf("select Location_ab, sum(weapons_obtained_legally like'Yes%')as Legal,sum(weapons_obtained_legally like'No%')as Illegal,
#      sum(weapons_obtained_legally like'Unknown%')as Unknown , count(weapons_obtained_legally)as Total from data2 group by Location_ab")


#summary(ll_data)
#sqldf("select Location_ab, count(*)from data2 group by Location_ab")

#transform(ll_data, new = (Legal / Total)*100)


###################us map total victims######################



colnames(all_mod)<-c('state','totalvic')
is.data.frame(all_mod)
as.tibble(all_mod)
p1<-plot_usmap(data=all_mod, values='totalvic',lines='red',labels='True')+
  scale_fill_continuous(
    low = "yellow", high = "red", name = "Mass shooting", label = scales::comma
  ) + theme(legend.position = "right")

p1
p<-ggplot(data=all_mod, aes(x=state, y=totalvic)) +
  geom_bar(stat="identity")
p



