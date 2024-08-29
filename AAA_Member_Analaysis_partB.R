
#rm(list=ls())
options(digit= 20)
pkages<-c("dplyr","broom", "tidyr", "lubridate", "MASS", "glmnet", "NbClust", "purrr", "ggplot2")
for(pkge in pkages){
  if(!pkge %in% installed.packages()) install.packages(pkge)
}
lapply(pkages, require, character.only= TRUE)
getwd(); setwd("C:/Users/maron/OneDrive/Desktop/rony_project")

member_sample_df= data.frame(read.csv("member_sample.csv"))

segment.vars= c("Member.Tenure.Years", "Race", "Length.Of.Residence", "Home.Owner", "Children")
member_sample= member_sample_df %>%
  mutate(Date.of.Birth= as.Date(Date.Of.Birth, format="%m/%d/%Y"),
         Age= 2020 - year(Date.of.Birth),
         avg_income= strsplit(Income_range, "-") %>% lapply(., function(x){
           unlist(x) %>% as.numeric %>% mean
         }) %>%unlist
  )%>%
  dplyr::filter(!is.na(Age))

pp1<-ggplot(member_sample, aes(x=avg_income)) + 
  geom_histogram(binwidth= 3, color="darkblue", fill="lightblue", alpha=0.9) +
  geom_vline(aes(xintercept=mean(Age)), color="blue", linetype="dashed", size=1) +
  ggtitle("Average Income Distribution of AAA Members") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Avg. Income of AAA Members")
print(pp1)

pp2<-ggplot(member_sample, aes(x=Age)) + 
  geom_histogram(binwidth= 3, color="darkblue", fill="lightblue", alpha=0.9) +
  geom_vline(aes(xintercept=mean(Age)), color="blue", linetype="dashed", size=1) +
  ggtitle("Age Distribution of AAA Members") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Age of AAA Members")
print(pp2)

pp4<-ggplot(member_sample, aes(x= Member.Tenure.Years)) + 
  geom_histogram(binwidth= 3, color="darkblue", fill="lightblue", alpha=0.9) +
  geom_vline(aes(xintercept=mean(Age)), color="blue", linetype="dashed", size=1) +
  ggtitle("Length of Residence of AAA Members") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Length of Residence")
print(pp4)

pp5<-ggplot(member_sample, aes(x= Length.Of.Residence)) + 
  geom_histogram(binwidth= 3, color="darkblue", fill="lightblue", alpha=0.9) +
  ggtitle("Length of Residence of AAA Members") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Length of Residence")
print(pp5)

#Incuding Race and Gender
pp6<-ggplot(member_sample, aes(x=Age, color= Gender)) + 
  geom_histogram(binwidth= 3, fill="lightblue", alpha=0.9, position="identity") +
  geom_vline(aes(xintercept=mean(Age)), color="blue", linetype="dashed", size=1) +
  ggtitle("Age Distribution of AAA Members") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab(" Age of AAA Members")
print(pp6)

pp7<-ggplot(member_sample, aes(x=Age, color= Race)) + 
  geom_histogram(binwidth= 3, fill="lightblue", alpha=0.9, position="identity") +
  #(aes(xintercept=mean(Age)), color="blue", linetype="dashed", size=1) +
  ggtitle("Age Distribution of AAA Members By Race") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab(" Age of AAA Members")
print(pp7)

#Incuding Race and Gender
pp8<-ggplot(member_sample, aes(x=avg_income, color= Gender)) + 
  geom_histogram(binwidth= 3, fill="lightblue", alpha=0.9, position="identity") +
  geom_vline(aes(xintercept=mean(Age)), color="blue", linetype="dashed", size=1) +
  ggtitle("Avg. Income Distribution of AAA Members By Gender") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab(" Avg. Income of AAA Members")
print(pp8)

pp9<-ggplot(member_sample, aes(x= avg_income, color= Race)) + 
  geom_histogram(binwidth= 3, fill="lightblue", alpha=0.9, position="identity") +
  ggtitle("Income of AAA Members By Race") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab(" Avg. Income of AAA Members")
print(pp9)

age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}
age_cat= age.cat(member_sample$avg_income, inc.min, inc.max, by= 10000,sep = "-", above.char = "+")

inc.min= min(na.omit(member_sample$avg_income))
inc.max= max(na.omit(member_sample$avg_income))
age.min= min(na.omit(member_sample$Age))
age.max= max(na.omit(member_sample$Age))
LoR.min= min(na.omit(member_sample$Length.Of.Residence))
LoR.max= max(na.omit(member_sample$Length.Of.Residence))
income.c1 <- cut(member_sample$avg_income, breaks = seq(inc.min, inc.max, by = 10000))
age.c1 <- cut(member_sample$Age, breaks = seq(age.min, age.max, by = 10))
LoR.c1 <- cut(member_sample$Age, breaks = seq(LoR.min, LoR.max, by = 5))
inc.tt= data.frame(table(income.c1))
age.tt= data.frame(table(age.c1))
LoR.tt= data.frame(table(LoR.c1))
inc<- ggplot(inc.tt, aes(income.c1, Freq)) + geom_bar(stat = "identity") + 
        xlab(label= "Income Bracket") + ylab("Counts") +
        ggtitle("Bar Chart of Average Income Distribution of AAA Members") +
        theme(plot.title= element_text(hjust= 0.5, size= 9, face="bold"),
        axis.text.x = element_text(angle = 45) ,
        axis.title.x= element_text(size= 8, face="bold.italic"),
        axis.title.y= element_text(size= 8, face="bold.italic"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())
age<- ggplot(age.tt, aes(age.c1, Freq)) + geom_bar(stat = "identity") + 
  xlab(label= "Age Bracket(Years)") + ylab("Counts") +
  ggtitle("Bar Chart of Age Distribution of AAA Members") +
  theme(plot.title= element_text(hjust= 0.5, size= 9, face="bold"),
        axis.text.x = element_text(angle = 45) ,
        axis.title.x= element_text(size= 8, face="bold.italic"),
        axis.title.y= element_text(size= 8, face="bold.italic"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

LoR<- ggplot(LoR.tt, aes(LoR.c1, Freq)) + geom_bar(stat = "identity") + 
  xlab(label= "Length of Residence") + ylab("Counts") +
  ggtitle("Bar Chart of Length of Residence Distribution of AAA Members") +
  theme(plot.title= element_text(hjust= 0.5, size= 9, face="bold"),
        axis.text.x = element_text(angle = 45) ,
        axis.title.x= element_text(size= 8, face="bold.italic"),
        axis.title.y= element_text(size= 8, face="bold.italic"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())
    

inc<- ggplot(inc.tt, aes(income.c1, Freq)) + geom_bar(stat = "identity") + 
  xlab(label= "Income Bracket") + ylab("Counts") +
  ggtitle("Bar Chart of Average Income Distribution of AAA Members") +
  theme(plot.title= element_text(hjust= 0.5, size= 9, face="bold"),
        axis.text.x = element_text(angle = 45) ,
        axis.title.x= element_text(size= 8, face="bold.italic"),
        axis.title.y= element_text(size= 8, face="bold.italic"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

