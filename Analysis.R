

#rm(list=ls())
options(digit= 20)
pkages<-c("dplyr","broom", "tidyr", "lubridate", "MASS", "glmnet", "NbClust", "purrr", "ggplot2")
for(pkge in pkages){
  if(!pkge %in% installed.packages()) install.packages(pkge)
}
lapply(pkages, require, character.only= TRUE)
getwd(); setwd("C:/Users/maron/OneDrive/Desktop/rony_project")

member_sample_df= data.frame(read.csv("member_sample.csv"))
income_ttb= data.frame(read.csv("income_brackets.csv"))
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

segment.vars= c("Member.Tenure.Years", "Race", "Length.Of.Residence", "Home.Owner", "Children")
member_sample= member_sample_df %>%
  mutate(Date.of.Birth= as.Date(Date.Of.Birth, format="%m/%d/%Y"),
         Age= 2020 - year(Date.of.Birth),
         avg_income= strsplit(Income_range, "-") %>% lapply(., function(x){
           unlist(x) %>% as.numeric %>% mean
         }) %>%unlist
  )%>%
  dplyr::arrange(avg_income)

inc_tt= member_sample[, colnames(member_sample) %in% 
                        c("Individual.Key" , "Income_range","avg_income")] %>%
                    dplyr::filter(!is.na(avg_income)) %>% dplyr::arrange(avg_income)
income_range.tt= data.frame(table(inc_tt$Income_range))%>%
                    dplyr::mutate(pct= paste0(round(prop.table(Freq),4)*100,"%"))%>%
                    dplyr::filter(!Var1=="")

inc<- ggplot(income_range.tt, aes(Var1, Freq, fill= Var1)) + geom_bar(stat = "identity") + 
  geom_text(aes(label= pct), vjust= -0.5, size= 3.0) +
  xlab(label= "Income Bracket($)") + ylab("Counts") +
  ggtitle("Count of AAA Members Specific Income Bracket") +
  theme(plot.title= element_text(hjust= 0.5, size= 9, face="bold"),
        axis.text.x = element_text(angle = 45) ,
        axis.title.x= element_text(size= 8, face="bold.italic"),
        axis.title.y= element_text(size= 8, face="bold.italic")) +
  theme(legend.title=element_blank())

age_cat= age.cat(member_sample$Age, 10, 120, by= 10,sep = "-", above.char = "+")    
age_tt= data.frame(table(age_cat))%>%
  dplyr::mutate(pct= paste0(round(prop.table(Freq),4)*100,"%"))
age<- ggplot(age_tt, aes(age_cat, Freq, fill= age_cat)) + geom_bar(stat = "identity") + 
  geom_text(aes(label= pct), vjust= -0.5, size= 3.0) + 
  xlab(label= "Age Bracket(Years)") + ylab("Counts") +
  ggtitle("Count of AAA Members In Each Age Bracket") +
  guides(col=guide_legend("Age Range")) +
  theme(plot.title= element_text(hjust= 0.5, size= 9, face="bold"),
        axis.text.x = element_text(angle = 45) ,
        axis.title.x= element_text(size= 8, face="bold.italic"),
        axis.title.y= element_text(size= 8, face="bold.italic"))+ 
  theme(legend.title=element_blank())

LoR_cat= age.cat(member_sample$Length.Of.Residence, 0, 50, by= 5,sep = "-", above.char = "+")    
LoR_tt= data.frame(table(LoR_cat))%>%
          dplyr::mutate(pct= paste0(round(prop.table(Freq),4)*100,"%"))
LoR<- ggplot(LoR_tt, aes(LoR_cat, Freq, fill= LoR_cat)) + geom_bar(stat = "identity") + 
  geom_text(aes(label= pct), vjust= -0.5, size= 3.0) + 
  xlab(label= "Length of Residence(Years)") + ylab("Counts") +
  ggtitle("Count of AAA Member with Length of Residence ") +
  theme(plot.title= element_text(hjust= 0.5, size= 9, face="bold"),
        axis.text.x = element_text(angle = 45) ,
        axis.title.x= element_text(size= 8, face="bold.italic"),
        axis.title.y= element_text(size= 8, face="bold.italic")) + 
  theme(legend.title=element_blank())

creditRange_tt= data.frame(table(member_sample$Credit.Ranges))%>%
                   dplyr::mutate(pct= paste0(round(prop.table(Freq),4)*100,"%"))%>%
                   dplyr::filter(!Var1=="")
creditR<- ggplot(creditRange_tt, aes(Var1, Freq, fill= Var1)) + geom_bar(stat = "identity") + 
  geom_text(aes(label= pct), vjust= -0.5, size= 3.0) + 
  xlab(label= "Credit Range") + ylab("Counts") +
  ggtitle("Count of AAA Member within  Credit Range") +
  theme(plot.title= element_text(hjust= 0.5, size= 9, face="bold"),
        axis.text.x = element_text(angle = 45) ,
        axis.title.x= element_text(size= 8, face="bold.italic"),
        axis.title.y= element_text(size= 8, face="bold.italic")) + 
  theme(legend.title=element_blank())

len.tenure_cat= age.cat(member_sample$Member.Tenure.Years, 0, 100, 
                                   by= 10,sep = "-", above.char = "+") 
len_of_tenure_tt= data.frame(table(len.tenure_cat))%>%
                      dplyr::mutate(pct= paste0(round(prop.table(Freq),4)*100,"%"))
len.of.tenure<- ggplot(len_of_tenure_tt, aes(len.tenure_cat, Freq, fill= len.tenure_cat)) + 
  geom_bar(stat = "identity") + geom_text(aes(label= pct), vjust= -0.5, size= 3.0) + 
  xlab(label= "Length of AAA membership(Years)") + ylab("Counts") +
  ggtitle("Count of AAA Members with\nLength of AAA Membership Bracket") +
  theme(plot.title= element_text(hjust= 0.5, size= 9, face="bold"),
        axis.text.x = element_text(angle = 45) ,
        axis.title.x= element_text(size= 8, face="bold.italic"),
        axis.title.y= element_text(size= 8, face="bold.italic")) + 
  theme(legend.title=element_blank())

race_tt= data.frame(table(member_sample$Race))%>%
             dplyr::mutate(pct= paste0(round(prop.table(Freq),4)*100,"%"))%>%
             dplyr::filter(!Var1=="")
race<- ggplot(race_tt, aes(Var1, Freq, fill= Var1)) + geom_bar(stat = "identity") + 
  geom_text(aes(label= pct), vjust= -0.5, size= 3.0) +
  xlab(label= "Race") + ylab("Counts") +
  ggtitle("Count of AAA Members with Specific Race") +
  theme(plot.title= element_text(hjust= 0.5, size= 9, face="bold"),
        axis.text.x = element_text(angle = 45) ,
        axis.title.x= element_text(size= 8, face="bold.italic"),
        axis.title.y= element_text(size= 8, face="bold.italic")) + 
  theme(legend.title=element_blank())

edu_tt= data.frame(table(member_sample$Education))%>%
         dplyr::mutate(pct= paste0(round(prop.table(Freq),4)*100,"%"))%>%
         dplyr::filter(!Var1=="")
educatn<- ggplot(edu_tt, aes(Var1, Freq, fill= Var1)) + geom_bar(stat = "identity") + 
  geom_text(aes(label= pct), vjust= -0.5, size= 3.0) +
  xlab(label= "Education") + ylab("Counts") +
  ggtitle("Count of AAA Members with Specific Education") +
  theme(plot.title= element_text(hjust= 0.5, size= 9, face="bold"),
        axis.text.x = element_text(angle = 45) ,
        axis.title.x= element_text(size= 8, face="bold.italic"),
        axis.title.y= element_text(size= 8, face="bold.italic")) + 
  theme(legend.title=element_blank())

sex_tt= data.frame(table(member_sample$Gender))%>%
           dplyr::mutate(pct= paste0(round(prop.table(Freq),4)*100,"%"))%>%
           dplyr::filter(!(Var1 %in% c("", "Unknown")))
gender<- ggplot(sex_tt, aes(Var1, Freq, fill= Var1)) + geom_bar(stat = "identity") + 
  geom_text(aes(label= pct), vjust= -0.5, size= 3.0) +
  xlab(label= "Gender") + ylab("Counts") +
  ggtitle("Count of AAA Members") +
  theme(plot.title= element_text(hjust= 0.5, size= 9, face="bold"),
        axis.text.x = element_text(angle = 45) ,
        axis.title.x= element_text(size= 8, face="bold.italic"),
        axis.title.y= element_text(size= 8, face="bold.italic")) + 
  theme(legend.title=element_blank())

problems_tt= data.frame(table(member_sample$Prob1.Code.Description))%>%
  dplyr::mutate(pct= paste0(round(prop.table(Freq),4)*100,"%"))%>%
  dplyr::filter(Var1!= "")
problms<- ggplot(problems_tt, aes(Var1, Freq, fill= Var1)) + geom_bar(stat = "identity") + 
  geom_text(aes(label= pct), vjust= -0.5, size= 3.0) +
  xlab(label= "Prob1.Code.Description") + ylab("Counts") +
  ggtitle("Count of Problem Code Description By AAA Members") +
  theme(plot.title= element_text(hjust= 0.5, size= 9, face="bold"),
        axis.text.x = element_text(angle = 45) ,
        axis.title.x= element_text(size= 8, face="bold.italic"),
        axis.title.y= element_text(size= 8, face="bold.italic")) + 
  theme(legend.title=element_blank())

member.status= data.frame(table(member_sample$Member.Status))%>%
                 dplyr::mutate(pct= paste0(round(prop.table(Freq),4)*100,"%"))%>%
                 dplyr::filter(Var1!= "")
mem.status<- ggplot(member.status, aes(Var1, Freq, fill= Var1)) + geom_bar(stat = "identity") + 
  xlab(label= "Member Status") + ylab("Counts") +
  ggtitle("Count of AAA Members By Member Status") +
  theme(plot.title= element_text(hjust= 0.5, size= 9, face="bold"),
        axis.text.x = element_text(angle = 45) ,
        axis.title.x= element_text(size= 8, face="bold.italic"),
        axis.title.y= element_text(size= 8, face="bold.italic")) +
  theme(legend.title=element_blank())





