
#rm(list=ls())
options(digit= 20)
pkages<-c("dplyr","broom", "tidyr", "magrittr", "MASS", "glmnet", "NbClust", "purrr", "ggplot2", "lubridate")
for(pkge in pkages){
  if(!pkge %in% installed.packages()) install.packages(pkge)
}
lapply(pkages, require, character.only= TRUE)
getwd(); setwd("C:/Users/maron/OneDrive/Desktop/rony_project")

member_sample_df= data.frame(read.csv("member_sample_LAPTOP_ARCLM27S.csv"))
member_sample= member_sample_df %>%
  mutate(Date.of.Birth= as.Date(Date.Of.Birth, format="%m/%d/%Y"),
         Age= 2020 - year(Date.of.Birth),
         avg_credit_score= strsplit(Credit.Ranges, "-") %>% lapply(., function(x){
           unlist(x) %>% as.numeric %>% mean
         }) %>%unlist,
         avg_income= strsplit(Income_range, "-") %>% lapply(., function(x){
          unlist(x) %>% as.numeric %>% mean
        }) %>%unlist,
        avg_inc_zscore= (avg_income - mean(avg_income, na.rm=TRUE))/sd(avg_income, na.rm=TRUE),
        Total_ERS.ENT.Count0_3years= ERS.ENT.Count.Year.1 + ERS.ENT.Count.Year.2 + ERS.ENT.Count.Year.3,
        ERS.Member.Cost0_3years= ERS.Member.Cost.Year.1 + ERS.Member.Cost.Year.2 + ERS.Member.Cost.Year.3,
    )

summary_stats=function(x){
  return(list(average= mean(x, na.rm=TRUE),
              median= median(x, na.rm=TRUE),
              std_dev= sd(x, na.rm=TRUE),
              num_NAs= sum(is.na(x)), 
              pct.NAs= paste0(round((sum(is.na(x))/length(x))*100,3),"%")),
              class_type= class(x)
  )
}

out<-data.frame(nClusters= NA, tot_withinss= NA)
set.seed(123)
# function to calculate total intra-cluster sum of square 
intra_ssw_cl <- function(df, vars, k) {
  ssw.totals= kmeans(na.omit(df[, colnames(df) %in% vars]), k, iter.max=100,nstart=100,
                             algorithm="Lloyd" )$tot.withinss
  return(ssw.totals)
}

loop_func= function(df, vars, n){
  for(i in 1:n){
    out[i,]<- list(i, intra_ssw_cl(df, vars, i))
  }
  return(out)
}

age_tot_ssw= loop_func(member_sample, c("Age"), 10)
avg_income_tot_ssw=loop_func(member_sample, c("avg_inc_zscore"), 10)
residence_tot_ssw=loop_func(member_sample, c("Length.Of.Residence"), 10)
Age_income_tot_ssw=loop_func(member_sample, c("Age", "avg_inc.zscore"), 10)
Age_income_tot_ssw=loop_func(member_sample, c("Age", "avg_inc.zscore"), 10)
Age_income_tot_ssw=loop_func(member_sample, c("Age", "avg_inc.zscore"), 10)

par(mfrow=c(2,2))
plot(age_tot_ssw$nClusters, age_tot_ssw$tot_withinss, 
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares",
     main="Intra-clusters SSW & Clusters\nby Age")
plot(avg_income_tot_ssw$nClusters, avg_income_tot_ssw$tot_withinss, 
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     xlim=c(min(avg_income_tot_ssw$nClusters), max(avg_income_tot_ssw$nClusters)),
     ylim=c(min( avg_income_tot_ssw$tot_withinss), max( avg_income_tot_ssw$tot_withinss)),
     ylab="Total intra-clusters sum of squares",
     main="Intra-clusters SSW & Clusters\nby Avg. Income")
plot(residence_tot_ssw$nClusters, residence_tot_ssw$tot_withinss, 
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares",
     main="Intra-clusters SSW & Clusters\nby Length of Residence")
plot(Age_income_tot_ssw$nClusters, Age_income_tot_ssw$tot_withinss, 
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     xlim=c(min(Age_income_tot_ssw$nClusters), max(Age_income_tot_ssw$nClusters)),
     ylim=c(min(Age_income_tot_ssw$tot_withinss), max(Age_income_tot_ssw$tot_withinss)),
     ylab="Total intra-clusters sum of squares",
     main="Intra-clusters SSW & Clusters\nby Age & Avg. Income")

cluster_by_Age<-kmeans(na.omit(member_sample[, "Age"]), 4,iter.max=100,nstart=50,algorithm="Lloyd")
cluster_by_income<-kmeans(na.omit(member_sample[, "avg_income"]), 4,iter.max=100,nstart=50,algorithm="Lloyd")
cluster_by_residence<-kmeans(na.omit(member_sample[, "Length.Of.Residence"]), 4,iter.max=100,nstart=50,algorithm="Lloyd")
cluster_by_Age_income<-kmeans(na.omit(member_sample[, c("Age", "avg_income")]), 4,iter.max=100,
                                                              nstart=50,algorithm="Lloyd")

member_sample %>%
  dplyr::filter(!is.na(Age))%>%
  ggplot(aes(x= Age, y = Total.Cost)) + 
  geom_point(stat = "identity", aes(color = as.factor(cluster_by_Age$cluster))) +
  scale_color_discrete(name=" ",breaks=c("1", "2", "3", "4"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")) +
  ggtitle("Market Segmentation of AAA Customers\n By Age Using K-means Clustering") +
  theme(plot.title = element_text(hjust = 0.5)) 

member_sample %>%
  dplyr::filter(!is.na(avg_income))%>%
  ggplot(aes(x= avg_income, y = Total.Cost)) + 
  geom_point(stat = "identity", aes(color = as.factor(cluster_by_income$cluster))) +
  scale_color_discrete(name=" ",breaks=c("1", "2", "3", "4"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")) +
  ggtitle("Market Segmentation of AAA Customers\n By Avg. Income Using K-means Clustering") +
  theme(plot.title = element_text(hjust = 0.5)) 

member_sample %>%
  dplyr::filter(!is.na(Length.Of.Residence))%>%
  ggplot(aes(x= Length.Of.Residence, y = Total.Cost)) + 
  geom_point(stat = "identity", aes(color = as.factor(cluster_by_residence$cluster))) +
  scale_color_discrete(name=" ",breaks=c("1", "2", "3", "4"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")) +
  ggtitle("Market Segmentation of AAA Customers\n By Length Of Residence Using 
          K-means Clustering") +
  theme(plot.title = element_text(hjust = 0.5)) 

member_sample %>%
  dplyr::filter(!is.na(Age)) %>%
  dplyr::filter(!is.na(avg_income)) %>%
     ggplot(aes(x= paste(Age,"_", "avg_income"), y= Total.Cost)) + 
     geom_point(stat= "identity", aes(color = as.factor(cluster_by_Age_income$cluster))) +
     scale_color_discrete(name=" ",breaks=c("1", "2", "3", "4"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")) +
     ggtitle("Market Segmentation of AAA Customers\n By Age & Income Using K-means Clustering") +
     theme(plot.title = element_text(hjust = 0.5))

member_sample %>%
  dplyr::filter(!is.na(Age)) %>%
  dplyr::filter(!is.na(avg_income)) %>%
  ggplot(aes(x= Age, y= Total.Cost)) + 
  geom_point(stat= "identity", aes(color = as.factor(cluster_by_Age_income$cluster))) +
  scale_color_discrete(name=" ",breaks=c("1", "2", "3", "4"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")) +
  ggtitle("Market Segmentation of AAA Customers\n By Age & Income Using K-means Clustering") +
  theme(plot.title = element_text(hjust = 0.5))

#segmentation by Age
cl_by_Age1<-member_sample[cluster_by_Age$cluster==1,] %>%
               dplyr::mutate(cluster_by_Age= 1)
cl_by_Age2<-member_sample[cluster_by_Age$cluster==2,] %>%
               dplyr::mutate(cluster_by_Age= 2)
cl_by_Age3<-member_sample[cluster_by_Age$cluster==3,] %>%
               dplyr::mutate(cluster_by_Age= 3)
cl_by_Age4<-member_sample[cluster_by_Age$cluster==4,] %>%
               dplyr::mutate(cluster_by_Age= 4)

#segmentation by Income
cl_by_income1<-member_sample[cluster_by_income$cluster==1,] %>%
  dplyr::mutate(cluster_by_income= 1)
cl_by_income2<-member_sample[cluster_by_income$cluster==2,] %>%
  dplyr::mutate(cluster_by_income= 2)
cl_by_income3<-member_sample[cluster_by_income$cluster==3,] %>%
  dplyr::mutate(cluster_by_income= 3)
cl_by_income4<-member_sample[cluster_by_income$cluster==1,] %>%
  dplyr::mutate(cluster_by_income= 4)

super_elite_summary4= cl_by_income4 %>%
  dplyr::filter(Education %in% c("Completed College", "Graduated School"))%>%
  dplyr::filter(avg_credit_score >700)%>%
  dplyr::filter(Home_Owner =="Home Owner") %>%
  dplyr::summarise(Avg.Age= mean(Age, na.rm=T),
                   Avg.credit_score= mean(avg_credit_score),
                   Avg.income= mean(avg_income),
                   Avg.LoR= mean(Length.Of.Residence),
                   Avg.Tenure= mean(Member.Tenure.Years),
                   premier.revenue= sum(Premier.Cost, na.rm=T),
                   basic.revenue= sum(Basic.Cost, na.rm=T),
                   total_cost= sum(Total.Cost),
                   Avg_of_roadside_calls= mean(Total_ERS.ENT.Count0_3years, na.rm=T),
                   Avg_of_ERS.Member.Cost0_3years= mean(ERS.Member.Cost0_3years, na.rm=T)) %>%
  dplyr::select(Avg.Age, Avg.credit_score, Avg.income, Avg.LoR, Avg.Tenure, premier.revenue, basic.revenue, 
                total_cost, Avg_of_roadside_calls, Avg_of_ERS.Member.Cost0_3years)


elite_summary3= cl_by_income3 %>%
  dplyr::filter(Length.Of.Residence >=15)%>%
  dplyr::filter(Education %in% c("Completed College", "Graduated School"))%>%
  dplyr::summarise(Avg.Age= mean(Age, na.rm=T),
                   Avg.credit_score= mean(avg_credit_score, na.rm=T),
                   Avg.income= mean(avg_income),
                   Avg.LoR= mean(Length.Of.Residence),
                   Avg.Tenure= mean(Member.Tenure.Years),
                   premier.revenue= sum(Premier.Cost, na.rm=T),
                   basic.revenue= sum(Basic.Cost, na.rm=T),
                   total_cost= sum(Total.Cost),
                   Avg_of_roadside_calls= mean(Total_ERS.ENT.Count0_3years, na.rm=T),
                   Avg_of_ERS.Member.Cost0_3years= mean(ERS.Member.Cost0_3years, na.rm=T)) %>%
  dplyr::select(Avg.Age, Avg.credit_score, Avg.income, Avg.LoR, Avg.Tenure, premier.revenue, basic.revenue, 
                total_cost, Avg_of_roadside_calls, Avg_of_ERS.Member.Cost0_3years)



#segmentation by Length of Residence
cluster_by_LoR1<-member_sample[cluster_by_residence$cluster==1,] %>%
  dplyr::mutate(cluster_by_Len_of_Residence= 1)
cluster_by_LoR2<-member_sample[cluster_by_residence$cluster==2,] %>%
  dplyr::mutate(cluster_by_Len_of_Residence= 2)
cluster_by_LoR3<-member_sample[cluster_by_residence$cluster==3,] %>%
  dplyr::mutate(cluster_by_Len_of_Residence= 3)
cluster_by_LoR4<-member_sample[cluster_by_residence$cluster==4,] %>%
  dplyr::mutate(cluster_by_Len_of_Residence= 4)

#segmentation by Age & Income 
cl_by_income_Age1<-member_sample[cluster_by_Age_income$cluster==1,] %>%
  dplyr::mutate(cluster_by_income_Age= 1)
cl_by_income_Age2<-member_sample[cluster_by_Age_income$cluster==2,] %>%
  dplyr::mutate(cluster_by_income_Age= 2)
cl_by_income_Age3<-member_sample[cluster_by_Age_income$cluster==3,] %>%
  dplyr::mutate(cluster_by_income_Age= 3)
cl_by_income_Age4<-member_sample[cluster_by_Age_income$cluster==4,] %>%
  dplyr::mutate(cluster_by_income_Age= 4)

cl_by_Age= bind_rows(cl_by_Age1, cl_by_Age2, cl_by_Age3,cl_by_Age4)
cl_by_income= bind_rows(cl_by_income1, cl_by_income2, cl_by_income3, cl_by_income4)
cl_by_len_of_Residence= bind_rows(cluster_by_LoR1, cluster_by_LoR2, cluster_by_LoR3, cluster_by_LoR4)
cl_by_Age_income= bind_rows(cl_by_income_Age1, cl_by_income_Age2, cl_by_income_Age3,cl_by_income_Age4)

write.csv(cl_by_Age, file=paste0(getwd(),"/","output", "/","cluster_by_Age.csv"))
write.csv(cl_by_income, file=paste0(getwd(),"/","output", "/","cluster_by_Income.csv"))
write.csv(cl_by_len_of_Residence, file=paste0(getwd(),"/","output", "/","cluster_by_Len_of_Resid.csv"))
write.csv(cl_by_Age_income, file=paste0(getwd(),"/","output", "/","cluster_by_Age_Income.csv"))

    

