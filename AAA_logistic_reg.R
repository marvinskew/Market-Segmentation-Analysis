

logit.inv<- function(y)1/(1 + exp(-y))
logit<-function(y) log(y/(1-y))
member_pdt_probability= data.frame()
keeps=c("Individual.Key","Household.Key" ,"FSV.CMSI.Flag", "FSV.Credit.Card.Flag","FSV.Deposit.Program.Flag", 
        "FSV.Home.Equity.Flag", "Race", "Length.Of.Residence","Home.Owner","Children","Education", "avg_credit_score", "Gender",
        "Total_ERS.ENT.Count0_3years" , "Right_Gender","Member.Tenure.Years","Member.Type","Basic.Cost","Calculated.Tow.Miles", 
        "Call.Canceled", "Motorcycle.Indicator", "Plus.Cost", "Plus.Indicator.Description","SVC.Facility.Name", "SVC.Facility.Type", "Total.Cost",                  
        "Was.Duplicated", "Was.Towed.To.AAR.Referral", "Age", "avg_income", "Calculated.Tow.Miles",
        "Mosaic.Household", "Mosaic.Global.Household", "kcl_B_IND_MosaicsGrouping", "New.Mover.Flag","Occupation.Code",               
        "Occupation.Group", "DTL.Prob1.Code.Description", "ERS.Member.Cost0_3years"
      )

keeps_wo_FSV_CMSI_Flag= c("FSV.Credit.Card.Flag", "FSV.Deposit.Program.Flag", "FSV.Home.Equity.Flag")
keeps_wo_FSV.Credit.Card.Flag= c("FSV.CMSI.Flag", "FSV.Deposit.Program.Flag", "FSV.Home.Equity.Flag")
keeps_wo_FSV.Deposit.Prg.Flag= c("FSV.CMSI.Flag", "FSV.Credit.Card.Flag", "FSV.Home.Equity.Flag")
keeps_wo_FSV.Home.Equity.Flag= c("FSV.CMSI.Flag", "FSV.Credit.Card.Flag", "FSV.Deposit.Program.Flag")

cl_by_income1= cl_by_income1%>%
                    dplyr::filter(!(Home_Owner =="Home Owner")) 
cl_by_income2= cl_by_income2%>% 
                     dplyr::filter(Member.Tenure.Years >=10)%>%
                     dplyr::filter(!(Education %in% c("Unknown")))
cl_by_income3= cl_by_income3 
                     #dplyr::filter(Length.Of.Residence >=15) #%>%
                     #dplyr::filter(avg_credit_score >=650)%>%
                     #dplyr::filter(Education %in% c("Completed College", "Graduated School"))
cl_by_income4= cl_by_income4%>%
                     dplyr::filter(Education %in% c("Completed College", "Graduated School"))%>%
                     dplyr::filter(avg_credit_score >=750)%>%
                     dplyr::filter(Home_Owner =="Home Owner")

cluster1_by_income= cl_by_income1[, colnames(cl_by_income1) %in% keeps]
cluster2_by_income= cl_by_income2[, colnames(cl_by_income2) %in% keeps]
cluster3_by_income= cl_by_income3[, colnames(cl_by_income3) %in% keeps]
cluster4_by_income= cl_by_income4[, colnames(cl_by_income4) %in% keeps]


#using cluster1_by_income
cl1_FSV_CMSI_Flag= cluster1_by_income[, !colnames(cluster1_by_income) %in% keeps_wo_FSV_CMSI_Flag] %>%mutate(
  FSV.CMSI_Flag= ifelse(FSV.CMSI.Flag=="Y", 1, 0)
)%>%
  dplyr::select(-c("FSV.CMSI.Flag"))

cl1_FSV_Credit_Card_Flag= cluster1_by_income[, !colnames(cluster1_by_income) %in% keeps_wo_FSV.Credit.Card.Flag] %>%
  mutate(FSV.Credit.Card_Flag= ifelse(FSV.Credit.Card.Flag=="Y", 1, 0)
  )%>%
  dplyr::select(-c("FSV.Credit.Card.Flag"))

cl1_FSV_Deposit_Program_Flag = cluster1_by_income[, !colnames(cluster1_by_income) %in% keeps_wo_FSV.Deposit.Prg.Flag] %>%
  mutate(FSV.Deposit.Program_Flag= ifelse(FSV.Deposit.Program.Flag=="Y", 1, 0)
  )%>%
  dplyr::select(-c("FSV.Deposit.Program.Flag"))

cl1_FSV_Home_Equity_Flag= cluster1_by_income[, !colnames(cluster1_by_income) %in% keeps_wo_FSV.Home.Equity.Flag] %>%mutate(
  FSV.Home.Equity_Flag= ifelse(FSV.Home.Equity.Flag=="Y", 1, 0)
)%>%
  dplyr::select(-c("FSV.Home.Equity.Flag"))

#using cluster2_by_income
cl2_FSV_CMSI_Flag= cluster2_by_income[, !colnames(cluster2_by_income) %in% keeps_wo_FSV_CMSI_Flag] %>%mutate(
  FSV.CMSI_Flag= ifelse(FSV.CMSI.Flag=="Y", 1, 0)
)%>%
  dplyr::select(-c("FSV.CMSI.Flag"))

cl2_FSV_Credit_Card_Flag= cluster2_by_income[, !colnames(cluster2_by_income) %in% keeps_wo_FSV.Credit.Card.Flag] %>%
  mutate(FSV.Credit.Card_Flag= ifelse(FSV.Credit.Card.Flag=="Y", 1, 0)
  )%>%
  dplyr::select(-c("FSV.Credit.Card.Flag"))

cl2_FSV_Deposit_Program_Flag = cluster2_by_income[, !colnames(cluster2_by_income) %in% keeps_wo_FSV.Deposit.Prg.Flag] %>%
  mutate(FSV.Deposit.Program_Flag= ifelse(FSV.Deposit.Program.Flag=="Y", 1, 0)
  )%>%
  dplyr::select(-c("FSV.Deposit.Program.Flag"))

cl2_FSV_Home_Equity_Flag= cluster2_by_income[, !colnames(cluster2_by_income) %in% keeps_wo_FSV.Home.Equity.Flag] %>%mutate(
  FSV.Home.Equity_Flag= ifelse(FSV.Home.Equity.Flag=="Y", 1, 0)
)%>%
  dplyr::select(-c("FSV.Home.Equity.Flag"))

#using cluster3_by_income

cl3_FSV_CMSI_Flag= cluster3_by_income[, !colnames(cluster3_by_income) %in% keeps_wo_FSV_CMSI_Flag] %>%mutate(
  FSV.CMSI_Flag= ifelse(FSV.CMSI.Flag=="Y", 1, 0)
)%>%
  dplyr::select(-c("FSV.CMSI.Flag"))

cl3_FSV_Credit_Card_Flag= cluster3_by_income[, !colnames(cluster3_by_income) %in% keeps_wo_FSV.Credit.Card.Flag] %>%
  mutate(FSV.Credit.Card_Flag= ifelse(FSV.Credit.Card.Flag=="Y", 1, 0)
  )%>%
  dplyr::select(-c("FSV.Credit.Card.Flag"))

cl3_FSV_Deposit_Program_Flag = cluster3_by_income[, !colnames(cluster3_by_income) %in% keeps_wo_FSV.Deposit.Prg.Flag] %>%
  mutate(FSV.Deposit.Program_Flag= ifelse(FSV.Deposit.Program.Flag=="Y", 1, 0)
  )%>%
  dplyr::select(-c("FSV.Deposit.Program.Flag"))

cl3_FSV_Home_Equity_Flag= cluster3_by_income[, !colnames(cluster3_by_income) %in% keeps_wo_FSV.Home.Equity.Flag] %>%mutate(
  FSV.Home.Equity_Flag= ifelse(FSV.Home.Equity.Flag=="Y", 1, 0)
)%>%
  dplyr::select(-c("FSV.Home.Equity.Flag"))

#using cluster4_by_income
cl4_FSV_CMSI_Flag= cluster4_by_income[, !colnames(cluster4_by_income) %in% keeps_wo_FSV_CMSI_Flag] %>%mutate(
  FSV.CMSI_Flag= ifelse(FSV.CMSI.Flag=="Y", 1, 0)
)%>%
  dplyr::select(-c("FSV.CMSI.Flag"))

cl4_FSV_Credit_Card_Flag= cluster4_by_income[, !colnames(cluster4_by_income) %in% keeps_wo_FSV.Credit.Card.Flag] %>%
  mutate(FSV.Credit.Card_Flag= ifelse(FSV.Credit.Card.Flag=="Y", 1, 0)
  )%>%
  dplyr::select(-c("FSV.Credit.Card.Flag"))

cl4_FSV_Deposit_Program_Flag= cluster4_by_income[, !colnames(cluster4_by_income) %in% keeps_wo_FSV.Deposit.Prg.Flag] %>%
  mutate(FSV.Deposit.Program_Flag= ifelse(FSV.Deposit.Program.Flag=="Y", 1, 0)
  )%>%
  dplyr::select(-c("FSV.Deposit.Program.Flag"))

cl4_FSV_Home_Equity_Flag= cluster4_by_income[, !colnames(cluster4_by_income) %in% keeps_wo_FSV.Home.Equity.Flag] %>%mutate(
  FSV.Home.Equity_Flag= ifelse(FSV.Home.Equity.Flag=="Y", 1, 0)
)%>%
  dplyr::select(-c("FSV.Home.Equity.Flag"))


#cluster 1
glm.cl1_FSV_CMSI_Flag=  glm(FSV.CMSI_Flag~., data= cl1_FSV_CMSI_Flag)
glm.cl1_FSV_Credit_Card_Flag= glm(FSV.Credit.Card_Flag~., data= cl1_FSV_Credit_Card_Flag)
glm.cl1_FSV_Deposit_Program_Flag= glm(FSV.Deposit.Program_Flag~., data= cl1_FSV_Deposit_Program_Flag)
glm.cl1_FSV_Home_Equity_Flag= glm(FSV.Home.Equity_Flag~., data= cl1_FSV_Home_Equity_Flag)

#cluster 2
glm.cl2_FSV_CMSI_Flag=  glm(FSV.CMSI_Flag~., data= cl2_FSV_CMSI_Flag)
glm.cl2_FSV_Credit_Card_Flag= glm(FSV.Credit.Card_Flag~., data= cl2_FSV_Credit_Card_Flag)
glm.cl2_FSV_Deposit_Program_Flag= glm(FSV.Deposit.Program_Flag~., data= cl2_FSV_Deposit_Program_Flag)
glm.cl2_FSV_Home_Equity_Flag= glm(FSV.Home.Equity_Flag~., data= cl2_FSV_Home_Equity_Flag)

#cluster 3
glm.cl3_FSV_CMSI_Flag=  glm(FSV.CMSI_Flag~., data= cl3_FSV_CMSI_Flag)
glm.cl3_FSV_Credit_Card_Flag= glm(FSV.Credit.Card_Flag~., data= cl3_FSV_Credit_Card_Flag)
glm.cl3_FSV_Deposit_Program_Flag= glm(FSV.Deposit.Program_Flag~., data= cl3_FSV_Deposit_Program_Flag)
glm.cl3_FSV_Home_Equity_Flag= glm(FSV.Home.Equity_Flag~., data= cl3_FSV_Home_Equity_Flag)

#cluster 4
glm.cl4_FSV_CMSI_Flag=  glm(FSV.CMSI_Flag~., data= cl4_FSV_CMSI_Flag)
glm.cl4_FSV_Credit_Card_Flag= glm(FSV.Credit.Card_Flag~., data= cl4_FSV_Credit_Card_Flag)
glm.cl4_FSV_Deposit_Program_Flag= glm(FSV.Deposit.Program_Flag~., data= cl4_FSV_Deposit_Program_Flag)
glm.cl4_FSV_Home_Equity_Flag= glm(FSV.Home.Equity_Flag~., data= cl4_FSV_Home_Equity_Flag)

#cluster 1 Probabilities
cl1_prob_FSV.CMSI.Flag= data.frame(augment(glm.cl1_FSV_CMSI_Flag)%>%
                          dplyr::mutate(prob_FSV.CMSI.Flag= logit.inv(glm.cl1_FSV_CMSI_Flag$fitted.values)))%>%
                          dplyr::summarise(mean_prob_FSV.CMSI.Flag= mean(prob_FSV.CMSI.Flag), 
                                        mean_prob_FSV.CMSI.Flag_gt0.5= mean(prob_FSV.CMSI.Flag[prob_FSV.CMSI.Flag > 0.5])) %>%
         dplyr::select(mean_prob_FSV.CMSI.Flag,  mean_prob_FSV.CMSI.Flag_gt0.5)


cl1_prob_FSV.Credit.Card.Flag= data.frame(augment(glm.cl1_FSV_Credit_Card_Flag)%>%
                                  dplyr::mutate(prob_FSV.Credit.Card.Flag= logit.inv(glm.cl1_FSV_Credit_Card_Flag$fitted.values)))%>%
                     dplyr::summarise(mean_prob_FSV.Credit.Card.Flag= mean(prob_FSV.Credit.Card.Flag), 
                                      mean_prob_FSV.Credit.Card.Flag_gt0.5= mean(prob_FSV.Credit.Card.Flag[prob_FSV.Credit.Card.Flag > 0.5])) %>%
                     dplyr::select(mean_prob_FSV.Credit.Card.Flag,  mean_prob_FSV.Credit.Card.Flag_gt0.5)

cl1_prob_FSV.Deposit.Program.Flag= data.frame(augment(glm.cl1_FSV_Deposit_Program_Flag)%>%
                                     dplyr::mutate(prob_FSV.Deposit.Program_Flag= logit.inv(glm.cl1_FSV_Deposit_Program_Flag$fitted.values)))%>%
                     dplyr::summarise(mean_prob_FSV.Deposit.Program.Flag= mean(prob_FSV.Deposit.Program.Flag), 
                                      mean_prob_FSV.Deposit.Program.Flag_gt0.5= mean(prob_FSV.Deposit.Program.Flag[prob_FSV.Deposit.Program.Flag > 0.5])) %>%
                                      dplyr::select(mean_prob_FSV.Deposit.Program.Flag,  mean_prob_FSV.Deposit.Program.Flag_gt0.5)

cl1_prob_FSV.Home.Equity.Flag= data.frame(augment(glm.cl1_FSV_Home_Equity_Flag)%>%
                                     dplyr::mutate(
                                       prob_FSV.Home.Equity.Flag= logit.inv(glm.cl1_FSV_Home_Equity_Flag$fitted.values)))%>%
                     dplyr::summarise(mean_prob_FSV.Home.Equity.Flag= mean(prob_FSV.Home.Equity.Flag), 
                                      mean_prob_FSV.Home.Equity.Flag_gt0.5= mean(prob_FSV.Home.Equity.Flag[prob_FSV.Home.Equity.Flag > 0.5])) %>%
                     dplyr::select(mean_prob_FSV.Home.Equity.Flag,  mean_prob_FSV.Home.Equity.Flag_gt0.5)

