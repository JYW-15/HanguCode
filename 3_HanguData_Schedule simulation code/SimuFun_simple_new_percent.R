ShowType<-function(df,TimeClass){
  # define a function to show the basic statistical information
  TPercent=c() 
  for(type in sort(unique(as.character(df$Type)))){
    number<-sum(df$Type==type)
    total<-length(df$Type)
    percent=100*number/total # percent(%)
    mean = mean(df[[TimeClass]][df$Type==type])
    std = sd(df[[TimeClass]][df$Type==type])
    cv = std/mean
    median = median(df[[TimeClass]][df$Type==type], na.rm = FALSE)
    num_to_16 = 16*percent/100 
    TPercent=rbind(TPercent,cbind(type,number,percent,mean,std,cv,median,num_to_16)) # get the data frame form of statistical information
  }
  head(TPercent)
}

Obj_fun<-function(CommonServTime,schd,rule_name,n,Ci,Co,sec,PRINT = F){
  # define Obj_fun to get the cost and PKI value of each rule 
  
  R_mat = schd2R_mat(CommonServTime,schd) # realized service time matrix obtained from CommonServTime
  rKPI = SimKPIH(R_mat,schd$X_Vec) #raw KPI (W_Mat,T_Vec,Idle_Vec)

  # Time related KPI computing
  KPI = rKPI2KPIH(schd,rKPI=rKPI,plan_completion = 4*60L*(60/sec),PRINT = PRINT) 
  # KPI$excess_wait_rate = sum(Wait>=excess_wait_threshhold)/(n_samp*n)*100
  # KPI$extreme_wait_rate = sum(Wait>=extreme_wait_threshhold)/(n_samp*n)*100
  # KPI$excess_OT_rate = sum(OT>=excess_OT_threshhold)/n_samp*100
  # KPI$extreme_OT_rate = sum(OT>=extreme_OT_threshhold)/n_samp*100
  # KPI$wait_mean = mean(Wait); KPI$wait_sd = sd(Wait)
  # KPI$OT_mean = mean(OT); KPI$OT_sd = sd(OT)
  # KPI$Idle_mean = mean(rKPI$Idle_Vec); KPI$Idle_sd = sd(rKPI$Idle_Vec))
  
  # Cost related KPI computing
  #Cw=1
  #Ci :unit time cost coefficient of idle time
  #Co :unit time cost coefficient of over time
  
  cost = n*KPI$wait_mean+Ci*KPI$Idle_mean+Co*KPI$OT_mean #mean cost per day
    
  Cost_KPI<-list(Cost=cost,KPI=KPI)
  if(PRINT){
    print(sprintf('mean cost for rule:%s is %.3f',rule_name,cost)) #control the print option
  }
  return(Cost_KPI)
}

RuleCost_fun<-function(order_index,Scenario_list,CommonServTime,n,Ci,Co,sec,PRINT = F){
  # define function to record all the rules records about the rule name, cost and KPI
  temp_name = Scenario_list$Type[[order_index]] 
  rule_name = paste(temp_name[1:length(temp_name)], collapse="") #get the rule name that can be used in Obj_fun
  schd = list(Type=Scenario_list$Type[[order_index]], X_Vec = Scenario_list$X_Vec[[order_index]])
  Cost_KPI = Obj_fun(CommonServTime,schd,rule_name,n,Ci,Co,sec,PRINT = F)
  Record<-list(Rule=rule_name,Cost=Cost_KPI$Cost,KPI=Cost_KPI$KPI)
  return(Record)
}


Slot_TimeKPI<-function(Record,CommonServTime,X_Vec,rKPI_Bench){
  schd = list(Type=strsplit(Record$Rule, "")[[1]], X_Vec = X_Vec)
  # define function to record all the rules records about the rule name, cost and KPI
  R_mat = schd2R_mat(CommonServTime,schd) # realized service time matrix obtained from CommonServTime
  rKPI = SimKPIH(R_mat,X_Vec) #raw KPI (W_Mat,T_Vec,Idle_Vec)
  # Time related KPI computing
  KPI = rKPI2KPIH(schd,rKPI=rKPI,plan_completion = 4*60L*(60/sec),PRINT = F)
  Wait_List=apply(rKPI[["W_Mat"]], 2, mean)
  Idle_List=apply(rKPI[["Idle_Mat"]], 2, mean)
  Wait_List_Ben=apply(rKPI_Bench[["W_Mat"]], 2, mean)
  Idle_List_Ben=apply(rKPI_Bench[["Idle_Mat"]], 2, mean)
  Slot_Time_KPI<-list(Wait=Wait_List,Idle=Idle_List,W_Bench=Wait_List_Ben,I_Bench=Idle_List_Ben)
  return(Slot_Time_KPI)
}

Get_wait_vec<-function(CommonServTime,Record,X_Vec){
  #simulated raw KPI which has W_Mat
  schd= list(Type=strsplit(Record$Rule, "")[[1]],X_Vec= X_Vec)
  rKPI <- schd2SimKPIH(CommonServTime, schd)
  wait_vec = apply(rKPI$W_Mat, 2, mean) # can be a scale of access equality for each slot
  return(wait_vec)
}


rKPI2KPIH <- function(schd,rKPI, #simulated raw KPI which has W_Mat and T_Vec
                     plan_completion = 4*60L*(60/sec), # 4-hour session
                     excess_wait_threshhold = 15L*(60/sec), # 15 min for excess wait
                     extreme_wait_threshhold = 45L*(60/sec), # 45 min for extreme wait
                     excess_OT_threshhold = 30L*(60/sec), # 30 min for excess OT
                     extreme_OT_threshhold = 45L*(60/sec), #45 min for extreme OT
                     PRINT = F){
  # return summary of rate of excess_wait and extreme_wait
  # as well as the % of sessions that have excess and extreme OT
  # if PRINT = T, also print out the summary
  KPI = list()
  n_samp = length(rKPI$T_Vec)
  stopifnot(dim(rKPI$W_Mat)[1]==n_samp)
  n = dim(rKPI$W_Mat)[2] #number of patients
  OT = pmax(rKPI$T_Vec-plan_completion,0)
  Wait = as.vector(rKPI$W_Mat)
  KPI$excess_wait_rate = sum(Wait>=excess_wait_threshhold)/(n_samp*n)*100
  KPI$extreme_wait_rate = sum(Wait>=extreme_wait_threshhold)/(n_samp*n)*100
  KPI$excess_OT_rate = sum(OT>=excess_OT_threshhold)/n_samp*100
  KPI$extreme_OT_rate = sum(OT>=extreme_OT_threshhold)/n_samp*100
  KPI$wait_mean = mean(Wait)
  #KPI$wait_sd = sd(Wait) # can be a scale of access equality 
  KPI$wait_sd = sd(apply(rKPI$W_Mat, 2, mean)) # can be a scale of access equality for each slot
  KPI$OT_mean = mean(OT); KPI$OT_sd = sd(OT)
  KPI$Idle_mean = mean(rKPI$Idle_Vec); KPI$Idle_sd = sd(rKPI$Idle_Vec)
  
  # new performance index (optional) ######################################################
  # KPI$OT_ppm = KPI$OT_mean/n #mean overtime per patient
  # KPI$Idle_ppm = KPI$Idle_mean/n #mean idle time per patient
  # 
  # wait_typemean = list()
  # idle_typemean = list()
  # over_typemean = list()
  # for (t in unique(schd$Type)){
  #   wait_typemean[t] = mean(rKPI$W_Mat[,schd$Type==t])
  #   idle_typemean[t] = mean(rKPI$Idle_Mat[,schd$Type==t])
  #   over_typemean[t] = mean(rKPI$Over_Mat[,schd$Type==t])
  # }
  # 
  # KPI$Wait_typemean = wait_typemean #mean wait time per patient per type
  # KPI$Idle_typemean = idle_typemean #mean idle time per patient per type
  # KPI$Over_typemean = over_typemean #mean over time per patient per type
  # 
  # if(PRINT){
  #   msg = sprintf('plan_end=%f, excess/extreme_wait_threshhold=(%f, %f), excess/extreme_OT_threshhold=(%f, %f)',
  #                 plan_completion, excess_wait_threshhold, extreme_wait_threshhold,
  #                 excess_OT_threshhold, extreme_OT_threshhold)
  #   print(msg)
  #   msg = sprintf('mean and sd of Wait= (%.2f, %.2f)',KPI$wait_mean,KPI$wait_sd)
  #   msg = sprintf('%s, mean and sd of OT= (%.2f, %.2f)',msg, KPI$OT_mean,KPI$OT_sd)
  #   msg = sprintf('%s, mean and sd of Idle= (%.2f, %.2f).',msg, KPI$Idle_mean,KPI$Idle_sd)
  #   print(msg)
  #   print(sprintf('excess and extreme wait rate=(%.2f%%, %.2f%%)',
  #                 KPI$excess_wait_rate,KPI$extreme_wait_rate))
  #   print(sprintf('excess and extreme OT rate=(%.2f%%, %.2f%%)',
  #                 KPI$excess_OT_rate,KPI$extreme_OT_rate))
  # }
  ##############################################################################
  return(KPI)
}

SimKPIH <- function(R_mat,X_Vec,plan_completion = 4*60L*(60/sec)){
  # Simulate raw KPI for all realized samples
  # Totally n_samp sample observations, then nrow(R_mat)=n_samp
  # For each row k, R_mat[k,i] is the service time of patient i
  # The inter-arrival time is vector: X_Vec, where X_Vec[i] is A[i+1]-A[i]
  # Given Realized Service Time Matrix R_mat and appointment Inter-arrival X_vec 
  # return rKPI: which the following fields: 
  #   W_Mat is the waiting time matrix
  #   T_Vec is the total realized session end time vector (one entry per replication)
  #   Idle_Vec is the session doctor's Idle time vector (one entry per replication)
  my.dim<-dim(R_mat)
  n_samp=my.dim[1];
  n=my.dim[2]; #number of patients
  assertthat::assert_that(length(X_Vec)==(n-1), 
                          msg='Length of Interarrival time !=(n-1)!!');
  ZERO_Vec=matrix(0,nrow=n_samp,ncol=1)
  W_Mat=matrix(0,nrow=n_samp,ncol=n); #Initialize waiting time matrix
  for(ii in 1:(n-1)){
    W_Mat[,ii+1]=pmax(ZERO_Vec,
                      W_Mat[,ii,drop=F]+R_mat[,ii,drop=F]-X_Vec[ii]); 
    # See the above analysis for this relationship
  }
  
  Idle_Mat=matrix(0,nrow=n_samp,ncol=n); #Initialize idle time matrix
  X_Vec_plus=rep(15*(60/sec), n)
  for(ii in 1:n){
    Idle_Mat[,ii]=pmax(ZERO_Vec,
                       X_Vec_plus[ii]-(W_Mat[,ii,drop=F]+R_mat[,ii,drop=F])); 
    # See the above analysis for this relationship
  }

  T_Vec=sum(X_Vec)+(W_Mat[,n]+R_mat[,n])
  #Idle_Mat[,n]=pmax(ZERO_Vec,plan_completion-T_Vec) #Doctors finish their work after serving all patients
  
  Over_Mat=matrix(0,nrow=n_samp,ncol=n); #Initialize over time matrix
  Over_Mat[,c(1:n-1)]=W_Mat[,c(2:n)]
  Over_Mat[,n]=pmax(ZERO_Vec,T_Vec-plan_completion)
  
  last_start_Vec = sum(X_Vec) + W_Mat[,n] #starting time of the last patient for all replications
  # doctor Idle time is the positive part of the gap between last patient's 
  # starting time and all the other patients service time. See Eq (5) of Chen and Robinson (2014 POM)
  Idle_Vec = pmax(last_start_Vec - rowSums(R_mat[,-n,drop=F]),0) 
  rKPI<-list(W_Mat=W_Mat,Idle_Mat=Idle_Mat,Over_Mat=Over_Mat,T_Vec=T_Vec, Idle_Vec = Idle_Vec) # raw KPI
  return(rKPI)
}

schd2SimKPIH <- function(CommonServTime, schd){
  # given the list CommonServTime and a schedule 'schd'
  # This one actually simulate and return the raw KPI 
  # The schedule 'schd' should have two components: Type and X_Vec (in seconds)
  # It first calls the schd2R_mat to form the Realized service time matrix
  # and then call SimKPI
  R_mat = schd2R_mat(CommonServTime,schd)
  rKPI = SimKPIH(R_mat,schd$X_Vec)
  return(rKPI)
}

Result_save <- function(dff,class_index,ClassType,OptRule,Impro_rule_rate1,Impro_perform_rate1){
  
  # save the result as a given format
  dff[class_index,]['Description']=ClassType
  dff[class_index,]['Opt_rule']=OptRule$Name
  dff[class_index,]['Opt_cost']=OptRule$Cost
  dff[class_index,]['Impro_rule_rate']=Impro_rule_rate1
  dff[class_index,]['Improve_rate']=Impro_perform_rate1
  dff[class_index,]['WAIT']=OptRule$KPI$wait_mean
  dff[class_index,]['OVER']=OptRule$KPI$OT_ppm
  dff[class_index,]['IDLE']=OptRule$KPI$Idle_ppm
  dff[class_index,]['WAIT_sd']=OptRule$KPI$wait_sd 
  dff[class_index,]['WAIT_typemean_A']=OptRule$KPI$Wait_typemean$A
  dff[class_index,]['WAIT_typemean_B']=OptRule$KPI$Wait_typemean$B
  dff[class_index,]['WAIT_typemean_C']=OptRule$KPI$Wait_typemean$C
  dff[class_index,]['WAIT_typemean_D']=OptRule$KPI$Wait_typemean$D
  dff[class_index,]['OVER_typemean_A']=OptRule$KPI$Over_typemean$A
  dff[class_index,]['OVER_typemean_B']=OptRule$KPI$Over_typemean$B
  dff[class_index,]['OVER_typemean_C']=OptRule$KPI$Over_typemean$C
  dff[class_index,]['OVER_typemean_D']=OptRule$KPI$Over_typemean$D
  dff[class_index,]['IDLE_typemean_A']=OptRule$KPI$Idle_typemean$A
  dff[class_index,]['IDLE_typemean_B']=OptRule$KPI$Idle_typemean$B
  dff[class_index,]['IDLE_typemean_C']=OptRule$KPI$Idle_typemean$C
  dff[class_index,]['IDLE_typemean_D']=OptRule$KPI$Idle_typemean$D
  return(dff)
}

RuleNameTrans<-function(Scenario_list){
  rule_name_list=list()
  for (order_index in 1:length(Scenario_list$Type)){
  temp_name = Scenario_list$Type[[order_index]]
  rule_name = paste(temp_name[1:length(temp_name)], collapse="")
  rule_name_list[order_index]=rule_name
  }
  return(rule_name_list)
}

PlotCostMat<-function(plotCorMat,Scenario1_list,Scenario2_list){
  # plot cost matrix  with color
  rule_name_list1=RuleNameTrans(Scenario1_list)
  rule_name_list2=RuleNameTrans(Scenario2_list)
  rownames(plotCorMat)=rule_name_list1
  colnames(plotCorMat)=rule_name_list2
  col1<-colorRampPalette(c( "#9B2226", "#AE2021","#BB3E03","#CA6702","#FFFFFF",
                              "#90e0ef", "#0077b6","#023e8a","#001219"))
    
  #cost matrix color
  library(corrplot)
  corrplot(as.matrix(plotCorMat),
             #method = 'color',
             is.corr=F,
             tl.cex = 0.6,
             cl.cex = 0.5,
             col = col1(10),
             tl.col = "black"
             #addCoef.col = "grey"
    )
    
    return(plotCorMat)
    
}

FullSeq_2Types_SimKPIH <- function(CommonServTime,Type1, Type2, n1, n2, X_Vec,
                                  plan_completion = 4*60L*(60/sec), # 4-hour session
                                  excess_wait_threshhold = 15L*(60/sec), # 15 min for excess wait
                                  extreme_wait_threshhold = 45L*(60/sec), # 45 min for extreme wait
                                  excess_OT_threshhold = 30L*(60/sec), # 30 min for excess OT
                                  extreme_OT_threshhold = 45L*(60/sec) #45 min for extreme OT
){
  # Input: 
  #     CommonServTime List
  #     Type1 and Type2 as type characters with n1 Type1 and n2 Type2
  # Output: Dataframe of simulation of full sequencing, which as the following fields
  #         Policy--character to indicate which positions are of Type1
  #         mean.wait, sd.wait=numeric(n_seq), these are mean and sd of waiting
  #         mean.T, sd.T, these are mean and sd of server ending time T
  #         mean.OT, sd.OT, these are mean and sd of server over time
  #         mean.Idle, sd.Idle, these are mean and sd of server Idle
  #         excess.wait.rate, extreme.wait.rate, these are mean excess and extreme wait
  #         excess.OT.rate, extreme.OT.rate, these are mean excess and extreme OT
  n = n1 + n2
  assertthat::assert_that(length(X_Vec)==n-1, msg= paste0('length(X_Vec)!=',n-1))
  assertthat::assert_that(is.subset(c(Type1,Type2), names(CommonServTime)),
                          msg= 'Type1 and Type2 should be in names(CommonServTime)')
  n_seq = choose(n,n1)
  
  df <- data.frame(Policy=character(n_seq),
                   Rule_name=character(n_seq),
                   #Cost=numeric(n_seq),
                   mean.wait=numeric(n_seq), sd.wait=numeric(n_seq), 
                   #mean.T = numeric(n_seq), sd.T=numeric(n_seq), 
                   mean.OT=numeric(n_seq), sd.OT=numeric(n_seq),
                   mean.Idle=numeric(n_seq), sd.Idle=numeric(n_seq),
                   excess.wait.rate=numeric(n_seq), extreme.wait.rate=numeric(n_seq),
                   excess.OT.rate=numeric(n_seq), extreme.OT.rate=numeric(n_seq),  
                   stringsAsFactors = F)
  seq.matrix <- SeqMat_2Types(n1, n2, n)
  assertthat::assert_that(n_seq==nrow(seq.matrix))
  #Simulate
  for(ii in 1:n_seq){
    Type1.index=rep(FALSE,n)
    Type1.index[seq.matrix[ii,]]=TRUE
    Policy = paste(paste(seq.matrix[ii,],collapse=','),Type1,sep=';')
    Name = rep(Type2,n)
    Name[Type1.index]=Type1
    Rule_name = paste(Name[1:length(Name)], collapse="")
    schd= list(Type=rep(Type2,n),X_Vec= X_Vec)
    schd$Type[Type1.index] = Type1
    rKPI <- schd2SimKPIH(CommonServTime, schd)
    KPI <- rKPI2KPIH(schd,rKPI)
    df[ii,] = data.frame(Policy,
                         Rule_name,
                         #n*KPI$wait_mean+Ci*KPI$Idle_mean+Co*KPI$OT_mean,
                         KPI$wait_mean,KPI$wait_sd,
                         #mean(rKPI$T_Vec), sd(rKPI$T_Vec),
                         KPI$OT_mean, KPI$OT_sd,
                         KPI$Idle_mean, KPI$Idle_sd,
                         KPI$excess_wait_rate,KPI$extreme_wait_rate,
                         KPI$excess_OT_rate,KPI$extreme_OT_rate,
                         stringsAsFactors = F)
  }
  return(df)
}

FullSeq2Scenariolist<-function(CommonServTime,Type1, Type2, n1, n2, X_Vec){
  df = FullSeq_2Types_SimKPIH(CommonServTime,Type1, Type2, n1, n2, X_Vec)
  Scenario1_list=list()
  name_list =sapply(df[['Rule_name']],function(x) strsplit(x,''))
  # Type value
  Scenario1_list$Type <-name_list
  # X_Vec value         
  Scenario1_list$X_Vec=rep(list(rep(15*(60/sec), n-1)),length(Scenario1_list$Type))
  #output
  output=list(Scenariolist=Scenario1_list,FullSeq=df)
  return(output)
}

FullSeq_3Types_SimKPIH <- function(CommonServTime,Type1, Type2, Type3, n1, n2, n3, X_Vec,
                                  plan_completion = 4*3600L/sec, # 4-hour session
                                  excess_wait_threshhold = 15*60L/sec, # 15 min for excess wait
                                  extreme_wait_threshhold = 45*60L/sec, # 45 min for extreme wait
                                  excess_OT_threshhold = 30*60L/sec, # 30 min for excess OT
                                  extreme_OT_threshhold = 45*60L/sec #45 min for extreme OT
){
  
  # Input: 
  #     CommonServTime List
  #     Type1, Type2  and Type3 as type characters with n1 Type1, n2 Type2  and n3 Type3
  # Output: Dataframe of simulation of full sequencing, which as the following fields
  #         Policy--character to indicate which positions are of Type1 and Type2
  #         mean.wait, sd.wait=numeric(n_seq), these are mean and sd of waiting
  #         mean.T, sd.T, these are mean and sd of server ending time T
  #         mean.OT, sd.OT, these are mean and sd of server over time
  #         mean.Idle, sd.Idle, these are mean and sd of server Idle
  #         excess.wait.rate, extreme.wait.rate, these are mean excess and extreme wait
  #         excess.OT.rate, extreme.OT.rate, these are mean excess and extreme OT
  # Note a Policy like '1;A-2,3,4;B' means 1 is of type A, and 2,3, 4 are of type B, 
  # and the rest are of the third Type.
  n = n1 + n2 + n3
  assertthat::assert_that(length(X_Vec)==n-1, msg= paste0('length(X_Vec)!=',n-1))
  assertthat::assert_that(is.subset(c(Type1,Type2,Type3), names(CommonServTime)),
                          msg= 'Type1, Type2 and Type3 should be in names(CommonServTime)')
  ## if one of n1, n2, n3 = 0, raise an error, and ask the user to use FullSeq_2Types_SimKPIH instead!
  assertthat::assert_that(n1 != 0 && n2 != 0 && n3 != 0,  
                          msg = 'One of n1, n2 and n3 is zero! Use FullSeq_2Types_SimKPIH!!!')
  
  # Idea:
  # first choose n1 slots from n and mark as type 1, and loop over these choices -- outer loop
  # secondly, amid the (n-n1) slots, choose n2 of them for type 2 -- inner loop
  # key: map the position of type 2 in (n-n1) slots to the original index
  # This can be done by remembering the indices of those (n-n1) slots
  n_seq = choose(n,n1) * choose(n-n1,n2) 
  
  df <- data.frame(Policy=character(n_seq),
                   Rule_name=character(n_seq),
                   mean.wait=numeric(n_seq), sd.wait=numeric(n_seq), 
                   #mean.T = numeric(n_seq), sd.T=numeric(n_seq), 
                   mean.OT=numeric(n_seq), sd.OT=numeric(n_seq),
                   mean.Idle=numeric(n_seq), sd.Idle=numeric(n_seq),
                   excess.wait.rate=numeric(n_seq), extreme.wait.rate=numeric(n_seq),
                   excess.OT.rate=numeric(n_seq), extreme.OT.rate=numeric(n_seq),  
                   stringsAsFactors = F)
  seq.matrix_Type1 <- SeqMat_2Types(n1, n2+n3, n) # n1 slots for Type1
  seq.matrix_Type2 <- SeqMat_2Types(n2, n3, n2+n3) # n2 slots for Type2
  assertthat::assert_that(n_seq == nrow(seq.matrix_Type1)* nrow(seq.matrix_Type2))
  #Simulation
  ind = 0
  for(ii in 1:nrow(seq.matrix_Type1)){# outer loop, Type1 position
    Type1.index=rep(FALSE,n)
    Type1.index[seq.matrix_Type1[ii,]]=TRUE
    Type2_3.original = which(!Type1.index) # the positions of (n-n1) slots for Type 2 and Type 3
    for(jj in 1:nrow(seq.matrix_Type2)){ # inner loop, Type2 position
      ind = ind + 1 # remember the id in total sequencing for result df
      Type2.index=rep(FALSE,n)
      Type2.index[Type2_3.original[seq.matrix_Type2[jj,]]] = TRUE
      Policy = paste( paste(paste(seq.matrix_Type1[ii,],collapse=','),Type1,sep=';'),
                      paste(paste(which(Type2.index),collapse = ','), Type2,sep=';'),
                      sep= '-'
      )
      Name = rep(Type3,n)
      Name[Type1.index]=Type1
      Name[Type2.index]=Type2
      Rule_name = paste(Name[1:length(Name)], collapse="")
      schd= list(Type = rep(Type3,n), X_Vec= X_Vec)                
      schd$Type[Type1.index] = Type1
      schd$Type[Type2.index] = Type2
      rKPI <- schd2SimKPIH(CommonServTime, schd)
      KPI <- rKPI2KPIH(schd,rKPI)
      df[ind,] = data.frame(Policy,
                            Rule_name,
                            KPI$wait_mean,KPI$wait_sd,
                            #mean(rKPI$T_Vec), sd(rKPI$T_Vec),
                            KPI$OT_mean, KPI$OT_sd,
                            KPI$Idle_mean, KPI$Idle_sd,
                            KPI$excess_wait_rate,KPI$extreme_wait_rate,
                            KPI$excess_OT_rate,KPI$extreme_OT_rate,
                            stringsAsFactors = F)
    }
  }
  return(df)
}


FullSeq3Scenariolist<-function(CommonServTime,Type1, Type2, Type3, n1, n2, n3, X_Vec){
  df = FullSeq_3Types_SimKPIH(CommonServTime,Type1, Type2, Type3, n1, n2, n3, X_Vec)
  Scenario1_list=list()
  name_list =sapply(df[['Rule_name']],function(x) strsplit(x,''))
  # Type value
  Scenario1_list$Type <-name_list
  # X_Vec value         
  Scenario1_list$X_Vec=rep(list(rep(15*(60/sec), n-1)),length(Scenario1_list$Type))
  #output
  output=list(Scenariolist=Scenario1_list,FullSeq=df)
  return(output)
}

FullSeq2Result<-function(CommonServTime,Cost_KPI_Benchmark,Type1, Type2, n1, n2, X_Vec){
  Scenario1=FullSeq2Scenariolist(CommonServTime,Type1, Type2, n1, n2, X_Vec)
  Scenario1_list=Scenario1$Scenariolist
  Scenario1_FullSeq=Scenario1$FullSeq
  ### Calculate Cost_list of all the rules
  n11 = length(Scenario1_list$Type) #the length of Scenario1 rules list
  Scenario1_record_list = list(Rule=list(),KPI=list(),Cost = list()) # define a void list to store the kpi data and rule information.
  ### Result
  for(order_index1 in 1:n11){
    i = order_index1
    Record1=RuleCost_fun(i,Scenario1_list,CommonServTime,n,Ci,Co,sec,PRINT = F)
    # cost record
    Scenario1_record_list$Cost[i] = Record1$Cost
    # Rule record
    Scenario1_record_list$Rule[i] = Record1$Rule
    # KPI record
    Scenario1_record_list$KPI[i]= list(Record1$KPI)
  }
  ## Peformance Index test
  Improvement1 = Cost_KPI_Benchmark$Cost-as.numeric(Scenario1_record_list$Cost) # Improvement based on benchmark rule
  Impro_rule_rate1 = length(Improvement1[Improvement1>0])/(n11) # the proportion of the rules that can improvement cost based on benchmark rule.
  Impro_perform_rate1 = max(Improvement1)/Cost_KPI_Benchmark$Cost #the improvement ratio of the best rule in our setting based on benchmark rule
  Optimal_rule_Index = which.max(Improvement1)
  Optimal_rule_Name = Scenario1_record_list$Rule[[Optimal_rule_Index]]
  Optimal_rule_KPI = Scenario1_record_list$KPI[[Optimal_rule_Index]]
  Optimal_rule_Cost = Scenario1_record_list$Cost[[Optimal_rule_Index]]
  OptRule1 = list(Name=Optimal_rule_Name,KPI=Optimal_rule_KPI,Cost = Optimal_rule_Cost)
  
  txt_note1 =sprintf('Classification type: %s. The Impro_rule_rate1 is %.2f%%, Impro_perform_rate1 is %.2f%%;The Optimal_rule is: %s, its cost is %.2f',
                     ClassType,Impro_rule_rate1*100,Impro_perform_rate1*100,
                     Optimal_rule_Name,Optimal_rule_Cost) #output document
  
  print(txt_note1)
  
  return(OptRule1)
}


SimuResult1<-function(Scenario1_list,Cost_KPI_Benchmark,ClassType,class_index,ResultPath){
  # result type1 : single rules list, no rules matrix
  n1 = length(Scenario1_list$Type) #the length of Scenario1 rules list
  Scenario1_record_list = list(Rule=list(),KPI=list(),Cost = list()) # define a void list to store the kpi data and rule information.
  for(order_index1 in 1:n1){
    i = order_index1
    Record1=RuleCost_fun(i,Scenario1_list,CommonServTime,n,Ci,Co,sec,PRINT = F)
    # cost record
    Scenario1_record_list$Cost[i] = Record1$Cost
    # Rule record
    Scenario1_record_list$Rule[i] = Record1$Rule
    # KPI record
    Scenario1_record_list$KPI[i]= list(Record1$KPI)
  }
  # Peformance Index test
  Improvement = Cost_KPI_Benchmark$Cost-as.numeric(Scenario1_record_list$Cost) # Improvement based on benchmark rule
  Impro_rule_rate = length(Improvement[Improvement>0])/(n1) # the proportion of the rules that can improvement cost based on benchmark rule.
  Impro_perform_rate = max(Improvement)/Cost_KPI_Benchmark$Cost #the improvement ratio of the best rule in our setting based on benchmark rule
  Optimal_rule_Index = which.max(Improvement)#obtain the index of max improvement rule
  Optimal_rule_Name = Scenario1_record_list$Rule[[Optimal_rule_Index]]#obtain the name of max improvement rule
  Optimal_rule_KPI = Scenario1_record_list$KPI[[Optimal_rule_Index]]#obtain the PKI of max improvement rule
  Optimal_rule_Cost = Scenario1_record_list$Cost[[Optimal_rule_Index]]#obtain the cost of max improvement rule
  OptRule = list(Name=Optimal_rule_Name,KPI=Optimal_rule_KPI,Cost = Optimal_rule_Cost) # the optimal rule
  
  txt_note =sprintf('Classification type: %s. The Impro_rule_rate is %.2f%%, Impro_perform_rate is %.2f%%; The Optimal_rule is: %s, its cost is %.2f',ClassType,Impro_rule_rate*100,Impro_perform_rate*100,Optimal_rule_Name,Optimal_rule_Cost) #output document
  
  print(txt_note)
  
  # Save the result
  dff <- read.csv(ResultPath,stringsAsFactors = F)
  dff=Result_save(dff,class_index,ClassType,OptRule,Impro_rule_rate,Impro_perform_rate)# call the Result_save function
  write.csv(dff,file=ResultPath,row.names = F)
  Result=list(txt_note=txt_note,Scenario1_record_list=Scenario1_record_list,OptRule=OptRule)
  return(Result)
}

SimuResult2<-function(Scenario1_list,Scenario2_list,Cost_KPI_Benchmark,ClassType,class_index,ResultPath,a,df){
  # result type1 : single rules list, no rules matrix
  n1 = length(Scenario1_list$Type)
  n2 = length(Scenario2_list$Type)           
  Cost_mat = matrix(nrow =n1, ncol=n2)
  Rule_mat = matrix(nrow =n1, ncol=n2)
  KPI_list=list(KPI_list1=list(),KPI_list2=list())
  ### Result
  for(order_index1 in 1:n1){
    i = order_index1
    Record1=RuleCost_fun(i,Scenario1_list,CommonServTime,n,Ci,Co,sec,PRINT = F)
    KPI_list$KPI_list1[i]= list(Record1$KPI)
    for(order_index2 in 1:n2){
      j = order_index2
      Record2=RuleCost_fun(j,Scenario2_list,CommonServTime,n,Ci,Co,sec,PRINT = F)
      
      # cost record
      Cost_mat[i,j] = a1*Record1$Cost+a2*Record2$Cost
      # Rule record
      Rule_mat[i,j] = paste(Record1$Rule, Record2$Rule, sep = " & ")
      # KPI record
      KPI_list$KPI_list2[j]= list(Record2$KPI)
      
    }
  }
  ## Peformance Index test
  Improvement = Cost_KPI_Benchmark$Cost-Cost_mat# Improvement based on benchmark rule
  Impro_rule_rate = length(Improvement[Improvement>0])/(n1*n2) # the proportion of the rules that can improvement cost based on benchmark rule.
  Impro_perform_rate = max(Improvement)/Cost_KPI_Benchmark$Cost #the improvement ratio of the best rule in our setting based on benchmark rule
  Optimal_rule_Index = which(Improvement == max(Improvement),arr.ind = T) # matrix index[a,b]
  Optimal_rule_Name = Rule_mat[Optimal_rule_Index]
  Optimal_rule_KPI1 = KPI_list$KPI_list1[[Optimal_rule_Index[1]]]
  Optimal_rule_KPI2 = KPI_list$KPI_list2[[Optimal_rule_Index[2]]]
  
  Name_list = names(Optimal_rule_KPI1)
  
  Optimal_KPI_value = a1*(as.numeric(Optimal_rule_KPI1[c(1:12)]))+a2*(as.numeric(Optimal_rule_KPI2[c(1:12)]))
  
  Optimal_rule_KPI=list()
  
  for (n in 1:(length(Name_list)-3)){
    Optimal_rule_KPI[[Name_list[n]]]= Optimal_KPI_value[n]
    
  }
  
  for (n in c(13:15)){
    for(m in str_sort(unique(df$Type))){
      k1=Optimal_rule_KPI1[[n]][[m]]
      k2=Optimal_rule_KPI2[[n]][[m]]
      if (is_null(k1)) k1=0 #to avoid the write error of the result
      if (is_null(k2)) k2=0
      Optimal_rule_KPI[[Name_list[n]]][[m]]=a1*k1+a2*k2
    }
  }
  
  Optimal_rule_Cost = Cost_mat[Optimal_rule_Index]
  OptRule = list(Name=Optimal_rule_Name,KPI=Optimal_rule_KPI,Cost = Optimal_rule_Cost)
  
  txt_note =sprintf('Classification type: %s. The Impro_rule_rate is %.2f%%, Impro_perform_rate is %.2f%%; The Optimal_rule is: %s, its cost is %.2f',ClassType,Impro_rule_rate*100,Impro_perform_rate*100,Optimal_rule_Name,Optimal_rule_Cost) #output document
  
  print(txt_note)
  
  # Save the result
  dff <- read.csv(ResultPath,stringsAsFactors = F)
  dff=Result_save(dff,class_index,ClassType,OptRule,Impro_rule_rate,Impro_perform_rate)# call the Result_save function
  write.csv(dff,file=ResultPath,row.names = F)
  
  Result=list(txt_note=txt_note,Cost_mat=Cost_mat,Rule_mat=Rule_mat,KPI_list=KPI_list,OptRule=OptRule)
  return(Result)
}


SeqRule3 <- function(TypeList, TypeCount){
  #  versions 3 current optimal
  TypeList=TypeList[TypeCount!=0] #1
  TypeCount=TypeCount[TypeCount!=0] #2 order is important!!
  
  #Function
  ##Function Group
  Group<-function(TypeList, TypeCount, RepCount_init=1){
    if (length(TypeList) != length(TypeCount)) {stop("Invalid input parameters.")}
    if (length(TypeList)==1) {
      GroupList=rep(TypeList, times = TypeCount)
    }else{
      CountLimit <- floor(TypeCount/RepCount_init)
      m <- lapply(CountLimit, function(x) seq_len(x))
      GroupList <- do.call(expand.grid, m)
      GroupList <- as.matrix(GroupList)
      
      RuleList <- lapply(seq_len(nrow(GroupList)), function(i) {
        TypeCount_New <- GroupList[i, ]
        Group <- rep(TypeList, times = TypeCount_New)
        Group <- paste0(Group, collapse = "")
        RepeatCount <- min(TypeCount %/% TypeCount_New)
        Groups <- paste0(rep(Group, RepeatCount), collapse = "")
        TypeCount_LO <- TypeCount - TypeCount_New * RepeatCount
        TypeList_LO <- unique(rep(TypeList, times = TypeCount_LO))
        TypeCount_LO <- TypeCount_LO[TypeCount_LO != 0]
        LeftList <- rep(TypeList_LO, times = TypeCount_LO)
        List1 <- paste0(c(Groups, LeftList), collapse = "")
        List2 <- paste0(c(LeftList, Groups), collapse = "")
        c(List1, List2)
      })
      RuleList <- unlist(RuleList)
      GroupList=lapply(RuleList, function(p) p)
    }
    return(unique(GroupList))
  }
  
  
  # Main 
  if (length(TypeList) != length(TypeCount)) {stop("Invalid input parameters.")}
  assertthat::assert_that(all(TypeList==sort(TypeList)), 
                          msg='TypeList must be ordered !!');
  
  if (length(TypeList)<=1) {
    RuleList=rep(TypeList, times = TypeCount)
    RuleList=paste0(RuleList,collapse="")
  }else{
    RuleList <- list()
    #Group
    GroupList=Group(TypeList, TypeCount)
    RuleList <- c(RuleList, GroupList)
  }
  lengths <- sapply(RuleList, nchar)
  if (length(unique(lengths)) > 1) {
    stop("String lengths are not consistent.")
  }
  return(unique(RuleList))
}


SeqRule2 <- function(TypeList, TypeCount){
  # versions 2 Simple Group+Alter+OneClassAtATime
  TypeList=TypeList[TypeCount!=0]
  TypeCount=TypeCount[TypeCount!=0] #order is important!!
  
  #Function
  ##Function OneClassAtATime
  OneClassAtATime <- function(TypeList, TypeCount) {
    
    if (length(TypeList) != length(TypeCount)) {stop("Invalid input parameters.")}
    if (length(TypeList)==1) {
      OneList=rep(TypeList, times = TypeCount)
    }else{
      x <- rep(TypeList, times = TypeCount)
      # Group by letter category
      groups <- split(x, x)
      # Generate full permutations for each group as a whole
      temp=permn(groups)
      perms <- lapply(temp, unlist)
      OneList <- lapply(perms, paste0, collapse="")
    }
    return(OneList)
  }
  
  ##Function Alter
  Alter <- function(TypeList, TypeCount) {
    if (length(TypeList) != length(TypeCount)) {stop("Invalid input parameters.")}
    if (length(TypeList)==1) {
      AlterList=rep(TypeList, times = TypeCount)
    }else{
      repcount=min(TypeCount)
      FullAlter1=permn(TypeList)
      RepAlter=lapply(FullAlter1, rep, repcount)
      AlterList=lapply(RepAlter, paste0, collapse="")
    }
    return(AlterList)
  }
  
  ##Function CartProd
  CartProd <- function(List1, List2) {
    # Cartesian_product
    len1 <- nchar(List1)
    len2 <- nchar(List2)
    
    if (is.null(List1) && !(is.null(List2))) {
      MixList <- lapply(List2, function(p) p)
    } else if (!(is.null(List1)) && is.null(List2)) {
      MixList <- lapply(List1, function(p) p)
    } else {
      MixList_temp1 <- expand.grid(List1, List2)
      MixList_temp2 <- paste0(MixList_temp1$Var1, MixList_temp1$Var2)
      MixList <- lapply(MixList_temp2, function(p) p)
    }
    return(MixList)
  }
  
  ##Function Group
  Group<-function(TypeList, TypeCount, RepCount_init=2){
    if (length(TypeList) != length(TypeCount)) {stop("Invalid input parameters.")}
    if (length(TypeList)==1) {
      GroupList=rep(TypeList, times = TypeCount)
    }else{
      CountLimit <- floor(TypeCount/RepCount_init)
      m <- lapply(CountLimit, function(x) seq_len(x))
      GroupList <- do.call(expand.grid, m)
      GroupList <- as.matrix(GroupList)
      
      RuleList <- lapply(seq_len(nrow(GroupList)), function(i) {
        TypeCount_New <- GroupList[i, ]
        Group <- rep(TypeList, times = TypeCount_New)
        Group <- paste0(Group, collapse = "")
        RepeatCount <- min(TypeCount %/% TypeCount_New)
        Groups <- paste0(rep(Group, RepeatCount), collapse = "")
        TypeCount_LO <- TypeCount - TypeCount_New * RepeatCount
        TypeList_LO <- unique(rep(TypeList, times = TypeCount_LO))
        TypeCount_LO <- TypeCount_LO[TypeCount_LO != 0]
        LeftList <- rep(TypeList_LO, times = TypeCount_LO)
        List1 <- paste0(c(Groups, LeftList), collapse = "")
        List2 <- paste0(c(LeftList, Groups), collapse = "")
        c(List1, List2)
      })
      RuleList <- unlist(RuleList)
      GroupList=lapply(RuleList, function(p) p)
    }
    return(unique(GroupList))
  }
  
  
  # Main 
  if (length(TypeList) != length(TypeCount)) {stop("Invalid input parameters.")}
  assertthat::assert_that(all(TypeList==sort(TypeList)), 
                          msg='TypeList must be ordered !!');
  
  if (length(TypeList)<=1) {
    RuleList=rep(TypeList, times = TypeCount)
    RuleList=paste0(RuleList,collapse="")
  }else{
    RuleList <- list()
    # OneClassAtATime
    OneList=OneClassAtATime(TypeList, TypeCount)
    RuleList <- c(RuleList, OneList)
    
    ##Alter
    AlterList=Alter(TypeList, TypeCount)
    ## LeftOver
    TypeCount_LO=TypeCount-min(TypeCount)*rep(c(1),length(TypeList))
    TypeList_LO=unique(rep(TypeList, times = TypeCount_LO))
    TypeCount_LO=TypeCount_LO[TypeCount_LO!=0]
    LeftOverList=SeqRule(TypeList_LO, TypeCount_LO)
    # MixList
    MixList1=CartProd(AlterList, LeftOverList)
    RuleList <- c(RuleList, MixList1)
    MixList2=CartProd(LeftOverList, AlterList)
    RuleList <- c(RuleList, MixList2)
    
    #Group
    GroupList=Group(TypeList, TypeCount)
    RuleList <- c(RuleList, GroupList)
  }
  lengths <- sapply(RuleList, nchar)
  if (length(unique(lengths)) > 1) {
    stop("String lengths are not consistent.")
  }
  return(unique(RuleList))
}


SeqRule <- function(TypeList, TypeCount){
  # versions 4
  TypeList=TypeList[TypeCount!=0]
  TypeCount=TypeCount[TypeCount!=0] #order is important!!
  #Function
  ##Function OneClassAtATime
  OneClassAtATime <- function(TypeList, TypeCount) {
    if (length(TypeList) != length(TypeCount)) {stop("Invalid input parameters.")}
    if (length(TypeList)==1) {
      OneList=paste0(rep(TypeList, times = TypeCount),collapse="")
    }else{
      x <- rep(TypeList, times = TypeCount)
      #x <- c("A", "B", "C", "C", "C", "B", "B", "A", "A", "A")
      # Group by letter category
      groups <- split(x, x)
      # Generate full permutations for each group as a whole
      temp=permn(groups)
      perms <- lapply(temp, unlist)
      OneList <- lapply(perms, paste0, collapse="")
    }
    return(OneList)
  }
  
  ##Function CartProd
  CartProd <- function(List1, List2) {
    # Cartesian_product
    len1 <- nchar(List1)
    len2 <- nchar(List2)
    
    if (is.null(List1) && !(is.null(List2))) {
      MixList <- lapply(List2, function(p) p)
    } else if (!(is.null(List1)) && is.null(List2)) {
      MixList <- lapply(List1, function(p) p)
    } else {
      MixList_temp1 <- expand.grid(List1, List2)
      MixList_temp2 <- paste0(MixList_temp1$Var1, MixList_temp1$Var2)
      MixList <- lapply(MixList_temp2, function(p) p)
    }
    return(MixList)
  }
  
  ##Function Group
  Group<-function(TypeList, TypeCount, RepCount_init=1){
    if (length(TypeList) != length(TypeCount)) {stop("Invalid input parameters.")}
    if (length(TypeList)==1) {
      GroupList=rep(TypeList, times = TypeCount)
    }else{
      RuleList=list()
      #RepCount_init=2
      CountLimit <- floor(TypeCount/RepCount_init)
      m <- lapply(CountLimit, function(x) seq_len(x))
      GroupList <- do.call(expand.grid, m)
      GroupList <- as.matrix(GroupList)
      
      RuleList <- lapply(seq_len(nrow(GroupList)), function(i) {
        TypeCount_New <- GroupList[i, ]
        Group=OneClassAtATime(TypeList,TypeCount_New)
        RepeatCount <- min(TypeCount %/% TypeCount_New)
        Groups=lapply(Group, function(p) rep(p,RepeatCount))
        Groups=lapply(Groups, function(p) paste0(p, collapse=""))
        
        TypeCount_LO <- TypeCount - TypeCount_New * RepeatCount
        TypeList_LO <- unique(rep(TypeList, times = TypeCount_LO))
        TypeCount_LO <- TypeCount_LO[TypeCount_LO != 0]
        LeftList=OneClassAtATime(TypeList_LO,TypeCount_LO)
        # Concatenate the remaining type and the repeated type into a new string
        MixList1=CartProd(Groups, LeftList)
        RuleList <- c(RuleList, MixList1)
        MixList2=CartProd(LeftList, Groups)
        RuleList <- c(RuleList, MixList2)
      })
      RuleList <- unlist(RuleList)
      GroupList=lapply(RuleList, function(p) p)
    }
    return(unique(GroupList))
  }
  
  
  # Main 
  if (length(TypeList) != length(TypeCount)) {stop("Invalid input parameters.")}
  assertthat::assert_that(all(TypeList==sort(TypeList)), 
                          msg='TypeList must be ordered !!');
  
  if (length(TypeList)<=1) {
    RuleList=rep(TypeList, times = TypeCount)
    RuleList=paste0(RuleList,collapse="")
  }else{
    RuleList <- list()
    #Group
    RuleList=Group(TypeList, TypeCount)
    #RuleList <- c(RuleList, GroupList)
  }
  lengths <- sapply(RuleList, nchar)
  if (length(unique(lengths)) > 1) {
    stop("String lengths are not consistent.")
  }
  return(unique(RuleList))
}


String2chars <- function(RuleList){
  RuleList_uni<-unique(RuleList)
  RuleList_chars<-strsplit(unlist(RuleList_uni), "")
  return(RuleList_chars)
}


CostValueFunc<-function(df,Ci,Co,n){
  df$Cost = n*df$mean.wait+Ci*df$mean.Idle+Co*df$mean.OT  #update the cost in df for every different Co,Ci value
  return(df)
}

Plot_Wait_Over<-function(Rule_mat,Cost_mat,KPI_list,a1,a2,n1,n2,Cost_KPI_Benchmark,Result_list2){
  # for mix rules
  Wait_Over <- data.frame(Rule=character(n1*n2),
                          Cost=numeric(n1*n2),
                          Wait_mean=numeric(n1*n2),
                          Wait_sd=numeric(n1*n2),
                          OT_mean=numeric(n1*n2),
                          OT_sd=numeric(n1*n2),
                          Idle_mean=numeric(n1*n2),
                          Idle_sd=numeric(n1*n2),
                          excess_wait_rate=numeric(n1*n2),
                          extreme_wait_rate=numeric(n1*n2),
                          excess_OT_rate=numeric(n1*n2),
                          extreme_OT_rate=numeric(n1*n2),
                          stringsAsFactors = F)
  for (i in c(1:n1)){
    for (j in c(1:n2)){
    ii=(j-1)*n1+i
    Wait_Over[ii,] <- data.frame(Rule_mat[i,j],
                                 Cost_mat[i,j],
                                 a1*KPI_list$KPI_list1[[i]]$wait_mean+a2*KPI_list$KPI_list2[[j]]$wait_mean,
                                 a1*KPI_list$KPI_list1[[i]]$wait_sd+a2*KPI_list$KPI_list2[[j]]$wait_sd,
                                 a1*KPI_list$KPI_list1[[i]]$OT_mean+a2*KPI_list$KPI_list2[[j]]$OT_mean,
                                 a1*KPI_list$KPI_list1[[i]]$OT_sd+a2*KPI_list$KPI_list2[[j]]$OT_sd,
                                 a1*KPI_list$KPI_list1[[i]]$Idle_mean+a2*KPI_list$KPI_list2[[j]]$Idle_mean,
                                 a1*KPI_list$KPI_list1[[i]]$Idle_sd+a2*KPI_list$KPI_list2[[j]]$Idle_sd,
                                 a1*KPI_list$KPI_list1[[i]]$excess_wait_rate+a2*KPI_list$KPI_list2[[j]]$excess_wait_rate,
                                 a1*KPI_list$KPI_list1[[i]]$extreme_wait_rate+a2*KPI_list$KPI_list2[[j]]$extreme_wait_rate,
                                 a1*KPI_list$KPI_list1[[i]]$excess_OT_rate+a2*KPI_list$KPI_list2[[j]]$excess_OT_rate,
                                 a1*KPI_list$KPI_list1[[i]]$extreme_OT_rate+a2*KPI_list$KPI_list2[[j]]$extreme_OT_rate,
                                 stringsAsFactors = F)
    
    }
  }
  ii=(n1*n2)+1
  Wait_Over[ii,] <- data.frame("FCFA",
                               1,#Cost_KPI_Benchmark$Cost
                               Cost_KPI_Benchmark$KPI$wait_mean,
                               Cost_KPI_Benchmark$KPI$wait_sd,
                               Cost_KPI_Benchmark$KPI$OT_mean,
                               Cost_KPI_Benchmark$KPI$OT_sd,
                               Cost_KPI_Benchmark$KPI$Idle_mean,
                               Cost_KPI_Benchmark$KPI$Idle_sd,
                               Cost_KPI_Benchmark$KPI$excess_wait_rate,
                               Cost_KPI_Benchmark$KPI$extreme_wait_rate,
                               Cost_KPI_Benchmark$KPI$excess_OT_rate,
                               Cost_KPI_Benchmark$KPI$extreme_OT_rate,
                               stringsAsFactors = F)
  
  ii=(n1*n2)+2
  Wait_Over[ii,] <- data.frame(Result_list2$OptRule$Name,
                               Result_list2$OptRule$Cost,
                               Result_list2$OptRule$KPI$wait_mean,
                               Result_list2$OptRule$KPI$wait_sd,
                               Result_list2$OptRule$KPI$OT_mean,
                               Result_list2$OptRule$KPI$OT_sd,
                               Result_list2$OptRule$KPI$Idle_mean,
                               Result_list2$OptRule$KPI$Idle_sd,
                               Result_list2$OptRule$KPI$excess_wait_rate,
                               Result_list2$OptRule$KPI$extreme_wait_rate,
                               Result_list2$OptRule$KPI$excess_OT_rate,
                               Result_list2$OptRule$KPI$extreme_OT_rate,
                               stringsAsFactors = F)
  
  Optimal_rule_Index = which(Cost_mat == min(Cost_mat),arr.ind = F) # matrix index[a,b]
  Optimal_rule_Name = Rule_mat[Optimal_rule_Index]

  P1<-ggplot(Wait_Over, aes(x=Wait_mean, y=OT_mean,color=Cost)) +
    geom_point(alpha=0.5,size=5) +labs(x='WAIT',y='OT')+
    annotate(geom = "text",x=Cost_KPI_Benchmark$KPI$wait_mean+0.02,y=Cost_KPI_Benchmark$KPI$OT_mean+0.15,label="FCFA",hjust="left",col = 'red')+
    annotate(geom = "point",x=Cost_KPI_Benchmark$KPI$wait_mean,y=Cost_KPI_Benchmark$KPI$OT_mean,colour="red",size=3)+
    annotate(geom = "text",x=Wait_Over[Optimal_rule_Index,'Wait_mean']+0.2,y=Wait_Over[Optimal_rule_Index,'OT_mean']+0.02,label=Optimal_rule_Name,hjust="left",col = 'green')+
    annotate(geom = "point",x=Wait_Over[Optimal_rule_Index,'Wait_mean'],y=Wait_Over[Optimal_rule_Index,'OT_mean'],colour="green",size=3)+
    annotate(geom = "text",x=Result_list2$OptRule$KPI$wait_mean+0.15,y=Result_list2$OptRule$KPI$OT_mean+0.02,label=Result_list2$OptRule$Name,hjust="left",col = 'orange')+
    annotate(geom = "point",x=Result_list2$OptRule$KPI$wait_mean,y=Result_list2$OptRule$KPI$OT_mean,colour="orange",size=3)+
    scale_size_continuous(range = c(5, 10))#Control the maximum and minimum bubbles and adjust the relative size of bubbles
  
  return(P1)
  
}

Plot_Wait_Over_Triple<-function(Rule_array,Cost_array,KPI_list,a1,a2,a3,n1,n2,n3,Cost_KPI_Benchmark,Result_list2){
  Wait_Over <- data.frame(Rule=character(n1*n2*n3),
                          Cost=numeric(n1*n2*n3),
                          Wait_mean=numeric(n1*n2*n3),
                          Wait_sd=numeric(n1*n2*n3),
                          OT_mean=numeric(n1*n2*n3),
                          OT_sd=numeric(n1*n2*n3),
                          Idle_mean=numeric(n1*n2*n3),
                          Idle_sd=numeric(n1*n2*n3),
                          excess_wait_rate=numeric(n1*n2*n3),
                          extreme_wait_rate=numeric(n1*n2*n3),
                          excess_OT_rate=numeric(n1*n2*n3),
                          extreme_OT_rate=numeric(n1*n2*n3),
                          stringsAsFactors = F)
  for (i in c(1:n1)){
    for (j in c(1:n2)){
      for (k in c(1:n3)){
      ii=(k-1)*(n1*n2)+(j-1)*n1+i
      Wait_Over[ii,] <- data.frame(Rule_array[i,j,k],
                                   Cost_array[i,j,k],
                                   a1*KPI_list$KPI_list1[[i]]$wait_mean+a2*KPI_list$KPI_list2[[j]]$wait_mean+a3*KPI_list$KPI_list3[[k]]$wait_mean,
                                   a1*KPI_list$KPI_list1[[i]]$wait_sd+a2*KPI_list$KPI_list2[[j]]$wait_sd+a3*KPI_list$KPI_list3[[k]]$wait_sd,
                                   a1*KPI_list$KPI_list1[[i]]$OT_mean+a2*KPI_list$KPI_list2[[j]]$OT_mean+a3*KPI_list$KPI_list3[[k]]$OT_mean,
                                   a1*KPI_list$KPI_list1[[i]]$OT_sd+a2*KPI_list$KPI_list2[[j]]$OT_sd+a3*KPI_list$KPI_list3[[k]]$OT_sd,
                                   a1*KPI_list$KPI_list1[[i]]$Idle_mean+a2*KPI_list$KPI_list2[[j]]$Idle_mean+a3*KPI_list$KPI_list3[[k]]$Idle_mean,
                                   a1*KPI_list$KPI_list1[[i]]$Idle_sd+a2*KPI_list$KPI_list2[[j]]$Idle_sd+a3*KPI_list$KPI_list3[[k]]$Idle_sd,
                                   a1*KPI_list$KPI_list1[[i]]$excess_wait_rate+a2*KPI_list$KPI_list2[[j]]$excess_wait_rate+a3*KPI_list$KPI_list3[[k]]$excess_wait_rate,
                                   a1*KPI_list$KPI_list1[[i]]$extreme_wait_rate+a2*KPI_list$KPI_list2[[j]]$extreme_wait_rate+a3*KPI_list$KPI_list3[[k]]$extreme_wait_rate,
                                   a1*KPI_list$KPI_list1[[i]]$excess_OT_rate+a2*KPI_list$KPI_list2[[j]]$excess_OT_rate+a3*KPI_list$KPI_list3[[k]]$excess_OT_rate,
                                   a1*KPI_list$KPI_list1[[i]]$extreme_OT_rate+a2*KPI_list$KPI_list2[[j]]$extreme_OT_rate+a3*KPI_list$KPI_list3[[k]]$extreme_OT_rate,
                                   stringsAsFactors = F)
      }
    }
  }
  ii=(n1*n2*n3)+1
  Wait_Over[ii,] <- data.frame("FCFA",
                               1,#Cost_KPI_Benchmark$Cost
                               Cost_KPI_Benchmark$KPI$wait_mean,
                               Cost_KPI_Benchmark$KPI$wait_sd,
                               Cost_KPI_Benchmark$KPI$OT_mean,
                               Cost_KPI_Benchmark$KPI$OT_sd,
                               Cost_KPI_Benchmark$KPI$Idle_mean,
                               Cost_KPI_Benchmark$KPI$Idle_sd,
                               Cost_KPI_Benchmark$KPI$excess_wait_rate,
                               Cost_KPI_Benchmark$KPI$extreme_wait_rate,
                               Cost_KPI_Benchmark$KPI$excess_OT_rate,
                               Cost_KPI_Benchmark$KPI$extreme_OT_rate,
                               stringsAsFactors = F)
  
  ii=(n1*n2*n3)+2 #New/Return
  Wait_Over[ii,] <- data.frame(Result_list2$OptRule$Name,
                               Result_list2$OptRule$Cost,
                               Result_list2$OptRule$KPI$wait_mean,
                               Result_list2$OptRule$KPI$wait_sd,
                               Result_list2$OptRule$KPI$OT_mean,
                               Result_list2$OptRule$KPI$OT_sd,
                               Result_list2$OptRule$KPI$Idle_mean,
                               Result_list2$OptRule$KPI$Idle_sd,
                               Result_list2$OptRule$KPI$excess_wait_rate,
                               Result_list2$OptRule$KPI$extreme_wait_rate,
                               Result_list2$OptRule$KPI$excess_OT_rate,
                               Result_list2$OptRule$KPI$extreme_OT_rate,
                               stringsAsFactors = F)
  
  Optimal_rule_Index = which(Cost_array == min(Cost_array),arr.ind = F) # matrix index [a*b]
  Optimal_rule_Name = Rule_array[Optimal_rule_Index]
  
  P1<-ggplot(Wait_Over, aes(x=Wait_mean, y=OT_mean,color=Cost)) +
    geom_point(alpha=0.5,size=5) +labs(x='WAIT',y='OT')+
    annotate(geom = "text",x=Cost_KPI_Benchmark$KPI$wait_mean+0.02,y=Cost_KPI_Benchmark$KPI$OT_mean+0.15,label="FCFA",hjust="left",col = 'red')+
    annotate(geom = "point",x=Cost_KPI_Benchmark$KPI$wait_mean,y=Cost_KPI_Benchmark$KPI$OT_mean,colour="red",size=3)+
    annotate(geom = "text",x=Wait_Over[Optimal_rule_Index,'Wait_mean']+0.2,y=Wait_Over[Optimal_rule_Index,'OT_mean']+0.02,label=Optimal_rule_Name,hjust="left",col = 'green')+
    annotate(geom = "point",x=Wait_Over[Optimal_rule_Index,'Wait_mean'],y=Wait_Over[Optimal_rule_Index,'OT_mean'],colour="green",size=3)+
    annotate(geom = "text",x=Result_list2$OptRule$KPI$wait_mean+0.15,y=Result_list2$OptRule$KPI$OT_mean+0.02,label=Result_list2$OptRule$Name,hjust="left",col = 'orange')+
    annotate(geom = "point",x=Result_list2$OptRule$KPI$wait_mean,y=Result_list2$OptRule$KPI$OT_mean,colour="orange",size=3)+
    scale_size_continuous(range = c(5, 10))#Control the maximum and minimum bubbles and adjust the relative size of bubbles
  
  return(P1)
  
}


Optcost_MixRule<-function(ClassType,CommonServTime,schd_Benchmark,n,Ci,Co,sec,df1_FullSeq,df2_FullSeq,a1,a2,NewReturn_Flag=T,PRINT=T){
  #print('Schedule Benchmark: all general type')
  rule_name = 'Benchmark'
  Cost_KPI_Benchmark=Obj_fun(CommonServTime,schd_Benchmark,rule_name,n,Ci,Co,sec,PRINT = F) #get the KPI of Benchmark
  Scenario1_list=list()
  Scenario2_list=list()
  
  df1_FullSeq=CostValueFunc(df1_FullSeq,Ci,Co,n) #update the cost in df for every different Co value
  
  Scenario1_mincost_rulename = df1_FullSeq$Rule_name[which.min(df1_FullSeq$Cost)]
  Scenario1_list$Type <-strsplit(Scenario1_mincost_rulename,'')
  Scenario1_list$X_Vec=rep(list(rep(15*(60/sec), n-1)),length(Scenario1_list$Type))
  
  
  df2_FullSeq=CostValueFunc(df2_FullSeq,Ci,Co,n) #update the cost in df for every different Co value
  
  Scenario2_mincost_rulename = df2_FullSeq$Rule_name[which.min(df2_FullSeq$Cost)]
  Scenario2_list$Type <-strsplit(Scenario2_mincost_rulename,'')
  Scenario2_list$X_Vec=rep(list(rep(15*(60/sec), n-1)),length(Scenario2_list$Type))
  
  n1 = length(Scenario1_list$Type)
  n2 = length(Scenario2_list$Type)           
  Cost_mat = matrix(nrow =n1, ncol=n2)
  Rule_mat = matrix(nrow =n1, ncol=n2)
  KPI_list=list(KPI_list1=list(),KPI_list2=list())
  
  ### Result
  for(order_index1 in 1:n1){
    i = order_index1
    Record1=RuleCost_fun(i,Scenario1_list,CommonServTime,n,Ci,Co,sec,PRINT = F)
    KPI_list$KPI_list1[i]= list(Record1$KPI)
    for(order_index2 in 1:n2){
      j = order_index2
      Record2=RuleCost_fun(j,Scenario2_list,CommonServTime,n,Ci,Co,sec,PRINT = F)
      # KPI record
      KPI_list$KPI_list2[j]= list(Record2$KPI)
      
      # cost record
      Cost_mat[i,j] = a1*Record1$Cost+a2*Record2$Cost
      # Rule record
      Rule_mat[i,j] = paste(Record1$Rule, Record2$Rule, sep = " & ")
      
      
    }
  }
  
  ## Peformance Index test
  Cost_bench_mat = Cost_mat/Cost_KPI_Benchmark$Cost
  Optimal_rule_Index = which(Cost_bench_mat == min(Cost_bench_mat),arr.ind = T) # matrix index[a,b]
  Optimal_rule_Name = Rule_mat[Optimal_rule_Index]
  
  Optimal_rule_KPI1 = KPI_list$KPI_list1[[Optimal_rule_Index[1]]] #KPI of Scenario 1
  Optimal_rule_KPI2 = KPI_list$KPI_list2[[Optimal_rule_Index[2]]] #KPI of Scenario 2
  
  Name_list = names(Optimal_rule_KPI1)
  Optimal_rule_KPI=list()
  
  ######################optional#########################
  #Optimal_KPI_value = a1*(as.numeric(Optimal_rule_KPI1[c(1:12)]))+a2*(as.numeric(Optimal_rule_KPI2[c(1:12)]))
  
  # for (nn in 1:(length(Name_list)-3)){
  #   Optimal_rule_KPI[[Name_list[nn]]]= Optimal_KPI_value[nn]
  #   
  # }
  ################################################
  
  Optimal_KPI_value = a1*(as.numeric(Optimal_rule_KPI1[c(1:10)]))+a2*(as.numeric(Optimal_rule_KPI2[c(1:10)]))
  
  for (nn in 1:(length(Name_list))){
    Optimal_rule_KPI[[Name_list[nn]]]= Optimal_KPI_value[nn]
    
  }
  
  #########################optional###########################
  # for (nn in c(13:15)){
  #   for(mm in str_sort(unique(df$Type))){
  #     k1=Optimal_rule_KPI1[[nn]][[mm]]
  #     k2=Optimal_rule_KPI2[[nn]][[mm]]
  #     if (is_null(k1)) k1=0 #to avoid the write error of the result
  #     if (is_null(k2)) k2=0
  #     Optimal_rule_KPI[[Name_list[nn]]][[mm]]=a1*k1+a2*k2
  #   }
  # }
  #####################################################
  
  Optimal_rule_Cost = Cost_bench_mat[Optimal_rule_Index]
  OptRule = list(Name=Optimal_rule_Name,KPI=Optimal_rule_KPI,Cost = Optimal_rule_Cost)
  
  if (PRINT==T) {
    if(NewReturn_Flag==T){
      txt_note =sprintf('Classification type: %s. When Ci=%.1f,Co=%.1f,the Optimal New_Return rule is: %s, its cost is %.3f',ClassType,Ci,Co,Optimal_rule_Name,Optimal_rule_Cost) #output document
      
    }
    else{
      txt_note =sprintf('Classification type: %s. When Ci=%.1f,Co=%.1f,the Optimal combination rule is: %s, its cost is %.3f',ClassType,Ci,Co,Optimal_rule_Name,Optimal_rule_Cost) #output document
    
      }
    print(txt_note)
 
  }
  
  Result=list(Cost_mat=Cost_mat,Rule_mat=Rule_mat,KPI_list=KPI_list,OptRule=OptRule,Cost_KPI_Benchmark=Cost_KPI_Benchmark,n1=n1,n2=n2)
  return(Result)
}

Optcost_TripleRule<-function(ClassType,CommonServTime,schd_Benchmark,n,Ci,Co,sec,df1_FullSeq,df2_FullSeq,df3_FullSeq,a1,a2,a3,PRINT=T){
  #print('Schedule Benchmark: all general type')
  rule_name = 'Benchmark'
  Cost_KPI_Benchmark=Obj_fun(CommonServTime,schd_Benchmark,rule_name,n,Ci,Co,sec,PRINT = F) #get the KPI of Benchmark
  Scenario1_list=list()
  Scenario2_list=list()
  Scenario3_list=list()
  
  df1_FullSeq=CostValueFunc(df1_FullSeq,Ci,Co,n) #update the cost in df for every different Co value
  
  Scenario1_mincost_rulename = df1_FullSeq$Rule_name[which.min(df1_FullSeq$Cost)]
  Scenario1_list$Type <-strsplit(Scenario1_mincost_rulename,'')
  Scenario1_list$X_Vec=rep(list(rep(15*(60/sec), n-1)),length(Scenario1_list$Type))
  
  
  df2_FullSeq=CostValueFunc(df2_FullSeq,Ci,Co,n) #update the cost in df for every different Co value
  
  Scenario2_mincost_rulename = df2_FullSeq$Rule_name[which.min(df2_FullSeq$Cost)]
  Scenario2_list$Type <-strsplit(Scenario2_mincost_rulename,'')
  Scenario2_list$X_Vec=rep(list(rep(15*(60/sec), n-1)),length(Scenario2_list$Type))
  
  df3_FullSeq=CostValueFunc(df3_FullSeq,Ci,Co,n) #update the cost in df for every different Co value
  
  Scenario3_mincost_rulename = df3_FullSeq$Rule_name[which.min(df3_FullSeq$Cost)]
  Scenario3_list$Type <-strsplit(Scenario3_mincost_rulename,'')
  Scenario3_list$X_Vec=rep(list(rep(15*(60/sec), n-1)),length(Scenario3_list$Type))
  
  
  n1 = length(Scenario1_list$Type)
  n2 = length(Scenario2_list$Type)
  n3 = length(Scenario3_list$Type)
  Cost_array=array(data = NA,dim = c(n1,n2,n3))
  Rule_array=array(data = NA,dim = c(n1,n2,n3))
  
  
  KPI_list=list(KPI_list1=list(),KPI_list2=list(),KPI_list3=list())
  
  ### Result
  for(order_index1 in 1:n1){
    i = order_index1
    Record1=RuleCost_fun(i,Scenario1_list,CommonServTime,n,Ci,Co,sec,PRINT = F)
    KPI_list$KPI_list1[i]= list(Record1$KPI)
    for(order_index2 in 1:n2){
      j = order_index2
      Record2=RuleCost_fun(j,Scenario2_list,CommonServTime,n,Ci,Co,sec,PRINT = F)
      KPI_list$KPI_list2[j]= list(Record2$KPI)
      for(order_index3 in 1:n3){
        k = order_index3
        Record3=RuleCost_fun(k,Scenario3_list,CommonServTime,n,Ci,Co,sec,PRINT = F)
        # cost record
        Cost_array[i,j,k] = a1*Record1$Cost+a2*Record2$Cost+a3*Record3$Cost
        # Rule record
        Rule_array[i,j,k] = paste(Record1$Rule, Record2$Rule, Record3$Rule,sep = " & ")
        # KPI record
        KPI_list$KPI_list3[k]= list(Record3$KPI)
      }
    }
  }
  
  ## Peformance Index test
  Cost_bench_array = Cost_array/Cost_KPI_Benchmark$Cost
  Optimal_rule_Index = which(Cost_bench_array == min(Cost_bench_array),arr.ind = T) # matrix index[a,b]
  Optimal_rule_Name = Rule_array[Optimal_rule_Index]
  Optimal_rule_KPI1 = KPI_list$KPI_list1[[Optimal_rule_Index[1]]]
  Optimal_rule_KPI2 = KPI_list$KPI_list2[[Optimal_rule_Index[2]]]
  Optimal_rule_KPI3 = KPI_list$KPI_list3[[Optimal_rule_Index[3]]]
  
  Name_list = names(Optimal_rule_KPI1)
  Optimal_rule_KPI=list()
  
  #############################optional######################################
  #Optimal_KPI_value = a1*(as.numeric(Optimal_rule_KPI1[c(1:12)]))+a2*(as.numeric(Optimal_rule_KPI2[c(1:12)]))+a3*(as.numeric(Optimal_rule_KPI3[c(1:12)]))
  
  # for (nn in 1:(length(Name_list)-3)){
  #   Optimal_rule_KPI[[Name_list[nn]]]= Optimal_KPI_value[nn]
  #   
  # }
  #############################optional###################################
  
  Optimal_KPI_value = a1*(as.numeric(Optimal_rule_KPI1[c(1:10)]))+a2*(as.numeric(Optimal_rule_KPI2[c(1:10)]))+a3*(as.numeric(Optimal_rule_KPI3[c(1:10)]))
  
  for (nn in 1:(length(Name_list))){
    Optimal_rule_KPI[[Name_list[nn]]]= Optimal_KPI_value[nn]
    
  }
  
  ########################optional###############################
  # for (nn in c(13:15)){
  #   for(mm in str_sort(unique(df$Type))){
  #     k1=Optimal_rule_KPI1[[nn]][[mm]]
  #     k2=Optimal_rule_KPI2[[nn]][[mm]]
  #     k3=Optimal_rule_KPI3[[nn]][[mm]]
  #     if (is_null(k1)) k1=0 #to avoid the write error of the result
  #     if (is_null(k2)) k2=0
  #     if (is_null(k3)) k3=0
  #     Optimal_rule_KPI[[Name_list[nn]]][[mm]]=a1*k1+a2*k2+a3*k3
  #   }
  # }
  ################################################################
  
  Optimal_rule_Cost = Cost_bench_array[Optimal_rule_Index]
  OptRule = list(Name=Optimal_rule_Name,KPI=Optimal_rule_KPI,Cost = Optimal_rule_Cost)
  
  if (PRINT==T) {
    txt_note =sprintf('Classification type: %s. When Ci=%.1f,Co=%.1f,the Optimal combination rule is: %s, its cost is %.3f',ClassType,Ci,Co,Optimal_rule_Name,Optimal_rule_Cost) #output document
    print(txt_note)
  }
  
  Result=list(Cost_array=Cost_array,Rule_array=Rule_array,KPI_list=KPI_list,OptRule=OptRule,Cost_KPI_Benchmark=Cost_KPI_Benchmark,n1=n1,n2=n2,n3=n3)
  return(Result)
}


Optcost_SingleRule<-function(ClassType,CommonServTime,schd_Benchmark,n,Ci,Co,sec,df_FullSeq,PRINT = F){
  #print('Schedule Benchmark: all general type')
  rule_name = 'Benchmark'
  Cost_KPI_Benchmark=Obj_fun(CommonServTime,schd_Benchmark,rule_name,n,Ci,Co,sec,PRINT = F) #get the KPI of Benchmark
  
  ## KPI of Experiment rules
  ### Calculate Cost_mat of all the rules combination listed above
  
  n1 = nrow(df_FullSeq) #the length of Scenario1 rules list
  #Scenario1_record_list = list(Rule=list(),KPI=list(),Cost = list()) # define a void list to store the kpi data and rule information.
  
  ### Result
  df_FullSeq=CostValueFunc(df_FullSeq,Ci,Co,n) #update the cost in df_FullSeq for every different Co value
  
  df_FullSeq$Cost_benchmark=df_FullSeq$Cost/Cost_KPI_Benchmark$Cost
  
  ##Peformance Index test
  Optimal_rule_Index = which.min(df_FullSeq$Cost_benchmark)
  Optimal_rule_Name = df_FullSeq$Rule_name[[Optimal_rule_Index]]
  Optimal_rule_Cost = df_FullSeq$Cost_benchmark[[Optimal_rule_Index]]
  OptRule = list(Name=Optimal_rule_Name,Cost = Optimal_rule_Cost)
  
  if (PRINT==T){
  txt_note =sprintf('Classification type: %s. When Ci=%.1f,Co=%.1f,the Optimal rule is: %s, its cost is %.3f',ClassType,Ci,Co,Optimal_rule_Name,Optimal_rule_Cost) #output document
  print(txt_note)
  }
  
  Result=list(KPI_list= df_FullSeq,OptRule=OptRule,Cost_KPI_Benchmark=Cost_KPI_Benchmark,n1=n1)
  return(Result)
}

Plot_Co_Optcost_pro<-function(result,Ci){
  #if (Ci==0){ylable="Optcost_withOutIdle"}else{ylable="Optcost_withIdle"}
  #ylable="Optimal cost"
  #ylable = expression("Lowest  " ~ frac(italic(z), italic(z[F])) ~ "  value")
  ylable = expression(frac(italic(z), italic(z[F])))
  mydata = result
  
  column_mapping <- list("New_Return_TSC" = "New/Return+TSC", "New_Return_CRG" = "New/Return+CRG",
                         "New_Return_Enum" = "New/Return+Enum", "New_Return_gaps" = "New/Return_gaps",
                         "CPS_K2_TSC" = "K2+TSC", "CPS_K2_CRG" = "K2+CRG", "CPS_K2_Enum" = "K2+Enum", 
                         "CPS_K3_CRG" = "K3+CRG", "CPS_K3_Enum" = "K3+Enum",
                         "CPS_K2_gaps" = "K2_gaps", "CPS_K3_gaps" = "K3_gaps", 
                         "CPSK2_CRG" = "K2+CRG_STD","CPSK3_CRG" = "K3+CRG_STD",
                         "NewReturn_TSC" = "New/Return+TSC_STD", "FCFA" = "FCFA_STD")
  
  names(mydata) <- lapply(names(mydata), function(x) ifelse(x %in% names(column_mapping), column_mapping[[x]], x))
  
  mydata <- melt(mydata,id = "Co")
  colnames(mydata) <- c("Co_value","Policy","Opt_cost")#Changing the column name
  
  p1<-ggplot(data = mydata,aes(x = Co_value,y = Opt_cost,group = Policy,color = Policy,shape = Policy))+
    theme_bw() +#Remove the background gray
    theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"))+
    theme(axis.text.x = element_text(size=15, angle = 0, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(size=15, hjust = 0.5, vjust = 0.5),
            axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))+
    #theme(legend.position = "top")+
    theme(panel.grid = element_blank())+
    theme(legend.position = c(0.98, 0.237), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 10),
          #legend.title = element_text(size = 10),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "white", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(0.80,1.00),breaks = seq(0.80, 1.00, 0.02))+
    labs(title = bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]),y=ylable)+ #or opt_cost_withOutIdle
    theme(plot.title = element_text(size = 15))+
    guides(color = guide_legend(nrow = 2, ncol = 2)) 
  
  return(p1)
  
}

Plot_Co_Optcost_overlap<-function(result,Ci){
  #if (Ci==0){ylable="Optcost_withOutIdle"}else{ylable="Optcost_withIdle"}
  #ylable = expression("Lowest  " ~ frac(italic(z), italic(z[F])) ~ "  value")
  ylable = expression(frac(italic(z), italic(z[F])))
  mydata = result
  
  column_mapping <- list("New_Return_TSC" = "New/Return+TSC", "New_Return_CRG" = "New/Return+CRG",
                         "New_Return_Enum" = "New/Return+Enum", "New_Return_gaps" = "New/Return_gaps",
                         "CPS_K2_TSC" = "K2+TSC", "CPS_K2_CRG" = "K2+CRG", "CPS_K2_Enum" = "K2+Enum", 
                         "CPS_K3_CRG" = "K3+CRG", "CPS_K3_Enum" = "K3+Enum",
                         "CPS_K2_gaps" = "K2_gaps", "CPS_K3_gaps" = "K3_gaps", 
                         "CPSK2_CRG" = "K2+CRG_STD","CPSK3_CRG" = "K3+CRG_STD",
                         "NewReturn_TSC" = "New/Return+TSC_STD", "FCFA" = "FCFA_STD")
  
  names(mydata) <- lapply(names(mydata), function(x) ifelse(x %in% names(column_mapping), column_mapping[[x]], x))
  
  mydata <- melt(mydata,id = "Co")
  colnames(mydata) <- c("Co_value","Policy","Opt_cost")#Changing the column name
  
  p1 <- ggplot(data = mydata, aes(x = Co_value, y = Opt_cost, group = Policy, color = Policy, shape = Policy)) +
    theme_bw() +
    theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed")) +
    theme(axis.text.x = element_text(size = 15, angle = 0, hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(size = 15, hjust = 0.5, vjust = 0.5),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16)) +
    theme(panel.grid = element_blank()) +
    theme(legend.position = c(0.7, 0.35),
          legend.justification = c(1, 1),
          legend.text = element_text(size = 10),
          #legend.title = element_text(size = 10),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "white", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA)) +
    geom_point(size = 2.5) +
    geom_line() +
    scale_x_continuous(limits = c(1, 10), breaks = seq(1, 10, 1)) +
    scale_y_continuous(limits = c(0.80, 1.00), breaks = seq(0.80, 1.00, 0.02)) +
    labs(title = bquote(paste(c[I], "=", .(Ci))),
         x = expression(c[O]),
         y = ylable)+
    theme(plot.title = element_text(size = 15))
  
  # 设置第二条线条的指示符号为空心圆，第三条线条的指示符号为十字
  p1 <- p1 + scale_shape_manual(values = c(16,1,3))  # 1 = 点, 2 = 空心圆, 3 = 十字
  #p1 <- p1 + scale_shape_manual(values = c(16,1,3,2))  # 1 = 点, 2 = 空心圆, 3 = 十字
  return(p1)
  
}

Plot_Co_Optcost<-function(result,Ci){
  #if (Ci==0){ylable="Optcost_withOutIdle"}else{ylable="Optcost_withIdle"}
  #ylable="Optimal cost"
  #ylable = expression("Lowest  " ~ frac(italic(z), italic(z[F])) ~ "  value")
  ylable = expression(frac(italic(z), italic(z[F])))
  mydata = result
  
  column_mapping <- list("New_Return_TSC" = "New/Return+TSC", "New_Return_CRG" = "New/Return+CRG",
                         "New_Return_Enum" = "New/Return+Enum", "New_Return_gaps" = "New/Return_gaps",
                         "CPS_K2_TSC" = "K2+TSC", "CPS_K2_CRG" = "K2+CRG", "CPS_K2_Enum" = "K2+Enum", 
                         "CPS_K3_CRG" = "K3+CRG", "CPS_K3_Enum" = "K3+Enum",
                         "CPS_K2_gaps" = "K2_gaps", "CPS_K3_gaps" = "K3_gaps", 
                         "CPSK2_CRG" = "K2+CRG_STD","CPSK3_CRG" = "K3+CRG_STD",
                         "NewReturn_TSC" = "New/Return+TSC_STD", "FCFA" = "FCFA_STD","K2SVF" = "K2+SVF","K3SVF" = "K3+SVF")
  
  names(mydata) <- lapply(names(mydata), function(x) ifelse(x %in% names(column_mapping), column_mapping[[x]], x))
  
  mydata <- melt(mydata,id = "Co")
  colnames(mydata) <- c("Co_value","Policy","Opt_cost")#Changing the column name
  
  p1<-ggplot(data = mydata,aes(x = Co_value,y = Opt_cost,group = Policy,color = Policy,shape = Policy))+
    theme_bw() +#Remove the background gray
    theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"))+
    theme(axis.text.x = element_text(size=15, angle = 0, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(size=15, hjust = 0.5, vjust = 0.5),
          axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))+
    #theme(legend.position = "top")+
    theme(panel.grid = element_blank())+
    theme(legend.position = c(0.5, 0.25), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 10),
          #legend.title = element_text(size = 10),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "white", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(0.80,1.00),breaks = seq(0.80, 1.00, 0.02))+
    labs(title = bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]),y=ylable)+ #or opt_cost_withOutIdle
    theme(plot.title = element_text(size = 15))
  
  return(p1)
  
}


Plot_Co_Wait_sd<-function(result,Ci){
  #if (Ci==0){ylable="Optcost_withOutIdle"}else{ylable="Optcost_withIdle"}
  #ylable="Standard deviation of waiting time (minute)"
  ylable = expression("STD"[W]*" (in minutes)")
  mydata = result
  
  column_mapping <- list("New_Return_TSC" = "New/Return+TSC", "New_Return_CRG" = "New/Return+CRG",
                         "New_Return_Enum" = "New/Return+Enum", "New_Return_gaps" = "New/Return_gaps",
                         "CPS_K2_TSC" = "K2+TSC", "CPS_K2_CRG" = "K2+CRG", "CPS_K2_Enum" = "K2+Enum", 
                         "CPS_K3_CRG" = "K3+CRG", "CPS_K3_Enum" = "K3+Enum",
                         "CPS_K2_gaps" = "K2_gaps", "CPS_K3_gaps" = "K3_gaps", 
                         "CPSK2_CRG" = "K2+CRG","CPSK3_CRG" = "K3+CRG",
                         "NewReturn_TSC" = "New/Return+TSC")
  
  names(mydata) <- lapply(names(mydata), function(x) ifelse(x %in% names(column_mapping), column_mapping[[x]], x))
  
  mydata <- melt(mydata,id = "Co")
  colnames(mydata) <- c("Co_value","Policy","Opt_cost")#Changing the column name
  
  p1<-ggplot(data = mydata,aes(x = Co_value,y = Opt_cost,group = Policy,color = Policy,shape = Policy))+
    theme_bw() +#Remove the background gray
    theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"))+
    theme(axis.text.x = element_text(size=10,angle = 0, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(size=10,hjust = 0.5, vjust = 0.5),
          axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))+
    #theme(legend.position = "top")+
    theme(panel.grid = element_blank())+
    theme(legend.position = c(0.95, 0.45), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 6),
          #legend.title = element_text(size = 8),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "white", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    geom_point(size=2)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(7,11),breaks = seq(7, 11, 0.2))+
    labs(title = bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]),y=ylable) #or opt_cost_withOutIdle
  
  return(p1)
  
}

Plot_Co_Wait_sd_Slot<-function(result,Ci){
  #if (Ci==0){ylable="Optcost_withOutIdle"}else{ylable="Optcost_withIdle"}
  #ylable="Standard deviation of waiting time (minute)"
  #ylable = expression("STD"[W]*" (in minutes)")
  #ylable = expression(frac(STD(w), STD[F]~(W)))
  ylable = expression(STD(W)/STD[F]~(W))
  mydata = result
  
  column_mapping <- list("New_Return_TSC" = "New/Return+TSC", "New_Return_CRG" = "New/Return+CRG",
                         "New_Return_Enum" = "New/Return+Enum", "New_Return_gaps" = "New/Return_gaps",
                         "CPS_K2_TSC" = "K2+TSC", "CPS_K2_CRG" = "K2+CRG", "CPS_K2_Enum" = "K2+Enum", 
                         "CPS_K3_CRG" = "K3+CRG", "CPS_K3_Enum" = "K3+Enum",
                         "CPS_K2_gaps" = "K2_gaps", "CPS_K3_gaps" = "K3_gaps", 
                         "CPSK2_CRG" = "K2+CRG","CPSK3_CRG" = "K3+CRG",
                         "NewReturn_TSC" = "New/Return+TSC")
  
  names(mydata) <- lapply(names(mydata), function(x) ifelse(x %in% names(column_mapping), column_mapping[[x]], x))
  
  mydata <- melt(mydata,id = "Co")
  colnames(mydata) <- c("Co_value","Policy","Opt_cost")#Changing the column name
  
  p1<-ggplot(data = mydata,aes(x = Co_value,y = Opt_cost,group = Policy,color = Policy,shape = Policy))+
    theme_bw() +#Remove the background gray
    theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"))+
    theme(axis.text.x = element_text(size=15,angle = 0, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(size=15,hjust = 0.5, vjust = 0.5),
          axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))+
    #theme(legend.position = "top")+
    theme(panel.grid = element_blank())+
    theme(legend.position = c(0.99, 0.99), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 11),
          #legend.title = element_text(size = 8),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "white", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
        
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(0.7,1.5),breaks = seq(0.7, 1.5, 0.1))+
    labs(title = bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]),y=ylable)+ #or opt_cost_withOutIdle
    theme(plot.title = element_text(size = 15))+
    guides(color = guide_legend(nrow = 2, ncol = 2)) 
  
  return(p1)
  
}


Plot_Co_OTime_mean<-function(result,Ci){
  #if (Ci==0){ylable="Optcost_withOutIdle"}else{ylable="Optcost_withIdle"}
  ylable="Overtime (in minutes)"
  #ylable = expression("STD"[W]*" (in minutes)")
  #ylable = expression(frac(STD(w), STD[F]~(W)))
  
  mydata = result
  
  column_mapping <- list("CPSK2_CRG_ot" = "K2+CRG","CPSK3_CRG_ot" = "K3+CRG",
                         "NewReturn_TSC_ot" = "New/Return+TSC","FCFA_ot" = "FCFA")
  
  names(mydata) <- lapply(names(mydata), function(x) ifelse(x %in% names(column_mapping), column_mapping[[x]], x))
  
  mydata <- melt(mydata,id = "Co")
  colnames(mydata) <- c("Co_value","Policy","Opt_cost")#Changing the column name
  
  p1<-ggplot(data = mydata,aes(x = Co_value,y = Opt_cost,group = Policy,color = Policy,shape = Policy))+
    theme_bw() +#Remove the background gray
    theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"))+
    theme(axis.text.x = element_text(size=15,angle = 0, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(size=15,hjust = 0.5, vjust = 0.5),
          axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))+
    #theme(legend.position = "top")+
    theme(panel.grid = element_blank())+
    theme(legend.position = c(0.99, 0.99), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 11),
          #legend.title = element_text(size = 8),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "white", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(3,13),breaks = seq(3, 13, 1))+
    labs(title = bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]),y=ylable)+ #or opt_cost_withOutIdle
    theme(plot.title = element_text(size = 15))+
    guides(color = guide_legend(nrow = 2, ncol = 2)) 
  
  return(p1)
  
}


              
Plot_Gaps_Optcost<-function(result,Ci){
  if (Ci==0){ylable="Optimality gap(%)"}else{ylable="Optimality gap(%)"}
  mydata = result
  
  column_mapping <- list("New_Return_TSC" = "New/Return+TSC", "New_Return_CRG" = "New/Return+CRG",
                         "New_Return_Enum" = "New/Return+Enum", "New_Return_gaps" = "New/Return_gaps",
                         "CPS_K2_TSC" = "K2+TSC", "CPS_K2_CRG" = "K2+CRG", "CPS_K2_Enum" = "K2+Enum", 
                         "CPS_K3_CRG" = "K3+CRG", "CPS_K3_Enum" = "K3+Enum",
                         "CPS_K2_gaps" = "K2_gaps", "CPS_K3_gaps" = "K3_gaps", 
                         "CPSK2_CRG" = "K2+CRG_STD","CPSK3_CRG" = "K3+CRG_STD",
                         "NewReturn_TSC" = "New/Return+TSC_STD", "FCFA" = "FCFA_STD")
  
  names(mydata) <- lapply(names(mydata), function(x) ifelse(x %in% names(column_mapping), column_mapping[[x]], x))
  
  mydata <- melt(mydata,id = "Co")
  
  colnames(mydata) <- c("Co_value","Gaps","Opt_cost")#Changing the column name

  p1<-ggplot(data = mydata,aes(x = Co_value,y = Opt_cost,group = Gaps,color = Gaps,shape = Gaps))+
    theme_bw() +#Remove the background gray
    theme(axis.text.x = element_text(size=10,angle = 0, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(size=10,hjust = 0.5, vjust = 0.5),
          axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))+
    #theme(legend.position = "top")+
    theme(panel.grid = element_blank())+
    theme(legend.position = c(0.99, 0.99), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 8),
          #legend.title = element_text(size = 10),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    geom_point(size=2)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
  
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(0,1.3),breaks = seq(0, 1.3, 0.1))+

    labs(title =bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]), 
         y=ylable) #or opt_cost_withOutIdle
  
  return(p1)
  
}


Plot_Impro_Optcost_NR<-function(result,Ci){
  if (Ci==0){ylable="Percentage of cost reduction (%)"}else{ylable="Percentage of cost reduction (%)"}
  mydata = result
  
  column_mapping <- list("K2CRG_NR_Impro" = "K2+CRG","K3CRG_NR_Impro" = "K3+CRG",
                         "K2CRG_FCFA_Impro" = "K2+CRG","K3CRG_FCFA_Impro" = "K3+CRG")
  
  names(mydata) <- lapply(names(mydata), function(x) ifelse(x %in% names(column_mapping), column_mapping[[x]], x))
  
  mydata <- melt(mydata,id = "Co")
  
  colnames(mydata) <- c("Co_value","Gaps","Opt_cost")#Changing the column name
  
  p1<-ggplot(data = mydata,aes(x = Co_value,y = Opt_cost,group = Gaps,color = Gaps,shape = Gaps))+
    theme_bw() +#Remove the background gray
    theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"))+
    theme(axis.text.x = element_text(size=15,angle = 0, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(size=15,hjust = 0.5, vjust = 0.5),
          axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))+
    #theme(legend.position = "top")+
    theme(panel.grid = element_blank())+
    theme(legend.position = c(0.99, 1), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 11),
          #legend.title = element_text(size = 10),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(0,10),breaks = seq(0, 10, 1))+
    
    labs(title =bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]), 
         y=ylable) #or opt_cost_withOutIdle
  
  # Add labels to the data points
  #p1 <- p1 +geom_text(aes(label = sprintf("%.2f", Opt_cost)), nudge_y = 0.2, size = 3)
  # Add labels for the maximum and minimum points
  max_value <- max(mydata$Opt_cost)
  min_value <- min(mydata$Opt_cost)

  p1 <- p1 +
    geom_text(data = mydata[mydata$Opt_cost == max_value, ],
              aes(label = sprintf("%.2f", Opt_cost)),
              size = 2.5,  # Set the font size for the labels
              vjust = -1,  # Adjust the vertical position of the label
              hjust = 0.5,
              show.legend = FALSE) +  # Adjust the horizontal position of the label
    geom_text(data = mydata[mydata$Opt_cost == min_value, ],
              aes(label = sprintf("%.2f", Opt_cost)),
              size = 2.5,  # Set the font size for the labels
              vjust = 2,  # Adjust the vertical position of the label
              hjust = 0.5,
              show.legend = FALSE)  # Adjust the horizontal position of the label
  

  return(p1)
  
}

Plot_Impro_Optcost_FCFA<-function(result,Ci){
  if (Ci==0){ylable="Percentage of cost reduction (%)"}else{ylable="Percentage of cost reduction (%)"}
  mydata = result
  
  column_mapping <- list("K2CRG_NR_Impro" = "K2+CRG","K3CRG_NR_Impro" = "K3+CRG",
                         "K2CRG_FCFA_Impro" = "K2+CRG","K3CRG_FCFA_Impro" = "K3+CRG")
  
  names(mydata) <- lapply(names(mydata), function(x) ifelse(x %in% names(column_mapping), column_mapping[[x]], x))
  
  mydata <- melt(mydata,id = "Co")
  
  colnames(mydata) <- c("Co_value","Gaps","Opt_cost")#Changing the column name
  
  p1<-ggplot(data = mydata,aes(x = Co_value,y = Opt_cost,group = Gaps,color = Gaps,shape = Gaps))+
    theme_bw() +#Remove the background gray
    theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"))+
    theme(axis.text.x = element_text(size=15, angle = 0, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(size=15, hjust = 0.5, vjust = 0.5),
          axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))+
    #theme(legend.position = "top")+
    theme(panel.grid = element_blank())+
    theme(legend.position = c(0.99, 1), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 11),
          #legend.title = element_text(size = 10),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(0,18),breaks = seq(0, 18, 2))+
    
    labs(title =bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]), 
         y=ylable) #or opt_cost_withOutIdle
  
  # Add labels to the data points
  #p1 <- p1 + geom_text(aes(label = sprintf("%.2f", Opt_cost)), nudge_y = 0.2, size = 3)
  # Add labels for the maximum and minimum points
  max_value <- max(mydata$Opt_cost)
  min_value <- min(mydata$Opt_cost)

  p1 <- p1 +
    geom_text(data = mydata[mydata$Opt_cost == max_value, ],
              aes(label = sprintf("%.2f", Opt_cost)),
              size = 2.5,  # Set the font size for the labels
              vjust = -1,  # Adjust the vertical position of the label
              hjust = 0.5,
              show.legend = FALSE) +  # Adjust the horizontal position of the label
    geom_text(data = mydata[mydata$Opt_cost == min_value, ],
              aes(label = sprintf("%.2f", Opt_cost)),
              size = 2.5,  # Set the font size for the labels
              vjust = 2,  # Adjust the vertical position of the label
              hjust = 0.5,
              show.legend = FALSE)  # Adjust the horizontal position of the label
  
  
  return(p1)
  
}

Plot_Impro_Optcost_K2_NR<-function(result,Ci){
  if (Ci==0){ylable="Percentage of cost reduction (%)"}else{ylable="Percentage of cost reduction (%)"}
  mydata = result
  
  column_mapping <- list("K2TSC_NRTSC_Impro" = "K2+TSC",
                         "K2CRG_K2TSC_Impro" = "K2+CRG")
  
  names(mydata) <- lapply(names(mydata), function(x) ifelse(x %in% names(column_mapping), column_mapping[[x]], x))
  
  mydata <- melt(mydata,id = "Co")
  
  colnames(mydata) <- c("Co_value","Gaps","Opt_cost")#Changing the column name
  
  p1<-ggplot(data = mydata,aes(x = Co_value,y = Opt_cost,group = Gaps,color = Gaps,shape = Gaps))+
    theme_bw() +#Remove the background gray
    theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"))+
    theme(axis.text.x = element_text(size=15, angle = 0, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(size=15, hjust = 0.5, vjust = 0.5),
          axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))+
    #theme(legend.position = "top")+
    theme(panel.grid = element_blank())+
    theme(legend.position = c(0.99, 1), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 11),
          #legend.title = element_text(size = 10),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(-3,5),breaks = seq(-3, 5, 0.5))+
    
    labs(title =bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]), 
         y=ylable) #or opt_cost_withOutIdle
  
  # Add labels to the data points
  #p1 <- p1 + geom_text(aes(label = sprintf("%.2f", Opt_cost)), nudge_y = 0.2, size = 3)
  # Add labels for the maximum and minimum points
  max_value <- max(mydata$Opt_cost)
  min_value <- min(mydata$Opt_cost)
  
  p1 <- p1 +
    geom_text(data = mydata[mydata$Opt_cost == max_value, ],
              aes(label = sprintf("%.2f", Opt_cost)),
              size = 2.5,  # Set the font size for the labels
              vjust = -1,  # Adjust the vertical position of the label
              hjust = 0.5,
              show.legend = FALSE) +  # Adjust the horizontal position of the label
    geom_text(data = mydata[mydata$Opt_cost == min_value, ],
              aes(label = sprintf("%.2f", Opt_cost)),
              size = 2.5,  # Set the font size for the labels
              vjust = 2,  # Adjust the vertical position of the label
              hjust = 0.5,
              show.legend = FALSE)  # Adjust the horizontal position of the label
  
  
  return(p1)
  
}

Plot_Impro_Optcost_CRG_TSC<-function(result,Ci){
  if (Ci==0){ylable="Percentage of cost reduction (%)"}else{ylable="Percentage of cost reduction (%)"}
  mydata = result
  
  column_mapping <- list("K2TSC_NRTSC_Impro" = "K2+TSC",
                         "K2CRG_K2TSC_Impro" = "K2+CRG")
  
  names(mydata) <- lapply(names(mydata), function(x) ifelse(x %in% names(column_mapping), column_mapping[[x]], x))
  
  mydata <- melt(mydata,id = "Co")
  
  colnames(mydata) <- c("Co_value","Gaps","Opt_cost")#Changing the column name
  
  p1<-ggplot(data = mydata,aes(x = Co_value,y = Opt_cost,group = Gaps,color = Gaps,shape = Gaps))+
    theme_bw() +#Remove the background gray
    theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"))+
    theme(axis.text.x = element_text(size=15, angle = 0, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(size=15, hjust = 0.5, vjust = 0.5),
          axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))+
    #theme(legend.position = "top")+
    theme(panel.grid = element_blank())+
    theme(legend.position = c(0.99, 1), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 11),
          #legend.title = element_text(size = 10),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(0,6),breaks = seq(0, 6, 0.5))+
    
    labs(title =bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]), 
         y=ylable) #or opt_cost_withOutIdle
  
  # Add labels to the data points
  #p1 <- p1 + geom_text(aes(label = sprintf("%.2f", Opt_cost)), nudge_y = 0.2, size = 3)
  # Add labels for the maximum and minimum points
  max_value <- max(mydata$Opt_cost)
  min_value <- min(mydata$Opt_cost)
  
  p1 <- p1 +
    geom_text(data = mydata[mydata$Opt_cost == max_value, ],
              aes(label = sprintf("%.2f", Opt_cost)),
              size = 2.5,  # Set the font size for the labels
              vjust = -1,  # Adjust the vertical position of the label
              hjust = 0.5,
              show.legend = FALSE) +  # Adjust the horizontal position of the label
    geom_text(data = mydata[mydata$Opt_cost == min_value, ],
              aes(label = sprintf("%.2f", Opt_cost)),
              size = 2.5,  # Set the font size for the labels
              vjust = 2,  # Adjust the vertical position of the label
              hjust = 0.5,
              show.legend = FALSE)  # Adjust the horizontal position of the label
  
  
  return(p1)
  
}

Plot_Impro_K2CRG_k3CRG<-function(result,Ci){
  if (Ci==0){ylable="Percentage of cost reduction (%)"}else{ylable="Percentage of cost reduction (%)"}
  mydata = result
  
  column_mapping <- list("K3CRG_K2CRG_Impro" = "(K2-k3)/k2")
  
  names(mydata) <- lapply(names(mydata), function(x) ifelse(x %in% names(column_mapping), column_mapping[[x]], x))
  
  mydata <- melt(mydata,id = "Co")
  
  colnames(mydata) <- c("Co_value","Gaps","Opt_cost")#Changing the column name
  
  p1<-ggplot(data = mydata,aes(x = Co_value,y = Opt_cost,group = Gaps,color = Gaps,shape = Gaps))+
    theme_bw() +#Remove the background gray
    theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"))+
    theme(axis.text.x = element_text(size=15, angle = 0, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(size=15, hjust = 0.5, vjust = 0.5),
          axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))+
    #theme(legend.position = "top")+
    theme(panel.grid = element_blank())+
    theme(legend.position = c(0.99, 1), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 11),
          #legend.title = element_text(size = 10),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(-3,5),breaks = seq(-3, 5, 0.5))+
    
    labs(title =bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]), 
         y=ylable) #or opt_cost_withOutIdle
  
  # Add labels to the data points
  #p1 <- p1 + geom_text(aes(label = sprintf("%.2f", Opt_cost)), nudge_y = 0.2, size = 3)
  # Add labels for the maximum and minimum points
  max_value <- max(mydata$Opt_cost)
  min_value <- min(mydata$Opt_cost)
  
  p1 <- p1 +
    geom_text(data = mydata[mydata$Opt_cost == max_value, ],
              aes(label = sprintf("%.2f", Opt_cost)),
              size = 2.5,  # Set the font size for the labels
              vjust = -1,  # Adjust the vertical position of the label
              hjust = 0.5,
              show.legend = FALSE) +  # Adjust the horizontal position of the label
    geom_text(data = mydata[mydata$Opt_cost == min_value, ],
              aes(label = sprintf("%.2f", Opt_cost)),
              size = 2.5,  # Set the font size for the labels
              vjust = 2,  # Adjust the vertical position of the label
              hjust = 0.5,
              show.legend = FALSE)  # Adjust the horizontal position of the label
  
  
  return(p1)
  
}


Plot_Slot_Cumsum<-function(result,nn,Ci=1){
  ylable <- bquote("slot "*.(nn)*" time")

  #ylable = "Culumative waiting time"

  mydata = result
  
  mydata <- melt(mydata,id = "Slot")
  colnames(mydata) <- c("Slot","Seq","Value")#Changing the column name
  
  p1<-ggplot(data = mydata,aes(x = Slot,y = Value,group = Seq,color = Seq,shape = Seq))+
    theme_bw() +#Remove the background gray
    theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"))+
    theme(axis.text.x = element_text(size=10,angle = 0, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(size=10,hjust = 0.5, vjust = 0.5),
          axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))+
    #theme(legend.position = "top")+
    theme(panel.grid = element_blank())+
    theme(legend.position = c(0.2, 0.85), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 8),
          #legend.title = element_text(size = 10),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "white", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    geom_point(size=2)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,16),breaks = seq(1, 16, 1))+
    scale_y_continuous(limits = c(0,110),breaks = seq(0, 110, 5))+
    labs(title = bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression("Slot"),
         y = ylable) #or opt_cost_withOutIdle
  
  return(p1)
  
}

Plot_Slot_Mean_Wait<-function(result,Ci,title){
  ylable <- bquote("Average "*.(title)*" time in each slot (in minutes)")
  
  #ylable = "Culumative waiting time"
  
  mydata = result
  
  column_mapping <- list("CPSK2_CRG_slw" = "K2+CRG", "CPSK3_CRG_slw" = "K3+CRG",
                         "NewReturn_TSC_slw" = "New/Return+TSC", "FCFA_slw" = "FCFA",
                         "CPSK2_CRG_sli" = "K2+CRG", "CPSK3_CRG_sli" = "K3+CRG",
                         "NewReturn_TSC_sli" = "New/Return+TSC", "FCFA_sli" = "FCFA")
  
  names(mydata) <- lapply(names(mydata), function(x) ifelse(x %in% names(column_mapping), column_mapping[[x]], x))
  
  mydata <- melt(mydata,id = "Slot")
  colnames(mydata) <- c("Slot","Seq","Value")#Changing the column name
  
  p1<-ggplot(data = mydata,aes(x = Slot,y = Value,group = Seq,color = Seq,shape = Seq))+
    theme_bw() +#Remove the background gray
    theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"))+
    theme(axis.text.x = element_text(size=12,angle = 0, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(size=12,hjust = 0.5, vjust = 0.5),
          axis.title.x=element_text(size=13),axis.title.y=element_text(size=13))+
    #theme(legend.position = "top")+
    theme(panel.grid = element_blank())+
    theme(legend.position = c(0.99, 0.24), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 10),
          #legend.title = element_text(size = 10),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "white", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,16),breaks = seq(1, 16, 1))+
    scale_y_continuous(limits = c(0,12),breaks = seq(0, 12, 1))+
    labs(title = bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression("Slot"),
         y = ylable)+ #or opt_cost_withOutIdle
    guides(color = guide_legend(nrow = 2, ncol = 2))
  return(p1)
  
}

Plot_Slot_Mean_Idle<-function(result,Ci,title){
  ylable <- bquote("Average "*.(title)*" time in each slot (in minutes)")
  
  #ylable = "Culumative waiting time"
  
  mydata = result
  
  column_mapping <- list("CPSK2_CRG_slw" = "K2+CRG", "CPSK3_CRG_slw" = "K3+CRG",
                         "NewReturn_TSC_slw" = "New/Return+TSC", "FCFA_slw" = "FCFA",
                         "CPSK2_CRG_sli" = "K2+CRG", "CPSK3_CRG_sli" = "K3+CRG",
                         "NewReturn_TSC_sli" = "New/Return+TSC", "FCFA_sli" = "FCFA")
  
  names(mydata) <- lapply(names(mydata), function(x) ifelse(x %in% names(column_mapping), column_mapping[[x]], x))
  
  mydata <- melt(mydata,id = "Slot")
  colnames(mydata) <- c("Slot","Seq","Value")#Changing the column name
  
  p1<-ggplot(data = mydata,aes(x = Slot,y = Value,group = Seq,color = Seq,shape = Seq))+
    theme_bw() +#Remove the background gray
    theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"))+
    theme(axis.text.x = element_text(size=12,angle = 0, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(size=12,hjust = 0.5, vjust = 0.5),
          axis.title.x=element_text(size=13),axis.title.y=element_text(size=13))+
    #theme(legend.position = "top")+
    theme(panel.grid = element_blank())+
    theme(legend.position = c(0.99, 0.24), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 10),
          #legend.title = element_text(size = 10),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "white", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,16),breaks = seq(1, 16, 1))+
    scale_y_continuous(limits = c(0,4),breaks = seq(0, 4, 0.4))+
    labs(title = bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression("Slot"),
         y = ylable)+ #or opt_cost_withOutIdle
    guides(color = guide_legend(nrow = 2, ncol = 2))
  return(p1)
  
}



Lookup_OptRecord_FullSeq<-function(FullSeq,Ci,Co,n){
  
  FullSeq=CostValueFunc(FullSeq,Ci,Co,n)
  Optimal_rule_Index=which.min(FullSeq$Cost)
  
  OptCost=FullSeq$Cost[Optimal_rule_Index]
  OptRule=FullSeq$Rule_name[Optimal_rule_Index]
  Record=list(Cost=OptCost,Rule=OptRule)
  return(Record)
}

Lookup_OptRecord_NonFullSeq<-function(FullSeq,Scenario_list,Ci,Co,n){
  #return the optimal NonFullSeq record 
  len=length(Scenario_list$Type)
  FullSeq=CostValueFunc(FullSeq,Ci,Co,n)
  FullSeq$NonFullSeq=FALSE
  for (i in 1:len){
    Name=Scenario_list$Type[[i]]
    Rule_name=paste(Name[1:length(Name)], collapse="")
    FullSeq$NonFullSeq[which(FullSeq$Rule_name==Rule_name)]=TRUE
  }
  
  NonFullSeq=FullSeq[FullSeq$NonFullSeq,]
  Optimal_rule_Index=which.min(NonFullSeq$Cost)
  OptCost=NonFullSeq$Cost[Optimal_rule_Index]
  OptRule=NonFullSeq$Rule_name[Optimal_rule_Index]
  OptWait_SD=NonFullSeq$sd.wait[Optimal_rule_Index]
  OptWait_Mean=NonFullSeq$mean.wait[Optimal_rule_Index]
  OptOT_Mean=NonFullSeq$mean.OT[Optimal_rule_Index]
  Record=list(Cost=OptCost,Rule=OptRule,Wsd=OptWait_SD,Wmean=OptWait_Mean,Omean=OptOT_Mean)
  return(Record)
}

Lookup_OptRecord_NonFullSeq_T<-function(FullSeq,Scenario_list,Ci,Co,n){
  #return the FullSeq with the information lable (True or False) of NonFullSeq
  len=length(Scenario_list$Type)
  FullSeq=CostValueFunc(FullSeq,Ci,Co,n)
  FullSeq$NonFullSeq=FALSE
  for (i in 1:len){
    Name=Scenario_list$Type[[i]]
    Rule_name=paste(Name[1:length(Name)], collapse="")
    FullSeq$NonFullSeq[which(FullSeq$Rule_name==Rule_name)]=TRUE
  }

  return(FullSeq)
}


Lookup_NonFullSeq<-function(FullSeq,Scenario_list){
  #Lookup and return the NonFullSeq from the FullSeq.
  len=length(Scenario_list$Type)
  FullSeq$NonFullSeq=FALSE
  for (i in 1:len){
    Name=Scenario_list$Type[[i]]
    Rule_name=paste(Name[1:length(Name)], collapse="")
    FullSeq$NonFullSeq[which(FullSeq$Rule_name==Rule_name)]=TRUE
  }
  
  df_NonFullSeq=subset(FullSeq, NonFullSeq)
  return(df_NonFullSeq)
}


RatioCalculation<-function(df,a11,a12,a21,a22,n){
  # used to calculate the ratio of A,B 
  assertthat::assert_that((a11+a12==a21+a22) && (a11+a12==n), 
                          msg='the total number must be equal to n !!')
  tot = dim(df)[1]
  x1 = sum(df$Type=='A')/tot*n; x2 = sum(df$Type=='B')/tot*n 
  print(paste0('x1=',x1, '; x2=',x2))
  
  # two different allocation: p1*[a11,a12], p2*[a21,a22]
  # p1*a11+p2*a21=x1, then 
  if(a11>a21) {
    p1<-(x1-min(a11,a21))/(a11-min(a11,a21))
  } else {
    p2<-(x1-min(a11,a21))/(a21-min(a11,a21))
  }
  
  # p1*a12+p2*a22=x2, then 
  if(a12>a22) {
    p1<-(x2-min(a12,a22))/(a12-min(a12,a22))
  } else {
    p2<-(x2-min(a12,a22))/(a22-min(a12,a22))
  }
  
  #print(p1*c(a11, a12) + p2*c(a21, a22) - c(x1, x2))
  print(paste0('p1=',p1, '; p2=',p2))
  
  stopifnot(p1>0 && p2>0)
  
  Ratio <- list(p1=p1,p2=p2)
  
  return(Ratio)
}

Ratio_Three_Calculation <- function(df, a11, a12, a13, a21, a22, a23, a31, a32, a33, n) {
  assertthat::assert_that((a11+a12+a13==n) && (a21+a22+a23==n) && (a31+a32+a33==n), 
                          msg='the total number in every situation must be equal to n !!')
  tot = dim(df)[1]
  x1 = sum(df$Type=='A')/tot*n; x2 = sum(df$Type=='B')/tot*n; x3 = sum(df$Type=='C')/tot*n
  print(paste0('x1=',x1, '; x2=',x2, '; x3=',x3))
  A <- matrix(c(a11, a21, a31, a12, a22, a32, a13, a23, a33), nrow = 3, byrow = TRUE)
  b <- c(x1, x2, x3)
  solution <- solve(A, b)
  Ratio <- list(p1=solution[1],p2=solution[2],p3=solution[3])
  print(paste0('p1=',solution[1], '; p2=',solution[2],'; p3=',solution[3]))
  return(Ratio)
}

# Rules in the literature
SeqRule_Liter<-function(TypeList,TypeCount){
  Type1=TypeList[1]; Type2=TypeList[2]
  n1=TypeCount[1];n2=TypeCount[2]
  
  AlterList=list()
  
  # ABG
  list1=c(rep(Type1,n1),rep(Type2,n2))
  AlterList <- c(AlterList, list(c(list1)))
  
  # ABND
  if(min(n1,n2)>1){
    list1=c(rep(Type1,floor(n1/2)),rep(Type2,n2),rep(Type1,n1-floor(n1/2)))
    list01=c(rep(Type1,ceiling(n1/2)),rep(Type2,n2),rep(Type1,n1-ceiling(n1/2)))
    AlterList <- c(AlterList, list(c(list1)), list(c(list01)))
  }
  
  # ALTER1 ABAB
  if (min(n1,n2)==0){
    return(unique(AlterList))
  } else {
    list1=rep(c(Type1,Type2),min(n1,n2))
    if (n1>n2) {list2=rep(Type1,(n1-n2))} else {list2=rep(Type2,(n2-n1))}
    AlterList <- c(AlterList, list(c(list1,list2)))
  }
  
  
  n=n1;n1=n2;n2=n;
  
  Type=Type1;Type1=Type2;Type2=Type;
  
  
  # BBG
  list1=c(rep(Type1,n1),rep(Type2,n2))
  AlterList <- c(AlterList, list(c(list1)))
  
  # BBND
  if(min(n1,n2)>1){
    list1=c(rep(Type1,floor(n1/2)),rep(Type2,n2),rep(Type1,n1-floor(n1/2)))
    list01=c(rep(Type1,ceiling(n1/2)),rep(Type2,n2),rep(Type1,n1-ceiling(n1/2)))
    AlterList <- c(AlterList, list(c(list1)), list(c(list01)))
  }
  
  # ALTER2 BABA
  if (min(n1,n2)==0){
    return(unique(AlterList))
  } else {
    list1=rep(c(Type1,Type2),min(n1,n2))
    if (n1>n2) {list2=rep(Type1,(n1-n2))} else {list2=rep(Type2,(n2-n1))}
    AlterList <- c(AlterList, list(c(list1,list2)))
  }
  
  OneList <- lapply(unique(AlterList), paste0, collapse="")
  return(OneList)
}
