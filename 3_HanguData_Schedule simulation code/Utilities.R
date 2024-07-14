library(tidyverse)
SimKPI <- function(R_mat,X_Vec){
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
  T_Vec=sum(X_Vec)+(W_Mat[,n]+R_mat[,n])
  last_start_Vec = sum(X_Vec) + W_Mat[,n] #starting time of the last patient for all replications
  # doctor Idle time is the positive part of the gap between last patient's 
  # starting time and all the other patients service time. See Eq (5) of Chen and Robinson (2014 POM)
  Idle_Vec = pmax(last_start_Vec - rowSums(R_mat[,-n,drop=F]),0) 
  rKPI<-list(W_Mat=W_Mat,T_Vec=T_Vec, Idle_Vec = Idle_Vec) # raw KPI
  return(rKPI)
}


is.subset <- function(A,B){
  # test if A is a subset of B
  return(setequal(A, intersect(A,B)))
}

Data2ServiceList <- function(df,Type_set = NULL, General=T){
  #From dataframe, get the ServiceList which contains lists of service time for
  # different type in Type_set
  # The dataframe df should have the following fields:
      # Type: which contains the type of an observation
      # Duration: which contains the service time of the observation
  # By default, the general type is included, that is to consider 'homogeneous'
  # service time that does not differentiate types. It takes all values from df$Duration
  # Type_set: set of types to consider. 
  #   If is.null(Type_set), Type_set=unique(as.character(df$Type))
  #   else: each value in Type_set should be contained in unique(df$Type)
  All_Types = unique(as.character(df$Type))
  if(is.null(Type_set)) Type_set = All_Types
  else{
    Type_set = as.character(Type_set)
    # Type_set should be a subset of All_Types. Stop if not
    #stopifnot(setequal(Type_set, intersect(Type_set,All_Types)))
    stopifnot(is.subset(Type_set,All_Types))
  }
  
  ServList = list()
  if(General)  ServList$General <- df$Duration #General Service time
  for(type in Type_set){
    ServList[[type]] <- df %>% filter(Type==type) %>% pull(Duration) #Service time Data for each types
  }  
  return(ServList)
}


ServL2ComServT <- 
  function(ServList, seed = 123, 
           n_samp= 10000, # num of replication
           max_n = 16, # num of patients/jobs
           Type_set = NULL # Type_set can either be specified or implied by ServList
           ){
  # Consolidate service time sample from Service Time List ServList
  # For reproducibility, set random seed
  # Return a list of n_samp-by-max_n matrices of observed service time
  #   - for each type in Type_set
  # For each type in Type_set
  #---Check if Type_set are vaild---#
  All_Types = names(ServList)
  if(is.null(Type_set)) Type_set = All_Types #default
  else{
    Type_set = as.character(Type_set)
    # Type_set should be a subset of All_Types. Stop if not
    stopifnot(is.subset(Type_set,All_Types))
  }
  ### don't want to alter the random state if it already exists
  ### So, save the random state if it exists, and then restore it at the end
  if (exists(".Random.seed")) oldseed <- .Random.seed
  set.seed(seed)
  CommonServTime = list()
  for(type in Type_set){
    CommonServTime[[type]]=
      matrix(data=sample(x=ServList[[type]],size=max_n*n_samp,replace = T),
                                  nrow = n_samp, ncol= max_n)
  }
  if (exists("oldseed")) .Random.seed <- oldseed
  return(CommonServTime)
  }



schd2R_mat <- function(CommonServTime,schd){
  # given the list CommonServTime and a schedule 'schd'
  # Return the realized service time R_mat for this schedule
  # The schedule 'schd' should have two components: Type and X_Vec
  ### Following check to make sure the validity of the input ###
  stopifnot(is.subset(c('Type','X_Vec'), names(schd)))
  n = length(schd$Type)
  stopifnot(n == length(schd$X_Vec)+1)
  #Type of each patient should belongs to the set of types that are in CommonServTime
  Type_set = unique(schd$Type)
  All_Types = names(CommonServTime)
  stopifnot(is.subset(Type_set, All_Types))
  ## each field of CommonServTime should have at least the same column (# of patients)
  ## as the schd has
  stopifnot(sapply(CommonServTime, ncol) >= n)
  ## Each CommonServTime field has the same nrow
  n_samp = nrow(CommonServTime[[1]])
  stopifnot(sapply(CommonServTime, nrow) == n_samp)
  ### Validity check completed ###
  
  R_mat = matrix(nrow = n_samp, ncol= n)
  #cur= list();
  #for(type in names(CommonServTime)){ # initialization
  #  cur[[ type ]] = 1
  #}
  for(type in Type_set){
    col_id = schd$Type == type
    N = sum(col_id)
    stopifnot(N>=1L) # if N<1, 1:N is not what we expected.
    R_mat[, col_id] = CommonServTime[[type]][,1:N]
  }
  return(R_mat)
}

schd2SimKPI <- function(CommonServTime, schd){
  # given the list CommonServTime and a schedule 'schd'
  # This one actually simulate and return the raw KPI 
  # The schedule 'schd' should have two components: Type and X_Vec (in seconds)
  # It first calls the schd2R_mat to form the Realized service time matrix
  # and then call SimKPI
  R_mat = schd2R_mat(CommonServTime,schd)
  rKPI = SimKPI(R_mat,schd$X_Vec)
  return(rKPI)
}

## Turn Visit-Condition typing into 'A','B','C' typing
## Not really needed. One can use any character-based labeling for types
## Just make sure you set the Type in your df after reading in the data
VisitCondition <- function(VisitType, Condition){
  # from VisitType and Condition, construct type A, B, and C
  # A: 'Follow-up'
  # B: 'Initial' & 'Non-Cancer'
  # C: 'Initial' & 'Cancer'
  n = length(VisitType)
  stopifnot( n ==length(Condition))
  Type = rep('C',n)
  Type[VisitType=='Follow-up'] <- 'A'
  Type[VisitType=='Initial' & Condition == 'Non-Cancer'] <- 'B'
  return(Type)
}

rKPI2KPI <- function(rKPI, #simulated raw KPI which has W_Mat and T_Vec
                    plan_completion = 4*3600L, # 4-hour session
                    excess_wait_threshhold = 15*60L, # 15 min for excess wait
                    extreme_wait_threshhold = 45*60L, # 45 min for extreme wait
                    excess_OT_threshhold = 30*60L, # 30 min for excess OT
                    extreme_OT_threshhold = 45*60L, #45 min for extreme OT
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
  KPI$wait_mean = mean(Wait); KPI$wait_sd = sd(Wait)
  KPI$OT_mean = mean(OT); KPI$OT_sd = sd(OT)
  KPI$Idle_mean = mean(rKPI$Idle_Vec); KPI$Idle_sd = sd(rKPI$Idle_Vec)
  if(PRINT){
  msg = sprintf('plan_end=%f, excess/extreme_wait_threshhold=(%f, %f), excess/extreme_OT_threshhold=(%f, %f)',
                plan_completion, excess_wait_threshhold, extreme_wait_threshhold,
                excess_OT_threshhold, extreme_OT_threshhold)
  print(msg)
  msg = sprintf('mean and sd of Wait= (%.2f, %.2f)',KPI$wait_mean,KPI$wait_sd)
  msg = sprintf('%s, mean and sd of OT= (%.2f, %.2f)',msg, KPI$OT_mean,KPI$OT_sd)
  msg = sprintf('%s, mean and sd of Idle= (%.2f, %.2f).',msg, KPI$Idle_mean,KPI$Idle_sd)
  print(msg)
  print(sprintf('excess and extreme wait rate=(%.2f%%, %.2f%%)',
                KPI$excess_wait_rate,KPI$extreme_wait_rate))
  print(sprintf('excess and extreme OT rate=(%.2f%%, %.2f%%)',
                KPI$excess_OT_rate,KPI$extreme_OT_rate))
  }
  return(KPI)
}

SeqMat_2Types <- function(n1, n2, n){
  # Input: two types with number n1 and n2, and total n (n1+n2 should be n)
  # Output: a matrix of seq, n1-column and choose(n,n1) rows. 
  #         Each row is a vector with n1 numbers, indicating the positions of type 1  entries
  # Example: n1 =2, n2 =1, n=3, totally 3 rows
  # Example output (first row means position 1 and 2 are of type 1, and the rest of type2)      
  #       [,1] [,2]
  #[1,]    1    2
  #[2,]    1    3
  #[3,]    2    3
  assertthat::assert_that(n1+n2==n,msg='Error,n1 + n2 != n')
  if(n1<1){return(matrix(rep(Type2,n),ncol = n))}
  if(n2<1){return(matrix(rep(Type1,n),ncol = n))}
  # next n1<n, n2 < n and n1+n2=n
  return(t(combn(n,n1)))
}

FullSeq_2Types_SimKPI <- function(CommonServTime,Type1, Type2, n1, n2, X_Vec,
                                  plan_completion = 4*3600L, # 4-hour session
                                  excess_wait_threshhold = 15*60L, # 15 min for excess wait
                                  extreme_wait_threshhold = 45*60L, # 45 min for extreme wait
                                  excess_OT_threshhold = 30*60L, # 30 min for excess OT
                                  extreme_OT_threshhold = 45*60L #45 min for extreme OT
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
                   mean.wait=numeric(n_seq), sd.wait=numeric(n_seq), 
                   mean.T = numeric(n_seq), sd.T=numeric(n_seq), 
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
    schd= list(Type=rep(Type2,n),X_Vec= X_Vec)
    schd$Type[Type1.index] = Type1
    rKPI <- schd2SimKPI(CommonServTime, schd)
    KPI <- rKPI2KPI(rKPI, 
                    plan_completion, 
                    excess_wait_threshhold,extreme_wait_threshhold,
                    excess_OT_threshhold,
                    extreme_OT_threshhold)
    df[ii,] = data.frame(Policy, 
                         KPI$wait_mean,KPI$wait_sd,
                         mean(rKPI$T_Vec), sd(rKPI$T_Vec),
                         KPI$OT_mean, KPI$OT_sd,
                         KPI$Idle_mean, KPI$Idle_sd,
                         KPI$excess_wait_rate,KPI$extreme_wait_rate,
                         KPI$excess_OT_rate,KPI$extreme_OT_rate,
                         stringsAsFactors = F)
  }
  return(df)
}

FullSeq_3Types_SimKPI <- function(CommonServTime,Type1, Type2, Type3, n1, n2, n3, X_Vec,
                                  plan_completion = 4*3600L, # 4-hour session
                                  excess_wait_threshhold = 15*60L, # 15 min for excess wait
                                  extreme_wait_threshhold = 45*60L, # 45 min for extreme wait
                                  excess_OT_threshhold = 30*60L, # 30 min for excess OT
                                  extreme_OT_threshhold = 45*60L #45 min for extreme OT
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
  ## if one of n1, n2, n3 = 0, raise an error, and ask the user to use FullSeq_2Types_SimKPI instead!
  assertthat::assert_that(n1 != 0 && n2 != 0 && n3 != 0,  
                          msg = 'One of n1, n2 and n3 is zero! Use FullSeq_2Types_SimKPI!!!')
  
  # Idea:
  # first choose n1 slots from n and mark as type 1, and loop over these choices -- outer loop
  # secondly, amid the (n-n1) slots, choose n2 of them for type 2 -- inner loop
  # key: map the position of type 2 in (n-n1) slots to the original index
  # This can be done by remembering the indices of those (n-n1) slots
  n_seq = choose(n,n1) * choose(n-n1,n2) 
  
  df <- data.frame(Policy=character(n_seq),
                   mean.wait=numeric(n_seq), sd.wait=numeric(n_seq), 
                   mean.T = numeric(n_seq), sd.T=numeric(n_seq), 
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
      schd= list(Type = rep(Type3,n), X_Vec= X_Vec)                
      schd$Type[Type1.index] = Type1
      schd$Type[Type2.index] = Type2
      rKPI <- schd2SimKPI(CommonServTime, schd)
      KPI <- rKPI2KPI(rKPI, 
                      plan_completion, 
                      excess_wait_threshhold,extreme_wait_threshhold,
                      excess_OT_threshhold,
                      extreme_OT_threshhold)
      df[ind,] = data.frame(Policy, 
                            KPI$wait_mean,KPI$wait_sd,
                            mean(rKPI$T_Vec), sd(rKPI$T_Vec),
                            KPI$OT_mean, KPI$OT_sd,
                            KPI$Idle_mean, KPI$Idle_sd,
                            KPI$excess_wait_rate,KPI$extreme_wait_rate,
                            KPI$excess_OT_rate,KPI$extreme_OT_rate,
                            stringsAsFactors = F)
    }
  }
  return(df)
}

Test_FullSeq2 <- function(){
  # use Initial vs Follow-up = 1:3 to test the full sequencing
  df <- read.csv('../data/Hangu_Data_Manual_Raw.csv',stringsAsFactors = F) %>%
    rename(Type=VisitType)
  
  df$Duration <- df$ServiceTime*60
  ServList = Data2ServiceList(df,Type_set =  NULL, General=T)
  CommonServTime = ServL2ComServT(ServList,seed = 123, 
                                  n_samp= 10000, # num of replication
                                  max_n = 16, # num of patients/jobs
                                  Type_set = NULL # Type_set implied by ServList
  )
  print("*****After Getting the CommonServTime matrices:******")
  print(str(CommonServTime))
  n = 16; inter_arrival = 15
  X_Vec = rep(inter_arrival*60, n-1)
  n1 = 4; n2 = 12
  df.res <- FullSeq_2Types_SimKPI(CommonServTime,'Initial', 'Follow-up', 4, 12, X_Vec)
  print("*****After Full sequencing of two types (new vs return):******")
  print(str(df.res))
  print(head(df.res))
  return(df.res)
}

Test <- function(){
  # conduct some test during the development process
  df <- read.csv('../data/Hangu_Data_Manual_Raw.csv',stringsAsFactors = F)
  df$Duration <- df$ServiceTime*60
  print("*****After data input into 'df' and turning into A,B,C types with Duration in Seconds:******")
  print(str(df))
  print(table(df$Type))
  ServList = Data2ServiceList(df,Type_set =  NULL, General=T)
  print("*****After extracting 'Duration' of each type from 'df':******")
  print(str(ServList))
  CommonServTime = ServL2ComServT(ServList,seed = 123, 
                                  n_samp= 10000, # num of replication
                                  max_n = 16, # num of patients/jobs
                                  Type_set = NULL # Type_set implied by ServList
                                  )
  print("*****After Getting the CommonServTime matrices:******")
  print(str(CommonServTime))
  
  n = 16; inter_arrival = 15
  X_Vec = rep(inter_arrival*60, n-1)
  rKPI = SimKPI(CommonServTime$General,X_Vec)
  print("*****After Simulating the homogeneous group with 15min inter-arrival:******")
  KPI = rKPI2KPI(rKPI,PRINT=T)
  print(str(KPI))
  ######
  schd = list(Type=rep('General',n),X_Vec= X_Vec)
  R_mat = schd2R_mat(CommonServTime, schd)
  rKPI = SimKPI(R_mat,X_Vec)
  print("*****After Simulating the homogeneous group with 15min inter-arrival:******")
  KPI = rKPI2KPI(rKPI,PRINT=T)
  print(str(KPI))
}

Test_Idle <- function(){
  print('Testing for Idling')
  n = 4; n_samp = 3 # 4 patients and 3 replications
  R_mat = matrix(nrow=n_samp, ncol=n)
  R_mat[1,] = c(10,11.1, 12.02, 13.003)
  R_mat[2,] = c(1, 1.1, 2.02, 3.003)
  R_mat[3,] = c(1, 101.1, 202.02, 303.003)
  X_Vec = rep(6,n-1)
  rKPI = SimKPI(R_mat,X_Vec)
  rKPI2KPI(rKPI, plan_completion = sum(X_Vec)+3.1415927,PRINT = T)
}

Test_FullSeq3 <- function(){
  # use 1 Initial-Cancer, 3 Initial-NonCancer and  12 Non-Initial to test the full sequencing
  df <- read.csv('../data/Hangu_Data_Manual_Raw.csv',stringsAsFactors = F) %>%
    rename(Type=VisitType)
  
  df$Duration <- df$ServiceTime*60
  # The following set-up the types for the experiment: 'InitCancer','InitNonCancer','Follow-up'
  tmp <- rep('Follow-up',nrow(df));
  tmp[df$Type=='Initial' & df$Condition == 'Cancer'] = 'InitCancer'
  tmp[df$Type=='Initial' & df$Condition != 'Cancer'] = 'InitNonCancer'
  df$Type = tmp
  ServList = Data2ServiceList(df,Type_set =  c('InitCancer','InitNonCancer','Follow-up'), General=T)
  CommonServTime = ServL2ComServT(ServList,seed = 123, 
                                  n_samp= 10000, # num of replication
                                  max_n = 16, # num of patients/jobs
                                  Type_set = c('InitCancer','InitNonCancer','Follow-up')
  )
  print("*****After Getting the CommonServTime matrices:******")
  print(str(CommonServTime))
  n = 16; inter_arrival = 15
  X_Vec = rep(inter_arrival*60, n-1)
  n1 = 1; n2 = 3; n3 = 12; n = n1 + n2 + n3
  df.res <- FullSeq_3Types_SimKPI(CommonServTime,'InitCancer','InitNonCancer','Follow-up', 1,3, 12, X_Vec)
  print("*****After Full sequencing of 3 types ('InitCancer','InitNonCancer','Follow-up'):******")
  print(str(df.res))
  print(head(df.res))
  return(df.res)
}

# TESTING = T
if(exists('TESTING') && TESTING) {
  Test()
}
if(exists('TESTING_IDLE') && TESTING_IDLE) Test_Idle()

if(exists('TESTING_FullSeq2') && TESTING_FullSeq2) {
  df.res <-Test_FullSeq2()
  savefile = './RawOutput/FullSeq2_Test/Init_Follow_4_12.csv'
  write.csv(x=df.res, file=savefile, row.names = F)
}

if(exists('TESTING_FullSeq3') && TESTING_FullSeq3) {
  df.res <-Test_FullSeq3()
  mydir = dirname(rstudioapi::getSourceEditorContext()$path)
  savefile = file.path(mydir, 'RawOutput/FullSeq3_Test/1IC_3INC_12NI.csv')
  if(!dir.exists(dirname(savefile))) dir.create(dirname(savefile),recursive = T)
  write.csv(x=df.res, file=savefile, row.names = F)
}