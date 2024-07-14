
Data2GenderList <- function(df,Type_set = NULL, General=T){
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
  
  GenderList = list()
  if(General)  GenderList$General <- df$Gender #General Gender
  for(type in Type_set){
    GenderList[[type]] <- df %>% filter(Type==type) %>% pull(Gender) #Gender Data for each types
  }  
  return(GenderList)
}

Data2AddressList <- function(df, Type_set = NULL, General=T){
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
  
  AddressList = list()
  if(General)  AddressList$General <- df$Address #General Gender
  for(type in Type_set){
    AddressList[[type]] <- df %>% filter(Type==type) %>% pull(Address) #Gender Data for each types
  }  
  return(AddressList)
}

SGList2ComSGList <- 
  function(ServList, GenderList, seed = 123, 
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
    ### Save the random state if it exists, and then restore it at the end
    if (exists(".Random.seed")) oldseed <- .Random.seed
    set.seed(seed)
    CommonServTime = list()
    CommonGender = list()
    for(type in Type_set){
      CommonServTime[[type]]=
        matrix(data=sample(x=ServList[[type]],size=max_n*n_samp,replace = T),
               nrow = n_samp, ncol= max_n)
    }
    ### Restore the random state if it was saved
    if (exists("oldseed")) .Random.seed <- oldseed
    
    ### Now repeat the process for generating gender information
    ### Save the random state if it exists, and then restore it at the end
    if (exists(".Random.seed")) oldseed <- .Random.seed
    set.seed(seed)
    for(type in Type_set){
      CommonGender[[type]]=
        matrix(data=sample(x=GenderList[[type]],size=max_n*n_samp,replace = T),
               nrow = n_samp, ncol= max_n)
    }
    ### Restore the random state if it was saved
    if (exists("oldseed")) .Random.seed <- oldseed
    
    return(list(ServTime=CommonServTime, Gender=CommonGender))
  }

SGAList2ComSGList <- 
  function(ServList, GenderList, AddressList, seed = 123, 
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
    
    ### Save the random state if it exists, and then restore it at the end
    if (exists(".Random.seed")) oldseed <- .Random.seed
    set.seed(seed)
    
    CommonServTime = list()
    CommonGender = list()
    CommonAddress = list()
    for(type in Type_set){
      CommonServTime[[type]]=
        matrix(data=sample(x=ServList[[type]],size=max_n*n_samp,replace = T),
               nrow = n_samp, ncol= max_n)
    }
    ### Restore the random state if it was saved
    if (exists("oldseed")) .Random.seed <- oldseed
    
    ### Now repeat the process for generating gender information
    ### Save the random state if it exists, and then restore it at the end
    if (exists(".Random.seed")) oldseed <- .Random.seed
    set.seed(seed)
    for(type in Type_set){
      CommonGender[[type]]=
        matrix(data=sample(x=GenderList[[type]],size=max_n*n_samp,replace = T),
               nrow = n_samp, ncol= max_n)
    }
    ### Restore the random state if it was saved
    if (exists("oldseed")) .Random.seed <- oldseed
    
    ### Now repeat the process for generating gender information
    ### Save the random state if it exists, and then restore it at the end
    if (exists(".Random.seed")) oldseed <- .Random.seed
    set.seed(seed)
    for(type in Type_set){
      CommonAddress[[type]]=
        matrix(data=sample(x=AddressList[[type]],size=max_n*n_samp,replace = T),
               nrow = n_samp, ncol= max_n)
    }
    ### Restore the random state if it was saved
    if (exists("oldseed")) .Random.seed <- oldseed
    
    return(list(ServTime=CommonServTime, Gender=CommonGender, Address=CommonAddress))
  }

schd2RTGA_mat <- function(CommonServTime,CommonGender,CommonAddress,schd){
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
  
  RT_mat = matrix(nrow = n_samp, ncol= n) #Realized service time
  RG_mat = matrix(nrow = n_samp, ncol= n) #Realized patient gender
  RA_mat = matrix(nrow = n_samp, ncol= n) #Realized patient address
  #cur= list();
  #for(type in names(CommonServTime)){ # initialization
  #  cur[[ type ]] = 1
  #}
  for(type in Type_set){
    col_id = schd$Type == type
    N = sum(col_id)
    stopifnot(N>=1L) # if N<1, 1:N is not what we expected.
    RT_mat[, col_id] = CommonServTime[[type]][,1:N]
    RG_mat[, col_id] = CommonGender[[type]][,1:N]
    RA_mat[, col_id] = CommonAddress[[type]][,1:N]
  }
  return(list(ServTime=RT_mat, Gender=RG_mat, Address=RA_mat))
}

GenderWKPI <-function(G_mat,RW_mat,Gender){
  # 
  Gender_Ind=G_mat==Gender
  
  # Gmean_tot=mean(RW_mat[Gender_Ind])
  # #Gvar_tot=var(RW_mat[Gender_Ind])
  # Gstd_tot=sd(RW_mat[Gender_Ind])
  
  RW_mat_masked <- RW_mat
  RW_mat_masked[!Gender_Ind] <- NA
  
  # Calculate the mean for each column
  Gmean_Slot <- colMeans(RW_mat_masked, na.rm = TRUE)
  Gmean_all <- mean(RW_mat_masked, na.rm = TRUE)
  Gsample <- na.omit(as.vector(RW_mat_masked))
  Gstd_all <- sd(RW_mat_masked, na.rm = TRUE)
  
  #calculate the variance for each column
  #Gvar_Slot <- apply(RW_mat_masked, 2, var, na.rm = TRUE)
  #Gstd_Slot <- apply(RW_mat_masked, 2, sd, na.rm = TRUE)
  
  #return(list(Gmean_tot=Gmean_tot,Gvar_tot=Gvar_tot,Gmean_Slot=Gmean_Slot,Gvar_Slot=Gvar_Slot))
  
  return(list(Gmean_Slot=Gmean_Slot,Gmean_Slot_all=mean(Gmean_Slot),Gstd_Slot_all=sd(Gmean_Slot),Gmean_all=Gmean_all,Gsample=Gsample,Gstd_all=Gstd_all))
}

AddressWKPI <-function(A_mat,RW_mat,Address){
  # 
  Address_Ind=A_mat==Address
  
  # Gmean_tot=mean(RW_mat[Gender_Ind])
  # #Gvar_tot=var(RW_mat[Gender_Ind])
  # Gstd_tot=sd(RW_mat[Gender_Ind])
  
  RW_mat_masked <- RW_mat
  RW_mat_masked[!Address_Ind] <- NA
  
  # Calculate the mean for each column
  Amean_Slot <- colMeans(RW_mat_masked, na.rm = TRUE)
  Amean_all <- mean(RW_mat_masked, na.rm = TRUE)
  Asample <- na.omit(as.vector(RW_mat_masked))
  Astd_all <- sd(RW_mat_masked, na.rm = TRUE)
  
  #calculate the variance for each column
  #Gvar_Slot <- apply(RW_mat_masked, 2, var, na.rm = TRUE)
  #Gstd_Slot <- apply(RW_mat_masked, 2, sd, na.rm = TRUE)
  
  #return(list(Gmean_tot=Gmean_tot,Gvar_tot=Gvar_tot,Gmean_Slot=Gmean_Slot,Gvar_Slot=Gvar_Slot))
  
  return(list(Amean_Slot=Amean_Slot,Amean_Slot_all=mean(Amean_Slot),Astd_Slot_all=sd(Amean_Slot),Amean_all=Amean_all,Asample=Asample,Astd_all=Astd_all))
}

Schd2GenderKPI<-function(CommonServTime,CommonGender,schd){
  RTG_mat = schd2RTG_mat(CommonServTime,CommonGender,schd) # realized service time and gender matrix obtained from CommonServTime
  R_mat = RTG_mat$ServTime
  G_mat = RTG_mat$Gender
  rKPI = SimKPIH(R_mat,schd$X_Vec) #raw KPI (W_Mat,T_Vec,Idle_Vec)
  RW_mat = rKPI$W_Mat
  GenderM_KPI=GenderWKPI(G_mat,RW_mat,Gender='M')
  GenderF_KPI=GenderWKPI(G_mat,RW_mat,Gender='F')
  return(list(rKPI=rKPI,Male=GenderM_KPI,Female=GenderF_KPI))
}

Schd2Gender_Address_KPI<-function(CommonServTime,CommonGender,CommonAddress,schd){
  RTGA_mat = schd2RTGA_mat(CommonServTime,CommonGender,CommonAddress,schd) # realized service time and gender matrix obtained from CommonServTime
  R_mat = RTGA_mat$ServTime
  G_mat = RTGA_mat$Gender
  A_mat = RTGA_mat$Address
  rKPI = SimKPIH(R_mat,schd$X_Vec) #raw KPI (W_Mat,T_Vec,Idle_Vec)
  RW_mat = rKPI$W_Mat
  GenderM_KPI=GenderWKPI(G_mat,RW_mat,Gender='M')
  GenderF_KPI=GenderWKPI(G_mat,RW_mat,Gender='F')
  AddressIn_KPI=AddressWKPI(A_mat,RW_mat,Address='In the city')
  AddressOut_KPI=AddressWKPI(A_mat,RW_mat,Address='Out of city')
  AddressOutP_KPI=AddressWKPI(A_mat,RW_mat,Address='Out of province')
  AddressNaN_KPI=AddressWKPI(A_mat,RW_mat,Address=NA)
  return(list(rKPI=rKPI,Male=GenderM_KPI,Female=GenderF_KPI,In=AddressIn_KPI,Out=AddressOut_KPI,OutP=AddressOutP_KPI,isNaN=AddressNaN_KPI))
}


RuleName2schd<-function(rule,n=16){
  Type <- unlist(strsplit(rule, ""))
  X_Vec = rep(15*(60/sec), n-1)
  schd = list(Type=Type, X_Vec = rep(15*(60/sec), n-1))
  return(schd)
}


Plot_GenderK2_Wait_mean_std_Slot<-function(result,Ci){
  #if (Ci==0){ylable="Optcost_withOutIdle"}else{ylable="Optcost_withIdle"}
  #ylable = expression(Mean(W) ("in minutes"))
  ylable <- expression(Time * " (" * italic("in minutes") * ")")
  mydata = result
  
  column_mapping <- list("K2CRG_Male_meanW" = "K2_Male_Mean(W)","K2CRG_Female_meanW" = "K2_Female_Mean(W)",
                         "K2CRG_Male_stdW" = "K2_Male_STD(W)","K2CRG_Female_stdW" = "K2_Female_STD(W)")
  
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
    theme(legend.position = c(0.99, 0.70), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 10),
          #legend.title = element_text(size = 8),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(1,7),breaks = seq(1, 7, 0.5))+
    labs(title = bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]),y=ylable)+ #or opt_cost_withOutIdle
    theme(plot.title = element_text(size = 15))+
    guides(color = guide_legend(nrow = 4, ncol = 1)) 
  
  return(p1)
  
}

Plot_GenderK2_Wait_mean_std_Slot_ALL<-function(result,Ci){
  #if (Ci==0){ylable="Optcost_withOutIdle"}else{ylable="Optcost_withIdle"}
  #ylable = expression(Mean(W) ("in minutes"))
  ylable <- expression(Time * " (" * italic("in minutes") * ")")
  mydata = result
  
  column_mapping <- list("K2CRG_Male_meanW" = "K2_Male_Mean(W)","K2CRG_Female_meanW" = "K2_Female_Mean(W)",
                         "K2CRG_Male_stdW" = "K2_Male_STD(W)","K2CRG_Female_stdW" = "K2_Female_STD(W)")
  
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
    theme(legend.position = c(0.99, 0.70), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 10),
          #legend.title = element_text(size = 8),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(4,11),breaks = seq(4, 11, 0.5))+
    labs(title = bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]),y=ylable)+ #or opt_cost_withOutIdle
    theme(plot.title = element_text(size = 15))+
    guides(color = guide_legend(nrow = 4, ncol = 1)) 
  
  return(p1)
  
}

Plot_Gender_Wait_mean_Slot_ALL<-function(result,Ci){
  #if (Ci==0){ylable="Optcost_withOutIdle"}else{ylable="Optcost_withIdle"}
  #ylable = expression(Mean(W) ("in minutes"))
  ylable <- expression(Mean(W) * " (" * italic("in minutes") * ")")
  mydata = result
  
  column_mapping <- list("K2CRG_Male_meanW" = "K2_Male","K2CRG_Female_meanW" = "K2_Female",
                         "K3CRG_Male_meanW" = "K3_Male","K3CRG_Female_meanW" = "K3_Female")
  
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
    theme(legend.position = c(0.99, 0.30), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 10),
          #legend.title = element_text(size = 8),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(4,7),breaks = seq(4, 7, 0.5))+
    labs(title = bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]),y=ylable)+ #or opt_cost_withOutIdle
    theme(plot.title = element_text(size = 15))+
    guides(color = guide_legend(nrow = 2, ncol = 1)) 
  
  return(p1)
  
}

Plot_GenderK2_Wait_mean_std_Slot_PRO_test <- function(result, Ci) {
  
  ylable1 <- expression(Mean(W) * " (" * italic("in minutes") * ")")
  ylable2 <- expression(STD(W) * " (" * italic("in minutes") * ")")
  
  mydata <- result
  
  column_mapping <- list(
    "K2CRG_Male_meanW" = "K2_Male_Mean(W)",
    "K2CRG_Female_meanW" = "K2_Female_Mean(W)",
    "K2CRG_Male_stdW" = "K2_Male_STD(W)",
    "K2CRG_Female_stdW" = "K2_Female_STD(W)"
  )
  
  names(mydata) <- lapply(names(mydata), function(x) ifelse(x %in% names(column_mapping), column_mapping[[x]], x))
  
  mydata <- melt(mydata, id = "Co")
  colnames(mydata) <- c("Co_value", "Policy", "Opt_cost") # 修改列名
  
  p1 <- ggplot(data = mydata, aes(x = Co_value, y = Opt_cost, group = Policy, color = Policy, shape = Policy)) +
    theme_bw() +
    theme(
      panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
      axis.text.x = element_text(size = 15, angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text.y = element_text(size = 15, hjust = 0.5, vjust = 0.5),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      panel.grid = element_blank(),
      legend.position = c(0.99, 0.70),
      legend.justification = c(1, 1),
      legend.text = element_text(size = 10),
      legend.title = element_blank(),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.key = element_rect(fill = "transparent", color = NA)
    ) +
    geom_point(size = 2.5) +
    geom_line() +
    scale_x_continuous(limits = c(1, 10), breaks = seq(1, 10, 1)) +
    scale_y_continuous(
      name = ylable1,
      limits = c(4, 11),  # 设置左边坐标轴范围为4到7
      breaks = c(4, 0.5, 7),
      sec.axis = sec_axis(~ ., name = ylable2, breaks = c(7, 0.5, 11))
    ) +
    labs(
      title = bquote(paste(c[I], "=", .(Ci))),
      x = expression(c[O])
    ) +
    theme(plot.title = element_text(size = 15))
  
  return(p1)
}



Plot_GenderK2K3_Wait_mean_Slot<-function(result,Ci){
  #if (Ci==0){ylable="Optcost_withOutIdle"}else{ylable="Optcost_withIdle"}
  #ylable = expression(Mean(W) ("in minutes"))
  ylable <- expression(Mean(W) * " (" * italic("in minutes") * ")")
  mydata = result
  
  column_mapping <- list("K2CRG_Male_meanW" = "K2+CRG_Male","K2CRG_Female_meanW" = "K2+CRG_Female",
                         "K3CRG_Male_meanW" = "K3+CRG_Male","K3CRG_Female_meanW" = "K3+CRG_Female")
  
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
    theme(legend.position = c(0.99, 0.38), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 10),
          #legend.title = element_text(size = 8),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(4,7),breaks = seq(4, 7, 0.5))+
    labs(title = bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]),y=ylable)+ #or opt_cost_withOutIdle
    theme(plot.title = element_text(size = 15))+
    guides(color = guide_legend(nrow = 4, ncol = 1)) 
  
  return(p1)
  
}


Plot_GenderK3_Wait_mean_std_Slot<-function(result,Ci){
  #if (Ci==0){ylable="Optcost_withOutIdle"}else{ylable="Optcost_withIdle"}
  #ylable = expression(STD(W) ("in minutes"))
  ylable <- expression(Time * " (" * italic("in minutes") * ")")
  
  mydata = result
  
  column_mapping <- list("K3CRG_Male_meanW" = "K3_Male_Mean(W)","K3CRG_Female_meanW" = "K3_Female_Mean(W)",
                         "K3CRG_Male_stdW" = "K3_Male_STD(W)","K3CRG_Female_stdW" = "K3_Female_STD(W)")
  
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
    theme(legend.position = c(0.99, 0.63), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 10),
          #legend.title = element_text(size = 8),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(1,7),breaks = seq(1, 7, 0.5))+
    labs(title = bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]),y=ylable)+ #or opt_cost_withOutIdle
    theme(plot.title = element_text(size = 15))+
    guides(color = guide_legend(nrow = 4, ncol = 1)) 
  
  return(p1)
  
}


Plot_GenderK3_Wait_mean_std_Slot_ALL<-function(result,Ci){
  #if (Ci==0){ylable="Optcost_withOutIdle"}else{ylable="Optcost_withIdle"}
  #ylable = expression(STD(W) ("in minutes"))
  ylable <- expression(Time * " (" * italic("in minutes") * ")")
  
  mydata = result
  
  column_mapping <- list("K3CRG_Male_meanW" = "K3_Male_Mean(W)","K3CRG_Female_meanW" = "K3_Female_Mean(W)",
                         "K3CRG_Male_stdW" = "K3_Male_STD(W)","K3CRG_Female_stdW" = "K3_Female_STD(W)")
  
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
    theme(legend.position = c(0.99, 0.63), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 10),
          #legend.title = element_text(size = 8),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(4,10),breaks = seq(4, 10, 0.5))+
    labs(title = bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]),y=ylable)+ #or opt_cost_withOutIdle
    theme(plot.title = element_text(size = 15))+
    guides(color = guide_legend(nrow = 4, ncol = 1)) 
  
  return(p1)
  
}


Plot_Gender_Wait_std_Slot_ALL<-function(result,Ci){
  #if (Ci==0){ylable="Optcost_withOutIdle"}else{ylable="Optcost_withIdle"}
  #ylable = expression(STD(W) ("in minutes"))
  ylable <- expression(STD(W) * " (" * italic("in minutes") * ")")
  
  mydata = result
  
  column_mapping <- list("K2CRG_Male_stdW" = "K2_Male","K2CRG_Female_stdW" = "K2_Female",
                         "K3CRG_Male_stdW" = "K3_Male","K3CRG_Female_stdW" = "K3_Female")
  
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
    theme(legend.position = c(0.99, 0.27), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 10),
          #legend.title = element_text(size = 8),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(7,11),breaks = seq(7, 11, 0.5))+
    labs(title = bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]),y=ylable)+ #or opt_cost_withOutIdle
    theme(plot.title = element_text(size = 15))+
    guides(color = guide_legend(nrow = 2, ncol = 1)) 
  
  return(p1)
  
}


Plot_GenderK2K3_Wait_std_Slot<-function(result,Ci){
  #if (Ci==0){ylable="Optcost_withOutIdle"}else{ylable="Optcost_withIdle"}
  #ylable = expression(STD(W) ("in minutes"))
  ylable <- expression(STD(W) * " (" * italic("in minutes") * ")")
  
  mydata = result
  
  column_mapping <- list("K2CRG_Male_stdW" = "K2+CRG_Male","K2CRG_Female_stdW" = "K2+CRG_Female",
                         "K3CRG_Male_stdW" = "K3+CRG_Male","K3CRG_Female_stdW" = "K3+CRG_Female")
  
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
    theme(legend.position = c(0.99, 0.38), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 10),
          #legend.title = element_text(size = 8),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(1,3),breaks = seq(1, 3, 0.5))+
    labs(title = bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]),y=ylable)+ #or opt_cost_withOutIdle
    theme(plot.title = element_text(size = 15))+
    guides(color = guide_legend(nrow = 4, ncol = 1)) 
  
  return(p1)
  
}

Plot_GenderK2K3_Wait_std_Slot_ALL<-function(result,Ci){
  #if (Ci==0){ylable="Optcost_withOutIdle"}else{ylable="Optcost_withIdle"}
  #ylable = expression(STD(W) ("in minutes"))
  ylable <- expression(STD(W) * " (" * italic("in minutes") * ")")
  
  mydata = result
  
  column_mapping <- list("K2CRG_Male_stdW" = "K2+CRG_Male","K2CRG_Female_stdW" = "K2+CRG_Female",
                         "K3CRG_Male_stdW" = "K3+CRG_Male","K3CRG_Female_stdW" = "K3+CRG_Female")
  
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
    theme(legend.position = c(0.99, 0.38), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 10),
          #legend.title = element_text(size = 8),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(7,11),breaks = seq(7, 11, 0.5))+
    labs(title = bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]),y=ylable)+ #or opt_cost_withOutIdle
    theme(plot.title = element_text(size = 15))+
    guides(color = guide_legend(nrow = 4, ncol = 1)) 
  
  return(p1)
  
}



Lookup_OptRecord_SVF<-function(FullSeq,Scenario_list,Ci,Co,n){
  #return the optimal NonFullSeq record 
  FullSeq=CostValueFunc(FullSeq,Ci,Co,n)
  FullSeq$NonFullSeq=FALSE
  
  Name=Scenario_list$Type
  Rule_name=paste(Name[1:length(Name)], collapse="")
  FullSeq$NonFullSeq[which(FullSeq$Rule_name==Rule_name)]=TRUE
  
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


Plot_Co_Optcost_MoRule<-function(result,Ci){
  #if (Ci==0){ylable="Optcost_withOutIdle"}else{ylable="Optcost_withIdle"}
  #ylable="Optimal cost"
  #ylable = expression("Lowest  " ~ frac(italic(z), italic(z[F])) ~ "  value")
  ylable = expression(frac(italic(z), italic(z[F])))
  mydata = result
  
  column_mapping <- list("CPS_K2_CRG" = "K2+CRG", "CPS_K3_CRG" = "K3+CRG",
                         "NewReturn_MoRule" = "New/Return+MR", "NewReturn_Pair" = "New/Return+PI",
                         "K2MoRule" = "K2+MR","K3MoRule" = "K3+MR")
  
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
    theme(legend.position = c(0.90, 0.25), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 10),
          #legend.title = element_text(size = 10),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(0.80,1.10),breaks = seq(0.80, 1.10, 0.05))+
    labs(title = bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]),y=ylable)+ #or opt_cost_withOutIdle
    theme(plot.title = element_text(size = 15))+
    guides(color = guide_legend(nrow = 2, ncol = 2)) 
  
  return(p1)
  
}

Plot_Co_Optcost_MoRule_NR<-function(result,Ci){
  #if (Ci==0){ylable="Optcost_withOutIdle"}else{ylable="Optcost_withIdle"}
  #ylable="Optimal cost"
  #ylable = expression("Lowest  " ~ frac(italic(z), italic(z[F])) ~ "  value")
  ylable = expression(frac(italic(z), italic(z[F])))
  mydata = result
  
  column_mapping <- list("CPS_K2_CRG" = "K2+CRG", "CPS_K3_CRG" = "K3+CRG","New_Return_CRG" = "New/Return+CRG",
                         "NewReturn_MoRule" = "New/Return+MR", "NewReturn_Pair" = "New/Return+PI",
                         "K2MoRule" = "K2+MR","K3MoRule" = "K3+MR")
  
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
    theme(legend.position = c(0.95, 0.25), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 10),
          #legend.title = element_text(size = 10),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(0.80,1.10),breaks = seq(0.80, 1.10, 0.05))+
    labs(title = bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]),y=ylable)+ #or opt_cost_withOutIdle
    theme(plot.title = element_text(size = 15))+
    guides(color = guide_legend(nrow = 2, ncol = 2)) 
  
  return(p1)
  
}


Plot_Co_Optcost_Pair<-function(result,Ci){
  #if (Ci==0){ylable="Optcost_withOutIdle"}else{ylable="Optcost_withIdle"}
  #ylable="Optimal cost"
  #ylable = expression("Lowest  " ~ frac(italic(z), italic(z[F])) ~ "  value")
  ylable = expression(frac(italic(z), italic(z[F])))
  mydata = result
  
  column_mapping <- list("CPS_K2_CRG" = "K2+CRG", "CPS_K2_Enum" = "K2+Enum", 
                         "CPS_K3_CRG" = "K3+CRG", "CPS_K3_Enum" = "K3+Enum",
                         "K2SVF" = "K2+SVF","K3SVF" = "K3+SVF",
                         "K2Pair" = "K2+PI","K3Pair" = "K3+PI")
  
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
    theme(legend.position = c(0.75, 0.25), # Set the legend in the upper right corner
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
    scale_y_continuous(limits = c(0.80,1.00),breaks = seq(0.80, 1.00, 0.05))+
    labs(title = bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]),y=ylable)+ #or opt_cost_withOutIdle
    theme(plot.title = element_text(size = 15))+
    guides(color = guide_legend(nrow = 2, ncol = 2)) 
  
  return(p1)
  
}

Plot_Co_Optcost_Pair_NR<-function(result,Ci){
  #if (Ci==0){ylable="Optcost_withOutIdle"}else{ylable="Optcost_withIdle"}
  #ylable="Optimal cost"
  #ylable = expression("Lowest  " ~ frac(italic(z), italic(z[F])) ~ "  value")
  ylable = expression(frac(italic(z), italic(z[F])))
  mydata = result
  
  column_mapping <- list("CPS_K2_CRG" = "K2+CRG", "CPS_K2_Enum" = "K2+Enum", 
                         "CPS_K3_CRG" = "K3+CRG", "CPS_K3_Enum" = "K3+Enum",
                         "K2SVF" = "K2+SVF","K3SVF" = "K3+SVF",
                         "K2Pair" = "K2+PI","K3Pair" = "K3+PI","NewReturn_Pair" = "New/Return+PI")
  
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
    theme(legend.position = c(0.6, 0.35), # Set the legend in the upper right corner
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
    scale_y_continuous(limits = c(0.80,1.00),breaks = seq(0.80, 1.00, 0.05))+
    labs(title = bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]),y=ylable)+ #or opt_cost_withOutIdle
    theme(plot.title = element_text(size = 15))#+
    #guides(color = guide_legend(nrow = 2, ncol = 2)) 
  
  return(p1)
  
}

Lookup_OptRecord_pairwise<-function(FullSeq,Scenario_list,Ci,Co,n){
  #return the optimal NonFullSeq record 
  FullSeq=CostValueFunc(FullSeq,Ci,Co,n)
  FullSeq$NonFullSeq=FALSE
  
  Name=Scenario_list
  Rule_name=paste(Name[1:length(Name)], collapse="")
  FullSeq$NonFullSeq[which(FullSeq$Rule_name==Rule_name)]=TRUE
  
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


Pairwise_interchange <- function(FullSeq,Scenario_list,Ci,Co,n) {
  n <- length(Scenario_list)
  stop <- FALSE
  
  while (!stop) {
    best_record <- Lookup_OptRecord_pairwise(FullSeq,Scenario_list,Ci,Co,n)
    best_cost <- best_record$Cost
    stop <- TRUE  # Assume we've found the best scenario
    
    # Loop through each pair of patients
    for (i in 1:(n-1)) {
      for (j in (i+1):n) {
        # Only swap patients if they are of different types
        if (Scenario_list[i] != Scenario_list[j]) {
          # Swap the patients
          
          temp <- Scenario_list[i]
          Scenario_list[i] <- Scenario_list[j]
          Scenario_list[j] <- temp
          
          # Check if this swap reduces the cost
          new_record <- Lookup_OptRecord_pairwise(FullSeq,Scenario_list,Ci,Co,n)
          new_cost <- new_record$Cost
          if (new_cost < best_cost) {
            best_cost <- new_cost
            best_record <- new_record
            stop <- FALSE  # We found a better scenario, continue searching
          } else {
            # Revert the swap
            temp <- Scenario_list[i]
            Scenario_list[i] <- Scenario_list[j]
            Scenario_list[j] <- temp
          }
        }
      }
    }
  }
  
  return(best_record)
}


Pairwise_interchange_Pro <- function(FullSeq,Scenario_list,Ci,Co,n) {
  n <- length(Scenario_list)
  stop <- FALSE
  
  while (!stop) {
    best_record <- Lookup_OptRecord_pairwise(FullSeq,Scenario_list,Ci,Co,n)
    best_cost <- best_record$Cost
    stop <- TRUE  # Assume we've found the best scenario
    
    # Loop through each pair of patients
    for (i in 1:(n-1)) {
      for (j in (i+1):n) {
        new_List <- Scenario_list
        # Only swap patients if they are of different types
        if (new_List[i] != new_List[j]) {
          # Swap the patients
          new_List[c(i,j)]=new_List[c(j,i)]
          # Check if this swap reduces the cost
          new_record <- Lookup_OptRecord_pairwise(FullSeq,new_List,Ci,Co,n)
          new_cost <- new_record$Cost
          if (new_cost < best_cost) {
            best_cost <- new_cost
            best_List <- new_List
            stop <- FALSE  # We found a better scenario, continue searching
          } 
        }
      }
    }
    if (stop == TRUE){
      break
    }else{
      Scenario_list=best_List
      }
  }
  
  return(best_record)
}

Plot_Co_Wait_sd_Pair_Slot<-function(result,Ci){
  #if (Ci==0){ylable="Optcost_withOutIdle"}else{ylable="Optcost_withIdle"}
  #ylable="Standard deviation of waiting time (minute)"
  #ylable = expression("STD"[W]*" (in minutes)")
  #ylable = expression(frac(STD(w), STD[F]~(W)))
  ylable = expression(STD(W)/STD[F]~(W))
  mydata = result
  
  column_mapping <- list("CPSK2_CRG" = "K2+CRG","CPSK3_CRG" = "K3+CRG",
                         "K2_Pair_Wsd" = "K2+PI", "K3_Pair_Wsd" = "K3+PI",
                         "NR_Pair_Wsd" = "New/Return+PI")
  
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
    theme(legend.position = c(0.99, 0.25), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 10),
          #legend.title = element_text(size = 8),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(0.3,2.5),breaks = seq(0.3, 2.5, 0.2))+
    labs(title = bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]),y=ylable)+ #or opt_cost_withOutIdle
    theme(plot.title = element_text(size = 15))+
    guides(color = guide_legend(nrow = 2, ncol = 3)) 
  
  return(p1)
  
}


Plot_Co_Wait_sd_Mo_Slot<-function(result,Ci){
  #if (Ci==0){ylable="Optcost_withOutIdle"}else{ylable="Optcost_withIdle"}
  #ylable="Standard deviation of waiting time (minute)"
  #ylable = expression("STD"[W]*" (in minutes)")
  #ylable = expression(frac(STD(w), STD[F]~(W)))
  ylable = expression(STD(W)/STD[F]~(W))
  mydata = result
  
  column_mapping <- list("CPSK2_CRG" = "K2+CRG","CPSK3_CRG" = "K3+CRG",
                         "K2_MoRule_Wsd" = "K2+MR", "K3_MoRule_Wsd" = "K3+MR",
                         "NR_MoRule_Wsd" = "New/Return+MR")
  
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
    theme(legend.position = c(1.0, 0.21), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 9.4),
          #legend.title = element_text(size = 8),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(0.5,2.0),breaks = seq(0.6, 2.0, 0.2))+
    labs(title = bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]),y=ylable)+ #or opt_cost_withOutIdle
    theme(plot.title = element_text(size = 15))+
    guides(color = guide_legend(nrow = 2, ncol = 3)) 
  
  return(p1)
  
}


Plot_Address_Wait_mean_std_Slot<-function(result,Ci){
  #if (Ci==0){ylable="Optcost_withOutIdle"}else{ylable="Optcost_withIdle"}
  #ylable = expression(Mean(W) ("in minutes"))
  ylable <- expression(Time * " (" * italic("in minutes") * ")")
  mydata = result
  
  column_mapping <- list("K2CRG_In_meanW" = "K2_In_Mean(W)","K2CRG_Out_meanW" = "K2_Out_Mean(W)","K2CRG_NaN_meanW" = "K2_NA_Mean(W)",
                         "K2CRG_In_stdW" = "K2_In_STD(W)","K2CRG_Out_stdW" = "K2_Out_STD(W)","K2CRG_NaN_stdW" = "K2_NA_STD(W)",
                         "K3CRG_In_meanW" = "K3_In_Mean(W)","K3CRG_Out_meanW" = "K3_Out_Mean(W)","K3CRG_NaN_meanW" = "K3_NA_Mean(W)",
                         "K3CRG_In_stdW" = "K3_In_STD(W)","K3CRG_Out_stdW" = "K3_Out_STD(W)","K3CRG_NaN_stdW" = "K3_NA_STD(W)"
                         )
  
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
    theme(legend.position = c(0.99, 0.535), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 10),
          #legend.title = element_text(size = 8),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(1.5,7),breaks = seq(1.5, 7, 0.5))+
    labs(title = bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]),y=ylable)+ #or opt_cost_withOutIdle
    theme(plot.title = element_text(size = 15))+
    guides(color = guide_legend(nrow = 3, ncol = 2)) 
  
  return(p1)
  
}

Plot_Address_Wait_mean_std_Slot_ALL<-function(result,Ci){
  #if (Ci==0){ylable="Optcost_withOutIdle"}else{ylable="Optcost_withIdle"}
  #ylable = expression(Mean(W) ("in minutes"))
  ylable <- expression(Time * " (" * italic("in minutes") * ")")
  mydata = result
  
  column_mapping <- list("K2CRG_In_meanW" = "K2_In_Mean(W)","K2CRG_Out_meanW" = "K2_Out_Mean(W)","K2CRG_NaN_meanW" = "K2_NA_Mean(W)",
                         "K2CRG_In_stdW" = "K2_In_STD(W)","K2CRG_Out_stdW" = "K2_Out_STD(W)","K2CRG_NaN_stdW" = "K2_NA_STD(W)",
                         "K3CRG_In_meanW" = "K3_In_Mean(W)","K3CRG_Out_meanW" = "K3_Out_Mean(W)","K3CRG_NaN_meanW" = "K3_NA_Mean(W)",
                         "K3CRG_In_stdW" = "K3_In_STD(W)","K3CRG_Out_stdW" = "K3_Out_STD(W)","K3CRG_NaN_stdW" = "K3_NA_STD(W)"
  )
  
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
    theme(legend.position = c(0.99, 0.535), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 10),
          #legend.title = element_text(size = 8),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(3,11),breaks = seq(3, 11, 0.5))+
    labs(title = bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]),y=ylable)+ #or opt_cost_withOutIdle
    theme(plot.title = element_text(size = 15))+
    guides(color = guide_legend(nrow = 3, ncol = 2)) 
  
  return(p1)
  
}



Plot_AddressK2K3_Wait_mean_Slot<-function(result,Ci){
  #if (Ci==0){ylable="Optcost_withOutIdle"}else{ylable="Optcost_withIdle"}
  #ylable = expression(Mean(W) ("in minutes"))
  ylable <- expression(Mean(W) * " (" * italic("in minutes") * ")")
  mydata = result
  
  column_mapping <- list("K2CRG_In_meanW" = "K2+CRG_In","K2CRG_Out_meanW" = "K2+CRG_OutC","K2CRG_NaN_meanW" = "K2+CRG_NA","K2CRG_OutP_meanW" = "K2+CRG_OutP",
                         "K3CRG_In_meanW" = "K3+CRG_In","K3CRG_Out_meanW" = "K3+CRG_OutC","K3CRG_NaN_meanW" = "K3+CRG_NA","K3CRG_OutP_meanW" = "K3+CRG_OutP")
  
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
    theme(legend.position = c(1.0, 0.38), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 10),
          #legend.title = element_text(size = 8),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(4,7),breaks = seq(4, 7, 0.5))+
    labs(title = bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]),y=ylable)+ #or opt_cost_withOutIdle
    theme(plot.title = element_text(size = 15))+
    guides(color = guide_legend(nrow = 8, ncol = 1)) 
  
  return(p1)
  
}

Plot_AddressK2K3_Wait_mean_Slot_ALL<-function(result,Ci){
  #if (Ci==0){ylable="Optcost_withOutIdle"}else{ylable="Optcost_withIdle"}
  #ylable = expression(Mean(W) ("in minutes"))
  ylable <- expression(Mean(W) * " (" * italic("in minutes") * ")")
  mydata = result
  
  column_mapping <- list("K2CRG_In_meanW" = "K2+CRG_In","K2CRG_Out_meanW" = "K2+CRG_OutC","K2CRG_NaN_meanW" = "K2+CRG_NA","K2CRG_OutP_meanW" = "K2+CRG_OutP",
                         "K3CRG_In_meanW" = "K3+CRG_In","K3CRG_Out_meanW" = "K3+CRG_OutC","K3CRG_NaN_meanW" = "K3+CRG_NA","K3CRG_OutP_meanW" = "K3+CRG_OutP")
  
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
    theme(legend.position = c(1.0, 0.95), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 10),
          #legend.title = element_text(size = 8),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(1,11),breaks = seq(1, 11, 1))+
    labs(title = bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]),y=ylable)+ #or opt_cost_withOutIdle
    theme(plot.title = element_text(size = 15))+
    guides(color = guide_legend(nrow = 8, ncol = 1)) 
  
  return(p1)
  
}


Plot_AddressK2K3_Wait_std_Slot<-function(result,Ci){
  #if (Ci==0){ylable="Optcost_withOutIdle"}else{ylable="Optcost_withIdle"}
  #ylable = expression(Mean(W) ("in minutes"))
  ylable <- expression(STD(W) * " (" * italic("in minutes") * ")")
  mydata = result
  
  column_mapping <- list("K2CRG_In_stdW" = "K2+CRG_In","K2CRG_Out_stdW" = "K2+CRG_OutC","K2CRG_NaN_stdW" = "K2+CRG_NA","K2CRG_OutP_stdW" = "K2+CRG_OutP",
                         "K3CRG_In_stdW" = "K3+CRG_In","K3CRG_Out_stdW" = "K3+CRG_OutC","K3CRG_NaN_stdW" = "K3+CRG_NA","K3CRG_OutP_stdW" = "K3+CRG_OutP")
  
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
    theme(legend.position = c(1.00, 0.38), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 10),
          #legend.title = element_text(size = 8),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(1,3),breaks = seq(1, 3, 0.5))+
    labs(title = bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]),y=ylable)+ #or opt_cost_withOutIdle
    theme(plot.title = element_text(size = 15))+
    guides(color = guide_legend(nrow = 4, ncol = 1)) 
  
  return(p1)
  
}




Plot_AddressK2K3_Wait_std_Slot_ALL<-function(result,Ci){
  #if (Ci==0){ylable="Optcost_withOutIdle"}else{ylable="Optcost_withIdle"}
  #ylable = expression(Mean(W) ("in minutes"))
  ylable <- expression(STD(W) * " (" * italic("in minutes") * ")")
  mydata = result
  
  column_mapping <- list("K2CRG_In_stdW" = "K2+CRG_In","K2CRG_Out_stdW" = "K2+CRG_OutC","K2CRG_NaN_stdW" = "K2+CRG_NA","K2CRG_OutP_stdW" = "K2+CRG_OutP",
                         "K3CRG_In_stdW" = "K3+CRG_In","K3CRG_Out_stdW" = "K3+CRG_OutC","K3CRG_NaN_stdW" = "K3+CRG_NA","K3CRG_OutP_stdW" = "K3+CRG_OutP")
  
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
    theme(legend.position = c(1.00, 0.42), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 10),
          #legend.title = element_text(size = 8),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA))+ # Align the legend relative to the upper right corner
    
    geom_point(size=2.5)+
    #coord_fixed(ratio = 5)+ #Set the ratio y:x = 0.3
    geom_line()+
    
    scale_x_continuous(limits = c(1,10),breaks = seq(1, 10, 1))+
    scale_y_continuous(limits = c(1,11),breaks = seq(1, 11, 1))+
    labs(title = bquote(paste(c[I], "=", .(Ci))), ##Add X,Y, and main headings
         x=expression(c[O]),y=ylable)+ #or opt_cost_withOutIdle
    theme(plot.title = element_text(size = 15))+
    guides(color = guide_legend(nrow = 4, ncol = 1)) 
  
  return(p1)
  
}


Moment_Rule <- function(RuleList1, RuleList2 = NULL) {
  Scenario <- list(Type = NULL, X_Vec = NULL)
  
  # If the second argument is empty, perform A
  if (is.null(RuleList2) || length(RuleList2) == 0) {
    RuleList1_r <- rev(RuleList1)
    Scenario$Type <- list(RuleList1, RuleList1_r)
    Scenario$X_Vec <- rep(list(rep(15 * (60 / sec), n-1)),length(Scenario$Type))
  } else {  # Otherwise run B
    RuleList1_r <- rev(RuleList1)
    RuleList2_r <- rev(RuleList2)
    Scenario$Type <- list(RuleList1, RuleList1_r, RuleList2, RuleList2_r)
    Scenario$X_Vec <- rep(list(rep(15 * (60 / sec), n-1)), length(Scenario$Type))
  }
  return(Scenario)
}

Plot_AddressK2_Wait_std_mean_Slot_ALL <- function(result, result2, Ci) {
  ylable <- expression(Mean(W) * " (" * italic("in minutes") * ")")
  mydata = result
  mydata2 = result2
  
  column_mapping <- list("K2CRG_In_meanW" = "K2+CRG_In", "K2CRG_Out_meanW" = "K2+CRG_OutC", "K2CRG_NaN_meanW" = "K2+CRG_NA", "K2CRG_OutP_meanW" = "K2+CRG_OutP",
                         "K2CRG_In_stdW" = "K2+CRG_In", "K2CRG_Out_stdW" = "K2+CRG_OutC", "K2CRG_NaN_stdW" = "K2+CRG_NA", "K2CRG_OutP_stdW" = "K2+CRG_OutP")
  
  names(mydata) <- lapply(names(mydata), function(x) ifelse(x %in% names(column_mapping), column_mapping[[x]], x))
  names(mydata2) <- lapply(names(mydata2), function(x) ifelse(x %in% names(column_mapping), column_mapping[[x]], x))
  
  mydata <- melt(mydata, id = "Co")
  colnames(mydata) <- c("Co_value", "Policy", "Opt_cost")
  
  mydata2 <- melt(mydata2, id = "Co")
  colnames(mydata2) <- c("Co_value", "Policy", "Opt_cost")
  
  p1 <- ggplot() +
    theme_bw() +
    theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed")) +
    theme(axis.text.x = element_text(size = 15, angle = 0, hjust = 0.5, vjust = 0.5), axis.text.y = element_text(size = 15, hjust = 0.5, vjust = 0.5),
          axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
    theme(panel.grid = element_blank()) +
    theme(legend.position = c(1.0, 0.95), # Set the legend in the upper right corner
          legend.justification = c(1, 1),
          legend.text = element_text(size = 10),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA)) + # Align the legend relative to the upper right corner
    coord_fixed(ratio = 5) + # Set the ratio y:x = 0.3
    geom_point(data = mydata, aes(x = Co_value, y = Opt_cost, group = Policy, color = Policy, shape = Policy), size = 2.5) +
    geom_line(data = mydata, aes(x = Co_value, y = Opt_cost, group = Policy, color = Policy)) +
    scale_x_continuous(limits = c(1, 10), breaks = seq(1, 10, 1)) +
    scale_y_continuous(limits = c(1, 11), breaks = seq(1, 11, 1)) +
    labs(title = bquote(paste(c[I], "=", .(Ci))), # Add X,Y, and main headings
         x = expression(c[O]), y = ylable) + # or opt_cost_withOutIdle
    theme(plot.title = element_text(size = 15)) +
    guides(color = guide_legend(nrow = 8, ncol = 1)) +
    geom_point(data = mydata2, aes(x = Co_value, y = Opt_cost, group = Policy, color = Policy, shape = Policy), size = 2.5) +
    geom_line(data = mydata2, aes(x = Co_value, y = Opt_cost, group = Policy, color = Policy)) +
    scale_y_continuous(sec.axis = sec_axis(~., name = "Standard Deviation"))
  
  return(p1)
}



CRG_SimKPIH <- function(CommonServTime,Scenario_list, X_Vec,
                                   plan_completion = 4*60L*(60/sec), # 4-hour session
                                   excess_wait_threshhold = 15L*(60/sec), # 15 min for excess wait
                                   extreme_wait_threshhold = 45L*(60/sec), # 45 min for extreme wait
                                   excess_OT_threshhold = 30L*(60/sec), # 30 min for excess OT
                                   extreme_OT_threshhold = 45L*(60/sec) #45 min for extreme OT
){
  # Input: 
  #     CommonServTime List
  # Output: Dataframe of simulation of full sequencing, which as the following fields
  #         mean.wait, sd.wait=numeric(n_seq), these are mean and sd of waiting
  #         mean.T, sd.T, these are mean and sd of server ending time T
  #         mean.OT, sd.OT, these are mean and sd of server over time
  #         mean.Idle, sd.Idle, these are mean and sd of server Idle
  #         excess.wait.rate, extreme.wait.rate, these are mean excess and extreme wait
  #         excess.OT.rate, extreme.OT.rate, these are mean excess and extreme OT

  n_seq = length(Scenario_list$Type)
  
  df <- data.frame(PolicyInd=numeric(n_seq),
                   Rule_name=character(n_seq),
                   #Cost=numeric(n_seq),
                   mean.wait=numeric(n_seq), sd.wait=numeric(n_seq), 
                   #mean.T = numeric(n_seq), sd.T=numeric(n_seq), 
                   mean.OT=numeric(n_seq), sd.OT=numeric(n_seq),
                   mean.Idle=numeric(n_seq), sd.Idle=numeric(n_seq),
                   excess.wait.rate=numeric(n_seq), extreme.wait.rate=numeric(n_seq),
                   excess.OT.rate=numeric(n_seq), extreme.OT.rate=numeric(n_seq),  
                   stringsAsFactors = F)

  #Simulate
  for(ii in 1:n_seq){
    PolicyInd = ii
    Rule_name = paste(Scenario_list$Type[[ii]],collapse="")
    schd= list(Type=Scenario_list$Type[[ii]],X_Vec= X_Vec)
    rKPI <- schd2SimKPIH(CommonServTime, schd)
    KPI <- rKPI2KPIH(schd,rKPI)
    df[ii,] = data.frame(PolicyInd,
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

# weighted_K2_sd <- function(D1, D2, p1, p2) {
#   # 计算数据集的大小
#   n1 <- length(D1)
#   n2 <- length(D2)
#   
#   # 计算数据集的均值
#   mean_D1 <- mean(D1)
#   mean_D2 <- mean(D2)
#   
#   # 计算数据集的标准差
#   std_D1 <- sd(D1)
#   std_D2 <- sd(D2)
#   
#   # 计算加权标准差
#   weighted_std <- sqrt((p1 * (n1 * std_D1^2 + n1 * mean_D1^2) + p2 * (n2 * std_D2^2 + n2 * mean_D2^2)) / (p1 * n1 + p2 * n2) - ((p1 * n1 * mean_D1 + p2 * n2 * mean_D2) / (p1 * n1 + p2 * n2))^2)
#   
#   return(weighted_std)
# }
# 
# weighted_K3_sd <- function(D1, D2, D3, p1, p2, p3) {
#   # 计算数据集的大小
#   n1 <- length(D1)
#   n2 <- length(D2)
#   n3 <- length(D3)
#   
#   # 计算数据集的均值
#   mean_D1 <- mean(D1)
#   mean_D2 <- mean(D2)
#   mean_D3 <- mean(D3)
#   
#   # 计算数据集的标准差
#   std_D1 <- sd(D1)
#   std_D2 <- sd(D2)
#   std_D3 <- sd(D3)
#   
#   # 计算加权标准差
#   weighted_std <- sqrt((p1 * (n1 * std_D1^2 + n1 * mean_D1^2) + p2 * (n2 * std_D2^2 + n2 * mean_D2^2) + p3 * (n3 * std_D3^2 + n3 * mean_D3^2)) / (p1 * n1 + p2 * n2 + p3 * n3) - ((p1 * n1 * mean_D1 + p2 * n2 * mean_D2 + p3 * n3 * mean_D3) / (p1 * n1 + p2 * n2 + p3 * n3))^2)
#   
#   return(weighted_std)
# }


# # # 例如：
# D1 <- c(1, 2, 3, 4, 5)
# D2 <- c(6, 7, 8, 9, 10)
# D3 <- c(11, 12, 13, 14, 15)
# p1 <- 0.4
# p2 <- 0.3
# p3 <- 0.3
# weighted_std <- weighted_sd(D1, D2, D3, p1, p2, p3)
# print(paste("三组数据集各自的标准差的加权和为：", weighted_std))


calculate_seq <- function(...) {
  args <- list(...)
  n <- sum(unlist(args))
  if (length(args) == 2) {
    n_seq <- choose(n, args[[1]])
  } else if (length(args) == 3) {
    n_seq <- choose(n, args[[1]]) * choose(n - args[[1]], args[[2]])
  } else {
    stop("Invalid number of arguments. Please provide 2 or 3 arguments.")
  }
  return(n_seq)
}

weighted_K2_sd <- function(D1, D2, p1, p2) {
  # Ref: https://www.statology.org/weighted-standard-deviation-excel/
  # to calculate a weighted standard deviation
  # 计算数据集的大小
  n1 <- length(D1)
  n2 <- length(D2)
  
  Weights_vec1 <- rep(1/n1, n1)*p1 
  Weights_vec2 <- rep(1/n2, n2)*p2
  Weights_vec <- c(Weights_vec1, Weights_vec2)
  
  Values_vec <- c(D1, D2)
  
  x_bar <- sum(Values_vec * Weights_vec)/sum(Weights_vec)
  M <- sum(Weights_vec != 0)
  
  weighted_std <- sqrt(sum((Values_vec-x_bar)^2*Weights_vec)/(((M-1)/M)*sum(Weights_vec)))
  
  return(weighted_std)
}

weighted_K3_sd <- function(D1, D2, D3, p1, p2, p3) {
  # 计算数据集的大小
  n1 <- length(D1)
  n2 <- length(D2)
  n3 <- length(D3)
  
  Weights_vec1 <- rep(1/n1, n1)*p1 
  Weights_vec2 <- rep(1/n2, n2)*p2
  Weights_vec3 <- rep(1/n3, n3)*p3
  
  Weights_vec <- c(Weights_vec1, Weights_vec2, Weights_vec3)
  
  Values_vec <- c(D1, D2, D3)
  
  x_bar <- sum(Values_vec * Weights_vec)/sum(Weights_vec)
  M <- sum(Weights_vec != 0)
  
  weighted_std <- sqrt(sum((Values_vec-x_bar)^2*Weights_vec)/(((M-1)/M)*sum(Weights_vec)))
  
  return(weighted_std)
}
