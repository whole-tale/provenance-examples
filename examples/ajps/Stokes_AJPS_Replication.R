  ### STOKES 2015, REPLICATION CODE ####
  ### ELECTORAL BACKLASH AGAINST CLIMATE POLICY
  # The following analyses were carried out using R version 3.0.2 (2013-09-25).
  # For further information requests, contact: stokes@polsci.ucsb.edu
  
  #### SET UP + LOAD PACKAGES ####
#  setwd("~/Downloads")
#  getwd()
  
  # To install packages, use the "install.packages" command. Example: "install.packages('reshape2')"
  library(foreign)
  library(lmtest)
  library(sandwich)
  library(Matching)
  library(ebal)
  library(memisc)
  library(ggplot2)
  library(sp)
  library(maptools) 
  library(rgdal) 
  library(rgeos)
  library(AER)
  library(plyr)
  library(scales) 
  library(RColorBrewer)
  
  rm(list=ls()) #to remove all data from workspace
  
  options(digits=10)
  options(scipen=10)
  set.seed(02139)
  
  ##### LOAD DATA ######
  divisions <- read.csv("replication_file.csv", stringsAsFactors=FALSE, na.strings=c("NA", ""),  strip.white = TRUE)
  divisions$poll_id_07 <- as.factor(divisions$poll_id_07) #several variables need to be factors to ensure the functions run properly
  divisions$master_id <- as.factor(divisions$master_id)
  divisions$poll_id_03 <- as.factor(divisions$poll_id_03)
  divisions$fed_id <- as.factor(divisions$fed_id)
  str(divisions,  list.len=100) # check what class all the variables are in
  summary(divisions) # look at data summary
  
  
  #### FUNCTIONS ####
  
  # DEMEANER FUNCTION 
  # Used with panel data to demean the data for each unit before estimating a linear regression with time period fixed effects included. Computationally equivalent to running a model with saturated fixed effects (i.e. with a coefficient for every unit and time period except the base categories), but much faster to compute.
  
  # Takes as inputs: 
  # yx = outcome variable and treatment variable as a dataset (e.g. vote share change and treatment status)
  # T = time factor used in fixed effects model, as factor (e.g. year)
  # group = unit factor used in fixed effects model, as factor (e.g. precinct)
  # w = weights for model (optional)
  
  # Output can be put into a linear regression with time periods included as covariates (minus base year category). 
  # Time periods should be preceeded with a "Y" when used in the linear regression to make them character strings, for example 2003 should be entered in as Y2003.
  
  # Example:
  # yx_diffed <- demeaner(yx=yx_data, T=as.factor(data$year), group=data$unit_id)
  # mod_1 <- lm(y ~ x + Y2000 + Y2002, data=yx_diffed) 
  # Years in this example are 2000 and 2002 and another category which is dropped, becoming the base year.
  
  demeaner <- function(yx, T, group, w=NULL){
    conds <- length(levels(T))
    n <- length(T)
    Td = as.data.frame(model.matrix(~T-1,model.frame(~T-1),contrasts=FALSE)[1:n,1:conds])
    colnames(Td)<- paste("Y", levels(T)[1:conds], sep="") #puts a Y in front of the year/time variable so they are character strings
    
    yx <- cbind(yx, Td)
    
    yx2 <- matrix(NA, nrow=nrow(yx), ncol=ncol(yx)) # this is a big empty matrix we will fill
    group <- droplevels(group)
    
    for (c in levels(group)){
      yx.c <- as.matrix(yx[group==c,],ncol=ncol(yx))
      if (is.null(w)){
        yx2[group==c,] <- yx.c - matrix(rep(colMeans(yx.c),times=nrow(yx.c)),ncol=ncol(yx.c),byrow=TRUE)
      } else {
        yx2[group==c,] <- yx.c - matrix(rep(apply(yx.c,2,weighted.mean,w=weights),times=nrow(yx.c)),ncol=ncol(yx.c),byrow=TRUE)
      }
    }
    
    colnames(yx2) <- colnames(yx)
    
    rm(yx)
    yx2=as.data.frame(yx2)
    return(yx2)
  }
  
  
  # VCOV CLUSTER FUNCTION WITH DEMEAN, vcovCluster_demean 
  # degrees of freedom modification (first), given panel unit specific fixed effects coefficients created through demeaner function
  # function to compute var-cov matrix using clustered robust standard errors
  # inputs:
  # model = model object from call to lm or glm
  # cluster = vector with cluster ID indicators
  # first = degrees of freedom modification because some df already used up in demean function (number of units in panel)
  # output:
  # cluster robust var-cov matrix
  # to call this for a model directly use:
  # coeftest(model,vcov = vcovCluster(model, cluster))
  # formula is similar to Stata's cluster command
  
  vcovCluster_demean <- function(model, cluster, first)
  {
    require(sandwich)
    require(lmtest)
    cluster <- droplevels(cluster)
    if(nrow(model.matrix(model))!=length(cluster)){
      stop("check your data: cluster variable has different N than model")
    }
    M <- length(unique(cluster))
    N <- length(cluster)           
    K <- model$rank   
    if(M<50){
      warning("Fewer than 50 clusters, variances may be unreliable (could try block bootstrap instead).")
    }
    dfc <- (M/(M - 1)) * ((N - 1)/(N - K - first)) #NOTE: need to change this 6186 if the clusters is different
    uj  <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
    rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
    return(rcse.cov)
  }
  
  # VCOV CLUSTER FUCTION
  # Generic vcovCluster function# inputs:
  # model = model object from call to lm or glm
  # cluster = vector with cluster ID indicators
  # output:
  # cluster robust var-cov matrix
  # to call this for a model directly use:
  # coeftest(model,vcov = vcovCluster(model, cluster))
  # formula is similar to Stata's cluster command
  vcovCluster <- function(model, cluster)
  {
    require(sandwich)
    require(lmtest)
    if(nrow(model.matrix(model))!=length(cluster)){
      stop("check your data: cluster variable has different N than model")
    }
    M <- length(unique(cluster))
    N <- length(cluster)           
    K <- model$rank   
    if(M<50){
      warning("Fewer than 50 clusters, variances may be unreliable (could try block bootstrap instead).")
    }
    dfc <- (M/(M - 1)) * ((N - 1)/(N - K )) 
    uj  <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
    rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
    return(rcse.cov)
  }
  
  # SUMMARY FUNCTION 
  ## Summarizes data.
  # Function used to compute basic statistics before creating some figures.
  ## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
  ##   data: a data frame.
  ##   measurevar: the name of a column that contains the variable to be summariezed
  ##   groupvars: a vector containing names of columns that contain grouping variables
  ##   na.rm: a boolean that indicates whether to ignore NA's
  ##   conf.interval: the percent range of the confidence interval (default is 95%)
  
  summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
    require(plyr)
    
    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
      if (na.rm) sum(!is.na(x))
      else       length(x)
    }
    
    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
                   .fun = function(xx, col) {
                     c(N    = length2(xx[[col]], na.rm=na.rm),
                       mean = mean   (xx[[col]], na.rm=na.rm),
                       sd   = sd     (xx[[col]], na.rm=na.rm)
                     )
                   },
                   measurevar
    )
    
    # Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))
    
    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
    
    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult
    
    return(datac)
  }
  
  
  
  #### MISSING DATA ####
  
  # Some census data is missing. This section creates replaces missing data given the methods reported in the paper. It is necessary to run this section before running many of the models below.
  dt <- divisions
  
  # Shows the amount 
  length(which(is.na(dt$log_pop))) / nrow(dt) #1%
  length(which(is.na(dt$log_pop_denc))) / nrow(dt) #1%
  length(which(is.na(dt$log_median_inc))) / nrow(dt) #4%
  length(which(is.na(dt$avg_home_val_log))) / nrow(dt) #3%
  length(which(is.na(dt$unemploy_rate))) / nrow(dt) #3%
  length(which(is.na(dt$p_uni_degree))) / nrow(dt) #2%
  length(which(is.na(dt$p_immigrant))) / nrow(dt) #2%
  length(which(is.na(dt$p_housing_own))) / nrow(dt) #2%
  
  #subset the data by year
  d2007 <- dt[which(dt$year==2007),]
  d2011 <- dt[which(dt$year==2011),]
  d2003 <- dt[which(dt$year==2003),]
  
  # Eliminate missing data by setting missing values equal to the mean for each year and variable
  d2007$log_pop_denc[which(is.na(d2007$log_pop_denc))] <- mean(d2007$log_pop_denc, na.rm=T)
  d2007$log_pop[which(is.na(d2007$log_pop))] <- mean(d2007$log_pop, na.rm=T)
  d2007$log_median_inc[which(is.na(d2007$log_median_inc))] <- mean(d2007$log_median_inc, na.rm=T)
  d2007$avg_home_val_log[which(is.na(d2007$avg_home_val_log))] <- mean(d2007$avg_home_val_log, na.rm=T)
  d2007$avg_home_val[which(is.na(d2007$avg_home_val))] <- mean(d2007$avg_home_val, na.rm=T)
  d2007$unemploy_rate[which(is.na(d2007$unemploy_rate))] <- mean(d2007$unemploy_rate, na.rm=T)
  d2007$p_uni_degree[which(is.na(d2007$p_uni_degree))] <- mean(d2007$p_uni_degree, na.rm=T)
  d2007$p_immigrant[which(is.na(d2007$p_immigrant))] <- mean(d2007$p_immigrant, na.rm=T)
  d2007$p_housing_own[which(is.na(d2007$p_housing_own))] <- mean(d2007$p_housing_own, na.rm=T)
  
  d2011$log_pop_denc[which(is.na(d2011$log_pop_denc))] <- mean(d2011$log_pop_denc, na.rm=T)
  d2011$log_pop[which(is.na(d2011$log_pop))] <- mean(d2011$log_pop, na.rm=T)
  d2011$log_median_inc[which(is.na(d2011$log_median_inc))] <- mean(d2011$log_median_inc, na.rm=T)
  d2011$avg_home_val_log[which(is.na(d2011$avg_home_val_log))] <- mean(d2011$avg_home_val_log, na.rm=T)
  d2011$avg_home_val[which(is.na(d2011$avg_home_val))] <- mean(d2011$avg_home_val, na.rm=T)
  d2011$unemploy_rate[which(is.na(d2011$unemploy_rate))] <- mean(d2011$unemploy_rate, na.rm=T)
  d2011$p_uni_degree[which(is.na(d2011$p_uni_degree))] <- mean(d2011$p_uni_degree, na.rm=T)
  d2011$p_immigrant[which(is.na(d2011$p_immigrant))] <- mean(d2011$p_immigrant, na.rm=T)
  d2011$p_housing_own[which(is.na(d2011$p_housing_own))] <- mean(d2011$p_housing_own, na.rm=T)
  
  d2003$log_pop_denc[which(is.na(d2003$log_pop_denc))] <- mean(d2003$log_pop_denc, na.rm=T)
  d2003$log_pop[which(is.na(d2003$log_pop))] <- mean(d2003$log_pop, na.rm=T)
  d2003$log_median_inc[which(is.na(d2003$log_median_inc))] <- mean(d2003$log_median_inc, na.rm=T)
  d2003$avg_home_val_log[which(is.na(d2003$avg_home_val_log))] <- mean(d2003$avg_home_val_log, na.rm=T)
  d2003$avg_home_val[which(is.na(d2003$avg_home_val))] <- mean(d2003$avg_home_val, na.rm=T)
  d2003$unemploy_rate[which(is.na(d2003$unemploy_rate))] <- mean(d2003$unemploy_rate, na.rm=T)
  d2003$p_uni_degree[which(is.na(d2003$p_uni_degree))] <- mean(d2003$p_uni_degree, na.rm=T)
  d2003$p_immigrant[which(is.na(d2003$p_immigrant))] <- mean(d2003$p_immigrant, na.rm=T)
  d2003$p_housing_own[which(is.na(d2003$p_housing_own))] <- mean(d2003$p_housing_own, na.rm=T)
  
  # put the data back together
  dt <- rbind(d2003,d2007,d2011)
  
  #make square terms for each variable - used in entropy balancing
  dt$log_pop_sq <- (dt$log_pop)^2
  dt$log_pop_denc_sq <- (dt$log_pop_denc)^2
  dt$log_median_inc_sq <- (dt$log_median_inc)^2
  dt$avg_home_val_log_sq <- (dt$avg_home_val_log)^2
  dt$unemploy_rate_sq <- (dt$unemploy_rate)^2
  dt$p_uni_degree_sq <- (dt$p_uni_degree)^2
  dt$p_immigrant_sq <- (dt$p_immigrant)^2
  dt$p_housing_own_sq <- (dt$p_housing_own)^2
  
  #subset
  d2007 <- dt[which(dt$year==2007),]
  d2011 <- dt[which(dt$year==2011),]
  d2003 <- dt[which(dt$year==2003),]
  
  # overwrite the initial data file with data without missing values
  divisions <- dt
  rm(dt)
  
  #### DATA CONSTRUCTION ####
  
  # Create a rural subset from the data.
  div_rural <- divisions[which(divisions$pop_denc <= 400 & divisions$year==2007),] # Cut out divisions with population density greater than 400 people per km^2 in 2007. N=3169.
  
  # Need to find the matching 2003 and 2011 divisions to complete the panel.
  a <- rbind(div_rural, d2011)
  dup_id <- which(duplicated(a$master_id)==T)
  dup_id_2 <- which(duplicated(a$master_id, fromLast = TRUE)==T)
  keep <- c(dup_id,dup_id_2)
  a <- a[keep,]
  length(which(a$year==2007)) == length(which(a$year==2011)) # This is a check - you want this to say TRUE.
  b <- rbind(d2003, div_rural)
  dup_id <- which(duplicated(b$master_id)==T)
  dup_id_2 <- which(duplicated(b$master_id, fromLast = TRUE)==T)
  keep <- c(dup_id,dup_id_2)
  b <- b[keep,]
  length(which(b$year==2007)) == length(which(b$year==2003)) # This is a check - you want this to say TRUE.
  div_rural <- rbind(a, b[which(b$year==2003),])
  rm(a, b, dup_id, dup_id_2, keep)
  
  div_rural$master_id <- droplevels(div_rural$master_id) #Important before running analysis because master_id is a factor.
  
  #Check to see all treated units are still in this rural dataset:
  propsr <- divisions[which(divisions$prop==1),] 
  opsr <- divisions[which(divisions$op==1),]
  length(unique(propsr$master_id)) #184
  length(unique(opsr$master_id)) #52
  rm(propsr, opsr)
  
  # Create weights for balanced models with entropy balancing:
  # Proposed variable
  N <- 6186
  #Will use X and then X^2 to balance for each variable
  X_bal <- d2003[,c("log_pop_denc", "log_pop", "log_median_inc", "avg_home_val_log", "unemploy_rate", "p_uni_degree", "p_immigrant", "p_housing_own", "log_pop_denc_sq", "log_pop_sq", "log_median_inc_sq", "avg_home_val_log_sq", "unemploy_rate_sq", "p_uni_degree_sq", "p_immigrant_sq", "p_housing_own_sq")] #variables to balance on, per the paper.
  ebalout <- ebalance(Treatment=d2003$treat_p, X=X_bal)
  
  #That only gives weights for controls; I'd like an N-dim vector with all weights:
  ebalw <- replicate(N, 1) #for every treated unit, give the weight of 1.
  ebalw[d2003$treat_p==0]=ebalout$w #for every control unit, give the weight from the model.
  
  #Add the proposed variable weights to the dataset
  d2007$weights_p <- ebalw
  d2003$weights_p <- ebalw
  d2011$weights_p <- ebalw
  
  divisions <- rbind(d2007, d2003, d2011)
  rm(ebalout, ebalw, N, X_bal)
  
  # Operational variable
  N <- 6186
  X_bal <- d2003[,c("log_pop_denc", "log_pop", "log_median_inc", "avg_home_val_log" ,"unemploy_rate" , "p_uni_degree", "p_immigrant", "p_housing_own", "log_pop_denc_sq", "log_pop_sq", "log_median_inc_sq", "avg_home_val_log_sq" ,"unemploy_rate_sq" , "p_uni_degree_sq", "p_immigrant_sq", "p_housing_own_sq")] #variables to balance on, per the paper.
  ebalout <- ebalance(Treatment=d2003$treat_o, X=X_bal)
  
  #That only gives weights for controls; I'd like an N-dim vector with all weights:
  ebalw <- replicate(N, 1) #for every treated unit, give the weight of 1.
  ebalw[d2003$treat_o==0]=ebalout$w #for every control unit, give the weight from the model.
  
  #weights are ebalw... 
  d2007$weights_o <- ebalw
  d2003$weights_o <- ebalw
  d2011$weights_o <- ebalw
  
  divisions <- rbind(d2007, d2003, d2011)
  rm(ebalout, ebalw, N, X_bal)
  
  
  # INSTRUMENT
  # Create the instrument cross section and matched pairs for Table 2.
  # Per the paper, the IV panel has NO units that are treated in 2007 (within 3km), NO units where wind speed data is missing (equal to zero), and uses change in liberal vote share rather than absolute value of DV.
  
  # First, subset the data to 2011 treated units that will be matched with controls.
  d2011_d <- d2011[which(d2011$avg_pwr > 0),] # Only keeps units with windspeed data greater than 0.
  d2007_d <- d2007[which(d2007$prop_3km != 1),] # Removes units already treated in 2007 (within 3 km of turbine).
  ivpanel <- rbind(d2007_d, d2011_d) # IV panel does not include 2003 data. It only looks at changes between 2007 and 2011. However, 2003 data is used for robustness checks in SI.
  
  # Make the panel equally balanced by cutting out units that were removed in one year but not the others.
  a <- ivpanel 
  dup_id <- which(duplicated(a$master_id)==T)
  dup_id_2 <- which(duplicated(a$master_id, fromLast = TRUE)==T)
  keep <- c(dup_id,dup_id_2)
  a <- a[keep,]
  length(which(a$year==2007)) == length(which(a$year==2011)) # you want this to say TRUE.
  rm(dup_id, dup_id_2, keep)
  b <- a[which(a$year==2007),]
  b <- rbind(b, d2003)
  dup_id <- which(duplicated(b$master_id)==T)
  dup_id_2 <- which(duplicated(b$master_id, fromLast = TRUE)==T)
  keep <- c(dup_id,dup_id_2)
  b <- b[keep,]
  length(which(b$year==2007)) == length(which(b$year==2003)) # you want this to say TRUE.
  a <- a[which(a$year==2011),]
  ivpanel <- rbind(a, b)
  rm(a, b, keep, dup_id, dup_id_2, d2011_d, d2007_d, d2003, d2007, d2011)
  
  ivd <- ivpanel[which(ivpanel$year==2011),] 
  iv07 <- ivpanel[which(ivpanel$year==2007),]

  sum(ivd$prop_3km) #354 - 2011 treated units, within 3 km of a proposed turbine - we will find matched control units for these.
  
  ivd$chng_lib <- ivd$perc_lib-iv07$perc_lib # new DV for instrumental variable cross section -- see Table 2.

  # Create pre-treatment average home value data in IV data
  ivd$avg_home_val_log_07 <- log(iv07$avg_home_val)
  ivd$avg_home_val_log_07[which(is.na(ivd$avg_home_val_log_07))] <- mean(ivd$avg_home_val_log_07, na.rm=T)
  
  # Create other variables necessary for IV model
  ivd$avg_pwr_log <- log(ivd$avg_pwr)
  ivd$mindistlake_log <- log(1+ivd$mindistlake)
  ivd$mindistlake_sq <- ivd$mindistlake * ivd$mindistlake
  ivd$long_sq <- ivd$longitude * ivd$longitude
  ivd$lat_sq <- ivd$latitude * ivd$latitude
  ivd$long_lat <- ivd$latitude * ivd$longitude
  
  # Find 354 matched pairs for instrumental variable model. Match based of four variables reported in paper. Use 2011 'ivd' dataset created above.
  match.covars <- ivd[,c("avg_home_val_log_07", "p_uni_degree", "log_median_inc", "log_pop_denc")]
  sum(ivd$prop_3km==1) #354 - number of treated units
  
  match <- Match(Y=ivd$chng_lib, Tr=ivd$prop_3km, X=match.covars, estimand="ATT", M=1, exact=c(F,F,F,F), replace=FALSE) # Finding 1 controls for each unit and not allowing reuse. Matches do not have to be exact.
  
  matchobj_tr <- ivd[match$index.treated,] # these are the treated units
  matchobj_ct <- ivd[match$index.control,] # these are the control units
  matchobj <- rbind(matchobj_tr, matchobj_ct) # matched dataset, for instrument analysis
  
  rm(match.covars, ivd, iv07, ivpanel, match)
  
  
  #### TABLES ####
  
  # Using the replication dataset, with missing values replaced (see previous section), the rural subset and weights from entropy balancing, this section of the code will allow you to replicate the tables in the article. You need to also run the functions above first.
  
  ### TABLE 1 ###
  
  # Column 1 (All Precincts), Section 1 (Turbine Proposal)
  # Using panel demeaning function 
  yx_data <- divisions[,c("perc_lib","prop")]
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(divisions$year), group=divisions$master_id)
  mod_1 <- lm(perc_lib ~ prop + Y2003 + Y2007, data=yx_diffed)
  round(coeftest(mod_1, vcov=vcovCluster_demean(mod_1, cluster=divisions$master_id, first=nrow(divisions)/3)),3) 
  
  # Alternatively, using canned R function -- note, this takes a long time to run (~20 minutes). A similar approach can be used with the other models below.
  #mod_1 <- lm(perc_lib ~ prop + factor(master_id) + factor(year), data=divisions)
  #round(coeftest(mod_1, vcov=vcovCluster(mod_1, cluster=divisions$master_id))[1:5,], 3)
  
  # Column 1 (All Precincts), Section 2 (Turbine Operational)
  # Using panel demeaning function 
  yx_data <- divisions[,c("perc_lib","op")]
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(divisions$year), group=divisions$master_id)
  mod_2 <- lm(perc_lib ~ op + Y2003 + Y2007, data=yx_diffed)
  round(coeftest(mod_2, vcov=vcovCluster_demean(mod_2, cluster=divisions$master_id, first=nrow(divisions)/3)),3) 
  
  # Alternatively, using canned R function -- note, this takes a long time to run (~20 minutes). A similar approach can be used with the other models below.
  #mod_2 <- lm(perc_lib ~ op + factor(master_id) + factor(year), data=divisions)
  #round(coeftest(mod_2, vcov=vcovCluster(mod_2, cluster=divisions$master_id))[1:5,], 3)
  
  
  # Column 2 (All Precincts with Controls), Section 1 (Turbine Proposal)
  divisions <- droplevels(divisions)
  yx_data <- divisions[,c("perc_lib", "prop", "p_uni_degree", "log_pop_denc", "unemploy_rate", "log_median_inc", "p_immigrant")] 
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(divisions$year), group=divisions$master_id) # group needs to be a factor as well
  mod_3 <- lm(perc_lib ~ prop + Y2003 + Y2007 + p_uni_degree + log_pop_denc + unemploy_rate + log_median_inc + p_immigrant, data=yx_diffed)
  round(coeftest(mod_3, vcov=vcovCluster_demean(mod_3, cluster=divisions$master_id, first=nrow(divisions)/3)),3)
  
  # Column 2 (All Precincts with Controls), Section 2 (Turbine Operational)
  divisions <- droplevels(divisions)
  yx_data <- divisions[,c("perc_lib", "op", "p_uni_degree", "log_pop_denc", "unemploy_rate", "log_median_inc", "p_immigrant")] 
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(divisions$year), group=divisions$master_id) # group needs to be a factor as well
  mod_4 <- lm(perc_lib ~ op + Y2003 + Y2007 + p_uni_degree + log_pop_denc + unemploy_rate + log_median_inc + p_immigrant, data=yx_diffed)
  round(coeftest(mod_4, vcov=vcovCluster_demean(mod_4, cluster=divisions$master_id, first=nrow(divisions)/3)),3)
  
  
  # Column 3 (Rural Precincts), Section 1 (Turbine Proposal)
  div_rural <- droplevels(div_rural) #Note: you need to have created the rural subset in Data Construction section above.
  yx_data <- div_rural[,c("perc_lib","prop")]
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(div_rural$year), group=div_rural$master_id)
  mod_5 <- lm(perc_lib ~ prop + Y2003 + Y2007, data=yx_diffed)
  round(coeftest(mod_5, vcov=vcovCluster_demean(mod_5, cluster=div_rural$master_id, first=nrow(div_rural)/3)),3) 
  
  # Column 3 (Rural Precincts), Section 2 (Turbine Operational)
  div_rural <- droplevels(div_rural)
  yx_data <- div_rural[,c("perc_lib","op")]
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(div_rural$year), group=div_rural$master_id) # change dataset
  mod_6 <- lm(perc_lib ~ op + Y2003 + Y2007, data=yx_diffed) #change treatment variable
  round(coeftest(mod_6, vcov=vcovCluster_demean(mod_6, cluster=div_rural$master_id, first=nrow(div_rural)/3)),3) 
  
  
  # Column 4 (Rural Precincts with Controls), Section 1 (Turbine Proposal)
  div_rural <- droplevels(div_rural)
  yx_data <- div_rural[,c("perc_lib", "prop", "p_uni_degree", "log_pop_denc", "unemploy_rate", "log_median_inc", "p_immigrant")] 
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(div_rural$year), group=div_rural$master_id)
  mod_7 <- lm(perc_lib ~ prop + Y2003 + Y2007 + p_uni_degree + log_pop_denc + unemploy_rate + log_median_inc + p_immigrant, data=yx_diffed)
  round(coeftest(mod_7, vcov=vcovCluster_demean(mod_7, cluster=div_rural$master_id, first=nrow(div_rural)/3)),3)
  
  # Column 4 (Rural Precincts with Controls), Section 2 (Turbine Operational)
  div_rural <- droplevels(div_rural)
  yx_data <- div_rural[,c("perc_lib", "op", "p_uni_degree", "log_pop_denc", "unemploy_rate", "log_median_inc", "p_immigrant")] 
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(div_rural$year), group=div_rural$master_id)
  mod_8 <- lm(perc_lib ~ op + Y2003 + Y2007 + p_uni_degree + log_pop_denc + unemploy_rate + log_median_inc + p_immigrant, data=yx_diffed)
  round(coeftest(mod_8, vcov=vcovCluster_demean(mod_8, cluster=div_rural$master_id, first=nrow(div_rural)/3)),3)
  
  
  # Column 5 (Balanced Precincts), Section 1 (Turbine Proposal)
  yx_data <- divisions[,c("perc_lib","prop")]
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(divisions$year), group=divisions$master_id)
  mod_9 <- lm(perc_lib ~ prop + Y2003 + Y2007, data=yx_diffed, weights=divisions$weights_p) # add in weights
  round(coeftest(mod_9, vcov=vcovCluster_demean(mod_9, cluster=divisions$master_id, first=nrow(divisions)/3)),3)
  
  # Column 5 (Balanced Precincts), Section 2 (Turbine Operational)
  yx_data <- divisions[,c("perc_lib","op")]
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(divisions$year), group=divisions$master_id)
  mod_10 <- lm(perc_lib ~ op + Y2003 + Y2007, data=yx_diffed, weights=divisions$weights_o) # add in weights
  round(coeftest(mod_10, vcov=vcovCluster_demean(mod_10, cluster=divisions$master_id, first=nrow(divisions)/3)),3)
  
  
  # Column 6 (Balanced Precincts), Section 1 (Turbine Proposal)
  yx_data <- divisions[,c("perc_lib", "prop", "p_uni_degree", "log_pop_denc", "unemploy_rate", "log_median_inc", "p_immigrant")]
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(divisions$year), group=divisions$master_id)
  mod_11 <- lm(perc_lib ~ prop + Y2003 + Y2007 + p_uni_degree + log_pop_denc + unemploy_rate + log_median_inc + p_immigrant, data=yx_diffed, weights=divisions$weights_p) # add in weights
  round(coeftest(mod_11, vcov=vcovCluster_demean(mod_11, cluster=divisions$master_id, first=nrow(divisions)/3)),3)
  
  # Column 6 (Balanced Precincts), Section 2 (Turbine Operational)
  yx_data <- divisions[,c("perc_lib", "op", "p_uni_degree", "log_pop_denc", "unemploy_rate", "log_median_inc", "p_immigrant")]
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(divisions$year), group=divisions$master_id)
  mod_12 <- lm(perc_lib ~ op + Y2003 + Y2007 + p_uni_degree + log_pop_denc + unemploy_rate + log_median_inc + p_immigrant, data=yx_diffed, weights=divisions$weights_o) # add in weights
  round(coeftest(mod_12, vcov=vcovCluster_demean(mod_12, cluster=divisions$master_id, first=nrow(divisions)/3)),3)
  
  rm(mod_1, mod_2, mod_3, mod_4, mod_5, mod_6, mod_7, mod_8, mod_9, mod_10, mod_11, mod_12, yx_data, yx_diffed)
  
  
  ### TABLE 2 ###
  # Run the IV reg with this matched pair data and the flexible geographic controls reported in the paper. Use 'matchobj' created in the data construction section above.
  
  # Column 1, First Stage
  first <- lm(prop_3km ~ avg_pwr_log + mindistlake + mindistlake_sq + longitude + latitude + long_sq + lat_sq + long_lat + as.factor(ed_id), data=matchobj)
  summary(first)
  
  # F-statistic on instrument 
  mod1 <- lm(prop_3km ~ longitude + latitude + long_sq + lat_sq + mindistlake + mindistlake_sq + long_lat + as.factor(ed_id), data=matchobj) # Run one model without the instrument.
  mod2 <- lm(prop_3km ~ avg_pwr_log + longitude + latitude + long_sq + lat_sq + mindistlake + mindistlake_sq + long_lat + as.factor(ed_id), data=matchobj) # Run a second model with the instrument.
  waldtest(mod2, mod1) #Compare the two using a wald test. F-statistic = 68.
  
  # Column 2, Second Stage
  summary(ivreg(chng_lib ~ prop_3km + mindistlake + mindistlake_sq + longitude + long_sq + latitude + lat_sq + long_lat + as.factor(ed_id) | avg_pwr_log + mindistlake + mindistlake_sq + longitude + long_sq + latitude + lat_sq + long_lat + as.factor(ed_id), data = matchobj))
  
  rm(first, mod1, mod2)
  
  ### TABLE 3 ###
  
  # Column 1, Proposal in Precinct effect on Turnout
  yx_data <- divisions[,c("perc_turnout","prop")]
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(divisions$year), group=divisions$master_id)
  mod_1 <- lm(perc_turnout ~ prop + Y2003 + Y2007, data=yx_diffed)
  round(coeftest(mod_1, vcov=vcovCluster_demean(mod_1, cluster=divisions$master_id, first=nrow(divisions)/3)),3) 
  
  # Column 2, Proposal in Precinct effect on Turnout
  yx_data <- divisions[,c("perc_turnout","prop_3km")]
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(divisions$year), group=divisions$master_id)
  mod_2 <- lm(perc_turnout ~ prop_3km + Y2003 + Y2007, data=yx_diffed)
  round(coeftest(mod_2, vcov=vcovCluster_demean(mod_2, cluster=divisions$master_id, first=nrow(divisions)/3)),3) 
  
  # Column 3, Operational in Precinct effect on Turnout
  yx_data <- divisions[,c("perc_turnout","op")]
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(divisions$year), group=divisions$master_id)
  mod_3 <- lm(perc_turnout ~ op + Y2003 + Y2007, data=yx_diffed)
  round(coeftest(mod_3, vcov=vcovCluster_demean(mod_3, cluster=divisions$master_id, first=nrow(divisions)/3)),3) 
  
  # Column 4, Operational in Precinct effect on Turnout
  yx_data <- divisions[,c("perc_turnout","op_3km")]
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(divisions$year), group=divisions$master_id)
  mod_4 <- lm(perc_turnout ~ op_3km + Y2003 + Y2007, data=yx_diffed)
  round(coeftest(mod_4, vcov=vcovCluster_demean(mod_4, cluster=divisions$master_id, first=nrow(divisions)/3)),3) 
  
  rm(mod_1, mod_2, mod_3, mod_4, yx_data, yx_diffed)
  
  
  ### TABLE 4 ###
  
  # Column 1, Proposal in Precinct effect on Federal Liberal Party Vote Share
  yx_data <- divisions[,c("perc_lib_fed","prop")] 
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(divisions$year), group=divisions$master_id) 
  mod_1 <- lm(perc_lib_fed ~ prop + Y2003 + Y2007, data=yx_diffed) 
  round(coeftest(mod_1, vcov=vcovCluster_demean(mod_1, cluster=divisions$fed_id, first=nrow(divisions)/3)),3) 
  
  # Column 2, Proposal within 3 km of Precinct effect on Federal Liberal Party Vote Share
  yx_data <- divisions[,c("perc_lib_fed","prop_3km")] 
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(divisions$year), group=divisions$master_id) 
  mod_2 <- lm(perc_lib_fed ~ prop_3km + Y2003 + Y2007, data=yx_diffed) 
  round(coeftest(mod_2, vcov=vcovCluster_demean(mod_2, cluster=divisions$fed_id, first=nrow(divisions)/3)),3) 
  
  # Column 3, Operational in Precinct effect on Federal Liberal Party Vote Share
  yx_data <- divisions[,c("perc_lib_fed","op")] 
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(divisions$year), group=divisions$master_id) 
  mod_3 <- lm(perc_lib_fed ~ op + Y2003 + Y2007, data=yx_diffed) 
  round(coeftest(mod_3, vcov=vcovCluster_demean(mod_3, cluster=divisions$fed_id, first=nrow(divisions)/3)),3) 
  
  # Column 4, Operational within 3 km of Precinct effect on Federal Liberal Party Vote Share
  yx_data <- divisions[,c("perc_lib_fed","op_3km")] 
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(divisions$year), group=divisions$master_id) 
  mod_4 <- lm(perc_lib_fed ~ op_3km + Y2003 + Y2007, data=yx_diffed) 
  round(coeftest(mod_4, vcov=vcovCluster_demean(mod_4, cluster=divisions$fed_id, first=nrow(divisions)/3)),3) 
  
  rm(mod_1, mod_2, mod_3, mod_4, yx_data, yx_diffed)
  
  
  ### Table 5 ###
  # Information compiled from websites.
  
  
  ### Table 6 ###
  # Balance test in SI. Proposals within 3km, pre-treatment (2003)
  d2003 <- divisions[which(divisions$year==2003),]
  mb <- MatchBalance(treat_p_3km ~ perc_lib + p_uni_degree + log_median_inc + avg_home_val + log_pop_denc, data=d2003, ks=T) 
  table <- baltest.collect(matchbal.out=mb, var.names=c("Liberal Vote Share", "Highest Degree Uni %",  "Median Income (log)", "Average value of home", "Population density (log)"), after=F)
  round(table, 3)
  
  rm(mb, table)
  
  
  ### Table 7 ###
  # Balance test in SI. Instrument.
  mb <- MatchBalance(prop_3km ~ p_uni_degree + log_median_inc + avg_home_val_log_07 + log_pop_denc, data=matchobj, ks=T) # Used matched pair dataset created above.
  table <- baltest.collect(matchbal.out=mb, var.names=c("Population with a university degree (%)", "Median income (log)", "Average home value 2006 (log)", "Pop Density (log)"), after=F)
  round(table, 3)
  
  rm(mb, table)
  
  
  #### FIGURES ####
  
  ### FIGURE 1 ###
  # Made using ArcGIS.
  
  
  ### FIGURE 2 ###
  ptrends <- divisions
  ptrends$year2 <- as.character(ptrends$year) # for labelling the plot
  
  # Left side of plot, Precincts with proposed turbines 
  ptrends$status <- ifelse(ptrends$treat_p==1, "treated", "control")
  pt <- summarySE(ptrends, measurevar="perc_lib", groupvars=c("status","year2")) # First need to run function above.
  pt
  
  pd <- position_dodge(.15) # The errorbars overlapped, so use position_dodge to move them horizontally
  
  plot <- ggplot(pt, aes(x=year2, y=perc_lib, colour=status, group=status)) + 
    geom_line(position=pd, size=1.40) +
    scale_colour_manual(values=c("#0072B2", "#B22222")) +
    geom_errorbar(aes(ymin=perc_lib-ci, ymax=perc_lib+ci), colour="black", width=.15, position=pd, size=0.75) +
    geom_point(position=pd, size=2.5)
  
  plot +  
    xlab("Election Year") + 
    ylab("Liberal Party Vote Share") +
    theme(plot.title = element_text(size=20), axis.text.x = element_text(colour="black", size=20), axis.text.y = element_text(colour="black", size=20), axis.title.x = element_text(colour="black", size=20), axis.title.y = element_text(colour="black", size=20), legend.position="none") +
    scale_y_continuous(labels=percent, limits=c(0.20, 0.57))
  
  # Right side of plot, Precincts with operational turbines 
  ptrends$status <- ifelse(ptrends$treat_o==1, "treated", "control") 
  pt <- summarySE(ptrends, measurevar="perc_lib", groupvars=c("status","year2"))
  pt
  
  plot <- ggplot(pt, aes(x=year2, y=perc_lib, colour=status, group=status, ymax=(0.55))) + 
    geom_line(position=pd, size=1.40) +
    scale_colour_manual(values=c("#0072B2", "#B22222")) +
    geom_errorbar(aes(ymin=perc_lib-ci, ymax=perc_lib+ci), colour="black", width=.15, position=pd, size=0.75) +
    geom_point(position=pd, size=2.5)
  
  plot +  
    xlab("Election Year") + 
    ylab("Liberal Party Vote Share") +
    theme(plot.title = element_text(size=20), axis.text.x = element_text(colour="black", size=20), axis.text.y = element_text(colour="black", size=20), axis.title.x = element_text(colour="black", size=20), axis.title.y = element_text(colour="black", size=20), legend.position="none") +
    scale_y_continuous(labels=percent, limits=c(0.20, 0.57)) 
  
  rm(pt, ptrends, plot, pd)
  
  
  ### FIGURE 3 ###
  
  # Left side of plot, Distance to proposed turbine
  # Set up empty object:
  plot_prop_omit <- data.frame(dist=c("0km","1km","2km","3km","4km","5km"), ATT=c(NA,NA,NA,NA,NA,NA), SES=c(NA,NA,NA,NA,NA,NA), ci_up=c(NA,NA,NA,NA,NA,NA), ci_low=c(NA,NA,NA,NA,NA,NA)) 
  plot_prop_omit$ATT <- as.numeric(plot_prop_omit$ATT)
  plot_prop_omit$SES <- as.numeric(plot_prop_omit$SES)
  plot_prop_omit$ci_up <- as.numeric(plot_prop_omit$ci_up)
  plot_prop_omit$ci_low <- as.numeric(plot_prop_omit$ci_low)
  str(plot_prop_omit)
  
  # 0km from turbine 
  dkm0 <- divisions[which(divisions$treat_p==1 | divisions$treat_p_6km==0),] # Doesn't allow any units treated within 1-6km to be controls.
  dkm0$master_id <- droplevels(dkm0$master_id)
  
  #demean
  yx_data <- dkm0[,c("perc_lib","prop")]
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(dkm0$year), group=dkm0$master_id) 
  mod_1 <- lm(perc_lib ~ prop + Y2003 + Y2007, data=yx_diffed)
  round(coeftest(mod_1, vcov=vcovCluster_demean(mod_1, cluster=dkm0$master_id, first=nrow(dkm0)/3)),5) 
  confint(mod_1, level=0.95)
  
  #save to object
  plot_prop_omit[1,2] <- -0.04317 #ATT
  plot_prop_omit[1,3] <- 0.00943 #SES
  plot_prop_omit[1,4] <- -0.030505 #ci_up
  plot_prop_omit[1,5] <- -0.055844 #ci_low
  
  # 1km 
  dkm1 <- divisions[which(divisions$treat_p_1km==1 & divisions$treat_p==0 | divisions$treat_p_6km==0),] # subset
  dkm1$master_id <- droplevels(dkm1$master_id) # drop levels
  
  #demean
  yx_data <- dkm1[,c("perc_lib","prop_1km")] #change prop_1km, dataset
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(dkm1$year), group=dkm1$master_id) # change dataset x2
  mod_1 <- lm(perc_lib ~ prop_1km + Y2003 + Y2007, data=yx_diffed) 
  round(coeftest(mod_1, vcov=vcovCluster_demean(mod_1, cluster=dkm1$master_id, first=nrow(dkm1)/3)),5)
  confint(mod_1, level=0.95)
  
  #save to object
  plot_prop_omit[2,2] <-  -0.03572 #ATT
  plot_prop_omit[2,3] <-  0.01384 #SES
  plot_prop_omit[2,4] <- -0.017599 #ci_up
  plot_prop_omit[2,5] <- -0.05384 #ci_low
  
  # 2km
  dkm2 <- divisions[which(divisions$treat_p_2km==1 & divisions$treat_p_1km==0 | divisions$treat_p_6km==0),]
  dkm2$master_id <- droplevels(dkm2$master_id)
  
  #demean
  yx_data <- dkm2[,c("perc_lib","prop_2km")] 
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(dkm2$year), group=dkm2$master_id) 
  mod_1 <- lm(perc_lib ~ prop_2km + Y2003 + Y2007, data=yx_diffed) 
  round(coeftest(mod_1, vcov=vcovCluster_demean(mod_1, cluster=dkm2$master_id, first=nrow(dkm2)/3)),5)
  confint(mod_1, level=0.95)
  
  #save to object 
  plot_prop_omit[3,2] <-  -0.05079 #ATT
  plot_prop_omit[3,3] <-  0.01414 #SES
  plot_prop_omit[3,4] <- -0.0347295 #ci_up
  plot_prop_omit[3,5] <- -0.066846 #ci_low
  
  # 3km 
  dkm3 <- divisions[which(divisions$treat_p_3km==1 & divisions$treat_p_2km==0 | divisions$treat_p_6km==0),]
  dkm3$master_id <- droplevels(dkm3$master_id) # drop levels
  
  #demean
  yx_data <- dkm3[,c("perc_lib","prop_3km")] 
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(dkm3$year), group=dkm3$master_id) 
  mod_1 <- lm(perc_lib ~ prop_3km + Y2003 + Y2007, data=yx_diffed) 
  round(coeftest(mod_1, vcov=vcovCluster_demean(mod_1, cluster=dkm3$master_id, first=nrow(dkm3)/3)),5) 
  confint(mod_1, level=0.95)
  
  #save to object
  plot_prop_omit[4,2] <-  -0.02144 #ATT
  plot_prop_omit[4,3] <-  0.01185 #SES
  plot_prop_omit[4,4] <- -0.004736 #ci_up
  plot_prop_omit[4,5] <- -0.038138 #ci_low
  
  #4km 
  dkm4 <- divisions[which(divisions$treat_p_4km==1 & divisions$treat_p_3km==0 | divisions$treat_p_6km==0),]
  dkm4$master_id <- droplevels(dkm4$master_id) # drop levels
  
  #demean
  yx_data <- dkm4[,c("perc_lib","prop_4km")] 
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(dkm4$year), group=dkm4$master_id)
  mod_1 <- lm(perc_lib ~ prop_4km + Y2003 + Y2007, data=yx_diffed)
  round(coeftest(mod_1, vcov=vcovCluster_demean(mod_1, cluster=dkm4$master_id, first=nrow(dkm4)/3)),5) 
  confint(mod_1, level=0.95)
  
  #save to object
  plot_prop_omit[5,2] <-  -0.00154 #ATT
  plot_prop_omit[5,3] <-  0.00966 #SES
  plot_prop_omit[5,4] <- 0.0121278 #ci_up
  plot_prop_omit[5,5] <- -0.015211 #ci_low
  
  #5km
  dkm5 <- divisions[which(divisions$treat_p_5km==1 & divisions$treat_p_4km==0 | divisions$treat_p_6km==0),]
  dkm5$master_id <- droplevels(dkm5$master_id) 
  
  #demean
  yx_data <- dkm5[,c("perc_lib","prop_5km")] 
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(dkm5$year), group=dkm5$master_id) 
  mod_1 <- lm(perc_lib ~ prop_5km + Y2003 + Y2007, data=yx_diffed) 
  round(coeftest(mod_1, vcov=vcovCluster_demean(mod_1, cluster=dkm5$master_id, first=nrow(dkm5)/3)),5) 
  confint(mod_1, level=0.95)
  
  #save to object
  plot_prop_omit[6,2] <-  -0.01562 #ATT
  plot_prop_omit[6,3] <-  0.00985 #SES
  plot_prop_omit[6,4] <- -0.00181 #ci_up
  plot_prop_omit[6,5] <- -0.0294203 #ci_low
  
  # Plot the results:
  ggplot(plot_prop_omit, aes(y=ATT, x=dist)) +
    geom_point() +
    geom_hline(aes(yintercept=0), color="black", linetype="dashed") +
    geom_errorbar(aes(ymin=ci_low, ymax=ci_up), width=.1) +
    xlab("distance to proposed turbine") +
    theme(axis.text.x = element_text(colour="black", size=18), axis.text.y = element_text(colour="black", size=18), axis.title.x = element_text(colour="black", size=18), axis.title.y = element_text(colour="black", size=18)) +
    scale_y_continuous(labels=percent)
  
  
  # Right side of plot, Distance to operational turbine
  # Set up empty object:
  plot_op_omit <- data.frame(dist=c("0km","1km","2km","3km","4km","5km"), ATT=c(NA,NA,NA,NA,NA,NA), SES=c(NA,NA,NA,NA,NA,NA), ci_up=c(NA,NA,NA,NA,NA,NA), ci_low=c(NA,NA,NA,NA,NA,NA))
  plot_op_omit
  plot_op_omit$ATT <- as.numeric(plot_op_omit$ATT)
  plot_op_omit$SES <- as.numeric(plot_op_omit$SES)
  plot_op_omit$ci_up <- as.numeric(plot_op_omit$ci_up)
  plot_op_omit$ci_low <- as.numeric(plot_op_omit$ci_low)
  
  # 0km 
  dkm0 <- divisions[which(divisions$treat_o==1 | divisions$treat_o_6km==0),]
  dkm0$master_id <- droplevels(dkm0$master_id)
  
  #demean
  yx_data <- dkm0[,c("perc_lib","op")] 
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(dkm0$year), group=dkm0$master_id) 
  mod_1 <- lm(perc_lib ~ op + Y2003 + Y2007, data=yx_diffed) 
  round(coeftest(mod_1, vcov=vcovCluster_demean(mod_1, cluster=dkm0$master_id, first=nrow(dkm0)/3)),5) 
  confint(mod_1, level=0.95)
  
  #save to object
  plot_op_omit[1,2] <- -0.09366 #ATT
  plot_op_omit[1,3] <- 0.01445 #SES
  plot_op_omit[1,4] <- -0.0702998 #ci_up
  plot_op_omit[1,5] <- -0.1170188 #ci_low
  
  # 1km 
  dkm1 <- divisions[which(divisions$treat_o_1km==1 & divisions$treat_o==0 | divisions$treat_o_6km==0),]
  dkm1$master_id <- droplevels(dkm1$master_id) # drop levels
  
  #demean
  yx_data <- dkm1[,c("perc_lib","op_1km")]
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(dkm1$year), group=dkm1$master_id)
  mod_1 <- lm(perc_lib ~ op_1km + Y2003 + Y2007, data=yx_diffed) 
  round(coeftest(mod_1, vcov=vcovCluster_demean(mod_1, cluster=dkm1$master_id, first=nrow(dkm1)/3)),5) 
  confint(mod_1, level=0.95)
  
  #save to object
  plot_op_omit[2,2] <-  -0.09497 #ATT
  plot_op_omit[2,3] <-  0.02693 #SES
  plot_op_omit[2,4] <- -0.06061 #ci_up
  plot_op_omit[2,5] <- -0.129317 #ci_low
  plot_op_omit
  
  # 2km
  dkm2 <- divisions[which(divisions$treat_o_2km==1 & divisions$treat_o_1km==0 | divisions$treat_o_6km==0),]
  dkm2$master_id <- droplevels(dkm2$master_id) # drop levels
  
  #demean
  yx_data <- dkm2[,c("perc_lib","op_2km")]
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(dkm2$year), group=dkm2$master_id) 
  mod_1 <- lm(perc_lib ~ op_2km + Y2003 + Y2007, data=yx_diffed) 
  round(coeftest(mod_1, vcov=vcovCluster_demean(mod_1, cluster=dkm2$master_id, first=nrow(dkm2)/3)),5)
  confint(mod_1, level=0.95)
  
  #save to object 
  plot_op_omit[3,2] <-  -0.10076 #ATT
  plot_op_omit[3,3] <-  0.02402 #SES
  plot_op_omit[3,4] <- -0.126482 #ci_up
  plot_op_omit[3,5] <- -0.07504353 #ci_low
  
  # 3km 
  dkm3 <- divisions[which(divisions$treat_o_3km==1 & divisions$treat_o_2km==0 | divisions$treat_o_6km==0),]
  dkm3$master_id <- droplevels(dkm3$master_id) # drop levels
  
  #demean
  yx_data <- dkm3[,c("perc_lib","op_3km")] 
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(dkm3$year), group=dkm3$master_id) 
  mod_1 <- lm(perc_lib ~ op_3km + Y2003 + Y2007, data=yx_diffed) 
  round(coeftest(mod_1, vcov=vcovCluster_demean(mod_1, cluster=dkm3$master_id, first=nrow(dkm3)/3)),5) 
  confint(mod_1, level=0.95)
  
  #save to object
  plot_op_omit[4,2] <-  -0.08997 #ATT
  plot_op_omit[4,3] <-  0.02265 #SES
  plot_op_omit[4,4] <- -0.05698 #ci_up
  plot_op_omit[4,5] <- -0.122950 #ci_low
  
  #4km 
  dkm4 <- divisions[which(divisions$treat_o_4km==1 & divisions$treat_o_3km==0 | divisions$treat_o_6km==0),]
  dkm4$master_id <- droplevels(dkm4$master_id) # drop levels
  
  #demean
  yx_data <- dkm4[,c("perc_lib","op_4km")]
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(dkm4$year), group=dkm4$master_id) 
  mod_1 <- lm(perc_lib ~ op_4km + Y2003 + Y2007, data=yx_diffed) 
  round(coeftest(mod_1, vcov=vcovCluster_demean(mod_1, cluster=dkm4$master_id, first=nrow(dkm4)/3)),5) 
  confint(mod_1, level=0.95)
  
  #save to object
  plot_op_omit[5,2] <-  -0.02609 #ATT
  plot_op_omit[5,3] <-  0.02829 #SES
  plot_op_omit[5,4] <- 0.00091 #ci_up
  plot_op_omit[5,5] <- -0.053102 #ci_low
  
  #5km
  dkm5 <- divisions[which(divisions$treat_o_5km==1 & divisions$treat_o_4km==0 | divisions$treat_o_6km==0),]
  dkm5$master_id <- droplevels(dkm5$master_id) # drop levels
  
  #demean
  yx_data <- dkm5[,c("perc_lib","op_5km")] 
  yx_diffed <- demeaner(yx=yx_data, T=as.factor(dkm5$year), group=dkm5$master_id) 
  mod_1 <- lm(perc_lib ~ op_5km + Y2003 + Y2007, data=yx_diffed)
  round(coeftest(mod_1, vcov=vcovCluster_demean(mod_1, cluster=dkm5$master_id, first=nrow(dkm5)/3)),5)
  confint(mod_1, level=0.95)
  
  #save to object 
  plot_op_omit[6,2] <-  0.01472 #ATT
  plot_op_omit[6,3] <-  0.02234 #SES
  plot_op_omit[6,4] <-  0.0413351 #ci_up
  plot_op_omit[6,5] <- -0.0119019 #ci_low
  
  # Plot:
  ggplot(plot_op_omit, aes(y=ATT, x=dist)) +
    geom_point() +
    geom_hline(aes(yintercept=0), color="black", linetype="dashed") +
    geom_errorbar(aes(ymin=ci_low, ymax=ci_up), width=.1) +
    xlab("distance to operational turbine") +
    theme(axis.text.x = element_text(colour="black", size=18), axis.text.y = element_text(colour="black", size=18), axis.title.x = element_text(colour="black", size=18), axis.title.y = element_text(colour="black", size=18)) +
    scale_y_continuous(labels=percent)
  
  # Remove dataframes:
  rm(plot_op_omit, plot_prop_omit, yx_data, yx_diffed, dkm0, dkm1, dkm2, dkm3, dkm4, dkm5, mod_1)
  
  
  ### FIGURE 4 ### 
  
  news <- read.csv("newspapers_count.csv", stringsAsFactors=FALSE, na.strings=c("NA", ""),  strip.white = TRUE)
  news
  
  ggplot(data=news, aes(x=year, y=count)) + geom_bar(stat="identity") +
    scale_x_continuous(breaks=c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011), name="Year") +
    scale_y_continuous(breaks=c(0, 10, 20, 30, 40, 50, 60), name="Number of Newspaper Articles") +
    theme(legend.position="none", axis.ticks.y=element_blank(), axis.text.x = element_text(size=13, color="black"), axis.text.y = element_text(size=13, color="black"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))
  
  rm(news)
  
  
  ### FIGURE 5 ###
  
  overlaps <- read.csv("precincts0711.csv", stringsAsFactors=FALSE, na.strings=c("NA", ""),  strip.white = TRUE)
  
  # Relevant variable, overlap_max, shows the amount of overlap between each precinct polygon in 2007 and 2011, as calculated in ArcGIS. 
  plot(hist(overlaps$overlap_max, breaks=20), xlab="Maximum Proportion of Overlap between 2007 and 2011 Divisions", main="Extent of Division Redistricting")
  
  rm(overlaps)
  
  
  ### FIGURE 6 ###
  
  d2011 <- divisions[which(divisions$year==2011),]
  d2003 <- divisions[which(divisions$year==2003),]
  
  # Left side of figure, 2003 liberal party voteshare
  mt <- mean(d2003$perc_lib[which(d2003$treat_p==1)])
  mc <- mean(d2003$perc_lib[which(d2003$treat_p==0)])
  df.hlines <- data.frame(treat_p=c("0","1"), hline=c(mc,mt))
  df.hlines
  
  plot <- ggplot(d2003, aes(x=as.factor(treat_p), y=perc_lib, fill=as.factor(treat_p))) + geom_point(alpha = 0.55, position='jitter', size=1.5) + scale_fill_brewer(palette="Set1") # Given jitter and alpha (density of points) the plots will vary slightly with each creation.
  
  plot + geom_errorbar(data=df.hlines, aes(y=hline, ymax=hline, ymin=hline), colour="blue") + 
    theme(legend.position="none", axis.text.x = element_text(size=13), axis.text.y = element_text(size=13)) + 
    scale_y_continuous(name="Liberal Party Vote Share", limits=c(0,0.8)) + 
    scale_x_discrete(labels=c("control", "treated"), name="") + 
    coord_flip() + 
    #opts(axis.text.y = theme_text(size = 12), axis.text.x = theme_text(size = 12)) + 
    ggtitle("2003")
  
  
  # Right side of figure, 2011 liberal party voteshare
  mt <- mean(d2011$perc_lib[which(d2011$prop==1)])
  mc <- mean(d2011$perc_lib[which(d2011$prop==0)])
  
  df.hlines <- data.frame(prop=c("0","1"), hline=c(mc,mt))
  df.hlines
  
  plot <- ggplot(d2011, aes(x=as.factor(prop), y=perc_lib, fill=as.factor(prop))) + geom_point(alpha = 0.55, size=1.5, position='jitter') + scale_fill_brewer(palette="Set1")
  
  plot + geom_errorbar(data=df.hlines, aes(y=hline, ymax=hline, ymin=hline, xmax=1), colour="blue") + 
    theme(legend.position="none", axis.ticks.y=element_blank(), axis.text.x = element_text(size=13), axis.text.y = element_text(size=13)) + 
    scale_y_continuous(name="Liberal Party Vote Share", breaks=c(0.0, 0.2, 0.4, 0.6, 0.8, 1), limits=c(0,0.8)) + 
    scale_x_discrete(labels="", name="") + 
    coord_flip() + 
    ggtitle("2011")
  
  rm(d2003, d2011, df.hlines, mc, mt, plot)
  
  
  ### FIGURE 7 ###
  
  matchobj$fits <- residuals(lm(chng_lib~as.factor(ed_id) + prop_3km + mindistlake + mindistlake_sq + longitude + latitude + long_lat + as.factor(ed_id), data=matchobj)) # fit the same model used in Table 2, without the instrument.
  
  # plot the residuals from that model against the instrument:
  plot(x=matchobj$avg_pwr_log, y=matchobj$fits, xlab="Average Wind Power", ylab="Residual Change in Liberal Party Vote Share", main="Residuals of Predicted Change in Vote Share, Given \n Treatment, Riding Fixed Effects and Geographic Covariates")
  lines(lowess(matchobj$avg_pwr_log,matchobj$fits), col="blue") 
