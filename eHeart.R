#Package dependencies
require(dplyr)
require(nlme)
require(tidyr)

#load "mixoutsamp" function for mixed model predictions
source("mixoutsamp_v2_MA.R")

#load eHeart HR
HR<-read.csv("HR_10year.csv")

#load mixed model output
fit<-load("mixed_model_output.rdata")

#The function expects the data in long format with a unique column for: 
#Patient ID (id.name)
#Sex (sex.name)
#Date of exposure (exposure.date.name)
#Risk factor (exposure.name)
#Risk factor measurement (exposure.value.name) 
#Date of formal assessment (baseline.date.name) E.g. today’s date


#Variables entered as string, e.g. id.name = "idno":
#id.name, sex.name, exposure.date.name, exposure.name, exposure.value.name, baseline.date.name, date.of.birth.name
#Risk factors (exposure.name) listed within exposure.name entered as string
#sbp.risk.factor, tchol.risk.factor, hdl.risk.factor, anti_hypt.risk.factor, statins.risk.factor, smoking.risk.factor, diabetes.risk.factor


eHeart<- function(gp_data,
                  id.name=NULL,
                  sex.name = NULL,
                  exposure.date.name = NULL, #Date of primary care records
                  exposure.name = NULL, #exposure.name variable lists risk.factors
                  exposure.value.name = NULL,
                  baseline.date.name = NULL,
                  date.of.birth.name = NULL,
                  
                  sbp.risk.factor = NULL,
                  tchol.risk.factor = NULL,
                  hdl.risk.factor = NULL,
                  anti_hypt.risk.factor = NULL,
                  statins.risk.factor = NULL,
                  smoking.risk.factor = NULL,
                  diabetes.risk.factor = NULL, 
                  
                  sex_men = 1,
                  sex_women = 2,
                  anti_hypt_neg = 0,
                  anti_hypt_pos = 1,
                  statins_neg = 0,
                  statins_pos = 1,
                  smoking_neg = 0,
                  smoking_pos = 1,
                  diabetes_neg = 0,
                  diabetes_pos = 1
)


{
  
  #---- Error message: missing required variables
  if (any(is.null(id.name),
          is.null(exposure.name),
          is.null(exposure.date.name),
          is.null(exposure.value.name))) {
    stop("Must specify all variables/columns in data")
  }
  #---- end of error message
  
  
  #---- Error message: specify unique name for risk factors within exposure.name
  if (any(is.null(sbp.risk.factor),
          is.null(tchol.risk.factor),
          is.null(hdl.risk.factor),
          is.null(smoking.risk.factor),
          is.null(diabetes.risk.factor))) {
    stop(message=paste0("Must specify all risk factors within variable: ",exposure.name))
  }
  #---- end of error message
  
  
  #---- Error message: Check for date of birth AND assessment date variables
  if (any(is.null(baseline.date.name),
          is.null(date.of.birth.name))){
    stop(message=paste0("Must specify all baseline.date.name date.of.birth.name variables"))
  }
  #---- end of error message
  
  
  #---- Error message: ID name does not exist in gp_data
  if (!(id.name %in% names(gp_data))) {stop("Error: specified id.name variable does not exist in dataset")}
  #---- end of error message
  
  #---- Error message: variable name does not exist in gp_data
  if (!(exposure.date.name %in% names(gp_data))) {stop("Error: specified exposure.date.name variable does not exist in dataset")}
  #---- end of error message
  
  #---- Error message: variable name does not exist in gp_data
  if (!(exposure.name %in% names(gp_data))) {stop("Error: specified exposure.name variable does not exist in dataset")}
  #---- end of error message
  
  #---- Error message: variable name does not exist in gp_data
  if (!(exposure.value.name %in% names(gp_data))) {stop("Error: specified exposure.value.name variable does not exist in dataset")}
  #---- end of error message
  
  #---- Error message: variable name does not exist in gp_data
  if (!(baseline.date.name %in% names(gp_data))) {stop("Error: specified baseline.date.name variable does not exist in dataset")}
  #---- end of error message
  
  #---- Error message: variable name does not exist in gp_data
  if (!(date.of.birth.name %in% names(gp_data))) {stop("Error: specified date.of.birth.name variable does not exist in dataset")}
  #---- end of error message
  
  #---- Error message: Check dates in baseline date variable is in format '%Y-%m-%d'
  if (baseline.date.name %in% names(gp_data)) {
    if (all(is.na(as.Date(as.character(gp_data[,baseline.date.name]),format="%Y-%m-%d")))) {
      stop(message=paste0("Convert variable ",baseline.date.name, " to date format '%Y-%m-%d'"))
    }
  }
  #---- end of error message
  
  #---- Error message: Check specified date (string) is in correct format
  if ( all(!(baseline.date.name %in% names(gp_data)), all(is.na(as.Date(as.character(baseline.date.name),format="%Y-%m-%d"))))) {
    stop(message=paste0("Entered date for baseline.date.name must be converted to date format 'YYYY-MM-DD'"))
  }
  #---- end of error message
  
  #---- Error message: Check exposure date is in format '%Y-%m-%d'
  if (exposure.date.name %in% names(gp_data)) {
    if (all(is.na(as.Date(as.character(gp_data[,exposure.date.name]),format="%Y-%m-%d")))) {
      stop(message=paste0("Convert variable ",exposure.date.name, " to date format '%Y-%m-%d'"))
    }
  }
  #---- end of error message
  
  #---- Error message: Check date of birth is in format '%Y-%m-%d'
  if (date.of.birth.name %in% names(gp_data)) {
    if (all(is.na(as.Date(as.character(gp_data[,date.of.birth.name]),format="%Y-%m-%d")))) {
      stop(message=paste0("Convert variable ",date.of.birth.name, " to date format 'YYYY-MM-DD'"))
    }
  }
  #---- end of error message
  
  ############################################################
  
  #VARIABLE CLEANING
  
  #Calculate exposure age
  exposure.age.name <- "exposure_age"
  gp_data[exposure.age.name] <- as.numeric(gp_data[,exposure.date.name] - gp_data[,date.of.birth.name])/365.25
  
  
  #Baseline age calculation if specify baseline.date variable
  if (baseline.date.name %in% names(gp_data)) {
    baseline.age.name <- "baseline_age"
    gp_data[baseline.age.name] <- as.numeric(gp_data[,baseline.date.name] - gp_data[,date.of.birth.name])/365.25
  }
  
  #Create baseline date variable if using string entered baseline date
  if (!(baseline.date.name %in% names(gp_data))) {
    gp_data["baseline_date"] <- as.Date(as.character(baseline.date.name))
    baseline.age.name <- "baseline_age"
    gp_data[baseline.age.name] <- as.numeric(gp_data[["baseline_date"]] - gp_data[,date.of.birth.name])/365.25
    
  }
  
  #Time before baseline age
  gp_data["agemj"] = gp_data[exposure.age.name] - gp_data[baseline.age.name]
  
  #Remove anyone outside of baseline age range 40-69
  gp_data <- subset(gp_data,gp_data[baseline.age.name]>=40 & gp_data[baseline.age.name]<70)
  
  #---- Error message: Check that blood pressure is in correct units
  if (mean(gp_data[gp_data[exposure.name] == sbp.risk.factor,exposure.value.name],na.rm=T) <100 |
      mean(gp_data[gp_data[exposure.name] == sbp.risk.factor,exposure.value.name],na.rm=T) >200 ) 
  {stop("sbp.risk.factor variable likely not in valid units - check values are in mmHg")}
  #---- end of error message
  
  #---- Error message: Check that tchol is in correct units
  if (mean(gp_data[gp_data[exposure.name] == tchol.risk.factor,exposure.value.name],na.rm=T) <3 |
      mean(gp_data[gp_data[exposure.name] == tchol.risk.factor,exposure.value.name],na.rm=T) >15 ) 
  {stop("tchol.risk.factor variable likely not in valid units - check values are in mmol/L")}
  #---- end of error message
  
  
  #---- Error message: Check that hdl is in correct units
  if (mean(gp_data[gp_data[exposure.name] == hdl.risk.factor,exposure.value.name],na.rm=T) <1 |
      mean(gp_data[gp_data[exposure.name] == hdl.risk.factor,exposure.value.name],na.rm=T) >3 ) 
  {stop("hdl.risk.factor variable likely not in valid units - check values are in mmol/L")}
  #---- end of error message
  
  
  
  #Remove outliers
  #Remove systolic blood pressure <60 or >250 mmHg
  #Remove total cholesterol <1.75 or >20 mmol/L
  #Remove HDL <0.3 or >3.1 mmol/L
  gp_data<-gp_data[!(gp_data[exposure.name] == sbp.risk.factor & (gp_data[exposure.value.name] <60 | gp_data[exposure.value.name] >250)),]  
  gp_data<-gp_data[!(gp_data[exposure.name] == tchol.risk.factor & (gp_data[exposure.value.name] <1.75 | gp_data[exposure.value.name] >20)),]  
  gp_data<-gp_data[!(gp_data[exposure.name] == hdl.risk.factor & (gp_data[exposure.value.name] <0.3 | gp_data[exposure.value.name] >3.1)),]  
  
  #######################################################
  
  #MIXED MODEL CALCULATIONS
  
  #Restrict to specified risk factors only
  
  mixed.risk.factors <- c(sbp.risk.factor,tchol.risk.factor,hdl.risk.factor,anti_hypt.risk.factor,
                          statins.risk.factor,smoking.risk.factor)
  
  gp_data_mixed<- gp_data[gp_data[,exposure.name] %in% mixed.risk.factors,] 
  
  gp_data_mixed<-gp_data_mixed[order(gp_data_mixed[id.name], gp_data_mixed[exposure.age.name]),]
  
  #Create two indicator variables: bpmed_use and statin_use for use with sbp and tchol
  
  #Find out first recorded age of anti hypertensive medication (if any)
  gp_data_mixed<- gp_data_mixed %>% group_by_at(vars(id.name,exposure.name)) %>% 
    mutate(anti_hypt_age = ifelse(exposure.name == anti_hypt.risk.factor,first(exposure.age.name),0)) %>% as.data.frame()
  gp_data_mixed <- gp_data_mixed %>% group_by_at(id.name) %>% mutate(anti_hypt_age2 = max(anti_hypt_age)) %>% as.data.frame()
  
  #For everyone with SBP, bpmed_use = 1 if the medication was prescribed after the SBP measurement. 0 otherwise 
  gp_data_mixed<- gp_data_mixed %>% group_by_at(id.name) %>% 
    mutate(bpmed_use = ifelse(exposure.name == sbp.risk.factor & exposure.age.name >= anti_hypt_age2 & anti_hypt_age2>0,
                              1,0)) %>% as.data.frame() 
  
  ##do the same thing for statins
  gp_data_mixed<- gp_data_mixed %>% group_by_at(vars(id.name,exposure.name)) %>% 
    mutate(statins_age = ifelse(exposure.name == statins.risk.factor,first(exposure.age.name),0)) %>% as.data.frame()
  gp_data_mixed <- gp_data_mixed %>% group_by_at(id.name) %>% mutate(statins_age2 = max(statins_age)) %>% as.data.frame()
  
  #For everyone with tchol, statins = 1 if the medication was prescribed after the tchol measurement. 0 otherwise 
  gp_data_mixed<- gp_data_mixed %>% group_by_at(id.name)  %>% 
    mutate(statin_use = ifelse(exposure.name == tchol.risk.factor & exposure.age.name >= statins_age2 & statins_age2>0,
                               1,0)) %>% as.data.frame() 
  
  #Removing anti_hypt medication and statin from exposure type
  gp_data_mixed<- select(gp_data_mixed, -c(anti_hypt_age, anti_hypt_age2,statins_age, statins_age2))
  mixed.risk.factors2 <- mixed.risk.factors[!mixed.risk.factors %in% c(anti_hypt.risk.factor,statins.risk.factor)]
  gp_data_mixed<- gp_data_mixed[gp_data_mixed[,exposure.name] %in% mixed.risk.factors2,]
  
  #rename smoking variable
  gp_data_mixed[exposure.name][gp_data_mixed[exposure.name] == smoking.risk.factor] <- "smokbin"
  
  #Change exposure type to factor
  gp_data_mixed[exposure.name] <- as.factor(gp_data_mixed[,exposure.name])
  
  #Rename variables
  gp_data_mixed<-gp_data_mixed %>% rename(values = all_of(exposure.value.name),
                                          exp.type = all_of(exposure.name),
                                          patid = all_of(id.name))
  
  
  #Calculate standardisation scaling factors
  gp_data_mixed<- gp_data_mixed %>% 
    group_by_at(vars(all_of(sex.name),exp.type)) %>% 
    mutate(mean_std = mean(values,na.rm=T), sd_std = sd(values,na.rm=T)) %>% 
    as.data.frame()
  
  gp_data_mixed[gp_data_mixed$exp.type == "smokbin",]$mean_std <-0
  gp_data_mixed[gp_data_mixed$exp.type == "smokbin",]$sd_std <-1
  gp_data_mixed$values<-(gp_data_mixed$values-gp_data_mixed$mean_std)/gp_data_mixed$sd_std
  standardise <- gp_data_mixed[c(paste0(sex.name),"exp.type","mean_std","sd_std")]
  standardise<- gp_data_mixed %>% group_by_at(vars(all_of(sex.name),exp.type))  %>% summarise(mean_std=mean(mean_std),sd_std=mean(sd_std)) %>% as.data.frame()
  
  
  # MIXED MODEL AND MERGE WITH LOCF of bpmed_use and statin_use
  results = NULL
  print("Mixed model calculations...")
  for (ii in c(sex_men,sex_women)) {
    gp_data_mixed_sex = gp_data_mixed[gp_data_mixed[sex.name] == ii,]
    age_range<-  sort(unique(floor(gp_data_mixed_sex[,baseline.age.name])))
    if (length(age_range)>0) {
      for (jj in age_range) {
        
        gp_data_mixed_sex_age = gp_data_mixed[floor(gp_data_mixed[baseline.age.name])==jj & gp_data_mixed$sex==ii,]
        
        if (dim(gp_data_mixed_sex_age)[1]>0) { #Continue only if there is an individual with sex i and age j
          
          locf_sbp <- gp_data_mixed_sex_age[gp_data_mixed_sex_age$exp.type == sbp.risk.factor,]
          locf_sbp <- locf_sbp %>%
            group_by(patid) %>%
            arrange(baseline.age.name) %>%
            filter(row_number()==n()) %>% as.data.frame()
          locf_bpmed_use <- locf_sbp[c("patid","bpmed_use")]
          
          locf_tchol <- gp_data_mixed_sex_age[gp_data_mixed_sex_age$exp.type == tchol.risk.factor,]
          locf_tchol <- locf_tchol %>%
            group_by(patid) %>%
            arrange(baseline_age) %>%
            filter(row_number()==n()) %>% as.data.frame()
          locf_statin_use <- locf_tchol[c("patid","statin_use")]
          
          # PART 3
          #Load eHeart mixed model output (mixed_model_output.rdata)
          sex_pos<-ifelse(ii==sex_men,1,2)
          pos <- ((jj-39))*sex_pos
          fit<-mixed_model_output[[pos]]
          
          #mixoutsamp fit
          ranef<-mixoutsamp(model=fit, newdata=gp_data_mixed_sex_age,id.name="patid")$random
          
          #merge indicator variables for bpmed and statin to random effects
          ranef<-merge(ranef,locf_bpmed_use,by="patid",all.x=T)
          ranef<-merge(ranef,locf_statin_use,by="patid",all.x=T)
          
          #Fix NA to 0 for individuals without anti hypt or statins
          ranef$bpmed_use[is.na(ranef$bpmed_use)] <- 0
          ranef$statin_use[is.na(ranef$statin_use)] <- 0
          
          #fixed intercept + random intercept
          ranef[,"sbp_mixscaled"] = fit$coefficients$fixed["exp.typesbp"][[1]] + as.numeric(levels(ranef$exp.typesbp))[ranef$exp.typesbp] + ranef$bpmed_use*fit$coefficients$fixed["bpmed_use"][[1]]
          ranef[,"tchol_mixscaled"] = fit$coefficients$fixed["exp.typetchol"][[1]] + as.numeric(levels(ranef$exp.typetchol))[ranef$exp.typetchol] + ranef$statin_use*fit$coefficients$fixed["statin_use"][[1]]
          ranef[,"hdl_mixscaled"] = fit$coefficients$fixed["exp.typehdl"][[1]] + as.numeric(levels(ranef$exp.typehdl))[ranef$exp.typehdl]
          ranef[,"smk_mixed"] = fit$coefficients$fixed["exp.typesmokbin"][[1]] + as.numeric(levels(ranef$exp.typesmokbin))[ranef$exp.typesmokbin]
          
          #convert scaled values back
          standardise_sex <- standardise[standardise$sex == ii,]
          
          ranef[,"sbp_mixed"] =  ranef$sbp_mixscaled
          ranef[,"tchol_mixed"] =  ranef$tchol_mixscaled
          ranef[,"hdl_mixed"] =  ranef$hdl_mixscaled
          
          ranef<-ranef[,c("patid","sbp_mixed","tchol_mixed","hdl_mixed","smk_mixed")]
          
          ranef$sbp_mean = standardise_sex[standardise_sex$exp.type == sbp.risk.factor,]$mean_std
          ranef$sbp_sd = standardise_sex[standardise_sex$exp.type == sbp.risk.factor,]$sd_std
          ranef$hdl_mean = standardise_sex[standardise_sex$exp.type == hdl.risk.factor,]$mean_std
          ranef$hdl_sd = standardise_sex[standardise_sex$exp.type == hdl.risk.factor,]$sd_std
          ranef$tchol_mean = standardise_sex[standardise_sex$exp.type == tchol.risk.factor,]$mean_std
          ranef$tchol_sd = standardise_sex[standardise_sex$exp.type == tchol.risk.factor,]$sd_std
          
          results<-rbind(results,ranef)
        }
        print(paste0("Sex:",ii))
        print(paste0("Age:",jj))
        
      }
      
    }
  }
  
  ################### RETRIEVING LAST OBSERVED VALUES OF DIABETES, BP MEDICATION AND DIABETES  ###################
  risk.factors<-c(mixed.risk.factors,diabetes.risk.factor)
  
  #Restrict to specified risk factors only
  data3<- gp_data[gp_data[,exposure.name] %in% risk.factors,] 
  
  #tmp dataset with age and sex
  age_sex <- gp_data %>%
    group_by_at(id.name)  %>% 
    filter(row_number()==1) %>%
    as.data.frame()
  
  age_sex <- age_sex[c(id.name,sex.name,"baseline_age")]
  age_sex <- age_sex %>% rename(patid = all_of(id.name))
  
  
  
  #LOCF of diabetes and bpmed
  #Last observed value as a new "locf" variable
  
  gp_locf<-data3 %>%
    group_by_at(vars(id.name,exposure.name)) %>%
    mutate(locf = last(exposure_value))  %>% 
    filter(row_number()==1) %>%
    as.data.frame()
  
  gp_locf <- gp_locf[c(id.name,exposure.name,"locf")]
  
  #To ensure we have all locf variables present irrespective of data
  for (ii in risk.factors) {
    gp_locf <- rbind(gp_locf, c(999999999,ii,NA))
  }
  
  
  gp_locf<- subset(gp_locf,gp_locf[,exposure.name] == diabetes.risk.factor | gp_locf[,exposure.name] == anti_hypt.risk.factor | gp_locf[,exposure.name] == statins.risk.factor)
  
  #reshape from long to wide
  gp_locf_wide <- gp_locf %>% group_by_at(id.name) %>% 
    pivot_wider(names_from=exposure_type,values_from= locf,names_prefix = "locf_") %>% 
    as.data.frame()
  gp_locf_wide<-gp_locf_wide %>% rename(patid = all_of(id.name),
                                        bpmed = (locf_anti_hypt), dm = (locf_diabetes_bin))
  
  gp_locf_wide<-subset(gp_locf_wide,gp_locf_wide$patid != 999999999)
  #Merge baseline age, sex and locf variables with mixed model results
  eheartscreen <- merge(results,age_sex,by="patid",all.x=TRUE)
  eheartscreen <- merge(eheartscreen,gp_locf_wide,by="patid",all.x=TRUE)
  #Change binary risk factors from NA to 0.
  
  qrisk_factors <- c("bpmed","dm","locf_statins")
  for (gg in qrisk_factors) {
    eheartscreen[paste0(gg)][is.na(eheartscreen[paste0(gg)])] <- 0
    eheartscreen[paste0(gg)] <- as.numeric(unlist(eheartscreen[paste0(gg)]))
  }
  
  ############################################################################
  
  ################### CALCULATE 10 YEAR EHEART RISK ###################
  eheartscreen_10yr = NULL
  
  for (sex in c(sex_men,sex_women)) {
    for (age in sort(unique(floor(eheartscreen[,baseline.age.name])))) {
      
      HR_age<- HR[HR$sex == sex & HR$Age==age,]
      
      eheartscreen_age <- eheartscreen[eheartscreen$sex==sex & floor(eheartscreen$baseline_age)==age,]
      
      if (dim(eheartscreen_age)[1]>0) {
        eheartscreen_age$LP <- eheartscreen_age$sbp_mixed * log(HR_age$sbp) + 
          eheartscreen_age$tchol_mixed * log(HR_age$tchol) +
          eheartscreen_age$hdl_mixed * log(HR_age$hdl) +
          eheartscreen_age$smk_mixed * log(HR_age$smk) +
          eheartscreen_age$bpmed * log(HR_age$bpmed) +
          eheartscreen_age$dm * log(HR_age$dm)
        
        eheartscreen_age$risk <- 1-((HR_age$s0)^exp(eheartscreen_age$LP))
        eheartscreen_10yr = rbind(eheartscreen_10yr,eheartscreen_age)
      }
    }
  } 
  
  names(eheartscreen_10yr)[names(eheartscreen_10yr) == "patid"] <- id.name
  eheartscreen_10yr_report<- eheartscreen_10yr[c(id.name,"sex","baseline_age","risk","bpmed","locf_statins")]
  return(eheartscreen_10yr_report)
  
}

