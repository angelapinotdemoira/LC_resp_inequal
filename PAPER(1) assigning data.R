library(DSMolgenisArmadillo)
library(DSI)
library(DSOpal)
library(dsBaseClient)

DSI::datashield.logout(connections)


#specify server url
armadillo_url_a <- "https://alspac-armadillo.molgenis.org"
# get ALSPAC token from central authentication server
token_a <- armadillo.get_token(armadillo_url_a)

# get EDEN token
armadillo_url_e <- "https://armadillo.sicopre.elfe-france.fr"
token_e <- armadillo.get_token(armadillo_url_e)
#you can use the same token for the 2 cohorts as they are on the same


#login data frame with key information to get on to the servers and validte user status:
#table="project"."table"


builder <- DSI::newDSLoginBuilder()
builder$append(server = "dnbc",  url = "https://opal.sund.ku.dk",
               user = "avaurup", password = "XXXX",
               table = "lc_dnbc_core_2_2.1_0_non_rep", driver = "OpalDriver")
builder$append(server = "ninfea",  url = "https://www.lifecycle.unito.it",
               user = "p21.copenhagen", password = "XXXXX",
               table = "lc_ninfea_core_2_1.p21_non_rep", driver = "OpalDriver")
builder$append(server = "alspac",
               url = armadillo_url_a,
               table = "lc20/2_1_core_1_3/non_rep",
               token = token_a,
               driver ="ArmadilloDriver")
builder$append(server = "moba",  url = "https://moba.nhn.no",
               user = "anne_aurup", password = "XXXX",
               table = "lc_moba_core_2_1.2_1_core_2021_7_non_rep_soc_ineq_resp_health", 
               driver = "OpalDriver")
builder$append(server ="eden",
               url = armadillo_url_e,
               token = token_e,
               table= "project28-eden/2_1_core_1_0/non_rep",
               driver="ArmadilloDriver")
builder$append(server = "genr",  url = "https://opal.erasmusmc.nl",
               user = "A.V.Aurup", password = "XXXXXX",
               table = "lc_genr_core_2_2.2_2_core_non_rep_APM_AVA_AKGJ__ECCNLC202159", driver = "OpalDriver")



logindata <- builder$build()

#The command to actually login and make the connection and what to call dataframe: D 
#and assigns stuff to the object 'connections':
connections <- DSI::datashield.login(logins = logindata, assign = F, symbol = "D")

datashield.tables(connections) # lists the tables that you have access to






### CREATE LIST OF NON REPEAT AND REPEAT VARIABLES --------

outcome_nonrep <- list ("child_id", "cohort_id", "mother_id", "variable_name",  
                        "asthma_current_MeDALL", "asthma_current_ISAAC", "asthma_ever_CHICOS", "asthma_ever_MeDALL",
                        "allergy_any_m", "allergy_inh_m", "eczema_m") 

outcome_rep <- list ("child_id", "cohort_id", "mother_id", "age_years", "ashtma_", "asthma_current_MeDALL_", "asthma_current_ISAAC_",
                     "asthma_ever_MeDALL", "FEV1_abs_", "FVC_abs_", "repro_", "FEV1_z_", "FVC_z_", "FEV1FVC_z_", "FeNO_",
                     "URTI_", "LRTI_", "whe_")

core_nonrep <- list ("child_id", "mother_id", "cohort_id", "ethn1_m", "ethn2_m",  "ethn3_m",  "eusilc_income_quintiles", "preg_smk", 
                     "agebirth_m_y", "sibling_pos","asthma_m", "asthma_bf", "sex",   "breastfed_excl", 
                     "breastfed_any", "breastfed_ever", "mode_delivery", "birth_weight", "ga_bj", "ga_lmp", "ga_us", "ga_mr", 
                     "cats_preg", "dogs_preg", "cats_quant_preg", "dogs_quant_preg", "eusilc_income", "green_dist_preg", "green_dist_birth",
                     "green_size_preg", "green_size_birth", "greenyn300_preg", "greenyn300_birth", "no2_preg", "pm10_preg", "pm25_preg",
                     "abroad_child", "ethn1_p", "ethn2_p", "ethn3_p", "smk_p", "outcome", "plurality", "abroad_mo",
                     "cob_m", "parity_m") 

core_yearlyrep <- list ("child_id", "cohort_id", "mother_id", "age_years", "smk_exp", "edu_m_", "hhincome_",
                        "famsize_child", "green_dist_", "green_size_", "greenyn300_", "childcarecentre_",
                        "childcareprof_", "cats_", "dogs_", "cats_quant_", "dogs_quant_", "no2_", "pm10_", "pm25_")  





## ---- Make list of opal table names for each cohort --------------------------

tables <- list(
  ninfea = data.frame(
    server = "ninfea",
    outcome_nonrep = "lc_ninfea_outcome_1_1.p21_non_rep",
    outcome_rep = "lc_ninfea_outcome_1_1.p21_yearly_rep",
    core_nonrep = "lc_ninfea_core_2_1.p21_non_rep",
    core_yearlyrep = "lc_ninfea_core_2_1.p21_yearly_rep",
    stringsAsFactors = FALSE),
  alspac = data.frame(
    server = "alspac",
    outcome_nonrep = "lc20/1_1_outcome_1_3/non_rep",
    outcome_rep = "lc20/1_1_outcome_1_3/yearly_rep",
    core_nonrep = "lc20/2_1_core_1_3/non_rep",
    core_yearlyrep = "lc20/2_1_core_1_3/yearly_rep",
    stringsAsFactors = FALSE),
  dnbc = data.frame(
    server = "dnbc",
    outcome_nonrep = "lc_dnbc_outcome_1_2.1_0_non_rep",
    outcome_rep = "lc_dnbc_outcome_1_2.1_0_yearly_rep",
    core_nonrep = "lc_dnbc_core_2_2.1_0_non_rep",
    core_yearlyrep = "lc_dnbc_core_2_2.1_0_yearly_rep",
    stringsAsFactors = FALSE),
  moba = data.frame(
    server = "moba",
    outcome_nonrep = "lc_moba_outcome_1_1.1_1_outcome_2021_2_non_rep_soc_ineq_resp_health",
    outcome_rep = "lc_moba_outcome_1_1.\t 1_1_outcome_2021_2_yearly_rep_soc_ineq_resp_health",
    core_nonrep = "lc_moba_core_2_1.2_1_core_2021_7_non_rep_soc_ineq_resp_health",
    core_yearlyrep = "lc_moba_core_2_1.2_1_core_2021_7_yearly_rep_soc_ineq_resp_health",
    stringsAsFactors = FALSE),
  eden = data.frame(
    server = "eden",
    outcome_nonrep = "project28-eden/1_1_outcome_1_0/non_rep",
    outcome_rep = "project28-eden/1_1_outcome_1_0/yearly_rep",
    core_nonrep = "project28-eden/2_1_core_1_0/non_rep",
    core_yearlyrep = "project28-eden/2_1_core_1_0/yearly_rep",
    stringsAsFactors = FALSE),
  genr = data.frame(
    server = "genr",
    outcome_nonrep = "lc_genr_outcome_1_2.1_2_outcome_non_rep_APM_AVA_AKGJ__ECCNLC202159",
    outcome_rep = "lc_genr_outcome_1_2.1_2_outcome_yearly_rep_APM_AVA_AKGJ__ECCNLC202159",
    core_nonrep = "lc_genr_core_2_2.2_2_core_non_rep_APM_AVA_AKGJ__ECCNLC202159",
    core_yearlyrep = "lc_genr_core_2_2.2_2_core_yearly_rep_APM_AVA_AKGJ__ECCNLC202159",
    stringsAsFactors = FALSE)  
)


##---Now assign data------------------------------
#(here data frames are created which contain the variables listed above)
#creatng new dataframes:

## Outcome non-rep
sapply(tables, function(x){
  
  datashield.assign(
    conns = connections[x[, "server"]], 
    symbol = "outcome_nonrep", 
    value = x[, "outcome_nonrep"], 
    variables = outcome_nonrep)
})


## Outcome rep  
sapply(tables, function(x){
  
  datashield.assign(
    conns = connections[x[, "server"]], 
    symbol = "outcome_rep", 
    value = x[, "outcome_rep"], 
    variables = outcome_rep)
})

## Core non-rep  
sapply(tables, function(x){
  
  datashield.assign(
    conns = connections[x[, "server"]], 
    symbol = "core_nonrep", 
    value = x[, "core_nonrep"], 
    variables = core_nonrep)
})

## Core yearly rep 
sapply(tables, function(x){
  
  datashield.assign(
    conns = connections[x[, "server"]], 
    symbol = "core_yearlyrep", 
    value = x[, "core_yearlyrep"], 
    variables = core_yearlyrep)
})


#check presence of data frames:
ds.ls()


#CHECKING WHETHER DATA HAS BEEN ASSIGNED:
ds.table("core_nonrep$sex", datasources = connections) #odd
ds.table("core_nonrep$sex", datasources = connections, useNA = "no") #looks good now!!

#checking plurality variable....:!
ds.table("core_nonrep$plurality", datasources = connections, useNA = "no")
# all 5 cohorts have the plurality variable!

#checking the outcome variable....:
ds.table("core_nonrep$outcome", datasources = connections, useNA = "no")
#moba does not have. cant show distribution for ninfea and eden due to low number of obs

#checking parity in alspac & moba:
ds.table("core_nonrep$parity_m", datasources = connections, useNA = "no")
#parity is now there in alspac % moba!!:-)



##--------Fill in each data frame so that each cohort has the same variables------
#Creates missing values columns in the server-side
#ds.dataFrameFill = This function checks if the input data frames have the same variables,
#(i.e. the same column names) in all of the used studies. When a study does not have some
#of the variables, the function generates those variables as vectors of missing values and
#combines them as columns to the input data frame.

ds.dataFrameFill(df.name="core_nonrep", newobj="core_nonrep", datasources = connections)
ds.dataFrameFill(df.name="core_yearlyrep", newobj="core_yearlyrep", datasources = connections)

ds.dataFrameFill(df.name="outcome_nonrep", newobj="outcome_nonrep", datasources = connections)
ds.dataFrameFill(df.name="outcome_rep", newobj="outcome_rep", datasources = connections)

#Angela comment: I do this step because sometimes the cohorts don’t have a variable and
#so haven’t assigned me access to that variable. If you take a look at the variables that
#you think some cohorts shouldn’t have, you’ll probably find that they’re missing


datashield.workspace_save(connections, 'dataframes') 
## save the workspace (this saves the data frames and objects created on each server)

ds.ls() # check what data frames and objects are present




#checking parity after dataframeFill
ds.table("core_nonrep$parity_m", datasources = connections)
ds.table("core_nonrep$parity_m", datasources = connections, useNA = "no")
#looks good - all sources have data


################Reshaping data ---------------------------------------------------
#(all data are in long format and need to be reshaped to wide so that we can join/merge the data frames)

#1) Exposure data
# Subset data so that only <4 years are included, core yearly repeated measures:
ds.dataFrameSubset(df.name = 'core_yearlyrep',
                   V1.name = 'core_yearlyrep$age_years',
                   V2.name = '3', # limit to 0-<4years
                   Boolean.operator = "<=",
                   keep.cols = NULL, 
                   rm.cols = NULL, 
                   keep.NAs = T, 
                   newobj = 'core_yearlyrep_3years',
                   datasources = connections,
                   notify.of.progress = FALSE)


ds.reShape(
  data.name='core_yearlyrep_3years',
  timevar.name = 'age_years',
  idvar.name = 'child_id',
  # v.names=c("pets_", "cats_",  "dogs_", "dogs_quant_", "cats_quant_"), 
  # v.names=c("cats_",  "dogs_", "dogs_quant_", "cats_quant_", "pets_quant"), 
  direction = 'wide', 
  newobj = "core_yearlyrep_wide",
  datasources = connections
)

#check:
ds.colnames("core_yearlyrep_wide")
ds.summary('core_yearlyrep_wide', datasources = connections)

#Do this when working on several cohorts..:
ds.dataFrameFill(df.name="core_yearlyrep_wide", newobj="core_yearlyrep_wide", datasources = connections)
#Error: The dataframes have the same variables. There are no missing variables to fill!


# 4) Rep outcome data 
#Remember: Outcome data is still in long format!:

###############ASK ANGELA ABOUT THIS

#Recode variable so that all non-missing values=1:
ds.Boole(V1 = ('outcome_rep$asthma_current_MeDALL_'), V2 = "0", Boolean.operator = ">=",
         numeric.output = TRUE, na.assign = "NA", newobj = 'asthma_current_nonmiss',
         datasources = connections[c(2,3,5)]) # only alspac, ninfea, eden (cohorts w repeated measuremnts)

#Multiply by age variable // = age of asthma assessment
ds.make(toAssign = "outcome_rep$age_years*asthma_current_nonmiss",
        newobj = "age_asthma_current", datasources = connections[c(2,3,5)]) # only alspac, ninfea, eden

#cbind them to outcome rep dataframe
ds.cbind(x=c('outcome_rep', 'age_asthma_current'), newobj = 'outcome_rep', datasources = connections[c(2,3,5)])

#check
ds.colnames("outcome_rep", datasources = connections[c(2,3,5)]) # only alspac, ninfea, eden
#ds.colnames("outcome_nonrep")

#creates variable in moba, dnbc and genr, so the df's look exactly the same in all 5 cohorts:
ds.dataFrameFill(df.name="outcome_rep", newobj="outcome_rep", datasources = connections)


#reshaping outcome_rep ( from long to wide, outcome measures are still in long format until now..)
ds.reShape(
  data.name='outcome_rep',
  timevar.name = 'age_years',
  idvar.name = 'child_id',
  v.names=c("asthma_current_MeDALL_", "age_asthma_current", "whe_"),
  direction = 'wide', 
  newobj = "outcome_wide",
  datasources = connections # doing this in all the 6 cohorts!
) 

ds.summary('outcome_wide', datasources = connections)

ds.dataFrameFill(df.name="outcome_wide", newobj="outcome_wide", datasources = connections) #do for all 6

datashield.workspace_save(connections, 'incl_outcome_wide')




### MERGE DATA FRAMES  -----------------------------
#now we have on line per child in all df, so we can merge....:

# NON-REPEATED EXPOSURE/COVARIATE DATA WITH NON-REPEATED OUTCOME DATA

ds.merge(
  x.name = 'core_nonrep', 
  y.name = 'outcome_nonrep', 
  by.x.names = 'child_id',
  by.y.names = 'child_id', 
  newobj = 'D',
  datasources = connections
)

ds.summary("D", datasources = connections) 



### WITH RESHAPED REPEATED DATA ----------
ds.merge(
  x.name = 'core_yearlyrep_wide', 
  y.name = 'D', 
  by.x.names = 'child_id',
  by.y.names = 'child_id', 
  newobj = 'D',
  datasources = connections
)

ds.summary("D", datasources = connections)


#(DNBC & MoBa only have one asthma measure...)
# WITH REPEATED MEASURE OUTCOME DATA

ds.merge(
  x.name = 'outcome_wide', 
  y.name = 'D', 
  by.x.names = 'child_id',
  by.y.names = 'child_id', 
  newobj = 'D',
  datasources = connections #including now all 6 cohorts!
)

ds.summary("D", datasources = connections)

ds.dataFrameFill(df.name="D", newobj="D", datasources = connections)

ds.colnames("D")



#SAVING:
datashield.workspace_save(connections, 'finaldataframes') 

connections <- DSI::datashield.login(logins = logindata, restore='finaldataframes')


### TESTING THE DISTRIBUTION OF MATERNAL EDUCATION VARIABLE 
ds.table("D$edu_m_.0", datasources = connections, useNA = "no") #looks fine

### TESTING THE WHEEZING VARIABLE IN THE NINFEA
ds.table("D$whe_.6", datasources = connections[2], useNA = "no") #5.7% prevalence at age 6 in crude study pop.
ds.table("D$whe_.6", datasources = connections[2]) # 48% missing in crude pop.

####testing parity
ds.table("D$parity_m", datasources = connections)




########## #check what variables are present ----------------------------

all_vars =ds.summary("D", datasources = connections)
all_vars = as.data.frame(lapply(X=all_vars,FUN = function(x){
  temp = sort(x[[4]])
}))


############## # check for missings --------------------------------------
#(this step creates tables summarizing the number of missing observations for each variable per cohort)

# Set studynames and numstudies
temp <- ds.summary('D$mother_id', datasources = connections)
study_names <- names(temp)
num_studies <- length(temp)
rm(temp)

#ds.length =  lenght of the variable column
fullNum = ds.length('D$child_id', type = 'split') 
fullNum

#####dnbc=96 825, ninfea=7 642, alspac=15 645, moba=105 751, eden=2002, genr=9901



################################list variables:
#Anne:
outcomes <- list("asthma_ever_CHICOS", "asthma_ever_MeDALL", "asthma_current_MeDALL", "asthma_current_MeDALL_.0",
                 "asthma_current_MeDALL_.1", "asthma_current_MeDALL_.2", "asthma_current_MeDALL_.3",
                 "asthma_current_MeDALL_.4", "asthma_current_MeDALL_.5", "asthma_current_MeDALL_.6",
                 "asthma_current_MeDALL_.7", "asthma_current_MeDALL_.8", "asthma_current_MeDALL_.9",
                 "asthma_current_MeDALL_.10", "asthma_current_MeDALL_.11", "asthma_current_MeDALL_.12",
                 "asthma_", "FEV1_abs_", "FVC_abs_", "repro_", "FEV1_z_", "FVC_z_", "FEV1FVC_z_", "FeNO_",
                 "URTI_", "LRTI_", "age_asthma_current.0", "age_asthma_current.1", "age_asthma_current.2",
                 "age_asthma_current.3", "age_asthma_current.4", "age_asthma_current.5",
                 "age_asthma_current.6", "age_asthma_current.7", "age_asthma_current.8",
                 "age_asthma_current.9", "age_asthma_current.10", "whe_.6")


exposures <- list("edu_m_.0", "edu_m_.1", "edu_m_.2", "edu_m_.3", "eusilc_income_quintiles", "eusilc_income",
                  "hhincome_.0", "hhincome_.1",
                  "hhincome_.2", "hhincome_.3")

covariates <- list("ethn1_m", "ethn2_m", "ethn3_m", "abroad_child", "ethn1_p", "ethn2_p", "ethn3_p",
                   "asthma_m", "asthma_bf", "sex", "age_years", "allergy_any_m", "allergy_inh_m",
                   "eczema_m", "outcome", "plurality", "parity_m")

mediators <- list("preg_smk", "agebirth_m_y", "sibling_pos", "breastfed_excl", "breastfed_any",
                  "breastfed_ever", "mode_delivery", "birth_weight", "ga_bj", "ga_lmp", "ga_us",
                  "ga_mr", "cats_preg", "dogs_preg", "cats_quant_preg", "dogs_quant_preg",
                  "green_dist_preg", "green_size_preg","greenyn300_preg", "no2_preg", "pm10_preg",
                  "pm25_preg","smk_p",
                  "smk_exp.0", "smk_exp.1", "smk_exp.2", "smk_exp.3", "famsize_child.0", "famsize_child.1",
                  "famsize_child.2", "famsize_child.3", "green_dist_.0", "green_dist_.1", "green_dist_.2",
                  "green_dist_.3", "green_size_.0", "green_size_.1", "green_size_.2", "green_size_.3",
                  "greenyn300_.0", "greenyn300_.1", "greenyn300_.2", "greenyn300_.3",
                  "childcarecentre_.0", "childcarecentre_.1", "childcarecentre_.2", "childcarecentre_.3",
                  "childcareprof_.0", "childcareprof_.1", "childcareprof_.2", "childcareprof_.3",
                  "cats_.0", "cats_.1", "cats_.2", "cats_.3", "dogs_.0", "dogs_.1", "dogs_.2", "dogs_.3",
                  "cats_quant_.0", "cats_quant_.1", "cats_quant_.2", "cats_quant_.3", "dogs_quant_.0",
                  "dogs_quant_.1", "dogs_quant_.2", "dogs_quant_.3", "no2_.0", "no2_.1", "no2_.2",
                  "no2_.3", "pm10_.0", "pm10_.1", "pm10_.2", "pm10_.3", "pm25_.0", "pm25_.1", "pm25_.2",
                  "pm25_.3")


#########################################################################################
#create the tables:
#Table 1 - outcomes
pre_missings_table1 = data.frame(cbind(fullNum))
#types_table = data.frame(cbind(study_names))

#'outcomes' in the line below refers to the variable list defined above
for (j in 1:length(outcomes)){
  print(paste0(j," start"))
  missing_vect = ds.numNA(paste0('D$',outcomes[j]))
  pre_missings_table1 = cbind(pre_missings_table1,unlist(missing_vect))
  #type_vect = ds.class(paste0('D$',outcomes[j]))
  #types_table = cbind(types_table,unlist(type_vect))
  print(paste0(j," end"))
}

#changing column names to variable names:
colnames(pre_missings_table1) <- c('Total in Study', outcomes)
#changing format:
pre_missings_table1 = t(pre_missings_table1)
pre_missings_table1 = as.data.frame(pre_missings_table1)
#NB: the output is the number of missing observations for each variable in the loop


#Table 2 - exposures
pre_missings_table2 = data.frame(cbind(fullNum))

for (j in 1:length(exposures)){
  print(paste0(j," start"))
  missing_vect = ds.numNA(paste0('D$',exposures[j]))
  pre_missings_table2 = cbind(pre_missings_table2,unlist(missing_vect))
  print(paste0(j," end"))
}

colnames(pre_missings_table2) <- c('Total in Study', exposures)
pre_missings_table2 = t(pre_missings_table2)
pre_missings_table2 = as.data.frame(pre_missings_table2)


#Table 3 - covariates (incl. outcome & plurality)
pre_missings_table3 = data.frame(cbind(fullNum))

for (j in 1:length(covariates)){
  print(paste0(j," start"))
  missing_vect = ds.numNA(paste0('D$',covariates[j]))
  pre_missings_table3 = cbind(pre_missings_table3,unlist(missing_vect))
  print(paste0(j," end"))
}

colnames(pre_missings_table3) <- c('Total in Study', covariates)
pre_missings_table3 = t(pre_missings_table3)
pre_missings_table3 = as.data.frame(pre_missings_table3)


#Table 4 - mediators
pre_missings_table4 = data.frame(cbind(fullNum))

for (j in 1:length(mediators)){
  print(paste0(j," start"))
  missing_vect = ds.numNA(paste0('D$',mediators[j]))
  pre_missings_table4 = cbind(pre_missings_table4,unlist(missing_vect))
  print(paste0(j," end"))
}

colnames(pre_missings_table4) <- c('Total in Study', mediators)
pre_missings_table4 = t(pre_missings_table4)
pre_missings_table4 = as.data.frame(pre_missings_table4)



##############################SAVING

datashield.workspace_save(connections, 'endofassigningdata') 


