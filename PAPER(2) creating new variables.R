########################################################################################
#ASTHMA OUTCOMES --------------------------------------------------
#------------------------------------------------------------------
#MeDALL
#ALSPAC - 7 years
#DNBC - a non-repeated variable (at 7 years)
#NINFEA - 7 years
#MOBA - a non-reapted variable (at 7 years) + a repeated variable at 7, but include the same information
#EDEN - 7 years

library(DSMolgenisArmadillo)
library(DSI)
library(DSOpal)
library(dsBaseClient)
library(remotes)
#INSTALLING THE MEDIATION PACKAGE!:
#The mediation package:
#install_github("datashield/dsMediationClient", ref = "main")
#New install from Sido 16-11:
install_github("datashield/dsMediationClient", ref = "v0.0.2-dev")
library(dsMediationClient)


#specify server url
armadillo_url_a <- "https://alspac-armadillo.molgenis.org"
# get ALSPAC token from central authentication server
token_a <- armadillo.get_token(armadillo_url_a)

# get EDEN token
armadillo_url_e <- "https://armadillo.sicopre.elfe-france.fr"
token_e <- armadillo.get_token(armadillo_url_e)
#you can use the same token for the 2 cohorts as they are on the same



#NB: it dosent matter what table is put here...
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
               user = "A.V.Aurup", password = "XXXXX",
               table = "lc_genr_core_2_2.2_2_core_non_rep_APM_AVA_AKGJ__ECCNLC202159", driver = "OpalDriver")




logindata <- builder$build()

connections <- DSI::datashield.login(logins = logindata, restore='endofassigningdata') 



##########For the yearly-repeated variables, we need to restrict observations to one measurement per child
#This is a bit tricky and requires a couple of steps :
#JUMP TO LINE 48 FOR NOW AS YOU DON'T HAVE REPEATED MEASURES -> dnbc
#First we need to change the repeated measures to integer variables:
#for year 6- 9:
for (i in c(6:9)) {
  to_eval = paste0("ds.asInteger(x='D$asthma_current_MeDALL_.",i,"', newobj = 'medall",i,"', datasources = connections)")
  eval(parse(text=to_eval)) 
}

#Now we identify whether an observation is missing or not for each year (requires multiple steps):

#step 1:use ds.Boole for each medallX variable to return 1s for NA values and 0 otherwise
#Step 2: replace NAs with zero in the medallX variables
#c(999, 999, 999) because of initial 3 cohorts, replacing missing from NA to 999
#IN PAPER w 6 cohorts: c(999, 999, 999 ,999 ,999, 999):
for (i in c(6,7,8,9)) {
  eval(parse(text=paste0("ds.replaceNA(x = 'medall",i,"', forNA = c(999, 999, 999, 999, 999, 999), newobj = 'boole",i,"')")))
  eval(parse(text=paste0("ds.Boole(V1 = 'boole",i,"', V2 = '999', Boolean.operator = '==', numeric.output = TRUE, newobj = 'boole",i,"')")))
  eval(parse(text=paste0("ds.replaceNA(x = 'medall",i,"', forNA = c(0, 0, 0, 0, 0, 0), newobj = 'medall2_",i,"')"))) 
} #boolian variable: 1 if it is equal to 999, and 0 if it is not..

#step 3: multiply the four (one per medallX) outcomes that you get from the Boole functions 
ds.make(toAssign = 'boole6*boole7*boole8*boole9', newobj = 'medall_v') #if you have data you get 0, because NA's are 1!

#step 4: in vector produced in step3 recode 1s with NA and 0s with 1
#NOW only cohorts with repeated measures(?):
ds.recodeValues(var.name = 'medall_v',
                values2replace.vector = c(0,1),
                new.values.vector = c(1,NA),
                force.output.format = 'numeric',
                newobj = 'medall_v', datasources = connections[c(2,3,5)]) #a child with 1 has data (from 0 to 1)

#step 5: add the four recoded medallX variables using ds.make
#(note that here, the end product "medall" will be "0" if one measure=0 or if all measures are NA
ds.make(toAssign = 'medall2_6 + medall2_7 + medall2_8 + medall2_9', newobj = 'medall', datasources = connections[c(2,3,5)])#cohorts with rep measures


#step 6: multiply the vector that you get from step 5 with the vector that you get from step 3
#(this recode values that are 0 but truely NA, as NA)
ds.make(toAssign = 'medall*medall_v', newobj = 'medall', datasources = connections[c(2,3,5)]) #cohorts with rep measures


#CHECK VARIABLE:
ds.table('medall', datasources = connections[c(2,3,5)]) # check 
#eden has 1123 children out of 2002 wihtout asthma data..? Ask Angela about this

ds.table('medall', datasources = connections[c(2,3,5)], useNA = "no") # check
#of the children with asthma information at 7 years 7,3% have asthma in eden cohort - in raw study population



#Create the medall object in DNBC & MOBA & GENR (which have non-rep measures):
ds.assign(toAssign='D$asthma_current_MeDALL', newobj='medall', datasources = connections[c(1,4,6)])


#adding on medall column in D dataframe in all 5 cohorts
ds.cbind(x=c('D', 'medall'), newobj = 'D', datasources = connections[c(1,2,3,4,5,6)])


#CHECK:
ds.table('D$medall', datasources = connections[c(1,2,3,4,5,6)])
#To find the % of cases excluding NAs, using the option useNA= "no":
ds.table('D$medall', datasources = connections[c(1,2,3,4,5,6)], useNA= "no")
#prevalence of asthma: 5.9% in dnbc, 2.2% in ninfea, 17.6% in aslapc, 5.6% in moba, 7.3% in eden
#6.5 in GenR


datashield.workspace_save(connections, 'asthma')



#####################################################################################################
#EXPOSURE
ds.table('D$edu_m_.0', datasources=connections)
ds.table('D$edu_m_.0', datasources=connections, useNA = "no")
ds.class("D$edu_m_.0", datasources = connections)
#=factor variable!

###two way table#####
ds.table(rvar="D$edu_m_.0",cvar="D$medall", report.chisq.tests= TRUE, useNA = "no", datasources=connections)
#Message from study 5: [6] "Study5: Failed: at least one cell has a non-zero count less than nfilter.tab i.e. 3"
##... --> talk to Angela about this!
#this will probably be solved when we combine the exposure to a binary variable

#####################################################################################################
#COVARIATES & MEDIATORS

##############################BREASTFEEDING

#Breastfed_any - categorize the variable
#Anne: Breastfeeding is assessed as duration!
#Create a new variable with three categories: never, <6 months, >=6 months:
ds.Boole(V1 ='D$breastfed_any', V2='6', Boolean.operator='>=',
         numeric.output=T, na.assign='NA', newobj='breastfed6m', datasources = connections)
ds.asNumeric("D$breastfed_ever","bfever_n", datasources = connections)

#Add these up to create the new categorical variable:
ds.make(toAssign = "bfever_n + breastfed6m", newobj = "breastfedcat",
        datasources = connections)
ds.asFactor("breastfedcat", "breastfedcat", baseline.level = 2) #convert to a factor variable

ds.cbind(x=c('D', 'breastfedcat'), newobj = 'D', datasources = connections)
ds.table('D$breastfedcat', datasources=connections)


################################################
#mode of delivery - csection

#NB not possible to code 0/1 so coded 1 2
ds.recodeValues(var.name = "D$mode_delivery", values2replace.vector = c(1,2,3,4,5),
                new.values.vector = c(1,1,2,2,2), force.output.format = "no",
                newobj = "csection", datasources = connections, notify.of.progress = FALSE)
ds.cbind(x=c('D', 'csection'), newobj = 'D', datasources = connections)
ds.table('D$csection', datasources=connections, useNA = "no")

#################################################################

################## MATERNAL SMOKING DURING PREGNANCY
ds.table('D$preg_smk', datasources=connections)

ds.assign(toAssign='D$preg_smk', newobj='smokingduringpreg', datasources = connections)
ds.cbind(x=c('D', 'smokingduringpreg'), newobj = 'D', datasources = connections)
ds.table('D$smokingduringpreg', datasources=connections)
ds.table('D$smokingduringpreg', datasources=connections, useNA = "no")

ds.class("D$smokingduringpreg", datasources = connections)
#factor variable


##################EXPPOSURE TO PASSIVE SMOKING

#checking pattern:
ds.table(rvar="D$smk_exp.0",cvar="D$smk_exp.1", useNA="no")

############1METHOD
#to integer:
ds.asInteger('D$smk_exp.0', 'int_smk_exp.0', datasources = connections)
ds.cbind(x=c('D', 'int_smk_exp.0'), newobj = 'D', datasources = connections)
ds.asInteger('D$smk_exp.1', 'int_smk_exp.1', datasources = connections)
ds.cbind(x=c('D', 'int_smk_exp.1'), newobj = 'D', datasources = connections)


#Creating new dataframe to calculate row means of
ds.dataFrame(x=c('D$int_smk_exp.0','D$int_smk_exp.1'), newobj='smk_expp', datasources = connections)


#Calculate the average across the dataframe:
ds.rowColCalc(x='smk_expp', operation='rowMeans', newobj='smk_exppmean', datasources=connections)

#boole
ds.Boole(V1 = 'smk_exppmean', V2 = "0", Boolean.operator = ">",
         numeric.output = TRUE, na.assign = "NA", newobj= 'smk_exppmean', datasources = connections)

####CONVERT TO FACTOR VARIABLE:

ds.asFactor('smk_exppmean', 'passivesmoke2y', forced.factor.levels=0:1, datasources = connections)
ds.table('passivesmoke2y', datasources = connections, useNA="no")
ds.cbind(x=c('D', 'passivesmoke2y'), newobj = 'D', datasources = connections[c(1,2,3,4,5,6)])
ds.table('D$passivesmoke2y', datasources=connections)

###REMEMBER:
#From Angela e-mail 03-11-2021: Try this, but you might find that you end up getting NaN for the 
#missing variables if all the children have only one observation. If that’s the case, I would instead 
#combine the data in the same way you did to create your current asthma outcome (which is slightly more
#long winded!). Note, that using the first approach, if one or more cohorts have several FUs, their 
#smoking prevalence will be exaggerated because if a child is coded 1 NA, or 1 0, they will be coded 1 
#(if that makes sense)...

###SAVING
datashield.workspace_save(connections, 'asthmainclmediators')



#############GESTATIONAL AGE
#log GA
#Anne: changed from ga to ga_bj --> NB: MOBA DOES NOT HAVE GA_BJ, ask Angela which ga measure is best to use
ds.log(x = "D$ga_bj", newobj = "log_ga_bj", datasources = connections)
ds.cbind(x=c('D', 'log_ga_bj' ), newobj = 'D', datasources = connections)

ds.mean('D$ga_bj', datasources=connections[c(1,2,3,4,5,6)])
ds.mean('D$log_ga_bj', datasources=connections[c(1,2,3,4,5,6)])

ds.histogram("D$ga_bj", datasources = connections[1]) #dnbc
ds.histogram("D$ga_bj", datasources = connections[2]) #ninfea
ds.histogram("D$ga_bj", datasources = connections[3]) #alspac
ds.histogram("D$ga_bj", datasources = connections[5]) #eden



###############LOW BIRTH WEIGHT, log-transformation also....?
#birth_weight

ds.log(x = "D$birth_weight", newobj = "log_birth_weight", datasources = connections)
ds.cbind(x=c('D', 'log_birth_weight' ), newobj = 'D', datasources = connections)

ds.mean('D$birth_weight', datasources=connections[c(1,2,3,4,5,6)])
ds.mean('D$log_birth_weight', datasources=connections[c(1,2,3,4,5,6)])

ds.histogram("D$birth_weight", datasources = connections[1]) #dnbc
ds.histogram("D$birth_weight", datasources = connections[2]) #ninfea
ds.histogram("D$birth_weight", datasources = connections[3]) #alspac
ds.histogram("D$birth_weight", datasources = connections[5]) #eden


####################################COVARIATES##########################################
#####sex of child

ds.table('D$sex', datasources=connections, useNA = "no")

##### ethnicity...
#check the ethnicity status in moba and eden....! how is the ethnic composition of these study populations..?
ds.table('D$ethn1_m', datasources=connections) # only eden has data
ds.table('D$ethn3_m', datasources=connections) #alspac and eden
ds.table('D$cob_m', datasources=connections) #ninfea and eden


##### Parental history of asthma/allergy/atopy
ds.table('D$allergy_any_m', datasources=connections) #Maternal history of any allergy before pregnancy (of index child)
ds.table('D$allergy_inh_m', datasources=connections) #Maternal history of inhalant allergy before pregnancy (of index child)
ds.table('D$eczema_m', datasources=connections) #Maternal history of eczema before pregnancy (of index child)
ds.table('D$asthma_m', datasources=connections) #Maternal history of asthma before pregnancy
ds.table('D$asthma_bf', datasources=connections) #Paternal history of asthma (biological father)

##### Maternal age at birth
ds.mean('D$agebirth_m_y', datasources=connections[c(1,2,3,4,5,6)])


datashield.workspace_save(connections, 'asthmainclmediators_covar')


ds.dataFrameFill(df.name="D", newobj="D", datasources = connections)


################# SAVING WORKSPACE #######################################

datashield.workspace_save(connections, 'finalworkspace')

##########################################################################


