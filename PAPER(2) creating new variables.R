#LifeCycle social inequalities in respiratory health study
#Anne Aurup and Angela Pinot de Moira
#Creating variables
#############################################################

#Load libraries

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

###############OUTCOME------------------------
#MeDALL
#ALSPAC - 7 years
#DNBC - a non-repeated variable (at 7 years)
#NINFEA - 7 years
#MOBA - a non-reapted variable (at 7 years) + a repeated variable at 7, but include the same information
#EDEN - 7 years

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

#create a numeric variable for poisson regression:
ds.asInteger('D$medall', 'poisson_medall', datasources = connections)
ds.cbind(x=c('D', 'poisson_medall'), newobj = 'D', datasources = connections)
ds.table(rvar="D$medall",cvar="D$poisson_medall", datasources = connections)



#CHECK:
ds.table('D$medall', datasources = connections[c(1,2,3,4,5,6)])
#To find the % of cases excluding NAs, using the option useNA= "no":
ds.table('D$medall', datasources = connections[c(1,2,3,4,5,6)], useNA= "no")
#prevalence of asthma: 5.9% in dnbc, 2.2% in ninfea, 17.6% in aslapc, 5.6% in moba, 7.3% in eden
#6.5 in GenR


datashield.workspace_save(connections, 'SES2')



#########################EXPOSURE --------------------------------------------
#Create a binary exposure variable (combine low and high education level)

ds.recodeLevels(x = "D$edu_m_.0", newCategories = c("1", "2", "2"), newobj = "bin_edu_m",
                datasources = connections)
ds.table("bin_edu_m", datasources = connections)

ds.cbind(x=c('D', 'bin_edu_m'), newobj = 'D', datasources = connections)

#####################################################################################################
#COVARIATES & MEDIATORS

##############################BREASTFEEDING-------------------------------------

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


########################## Mode of delivery - csection -------------------------

#NB not possible to code 0/1 so coded 1 2
ds.recodeValues(var.name = "D$mode_delivery", values2replace.vector = c(1,2,3,4,5),
                new.values.vector = c(1,1,2,2,2), force.output.format = "no",
                newobj = "csection", datasources = connections, notify.of.progress = FALSE)
ds.cbind(x=c('D', 'csection'), newobj = 'D', datasources = connections)
ds.table('D$csection', datasources=connections, useNA = "no")


##################EXPPOSURE TO PASSIVE SMOKING ---------------------------------

#checking pattern:
ds.table(rvar="D$smk_exp.0",cvar="D$smk_exp.1", useNA="no")

############1METHOD
#to integer:
ds.asInteger('D$smk_exp.0', 'int_smk_exp.0', datasources = connections)
ds.asInteger('D$smk_exp.1', 'int_smk_exp.1', datasources = connections)

#Creating new dataframe to calculate row means of
ds.dataFrame(x=c('int_smk_exp.0','int_smk_exp.1'), newobj='smk_expp', datasources = connections)

#Calculate the average across the dataframe:
ds.rowColCalc(x='smk_expp', operation='rowMeans', newobj='smk_exppmean', datasources=connections)

#boole
ds.Boole(V1 = 'smk_exppmean', V2 = "0", Boolean.operator = ">",
         numeric.output = TRUE, na.assign = "NA", newobj= 'passivesmoke2y', datasources = connections)

####CONVERT TO FACTOR VARIABLE:

ds.asFactor('passivesmoke2y', 'passivesmoke2y', forced.factor.levels=0:1, datasources = connections)
ds.cbind(x=c('D', 'passivesmoke2y'), newobj = 'D', datasources = connections[c(1,2,3,4,5,6)])
ds.table('D$passivesmoke2y', 'D$smk_exp.0', datasources=connections)
ds.table('D$passivesmoke2y', 'D$smk_exp.1', datasources=connections)

###SAVING
datashield.workspace_save(connections, 'SES2')



#############GESTATIONAL AGE ---------------------------------------------------
ds.assign(toAssign="D$ga_lmp", newobj='ga',datasources = connections['moba']) #MoBa don't have "ga_bj"
ds.assign(toAssign="D$ga_bj", newobj='ga',datasources = connections[c(1,2,3,5,6)])

#log GA
#Anne: changed from ga to ga_bj --> NB: MOBA DOES NOT HAVE GA_BJ, ask Angela which ga measure is best to use
ds.log(x = "D$ga_bj", newobj = "log_ga_bj", datasources = connections)
ds.log(x = "D$ga_lmp", newobj = "log_ga_bj", datasources = connections['moba'])

ds.cbind(x=c('D', 'log_ga_bj', 'ga' ), newobj = 'D', datasources = connections)

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

########### Making the ADVERSE REPRODUCTIVE OUTCOMES - common variable ---------

# From Maja:
#Adverse reproductive outcomes would include:
#- preterm birth (<37 weeks of gestation) here it is better to use GAbj variable as it the most complete one
#- low birth weight (<2500 grams)
#- cesarean.
#So you would have 1 if either of the three is 1 and 0 if all are zero. This also means that you 
#would need to exclude all those with missing data in at least one of the three variables.

######## 1) Making the preterm variable - <37 weeks of gestation 

#converting 37 weeks to days: 37*7 = 259 days:

ds.Boole(V1 ='D$ga', V2='259', Boolean.operator='<',
         numeric.output=T, na.assign='NA', newobj='preterm', datasources = connections[c(1,2,3,4,5,6)])

# check the mean GA in the two categories of preterm:
ds.meanSdGp(
  x = 'D$ga',
  y = 'preterm',
  type = "both",
  do.checks = FALSE,
  datasources = connections)


######## 2) Making low birth weight variable - <2500 grams 

ds.Boole(V1 ='D$birth_weight', V2='2500', Boolean.operator='<',
         numeric.output=T, na.assign='NA', newobj='lowbw', datasources = connections[c(1,2,3,4,5,6)])

# check the mean bw in the two categories of lowbw:
ds.meanSdGp(
  x = 'D$birth_weight',
  y = 'lowbw',
  type = "both",
  do.checks = FALSE,
  datasources = connections)
#looks fine


######## 3) Born by cesearan yes or no 


ds.asNumeric('D$mode_delivery', 'csection_new', datasources = connections[c(1,2,3,4,5,6)])

ds.Boole(V1 ='csection_new', V2='3', Boolean.operator='>=',
         numeric.output=T, na.assign='NA', newobj='csection_new', datasources = connections[c(1,2,3,4,5,6)])


######### FINAL STEP: Making of the combined adverse reproductive variable


ds.make(toAssign = "preterm + lowbw + csection_new", newobj = "adverse_rep_outcomes",
        datasources = connections[c(1,2,3,4,5,6)])


ds.Boole(V1 ='adverse_rep_outcomes', V2='1', Boolean.operator='>=',
         numeric.output=T, na.assign='NA', newobj='adverse_rep_outcomes', datasources = connections[c(1,2,3,4,5,6)])


#Now check the variable makes sense:
for (i in c('preterm', 'lowbw', 'D$csection')) {
  x = ds.table('adverse_rep_outcomes', i, datasources = connections[c(1,2,3,4,5,6)])
  print(x)
}

ds.cbind(x=c('D', 'adverse_rep_outcomes'), newobj = 'D', datasources = connections[c(1,2,3,4,5,6)])



####################################COVARIATES----------------------------------

##### ethnicity...
#check the ethnicity status in moba and eden....! how is the ethnic composition of these study populations..?
ds.table('D$ethn1_m', datasources=connections) # only eden has data
ds.table('D$ethn3_m', datasources=connections) #alspac and eden
ds.table('D$cob_m', datasources=connections) #ninfea and eden

#### parity...
#note:EDEN missing a lot of observations for sibling position
#INMA and NINFEA have small cells, so need to recode using ds.recodeValues initially, otherwise recode fails:
ds.recodeValues(var.name = "D$parity", values2replace.vector = c(0,1,2,3,4),
                new.values.vector = c(1,2,3,3,3), force.output.format = "no",
                newobj = "parity2", datasources = connections, notify.of.progress = FALSE)
ds.table("D$parity","parity2")
ds.cbind(x=c('D', 'parity2' ), newobj = 'D', datasources = connections)


datashield.workspace_save(connections, 'SES2')


ds.dataFrameFill(df.name="D", newobj="D", datasources = connections)


################# SAVING WORKSPACE #######################################

datashield.workspace_save(connections, 'SES2')

##########################################################################


