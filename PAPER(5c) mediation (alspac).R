library(DSMolgenisArmadillo)
library(DSI)
library(DSOpal)
library(dsBaseClient)

library(purrr)
library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(remotes)
library(dsHelper)

install_github("datashield/dsMediationClient") #NB: newest version is now v0.0.3-dev! (Demetris at LC GA 19-05-22)
library(dsMediationClient)


#DSI::datashield.logout(connections)


#################################################################################

#specify server url
armadillo_url_a <- "https://alspac-armadillo.molgenis.org"
# get ALSPAC token from central authentication server
token_a <- armadillo.get_token(armadillo_url_a)



builder <- DSI::newDSLoginBuilder()
builder$append(server = "alspac",
               url = armadillo_url_a,
               table = "lc20/2_1_core_1_3/non_rep",
               token = token_a,
               driver ="ArmadilloDriver",
               profile = "mediation")


logindata <- builder$build()


connections <- DSI::datashield.login(logins = logindata, restore='flowchartall')


ds.ls()

############################################################################################


####### REMEMBER NEW FINAL DATAFRAME ------> Dmedall11

####### FINAL WORKSPACE --------> flowchartall


####################################################################################################

#checking final study populations
ds.summary("Dmedall11", datasources =  connections)


#3744 alspac



############################################################################################

#Making few additions before the analysis

############################################################################################



#converting outcome to integer to use in some Poisson models:
ds.asInteger('Dmedall11$medall', 'poisson_medall', datasources = connections)
ds.cbind(x=c('Dmedall11', 'poisson_medall'), newobj = 'Dmedall11', datasources = connections)
ds.table(rvar="Dmedall11$medall",cvar="Dmedall11$poisson_medall", datasources = connections)
#looks fine!





#THE ANALYSIS


############################# ALSPAC ###########################################



###################### MEDIATION ANALYSIS W. SMOKING DURNG PREGNANCY ###########

#Starting over, removing old saved objects:
#server-side object:
ds.rm("impFit.DS", datasources = connections)
#locally:
rm("impFit.DS")




#################################### STEP 1: FIT A WORKING MODEL ################################


######+SLMA:
#added combine.with.metafor = FALSE, the option for not combining with metafor
impFit.DS <- ds.glmSLMA(formula = 'poisson_medall ~ bin_edu_m + smokingduringpreg + agebirth_m_y + asthma_m + asthma_bf',
                        family = "poisson", dataName = "Dmedall11", newobj ="impFit.DS",
                        combine.with.metafor = FALSE, datasources = connections)
#see output
impFit.DS



###################### STEP 2: neImpute imputes the nested counterfactual outcomes #######################
ds.rm("expData", datasources = connections)
#locally:
rm("expData")


ds.neImpute(object = 'impFit.DS', newobj = 'expData', datasources = connections)

#check the names of the two imputed counterfactual outcomes
ds.colnames('expData', datasources = connections)
#bin_edu_m0 bin_edu_m1



########################## STEP 3: Fit natural effect model to the imputed dataset #######################
ds.rm("neMod.DS", datasources = connections)
#locally:
rm("neMod.DS")



neMod.DS <- ds.neModel(formula = 'poisson_medall ~ bin_edu_m0 + bin_edu_m1 + agebirth_m_y + asthma_m + asthma_bf',
                       family = "poisson", expData = "expData", se = "robust", newobj = "neMod.DS",
                       datasources = connections)

neMod.DS


#### neEffdemcomp-function to get effect estimates and CI (??? missing, talk to Demetris about this):
ds.rm("effdecomp", datasources = connections)
#locally:
rm("effdecomp")

effdecomp <- ds.neEffdecomp(model = "neMod.DS", datasources = connections)

effdecomp




################################################################################
###################### MEDIATION ANALYSIS W. 2 MEDIATORS #######################


#Starting over, removing old saved objects:
#server-side object:
ds.rm("impFit.DS", datasources = connections)
#locally:
rm("impFit.DS")



#################################### STEP 1: FIT A WORKING MODEL ################################


######+SLMA:
#added combine.with.metafor = FALSE, the option for not combining with metafor
impFit.DS <- ds.glmSLMA(formula = 'poisson_medall ~ bin_edu_m + smokingduringpreg * adverse_rep_outcomes + agebirth_m_y + asthma_m + asthma_bf',
                        family = "poisson", dataName = "Dmedall11", newobj ="impFit.DS",
                        combine.with.metafor = FALSE, datasources = connections)
impFit.DS




###################### STEP 2: neImpute imputes the nested counterfactual outcomes #######################
ds.rm("expData", datasources = connections)
#locally:
rm("expData")


ds.neImpute(object = 'impFit.DS', nMed = 2, newobj = 'expData', datasources = connections)


#check the names of the two imputed counterfactual outcomes
ds.colnames('expData', datasources = connections)
#bin_edu_m0 & bin_edu_m1


########################## STEP 3: Fit natural effect model to the imputed dataset #######################
ds.rm("neMod.DS", datasources = connections)
#locally:
rm("neMod.DS")


neMod.DS <- ds.neModel(formula = 'poisson_medall ~ bin_edu_m0 + bin_edu_m1 + agebirth_m_y + asthma_m + asthma_bf',
                       family = "poisson", expData = "expData", se = "robust", newobj = "neMod.DS",
                       datasources = connections)

neMod.DS


#### neEffdemcomp-function to get effect estimates and CI (??? missing, talk to Demetris about this):
ds.rm("effdecomp", datasources = connections)
#locally:
rm("effdecomp")

effdecomp <- ds.neEffdecomp(model = "neMod.DS", datasources = connections)

effdecomp





################################################################################
###################### MEDIATION ANALYSIS W. 3 MEDIATORS #######################


#Starting over, removing old saved objects:
#server-side object:
ds.rm("impFit.DS", datasources = connections)
#locally:
rm("impFit.DS")



#################################### STEP 1: FIT A WORKING MODEL ################################


######+SLMA:
#added combine.with.metafor = FALSE, the option for not combining with metafor
impFit.DS <- ds.glmSLMA(formula = 'poisson_medall ~ bin_edu_m + smokingduringpreg * adverse_rep_outcomes * breastfedcat + agebirth_m_y + asthma_m + asthma_bf',
                        family = "poisson", dataName = "Dmedall11", newobj ="impFit.DS",
                        combine.with.metafor = FALSE, datasources = connections)
#see output
impFit.DS




###################### STEP 2: neImpute imputes the nested counterfactual outcomes #######################
ds.rm("expData", datasources = connections)
#locally:
rm("expData")


ds.neImpute(object = 'impFit.DS', nMed = 3, newobj = 'expData', datasources = connections)


#check the names of the two imputed counterfactual outcomes
ds.colnames('expData', datasources = connections)
#bin_edu_m0 & bin_edu_m1


########################## STEP 3: Fit natural effect model to the imputed dataset #######################
ds.rm("neMod.DS", datasources = connections)
#locally:
rm("neMod.DS")


neMod.DS <- ds.neModel(formula = 'poisson_medall ~ bin_edu_m0 + bin_edu_m1 + agebirth_m_y + asthma_m + asthma_bf',
                       family = "poisson", expData = "expData", se = "robust", newobj = "neMod.DS",
                       datasources = connections)
neMod.DS


#### neEffdemcomp-function to get effect estimates and CI (??? missing, talk to Demetris about this):
ds.rm("effdecomp", datasources = connections)
#locally:
rm("effdecomp")

effdecomp <- ds.neEffdecomp(model = "neMod.DS", datasources = connections)

effdecomp





################################################################################
###################### MEDIATION ANALYSIS W. 4 MEDIATORS #######################


#Starting over, removing old saved objects:
#server-side object:
ds.rm("impFit.DS", datasources = connections)
#locally:
rm("impFit.DS")



#################################### STEP 1: FIT A WORKING MODEL ################################


######+SLMA:
#added combine.with.metafor = FALSE, the option for not combining with metafor
impFit.DS <- ds.glmSLMA(formula = 'poisson_medall ~ bin_edu_m + smokingduringpreg * adverse_rep_outcomes * breastfedcat * passivesmoke2y + agebirth_m_y + asthma_m + asthma_bf',
                        family = "poisson", dataName = "Dmedall11", newobj ="impFit.DS",
                        combine.with.metafor = FALSE, datasources = connections)
#see output
impFit.DS




###################### STEP 2: neImpute imputes the nested counterfactual outcomes #######################
ds.rm("expData", datasources = connections)
#locally:
rm("expData")


ds.neImpute(object = 'impFit.DS', nMed = 4, newobj = 'expData', datasources = connections)


#check the names of the two imputed counterfactual outcomes
ds.colnames('expData', datasources = connections)
#bin_edu_m0 & bin_edu_m1


########################## STEP 3: Fit natural effect model to the imputed dataset #######################
ds.rm("neMod.DS", datasources = connections)
#locally:
rm("neMod.DS")


neMod.DS <- ds.neModel(formula = 'poisson_medall ~ bin_edu_m0 + bin_edu_m1 + agebirth_m_y + asthma_m + asthma_bf',
                       family = "poisson", expData = "expData", se = "robust", newobj = "neMod.DS",
                       datasources = connections)

neMod.DS


#### neEffdemcomp-function to get effect estimates and CI (??? missing, talk to Demetris about this):
ds.rm("effdecomp", datasources = connections)
#locally:
rm("effdecomp")

effdecomp <- ds.neEffdecomp(model = "neMod.DS", datasources = connections)

effdecomp




