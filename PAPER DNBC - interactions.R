
library(DSMolgenisArmadillo)
library(DSI)
library(DSOpal)
library(dsBaseClient)
library(remotes)

install_github("datashield/dsMediationClient", ref = "v0.0.3-dev", force = TRUE) #NB: newest version is now v0.0.3-dev! (Demetris at LC GA 19-05-22)
library(dsMediationClient)


#################################################################################

#specify server url
armadillo_url_a <- "https://alspac-armadillo.molgenis.org"
# get ALSPAC token from central authentication server
token_a <- armadillo.get_token(armadillo_url_a)

# get EDEN token
armadillo_url_e <- "https://armadillo.sicopre.elfe-france.fr"
token_e <- armadillo.get_token(armadillo_url_e)
#you can use the same token for the 2 cohorts as they are on the same



builder <- DSI::newDSLoginBuilder()
builder$append(server = "dnbc",  url = "https://opal.sund.ku.dk",
               user = "avaurup", password = "ONd1qtCSpH",
               table = "lc_dnbc_core_2_2.1_0_non_rep", driver = "OpalDriver")
builder$append(server = "ninfea",  url = "https://www.lifecycle.unito.it",
               user = "p21.copenhagen", password = "941ty#47A!",
               table = "lc_ninfea_core_2_1.p21_non_rep", driver = "OpalDriver")
builder$append(server = "alspac",
               url = armadillo_url_a,
               table = "lc20/2_1_core_1_3/non_rep",
               token = token_a,
               driver ="ArmadilloDriver")
builder$append(server = "moba",  url = "https://moba.nhn.no",
               user = "anne_aurup", password = "123_socineqresp_123",
               table = "lc_moba_core_2_1.2_1_core_2021_7_non_rep_soc_ineq_resp_health", 
               driver = "OpalDriver")
builder$append(server ="eden",
               url = armadillo_url_e,
               token = token_e,
               table= "project28-eden/2_1_core_1_0/non_rep",
               driver="ArmadilloDriver")
builder$append(server = "genr",  url = "https://opal.erasmusmc.nl",
               user = "A.V.Aurup", password = "xK_4kf%%hNw!",
               table = "lc_genr_core_2_2.2_2_core_non_rep_APM_AVA_AKGJ__ECCNLC202159", driver = "OpalDriver")


logindata <- builder$build()


connections <- DSI::datashield.login(logins = logindata, restore='flowchartall')


ds.ls()

############################################################################################


####### REMEMBER NEW FINAL DATAFRAME ------> Dmedall11

####### FINAL WORKSPACE --------> flowchartall


####################################################################################################

#checking final study populations
ds.summary("Dmedall11", datasources =  connections)

#32383 dnbc
#3146 ninfea
#3744 alspac
#34390 moba
#793 eden
#1593 genr

############################################################################################


#age squared to use in dnbc model
ds.assign(toAssign="Dmedall11$agebirth_m_y*Dmedall11$agebirth_m_y", newobj='matagesqu', datasources=connections)
ds.cbind(x=c('Dmedall11', 'matagesqu'), newobj = 'Dmedall11', datasources = connections)




######################### EXAMINES POSSIBLE INTERACTIONS BETWEEN E AND EACH OF M'S ##############



################################################################################################

###################### MEDIATION ANALYSIS W. SMOKING DURNG PREGNANCY ###########################


#Starting over, removing old saved objects:
#server-side object:
ds.rm("impFit.DS", datasources = connections[1])
#locally:
rm("impFit.DS")

#####
#TRYING INTERAKTION BETWEEN E and maternal smoking during pregnancy:


#################################### STEP 1: FIT A WORKING MODEL ################################

#FIRST; converting outcome to integer to use in Poisson regression:
ds.asInteger('Dmedall11$medall', 'poisson_medall', datasources = connections)
ds.cbind(x=c('Dmedall11', 'poisson_medall'), newobj = 'Dmedall11', datasources = connections)
ds.table(rvar="Dmedall11$medall",cvar="Dmedall11$poisson_medall", datasources = connections)
#looks fine!


######+SLMA:
#added combine.with.metafor = FALSE, the option for not combining with metafor
impFit.DS <- ds.glmSLMA(formula = 'poisson_medall ~ bin_edu_m * smokingduringpreg + agebirth_m_y + matagesqu + asthma_m + asthma_bf',
                        family = "poisson", dataName = "Dmedall11", newobj ="impFit.DS",
                        combine.with.metafor = FALSE, datasources = connections[1])
#see output
impFit.DS
impFit.DS$output.summary$study1$coefficients




###################### STEP 2: neImpute imputes the nested counterfactual outcomes #######################
ds.rm("expData", datasources = connections[1])
#locally:
rm("expData")

ds.neImpute(object = 'impFit.DS', newobj = 'expData', datasources = connections[1])

#check the names of the two imputed counterfactual outcomes
ds.colnames('expData', datasources = connections[1])
#bin_edu_m0 & bin_edu_m1


########################## STEP 3: Fit natural effect model to the imputed dataset #######################
ds.rm("neMod.DS", datasources = connections[1])
#locally:
rm("neMod.DS")


#allows NIE to be different according to level of exposure
neMod.DS <- ds.neModel(formula = 'poisson_medall ~ bin_edu_m0 * bin_edu_m1 + agebirth_m_y + matagesqu + asthma_m + asthma_bf',
                       family = "poisson", expData = "expData", se = "robust", newobj = "neMod.DS",
                       datasources = connections[1])

neMod.DS
 #### INTERACTION TERM INSIGNIIFICANT: bin_edu_m02:bin_edu_m12 p = 1.266559e-0





################################################################################################

################################## ANALYSIS W. ADVERSE REPRODUCTIVE OUTCOMES ###########################


#Starting over, removing old saved objects:
#server-side object:
ds.rm("impFit.DS", datasources = connections[1])
#locally:
rm("impFit.DS")

#####
#TRYING INTERAKTION BETWEEN E OG M
#################################### STEP 1: FIT A WORKING MODEL ################################

######+SLMA:
#added combine.with.metafor = FALSE, the option for not combining with metafor
impFit.DS <- ds.glmSLMA(formula = 'poisson_medall ~ bin_edu_m * adverse_rep_outcomes + agebirth_m_y + matagesqu + asthma_m + asthma_bf',
                        family = "poisson", dataName = "Dmedall11", newobj ="impFit.DS",
                        combine.with.metafor = FALSE, datasources = connections[1])
#see output
impFit.DS
impFit.DS$output.summary$study1$coefficients




###################### STEP 2: neImpute imputes the nested counterfactual outcomes #######################
ds.rm("expData", datasources = connections[1])
#locally:
rm("expData")

ds.neImpute(object = 'impFit.DS', newobj = 'expData', datasources = connections[1])



########################## STEP 3: Fit natural effect model to the imputed dataset #######################
ds.rm("neMod.DS", datasources = connections[1])
#locally:
rm("neMod.DS")

#allows NIE to be different according to level of exposure
neMod.DS <- ds.neModel(formula = 'poisson_medall ~ bin_edu_m0 * bin_edu_m1 + agebirth_m_y + matagesqu + asthma_m + asthma_bf',
                       family = "poisson", expData = "expData", se = "robust", newobj = "neMod.DS",
                       datasources = connections[1])

neMod.DS
#### INTERACTION TERM INSIGNIIFICANT: bin_edu_m02:bin_edu_m12 p = 1.591632e-01





################################################################################################

########################################## ANALYSIS W. BREASTFEEDING ###########################


#Starting over, removing old saved objects:
#server-side object:
ds.rm("impFit.DS", datasources = connections[c(1,2,3)])
#locally:
rm("impFit.DS")

#####
#TRYING INTERAKTION BETWEEN E OG M
#################################### STEP 1: FIT A WORKING MODEL ################################ 

######+SLMA:
#added combine.with.metafor = FALSE, the option for not combining with metafor
impFit.DS <- ds.glmSLMA(formula = 'poisson_medall ~ bin_edu_m * breastfedcat + agebirth_m_y + matagesqu + asthma_m + asthma_bf',
                        family = "poisson", dataName = "Dmedall11", newobj ="impFit.DS",
                        combine.with.metafor = FALSE, datasources = connections[1])
#see output
impFit.DS
impFit.DS$output.summary$study1$coefficients




###################### STEP 2: neImpute imputes the nested counterfactual outcomes #######################
ds.rm("expData", datasources = connections[c(1,2,3)])
#locally:
rm("expData")

ds.neImpute(object = 'impFit.DS', newobj = 'expData', datasources = connections[1])



########################## STEP 3: Fit natural effect model to the imputed dataset #######################
ds.rm("neMod.DS", datasources = connections[1])
#locally:
rm("neMod.DS")

#allows NIE to be different according to level of exposure
neMod.DS <- ds.neModel(formula = 'poisson_medall ~ bin_edu_m0 * bin_edu_m1 + agebirth_m_y + matagesqu + asthma_m + asthma_bf',
                       family = "poisson", expData = "expData", se = "robust", newobj = "neMod.DS",
                       datasources = connections[1])

neMod.DS
#### INTERACTION TERM SIGNIIFICANT!!!!!: bin_edu_m02:bin_edu_m12 p = 1.160663e-02





################################################################################################

#################################### ANALYSIS W. PASSIVE SMOKING ###########################


#Starting over, removing old saved objects:
#server-side object:
ds.rm("impFit.DS", datasources = connections[1])
#locally:
rm("impFit.DS")

#####
#TRYING INTERAKTION BETWEEN E OG M
#################################### STEP 1: FIT A WORKING MODEL ################################
######+SLMA:
#added combine.with.metafor = FALSE, the option for not combining with metafor
impFit.DS <- ds.glmSLMA(formula = 'poisson_medall ~ bin_edu_m * passivesmoke2y + agebirth_m_y + matagesqu + asthma_m + asthma_bf',
                        family = "poisson", dataName = "Dmedall11", newobj ="impFit.DS",
                        combine.with.metafor = FALSE, datasources = connections[1])
#see output
impFit.DS
impFit.DS$output.summary$study1$coefficients




###################### STEP 2: neImpute imputes the nested counterfactual outcomes #######################
ds.rm("expData", datasources = connections[1])
#locally:
rm("expData")

ds.neImpute(object = 'impFit.DS', newobj = 'expData', datasources = connections[1])



########################## STEP 3: Fit natural effect model to the imputed dataset #######################
ds.rm("neMod.DS", datasources = connections[1])
#locally:
rm("neMod.DS")

#allows NIE to be different according to level of exposure
neMod.DS <- ds.neModel(formula = 'poisson_medall ~ bin_edu_m0 * bin_edu_m1 + agebirth_m_y + matagesqu + asthma_m + asthma_bf',
                       family = "poisson", expData = "expData", se = "robust", newobj = "neMod.DS",
                       datasources = connections[1])

neMod.DS
#### INTERACTION TERM INSIGNIIFICANT: bin_edu_m02:bin_edu_m12 p = 3.453626e-01







