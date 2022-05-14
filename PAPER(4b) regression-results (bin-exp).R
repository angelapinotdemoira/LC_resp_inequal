######

library(DSMolgenisArmadillo)
library(DSI)
library(DSOpal)
library(dsBaseClient)
library(remotes)

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
#check the non-dislosure filters set for each cohort:
#ds.listDisclosureSettings()

#checking final study populations
ds.summary("Dmedall11", datasources =  connections)

#32383 dnbc
#3146 ninfea
#3744 alspac
#34390 moba
#793 eden
#1593 genr









##########################################################################################

#REGRESSION MODELS WITH  BINARY EXPOSURE VARIABLE

#Distribution of maternal education across cohorts:
ds.table('Dmedall11$bin_edu_m', datasources=connections)

######################################## unadjusted regression models

####################### POISSON REGRESSIONS:



##### ALSPAC

#unadjusted poisson:
fit2aslpac_poisson <- ds.glm(formula = medall ~ bin_edu_m, data = 'Dmedall11', family = 'poisson', 
                            datasources = connections[3])
#print estimate table:
fit2aslpac_poisson


#unadjusted logreg:
#fitaslpac_logreg <- ds.glm(formula = medall ~ edu_m_.0, data = 'Dmedall11', family = 'binomial', 
#                           datasources = connections[3])
#print estimate table:
#fitaslpac_logreg



##### DNBC

#unadjusted poisson:
#FIRST: converting outcome to integer to get poisson reg to work:
ds.asInteger('Dmedall11$medall', 'poisson_medall', datasources = connections[1])
ds.cbind(x=c('Dmedall11', 'poisson_medall'), newobj = 'Dmedall11', datasources = connections[1])
ds.table(rvar="Dmedall11$medall",cvar="Dmedall11$poisson_medall", datasources = connections[1])
#looks fine!

fit2dnbc_poisson <- ds.glm(formula = poisson_medall ~ bin_edu_m, data = 'Dmedall11', family = 'poisson', 
                          datasources = connections[1])
#print estimate table:
fit2dnbc_poisson


#unadjusted logreg:
#fitdnbc_logreg <- ds.glm(formula = medall ~ edu_m_.0, data = 'Dmedall11', family = 'binomial', 
#                           datasources = connections[1])
#print estimate table:
#fitdnbc_logreg




##### MOBA

#unadjusted poisson:
#FIRST: converting outcome to integar:
ds.asInteger('Dmedall11$medall', 'poisson_medall', datasources = connections[4])
ds.cbind(x=c('Dmedall11', 'poisson_medall'), newobj = 'Dmedall11', datasources = connections[4])
ds.table(rvar="Dmedall11$medall",cvar="Dmedall11$poisson_medall", datasources = connections[4])
#looks fine!

fit2moba_poisson <- ds.glm(formula = poisson_medall ~ bin_edu_m, data = 'Dmedall11', family = 'poisson', 
                          datasources = connections[4])
#print estimate table:
fit2moba_poisson

#unadjusted logreg:
#fitmoba_logreg <- ds.glm(formula = medall ~ edu_m_.0, data = 'Dmedall11', family = 'binomial', 
#                           datasources = connections[4])
#print estimate table:
#fitmoba_logreg





##### EDEN

#unadjusted poisson:
fit2eden_poisson <- ds.glm(formula = medall ~ bin_edu_m, data = 'Dmedall11', family = 'poisson', 
                          datasources = connections[5])
#print estimate table:
fit2eden_poisson


#unadjusted logreg:
#fiteden_logreg <- ds.glm(formula = medall ~ edu_m_.0, data = 'Dmedall11', family = 'binomial', 
#                           datasources = connections[5])
#print estimate table:
#fiteden_logreg




##### GEN R

#unadjusted poisson:
#FIRST: converting outcome to integer:
ds.asInteger('Dmedall11$medall', 'poisson_medall', datasources = connections[6])
ds.cbind(x=c('Dmedall11', 'poisson_medall'), newobj = 'Dmedall11', datasources = connections[6])
ds.table(rvar="Dmedall11$medall",cvar="Dmedall11$poisson_medall", datasources = connections[6])
#looks fine!

fit2genr_poisson <- ds.glm(formula = poisson_medall ~ bin_edu_m, data = 'Dmedall11', family = 'poisson', 
                          datasources = connections[6])
#print estimate table:
fit2genr_poisson

#unadjusted logreg:
#fitgenr_logreg <- ds.glm(formula = medall ~ edu_m_.0, data = 'Dmedall11', family = 'binomial', 
#                         datasources = connections[6])
#print estimate table:
#fitgenr_logreg




##### NINFEA

#unadjusted poisson:
fit2ninfea_poisson <- ds.glm(formula = medall ~ bin_edu_m, data = 'Dmedall11', family = 'poisson', 
                            datasources = connections[2])
#print estimate table:
fit2ninfea_poisson








############################# MULTI-VARIATE ANALYSES ###########################

#First, Creating squared and cubic terms of maternal age at birht, agebirth_m_y

###########SQUARED
ds.assign(toAssign="Dmedall11$agebirth_m_y*Dmedall11$agebirth_m_y", newobj='matagesqu', datasources=connections)
ds.mean("matagesqu", datasources = connections)

#bind to final dataframe:
ds.cbind(x=c('Dmedall11', 'matagesqu'), newobj = 'Dmedall11', datasources = connections)
ds.mean('Dmedall11$matagesqu', datasources=connections)


##########CUBIC
ds.assign(toAssign="Dmedall11$agebirth_m_y*Dmedall11$agebirth_m_y*Dmedall11$agebirth_m_y",
          newobj='matagecub', datasources=connections)
ds.mean("matagecub", datasources = connections)

#bind to final dataframe:
ds.cbind(x=c('Dmedall11', 'matagecub'), newobj = 'Dmedall11', datasources = connections)
ds.mean('Dmedall11$matagecub', datasources=connections)



######################################## ADJUSTED ANALYSIS

#First, linearity check

#alspac

#checking which form of maternal age at birth is the better fit
adjustedfit2alspac_cubic <- ds.glm(formula = medall ~ bin_edu_m + asthma_m + agebirth_m_y + matagesqu + matagecub,
                                  data = 'Dmedall11', family = 'poisson', datasources = connections[3])
adjustedfit2alspac_cubic
#P-value for cubic form = 0.812
#run model again now without the cubic form:
adjustedfit2alspac_squared <- ds.glm(formula = medall ~ bin_edu_m + asthma_m + agebirth_m_y + matagesqu,
                                    data = 'Dmedall11', family = 'poisson', datasources = connections[3])
adjustedfit2alspac_squared
#P-value for squ form = 0.799


#####FINAL MODEL WITH MATERNAL AGE AT BIRTH IS LINEAR COVARIATE DUE TO INSIGNIFICANT CUBIC AND SQUARED TERMS:

adjustedfit2alspac <- ds.glm(formula = medall ~ bin_edu_m + asthma_m + agebirth_m_y,
                            data = 'Dmedall11', family = 'poisson', datasources = connections[3])
adjustedfit2alspac






#DNBC
#checking which form of maternal age at birth is the better fit
adjustedfit2dnbc_cubic <- ds.glm(formula = medall ~ bin_edu_m + asthma_m + agebirth_m_y + matagesqu + matagecub,
                                data = 'Dmedall11', family = 'poisson', datasources = connections[1])
adjustedfit2dnbc_cubic
#P-vlaue for cubic form = 
#runs model again now without the cubic form
adjustedfit2dnbc_squared <- ds.glm(formula = medall ~ bin_edu_m + asthma_m + agebirth_m_y + matagesqu,
                                  data = 'Dmedall11', family = 'poisson', datasources = connections[1])
adjustedfit2dnbc_squared
#P-value for squ form = 



#####FINAL MODEL WITH MATERNAL AGE AT BIRTH as SQUARED-TERM

adjustedfit2dnbc <- ds.glm(formula = medall ~ bin_edu_m + asthma_m + agebirth_m_y + matagesqu,
                          data = 'Dmedall11', viewVarCov = TRUE, family = 'poisson', datasources = connections[1])
adjustedfit2dnbc




### NINFEA

#checking which form of maternal age at birth is the better fit
adjustedfit2ninfea_cubic <- ds.glm(formula = medall ~ bin_edu_m + asthma_m + agebirth_m_y + matagesqu + matagecub,
                                  data = 'Dmedall11', family = 'poisson', datasources = connections[2])
adjustedfit2ninfea_cubic
#P-vlaue for cubic form = 0.666

#runs model again now without the cubic form:
adjustedfit2ninfea_squared <- ds.glm(formula = medall ~ bin_edu_m + asthma_m + agebirth_m_y + matagesqu,
                                    data = 'Dmedall11', family = 'poisson', datasources = connections[2])
adjustedfit2ninfea_squared
#P-value for squ form = 0.2613


#####FINAL MODEL WITH MATERNAL AGE AT BIRTH as LINEAR term:

adjustedfit2ninfea <- ds.glm(formula = medall ~ bin_edu_m + asthma_m + agebirth_m_y,
                            data = 'Dmedall11', family = 'poisson', datasources = connections[2])
adjustedfit2ninfea






### EDEN

#checking which form of maternal age at birth is the better fit
adjustedfit2eden_cubic <- ds.glm(formula = medall ~ bin_edu_m + asthma_m + agebirth_m_y + matagesqu + matagecub,
                                data = 'Dmedall11', family = 'poisson', datasources = connections[5])
adjustedfit2eden_cubic
#P-vlaue for cubic form = 0.5996

#runs model again now without the cubic form
adjustedfit2eden_squared <- ds.glm(formula = medall ~ bin_edu_m + asthma_m + agebirth_m_y + matagesqu,
                                  data = 'Dmedall11', family = 'poisson', datasources = connections[5])
adjustedfit2eden_squared
#P-value for squ form = 0.986


#####FINAL MODEL WITH MATERNAL AGE AT BIRTH as LINEAR term:

adjustedfit2eden <- ds.glm(formula = medall ~ bin_edu_m + asthma_m + agebirth_m_y,
                          data = 'Dmedall11', family = 'poisson', datasources = connections[5])
adjustedfit2eden





### GEN R

#FIRST: converting outcome to integer:
ds.asInteger('Dmedall11$medall', 'poisson_medall', datasources = connections[6])
ds.cbind(x=c('Dmedall11', 'poisson_medall'), newobj = 'Dmedall11', datasources = connections[6])
ds.table(rvar="Dmedall11$medall",cvar="Dmedall11$poisson_medall", datasources = connections[6])
#looks fine!

#checking which form of maternal age at birth is the better fit
adjustedfit2genr_cubic <- ds.glm(formula = poisson_medall ~ bin_edu_m + asthma_m + agebirth_m_y + matagesqu + matagecub,
                                data = 'Dmedall11', family = 'poisson', datasources = connections[6])
adjustedfit2genr_cubic
#P-vlaue for cubic form = 0.483

#runs model again now without the cubic form:
adjustedfit2genr_squared <- ds.glm(formula = poisson_medall ~ bin_edu_m + asthma_m + agebirth_m_y + matagesqu,
                                  data = 'Dmedall11', family = 'poisson', datasources = connections[6])
adjustedfit2genr_squared
#P-value for squ form = 0.0093 !!!


#####FINAL MODEL WITH MATERNAL AGE AT BIRTH as SQUARED term:

adjustedfit2genr <- ds.glm(formula = poisson_medall ~ bin_edu_m + asthma_m + agebirth_m_y + matagesqu,
                          data = 'Dmedall11', family = 'poisson', datasources = connections[6])
adjustedfit2genr





### MOBA

#FIRST: converting outcome to integer:
ds.asInteger('Dmedall11$medall', 'poisson_medall', datasources = connections[4])
ds.cbind(x=c('Dmedall11', 'poisson_medall'), newobj = 'Dmedall11', datasources = connections[4])
ds.table(rvar="Dmedall11$medall",cvar="Dmedall11$poisson_medall", datasources = connections[4])
#looks fine!

#checking which form of maternal age at birth is the better fit
adjustedfit2moba_cubic <- ds.glm(formula = poisson_medall ~ bin_edu_m + asthma_m + agebirth_m_y + matagesqu + matagecub,
                                data = 'Dmedall11', family = 'poisson', datasources = connections[4])
adjustedfit2moba_cubic
#P-vlaue for cubic form = 0.434

#runs model again now without the cubic form:
adjustedfit2moba_squared <- ds.glm(formula = poisson_medall ~ bin_edu_m + asthma_m + agebirth_m_y + matagesqu,
                                  data = 'Dmedall11', family = 'poisson', datasources = connections[4])
adjustedfit2moba_squared
#P-value for squ form = 0.8229


#####FINAL MODEL WITH MATERNAL AGE AT BIRTH as LINEAR term:

adjustedfit2moba <- ds.glm(formula = poisson_medall ~ bin_edu_m + asthma_m + agebirth_m_y,
                          data = 'Dmedall11', family = 'poisson', datasources = connections[4])
adjustedfit2moba





