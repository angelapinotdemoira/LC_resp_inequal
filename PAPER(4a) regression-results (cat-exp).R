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



#########################################################################

#NB: NEW BINARY EXP VARIABLE: bin_edu_m (high (1) & medium + low (2))

########################################################################

#Distribution of maternal education across cohorts:
ds.table('Dmedall11$bin_edu_m', datasources=connections) #binary
ds.table('Dmedall11$edu_m_.0', datasources=connections) #categorical


#Distribution of childhood asthma across cohrots:
ds.table('Dmedall11$medall', datasources=connections)


#########################################################################

###two way tables#####

#TABLE ´X - binary exposure
ds.table(rvar="Dmedall11$bin_edu_m",cvar="Dmedall11$medall", report.chisq.tests= TRUE)

#TABLE X - categorical exposure
ds.table(rvar="Dmedall11$edu_m_.0",cvar="Dmedall11$medall", report.chisq.tests= TRUE)



########################################################################

#NB: did not edit this yet, as Angela might have a nice table R solution for the descriptive tables..:

#TABLE 1
#covariates
ds.mean("Dmedall11$agebirth_m_y", datasources = connections)
ds.histogram("Dmedall11$agebirth_m_y", datasources = connections)
ds.var("Dmedall11$agebirth_m_y", datasources = connections)

ds.table("Dmedall11$asthma_m", datasources = connections)

ds.table("Dmedall11$asthma_bf", datasources = connections)

#ethnicity
ds.table("Dmedall11$cob_m", datasources = connections[2])
ds.table("Dmedall11$ethn3_m", datasources = connections[3])


#mediators
ds.table("Dmedall11$smokingduringpreg", datasources = connections)
ds.table("Dmedall11$passivesmoke2y", datasources = connections)
ds.table("Dmedall11$breastfedcat", datasources = connections)
#ds.table("Dmedall11$csection", datasources = connections)


#outcome
ds.table("Dmedall11$medall", datasources = connections)

#exp
ds.table("Dmedall8$edu_m_.0", datasources = connections)


#Distribution of mediators across sep in alspac, dnc and ninfea, table 2
ds.table(rvar="Dmedall8$smokingduringpreg",cvar="Dmedall8$edu_m_.0", report.chisq.tests= TRUE, useNA = "no")
ds.table(rvar="Dmedall8$passivesmoke2y",cvar="Dmedall8$edu_m_.0", report.chisq.tests= TRUE, useNA = "no")
ds.table(rvar="Dmedall8$breastfedcat",cvar="Dmedall8$edu_m_.0", report.chisq.tests= TRUE, useNA = "no")
ds.table(rvar="Dmedall8$csection",cvar="Dmedall8$edu_m_.0", report.chisq.tests= TRUE, useNA = "no")

#Distribution of mediators across asthma in alspac, dnc and ninfea, table 2
ds.table(rvar="Dmedall8$smokingduringpreg",cvar="Dmedall8$medall", report.chisq.tests= TRUE, useNA = "no")
ds.table(rvar="Dmedall8$passivesmoke2y",cvar="Dmedall8$medall", report.chisq.tests= TRUE, useNA = "no")
ds.table(rvar="Dmedall8$breastfedcat",cvar="Dmedall8$medall", report.chisq.tests= TRUE, useNA = "no")
ds.table(rvar="Dmedall8$csection",cvar="Dmedall8$medall", report.chisq.tests= TRUE, useNA = "no")


##########################################################################################

#REGRESSION MODELS WITH CATEGORICAL EXPOSURE VARIABLE

######################################## unadjusted regression models

####################### POISSON REGRESSIONS:

##### ALSPAC

#unadjusted poisson:
fitaslpac_poisson <- ds.glm(formula = medall ~ edu_m_.0, data = 'Dmedall11', family = 'poisson', 
                            datasources = connections[3])
#print estimate table:
fitaslpac_poisson


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

fitdnbc_poisson <- ds.glm(formula = poisson_medall ~ edu_m_.0, data = 'Dmedall11', family = 'poisson', 
                          datasources = connections[1])
#print estimate table:
fitdnbc_poisson


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

fitmoba_poisson <- ds.glm(formula = poisson_medall ~ edu_m_.0, data = 'Dmedall11', family = 'poisson', 
                          datasources = connections[4])
#print estimate table:
fitmoba_poisson

#unadjusted logreg:
#fitmoba_logreg <- ds.glm(formula = medall ~ edu_m_.0, data = 'Dmedall11', family = 'binomial', 
#                           datasources = connections[4])
#print estimate table:
#fitmoba_logreg





##### EDEN

#unadjusted poisson:
fiteden_poisson <- ds.glm(formula = medall ~ edu_m_.0, data = 'Dmedall11', family = 'poisson', 
                          datasources = connections[5])
#print estimate table:
fiteden_poisson


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

fitgenr_poisson <- ds.glm(formula = poisson_medall ~ edu_m_.0, data = 'Dmedall11', family = 'poisson', 
                          datasources = connections[6])
#print estimate table:
fitgenr_poisson

#unadjusted logreg:
#fitgenr_logreg <- ds.glm(formula = medall ~ edu_m_.0, data = 'Dmedall11', family = 'binomial', 
#                         datasources = connections[6])
#print estimate table:
#fitgenr_logreg




##### NINFEA

#unadjusted poisson:
fitninfea_poisson <- ds.glm(formula = medall ~ edu_m_.0, data = 'Dmedall11', family = 'poisson', 
                          datasources = connections[2])
#print estimate table:
fitninfea_poisson







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
adjustedfitalspac_cubic <- ds.glm(formula = medall ~ edu_m_.0 + asthma_m + agebirth_m_y + matagesqu + matagecub,
                                  data = 'Dmedall11', family = 'poisson', datasources = connections[3])
adjustedfitalspac_cubic
#P-value for cubic form = 0.7233
#run model again now without the cubic form:
adjustedfitalspac_squared <- ds.glm(formula = medall ~ edu_m_.0 + asthma_m + agebirth_m_y + matagesqu,
                                    data = 'Dmedall11', family = 'poisson', datasources = connections[3])
adjustedfitalspac_squared
#P-value for squ form = 0.928


#####FINAL MODEL WITH MATERNAL AGE AT BIRTH IS LINEAR COVARIATE DUE TO INSIGNIFICANT CUBIC AND SQUARED TERMS:

adjustedfitalspac <- ds.glm(formula = medall ~ edu_m_.0 + asthma_m + agebirth_m_y,
                            data = 'Dmedall11', family = 'poisson', datasources = connections[3])
adjustedfitalspac



#CALCULATING OVERAL P-VALUE

adjustedfitalspac_lowref$coefficients[,1]

B_2aa <- adjustedfitalspac_lowref$coefficients[,1]

V_2aa <-adjustedfitalspac_lowref$VarCovMatrix

#MANUELT WALD-TEST:
#vi vil gerne tjekke at de 4 interaktionshypoteser er 0:
rr1aa<-c(0,1,0,0,0)
rr2aa<-c(0,0,1,0,0)


#combine with coefficient vector, check that the parameter estimates are the same!
c(rr1aa%*%B_2aa,rr2aa%*%B_2aa)

# do manually Wald, note that how we transpose now matters
#teststørrelsen:
tm_aa2<-t(c(rr1aa%*%B_2aa,rr2aa%*%B_2aa))%*%solve(rbind(rr1aa,rr2aa)%*%V_2aa%*%cbind(rr1aa,rr2aa))%*%c(rr1aa%*%B_2aa,rr2aa%*%B_2aa)
c(tm_aa2,1 - pchisq(tm_aa2, 2)) #tester teststørrelsen i en chi i anden tabel, med df=4
#c=hi i anden testsrørelsen, p-værdi


#### wald test for a joint hypothesis




#DNBC
#checking which form of maternal age at birth is the better fit
adjustedfitdnbc_cubic <- ds.glm(formula = medall ~ edu_m_.0 + asthma_m + agebirth_m_y + matagesqu + matagecub,
                                data = 'Dmedall11', family = 'poisson', datasources = connections[1])
adjustedfitdnbc_cubic
#P-vlaue for cubic form = 
#runs model again now without the cubic form
adjustedfitdnbc_squared <- ds.glm(formula = medall ~ edu_m_.0 + asthma_m + agebirth_m_y + matagesqu,
                                  data = 'Dmedall11', family = 'poisson', datasources = connections[1])
adjustedfitdnbc_squared
#P-value for squ form = 



#####FINAL MODEL WITH MATERNAL AGE AT BIRTH as SQUARED-TERM

adjustedfitdnbc <- ds.glm(formula = medall ~ edu_m_.0 + asthma_m + agebirth_m_y + matagesqu,
                          data = 'Dmedall11', viewVarCov = TRUE, family = 'poisson', datasources = connections[1])
adjustedfitdnbc



#CALCULATING OVERAL P-VALUE:

adjustedfitdnbc_lowref$coefficients[,1]

B_2bb <- adjustedfitdnbc_lowref$coefficients[,1]

V_2bb <-adjustedfitdnbc_lowref$VarCovMatrix

#MANUELT WALD-TEST:
#vi vil gerne tjekke at de 4 interaktionshypoteser er 0:
rr1bb<-c(0,1,0,0,0,0)
rr2bb<-c(0,0,1,0,0,0)


#combine with coefficient vector, check that the parameter estimates are the same!
c(rr1bb%*%B_2bb,rr2bb%*%B_2bb)

# do manually Wald, note that how we transpose now matters
#teststørrelsen:
tm_bb2<-t(c(rr1bb%*%B_2bb,rr2bb%*%B_2bb))%*%solve(rbind(rr1bb,rr2bb)%*%V_2bb%*%cbind(rr1bb,rr2bb))%*%c(rr1bb%*%B_2bb,rr2bb%*%B_2bb)
c(tm_bb2,1 - pchisq(tm_bb2, 2)) #tester teststørrelsen i en chi i anden tabel, med df=4
#c=hi i anden testsrørelsen, p-værdi


#### wald test for a joint hypothesis




### NINFEA

#checking which form of maternal age at birth is the better fit
adjustedfitninfea_cubic <- ds.glm(formula = medall ~ edu_m_.0 + asthma_m + agebirth_m_y + matagesqu + matagecub,
                                  data = 'Dmedall11', family = 'poisson', datasources = connections[2])
adjustedfitninfea_cubic
#P-vlaue for cubic form = 0.622
#runs model again now without the cubic form
adjustedfitninfea_squared <- ds.glm(formula = medall ~ edu_m_.0 + asthma_m + agebirth_m_y + matagesqu,
                                    data = 'Dmedall11', family = 'poisson', datasources = connections[2])
adjustedfitninfea_squared
#P-value for squ form = 0.301


#####FINAL MODEL WITH MATERNAL AGE AT BIRTH as LINEAR term:

adjustedfitninfea <- ds.glm(formula = medall ~ edu_m_.0 + asthma_m + agebirth_m_y,
                            data = 'Dmedall11', family = 'poisson', datasources = connections[2])
adjustedfitninfea



#CALCULATING OVERAL P-VALUE

adjustedfitninfea_lowref$coefficients[,1]

B_2nn <- adjustedfitninfea_lowref$coefficients[,1]

V_2nn <-adjustedfitninfea_lowref$VarCovMatrix

#MANUELT WALD-TEST:
#vi vil gerne tjekke at de 4 interaktionshypoteser er 0:
rr1nn<-c(0,1,0,0,0)
rr2nn<-c(0,0,1,0,0)


#combine with coefficient vector, check that the parameter estimates are the same!
c(rr1nn%*%B_2nn,rr2nn%*%B_2nn)

# do manually Wald, note that how we transpose now matters
#teststørrelsen:
tm_nn2<-t(c(rr1nn%*%B_2nn,rr2nn%*%B_2nn))%*%solve(rbind(rr1nn,rr2nn)%*%V_2nn%*%cbind(rr1nn,rr2nn))%*%c(rr1nn%*%B_2nn,rr2nn%*%B_2nn)
c(tm_nn2,1 - pchisq(tm_nn2, 2)) #tester teststørrelsen i en chi i anden tabel, med df=4
#c=hi i anden testsrørelsen, p-værdi


#### wald test for a joint hypothesis



### EDEN

#checking which form of maternal age at birth is the better fit
adjustedfiteden_cubic <- ds.glm(formula = medall ~ edu_m_.0 + asthma_m + agebirth_m_y + matagesqu + matagecub,
                                  data = 'Dmedall11', family = 'poisson', datasources = connections[5])
adjustedfiteden_cubic
#P-vlaue for cubic form = 0.6757
#runs model again now without the cubic form
adjustedfiteden_squared <- ds.glm(formula = medall ~ edu_m_.0 + asthma_m + agebirth_m_y + matagesqu,
                                    data = 'Dmedall11', family = 'poisson', datasources = connections[5])
adjustedfiteden_squared
#P-value for squ form = 0.9164


#####FINAL MODEL WITH MATERNAL AGE AT BIRTH as LINEAR term:

adjustedfiteden <- ds.glm(formula = medall ~ edu_m_.0 + asthma_m + agebirth_m_y,
                            data = 'Dmedall11', family = 'poisson', datasources = connections[5])
adjustedfiteden



#CALCULATING OVERAL P-VALUE ..:



### GEN R

#FIRST: converting outcome to integer:
ds.asInteger('Dmedall11$medall', 'poisson_medall', datasources = connections[6])
ds.cbind(x=c('Dmedall11', 'poisson_medall'), newobj = 'Dmedall11', datasources = connections[6])
ds.table(rvar="Dmedall11$medall",cvar="Dmedall11$poisson_medall", datasources = connections[6])
#looks fine!

#checking which form of maternal age at birth is the better fit
adjustedfitgenr_cubic <- ds.glm(formula = poisson_medall ~ edu_m_.0 + asthma_m + agebirth_m_y + matagesqu + matagecub,
                                data = 'Dmedall11', family = 'poisson', datasources = connections[6])
adjustedfitgenr_cubic
#P-vlaue for cubic form = 0.6917

#runs model again now without the cubic form:
adjustedfitgenr_squared <- ds.glm(formula = poisson_medall ~ edu_m_.0 + asthma_m + agebirth_m_y + matagesqu,
                                  data = 'Dmedall11', family = 'poisson', datasources = connections[6])
adjustedfitgenr_squared
#P-value for squ form = 0.0044 !!!


#####FINAL MODEL WITH MATERNAL AGE AT BIRTH as SQUARED term:

adjustedfitgenr <- ds.glm(formula = poisson_medall ~ edu_m_.0 + asthma_m + agebirth_m_y + matagesqu,
                          data = 'Dmedall11', family = 'poisson', datasources = connections[6])
adjustedfitgenr



#CALCULATING OVERAL P-VALUE ..:




### MOBA

#FIRST: converting outcome to integer:
ds.asInteger('Dmedall11$medall', 'poisson_medall', datasources = connections[4])
ds.cbind(x=c('Dmedall11', 'poisson_medall'), newobj = 'Dmedall11', datasources = connections[4])
ds.table(rvar="Dmedall11$medall",cvar="Dmedall11$poisson_medall", datasources = connections[4])
#looks fine!

#checking which form of maternal age at birth is the better fit
adjustedfitmoba_cubic <- ds.glm(formula = poisson_medall ~ edu_m_.0 + asthma_m + agebirth_m_y + matagesqu + matagecub,
                                data = 'Dmedall11', family = 'poisson', datasources = connections[4])
adjustedfitmoba_cubic
#P-vlaue for cubic form = 0.41597

#runs model again now without the cubic form:
adjustedfitmoba_squared <- ds.glm(formula = poisson_medall ~ edu_m_.0 + asthma_m + agebirth_m_y + matagesqu,
                                  data = 'Dmedall11', family = 'poisson', datasources = connections[4])
adjustedfitmoba_squared
#P-value for squ form = 0.8448


#####FINAL MODEL WITH MATERNAL AGE AT BIRTH as LINEAR term:

adjustedfitmoba <- ds.glm(formula = poisson_medall ~ edu_m_.0 + asthma_m + agebirth_m_y,
                          data = 'Dmedall11', family = 'poisson', datasources = connections[4])
adjustedfitmoba



#CALCULATING OVERALL P-VALUE.........:










