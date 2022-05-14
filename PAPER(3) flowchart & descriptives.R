

####################################################################################
#Source Tim's variables and other libraries:

library(purrr)
library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(remotes)
#install_github("lifecycle-project/ds-helper", force=TRUE)
library(dsHelper)

#setwd("/home/angela/angela/WP1 paper")
#source("Tims_getStats.R")


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


connections <- DSI::datashield.login(logins = logindata, restore='finalworkspace')


ds.ls()

ds.summary("D", datasources =  connections[c(1,2,3,4,5,6)])



########################################################################################
#Flow chart
########################################################################################
#Start of FLOWCHART



ds.dim("D", datasources = connections[c(1,2,3,4,5,6)])
#DNBC 96 825 --> Live-borns 

#NINFEA 7642 --> From Maja: In Opal we included all pregnant women who were recruited 
#independently of their first follow-up status.
#A part of them in fact is either lost to the first follow-up (6 months after delivery) or the children
#were not born alive. 
#We have the variable outcome, but only for some of them we know whether the child was born alive or not. 

#ALSPAC 15 645 --> From Tim: 15,454 is the number of pregnancies, 
#and 15,645 is the number of foetuses from those pregnancies

#MOBA --> 105 751

#eden--> 2002

ds.table("D$outcome", datasources = connections[c(1,3)], useNA = "no")
#cannot produce results for ninfea because of low numbers
#The table looks odd for alspac!!! - very low number of observations... --> use useNA="no"
ds.table("D$plurality", datasources = connections, useNA = "no")


#checking the outcome - variable in moba
ds.colnames("D", datasources = connections[c(1,2,3,4)])
ds.table("D$outcome", datasources = connections[c(1,2,3,4)], useNA = "no")


ds.colnames("outcome_rep", datasources = connections[c(2,3)]) 

ds.colnames("core_nonrep", datasources = connections) 



########### remove non-live births -------------------------------------------
ds.table("D$outcome", datasources = connections[c(1,2,3,4,5,6)], useNA = "no")
#MoBa does not have outcome(?) --> use the variable “sex” as a proxy for outcome

ds.dataFrameSubset(
  df.name = "D",
  V1.name = "D$outcome",
  V2.name = "1",
  Boolean.operator = "==",
  newobj = "Dlb",
  datasources = connections[c(1,2,3,5,6)], #excl. moba
  notify.of.progress = FALSE
)

ds.dim("Dlb", datasources = connections[c(1,2,3,5,6)])
# dnbc 96 825
# ninfea 6905
# alspac 14 823
# eden 1900
# moba 89 315 (see sub-setting below)
# genr 9747


########### remove non-live births in Moba (sex as proxy for outcome)-----------
#checking sex distribution:
ds.table("D$sex", datasources = connections[c(1,2,3,4,5,6)], useNA = "no")
#89315 obs in moba with sex info


ds.recodeLevels(x = "D$sex",
                newCategories = c("1","1"),
                newobj = "sex2",
                datasources = connections[c(4)])
ds.cbind(x=c("D", "sex2"), newobj = "D", datasources = connections[c(4)])

ds.dataFrameSubset(
  df.name = "D",
  V1.name = "D$sex2",
  V2.name = "1",
  Boolean.operator = "==",
  newobj = "Dlb",
  datasources = connections[c(4)],
  notify.of.progress = FALSE
)


#ds.length =  lenght of the variable column
fullNum = ds.length('Dlb$child_id', type = 'split') 
fullNum
#89 315 liveborns in moba


#Note from Angela on the ninfea data: 
#I make the assumption that if they have outcome data they were live-born – which must be the case
#->It might be work double checking that there are no non-liveborn children with an outcome 
#(by cross-tabbing the two variables).
ds.table(rvar="D$outcome",cvar="D$medall", useNA = "no")
#There are no non-liveborn children whit an outcome!



######################### remove twins and other multiples------------------------------
ds.table("Dlb$plurality", datasources = connections[c(1,2,3,4,5,6)], useNA = "no") 

ds.dataFrameSubset(
  df.name = "Dlb",
  V1.name = "Dlb$plurality",
  V2.name = "1",
  Boolean.operator = "==",
  newobj = "Dplu",
  datasources = connections[c(1,2,3,4,5,6)], 
  notify.of.progress = FALSE
)

ds.dim("Dplu", datasources = connections[c(1,2,3,4,5,6)])
#92 660 dnbc (from Angela's flowchart = 92 660)
#6632 ninfea (from Angela's flowchart = 6 635)
#14 448 alspac (from Angela's flowchart = 14 448)
#86 361 moba
#1900 eden (no non-singletions)
#9504 in genr


###########################EXPOSURE DATA = EDUCATION at birth -> excl. children with no exposure data

ds.table('Dplu$edu_m_.0', datasources=connections[c(1,2,3,4,5,6)])
ds.table('Dplu$edu_m_.0', datasources=connections[c(1,2,3,4,5,6)], useNA = "no") 

ds.asInteger('Dplu$edu_m_.0', 'int_edu_m_.0', datasources = connections[c(1,2,3,4,5,6)])
ds.cbind(x=c('Dplu', 'int_edu_m_.0'), newobj = 'Dplu', datasources = connections[c(1,2,3,4,5,6)])
ds.summary("Dplu", datasources = connections[c(1,2,3,4,5,6)])
ds.table(rvar="Dplu$edu_m_.0",cvar="Dplu$int_edu_m_.0", datasources = connections[c(1,2,3,4,5,6)])#looks fine!

ds.dataFrameSubset(
  df.name = "Dplu",
  V1.name = "Dplu$int_edu_m_.0",
  V2.name = "0",
  Boolean.operator = ">",
  newobj = "Dexp",
  datasources = connections[c(1,2,3,4,5,6)],
  notify.of.progress = FALSE
)

ds.dim("Dexp", datasources = connections[c(1,2,3,4,5,6)])

# 80 654 dnbc
# 6 577 ninfea
# 12 070 alspac
# 81 161 moba
# 1884 eden
# 8332 genr


###############################OUTCOME DATA = childhood asthma -> excl. children with no asthma data 

ds.table('Dexp$medall', datasources=connections[c(1,2,3,4,5,6)])
ds.table('Dexp$medall', datasources=connections[c(1,2,3,4,5,6)], useNA = "no") 

ds.asInteger('Dexp$medall', 'int_medall', datasources = connections[c(1,2,3,4,5,6)])
ds.cbind(x=c('Dexp', 'int_medall'), newobj = 'Dexp', datasources = connections[c(1,2,3,4,5,6)])
ds.summary("Dexp", datasources = connections[c(1,2,3,4,5,6)])
ds.table(rvar="Dexp$medall",cvar="Dexp$int_medall", datasources = connections[c(1,2,3,4,5,6)])#looks fine!

ds.dataFrameSubset(
  df.name = "Dexp",
  V1.name = "Dexp$int_medall",
  V2.name = "0",
  Boolean.operator = ">=",
  newobj = "Dmedall",
  datasources = connections[c(1,2,3,4,5,6)], 
  notify.of.progress = FALSE
)

ds.dim("Dmedall", datasources = connections[c(1,2,3,4,5,6)])
#49 099 dnbc
#3 888 ninfea
#6 059 alspac
#48 000 moba
#876 eden
#4490 genr

ds.table('Dmedall$edu_m_.0', datasources=connections[c(1,2,3,4,5)])
ds.table('Dmedall$edu_m_.0', datasources=connections[c(1,2,3,4,5)], useNA = "no") 

##############################################################SAVING WORKSPACE

datashield.workspace_save(connections, 'flowchartasthma')

##############################################################RESTORE WORKSPACE

connections <- DSI::datashield.login(logins = logindata, restore='flowchartasthma') 

#CHECKING THINGS...:
ds.table(rvar="Dmedall$medall",cvar="Dmedall$edu_m_.0", datasources = connections)


########################### - MEDIATORS -> excl. children with no mediator info
#smoking in preg
#passive smoking in first 2 years
#breastfeeding
#MoD

#####BREASTFEEDING, Dmedall

ds.table('Dmedall$breastfedcat', datasources=connections[c(1,2,3,4,5,6)]) 
# 16 057 have missing info on breastfeeding in dnbc
# 137 have missing info on breastfeeding in ninfea
# 253 have missing info on breatfeeding in alspac

ds.asInteger('Dmedall$breastfedcat', 'int_breastfedcat', datasources = connections[c(1,2,3,4,5,6)])
ds.cbind(x=c('Dmedall', 'int_breastfedcat'), newobj = 'Dmedall', datasources = connections[c(1,2,3,4,5,6)])
ds.summary("Dmedall", datasources = connections[c(1,2,3,4,5,6)])
ds.table(rvar="Dmedall$breastfedcat",cvar="Dmedall$int_breastfedcat", datasources = connections[c(1,2,3,4,5,6)])
#looks fine!

ds.dataFrameSubset(
  df.name = "Dmedall",
  V1.name = "Dmedall$int_breastfedcat",
  V2.name = "0",
  Boolean.operator = ">=",
  newobj = "Dmedall2",
  datasources = connections[c(1,2,3,4,5,6)],
  notify.of.progress = FALSE
)

ds.dim("Dmedall2", datasources = connections[c(1,2,3,4,5,6)])
#Left in study populations after adjusting for breastfeeding:
#dnbc = 33 042
#ninfea = 3751
#alspac = 5806
#moba = 48 000
#eden = 875
#genr = 3161

#####MODE OF DELIVERY
ds.table('Dmedall2$csection', datasources=connections[c(1,2,3,4,5,6)])
# 144 have missing info on mod in dnbc
# 113 have missing info on mod in ninfea
# 197 have missing info on mod in alspac

ds.asInteger('Dmedall2$csection', 'int_csection', datasources = connections[c(1,2,3,4,5,6)])
ds.cbind(x=c('Dmedall2', 'int_csection'), newobj = 'Dmedall2', datasources = connections[c(1,2,3,4,5,6)])
ds.summary("Dmedall2", datasources = connections[c(1,2,3,4,5,6)])
ds.table(rvar="Dmedall2$csection",cvar="Dmedall2$int_csection", datasources = connections[c(1,2,3,4,5,6)])
#looks fine!

ds.dataFrameSubset(
  df.name = "Dmedall2",
  V1.name = "Dmedall2$int_csection",
  V2.name = "0",
  Boolean.operator = ">=",
  newobj = "Dmedall3",
  datasources = connections[c(1,2,3,4,5,6)], 
  notify.of.progress = FALSE
)

ds.dim("Dmedall3", datasources = connections[c(1,2,3,4,5,6)])
#Left in study populations after adjusting for mod:
# in dnbc 32 898
# in ninfea 3 638
# in alspac 5609
# in moba 44 692
# in eden 798
# in genr 2872



#####SMOKING IN PREGNANCY, smokingduringpreg
ds.table('Dmedall3$smokingduringpreg', datasources=connections[c(1,2,3,4,5,6)]) 
# 195 have missing info in dnbc
# 25 have missing info in ninfea
# 431 have missing info in alspac

ds.asInteger('Dmedall3$smokingduringpreg', 'int_smokingduringpreg', datasources = connections[c(1,2,3,4,5,6)])
ds.cbind(x=c('Dmedall3', 'int_smokingduringpreg'), newobj = 'Dmedall3', datasources = connections[c(1,2,3,4,5,6)])
ds.summary("Dmedall3", datasources = connections[c(1,2,3,4,5,6)])
ds.table(rvar="Dmedall3$smokingduringpreg",cvar="Dmedall3$int_smokingduringpreg", datasources = connections[c(1,2,3,4,5,6)])
#looks fine!

ds.dataFrameSubset(
  df.name = "Dmedall3",
  V1.name = "Dmedall3$int_smokingduringpreg",
  V2.name = "0",
  Boolean.operator = ">=",
  newobj = "Dmedall4",
  datasources = connections[c(1,2,3,4,5,6)], 
  notify.of.progress = FALSE
)

ds.dim("Dmedall4", datasources = connections[c(1,2,3,4,5,6)])
#Left in study populations after adjusting for smokingduringpreg:
# 32 703 in dnbc
# 3 613 in ninfea
# 5 178 in alspac
# 44 692 in moba
# 796 in eden
# 2654 in genr


#####EXPOSURE TO PASSIVE SMOKING, passivesmoke2y
ds.table('Dmedall4$passivesmoke2y', datasources=connections[c(1,2,3,4,5,6)])
# 2 have missing info in dnbc
# 76 have missing info in ninfea
#2 have missing info in alspac

ds.asInteger('Dmedall4$passivesmoke2y', 'int_passivesmoke2y', datasources = connections[c(1,2,3,4,5,6)])
ds.cbind(x=c('Dmedall4', 'int_passivesmoke2y'), newobj = 'Dmedall4', datasources = connections[c(1,2,3,4,5,6)])
ds.summary("Dmedall4", datasources = connections[c(1,2,3,4,5,6)])
ds.table(rvar="Dmedall4$passivesmoke2y",cvar="Dmedall4$int_passivesmoke2y", datasources = connections[c(1,2,3,4,5,6)])
#looks fine!

ds.dataFrameSubset(
  df.name = "Dmedall4",
  V1.name = "Dmedall4$int_passivesmoke2y",
  V2.name = "0",
  Boolean.operator = ">=",
  newobj = "Dmedall5",
  datasources = connections[c(1,2,3,4,5,6)], 
  notify.of.progress = FALSE
)

ds.dim("Dmedall5", datasources = connections[c(1,2,3,4,5,6)])
#Left in study populations after adjusting for passive smoking:
#  32 701 in dnbc
#  3 537 in ninfea
#  5 176 in alspac
# 44 665 in moba
# 795 in eden
# 2123 in genr


##############################################################SAVING WORKSPACE

datashield.workspace_save(connections, 'flowchartmediators')

##############################################################RESTORE WORKSPACE

connections <- DSI::datashield.login(logins = logindata, restore='flowchartmediators') 





#######################################COVARIATES
# DONE // Sex of child
# DONE // Ethnicity
# DONE //Parental history of asthma/allergy/atopy // Genetically predisposed individuals...: Several variables
# DONE // Maternal age at birth
#Parity??????????


######maternal age at birth, agebirth_m_y
ds.mean("Dmedall5$agebirth_m_y", datasources = connections[c(1,2,3,4,5,6)])
#NB: Actually all children in ninfea and dnbc have this variable, but missing n=89 in alspac and 45 in moba

ds.dataFrameSubset(
  df.name = "Dmedall5",
  V1.name = "Dmedall5$agebirth_m_y",
  V2.name = "0",
  Boolean.operator = ">=",
  newobj = "Dmedall6",
  datasources = connections[c(1,2,3,4,5,6)], 
  notify.of.progress = FALSE
)

ds.dim("Dmedall6", datasources = connections[c(1,2,3,4,5,6)])
#Left in study populations after adjusting for maternal age at birth:
# 32 701 dnbc
# 3 537 ninfea
# 5 087 in alspac
# 44 620 in moba
# 794 in eden
# 2123 in genr


######Sex of the child, medall6
ds.table('Dmedall6$sex', datasources=connections[c(1,2,3,4,5,6)])
ds.table('Dmedall6$sex', datasources=connections[c(1,2,3,4,5,6)], useNA = "no")
#the same distribution
#there are no NA's!


##############ethnicity
#In Angelas manuscript adjusting for maternal ethnic background (white/Black, Asian or minority ethnic/mixed)
#in NINFEA: Country of birth (Italy/EU member state/other) used as a proxy for ethnicity; 96% of mothers were born in Italy.
ds.table("Dmedall6$cob_m", datasources = connections[2])
#There are no NA's!
ds.table("Dmedall6$cob_m", datasources = connections[2], useNA = "no")
#=Born in italy = 96,6%
#=Born in other EU country = 1,7%
#=Born in other country = 1,7%

#In DNBC:
#> 98% of mothers of white ethnic background
#comment from Angela on use of reference on this:  I think it's OK to put it as a personal communication
#(maybe citing Anne-Marie, the PI of the DNBC). You could also ask Anne-Marie, but I don't think she has one

#In ASLPAC:
#ALSPac has ethn3_m = Best estimate of mother's ethnic background (1=Western/2=Non-western/3=Mixed)
#Comment from Tim 25-11: Yep it’s pretty homogenous – vast majority white British. 
#I think the cohort profile (either ALSPAC or LifeCycle) should give you the exact breakdown?
#From Cohort Profile (Boyd) = 96% white ethnicity
ds.table("Dmedall6$ethn3_m", datasources = connections[3])
# 43 missing - 0,8%
ds.table("Dmedall6$ethn3_m", datasources = connections[3], useNA = "no")
# 4 987 are Western (1) - 98,9%
# 57 are non-western (2) - 1,1%
# 0 (3)

#subsetting only in alspac!!
#OBS: OVERWRITING Dmedall6:

#But first change from factor to integer!:
ds.asInteger('Dmedall6$ethn3_m', 'int_ethn3_m', datasources = connections[3])
ds.cbind(x=c('Dmedall6', 'int_ethn3_m'), newobj = 'Dmedall6', datasources = connections[3])
ds.summary("Dmedall6", datasources = connections[3])
ds.table(rvar="Dmedall6$ethn3_m",cvar="Dmedall6$int_ethn3_m", datasources = connections[3])
#looks fine!


ds.dim("Dmedall6", datasources = connections[c(1,2,3)])
#Left in study populations after adjusting for ethnicity in alspac only!:
# 32 701 dnbc
# 3537 ninfea
# 5087





#####PARENTAL HISTORY OF ASTHMA - Genetically predisposed individuals
#Variables from Angela’s manuscript // Angela adjusts for this in her analyses: 
#- Doctor diagnosed maternal asthma before pregnancy (yes/no)
#- Doctor diagnosed paternal asthma before pregnancy (yes/no)
#- Doctor diagnosed maternal inhalant allergy (yes/no)

#From Chapter 5 - "A parental history of asthma and patient history of eczema or allergic 
#sensitization have been identified as risk factors for asthma in children."


####MATERNAL HISTORY OF ASTHMA
ds.table('Dmedall6$asthma_m', datasources=connections[c(1,2,3,4,5,6)])
#  17 have missing info in dnbc
# 60  have missing info in ninfea
# 80 have missing info in alspac
# 76 missing in genr

ds.asInteger('Dmedall6$asthma_m', 'int_asthma_m', datasources = connections[c(1,2,3,4,5,6)])
ds.cbind(x=c('Dmedall6', 'int_asthma_m'), newobj = 'Dmedall6', datasources = connections[c(1,2,3,4,5,6)])
ds.summary("Dmedall6", datasources = connections[c(1,2,3,4,5,6)])
ds.table(rvar="Dmedall6$asthma_m",cvar="Dmedall6$int_asthma_m", datasources = connections[c(1,2,3,4,5,6)])
#looks fine!

ds.dataFrameSubset(
  df.name = "Dmedall6",
  V1.name = "Dmedall6$int_asthma_m",
  V2.name = "0",
  Boolean.operator = ">=",
  newobj = "Dmedall7",
  datasources = connections[c(1,2,3,4,5,6)], 
  notify.of.progress = FALSE
)

ds.dim("Dmedall7", datasources = connections[c(1,2,3,4,5,6)])
#Left in study populations after adjusting:
# 32 684 in dnbc
# 3 477 in ninfea
# 4 964 in aslpac //without subsetting to ethnicity before this: 5 005 in alspac
# 44 620 in moba
# 795 in eden
# 2047 in genr


#############PATERNAL HISTORY OF ASTHMA
#Angela comment on the missing data in asthma_bf = Yes, asthma_bf is missing a lot of data..
#I still kept it in for my analysis..but perhaps you could do a sensitivity analysis to see how
#important it is? Of course it might be the children you add back in that might alter your results
#(you could restrict your population so that the two populations are the same)…if that makes sense!

ds.table('Dmedall7$asthma_bf', datasources=connections[c(1,2,3,4,5,6)])
#  205 have missing info in dnbc
# 200  have missing info in ninfea
# 1171 have missing info in alspac // 1186 in alspac

ds.asInteger('Dmedall7$asthma_bf', 'int_asthma_bf', datasources = connections[c(1,2,3,4,5,6)])
ds.cbind(x=c('Dmedall7', 'int_asthma_bf'), newobj = 'Dmedall7', datasources = connections[c(1,2,3,4,5,6)])
ds.summary("Dmedall7", datasources = connections[c(1,2,3,4,5,6)])
ds.table(rvar="Dmedall7$asthma_bf",cvar="Dmedall7$int_asthma_bf", datasources = connections[c(1,2,3,4,5,6)])
#looks fine!

ds.dataFrameSubset(
  df.name = "Dmedall7",
  V1.name = "Dmedall7$int_asthma_bf",
  V2.name = "0",
  Boolean.operator = ">=",
  newobj = "Dmedall8",
  datasources = connections[c(1,2,3,4,5,6)], 
  notify.of.progress = FALSE
)

ds.dim("Dmedall8", datasources = connections[c(1,2,3,4,5,6)])
#Left in study populations after adjusting:
# 32 479 in dnbc
# 3 277 in ninfea
# 3819 in aslpac
# 36 858 in moba
# 795 in eden
# 1596 in genr

ds.colnames("Dmedall8", datasources = connections[c(1,2,3,4,5,6)])



#########checking ga and bw
ds.mean("Dmedall8$birth_weight", datasources = connections[c(1,2,3,4,5,6)])
# 96 missing in dnbc
# 19 missing in ninfea
# 33 missing in alspac
# 828 missing in moba
# 0 missing in eden
# 0 missing in genr:)


#excluding children with missing info in bw:
ds.dataFrameSubset(
  df.name = "Dmedall8",
  V1.name = "Dmedall8$birth_weight",
  V2.name = "0",
  Boolean.operator = ">=",
  newobj = "Dmedall9",
  datasources = connections[c(1,2,3,4,5,6)], 
  notify.of.progress = FALSE
)

ds.dim("Dmedall9", datasources = connections[c(1,2,3,4,5,6)])
# 32 383 in dnbc
# 3258 in ninfea
#3786 in alspac
# 36 030 in moba
# 795 in eden
# 1596 in genr


#excluding with missing info on ga:
#Angela used the ga_bj – for moba Angela’s used last menstrual period (most complete one compared to ultrasound?)
#GA_BJ 
ds.assign(toAssign="Dmedall9$ga_lmp", newobj='ga',datasources = connections[4]) #only in moba
ds.cbind(x=c('Dmedall9', 'ga'), newobj = 'Dmedall9', datasources = connections[4])
ds.mean("Dmedall9$ga", datasources = connections[4])
#1640 missing in moba on ga_lmp

ds.assign(toAssign="Dmedall9$ga_bj", newobj='ga',datasources = connections[c(1,2,3,5,6)])
ds.cbind(x=c('Dmedall9', 'ga'), newobj = 'Dmedall9', datasources = connections[c(1,2,3,5,6)])
ds.mean("Dmedall9$ga", datasources = connections[c(1,2,3,5,6)])
# 0 missings in dnbc, alspac, eden and genr. 2 obs missing in ninfea.

ds.dataFrameSubset(
  df.name = "Dmedall9",
  V1.name = "Dmedall9$ga",
  V2.name = "0",
  Boolean.operator = ">=",
  newobj = "Dmedall10",
  datasources = connections[c(1,2,3,4,5,6)], 
  notify.of.progress = FALSE
)

ds.dim("Dmedall10", datasources = connections[c(1,2,3,4,5,6)])
#32 383 in dnbc
#3256 in ninfea
#3786 in alspac
#34390 in moba
#795 in eden
#1596 in genr
#----------------->study populations with info on BW and GA also! Remember to add in parity!! (see below)





########### Making the ADVERSE REPRODUCTIVE OUTCOMES - common variable

# From Maja:
#Adverse reproductive outcomes would include:
#- preterm birth (<37 weeks of gestation) here it is better to use GAbj variable as it the most complete one
#- low birth weight (<2500 grams)
#- cesarean.
#So you would have 1 if either of the three is 1 and 0 if all are zero. This also means that you 
#would need to exclude all those with missing data in at least one of the three variables.

######## 1) Making the preterm variable - <37 weeks of gestation ##########


ds.mean("Dmedall10$ga", datasources = connections) #in days!

#converting 37 weeks to days: 37*7 = 259 days:

ds.Boole(V1 ='Dmedall10$ga', V2='259', Boolean.operator='<',
         numeric.output=T, na.assign='NA', newobj='preterm', datasources = connections[c(1,2,3,4,5,6)])

#check that preterm is a factor variable:
#ds.class('preterm', datasources = connections[c(1,2,3,5)]) #numeric variable
#changing to factor:
#ds.asFactor('preterm', 'preterm_c', datasources = connections[c(1,2,3,4,5)])
#ds.cbind(x=c('Dmedall10', 'preterm_c'), newobj = 'Dmedall10', datasources = connections[c(1,2,3,4,5)])
#ds.class('Dmedall10$preterm_c', datasources = connections[c(1,2,3,4,5)]) 


#ds.table("Dmedall10$preterm_c", datasources = connections[c(1,2,3,4,5)]) 


# check the mean GA in the two categories of preterm:
#ds.meanSdGp(
#  x = 'Dmedall10$ga',
#  y = 'Dmedall10$preterm_c',
#  type = "both",
#  do.checks = FALSE,
#  datasources = connections[c(1,2,3,4,5)]
#)
#looks fine!


######## 2) Making low birth weight variable - <2500 grams ##########

ds.mean("Dmedall10$birth_weight", datasources = connections[c(1,2,3,4,5,6)]) 

#make boolian
ds.Boole(V1 ='Dmedall10$birth_weight', V2='2500', Boolean.operator='<',
         numeric.output=T, na.assign='NA', newobj='lowbw', datasources = connections[c(1,2,3,4,5,6)])

#check that lowbw is a factor variable:
#('lowbw', datasources = connections[c(1,2,3,4,5)]) #numeric variable
#changing to factor:
#ds.asFactor('lowbw', 'lowbw_c', datasources = connections[c(1,2,3,4,5)])
#ds.cbind(x=c('Dmedall10', 'lowbw_c'), newobj = 'Dmedall10', datasources = connections[c(1,2,3,4,5)])
#ds.class('Dmedall10$lowbw_c', datasources = connections[c(1,2,3,4,5)]) #now factor variable 


#ds.table("Dmedall10$lowbw_c", datasources = connections[c(1,2,3,4,5)]) #

# check the mean bw in the two categories of lowbw:
#ds.meanSdGp(
#  x = 'Dmedall10$birth_weight',
#  y = 'Dmedall10$lowbw_c',
#  type = "both",
#  do.checks = FALSE,
#  datasources = connections[c(1,2,3,4,5)]
#)
#looks fine


######## 3) Born by cesearan yes or no ##########

#OLD CODE - making of the csection - variable:
#mode of delivery - csection
#NB not possible to code 0/1 so coded 1 2
#ds.recodeValues(var.name = "D$mode_delivery", values2replace.vector = c(1,2,3,4,5),
#                new.values.vector = c(1,1,2,2,2), force.output.format = "no",
#                newobj = "csection", datasources = connections, notify.of.progress = FALSE)
#ds.cbind(x=c('D', 'csection'), newobj = 'D', datasources = connections)
#ds.table('D$csection', datasources=connections, useNA = "no")


ds.asNumeric('Dmedall10$mode_delivery', 'csection_new', datasources = connections[c(1,2,3,4,5,6)])

ds.Boole(V1 ='csection_new', V2='3', Boolean.operator='>=',
         numeric.output=T, na.assign='NA', newobj='csection_new', datasources = connections[c(1,2,3,4,5,6)])

#ds.cbind(x=c('Dmedall10', 'csection_new'), newobj = 'Dmedall10', datasources = connections[c(1,2,3,5)])

#ds.class('Dmedall10$csection_new', datasources = connections[c(1,2,3,5)]) #numeric




######### FINAL STEP: Making of the combined adverse reproductive variable ##########


ds.make(toAssign = "preterm + lowbw + csection_new", newobj = "adverse_rep_outcomes",
        datasources = connections[c(1,2,3,4,5,6)])


ds.Boole(V1 ='adverse_rep_outcomes', V2='1', Boolean.operator='>=',
         numeric.output=T, na.assign='NA', newobj='adverse_rep_outcomes', datasources = connections[c(1,2,3,4,5,6)])


#Now check the variable makes sense:
for (i in c('preterm', 'lowbw', 'Dmedall10$mode_delivery')) {
  x = ds.table('adverse_rep_outcomes', i, datasources = connections[c(1,2,3,4,5,6)])
  print(x)
}

ds.cbind(x=c('Dmedall10', 'adverse_rep_outcomes'), newobj = 'Dmedall10', datasources = connections[c(1,2,3,4,5,6)])


##### succeeded!!!


################################## PARITY

ds.table('Dmedall10$parity_m', datasources=connections[c(1,2,3,4,5,6)])


ds.asInteger('Dmedall10$parity_m', 'int_parity_m', datasources = connections[c(1,2,3,4,5,6)])
ds.cbind(x=c('Dmedall10', 'int_parity_m'), newobj = 'Dmedall10', datasources = connections[c(1,2,3,4,5,6)])
ds.summary("Dmedall10", datasources = connections[c(1,2,3,4,5,6)])
ds.table(rvar="Dmedall10$parity_m",cvar="Dmedall10$int_parity_m", datasources = connections[c(1,2,3,4,5,6)])
#looks fine!

ds.dataFrameSubset(
  df.name = "Dmedall10",
  V1.name = "Dmedall10$int_parity_m",
  V2.name = "0",
  Boolean.operator = ">=",
  newobj = "Dmedall11",
  datasources = connections[c(1,2,3,4,5,6)], 
  notify.of.progress = FALSE
)

ds.dim("Dmedall11", datasources = connections[c(1,2,3,4,5,6)])
#Left in study populations after adjusting:
# 32 383 in dnbc
# 3 146 in ninfea
# 3 744 in aslpac
# 34 390 in moba
# 793 in eden
# 1 593 in genr







######################## NEW BINARY EXPOSURE (high (1) vs medium + low (2)) #########################
ds.table("Dmedall11$edu_m_.0", datasources = connections)

ds.recodeLevels(x = "Dmedall11$edu_m_.0", newCategories = c("1", "2", "2"), newobj = "bin_edu_m",
                datasources = connections)
ds.table("bin_edu_m", datasources = connections)

ds.cbind(x=c('Dmedall11', 'bin_edu_m'), newobj = 'Dmedall11', datasources = connections)
ds.table('Dmedall11$bin_edu_m', datasources=connections)




##############################################################SAVING WORKSPACE

datashield.workspace_save(connections, 'flowchartall')

##############################################################



