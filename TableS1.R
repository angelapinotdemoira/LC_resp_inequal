#Script to descriptive table S1
#June 2022
#Angela Pinot de Moira
#####################################################

####################################################################################
#Source libraries:

library(purrr)
library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
#library(remotes)
#install_github("lifecycle-project/ds-helper", force=TRUE)
#library(dsHelper)

#setwd("/home/angela/angela/WP1 paper")
#source("Tims_getStats.R")


#################################################################################################################################
#Population is based on participants with:
#1)outcome - medall
#2)exposure -  edu_m_.0/bin_edu_m
#3)mediators - adverse_rep_outcomes, passivesmoke2y, breastfedcat, preg_smk 
#4)covariates - agebirth_m_y, asthma_m, asthma_bf

#------------Establish the population:


ds.dataFrame(x = c("D$child_id", "D$medall", "D$bin_edu_m", "D$edu_m_.0", "D$adverse_rep_outcomes", "D$passivesmoke2y", 
                   "D$breastfedcat", "D$preg_smk", "D$agebirth_m_y", "D$asthma_m", "D$asthma_bf", "D$parity2", "D$sex"),
             newobj = "D2", datasources = connections[c( 'dnbc', 'eden', 'moba','ninfea')], 
             notify.of.progress = FALSE)
ds.dataFrame(x = c("D$child_id", "D$medall", "D$bin_edu_m", "D$edu_m_.0", "D$adverse_rep_outcomes", "D$passivesmoke2y", 
                   "D$breastfedcat", "D$preg_smk", "D$agebirth_m_y", "D$asthma_m", "D$asthma_bf", "D$parity2", "D$ethn3_m", "D$sex"),
             newobj = "D2", datasources = connections[c('alspac', 'genr')], 
             notify.of.progress = FALSE) # include ethnicity for ALSPAC and Gen R
ds.completeCases(x1 = "D2", newobj = "D2", datasources = connections)

ds.dim("D2")

#merge back ethn1_m (for EDEN):

ds.dataFrame(x = c("D$child_id", "D$ethn1_m"),
             newobj = "ethn", datasources = connections, 
             notify.of.progress = FALSE)

ds.merge(
  x.name = 'D2', 
  y.name = 'ethn', 
  by.x.names = 'child_id',
  by.y.names = 'child_id', 
  newobj = 'tableS1',
  datasources = connections
)

######################Supplementary tables----------------------------------------------

#Create a white ethnicity variable in cohorts with data
ds.Boole(V1 = "tableS1$ethn3_m", V2='1', Boolean.operator='==',
         numeric.output=T, na.assign='NA', newobj=paste0("white"), datasources = connections[c('alspac', 'genr')])
#For EDEN, use "ethn1_m":
#ds.Boole(V1 = "tableS1$ethn1_m", V2='1', Boolean.operator='==',
#numeric.output=T, na.assign='NA', newobj=paste0("white"), datasources = connections[c('eden')])


ds.cbind(x=c('tableS1', 'white'), newobj = 'tableS1', datasources = connections[c('alspac', 'genr')])
ds.dataFrameFill("tableS1", "tableS1", datasources = connections)


#####Set up headings and create table:--------------------
fullNum = ds.length('tableS1$child_id', type = 'split', datasources = connections[c('alspac', 'dnbc', 'eden', 'genr', 'moba')]) 
fullNum <- format(fullNum, big.mark=",", scientific=FALSE)

temp <- ds.summary('tableS1$sex', datasources = connections[c('alspac', 'dnbc', 'eden', 'genr', 'moba')])
study_names <- names(temp)
num_studies <- length(temp)
rm(temp)


#########Create table -----------------------------------


table_s1 = data.frame(rbind(fullNum))
colnames(table_s1)<- study_names


#Binary categorical variables

vars = c("medall", "preg_smk", "adverse_rep_outcomes", "passivesmoke2y", "sex", "asthma_m", 
         "asthma_bf", "white")

for (j in 1:length(vars)){
  print(paste0(vars[j]," start"))
  summ_table = ds.table(paste0('tableS1$',vars[j]), exclude = c("NA"), useNA = c("no"),datasources = connections[c('alspac', 'dnbc', 'eden', 'genr', 'moba')])
  counts <- summ_table$output.list$TABLE_rvar.by.study_counts[c(2),] 
  counts <- format(counts, big.mark=",", scientific=FALSE)
  per <- summ_table$output.list$TABLE_rvar.by.study_col.props[c(2),]
  per <- per*100
  per <- format(round(per, 1), nsmall = 1)
  table1_temp <-data.frame(rbind(paste0(counts,' (',per,')')))
  colnames(table1_temp) <- study_names
  rownames(table1_temp) <- vars[j]
  table_s1 = rbind(table_s1,table1_temp)
  table_s1
  rm(counts, per, table1_temp, summ_table)
}


#Other categorical variables

vars = c("breastfedcat", "parity2")

for (j in 1:length(vars)){
  print(paste0(vars[j]," start"))
  summ_table = ds.table(paste0('tableS1$',vars[j]), exclude = c("NA"), useNA = c("no"),datasources = connections[c('alspac', 'dnbc', 'eden', 'genr', 'moba')])
      for (i in c(1:3)) {
      counts <- summ_table$output.list$TABLE_rvar.by.study_counts[c(i),] 
      counts <- format(counts, big.mark=",", scientific=FALSE)
      per <- summ_table$output.list$TABLE_rvar.by.study_col.props[c(i),]
      per <- per*100
      per <- format(round(per, 1), nsmall = 1)
      table1_temp <-data.frame(rbind(paste0(counts,' (',per,')')))
      colnames(table1_temp) <- study_names
      rownames(table1_temp) <- paste0(vars[j],",",i)
      table_s1 = rbind(table_s1,table1_temp)
    }
  rm(counts, per, table1_temp, summ_table)
}

#Continuous variables  
vars = c("agebirth_m_y")

for (j in 1:length(vars)){
  print(paste0(vars[j]," start"))
  mean <- ds.mean(paste0("tableS1$",vars[j]), datasources = connections[c('alspac', 'dnbc', 'eden', 'genr', 'moba')])
  mean <- mean$Mean.by.Study[,c(1)]
  mean <- format(round(mean, 2), nsmall = 2)
  var <- ds.var(paste0("tableS1$",vars[j]), datasources = connections[c('alspac', 'dnbc', 'eden', 'genr', 'moba')])
  var <- var$Variance.by.Study[,c(1)]
  sd <- c(sqrt(var))
  sd <- format(round(sd, 2), nsmall = 2)
  table1_temp <-data.frame(rbind(paste0(mean,' (',sd,')')))
  colnames(table1_temp) <- study_names
  rownames(table1_temp) <- vars[j]
  table_s1 = rbind(table_s1,table1_temp)
  table_s1
  rm(mean, var, table1_temp, sd)
}



## To create strata by of maternal education, subset the data and start again:
#1
table_s1a <- table_s1 #save as new table so it doesn't get overwritten

ds.assign(toAssign = "tableS1",
          newobj = "tableS1a",
          datasources = connections) #save dataframe

ds.dataFrameSubset(
  df.name = "tableS1",
  V1.name = "tableS1$bin_edu_m",
  V2.name = "1",
  Boolean.operator = "==",
  newobj = "tableS1",
  datasources = connections,
  notify.of.progress = FALSE
)

#2
table_s1_1 <- table_s1 #save as new table so it doesn't get overwritten

ds.assign(toAssign = "tableS1a",
          newobj = "tableS1",
          datasources = connections) #refresh dataframe

ds.dataFrameSubset(
  df.name = "tableS1",
  V1.name = "tableS1$bin_edu_m",
  V2.name = "2",
  Boolean.operator = "==",
  newobj = "tableS1",
  datasources = connections,
  notify.of.progress = FALSE
)


table_s1_2 <- table_s1 #save as new table so it doesn't get overwritten


