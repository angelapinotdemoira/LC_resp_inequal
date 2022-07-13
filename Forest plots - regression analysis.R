#Forest plots for regression analysis
#Angela Pinot de Moira
#June 2022
###########################################

#------------Establish the population:


ds.dataFrame(x = c("D$child_id", "D$medall", "D$poisson_medall", "D$bin_edu_m", "D$edu_m_.0", "D$adverse_rep_outcomes", "D$passivesmoke2y", 
                   "D$breastfedcat", "D$preg_smk", "D$agebirth_m_y", "D$asthma_m", "D$asthma_bf", "D$parity2", "D$sex"),
             newobj = "D2", datasources = connections[c( 'dnbc', 'eden', 'moba','ninfea')], 
             notify.of.progress = FALSE)
ds.dataFrame(x = c("D$child_id", "D$medall", "D$poisson_medall", "D$bin_edu_m", "D$edu_m_.0", "D$adverse_rep_outcomes", "D$passivesmoke2y", 
                   "D$breastfedcat", "D$preg_smk", "D$agebirth_m_y", "D$asthma_m", "D$asthma_bf", "D$parity2", "D$ethn3_m", "D$sex"),
             newobj = "D2", datasources = connections[c('alspac', 'genr')], 
             notify.of.progress = FALSE) # include ethnicity for ALSPAC and Gen R
ds.completeCases(x1 = "D2", newobj = "D2", datasources = connections)

ds.dim("D2")

#First, Creating squared and cubic terms of maternal age at birht, agebirth_m_y

###########SQUARED
ds.assign(toAssign="D2$agebirth_m_y*D2$agebirth_m_y", newobj='matagesqu', datasources=connections)
##########CUBIC
ds.assign(toAssign="D2$agebirth_m_y*D2$agebirth_m_y*D2$agebirth_m_y",
          newobj='matagecub', datasources=connections)
#bind to final dataframe:
ds.cbind(x=c('D2', 'matagesqu', 'matagecub'), newobj = 'D2', datasources = connections)

#datashield.workspace_save(connections, 'SES3') 

########Regression models -------------------------------------------

#Unadjusted--------------------------------------------------------------------

#Build model:
data_table = "D2"
outcome = 'poisson_medall'
exposure = 'bin_edu_m'

# Run model

for (i in c(1,3,4,5,6)) {
  to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',exposure)))))")
  eval(parse(text=to_eval))
  to_eval = paste0("model_",i," = ds.glm(formula = fmla",i,", data = data_table, family = 'poisson', viewVarCov = TRUE,
  viewCor = TRUE, datasources = connections[",i,"])")
  eval(parse(text=to_eval))
}

#Adjusted--------------------------------------------------------------------

for (i in c(1,3,4,5,6)) {
  if(i==1){
    covariates=c("asthma_m", "agebirth_m_y", "matagesqu", "parity2") #DNBC
  }else if (i == 3){
    covariates=c("asthma_m", "agebirth_m_y", "ethn3_m", "parity2") #ALSPAC
  }else if (i==6) {
    covariates=c("asthma_m", "agebirth_m_y", "matagesqu", "ethn3_m", "parity2") #Gen R
  }else {
    covariates=c("asthma_m", "agebirth_m_y", "parity2") #MoBa and EDEN
  }
  to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',exposure), 
                                                                  paste0(data_table, '$',covariates)), collapse= '+')))")
  eval(parse(text=to_eval))
  to_eval = paste0("adj_model_",i," = ds.glm(formula = fmla",i,", data = data_table, family = 'binomial', viewVarCov = TRUE,
  viewCor = TRUE, datasources = connections[",i,"])")
  eval(parse(text=to_eval))
}


## Obtain effect estimates-------------------------------------------------------

#Unadjusted:
coeff=paste0(exposure, "2")
for (i in c(coeff)) {
  coefficient = paste0(data_table, '$', i)
  to_eval1 = paste0("yi_",i," = c(model_3$coefficients[coefficient,'Estimate'],
                    model_1$coefficients[coefficient,'Estimate'],
                    model_5$coefficients[coefficient,'Estimate'],
                    model_6$coefficients[coefficient,'Estimate'],
                    model_4$coefficients[coefficient,'Estimate']
                    )")
  eval(parse(text=to_eval1))
  to_eval2 = paste0("sei_",i," <- c(model_3$coefficients[coefficient,'Std. Error'],
                                    model_1$coefficients[coefficient,'Std. Error'],
                                    model_5$coefficients[coefficient,'Std. Error'],
                                    model_6$coefficients[coefficient,'Std. Error'],
                                    model_4$coefficients[coefficient,'Std. Error']
                                    )")
  eval(parse(text=to_eval2))
  #Random effects model:
  to_eval3 = paste0("res_",i," <- rma(yi_",i,", sei=sei_",i,", control=list(maxiter=2000))")
  eval(parse(text=to_eval3))
} 

forest(res_bin_edu_m2, atransf=exp)
res_bin_edu_m2

#Adjusted:
coeff=paste0(exposure, "2")
for (i in c(coeff)) {
  coefficient = paste0(data_table, '$', i)
  to_eval1 = paste0("yi_adj_",i," = c(adj_model_3$coefficients[coefficient,'Estimate'],
                    adj_model_1$coefficients[coefficient,'Estimate'],
                    adj_model_5$coefficients[coefficient,'Estimate'],
                    adj_model_6$coefficients[coefficient,'Estimate'],
                    adj_model_4$coefficients[coefficient,'Estimate']
                    )")
  eval(parse(text=to_eval1))
  to_eval2 = paste0("sei_adj_",i," <- c(adj_model_3$coefficients[coefficient,'Std. Error'],
                                    adj_model_1$coefficients[coefficient,'Std. Error'],
                                    adj_model_5$coefficients[coefficient,'Std. Error'],
                                    adj_model_6$coefficients[coefficient,'Std. Error'],
                                    adj_model_4$coefficients[coefficient,'Std. Error']
                                    )")
  eval(parse(text=to_eval2))
  #Random effects model:
  to_eval3 = paste0("res_adj_",i," <- rma(yi_adj_",i,", sei=sei_adj_",i,", control=list(maxiter=2000))")
  eval(parse(text=to_eval3))
} 

forest(res_adj_bin_edu_m2, atransf=exp)
res_adj_bin_edu_m2



##2x2 table:--------------------------------------------------------------------

temp <- ds.summary('D2$sex', datasources = connections[c('alspac','dnbc','eden','genr','moba')])
study_names <- names(temp)
num_studies <- length(temp)
rm(temp)

output <- ds.table2D(paste0(data_table,"$",outcome), paste0(data_table,"$",exposure),  datasources =connections[c('alspac','dnbc','eden','genr','moba')])
eval(parse(text=(paste0("counts_",exposure," <- data.frame(matrix(unlist(output$counts), nrow = num_studies, ncol = 9, byrow=T))"))))
eval(parse(text=(paste0("perc_",exposure," <- data.frame(matrix(unlist(output$colPercent), nrow = num_studies, ncol = 9, byrow=T))"))))

counts1 = c(counts_bin_edu_m[,c(2)])  
perc1 = c(perc_bin_edu_m[,c(2)])
counts2 = c(counts_bin_edu_m[,c(5)])  
perc2 = c(perc_bin_edu_m[,c(5)])
table_2D <- data.frame(cbind(study_names, counts1,perc1,counts2,perc2))
colnames(table_2D) <- c('Study', 'Highcount', 'Highperc', 'Lowcount', 'Lowperc')
rm(output,counts_bin_edu_m, perc_bin_edu_m, counts1,perc1,counts2,perc2)




# Forest plot:

#1) Unadjusted:

par(op)
setwd("~/LC_resp_inequal/Graphs")

png(
  file = "crude_ses_model.png", 
  width = 17.5, 
  height = 11.5, 
  units = "cm",
  res = 500)


estimates_crude <- c(yi_bin_edu_m2)
variances_crude <- c(sei_bin_edu_m2)
study_names2 <- c("ALSPAC", "DNBC", "EDEN", "Gen R", "MoBa") 

par(op)
op <- par(cex=0.90, font=2)
forest(estimates_crude, sei=variances_crude, ylim=c(4, 12), 
       rows=c(9:5), xlim=c(-13,7), alim=c(-4,5), at=log(c(0.5, 1, 3)), psize=(0.5), atransf=exp, showweights=FALSE,
       ilab=cbind(paste0(table_2D$Highcount, " (", table_2D$Highperc, ")"), paste0(table_2D$Lowcount, " (", 
                                                                                             table_2D$Lowperc, ")")),
       ilab.xpos = c(-7.5, -4.0), header = FALSE, 
       xlab="Crude Risk Ratio", refline=log(1), 
       slab=cbind(paste0(study_names2)), mlab="")


#Add labels:
op <- par(cex=0.9, font=2)
text(c(-7.75, -4.0), 11, c("High", "Low / medium"))
text(c(-5.5),     12, c("Maternal education level n (%)"))
text(5, 11, "Risk ratio [95% CI]")
text(-13.0, 11, pos=4, "Cohort")
par(op)

dev.off()


###------------------------------

#1) Adjusted:

par(op)
setwd("~/LC_resp_inequal/Graphs")

png(
  file = "adjusted_ses_model.png", 
  width = 17.5, 
  height = 11.5, 
  units = "cm",
  res = 500)


estimates_adjusted <- c(yi_adj_bin_edu_m2)
variances_adjusted <- c(sei_adj_bin_edu_m2)
study_names2 <- c("ALSPAC", "DNBC", "EDEN", "Gen R", "MoBa") 

par(op)
op <- par(cex=0.90, font=2)
forest(estimates_adjusted, sei=variances_adjusted, ylim=c(4, 12), 
       rows=c(9:5), xlim=c(-13,7), alim=c(-4,5), at=log(c(0.5, 1, 3)), psize=(0.5), atransf=exp, showweights=FALSE,
       ilab=cbind(paste0(table_2D$Highcount, " (", table_2D$Highperc, ")"), paste0(table_2D$Lowcount, " (", 
                                                                                   table_2D$Lowperc, ")")),
       ilab.xpos = c(-7.5, -4.0), header = FALSE, 
       xlab="Adjusted Risk Ratio", refline=log(1), 
       slab=cbind(paste0(study_names2)), mlab="")


#Add labels:
op <- par(cex=0.9, font=2)
text(c(-7.75, -4.0), 11, c("High", "Low / medium"))
text(c(-5.5),     12, c("Maternal education level, n (%)"))
text(5, 11, "Risk ratio [95% CI]")
text(-13.0, 11, pos=4, "Cohort")
par(op)

dev.off()