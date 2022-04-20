########### Making the ADVERSE REPRODUCTIVE OUTCOMES - common variable
#Spring 2022
# From Maja:
#Adverse reproductive outcomes would include:
#- preterm birth (<37 weeks of gestation) here it is better to use GAbj variable as it the most complete one
#- low birth weight (<2500 grams)
#- cesarean.
#So you would have 1 if either of the three is 1 and 0 if all are zero. This also means that you 
#would need to exclude all those with missing data in at least one of the three variables.

######## 1) Making the preterm variable - <37 weeks of gestation ##########



#preterm variable:

ds.Boole(V1 ='D1$ga_bj', V2='259', Boolean.operator='<',
         numeric.output=T, na.assign='NA', newobj='preterm', datasources = connections[c(1,8,9,10)])


#low birth weight variable - <2500 grams:
ds.Boole(V1 ='D1$birth_weight', V2='2500', Boolean.operator='<',
         numeric.output=T, na.assign='NA', newobj='lowbw', datasources = connections[c(1,8,9,10)])

#Born by cesearan yes or no:

ds.asNumeric('D1$mode_delivery', 'csection', datasources = connections[c(1,8,9,10)])
ds.Boole(V1 ='csection', V2='3', Boolean.operator='>=',
         numeric.output=T, na.assign='NA', newobj='csection', datasources = connections[c(1,8,9,10)])


######### FINAL STEP: Making of the combined adverse reproductive variable ##########

ds.make(toAssign = "preterm + lowbw + csection", newobj = "adverse_rep_outcomes",
        datasources = connections[c(1,8,9,10)]) 

ds.Boole(V1 ='adverse_rep_outcomes', V2='1', Boolean.operator='>=',
         numeric.output=T, na.assign='NA', newobj='adverse_rep_outcomes', datasources = connections[c(1,8,9,10)])

#Now check the variable makes sense:
for (i in c('preterm', 'lowbw', 'D1$mode_delivery')) {
  x = ds.table('adverse_rep_outcomes', i, datasources = connections[c(1,8,9,10)])
  print(x)
}

ds.cbind(x=c('D1', 'adverse_rep_outcomes'), newobj = 'D1', datasources = connections[c(1,8,9,10)])


