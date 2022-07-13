#Fig 1 for social inequalities in resp. health paper
#Angela Pinot de Moira
#2022

#load libraries:
library(purrr)
library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(extrafont)
library(ggthemes)


######################################################################################
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




###### Figures-----------------------------------
#1)outcome - medall
#2)exposure -  edu_m_.0/bin_edu_m
#3)mediators - adverse_rep_outcomes, passivesmoke2y, breastfedcat, preg_smk 
#4)covariates - agebirth_m_y, asthma_m, asthma_bf

vars = c("medall","edu_m_.0", "preg_smk", "adverse_rep_outcomes", "breastfedcat", "passivesmoke2y")
vars = c("medall","bin_edu_m", "preg_smk", "adverse_rep_outcomes", "breastfedcat", "passivesmoke2y")

for (j in 1:length(vars)){
  print(paste0(vars[j]," start"))
     summ_table = ds.table(paste0('D2$',vars[j]), exclude = c("NA"), useNA = c("no"),datasources = connections[c('alspac', 'dnbc', 'eden', 'genr', 'moba')])
    study_names2 <- c('alspac', 'dnbc', 'eden', 'genr', 'moba')
    if(vars[j]=="medall"| vars[j]=="bin_edu_m" | vars[j]=="preg_smk"| vars[j]=="adverse_rep_outcomes"| vars[j]=="passivesmoke2y") {
    for (i in c(1:2)) {
      if(i==1) {
        per <- summ_table$output.list$TABLE_rvar.by.study_col.props[c(i),]
        per <- per*100
        per <- format(round(per,1), nsmall = 1)
        eval(parse(text=(paste0("table1_",vars[j]," <- data.frame(cbind(per))"))))
      } else {
        per <- summ_table$output.list$TABLE_rvar.by.study_col.props[c(i),]
        per <- per*100
        per <- format(round(per,1), nsmall = 1)
        eval(parse(text=(paste0("table1_",vars[j]," <- cbind(table1_",vars[j],", per)"))))
        eval(parse(text=(paste0("table1_",vars[j]," <- cbind(study_names2,table1_",vars[j],")"))))
        eval(parse(text=(paste0("colnames(table1_",vars[j],") <- c('cohort', 'per1','per2')"))))
      } 
    }
  } else {
    for (i in c(1:3)) {
      if(i==1) {
        per <- summ_table$output.list$TABLE_rvar.by.study_col.props[c(i),]
        per <- per*100
        per <- format(round(per,1), nsmall = 1)
        eval(parse(text=(paste0("table1_",vars[j]," <- data.frame(cbind(per))"))))
        eval(parse(text=(paste0("table1_",vars[j]," <- cbind(study_names2,table1_",vars[j],")"))))
      } else {
        per <- summ_table$output.list$TABLE_rvar.by.study_col.props[c(i),]
        per <- per*100
        per <- format(round(per,1), nsmall = 1)
        eval(parse(text=(paste0("table1_",vars[j]," <- cbind(table1_",vars[j],", per)"))))
      } } 
    eval(parse(text=(paste0("colnames(table1_",vars[j],") <- c('cohort', 'per1','per2', 'per3')"))))
  }
  rm(per, table1_temp, summ_table)
}

#############PLOTS-----------------------------------------------

library(reshape2)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)

###Asthma------------------

dat2 <-melt(table1_medall, id.vars="cohort") # reshape data
dat2$value <-as.numeric(dat2$value) # change values to numeric

dat2 <- ddply(dat2, .(cohort),
              transform, pos = value+2.5) # create a position variable
dat2$pos[dat2$variable == "per1"] <- "NA" # delete the position values for "Nos"
dat2$pos <- as.numeric(dat2$pos)

dat2 <- as.data.frame(dat2)
dat2$variable <- factor(dat2$variable, levels = c("per1","per2"),
                        labels = c("No","Yes")) # label some variables
dat2$cohort <- factor(dat2$cohort, levels = c("alspac","dnbc","eden","genr","moba"),
                      labels = c("ALSPAC","DNBC","EDEN","Gen R","MoBa")) # label some variables



fill <- c("#FFFFFF","#006699" )

plot1 <- ggplot() +
  geom_bar(aes(y = value, x = cohort, fill = variable), data = dat2,
           stat="identity") +
  geom_text(data=dat2, aes(x = cohort, y = pos, label = paste0(value,"%")),
            colour="black", family="Calibri", size=3) +
  # theme(legend.position="bottom", legend.direction="horizontal",
  #        legend.title = element_blank()) +
  theme(legend.position="none") + #hides the legend
  scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
  labs(x="", y="Percentage", tag = "A") +
  ggtitle("Current asthma at school age") +
  scale_fill_manual(values=fill) +
  theme(axis.line = element_line(size=0.25, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 11, family = "Calibri", face = "bold"),
        text=element_text(family="Calibri"),
        axis.text.x=element_text(colour="black", size = 7),
        axis.text.y=element_text(colour="black", size = 8))
plot1 <- plot1 + theme(
  plot.title = element_text(hjust = 0.5))
plot1



###Smoking in pregnancy ------------------

dat2 <-melt(table1_preg_smk, id.vars="cohort") # reshape data
dat2$value <-as.numeric(dat2$value) # change values to numeric

dat2 <- ddply(dat2, .(cohort),
              transform, pos = value+2.5) # create a position variable
dat2$pos[dat2$variable == "per1"] <- "NA" # delete the position values for "Nos"
dat2$pos <- as.numeric(dat2$pos)

dat2 <- as.data.frame(dat2)
dat2$variable <- factor(dat2$variable, levels = c("per1","per2"),
                        labels = c("No","Yes")) # label some variables
dat2$cohort <- factor(dat2$cohort, levels = c("alspac","dnbc","eden","genr","moba"),
                      labels = c("ALSPAC","DNBC","EDEN","Gen R","MoBa")) # label some variables



fill <- c("#FFFFFF","#006699" )

plot2 <- ggplot() +
  geom_bar(aes(y = value, x = cohort, fill = variable), data = dat2,
           stat="identity") +
  geom_text(data=dat2, aes(x = cohort, y = pos, label = paste0(value,"%")),
            colour="black", family="Calibri", size=3) +
  theme(legend.position="none") +
  scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
  labs(x="", y="Percentage", tag = "C") +
  ggtitle("Smoking in pregnancy") +
  scale_fill_manual(values=fill) +
  theme(axis.line = element_line(size=0.25, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 11, family = "Calibri", face = "bold"),
        text=element_text(family="Calibri"),
        axis.text.x=element_text(colour="black", size = 7),
        axis.text.y=element_text(colour="black", size = 8))
plot2 <- plot2 + theme(
  plot.title = element_text(hjust = 0.5))
plot2

###Adverse reproductive outcomes ------------------

dat2 <-melt(table1_adverse_rep_outcomes, id.vars="cohort") # reshape data
dat2$value <-as.numeric(dat2$value) # change values to numeric

dat2 <- ddply(dat2, .(cohort),
              transform, pos = value+2.5) # create a position variable
dat2$pos[dat2$variable == "per1"] <- "NA" # delete the position values for "Nos"
dat2$pos <- as.numeric(dat2$pos)

dat2 <- as.data.frame(dat2)
dat2$variable <- factor(dat2$variable, levels = c("per1","per2"),
                        labels = c("No","Yes")) # label some variables
dat2$cohort <- factor(dat2$cohort, levels = c("alspac","dnbc","eden","genr","moba"),
                      labels = c("ALSPAC","DNBC","EDEN","Gen R","MoBa")) # label some variables



fill <- c("#FFFFFF","#006699" )

plot3 <- ggplot() +
  geom_bar(aes(y = value, x = cohort, fill = variable), data = dat2,
           stat="identity") +
  geom_text(data=dat2, aes(x = cohort, y = pos, label = paste0(value,"%")),
            colour="black", family="Calibri", size=3) +
  theme(legend.position="none") +
  scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
  labs(x="", y="", tag = "D") +
  ggtitle("Adverse reproductive outcome") +
  scale_fill_manual(values=fill) +
  theme(axis.line = element_line(size=0.25, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 11, family = "Calibri", face = "bold"),
        text=element_text(family="Calibri"),
        axis.text.x=element_text(colour="black", size = 7),
        axis.text.y=element_text(colour="black", size = 8))
plot3 <- plot3 + theme(
  plot.title = element_text(hjust = 0.5))
plot3


###Environmental tobacco smoke ------------------

dat2 <-melt(table1_passivesmoke2y, id.vars="cohort") # reshape data
dat2$value <-as.numeric(dat2$value) # change values to numeric

dat2 <- ddply(dat2, .(cohort),
              transform, pos = value+2.5) # create a position variable
dat2$pos[dat2$variable == "per1"] <- "NA" # delete the position values for "Nos"
dat2$pos <- as.numeric(dat2$pos)

dat2 <- as.data.frame(dat2)
dat2$variable <- factor(dat2$variable, levels = c("per1","per2"),
                        labels = c("No","Yes")) # label some variables
dat2$cohort <- factor(dat2$cohort, levels = c("alspac","dnbc","eden","genr","moba"),
                      labels = c("ALSPAC","DNBC","EDEN","Gen R","MoBa")) # label some variables

fill <- c("#FFFFFF","#006699" )

plot6 <- ggplot() +
  geom_bar(aes(y = value, x = cohort, fill = variable), data = dat2,
           stat="identity") +
  geom_text(data=dat2, aes(x = cohort, y = pos, label = paste0(value,"%")),
            colour="black", family="Calibri", size=3) +
  theme(legend.position="none") +
  scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
  labs(x="", y="", tag = "F") +
  ggtitle("Environmental tobacco smoke") +
  scale_fill_manual(values=fill) +
  theme(axis.line = element_line(size=0.25, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 11, family = "Calibri", face = "bold"),
        text=element_text(family="Calibri"),
        axis.text.x=element_text(colour="black", size = 7),
        axis.text.y=element_text(colour="black", size = 8))
plot6 <- plot6 + theme(
  plot.title = element_text(hjust = 0.5))
plot6




###Categorical maternal education level------------------
table1_edu_m_.0$per1 <- as.character(table1_edu_m_.0$per1)
table1_edu_m_.0$per1 <- as.numeric(table1_edu_m_.0$per1)
table1_edu_m_.0$per2 <- as.character(table1_edu_m_.0$per2)
table1_edu_m_.0$per2 <- as.numeric(table1_edu_m_.0$per2)
table1_edu_m_.0$per3 <- as.character(table1_edu_m_.0$per3)
table1_edu_m_.0$per3 <- as.numeric(table1_edu_m_.0$per3)

dat2 <-melt(table1_edu_m_.0, id.vars="cohort") # reshape data
dat2$value <-as.numeric(dat2$value) # change values to numeric

dat2 <- ddply(dat2, .(cohort),
              transform, pos = value+2.5) # create a position variable
dat2$pos[dat2$variable == "per2"] <- dat2$value[dat2$variable == "per3"] + dat2$value[dat2$variable == "per2"] + 2.5 # edit position for medium level
dat2$pos[dat2$variable == "per1"] <- dat2$value[dat2$variable == "per3"] + dat2$value[dat2$variable == "per2"] + dat2$value[dat2$variable == "per1"] + 2.5 # edit position for high level

dat2 <- as.data.frame(dat2)
dat2$variable <- factor(dat2$variable, levels = c("per1","per2","per3"),
                        labels = c("High","Medium","Low")) # label some variables
dat2$cohort <- factor(dat2$cohort, levels = c("alspac","dnbc","eden","genr","moba"),
                      labels = c("ALSPAC","DNBC","EDEN","Gen R","MoBa")) # label some variables

fill <- c("#99CCEE","#0099CC","#006699" )

plot4 <- ggplot() +
  geom_bar(aes(y = value, x = cohort, fill = variable), data = dat2,
           stat="identity") +
  geom_text(data=dat2, aes(x = cohort, y = pos, label = paste0(value,"%")),
            colour="black", family="Calibri", size=3) +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(), legend.margin = margin(0, 0, 0, 0), 
        legend.box.margin = margin(0, 0, 0, 0), legend.text = element_text(size = 8)) +
  scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
  labs(x="", y="", tag = "B") +
  ggtitle("Maternal education level") +
  scale_fill_manual(values=fill) +
  theme(axis.line = element_line(size=0.25, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 11, family = "Calibri", face = "bold"),
        text=element_text(family="Calibri"),
        axis.text.x=element_text(colour="black", size = 7),
        axis.text.y=element_text(colour="black", size = 8))
plot4 <- plot4 + theme(
  plot.title = element_text(hjust = 0.5))
plot4

###Binary maternal education ------------------

dat2 <-melt(table1_bin_edu_m, id.vars="cohort") # reshape data
dat2$value <-as.numeric(dat2$value) # change values to numeric

dat2 <- ddply(dat2, .(cohort),
              transform, pos = value+2.5) # create a position variable
dat2$pos[dat2$variable == "per1"] <- dat2$value[dat2$variable == "per1"] + dat2$value[dat2$variable == "per2"] + 2.5 # edit position for medium level
dat2$pos <- as.numeric(dat2$pos)

dat2 <- as.data.frame(dat2)
dat2$variable <- factor(dat2$variable, levels = c("per1","per2"),
                        labels = c("High","Low/medium")) # label some variables
dat2$cohort <- factor(dat2$cohort, levels = c("alspac","dnbc","eden","genr","moba"),
                      labels = c("ALSPAC","DNBC","EDEN","Gen R","MoBa")) # label some variables



fill <- c("#99CCEE","#006699" )


plot4 <- ggplot() +
  geom_bar(aes(y = value, x = cohort, fill = variable), data = dat2,
           stat="identity") +
  geom_text(data=dat2, aes(x = cohort, y = pos, label = paste0(value,"%")),
            colour="black", family="Calibri", size=3) +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(), legend.margin = margin(0, 0, 0, 0), 
        legend.box.margin = margin(0, 0, 0, 0), legend.text = element_text(size = 8)) +
  scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
  labs(x="", y="", tag = "B") +
  ggtitle("Maternal education level") +
  scale_fill_manual(values=fill) +
  theme(axis.line = element_line(size=0.25, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 11, family = "Calibri", face = "bold"),
        text=element_text(family="Calibri"),
        axis.text.x=element_text(colour="black", size = 7),
        axis.text.y=element_text(colour="black", size = 8))
plot4 <- plot4 + theme(
  plot.title = element_text(hjust = 0.5))
plot4


###Breastfeeding------------------
table1_breastfedcat <- table1_breastfedcat2 # backup data
table1_breastfedcat$per1 <- as.character(table1_breastfedcat$per1)
table1_breastfedcat$per1 <- as.numeric(table1_breastfedcat$per1)
table1_breastfedcat$per2 <- as.character(table1_breastfedcat$per2)
table1_breastfedcat$per2 <- as.numeric(table1_breastfedcat$per2)
table1_breastfedcat$per3 <- as.character(table1_breastfedcat$per3)
table1_breastfedcat$per3 <- as.numeric(table1_breastfedcat$per3)

dat2 <-melt(table1_breastfedcat, id.vars="cohort") # reshape data
dat2$value <-as.numeric(dat2$value) # change values to numeric

dat2 <- ddply(dat2, .(cohort),
              transform, pos = value+2.5) # create a position variable
dat2$pos[dat2$variable == "per2"] <- dat2$value[dat2$variable == "per3"] + dat2$value[dat2$variable == "per2"] + 1.0 # edit position for medium level
dat2$pos[dat2$variable == "per1"] <- dat2$value[dat2$variable == "per3"] + dat2$value[dat2$variable == "per2"] + dat2$value[dat2$variable == "per1"] + 6.0 # edit position for high level

dat2 <- as.data.frame(dat2)
dat2$variable <- factor(dat2$variable, levels = c("per1","per2","per3"),
                        labels = c("Never","<6 months",">=6 months")) # label some variables
dat2$cohort <- factor(dat2$cohort, levels = c("alspac","dnbc","eden","genr","moba"),
                      labels = c("ALSPAC","DNBC","EDEN","Gen R","MoBa")) # label some variables

fill <- c("#99CCEE","#0099CC","#006699" )

plot5 <- ggplot() +
  geom_bar(aes(y = value, x = cohort, fill = variable), data = dat2,
           stat="identity") +
  geom_text(data=dat2, aes(x = cohort, y = pos, label = paste0(value,"%")),
            colour="black", family="Calibri", size=3) +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(), legend.margin = margin(0, 0, 0, 0), 
        legend.box.margin = margin(0, 0, 0, 0), legend.text = element_text(size = 8)) +
  scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
  labs(x="", y="Percentage", tag = "E") +
  ggtitle("Breastfeeding duration") +
  scale_fill_manual(values=fill) +
  theme(axis.line = element_line(size=0.25, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 11, family = "Calibri", face = "bold"),
        text=element_text(family="Calibri"),
        axis.text.x=element_text(colour="black", size = 7),
        axis.text.y=element_text(colour="black", size = 8))
plot5 <- plot5 + theme(
  plot.title = element_text(hjust = 0.5))
plot5


###Combine plots---------------------------------

library(ggplot2)
library(gridExtra)
library(grid)


plot1<- plot1 + theme(plot.margin=unit(c(2,0.25,0,0.25),"cm"))
plot4<- plot4 + theme(plot.margin=unit(c(2,0.25,0,0.25),"cm"))

p1 <- grid.arrange(arrangeGrob(plot1 + theme(legend.position="none"),
                               plot4 + theme(legend.position="bottom"),
                               widths = c(7, 7)),
                   nrow=1,heights=c(10))
p1


plot2<- plot2 + theme(plot.margin=unit(c(2,0.25,0,0.25),"cm"))
plot3<- plot3 + theme(plot.margin=unit(c(2,0.25,0,0.25),"cm"))

p2 <- grid.arrange(arrangeGrob(plot2 + theme(legend.position="none"),
                               plot3 + theme(legend.position="none"),
                               widths = c(7, 7)),
                   nrow=1,heights=c(10))
p2    


plot5<- plot5 + theme(plot.margin=unit(c(2,0.25,0,0.25),"cm"))
plot6<- plot6 + theme(plot.margin=unit(c(2,0.25,0,0.25),"cm"))

p3 <- grid.arrange(arrangeGrob(plot5 + theme(legend.position="bottom"),
                               plot6 + theme(legend.position="none"),
                               widths = c(7, 7)),
                   nrow=1,heights=c(10))
p3 

setwd("~/LC_resp_inequal/Graphs")
png(
  file = "Fig2.png", 
  width = 18.8, 
  height = 27.5, 
  units = "cm",
  res = 500)

p4 <- grid.arrange(arrangeGrob(p1,p2,p3))

dev.off()
