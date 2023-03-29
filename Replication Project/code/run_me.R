####################
#REPLICATION FILES: MAIN FILE
#Article: "How the Party Commands the Gun: The Foreign-Domestic Threat Dilemma in China"
#Authors: Daniel Mattingly
#This Version: July 19, 2022


####################
#Description: Running this script will create all the figures and tables in the article.
#The R script files were created with R version 4.0.3 running on Mac OS Big Sur (11.6)
#It was tested on four systems: on Mac OS Big Sur, on GNU Bash 3.2.57(1), on MATE Desktop Environment 1.1.16, and on Windows 10 Enterprise for Virtual Desktops
####################

##Clean the workspace

rm(list=ls())


#OPTION 1: Manually set the working directory here:

setwd('~/Downloads/Mattingly_AJPS_Replication')

#For example, if the replication directory has been unzipped in a Mac Downloads folder, 
#in a folder you have renamed Mattingly_AJPS_Replication
#the above might read something like: setwd('~/Downloads/Mattingly_AJPS_Replication') 

#OPTION 2: In addition, setwd() can be commented out using # and this script can be run from the 
#Unix command line (or the Terminal app on Macs). First navigate in the shell to the
#replication directory. Then, run the following command: R CMD BATCH run_me.R

#OPTION 3: In an IDE like RStudio, the project directory can be set by the user (e.g. in the Session menu in RStudio).

####################

#Check to see if necessary packages are installed.
#If not, the packages will be installed automatically.

packages <- c("plm", "rio", "lmtest", "stargazer", "sandwich", "ggplot2")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
rm(packages, new.packages)

#Load packages

require(rio)
require(plm)
require(sandwich)
require(lmtest)
require(stargazer)
require(ggplot2)
rm(list=ls())


##
##Load data
##

bio <- import("bio_data.csv", encoding = "UTF-8")
career <- import("career_data.csv", encoding = "UTF-8")
positions <- import("key_positions_data.csv", encoding = "UTF-8")

##
##Subset data to PLA officers holding key positions during relevant period:
##Deng era to onset of reforms
##

positions <- positions[positions$start_date_year>1978&positions$start_date_year<2018,]
bio <- bio[bio$name %in% positions$name,]
career <- career[career$unique_id %in% bio$unique_id,]


##
##Create variables
##

eval(parse("variable_creation.R", encoding="UTF-8"))

##
##Output main text tables
##

eval(parse("main_tables.R", encoding="UTF-8"))

##Note that running this script will produce two error messages:
##"In sqrt(diag(se)) : NaNs produced"
##This is caused by the two panel regressions interacting year and birth year fixed effects. 
##For some year-birth year interactions coefficients cannot be estimated because there are not 
##adequate observations in that strata. This issue is discussed in the main text.


##
##Output Figure 2
##


#Due to ggplot2 compatability issues, better to produce Figure 2 in the main script...

#################################################
##Figure 2a-b: Effect of Network Ties by Leader##
#################################################


lm1 <- lm(general_deng~deng.network, data=bio)
lm2 <- lm(general_jiang~jiang.network, data=bio)
lm3 <- lm(general_hu~hu.network, data=bio)
lm4 <- lm(general_xi~xi.network, data=bio)


estimates <- data.frame(pe=c(summary(lm1)$coefficients[2,1], 
                             summary(lm2)$coefficients[2,1],
                             summary(lm3)$coefficients[2,1],
                             summary(lm4)$coefficients[2,1]),
                        se.high=c(summary(lm1)$coefficients[2,1]+1.96*summary(lm1)$coefficients[2,2],
                                  summary(lm2)$coefficients[2,1]+1.96*summary(lm2)$coefficients[2,2],
                                  summary(lm3)$coefficients[2,1]+1.96*summary(lm3)$coefficients[2,2],
                                  summary(lm4)$coefficients[2,1]+1.96*summary(lm4)$coefficients[2,2]),
                        se.low=c(summary(lm1)$coefficients[2,1]-1.96*summary(lm1)$coefficients[2,2],
                                 summary(lm2)$coefficients[2,1]-1.96*summary(lm2)$coefficients[2,2],
                                 summary(lm3)$coefficients[2,1]-1.96*summary(lm3)$coefficients[2,2],
                                 summary(lm4)$coefficients[2,1]-1.96*summary(lm4)$coefficients[2,2]),
                        names=c("Deng\nXiaoping", "Jiang\nZemin", "Hu\nJintao", "Xi\nJinping")
)
estimates$names <- factor(estimates$names, levels = c("Deng\nXiaoping", "Jiang\nZemin", "Hu\nJintao", "Xi\nJinping"))


theme_bw1 <- function(base_size = 20, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x =       element_text(size = base_size, colour = "black",  hjust = .5 , vjust=1),
      axis.text.y =       element_text(size = base_size , colour = "black", hjust = 0 , vjust=.5 ),
      axis.ticks =        element_line(colour = "grey50"),
      #   axis.title.y =      element_text(size = base_size,angle=90,vjust=.01,hjust=.1),
      legend.position = "none"
    )
}

pdf("Figure_2a.pdf", width=7)
p = ggplot(estimates, aes(y=pe, x=names))
p = p + geom_hline(yintercept = 0,size=1,colour="black",linetype="dotted") 
p = p + geom_pointrange(aes(ymin=se.high,ymax=se.low), size=1.4, col="black")
p = p + scale_y_continuous(name="Promotion to General",  limits=c(-1, 1)) 
p = p + scale_x_discrete(name="") 
p = p  + theme_bw1()
p
dev.off()



lm1 <- lm(cmc_deng~deng.network, data=bio)
lm2 <- lm(cmc_jiang~jiang.network, data=bio)
lm3 <- lm(cmc_hu~hu.network, data=bio)
lm4 <- lm(cmc_xi~xi.network, data=bio)



estimates <- data.frame(pe=c(summary(lm1)$coefficients[2,1], 
                             summary(lm2)$coefficients[2,1],
                             summary(lm3)$coefficients[2,1],
                             summary(lm4)$coefficients[2,1]),
                        se.high=c(summary(lm1)$coefficients[2,1]+1.96*summary(lm1)$coefficients[2,2],
                                  summary(lm2)$coefficients[2,1]+1.96*summary(lm2)$coefficients[2,2],
                                  summary(lm3)$coefficients[2,1]+1.96*summary(lm3)$coefficients[2,2],
                                  summary(lm4)$coefficients[2,1]+1.96*summary(lm4)$coefficients[2,2]),
                        se.low=c(summary(lm1)$coefficients[2,1]-1.96*summary(lm1)$coefficients[2,2],
                                 summary(lm2)$coefficients[2,1]-1.96*summary(lm2)$coefficients[2,2],
                                 summary(lm3)$coefficients[2,1]-1.96*summary(lm3)$coefficients[2,2],
                                 summary(lm4)$coefficients[2,1]-1.96*summary(lm4)$coefficients[2,2]),
                        names=c("Deng\nXiaoping", "Jiang\nZemin", "Hu\nJintao", "Xi\nJinping")
)
estimates$names <- factor(estimates$names, levels = c("Deng\nXiaoping", "Jiang\nZemin", "Hu\nJintao", "Xi\nJinping"))


pdf("Figure_2b.pdf", width=7)
p = ggplot(estimates, aes(y=pe, x=names))
p = p + geom_hline(yintercept = 0,size=1,colour="black",linetype="dotted") 
p = p + geom_pointrange(aes(ymin=se.high,ymax=se.low), size=1.4, col="black")
p = p + scale_y_continuous(name="Promotion to Central Military Commission",  limits=c(-1, 1)) 
p = p + scale_x_discrete(name="") 
p = p  + theme_bw1()
p
dev.off()


##
##Output appendix tables and figures
##

eval(parse("appendix_tables_figures.R", encoding="UTF-8"))

##Note that running this file will produce five error messages reading:
##"In sqrt(diag(se)) : NaNs produced"
##This is caused by the panel regressions interacting year and birth year fixed effects. 
##For some year-birth year interactions coefficients cannot be estimated because there are not 
##adequate observations in that strata. This issue is discussed briefly in the main text.



