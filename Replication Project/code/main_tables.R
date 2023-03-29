####################
#REPLICATION FILES: MAIN FIGURES AND TABLES
#Article: "How the Party Commands the Gun: The Foreign-Domestic Threat Dilemma in China"
#Authors: Daniel Mattingly
#This Version: July 19, 2022

################
###IMPORTANT!###
################

#If you run this file from the RStudio IDE please ensure that you have opened the file with UTF-8 encoding
#This ensure Chinese will load properly
#You may need to go to the following:
#1. Click the file menu
#2. select "Reopen with encoding" 
#3. In the pop-up menu select "UTF-8 (system default)"
#4. Select "Ok"

####################
#Description: Running this script will create the tables and figures in the main texst.
#The script was written with R version 4.0.3
#It was created and tested on Mac OS Big Sur.
####################

###IMPORTANT: To produce the ggplot figures it may be necessary to run the
###script on the console rather than using the source() function in run_me_main.R


####################################
##Table 1: Correlates of Promotion##
####################################

lm1 <- lm(general~cmc_chair_connection_current, data=bio)
lm2 <- lm(general~combat_post_1949, data=bio)
lm3 <- lm(general~cmc_chair_connection_current+combat_post_1949+college+participated_long_march+commissar+minority+parent_CCP_leader+rural+cohort_decade, data=bio)
lm4 <- lm(cmc~cmc_chair_connection_current, data=bio)
lm5 <- lm(cmc~combat_post_1949, data=bio)
lm6 <- lm(cmc~cmc_chair_connection_current+combat_post_1949+college+participated_long_march+commissar+minority+parent_CCP_leader+rural+cohort_decade, data=bio)

rob.fit1        <- coeftest(lm1, function(x) vcovHC(x, type="HC3"))
rob.fit2        <- coeftest(lm2, function(x) vcovHC(x, type="HC3"))
rob.fit3        <- coeftest(lm3, function(x) vcovHC(x, type="HC3"))
rob.fit4        <- coeftest(lm4, function(x) vcovHC(x, type="HC3"))
rob.fit5        <- coeftest(lm5, function(x) vcovHC(x, type="HC3"))
rob.fit6        <- coeftest(lm6, function(x) vcovHC(x, type="HC3"))

stargazer(lm1, lm2, lm3, lm4, lm5, lm6, type = "latex", 
          se = list(rob.fit1[,"Std. Error"], rob.fit2[,"Std. Error"],
                    rob.fit3[,"Std. Error"], rob.fit4[,"Std. Error"],
                    rob.fit5[,"Std. Error"], rob.fit6[,"Std. Error"]), 
          omit=c("cohort_decade"),
          label = "main_results", 
          omit.stat=c("f", "ser"),
          dep.var.labels = c("Promoted to General", "Promoted to CMC"),
          add.lines=list(c("Birth cohort fixed effects", "", "", "$\\checkmark$", "", "", "$\\checkmark$")),
          title = "Promotion to full general-level position and to the CCP Central Military Commission",
          star.char = c("+", "*", "**"),
          notes.append=FALSE,
          notes = c("PLA officers connected to the top civilian leader or with combat experience are more likely to be promoted. Ordinary-least squares regression on cross-sectional dataset of PLA officers in the post-1978 period. The outcomes are a binary indicator for promotion to a general-grade position (military region commander or commissar and above) and promotion to the the Central Military Commission (CMC). Robust standard errors are in parentheses.$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01}"),
          covariate.labels = c("Career Tie to Paramount Leader", "Combat Experience, Post-1949", "College-Level Education", "Long March Participant", "Political Commissar Experience", "Ethnic Minority", "Princeling", "Rural Birth"),
          out = "Table_1.tex")



#############################
##Table 2: Domestic threats##
#############################


lm1 <- plm(general_varying~cmc_connection_varying*domestic+as.factor(year),  data=panel_year, model="within")

cluster.fit1 <- coeftest(lm1, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))

lm2 <- plm(general_varying~cmc_connection_varying*domestic+age+combat_post_1949*domestic+college*domestic+participated_long_march*domestic+commissar*domestic+minority*domestic+parent_CCP_leader*domestic+rural*domestic+as.factor(year),  data=panel_year, model="within")

cluster.fit2 <- coeftest(lm2, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))

lm3 <- plm(general_varying~cmc_connection_varying*domestic+age+combat_post_1949*domestic+college*domestic+participated_long_march*domestic+commissar*domestic+minority*domestic+parent_CCP_leader*domestic+rural*domestic+as.factor(year)*cohort_decade,  data=panel_year, model="within")

cluster.fit3 <- coeftest(lm3, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))

lm4 <- plm(general_varying~cmc_connection_varying*domestic+age+combat_post_1949*domestic+college*domestic+participated_long_march*domestic+commissar*domestic+minority*domestic+parent_CCP_leader*domestic+rural*domestic++as.factor(year)*as.factor(birth_year),  data=panel_year, model="within")

cluster.fit4 <- coeftest(lm4, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))


stargazer(lm1, lm2, lm3, lm4, type = "latex", 
          se = list(cluster.fit1[,"Std. Error"], cluster.fit2[,"Std. Error"],
                    cluster.fit3[,"Std. Error"], cluster.fit4[,"Std. Error"]), 
          omit=c("cohort_decade", "year", "Constant","age" , "college", "participated_long_march", "commissar", "combat_post_1949", "minority", "participated_long_march", "rural", "parent_CCP_leader"),
          omit.stat=c("f", "ser"),
          label=c("domestic_threat"),
          dep.var.labels = c("Promoted to General (Mean: 0.295)"),
          add.lines=list(c("Individual fixed effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
                         c("Year fixed effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
                         c("Domestic Threat X Controls", "", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
                         c("Year FE X Birth decade FE", "", "", "$\\checkmark$", ""),
                         c("Year FE X Birth year FE", "", "", "", "$\\checkmark$"),
                         c("Clusters", rep(length(unique(panel_year$unique_id)), 4))),
         covariate.labels = c("Career Tie to Current CMC Chairman", "Period of Domestic Threat", "Career Tie to Chairman X Domestic Threat"),
         title = "Promotion during periods of domestic threat.",
         star.char = c("+", "*", "**"),
         notes.append=FALSE,
         notes = c("PLA officers with a tie to the sitting top leader are especially likely to be promoted during periods of domestic threat, here operationalized as the period after the 1989 protests and after the 2012 Bo Xilai incident. See Table A13, p. A18 and Table A16, p. A21 for alternate operationalizations. Two-way fixed effects regression on a panel dataset of PLA officers with yearly observations. Outcome is promotion to general-grade position (military region commander or commissar and above). Robust standard errors are clustered by individual.$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01}"),
          out = "Table_2.tex")




############################
##Table 3: Foreign threats##
############################


lm1 <- plm(general_varying~combat_post_1949*foreign+as.factor(year),  data=panel_year, model="within")

cluster.fit1 <- coeftest(lm1, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))

lm2 <- plm(general_varying~combat_post_1949*foreign+age+cmc_connection_varying*foreign+college*foreign+participated_long_march*foreign+commissar*foreign+combat_post_1949*foreign+parent_CCP_leader*foreign+rural*foreign+as.factor(year),  data=panel_year, model="within")

cluster.fit2 <- coeftest(lm2, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))

lm3 <- plm(general_varying~combat_post_1949*foreign+age+cmc_connection_varying*foreign+college*foreign+participated_long_march*foreign+commissar*foreign+combat_post_1949*foreign+parent_CCP_leader*foreign+rural*foreign+as.factor(year)*cohort_decade,  data=panel_year, model="within")

cluster.fit3 <- coeftest(lm3, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))

lm4 <- plm(general_varying~combat_post_1949*foreign+age+cmc_connection_varying*foreign+college*foreign+participated_long_march*foreign+commissar*foreign+combat_post_1949*foreign+parent_CCP_leader*foreign+rural*foreign+as.factor(year)*as.factor(birth_year),  data=panel_year, model="within")

cluster.fit4 <- coeftest(lm4, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))



stargazer(lm1, lm2, lm3, lm4, type = "latex", 
          se = list(cluster.fit1[,"Std. Error"], cluster.fit2[,"Std. Error"],
                    cluster.fit3[,"Std. Error"], cluster.fit4[,"Std. Error"]), 
          omit=c("cohort_decade", "year", "Constant","age" , "college", "participated_long_march", "commissar", "cmc_connection_varying", "minority", "participated_long_march", "rural", "parent_CCP_leader"),
          omit.stat=c("f", "ser"),
          label=c("foreign_threat"),
          dep.var.labels = c("Promoted to General (Mean: 0.295)"),
          add.lines=list(c("Individual fixed effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
                         c("Year fixed effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
                         c("Foreign Threat X Controls", "", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
                         c("Year FE X Birth decade FE", "", "", "$\\checkmark$", ""),
                         c("Year FE X Birth year FE", "", "", "", "$\\checkmark$"),
                         c("Clusters", rep(length(unique(panel_year$unique_id)), 4))),
          covariate.labels = c("Period of Foreign Threat", "Combat Experience X Foreign Threat"),
          title = "Promotion during periods of foreign threat.",
          star.char = c("+", "*", "**"),
          notes.append=FALSE,
          notes = c("Officers with combat experience are especially likely to promoted during periods of foreign threat, here operationalized as the period of rising tensions with the United States in the early 2000s. See Table A14, p. A19 and Table A17, p. A22 for alternate operationalizations.  Two-way fixed effects regression on a panel dataset of PLA officers with yearly observations. Outcome is promotion to general-grade position (military region commander or commissar and above). Robust standard errors are clustered by individual. $^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01}"),
          out = "Table_3.tex")






