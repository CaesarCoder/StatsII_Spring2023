


###############################
##Appendix: Descriptive Stats##
###############################


stargazer(bio[,c("general", "general_rank", "cc_full_member","cc_full_alt_member", "politburo_member", "cmc",
                 "cmc_chair_connection_current", "cmc_chair_connection_current_alt",
                 "xi.network", "hu.network", "jiang.network", "deng.network",
                 "xi.network.alt", "hu.network.alt", "jiang.network.alt", 
                 "college", "postgrad",  "combat_post_1949", 
                 "participated_long_march",  "commissar", "minority", "parent_CCP_leader", "rural")],
          covariate.labels = c( "Full General-Level Position (MR Commander or Above)",  "Awarded Rank of Shangjiang (After 1987)",
                                "CCP Central Committee Full Member", "CCP Central Committee Full or Alternate",
                                "CCP Politboro Member", "CCP Central Military Commission Member",
                                "Career tie to top leader",  "Career tie to top leader (excluding MR)", 
                               "Xi Jinping network", "Hu Jintao network", "Jiang Zemin network", "Deng Xiaoping network",
                               "Xi Jinping network (excluding MR)", "Hu Jintao network (excluding MR)", "Jiang Zemin network (excluding MR)", 
                               "College-level graduate",  "Postgraduate training", "Combat Experience After 1949",
                                "Long March Veteran", "Served as Commissar", "Ethnic Minority", "Princeling", "Rural Birth"),
          omit.summary.stat = c("p25", "p75", "N"),
          out = "Table_A1.tex")


#########################################
##Appendix: Alternate Outcome Variables##
#########################################


lm1 <- lm(cc_full_member~cmc_chair_connection_current, data=bio)
lm2 <- lm(cc_full_member~combat_post_1949, data=bio)
lm3 <- lm(cc_full_member~cmc_chair_connection_current+combat_post_1949+college+participated_long_march+commissar+minority+parent_CCP_leader+rural+cohort_decade, data=bio)
lm4 <- lm(cc_full_alt_member~cmc_chair_connection_current, data=bio)
lm5 <- lm(cc_full_alt_member~combat_post_1949, data=bio)
lm6 <- lm(cc_full_alt_member~cmc_chair_connection_current+combat_post_1949+college+participated_long_march+commissar+minority+parent_CCP_leader+rural+cohort_decade, data=bio)

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
          label = "table_a3", 
          omit.stat=c("f", "ser"),
          dep.var.labels = c( "Central Committee Full Member",  "Central Committee Full or Alternate Member"),
          add.lines=list(c("Birth cohort fixed effects", "", "", "$\\checkmark$", "", "", "$\\checkmark$")),
          title = "Alternate Outcomes: Promotion to the Central Committee, Full and Alternate Members",
          star.char = c("+", "*", "**"),
          notes.append=FALSE,
          notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01}"),
          covariate.labels = c("Career Tie to Paramount Leader", "Combat Experience, Post-1949", "College-Level Education", "Long March Participant", "Political Commissar Experience", "Ethnic Minority", "Princeling", "Rural Birth"),
          out = "Table_A3.tex")



lm1 <- lm(politburo_member~cmc_chair_connection_current, data=bio)
lm2 <- lm(politburo_member~combat_post_1949, data=bio)
lm3 <- lm(politburo_member~cmc_chair_connection_current+combat_post_1949+college+participated_long_march+commissar+minority+parent_CCP_leader+rural+cohort_decade, data=bio)
lm4 <- lm(general_rank~cmc_chair_connection_current, data=bio)
lm5 <- lm(general_rank~combat_post_1949, data=bio)
lm6 <- lm(general_rank~cmc_chair_connection_current+combat_post_1949+college+participated_long_march+commissar+minority+parent_CCP_leader+rural+cohort_decade, data=bio)

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
          label = "table_a4", 
          omit.stat=c("f", "ser"),
          dep.var.labels = c( "Promoted to CCP Politburo", "Promoted to 3-Star General"),
          add.lines=list(c("Birth cohort fixed effects", "", "", "$\\checkmark$", "", "", "$\\checkmark$")),
          title = "Alternate Outcomes: Promotion to CCP Politburo and Three-Star General (Shangjiang)",
          star.char = c("+", "*", "**"),
          notes.append=FALSE,
          notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01}"),
          covariate.labels = c("Career Tie to Paramount Leader", "Combat Experience, Post-1949", "College-Level Education", "Long March Participant", "Political Commissar Experience", "Ethnic Minority", "Princeling", "Rural Birth"),
          out = "Table_A4.tex")



#############################################
##Appendix: Alternate Explanatory Variables##
#############################################

lm1 <- lm(general~cmc_chair_connection_current_alt, data=bio)
lm2 <- lm(general~combat_post_1949, data=bio)
lm3 <- lm(general~cmc_chair_connection_current_alt+combat_post_1949+college+participated_long_march+commissar+minority+parent_CCP_leader+rural+cohort_decade, data=bio)
lm4 <- lm(cmc~cmc_chair_connection_current_alt, data=bio)
lm5 <- lm(cmc~combat_post_1949, data=bio)
lm6 <- lm(cmc~cmc_chair_connection_current_alt+combat_post_1949+college+participated_long_march+commissar+minority+parent_CCP_leader+rural+cohort_decade, data=bio)

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
          label = "table_a5", 
          omit.stat=c("f", "ser"),
          dep.var.labels = c("Promoted to General", "Promoted to CMC"),
          add.lines=list(c("Birth cohort fixed effects", "", "", "$\\checkmark$", "", "", "$\\checkmark$")),
          title = "Alternate Explanatory Variable: Leader Connections Limited to Military District",
          star.char = c("+", "*", "**"),
          notes.append=FALSE,
          notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01}"),
          covariate.labels = c("Career Tie to Paramount Leader (Military District Only)", "Combat Experience, Post-1949", "College-Level Education", "Long March Participant", "Political Commissar Experience", "Ethnic Minority", "Princeling", "Rural Birth"),
          out = "Table_A5.tex")



#########################################################
##Appendix: Alternate Outcome and Explanatory Variables##
#########################################################


lm1 <- lm(cc_full_member~cmc_chair_connection_current_alt, data=bio)
lm2 <- lm(cc_full_member~combat_post_1949, data=bio)
lm3 <- lm(cc_full_member~cmc_chair_connection_current_alt+combat_post_1949+college+participated_long_march+commissar+minority+parent_CCP_leader+rural+cohort_decade, data=bio)
lm4 <- lm(cc_full_alt_member~cmc_chair_connection_current_alt, data=bio)
lm5 <- lm(cc_full_alt_member~combat_post_1949, data=bio)
lm6 <- lm(cc_full_alt_member~cmc_chair_connection_current_alt+combat_post_1949+college+participated_long_march+commissar+minority+parent_CCP_leader+rural+cohort_decade, data=bio)

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
          label = "table_a6", 
          omit.stat=c("f", "ser"),
          dep.var.labels = c( "Central Committee Full Member",  "Central Committee Full or Alternate Member"),
          add.lines=list(c("Birth cohort fixed effects", "", "", "$\\checkmark$", "", "", "$\\checkmark$")),
          title = "Alternate Explanatory Variable and Alternate Outcomes 1",
          star.char = c("+", "*", "**"),
          notes.append=FALSE,
          notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01}"),
          covariate.labels = c("Career Tie to Paramount Leader (Military District Only)", "Combat Experience, Post-1949", "College-Level Education", "Long March Participant", "Political Commissar Experience", "Ethnic Minority", "Princeling", "Rural Birth"),
          out = "Table_A6.tex")



lm1 <- lm(politburo_member~cmc_chair_connection_current_alt, data=bio)
lm2 <- lm(politburo_member~combat_post_1949, data=bio)
lm3 <- lm(politburo_member~cmc_chair_connection_current_alt+combat_post_1949+college+participated_long_march+commissar+minority+parent_CCP_leader+rural+cohort_decade, data=bio)
lm4 <- lm(general_rank~cmc_chair_connection_current_alt, data=bio)
lm5 <- lm(general_rank~combat_post_1949, data=bio)
lm6 <- lm(general_rank~cmc_chair_connection_current_alt+combat_post_1949+college+participated_long_march+commissar+minority+parent_CCP_leader+rural+cohort_decade, data=bio)

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
          label = "table_a7", 
          omit.stat=c("f", "ser"),
          dep.var.labels = c( "Promoted to CCP Politburo", "Promoted to 3 Star General"),
          add.lines=list(c("Birth cohort fixed effects", "", "", "$\\checkmark$", "", "", "$\\checkmark$")),
          title = "Alternate Explanatory Variable and Alternate Outcomes 2",
          star.char = c("+", "*", "**"),
          notes.append=FALSE,
          notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01}"),
          covariate.labels = c("Career Tie to Paramount Leader (Military District Only)", "Combat Experience, Post-1949", "College-Level Education", "Long March Participant", "Political Commissar Experience", "Ethnic Minority", "Princeling", "Rural Birth"),
          out = "Table_A7.tex")



#################################
##Appendix: Logistic Regression##
#################################

lm1 <- glm(general~cmc_chair_connection_current, data=bio, family=binomial(link="logit"))
lm2 <- glm(general~combat_post_1949, data=bio, family=binomial(link="logit"))
lm3 <- glm(general~cmc_chair_connection_current+combat_post_1949+college+participated_long_march+commissar+minority+parent_CCP_leader+rural+cohort_decade, data=bio, family=binomial(link="logit"))
lm4 <- glm(cmc~cmc_chair_connection_current, data=bio, family=binomial(link="logit"))
lm5 <- glm(cmc~combat_post_1949, data=bio, family=binomial(link="logit"))
lm6 <- glm(cmc~cmc_chair_connection_current+combat_post_1949+college+participated_long_march+commissar+minority+parent_CCP_leader+rural+cohort_decade, data=bio, family=binomial(link="logit"))

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
          label = "table_a8", 
          omit.stat=c("f", "ser"),
          dep.var.labels = c("Promoted to General", "Promoted to CMC"),
          add.lines=list(c("Birth cohort fixed effects", "", "", "$\\checkmark$", "", "", "$\\checkmark$")),
          title = "Logistic Regression, Main Table: Promotion to full general and to the CCP Central Military Commission",
          star.char = c("+", "*", "**"),
          notes.append=FALSE,
          notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01}"),
          covariate.labels = c("Career Tie to Paramount Leader", "Combat Experience, Post-1949", "College-Level Education", "Long March Participant", "Political Commissar Experience", "Ethnic Minority", "Princeling", "Rural Birth"),
          out = "Table_A8.tex")




lm1 <- glm(cc_full_member~cmc_chair_connection_current, data=bio, family=binomial(link="logit"))
lm2 <- glm(cc_full_member~combat_post_1949, data=bio, family=binomial(link="logit"))
lm3 <- glm(cc_full_member~cmc_chair_connection_current+combat_post_1949+college+participated_long_march+commissar+minority+parent_CCP_leader+rural+cohort_decade, data=bio, family=binomial(link="logit"))
lm4 <- glm(cc_full_alt_member~cmc_chair_connection_current, data=bio, family=binomial(link="logit"))
lm5 <- glm(cc_full_alt_member~combat_post_1949, data=bio, family=binomial(link="logit"))
lm6 <- glm(cc_full_alt_member~cmc_chair_connection_current+combat_post_1949+college+participated_long_march+commissar+minority+parent_CCP_leader+rural+cohort_decade, data=bio, family=binomial(link="logit"))

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
          label = "table_a9", 
          omit.stat=c("f", "ser"),
          dep.var.labels = c( "Central Committee Full Member",  "Central Committee Full or Alternate Member"),
          add.lines=list(c("Birth cohort fixed effects", "", "", "$\\checkmark$", "", "", "$\\checkmark$")),
          title = "Logistic Regression, Alternate Table: Promotion to CCP Central Committee",
          star.char = c("+", "*", "**"),
          notes.append=FALSE,
          notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01}"),
          covariate.labels = c("Career Tie to Paramount Leader", "Combat Experience, Post-1949", "College-Level Education", "Long March Participant", "Political Commissar Experience", "Ethnic Minority", "Princeling", "Rural Birth"),
          out = "Table_A9.tex")

lm1 <- glm(politburo_member~cmc_chair_connection_current, data=bio, family=binomial(link="logit"))
lm2 <- glm(politburo_member~combat_post_1949, data=bio, family=binomial(link="logit"))
lm3 <- glm(politburo_member~cmc_chair_connection_current+combat_post_1949+college+participated_long_march+commissar+minority, data=bio, family=binomial(link="logit"))
lm4 <- glm(general_rank~cmc_chair_connection_current, data=bio, family=binomial(link="logit"))
lm5 <- glm(general_rank~combat_post_1949, data=bio, family=binomial(link="logit"))
lm6 <- glm(general_rank~cmc_chair_connection_current+combat_post_1949+college+participated_long_march+commissar+minority+parent_CCP_leader+rural+cohort_decade, data=bio, family=binomial(link="logit"))

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
          label = "table_a10", 
          omit.stat=c("f", "ser"),
          dep.var.labels = c( "Promoted to CCP Politburo", "Promoted to 3-Star General"),
          add.lines=list(c("Birth cohort fixed effects", "", "", "$\\checkmark$", "", "", "$\\checkmark$")),
          title = "Logistic Regression, Alternate Table: Promotion to Politburo and Three-Star General (Shanjiang)",
          star.char = c("+", "*", "**"),
          notes.append=FALSE,
          notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01}"),
          covariate.labels = c("Career Tie to Paramount Leader", "Combat Experience, Post-1949", "College-Level Education", "Long March Participant", "Political Commissar Experience", "Ethnic Minority", "Princeling", "Rural Birth"),
          out = "Table_A10.tex")


#####################################
##Appendix: Subset to post-Deng Era##
#####################################


lm1 <- lm(general~cmc_chair_connection_current, data=bio, subset=post_deng==1)
lm2 <- lm(general~combat_post_1949, data=bio, subset=post_deng==1)
lm3 <- lm(general~cmc_chair_connection_current+combat_post_1949+college+participated_long_march+commissar+minority+parent_CCP_leader+rural, data=bio, subset=post_deng==1)
lm4 <- lm(cmc~cmc_chair_connection_current, data=bio, subset=post_deng==1)
lm5 <- lm(cmc~combat_post_1949, data=bio, subset=post_deng==1)
lm6 <- lm(cmc~cmc_chair_connection_current+combat_post_1949+college+participated_long_march+commissar+minority+parent_CCP_leader+rural, data=bio, subset=post_deng==1)

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
          label = "table_a11", 
          omit.stat=c("f", "ser"),
          dep.var.labels = c("Promoted to General", "Promoted to CMC"),
          title = "Post-Deng Officers Only: Promotion to full general-level position and to the CCP Central Military Commission",
          star.char = c("+", "*", "**"),
          notes.append=FALSE,
          notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01}"),
          covariate.labels = c("Career Tie to Paramount Leader", "Combat Experience, Post-1949", "College-Level Education", "Long March Participant", "Political Commissar Experience", "Ethnic Minority", "Princeling", "Rural Birth"),
          out = "Table_A11.tex")




##############################################
##Appendix: Panel Analysis of CMC Connection##
##############################################


lm1 <- plm(general_varying~cmc_connection_varying+as.factor(year),  data=panel_year, model="within")

cluster.fit1 <- coeftest(lm1, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))

lm2 <- plm(general_varying~cmc_connection_varying+age+as.factor(year),  data=panel_year, model="within")

cluster.fit2 <- coeftest(lm2, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))

lm3 <- plm(general_varying~cmc_connection_varying+age+as.factor(year)*cohort_decade,  data=panel_year, model="within")

cluster.fit3 <- coeftest(lm3, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))

lm4 <- plm(general_varying~cmc_connection_varying+age+as.factor(year)*as.factor(birth_year),  data=panel_year, model="within")

cluster.fit4 <- coeftest(lm4, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))


stargazer(lm1, lm2, lm3, lm4, type = "latex", 
          se = list(cluster.fit1[,"Std. Error"], cluster.fit2[,"Std. Error"],
                    cluster.fit3[,"Std. Error"], cluster.fit4[,"Std. Error"]), 
          omit=c("cohort_decade", "year", "Constant","age" , "college", "participated_long_march", "commissar", "combat_post_1949", "minority", "participated_long_march", "rural", "parent_CCP_leader"),
          omit.stat=c("f", "ser"),
          label=c("table_a12"),
          dep.var.labels = c("Promoted to General (Mean: 0.295)"),
          add.lines=list(c("Individual fixed effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
                         c("Year fixed effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
                         c("Domestic Threat X Controls", "", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
                         c("Year FE X Birth decade FE", "", "", "$\\checkmark$", ""),
                         c("Year FE X Birth year FE", "", "", "", "$\\checkmark$"),
                         c("Clusters", rep(length(unique(panel_year$unique_id)), 4))),
          covariate.labels = c("Career Tie to Current CMC Chairman"),
          star.char = c("+", "*", "**"),
          notes.append=FALSE,
          notes = c("Standard errors clustered by individual. $^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01}"),
          title = "Promotion to general, time-varying measure of career tie to CMC Chairman.",
          out = "Table_A12.tex")





######################################################
##Panel Analysis: Domestic threats alternate measure##
######################################################


lm1 <- plm(general_varying~cmc_connection_varying*domestic2+as.factor(year),  data=panel_year, model="within")

cluster.fit1 <- coeftest(lm1, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))

lm2 <- plm(general_varying~cmc_connection_varying*domestic2+age+combat_post_1949*domestic2+college*domestic2+participated_long_march*domestic2+commissar*domestic2+minority*domestic2+parent_CCP_leader*domestic2+rural*domestic2+as.factor(year),  data=panel_year, model="within")

cluster.fit2 <- coeftest(lm2, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))

lm3 <- plm(general_varying~cmc_connection_varying*domestic2+age+combat_post_1949*domestic2+college*domestic2+participated_long_march*domestic2+commissar*domestic2+minority*domestic2+parent_CCP_leader*domestic2+rural*domestic2+as.factor(year)*cohort_decade,  data=panel_year, model="within")

cluster.fit3 <- coeftest(lm3, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))

lm4 <- plm(general_varying~cmc_connection_varying*domestic2+age+combat_post_1949*domestic2+college*domestic2+participated_long_march*domestic2+commissar*domestic2+minority*domestic2+parent_CCP_leader*domestic2+rural*domestic2++as.factor(year)*as.factor(birth_year),  data=panel_year, model="within")

cluster.fit4 <- coeftest(lm4, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))


stargazer(lm1, lm2, lm3, lm4, type = "latex", 
          se = list(cluster.fit1[,"Std. Error"], cluster.fit2[,"Std. Error"],
                    cluster.fit3[,"Std. Error"], cluster.fit4[,"Std. Error"]), 
          omit=c("cohort_decade", "year", "Constant","age" , "college", "participated_long_march", "commissar", "combat_post_1949", "minority", "participated_long_march", "rural", "parent_CCP_leader"),
          omit.stat=c("f", "ser"),
          label=c("table_a13"),
          dep.var.labels = c("Promoted to General (Mean: 0.295)"),
          add.lines=list(c("Individual fixed effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
                         c("Year fixed effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
                         c("Domestic Threat X Controls", "", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
                         c("Year FE X Birth decade FE", "", "", "$\\checkmark$", ""),
                         c("Year FE X Birth year FE", "", "", "", "$\\checkmark$"),
                         c("Clusters", rep(length(unique(panel_year$unique_id)), 4))),
          covariate.labels = c("Career Tie to Current CMC Chairman", "Period of Domestic Threat", "Career Tie to Chairman X Domestic Threat"),
          star.char = c("+", "*", "**"),
          notes.append=FALSE,
          notes = c("Standard errors clustered by individual. $^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01}"),
          title = "Promotion during periods of domestic threat. Alternate measure of threat including 1978 transition to Deng.",
          out = "Table_A13.tex")




#####################################################
##Panel Analysis: Foreign threats alternate measure##
#####################################################


lm1 <- plm(general_varying~combat_post_1949*foreign2+as.factor(year),  data=panel_year, model="within")

cluster.fit1 <- coeftest(lm1, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))

lm2 <- plm(general_varying~combat_post_1949*foreign2+age+cmc_connection_varying*foreign2+college*foreign2+participated_long_march*foreign2+commissar*foreign2+combat_post_1949*foreign2+parent_CCP_leader*foreign2+rural*foreign2+as.factor(year),  data=panel_year, model="within")

cluster.fit2 <- coeftest(lm2, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))

lm3 <- plm(general_varying~combat_post_1949*foreign2+age+cmc_connection_varying*foreign2+college*foreign2+participated_long_march*foreign2+commissar*foreign2+combat_post_1949*foreign2+parent_CCP_leader*foreign2+rural*foreign2+as.factor(year)*cohort_decade,  data=panel_year, model="within")

cluster.fit3 <- coeftest(lm3, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))

lm4 <- plm(general_varying~combat_post_1949*foreign2+age+cmc_connection_varying*foreign2+college*foreign2+participated_long_march*foreign2+commissar*foreign2+combat_post_1949*foreign2+parent_CCP_leader*foreign2+rural*foreign2+as.factor(year)*as.factor(birth_year),  data=panel_year, model="within")

cluster.fit4 <- coeftest(lm4, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))



stargazer(lm1, lm2, lm3, lm4, type = "latex", 
          se = list(cluster.fit1[,"Std. Error"], cluster.fit2[,"Std. Error"],
                    cluster.fit3[,"Std. Error"], cluster.fit4[,"Std. Error"]), 
          omit=c("cohort_decade", "year", "Constant","age" , "college", "participated_long_march", "commissar", "cmc_connection_varying", "minority", "participated_long_march", "rural", "parent_CCP_leader"),
          omit.stat=c("f", "ser"),
          label=c("table_a14"),
          dep.var.labels = c("Promoted to General (Mean: 0.295)"),
          add.lines=list(c("Individual fixed effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
                         c("Year fixed effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
                         c("Foreign Threat X Controls", "", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
                         c("Year FE X Birth decade FE", "", "", "$\\checkmark$", ""),
                         c("Year FE X Birth year FE", "", "", "", "$\\checkmark$"),
                         c("Clusters", rep(length(unique(panel_year$unique_id)), 4))),
          covariate.labels = c("Period of Foreign Threat", "Combat Experience X Foreign Threat"),
          star.char = c("+", "*", "**"),
          notes.append=FALSE,
          notes = c("Standard errors clustered by individual. $^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01}"),
          title = "Promotion during periods of foreign threat. Alternate measure of threat including Vietnam conflict.",
          out = "Table_A14.tex")




###############################################################################
##Panel Analysis: Domestic threats and career ties excluding military regions##
###############################################################################


lm1 <- plm(general_varying~cmc_connection_varying_alt*domestic+as.factor(year),  data=panel_year, model="within")

cluster.fit1 <- coeftest(lm1, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))

lm2 <- plm(general_varying~cmc_connection_varying_alt*domestic+age+combat_post_1949*domestic+college*domestic+participated_long_march*domestic+commissar*domestic+minority*domestic+parent_CCP_leader*domestic+rural*domestic+as.factor(year),  data=panel_year, model="within")

cluster.fit2 <- coeftest(lm2, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))

lm3 <- plm(general_varying~cmc_connection_varying_alt*domestic+age+combat_post_1949*domestic+college*domestic+participated_long_march*domestic+commissar*domestic+minority*domestic+parent_CCP_leader*domestic+rural*domestic+as.factor(year)*cohort_decade,  data=panel_year, model="within")

cluster.fit3 <- coeftest(lm3, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))

lm4 <- plm(general_varying~cmc_connection_varying_alt*domestic+age+combat_post_1949*domestic+college*domestic+participated_long_march*domestic+commissar*domestic+minority*domestic+parent_CCP_leader*domestic+rural*domestic++as.factor(year)*as.factor(birth_year),  data=panel_year, model="within")

cluster.fit4 <- coeftest(lm4, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))


stargazer(lm1, lm2, lm3, lm4, type = "latex", 
          se = list(cluster.fit1[,"Std. Error"], cluster.fit2[,"Std. Error"],
                    cluster.fit3[,"Std. Error"], cluster.fit4[,"Std. Error"]), 
          omit=c("cohort_decade", "year", "Constant","age" , "college", "participated_long_march", "commissar", "combat_post_1949", "minority", "participated_long_march", "rural", "parent_CCP_leader"),
          omit.stat=c("f", "ser"),
          label=c("table_a15"),
          dep.var.labels = c("Promoted to General (Mean: 0.295)"),
          add.lines=list(c("Individual fixed effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
                         c("Year fixed effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
                         c("Domestic Threat X Controls", "", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
                         c("Year FE X Birth decade FE", "", "", "$\\checkmark$", ""),
                         c("Year FE X Birth year FE", "", "", "", "$\\checkmark$"),
                         c("Clusters", rep(length(unique(panel_year$unique_id)), 4))),
          covariate.labels = c("Career Tie to Current CMC Chairman (Alt Measure)", "Period of Domestic Threat", "Career Tie to Chairman (Alt Measure) Domestic Threat"),
          star.char = c("+", "*", "**"),
          notes.append=FALSE,
          notes = c("Standard errors clustered by individual. $^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01}"),
          title = "Promotion during periods of domestic threat. Alternate measure of ties, excluding Military Region.",
          out = "Table_A15.tex")




################################################################
##Appendix: Domestic Threat Measure, Excluding Different Years##
################################################################



lm1 <- plm(general_varying~cmc_connection_varying*domestic1990+as.factor(year),  data=panel_year, model="within")

cluster.fit1 <- coeftest(lm1, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))


lm2 <- plm(general_varying~cmc_connection_varying*domestic1991+as.factor(year),  data=panel_year, model="within")

cluster.fit2 <- coeftest(lm2, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))


lm3 <- plm(general_varying~cmc_connection_varying*domestic1992+as.factor(year),  data=panel_year, model="within")

cluster.fit3 <- coeftest(lm3, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))


lm4 <- plm(general_varying~cmc_connection_varying*domestic1993+as.factor(year),  data=panel_year, model="within")

cluster.fit4 <- coeftest(lm4, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))


lm5 <- plm(general_varying~cmc_connection_varying*domestic2012+as.factor(year),  data=panel_year, model="within")

cluster.fit5 <- coeftest(lm5, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))


lm6 <- plm(general_varying~cmc_connection_varying*domestic2013+as.factor(year),  data=panel_year, model="within")

cluster.fit6 <- coeftest(lm6, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))


lm7 <- plm(general_varying~cmc_connection_varying*domestic2014+as.factor(year),  data=panel_year, model="within")

cluster.fit7 <- coeftest(lm7, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))


lm8 <- plm(general_varying~cmc_connection_varying*domestic2015+as.factor(year),  data=panel_year, model="within")

cluster.fit8 <- coeftest(lm8, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))


stargazer(lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8, type = "latex", 
          omit=c("year"),
          se = list(cluster.fit1[,"Std. Error"], cluster.fit2[,"Std. Error"],
                    cluster.fit3[,"Std. Error"], cluster.fit4[,"Std. Error"],
                    cluster.fit5[,"Std. Error"], cluster.fit6[,"Std. Error"],
                    cluster.fit7[,"Std. Error"], cluster.fit8[,"Std. Error"]), 
          label = "table_a16", 
          omit.stat=c("f", "ser"),
          dep.var.labels = c("Promoted to Central military Commission"),
          title = "Alternative Explanatory Variable: Exclusion of individual years counted as a year of domestic threat. For legibility, domestic threat coefficients are not shown.",
          covariate.labels = c("Career Tie to Current CMC Chair", "Domestic Threat (Excluding 1990)", "Domestic Threat (Excluding 1991)", "Domestic Threat (Excluding 1992)", "Domestic Threat (Excluding 1993)", "Domestic Threat (Excluding 2012)", "Domestic Threat (Excluding 2013)", "Domestic Threat (Excluding 2014)", "Domestic Threat (Excluding 2015)",
                               "Domestic Threat (Excluding 1990) X Career Tie", "Domestic Threat (Excluding 1991) X Career Tie", "Domestic Threat (Excluding 1992) X Career Tie", "Domestic Threat (Excluding 1993) X Career Tie", "Domestic Threat (Excluding 2012) X Career Tie", "Domestic Threat (Excluding 2013) X Career Tie", "Domestic Threat (Excluding 2014) X Career Tie", "Domestic Threat (Excluding 2015) X Career Tie"),
          add.lines=list(c("Individual fixed effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
                         c("Year fixed effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$")),
          star.char = c("+", "*", "**"),
          notes.append=FALSE,
          notes = c("Standard errors clustered by individual. $^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01}"),
          out = "Table_A16.tex")



###############################################################
##Appendix: Foreign Threat Measure, Excluding Different Years##
###############################################################


lm1 <- plm(general_varying~combat_post_1949*foreign2000+as.factor(year),  data=panel_year, model="within")

cluster.fit1 <- coeftest(lm1, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))


lm2 <- plm(general_varying~combat_post_1949*foreign2001+as.factor(year),  data=panel_year, model="within")

cluster.fit2 <- coeftest(lm2, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))


lm3 <- plm(general_varying~combat_post_1949*foreign2002+as.factor(year),  data=panel_year, model="within")

cluster.fit3 <- coeftest(lm3, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))


lm4 <- plm(general_varying~combat_post_1949*foreign2003+as.factor(year),  data=panel_year, model="within")

cluster.fit4 <- coeftest(lm4, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))



stargazer(lm1, lm2, lm3, lm4,type = "latex", 
          omit=c("year"),
          label = "table_a17", 
          se = list(cluster.fit1[,"Std. Error"], cluster.fit2[,"Std. Error"],
                    cluster.fit3[,"Std. Error"], cluster.fit4[,"Std. Error"]),
          omit.stat=c("f", "ser"),
          dep.var.labels = c("Promoted to Central military Commission"),
          title = "Alternative Explanatory Variable: Exclusion of individual years counted as a year of foreign threat. For legibility, foreign threat coefficients are not shown.",
          covariate.labels = c("Foreign Threat (Excluding 2000)", "Foreign Threat (Excluding 2001)", "Foreign Threat (Excluding 2002)", "Foreign Threat (Including 2003)",
                               "Combat Exp X Foreign Threat (Excluding 2000)", "Combat Exp X Foreign Threat (Excluding 2001)", "Combat Exp X Foreign Threat (Excluding 2002)", "Combat Exp X Foreign Threat (Including 2003)"),
          add.lines=list(c("Individual fixed effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
                         c("Year fixed effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$")),
          star.char = c("+", "*", "**"),
          notes.append=FALSE,
          notes = c("Standard errors clustered by individual. $^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01}"),
          out = "Table_A17.tex")

###########################################################
##Appendix: Cross-sectional career tie analysis by leader##
###########################################################


lm1 <- lm(general_deng~deng.network, data=bio)
lm2 <- lm(general_deng~deng.network+combat_post_1949+college+participated_long_march+commissar+minority+parent_CCP_leader+rural+cohort_decade, data=bio)
lm3 <- lm(general_jiang~jiang.network, data=bio)
lm4 <- lm(general_jiang~jiang.network+combat_post_1949+college+participated_long_march+commissar+minority+parent_CCP_leader+rural+cohort_decade, data=bio)
lm5 <- lm(general_hu~hu.network, data=bio)
lm6 <- lm(general_hu~hu.network+combat_post_1949+college+participated_long_march+commissar+minority+parent_CCP_leader+rural+cohort_decade, data=bio)
lm7 <- lm(general_xi~xi.network, data=bio)
lm8 <- lm(general_xi~xi.network+combat_post_1949+college+participated_long_march+commissar+minority+parent_CCP_leader+rural+cohort_decade, data=bio)

stargazer(lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8, type = "latex", 
          omit=c("combat_post_1949", "college", "participated_long_march", "commissar", "minority", "parent_CCP_leader", "rural", "cohort_decade"),
          label = "table_a18", 
          omit.stat=c("f", "ser"),
          dep.var.labels = c("Promoted to General (Deng)", "Promoted to General (Jiang)", "Promoted to General (Hu)", "Promoted to General (Xi)"),
          title = "Cross-Sectional Measure of Promotion to General by CMC Chairman, Restricted to Generals Eligible For Promotion During Each Chairmans' Term.",
          covariate.labels = c("Career Tie to Deng Xiaoping", "Career Tie to Jiang Zemin", "Career Tie to Hu Jintao", "Career Tie to Xi Jinping"),
          star.char = c("+", "*", "**"),
          notes.append=FALSE,
          notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01}"),
          out = "Table_A18.tex")


lm1 <- lm(cmc_deng~deng.network, data=bio)
lm2 <- lm(cmc_deng~deng.network+combat_post_1949+college+participated_long_march+commissar+minority+parent_CCP_leader+rural+cohort_decade, data=bio)
lm3 <- lm(cmc_jiang~jiang.network, data=bio)
lm4 <- lm(cmc_jiang~jiang.network+combat_post_1949+college+participated_long_march+commissar+minority+parent_CCP_leader+rural+cohort_decade, data=bio)
lm5 <- lm(cmc_hu~hu.network, data=bio)
lm6 <- lm(cmc_hu~hu.network+combat_post_1949+college+participated_long_march+commissar+minority+parent_CCP_leader+rural+cohort_decade, data=bio)
lm7 <- lm(cmc_xi~xi.network, data=bio)
lm8 <- lm(cmc_xi~xi.network+combat_post_1949+college+participated_long_march+commissar+minority+parent_CCP_leader+rural+cohort_decade, data=bio)

stargazer(lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8, type = "latex", 
          omit=c("combat_post_1949", "college", "participated_long_march", "commissar", "minority", "parent_CCP_leader", "rural", "cohort_decade"),
          label = "table_a19", 
          omit.stat=c("f", "ser"),
          dep.var.labels = c("Promoted to CMC (Deng)", "Promoted to CMC (Jiang)", "Promoted to CMC (Hu)", "Promoted to CMC (Xi)"),
          title = "Cross-Sectional Measure of Promotion to CMC by CMC Chairman, Restricted to Generals Eligible For Promotion During Each Chairmans' Term.",
          covariate.labels = c("Career Tie to Deng Xiaoping", "Career Tie to Jiang Zemin", "Career Tie to Hu Jintao", "Career Tie to Xi Jinping"),
          add.lines=list(c("Control variables", "", "$\\checkmark$","", "$\\checkmark$","", "$\\checkmark$", "", "$\\checkmark$")),
          star.char = c("+", "*", "**"),
          notes.append=FALSE,
          notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01}"),
          out = "Table_A19.tex")


######################################
##Appendix: Panel Analysis by Leader##
######################################


lm1 <- plm(general_varying~deng.varying+jiang.varying+hu.varying+xi.varying+as.factor(year),  data=panel_year, model="within")

cluster.fit1 <- coeftest(lm1, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))

lm2 <- plm(general_varying~deng.varying+jiang.varying+hu.varying+xi.varying+age+as.factor(year),  data=panel_year, model="within")

cluster.fit2 <- coeftest(lm2, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))

lm3 <- plm(general_varying~deng.varying+jiang.varying+hu.varying+xi.varying+age+as.factor(year)*cohort_decade,  data=panel_year, model="within")

cluster.fit3 <- coeftest(lm3, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))

lm4 <- plm(general_varying~deng.varying+jiang.varying+hu.varying+xi.varying+age+as.factor(year)*as.factor(birth_year),  data=panel_year, model="within")

cluster.fit4 <- coeftest(lm4, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))

stargazer(lm1, lm2, lm3, lm4, type = "latex", 
          se = list(cluster.fit1[,"Std. Error"], cluster.fit2[,"Std. Error"],
                    cluster.fit3[,"Std. Error"], cluster.fit4[,"Std. Error"]), 
          omit=c("cohort_decade", "year", "Constant","age" , "college", "participated_long_march", "commissar", "combat_post_1949", "minority", "participated_long_march", "rural", "parent_CCP_leader"),
          omit.stat=c("f", "ser"),
          label=c("table_a20"),
          dep.var.labels = c("Promoted to General (Mean: 0.295)"),
          add.lines=list(c("Individual fixed effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
                         c("Year fixed effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
                         c("Domestic Threat X Controls", "", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
                         c("Year FE X Birth decade FE", "", "", "$\\checkmark$", ""),
                         c("Year FE X Birth year FE", "", "", "", "$\\checkmark$"),
                         c("Clusters", rep(length(unique(panel_year$unique_id)), 4))),
          covariate.labels = c("Career Tie to Deng Xiaoping", "Career Tie to Jiang Zemin", "Career Tie to Hu Jintao", "Career Tie to Xi Jinping"),
          title = "Promotion to general, time-varying measure of career tie to CMC Chairman.",
          star.char = c("+", "*", "**"),
          notes.append=FALSE,
          notes = c("Standard errors clustered by individual. $^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01}"),
          out = "Table_A20.tex")




#########################################
##Appendix: Competence-Loyalty Tradeoff##
#########################################


lm1 <- plm(general_varying~cmc_connection_varying*education_level+as.factor(year),  data=panel_year, model="within")

cluster.fit1 <- coeftest(lm1, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))

lm2 <- plm(general_varying~cmc_connection_varying*college+as.factor(year),  data=panel_year, model="within")

cluster.fit2 <- coeftest(lm2, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))

lm3 <- plm(general_varying~cmc_connection_varying*postgrad+as.factor(year),  data=panel_year, model="within")

cluster.fit3 <- coeftest(lm3, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))

lm4 <- plm(general_varying~cmc_connection_varying*combat_post_1949+as.factor(year),  data=panel_year, model="within")

cluster.fit4 <- coeftest(lm4, vcov = function(x) 
  plm::vcovHC(x, method = 'white1', cluster = 'group', type = 'HC3'))


stargazer(lm1, lm2, lm3, lm4, type = "latex", 
          se = list(cluster.fit1[,"Std. Error"], cluster.fit2[,"Std. Error"],
                    cluster.fit3[,"Std. Error"], cluster.fit4[,"Std. Error"]), 
          omit=c("year"),
          omit.stat=c("f", "ser"),
          label=c("table_a21"),
          dep.var.labels = c("Promoted to General (Mean: 0.295)"),
          add.lines=list(c("Individual fixed effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
                         c("Year fixed effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
                         c("Clusters", rep(length(unique(panel_year$unique_id)), 4))),
          covariate.labels = c("Career Tie to Current CMC Chairman", "Career Tie X Education (Continuous Variable)", "Career Tie X College Degree", "Career Tie X Graduate Degree", "Career Tie X Combat Experience"),
          title = "The Competence-Loyalty Tradeoff: Generals with more education and connection to sitting CMC Chairman less likely to be promoted.",
          star.char = c("+", "*", "**"),
          notes.append=FALSE,
          notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01}"),
          out = "Table_A21.tex")
