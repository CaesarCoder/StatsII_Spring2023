####################
#REPLICATION FILES: VARIABLE CREATION
#Article: "How the Party Commands the Gun: The Foreign-Domestic Threat Dilemma in China"
#Authors: Daniel Mattingly
#This Version: July 19, 2022

################
###IMPORTANT!###
################

#If you run this file from the RStudio IDE please ensure that you have opened the file with UTF-8 encoding
#This ensure Chinese will load properly. 
#You may need to go to the following:
#1. Click the file menu
#2. select "Reopen with encoding" 
#3. In the pop-up menu select "UTF-8 (system default)"
#4. Select "Ok"
#If you do not see Chinese characters on lines 236 to 288, something has gone wrong!

####################
#Description: Running this script will create the key variables used in the analysis.
#The script was written with R version 4.0.3 on Mac OS Big Sur
####################


################################
##Outcomes: Promotion Measures##
################################


##
##Rank Achieved By Each Officer
##

##Code as being Military Region Region Leader (正大军区职) or Equivalent 
##These positions are one grade below the Central Military Commission
##In the Reform Era, the primary rank for officers holding these positions is three-star general (上将).


positions$general_rank <- 0
positions$general_rank[which(positions$position_id %in% c(100, 201, 301, 303, 401, 403, 501, 503, 601, 603, 701, 703, 801, 803, 1201,  
                                                          1203, 1301, 1303,1401, 1403, 1501, 1503, 1601, 1603, 1701, 1703, 1801, 1803,
                                                          1901, 1903, 2001, 2003, 2101, 2103, 2201, 2203, 2301, 2303, 2401, 2403,
                                                          2501, 2503, 2601, 2603, 2701, 2703))] <- 1


##
##Code individuals who ever held a position of MR Region Leader or Equivalent
##

bio$general <- 0
bio$general[bio$name %in% positions$name[positions$general_rank==1]] <- 1

##
##Indicator for whether an officer served in the Central Military Commmission
##

bio$cmc[is.na(bio$cmc)] <- 0



##
##Code individuals who achived the rank of full general (上将)
##Note that after initial promotions in the 1950s, this system was inactive until 1988
##

bio$general_rank <- 0
bio$general_rank[which(bio$shangjiang_year>1949)] <- 1


##
##Variable for whether individual was a Central Committee full member in the 12th to 18th party congresses
##

bio$cc_full_member <- NA

for(i in 1:nrow(bio)){
  bio$cc_full_member[i] <- length(which(bio[,paste0("cc", 12:18)][i,]<3000))
}

bio$cc_full_member[bio$cc_full_member>1] <- 1


##
##Variable for whether individual was a Central Committee full member in the 12th to 18th party congresses
##

bio$cc_full_alt_member <- NA

for(i in 1:nrow(bio)){
  bio$cc_full_alt_member[i] <- length(which(bio[,paste0("cc", 12:18)][i,]<4000))
}

bio$cc_full_alt_member[bio$cc_full_alt_member>1] <- 1



##
##Variable for whether individual was a Politburo member in the 12th to 18th party congresses
##

bio$politburo_member <- NA

for(i in 1:nrow(bio)){
  bio$politburo_member[i] <- length(which(bio[,paste0("cc", 12:18)][i,]<2000))
}

bio$politburo_member[bio$politburo_member>1] <- 1


##
##Variable for whether individuals were promoted to general in the Xi Era 
##Restricted to generals active in the Xi Era
##

bio$general_xi <- NA
bio$general_xi[bio$name %in% positions$name[which(positions$start_date_year>=2012)]] <- 0
bio$general_xi[which(bio$shangjiang_year>2012&bio$shangjiang_year<2018)] <- 1


##
##Variable for whether individuals were promoted to general in the Hu Era
##Restricted to generals active in the Hu Era
##

bio$general_hu <- NA
bio$general_hu[bio$name %in% positions$name[which(positions$start_date_year>=2004&positions$start_date_year<2012)]] <- 0
bio$general_hu[which(bio$shangjiang_year>=2004&bio$shangjiang_year<2012)] <- 1

##
##Variable for whether individuals were promoted to general in the Jiang Era
##Restricted to generals active in the Jiang Era
##

bio$general_jiang <- NA
bio$general_jiang[bio$name %in% positions$name[which(positions$start_date_year>1989&positions$start_date_year<2004)]] <- 0
bio$general_jiang[which(bio$shangjiang_year>1989&bio$shangjiang_year<=2004)] <- 1

##
##Variable for whether individuals were promoted to general in the Deng Era
##Restricted to generals active in the Deng Era
##

bio$general_deng <- NA
bio$general_deng[bio$name %in% positions$name[which(positions$start_date_year>=1978&positions$start_date_year<=1989)]] <- 0
bio$general_deng[which(bio$shangjiang_year==1988)] <- 1

##
##Variable for whether individuals were promoted to the Central Committee in the Xi Era 
##Restricted to generals active in the Xi Era
##

bio$cc_xi <- NA
bio$cc_xi[bio$name %in% positions$name[which(positions$start_date_year>=2012&positions$start_date_year<2018)]] <- 0
bio$cc_xi[which(bio$cc18<3000)] <- 1

##
##Variable for whether individuals were promoted to the Central Committee in the Hu Era 
##Restricted to generals active in the Hu Era
##

bio$cc_hu <- NA
bio$cc_hu[bio$name %in% positions$name[which(positions$start_date_year>=2004&positions$start_date_year<2012)]] <- 0
bio$cc_hu[which(bio$cc16<3000|bio$cc17<3000)] <- 1

##
##Variable for whether individuals were promoted to the Central Committee in the Jiang Era 
##Restricted to generals active in the Jiang Era
##

bio$cc_jiang <- NA
bio$cc_jiang[bio$name %in% positions$name[which(positions$start_date_year>1989&positions$start_date_year<2004)]] <- 0
bio$cc_jiang[which(bio$cc14<3000|bio$cc15<3000)] <- 1

##
##Variable for whether individuals were promoted to the Central Committee in the Deng Era 
##Restricted to generals active in the Deng Era
##

bio$cc_deng <- NA
bio$cc_deng[bio$name %in% positions$name[which(positions$start_date_year<=1989)]] <- 0
bio$cc_deng[which(bio$cc12<3000|bio$cc13<3000)] <- 1


##
##Variable for whether individuals were promoted to the Central Military Commission in the Xi Era 
##Restricted to generals active in the Xi Era
##

bio$cmc_xi <- NA
bio$cmc_xi[bio$name %in% positions$name[which(positions$start_date_year>=2012)]] <- 0
bio$cmc_xi[which(bio$name %in% positions$name[which(positions$position_id==100&positions$start_date_year>=2012)])] <- 1

##
##Variable for whether individuals were promoted to the Central Military Commission in the Hu Era 
##Restricted to generals active in the Hu Era
##

bio$cmc_hu <- NA
bio$cmc_hu[bio$name %in% positions$name[which(positions$start_date_year>=2004&positions$start_date_year<2012)]] <- 0
bio$cmc_hu[which(bio$name %in% positions$name[which(positions$position_id==100&positions$start_date_year>=2004&positions$start_date_year<2012)])] <- 1

##
##Variable for whether individuals were promoted to the Central Military Commission in the Jiang Era 
##Restricted to generals active in the Jiang Era
##

bio$cmc_jiang <- NA
bio$cmc_jiang[bio$name %in% positions$name[which(positions$start_date_year>=1989&positions$start_date_year<2004)]] <- 0
bio$cmc_jiang[which(bio$name %in% positions$name[which(positions$position_id==100&positions$start_date_year>=1989&positions$start_date_year<2004)])] <- 1

##
##Variable for whether individuals were promoted to the Central Military Commission in the Deng Era 
##Restricted to generals active in the Deng Era
##

bio$cmc_deng <- NA
bio$cmc_deng[bio$name %in% positions$name[which(positions$start_date_year>=1989&positions$start_date_year<2004)]] <- 0
bio$cmc_deng[which(bio$name %in% positions$name[which(positions$position_id==100&positions$start_date_year>=1978&positions$start_date_year<1989)])] <- 1



##########################################################
##Explanatory Variables: Connections to paramount leader##
##########################################################


######
#####Code service in different regions in the career data
#####

##Nanjing Military Region

career$nanjing.mr <- 0
career$nanjing.mr[grep("南京军区", career$institution)] <- 1

##Chengdu Military Region

career$chengdu.mr <- 0
career$chengdu.mr[grep("成都军区", career$institution)] <- 1



##Guizhou Military District

career$guizhou.md <- 0
career$guizhou.md[grep("贵州省军区", career$institution)] <- 1
career$guizhou.md[grep("贵州军区", career$institution)] <- 1

##Tibet Military District 

career$xizang.md <- 0
career$xizang.md[grep("西藏军区", career$institution)] <- 1
career$xizang.md[grep("西藏总队", career$institution)] <- 1


##Zhejiang Military District

career$zhejiang.md <- 0
career$zhejiang.md[grep("浙江军区", career$institution)] <- 1
career$zhejiang.md[grep("浙江省军区", career$institution)] <- 1


##Fujian Military District, Including 31st Group Army

career$fujian.md <- 0
career$fujian.md[grep("空军第八军", career$institution)] <- 1
career$fujian.md[grep("第十三集团", career$institution)] <- 1
career$fujian.md[grep("第31军", career$institution)] <- 1
career$fujian.md[grep("福建", career$institution)] <- 1

##Fuzhou Military Sub-District and related institutions, including 31st Group Army

career$fuzhou.msd <- 0
career$fuzhou.msd[grep("福州", career$institution)] <- 1
career$fuzhou.msd[grep("空军第八军", career$institution)] <- 1

##Zhejiang Military District

career$shanghai.garrison <- 0
career$shanghai.garrison[grep("上海", career$institution)] <- 1


##General Staff Department of the Central Military Commission

career$general_staff <- 0
career$general_staff[grep("军事委员会, 总参谋部", career$institution)] <- 1


######
#####Code connections to each paramount leader
#####

##Connections to Deng Xiaoping based on service in General Staff office 
##when Deng was Chief of Staff or Second Field Army, of which Deng was Commissar

bio$deng.network <- 0
bio$deng.network[bio$unique_id %in% career$unique_id[which(career$start_year<=1980&career$end_year>=1975&career$general_staff==1)]] <- 1
bio$deng.network[which(bio$second_field==1)] <- 1

##Connections to Jiang Zemin
##Based on concurrent service in Shanghai

bio$jiang.network <- 0
bio$jiang.network[bio$unique_id %in% career$unique_id[which(career$start_year<=1989&career$end_year>=1985&career$shanghai.garrison==1)]] <- 1
bio$jiang.network[bio$unique_id %in% career$unique_id[which(career$start_year<=1989&career$end_year>=1985&career$nanjing.mr==1)]] <- 1

##Connections to Hu Jintao
##Based on concurrent service in Guizhou and Tibet

bio$hu.network <- 0
bio$hu.network[bio$unique_id %in% career$unique_id[which(career$start_year<=1988&career$end_year>=1985&career$guizhou.md==1)]] <- 1
bio$hu.network[bio$unique_id %in% career$unique_id[which(career$start_year<=1992&career$end_year>=1985&career$chengdu.mr==1)]] <- 1
bio$hu.network[bio$unique_id %in% career$unique_id[which(career$start_year<=1992&career$end_year>=1989&career$xizang.md==1)]] <- 1

##Connections to Xi Jinping
##Based on concurrent service in Shanghai, Zhejiang, Fujian, Fuzhou

bio$xi.network <- 0
bio$xi.network[bio$unique_id %in% career$unique_id[which(career$start_year<=2007&career$end_year>1999&career$nanjing.mr==1)]] <- 1
bio$xi.network[bio$unique_id %in% career$unique_id[which(career$start_year<=2007&career$end_year>2002&career$zhejiang.md==1)]] <- 1
bio$xi.network[bio$unique_id %in% career$unique_id[which(career$start_year<=2002&career$end_year>1999&career$fujian.md==1)]] <- 1
bio$xi.network[bio$unique_id %in% career$unique_id[which(career$start_year<=1996&career$end_year>1990&career$fuzhou.msd==1)]] <- 1
bio$xi.network[bio$unique_id %in% career$unique_id[which(career$start_year<=2007&career$end_year>=2007&career$shanghai.garrison==1)]] <- 1

#Zhang Youxia's father fought alongside Xi Jinping's father in the revolution
#and Xi and Zhang reportedly have known each other since they were children.
#Including or excluding Zhang Youxia does not alter the results but is included for completeness.

bio$xi.network[bio$name=="张又侠"] <- 1

##Alternative measures of connections to Xi excluding Military Regions

bio$xi.network.alt <- 0
bio$xi.network.alt[bio$unique_id %in% career$unique_id[which(career$start_year<=2007&career$end_year>2002&career$zhejiang.md==1)]] <- 1
bio$xi.network.alt[bio$unique_id %in% career$unique_id[which(career$start_year<=2002&career$end_year>1999&career$fujian.md==1)]] <- 1
bio$xi.network.alt[bio$unique_id %in% career$unique_id[which(career$start_year<=1996&career$end_year>1990&career$fuzhou.msd==1)]] <- 1
bio$xi.network.alt[bio$unique_id %in% career$unique_id[which(career$start_year<=2007&career$end_year>=2007&career$shanghai.garrison==1)]] <- 1

##Alternative measures of connections to Jiang excluding Military Regions

bio$jiang.network.alt <- 0
bio$jiang.network.alt[bio$unique_id %in% career$unique_id[which(career$start_year<=1989&career$end_year>=1985&career$shanghai.garrison==1)]] <- 1

##Alternative measures of connections to Hu excluding Military Regions

bio$hu.network.alt <- 0
bio$hu.network.alt[bio$unique_id %in% career$unique_id[which(career$start_year<=1988&career$end_year>=1985&career$guizhou.md==1)]] <- 1
bio$hu.network.alt[bio$unique_id %in% career$unique_id[which(career$start_year<=1992&career$end_year>=1989&career$xizang.md==1)]] <- 1



######
#####Code connections to each paramount leader
#####

##
##Connected to any CMC chairman
##

bio$cmc_chair_connection <- 0
bio$cmc_chair_connection[which(bio$xi.network==1|bio$jiang.network==1|bio$hu.network==1|bio$deng.network==1)] <- 1

##
##Connected to any CMC chairman excluding military regions
##

bio$cmc_chair_connection_alt <- 0
bio$cmc_chair_connection_alt[which(bio$xi.network.alt==1|bio$jiang.network.alt==1|bio$hu.network.alt==1|bio$deng.network==1)] <- 1

##
##Connections to current CMC chairman
##Excludes connections when general served on Central Committee during prior CMC chair
##




bio$cmc_chair_connection_current <- bio$cmc_chair_connection
bio$cmc_chair_connection_current[which(bio$xi.network==1&bio$hu.network==0&bio$jiang.network==0&bio$deng.network==0&
                                         (bio$cc12<3000|bio$cc13<3000|bio$cc14<3000|bio$cc15<3000|bio$cc16<3000|bio$cc17<3000)&(is.na(bio$cc18)&is.na(bio$cc19)))] <- NA
bio$cmc_chair_connection_current[which(bio$xi.network==0&bio$hu.network==1&bio$jiang.network==0&bio$deng.network==0&
                                         (bio$cc12<3000|bio$cc13<3000|bio$cc14<3000|bio$cc15<3000)&(is.na(bio$cc16)&is.na(bio$cc17)))] <- NA
bio$cmc_chair_connection_current[which(bio$xi.network==0&bio$hu.network==0&bio$jiang.network==1&bio$deng.network==0&
                                         (bio$cc12<3000|bio$cc13<3000)&(is.na(bio$cc14)&is.na(bio$cc15)))] <- NA
bio$cmc_chair_connection_current[which(bio$xi.network==0&bio$hu.network==0&bio$jiang.network==0&bio$deng.network==1&
                                         (bio$cc15<3000|bio$cc16<3000)&(is.na(bio$cc12)&is.na(bio$cc13)))] <- NA




##
##Connections to current CMC chairman excluding military regions
##(Excludes connections when general served on Central Committee during prior or subsequent CMC chair)
##


bio$cmc_chair_connection_current_alt <- bio$cmc_chair_connection_alt
bio$cmc_chair_connection_current_alt[which(bio$xi.network.alt==1&bio$hu.network.alt==0&bio$jiang.network.alt==0&bio$deng.network==0&
                                             (bio$cc12<3000|bio$cc13<3000|bio$cc14<3000|bio$cc15<3000|bio$cc16<3000|bio$cc17<3000)&(is.na(bio$cc18)&is.na(bio$cc19)))] <- NA
bio$cmc_chair_connection_current_alt[which(bio$xi.network.alt==0&bio$hu.network.alt==1&bio$jiang.network.alt==0&bio$deng.network==0&
                                             (bio$cc12<3000|bio$cc13<3000|bio$cc14<3000|bio$cc15<3000)&(is.na(bio$cc16)&is.na(bio$cc17)))] <- NA
bio$cmc_chair_connection_current_alt[which(bio$xi.network.alt==0&bio$hu.network.alt==0&bio$jiang.network.alt==1&bio$deng.network==0&
                                             (bio$cc12<3000|bio$cc13<3000)&(is.na(bio$cc14)&is.na(bio$cc15)))] <- NA
bio$cmc_chair_connection_current_alt[which(bio$xi.network.alt==0&bio$hu.network.alt==0&bio$jiang.network.alt==0&bio$deng.network==1&
                                             (bio$cc15<3000|bio$cc16<3000)&(is.na(bio$cc12)&is.na(bio$cc13)))] <- NA

######################################################
##Explanatory Variables: Measures of Professionalism##
######################################################

##
##Combat Experiencein the Korean, Indian, and Vietnam conflicts
##


bio$combat_post_1949 <- 0
bio$combat_post_1949[grep("Korean", bio$combat)] <- 1
bio$combat_post_1949[grep("Indian", bio$combat)] <- 1
bio$combat_post_1949[grep("Vietnam", bio$combat)] <- 1

##
##College experience, including military academies
##

bio$college <- 0
bio$college[which(bio$education>1)] <- 1

##
##Post-graduate experience
##

bio$postgrad <- 0
bio$postgrad[which(bio$education>2)] <- 1



#########################
##Additional covariates##
#########################

##
##Long March participant
##

bio$participated_long_march[is.na(bio$participated_long_march)] <- 0


##
##Princeling
##


bio$parent_CCP_leader[is.na(bio$parent_CCP_leader)] <- 0


##
##Served in political commissar role
##


bio$commissar <- 0
bio$commissar[bio$name %in% positions$name[positions$position_id %in% c(303, 304, 403, 404, 503, 504, 603, 604, 703, 704, 803, 804,
                                                                        1201, 1202, 1301, 1302, 1401, 1402, 1501, 1502, 1601, 1602,
                                                                        1701, 1702, 1801, 1802, 1901, 1902, 2001, 2002, 2101, 2202,
                                                                        2301, 2302, 2401, 2402, 2501, 2502, 2601, 2602)]] <- 1

##
##Ethnic minority
##


bio$minority <- 0
bio$minority[which(bio$ethnicity!="汉族"&bio$ethnicity!="")] <- 1


##
##Rural birth
##


bio$rural[is.na(bio$rural)] <- 0
#bio$rural[which(bio$birth_province=="")] <- NA


##
##Decade of birth year
##


bio$cohort_decade <- as.factor(substr(bio$birth_year, 3, 3))


##
##Decade of birth year
##


bio$post_deng <- 0
bio$post_deng[bio$name %in% positions$name[which(positions$start_date_year>1989|positions$end_date_year>1989)]] <- 1


################################
##Domestic and foreign threats##
################################

##
##Create officer-year panel
##


##
##Create variable for first year someone reaches a military 
##region leader (正大军区职)-level position
##

bio$general_year <- NA
for(i in 1:nrow(bio)){
  if(sum(positions$start_date_year[which(positions$general_rank==1&positions$name==bio$name[i])], na.rm = T)){
    bio$general_year[i] <- min(positions$start_date_year[which(positions$general_rank==1&positions$name==bio$name[i])])
  }
}



##
##Create variable for the first year someone appears in the dataset 
##as a military region deputy leader (副大军区职) or equivalent
##

bio$first_year <- NA
for(i in 1:nrow(bio)){
  if(sum(positions$start_date_year[which(positions$name==bio$name[i])], na.rm=T)>0){
    bio$first_year[i] <- min(positions$start_date_year[which(positions$name==bio$name[i])])}
}


##
##Create variable for the last year someone appears in the dataset 
##


bio$last_year <- NA
for(i in 1:nrow(bio)){
  if(sum(positions$end_date_year[which(positions$name==bio$name[i])], na.rm=T)>0){
    bio$last_year[i] <- max(positions$end_date_year[which(positions$name==bio$name[i])], na.rm=TRUE)}
}



##
##Create panel dataset
##Time-invariant covariates will be repeated for each row
##

panel_year <- NULL
for(i in 1:nrow(bio)){
  if(is.na(bio$last_year[i])==FALSE){
    year <- seq(bio$first_year[i], bio$last_year[i])
    temp <- cbind(bio[rep(i, length(year)),], year)
    panel_year <- rbind(panel_year, temp)
    rm(temp, year)
  }
}


##
##Create a time-varying indicator for each leader's term in office as CMC Chair
##

panel_year$xi.office <- 0
panel_year$xi.office[panel_year$year>2011] <- 1

panel_year$hu.office <- 0
panel_year$hu.office[panel_year$year<2012&panel_year$year>2003] <- 1

panel_year$jiang.office <- 0
panel_year$jiang.office[panel_year$year<2004&panel_year$year>1989] <- 1

panel_year$deng.office <- 0
panel_year$deng.office[panel_year$year<1990] <- 1



##
##Create a time-varying indicator for whether an individual is connected 
##to a leader while that leader is the current CMC Chair
##

panel_year$xi.varying <- panel_year$xi.office*panel_year$xi.network
panel_year$hu.varying <- panel_year$hu.office*panel_year$hu.network
panel_year$jiang.varying <- panel_year$jiang.office*panel_year$jiang.network
panel_year$deng.varying <- panel_year$deng.office*panel_year$deng.network



panel_year$xi.varying.alt <- panel_year$xi.office*panel_year$xi.network.alt
panel_year$hu.varying.alt <- panel_year$hu.office*panel_year$hu.network.alt
panel_year$jiang.varying.alt <- panel_year$jiang.office*panel_year$jiang.network.alt




##
##Create a time-varying indicator for whether an individual is connected 
##to the current CMC Chair
##


panel_year$cmc_connection_varying <- panel_year$xi.varying+panel_year$hu.varying+panel_year$deng.varying+panel_year$jiang.varying

panel_year$cmc_connection_varying_alt <- panel_year$xi.varying.alt+panel_year$hu.varying.alt+panel_year$deng.varying+panel_year$jiang.varying


##
##Create measure for generals by year
##


panel_year$general_varying <- 0
panel_year$general_varying[which(panel_year$general_year<=panel_year$year)] <- 1



##
##Create time-varying measure for age
##

panel_year$age <- panel_year$year-panel_year$birth_year


####
####Create indicators for periods of foreign threat
####


##
##Indicator for post-2000 increased tensions with United States
##

panel_year$foreign <- 0
panel_year$foreign[panel_year$year==2000|panel_year$year==2001|panel_year$year==2002] <- 1

##
##Alternate indicator for post-2000 increased tensions with United States
##plus conflict with Vietnam
##


panel_year$foreign2 <- 0
panel_year$foreign2[panel_year$year==1979|panel_year$year==1980|panel_year$year==1981|panel_year$year==2000|panel_year$year==2001|panel_year$year==2002] <- 1

##
##Alternate indicator for foreign threat
##excluding certain years
##

panel_year$foreign2000 <- 0
panel_year$foreign2000[panel_year$year==2001|panel_year$year==2002] <- 1

panel_year$foreign2001 <- 0
panel_year$foreign2001[panel_year$year==2000|panel_year$year==2002] <- 1

panel_year$foreign2002 <- 0
panel_year$foreign2002[panel_year$year==2000|panel_year$year==2001] <- 1

panel_year$foreign2003 <- 0
panel_year$foreign2003[panel_year$year==2000|panel_year$year==2001|panel_year$year==2002] <- 1




####
####Create indicators for periods of domestic threat
####


##
##Indicator for elite threats in the wake of Tiananmen and the Bo incident
##


panel_year$domestic <- 0
panel_year$domestic[panel_year$year==1990|panel_year$year==1991|panel_year$year==1992|panel_year$year==1993|panel_year$year==2012|panel_year$year==2013|panel_year$year==2014|panel_year$year==2015] <- 1


##
##Alternate indicator for elite threats including Deng's outster of Hua Guofeng and consolidation of power
##


panel_year$domestic2 <- 0
panel_year$domestic2[panel_year$year==1979|panel_year$year==1980|panel_year$year==1981|panel_year$year==1982|panel_year$year==1990|panel_year$year==1991|panel_year$year==1992|panel_year$year==1993|panel_year$year==2012|panel_year$year==2013|panel_year$year==2014|panel_year$year==2015] <- 1

##
##Alternate indicator for domestic threat
##excluding certain years
##


panel_year$domestic1990 <- 0
panel_year$domestic1990[panel_year$year==1991|panel_year$year==1992|panel_year$year==1993|panel_year$year==2012|panel_year$year==2013|panel_year$year==2014|panel_year$year==2015] <- 1


panel_year$domestic1991 <- 0
panel_year$domestic1991[panel_year$year==1990|panel_year$year==1992|panel_year$year==1993|panel_year$year==2012|panel_year$year==2013|panel_year$year==2014|panel_year$year==2015] <- 1


panel_year$domestic1992 <- 0
panel_year$domestic1992[panel_year$year==1990|panel_year$year==1991|panel_year$year==1993|panel_year$year==2012|panel_year$year==2013|panel_year$year==2014|panel_year$year==2015] <- 1

panel_year$domestic1993 <- 0
panel_year$domestic1993[panel_year$year==1990|panel_year$year==1991|panel_year$year==1992|panel_year$year==2012|panel_year$year==2013|panel_year$year==2014|panel_year$year==2015] <- 1

panel_year$domestic2012 <- 0
panel_year$domestic2012[panel_year$year==1990|panel_year$year==1991|panel_year$year==1992|panel_year$year==1993|panel_year$year==2013|panel_year$year==2014|panel_year$year==2015] <- 1

panel_year$domestic2013 <- 0
panel_year$domestic2013[panel_year$year==1990|panel_year$year==1991|panel_year$year==1992|panel_year$year==1993|panel_year$year==2012|panel_year$year==2014|panel_year$year==2015] <- 1

panel_year$domestic2014 <- 0
panel_year$domestic2014[panel_year$year==1990|panel_year$year==1991|panel_year$year==1992|panel_year$year==1993|panel_year$year==2012|panel_year$year==2013|panel_year$year==2015] <- 1

panel_year$domestic2015 <- 0
panel_year$domestic2015[panel_year$year==1990|panel_year$year==1991|panel_year$year==1992|panel_year$year==1993|panel_year$year==2012|panel_year$year==2013|panel_year$year==2014] <- 1


##
##Create ranked education year variable
##


panel_year$education_level <- panel_year$education
panel_year$education_level[is.na(panel_year$education_level)] <- 1



##
##Create pdata.frame object
##

panel_year <- pdata.frame(panel_year, index = c("unique_id", "year"), drop.index = F, row.names = T)




############################################################################






####################
#Description: Running this script will create all the figures and tables in the article.
#The R script files were created with R version 4.0.3 running on Mac OS Big Sur (11.6)
#It was tested on four systems: on Mac OS Big Sur, on GNU Bash 3.2.57(1), on MATE Desktop Environment 1.1.16, and on Windows 10 Enterprise for Virtual Desktops
####################

##Clean the workspace

rm(list=ls())


#OPTION 1: Manually set the working directory here:

setwd('C:/Users/Caesar/Documents/GitHub/StatsII_Spring2023/Replication Project')

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
#rm(list=ls())


##
##Load data
##

bio <- import("C:/Users/Caesar/Documents/GitHub/StatsII_Spring2023/Replication Project/data/bio_data.csv", encoding = "UTF-8")
career <- import("C:/Users/Caesar/Documents/GitHub/StatsII_Spring2023/Replication Project/data/career_data.csv", encoding = "UTF-8")
positions <- import("C:/Users/Caesar/Documents/GitHub/StatsII_Spring2023/Replication Project/data/key_positions_data.csv", encoding = "UTF-8")

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

eval(parse("C:/Users/Caesar/Documents/GitHub/StatsII_Spring2023/Replication Project/code/variable_creation.R", encoding="UTF-8"))

##
##Output main text tables
##

eval(parse("C:/Users/Caesar/Documents/GitHub/StatsII_Spring2023/Replication Project/code/main_tables.R", encoding="UTF-8"))

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
summary(lm1)  # Caesar add it 
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

eval(parse("C:/Users/Caesar/Documents/GitHub/StatsII_Spring2023/Replication Project/code/appendix_tables_figures.R", encoding="UTF-8"))

##Note that running this file will produce five error messages reading:
##"In sqrt(diag(se)) : NaNs produced"
##This is caused by the panel regressions interacting year and birth year fixed effects. 
##For some year-birth year interactions coefficients cannot be estimated because there are not 
##adequate observations in that strata. This issue is discussed briefly in the main text.











### Run glm here: 

# Promotion to General



glmgen1 <- glm(general_deng~deng.network, family = binomial(link = "logit"), data=bio)
summary(glmgen1)
stargazer(glmgen1, title = "glm: General1")

glmgen2 <- glm(general_jiang~jiang.network,  family = binomial(link = "logit"), data=bio)
summary(glmgen2)
stargazer(glmgen2, title = "glm: General2")

glmgen3 <- glm(general_hu~hu.network, family = binomial(link = "logit"), data=bio)
summary(glmgen3)
stargazer(glmgen3, title = "glm: General3")

glmgen4 <- glm(general_xi~xi.network, family = binomial(link = "logit"), data=bio)
summary(glmgen4)
stargazer(glmgen4, title = "glm: Genearl4")


## Promotion to CMC


glmCMC1 <- glm(cmc_deng~deng.network, family = binomial(link = "logit"), data=bio)
summary(glmCMC1) 
stargazer(glmgen1, title = "glm: CMC1")

glmCMC2 <- glm(cmc_jiang~jiang.network, family = binomial(link = "logit"), data=bio)
stargazer(glmgen2, title = "glm: CMC2")

glmCMC3 <- glm(cmc_hu~hu.network, family = binomial(link = "logit"), data=bio)
stargazer(glmgen3, title = "glm: CMC3")

glmCMC4 <- glm(cmc_xi~xi.network, family = binomial(link = "logit"), data=bio)
stargazer(glmgen4, title = "glm: CMC4")













####################################################################










### add interaction below









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













### adding interaction 




lm6_int <- lm(cc_full_alt_member~cmc_chair_connection_current+combat_post_1949+college+participated_long_march+commissar+minority+parent_CCP_leader+rural+cohort_decade + minority : rural, data=bio)
summary(lm6_int)
stargazer(lm6_int, "text")


summary(lm6)
