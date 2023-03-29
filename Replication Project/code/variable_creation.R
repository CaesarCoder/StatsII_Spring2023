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








