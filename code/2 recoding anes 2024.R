#############################
# Michael Strausz
# Recoding ANES 2024 data
# 5/18/2025 : 5/26/2025
# Recoding data from
# https://electionstudies.org/data-center/2024-time-series-study/
#############################

#libraries----------------------------
library(tidyverse)

#recode age--------------------
#V241458x
anes2024<-anes2024 %>%
  mutate(age=na_if(age, -2))

#recode female--------------
#V241550
anes2024$female[anes2024$female<0]<-NA
anes2024$female[anes2024$female==1]<-0
anes2024$female[anes2024$female==2]<-1

#recode marital-------------
#V241459
anes2024$marital[anes2024$marital<0]<-NA
anes2024$marital[anes2024$marital<3]<-"married"
anes2024$marital[anes2024$marital==3]<-"widowed"
anes2024$marital[anes2024$marital==4]<-"divorced"
anes2024$marital[anes2024$marital==5]<-"separated"
anes2024$marital[anes2024$marital==6]<-"never married"

#recode mil--------------
#V241470
anes2024<-anes2024 %>%
  mutate(mil=na_if(mil, -9)) %>% 
  mutate(mil=na_if(mil, -1)) %>% 
  mutate(mil=recode(mil, '1'="active duty",'2'="formerly active duty",
                         '3'="never served")) 

#recode union--------------
#V241497
anes2024$union[anes2024$union<0]<-NA
anes2024$union[anes2024$union==2]<-0

#recode race-----------------
#V241501x
anes2024$race[anes2024$race<0]<-NA
anes2024$race[anes2024$race==1]<-"White"
anes2024$race[anes2024$race==2]<-"Black"
anes2024$race[anes2024$race==3]<-"Hispanic"
anes2024$race[anes2024$race==4]<-"Asian or Pacific Islander"
anes2024$race[anes2024$race==5]<-"Native American"
anes2024$race[anes2024$race==6]<-"Multiple races"

#recode children----------------
#V241521
anes2024$children[anes2024$children<0]<-NA

#recode landline----------------
#V241525
anes2024$landline[anes2024$landline<0]<-NA
anes2024$landline[anes2024$landline==2]<-0

#recode smartphone----------------
#V241528
anes2024$smartphone[anes2024$smartphone<0]<-NA
anes2024$smartphone[anes2024$smartphone==2]<-0

#recode passport-----------
#V241537
anes2024$passport[anes2024$passport<0]<-NA
anes2024$passport[anes2024$passport==2]<-0

#recode trans---------------
#V241552
anes2024$trans[anes2024$trans<0]<-NA
anes2024$trans[anes2024$trans==2]<-0

#recode gayLesbian--------------
#V242144
anes2024$gayLesbian[anes2024$gayLesbian<0]<-NA
anes2024$gayLesbian[anes2024$gayLesbian==1]<-"straight"
anes2024$gayLesbian[anes2024$gayLesbian==2]<-"gay/lesbian"
anes2024$gayLesbian[anes2024$gayLesbian==3]<-"bisexual"
anes2024$gayLesbian[anes2024$gayLesbian==4]<-"other"

#recode income------------------
#V241567x
anes2024$income[anes2024$income<0]<-NA
anes2024<-anes2024 %>% 
  mutate(income=recode_factor(income, '1'="under $9,999",'2'="$10,000 to $29,999",
                              '3'="$30,000 to $59,999",'4'="$60,000 to $99,999",
                              '5'="$100,000 to $249,999",'6'="$250,000 or more"))         
anes2024$income<-as.ordered(anes2024$income)
#recode guns-----------------
#V241583
anes2024$guns[anes2024$guns<0]<-NA

#recode NCIS---------------
#V241601r
anes2024$ncis[anes2024$ncis<0]<-NA

#recode polTalk---------------
#V242025
anes2024$polTalk[anes2024$polTalk<0]<-NA

#recode relig-------------
#V241422
anes2024$relig[anes2024$relig<0]<-NA
anes2024$relig[anes2024$relig==1]<-"Protestant"
anes2024$relig[anes2024$relig==2]<-"Roman Catholic"
anes2024$relig[anes2024$relig==3]<-"Orthodox Christian"
anes2024$relig[anes2024$relig==4]<-"Latter-Day Saints"
anes2024$relig[anes2024$relig==5]<-"Jewish"
anes2024$relig[anes2024$relig==6]<-"Muslim"
anes2024$relig[anes2024$relig==7]<-"Buddhist"
anes2024$relig[anes2024$relig==8]<-"Hindu"
anes2024$relig[anes2024$relig==9]<-"Atheist"
anes2024$relig[anes2024$relig==10]<-"Agnostic"
anes2024$relig[anes2024$relig==11]<-"Other"
anes2024$relig[anes2024$relig==12]<-"Nothing in particular"

#recode bible-------------
#V241421
anes2024$bible[anes2024$bible<0]<-NA
anes2024$bible[anes2024$bible==5]<-NA

anes2024<-anes2024 %>%
  mutate(bible=recode_factor(bible, '1'="literal word of g-d",
                                 '2'="inspired by g-d",'3'="word of people"))
anes2024$bible<-as.ordered(anes2024$bible)

#recode religAttn--------------
#V241440
anes2024$religAttn[anes2024$religAttn<0]<-NA
anes2024<-anes2024 %>%
  mutate(religAttn=recode_factor(religAttn, '1'="every week",
                                    '2'="almost every week",'3'="once or twice a month",
                                    '4'="a few times a year",'5'="never"))
anes2024$religAttn<-as.ordered(anes2024$religAttn)

#recode education--------------
#V241465x
anes2024$education[anes2024$education<0]<-NA
anes2024<-anes2024 %>% 
  mutate(education=recode_factor(education, '1'="less than high school",'2'="high school",
                         '3'="some college",'4'="ba",'5'="graduate degree"))
anes2024$education<-as.ordered(anes2024$education)

#recode studentLoans--------------
#V241569
anes2024$studentLoans[anes2024$studentLoans<0]<-NA

#recode senateTerm------------------
#V241612
anes2024$senateTerm[anes2024$senateTerm<0]<-NA

#recode houseKnowledge--------------
#V241614
anes2024$houseKnowledge[anes2024$houseKnowledge<0]<-NA
anes2024$houseKnowledge[anes2024$houseKnowledge==1]<-0
anes2024$houseKnowledge[anes2024$houseKnowledge==2]<-1

#recode presPredict----------
#V241211
anes2024$presPredict[anes2024$presPredict<0]<-NA
anes2024$presPredict[anes2024$presPredict==1]<-"Kamala Harris"
anes2024$presPredict[anes2024$presPredict==2]<-"Donald Trump"
anes2024$presPredict[anes2024$presPredict==5]<-NA

#recode v2024-------------------
#V242096x
anes2024$v2024[anes2024$v2024<0]<-NA
anes2024$v2024[anes2024$v2024==1]<-"Kamala Harris"
anes2024$v2024[anes2024$v2024==2]<-"Donald Trump"
anes2024$v2024[anes2024$v2024==3]<-"Robert F. Kennedy, Jr"
anes2024$v2024[anes2024$v2024==4]<-"Cornel West"
anes2024$v2024[anes2024$v2024==5]<-"Jill Stein"
anes2024$v2024[anes2024$v2024==6]<-"Other"

#recode v2020-----------
#V241106x
anes2024$v2020[anes2024$v2020<0]<-NA
anes2024$v2020[anes2024$v2020==1]<-"Did not vote"
anes2024$v2020[anes2024$v2020==2]<-"Donald Trump"
anes2024$v2020[anes2024$v2020==3]<-"Joe Biden"
anes2024$v2020[anes2024$v2020==4]<-"Other"

#recode v2016---------------
#V241108
anes2024$v2016[anes2024$v2016<0]<-NA
anes2024$v2016[anes2024$v2016==1]<-"Hillary Clinton"
anes2024$v2016[anes2024$v2016==2]<-"Donald Trump"
anes2024$v2016[anes2024$v2016==5]<-"Other"

#recode firstReg-------------
#V242054x
anes2024$firstReg[anes2024$firstReg<0]<-NA
anes2024<-anes2024 %>%
  mutate(firstReg=recode_factor(firstReg, '1'="Have never registered",
                                 '2'="In the past year",'3'="1-2 years ago",
                                 '4'="3-4 years ago",'5'="5 or more years ago"))
anes2024$firstReg<-as.ordered(anes2024$firstReg)

#recode turnout-----------
#V242095x
anes2024$turnout[anes2024$turnout<0]<-NA

#recode hope----------------
#V241118
anes2024$hope[anes2024$hope<0]<-NA
anes2024<-anes2024 %>%
  mutate(hope=recode_factor(hope, '1'="not at all",
                            '2'="a little",'3'="somewhat",
                            '4'="very",'5'="extremely"))
anes2024$hope<-as.ordered(anes2024$hope)

#recode fear---------------
#V241119
anes2024$fear[anes2024$fear<0]<-NA
anes2024<-anes2024 %>%
  mutate(fear=recode_factor(fear, '1'="not at all",
                            '2'="a little",'3'="somewhat",
                            '4'="very",'5'="extremely"))
anes2024$fear<-as.ordered(anes2024$fear)

#recode outrage---------------
#V241120
anes2024$outrage[anes2024$outrage<0]<-NA
anes2024<-anes2024 %>%
  mutate(outrage=recode_factor(outrage, '1'="not at all",
                            '2'="a little",'3'="somewhat",
                            '4'="very",'5'="extremely"))
anes2024$outrage<-as.ordered(anes2024$outrage)

#recode attention-------
#V241004
anes2024<-anes2024 %>%
  mutate(attention=na_if(attention, -9)) %>% 
  mutate(attention=recode_factor(attention, '1'="always",
                                 '2'="most of the time",'3'="half of the time",
                                 '4'="some of the time",'5'="never"))
anes2024$attention<-as.ordered(anes2024$attention)

#recode trustGov---------
#V241229
anes2024$trustGov[anes2024$trustGov<0]<-NA
anes2024<-anes2024 %>%
  mutate(trustGov=recode_factor(trustGov, '1'="always",
                                '2'="most of the time",'3'="half of the time",
                                '4'="some of the time",'5'="never"))
anes2024$trustGov<-as.ordered(anes2024$trustGov)

#recode trustCourts---------
#V241230
anes2024$trustCourts[anes2024$trustCourts<0]<-NA
anes2024<-anes2024 %>%
  mutate(trustCourts=recode_factor(trustCourts, '1'="always",
                                '2'="most of the time",'3'="half of the time",
                                '4'="some of the time",'5'="never"))
anes2024$trustCourts<-as.ordered(anes2024$trustCourts)

#recode trustPeople-------------------
#V241234
anes2024$trustPeople[anes2024$trustPeople<0]<-NA
anes2024<-anes2024 %>%
  mutate(trustPeople=recode_factor(trustPeople, '1'="always",
                                   '2'="most of the time",'3'="half of the time",
                                   '4'="some of the time",'5'="never"))
anes2024$trustPeople<-as.ordered(anes2024$trustPeople)

#recode trustSci-----------------
#V242420
anes2024$trustSci[anes2024$trustSci<0]<-NA
anes2024<-anes2024 %>%
  mutate(trustSci=recode_factor(trustSci, '1'="a lot",
                                   '2'="somewhat",'3'="not very much",
                                   '4'="not at all"))
anes2024$trustSci<-as.ordered(anes2024$trustSci)

#recode trustMSM------------------------
#V242422
anes2024$trustMSM[anes2024$trustMSM<0]<-NA
anes2024<-anes2024 %>%
  mutate(trustMSM=recode_factor(trustMSM, '1'="a lot",
                                '2'="somewhat",'3'="not very much",
                                '4'="not at all"))
anes2024$trustMSM<-as.ordered(anes2024$trustMSM)

#recode trustSocialMedia------------
#V242423
anes2024$trustSocialMedia[anes2024$trustSocialMedia<0]<-NA
anes2024<-anes2024 %>%
  mutate(trustSocialMedia=recode_factor(trustSocialMedia, '1'="a lot",
                                '2'="somewhat",'3'="not very much",
                                '4'="not at all"))
anes2024$trustSocialMedia<-as.ordered(anes2024$trustSocialMedia)

#recode healthPolicy-----------------
#V241245
anes2024$healthPolicy[anes2024$healthPolicy<0]<-NA
anes2024$healthPolicy[anes2024$healthPolicy==99]<-NA
anes2024<-anes2024 %>%
  mutate(healthPolicy=recode_factor(healthPolicy, '1'="Government insurance",
                                '2'="2",'3'="3",'4'="4",'5'="5",'6'="6",'7'=
                                  "Private insurance"))
anes2024$healthPolicy<-as.ordered(anes2024$healthPolicy)

#recode abortion------------
#V241248
anes2024$abortion[anes2024$abortion<0]<-NA
anes2024$abortion[anes2024$abortion==99]<-NA
anes2024<-anes2024 %>%
  mutate(abortion=recode_factor(abortion, '1'="always permitted",
                                    '2'="2",'3'="3",'4'="4",'5'="5",'6'="6",'7'=
                                      "never permitted"))
anes2024$abortion<-as.ordered(anes2024$abortion)

#recode univApproval------------
#V241287x
anes2024$univApproval[anes2024$univApproval<0]<-NA
anes2024<-anes2024 %>% 
  mutate(univApproval=recode_factor(univApproval,'1'="approve strongly",
                                    '2'="approve",
                                    '3'="approve weakly",
                                    '4'="neither",
                                    '5'="disapprove weakly",
                                    '6'="disapprove",
                                    '7'="disapprove strongly"))
anes2024$univApproval<-as.ordered(anes2024$univApproval)

#recode deiApproval------------
#V241290x
anes2024$deiApproval[anes2024$deiApproval<0]<-NA
anes2024<-anes2024 %>% 
  mutate(deiApproval=recode_factor(deiApproval,'1'="favor strongly",
                                   '2'="favor",
                                   '3'="favor weakly",
                                   '4'="neither",
                                   '5'="oppose weakly",
                                   '6'="oppose",
                                   '7'="oppose strongly"))
anes2024$deiApproval<-as.ordered(anes2024$deiApproval)

#recode voterID------------
#V241319x
anes2024$voterID[anes2024$voterID<0]<-NA
anes2024<-anes2024 %>% 
  mutate(voterID=recode_factor(voterID,'1'="favor strongly",
                               '2'="favor",
                               '3'="favor weakly",
                               '4'="neither",
                               '5'="oppose weakly",
                               '6'="oppose",
                               '7'="oppose strongly"))
anes2024$voterID<-as.ordered(anes2024$voterID)

#recode gayMarriage----------
#V241385x
anes2024$gayMarriage[anes2024$gayMarriage<0]<-NA
anes2024<-anes2024 %>% 
  mutate(gayMarriage=recode_factor(gayMarriage,'1'="favor strongly",
                               '2'="favor",
                               '3'="favor weakly",
                               '4'="neither",
                               '5'="oppose weakly",
                               '6'="oppose",
                               '7'="oppose strongly"))
anes2024$gayMarriage<-as.ordered(anes2024$gayMarriage)

#recode armUkraine---------
#V241400x
anes2024$armUkraine[anes2024$armUkraine<0]<-NA
anes2024<-anes2024 %>% 
  mutate(armUkraine=recode_factor(armUkraine,'1'="favor strongly",
                                   '2'="favor",
                                   '3'="favor weakly",
                                   '4'="neither",
                                   '5'="oppose weakly",
                                   '6'="oppose",
                                   '7'="oppose strongly"))
anes2024$armUkraine<-as.ordered(anes2024$armUkraine)

#recode armIsrael---------
#V241403x
anes2024$armIsrael[anes2024$armIsrael<0]<-NA
anes2024<-anes2024 %>% 
  mutate(armIsrael=recode_factor(armIsrael,'1'="favor strongly",
                                  '2'="favor",
                                  '3'="favor weakly",
                                  '4'="neither",
                                  '5'="oppose weakly",
                                  '6'="oppose",
                                  '7'="oppose strongly"))
anes2024$armIsrael<-as.ordered(anes2024$armIsrael)

#recode gunCon-------------
#V242325
anes2024$gunCon[anes2024$gunCon<0]<-NA
anes2024$gunCon[anes2024$gunCon==1]<-"More difficult"
anes2024$gunCon[anes2024$gunCon==2]<-"Easier"
anes2024$gunCon[anes2024$gunCon==3]<-"About the same"

#recode envBusiness-----------------
#V241258
anes2024$envBusiness[anes2024$envBusiness<0]<-NA
anes2024$envBusiness[anes2024$envBusiness==99]<-NA
anes2024<-anes2024 %>%
  mutate(envBusiness=recode_factor(envBusiness, '1'="Need more env. regulations on business",
                                    '2'="2",'3'="3",'4'="4",'5'="5",'6'="6",'7'=
                                      "Env. regulations too burdensome already"))
anes2024$envBusiness<-as.ordered(anes2024$envBusiness)

#recode climateChangeAction--------------
#V241366x
anes2024$climateChangeAction[anes2024$climateChangeAction<0]<-NA
anes2024<-anes2024 %>% 
  mutate(climateChangeAction=recode_factor(climateChangeAction,'1'="much more",
                                           '2'="more",'3'="a bit more",
                                           '4'="neither more or less",
                                           '5'="a bit less",'6'="less",'7'="much less"))
anes2024$climateChangeAction<-as.ordered(anes2024$climateChangeAction)

#recode climateChangeEffect--------------
#V242321
anes2024$climateChangeEffect[anes2024$climateChangeEffect<0]<-NA
anes2024<-anes2024 %>% 
  mutate(climateChangeEffect=recode_factor(climateChangeEffect,'1'="not at all",
                                           '2'="a little",'3'="a moderate amount",
                                           '4'="a lot",'5'="a great deal"))

#recode ideology----------------------
#V241177
anes2024$ideology[anes2024$ideology<0]<-NA
anes2024$ideology[anes2024$ideology==99]<-NA
anes2024<-anes2024 %>% 
  mutate(ideology=recode_factor(ideology,'1'="very liberal",'2'="liberal",
                                '3'="slightly liberal",'4'="moderate",
                                '5'="slightly conservative",'6'="conservative",
                                '7'="very conservative"))
anes2024$ideology<-as.ordered(anes2024$ideology)

#recode partyID-----------------------------
#V241227x
anes2024$partyID[anes2024$partyID<0]<-NA
anes2024<-anes2024 %>% 
  mutate(partyID=recode_factor(partyID,'1'="strong democrat",'2'="democrat",
                                '3'="weak democrat",'4'="independent",
                                '5'="weak republican",'6'="republican",
                                '7'="strong republican"))
anes2024$partyID<-as.ordered(anes2024$partyID)

#recode pressFreedom------------
#V241324
anes2024$pressFreedom[anes2024$pressFreedom<0]<-NA
anes2024<-anes2024 %>% 
  mutate(pressFreedom=recode_factor(pressFreedom,'1'="not important",
                                    '2'="a little important",
                                    '3'="moderately important",
                                    '4'="very important",
                                    '5'="extremely important"))
anes2024$pressFreedom<-as.ordered(anes2024$pressFreedom) 

#recode sepPowers-----------------
#V241325
anes2024$sepPowers[anes2024$sepPowers<0]<-NA
anes2024<-anes2024 %>% 
  mutate(sepPowers=recode_factor(sepPowers,'1'="not important",
                                    '2'="a little important",
                                    '3'="moderately important",
                                    '4'="very important",
                                    '5'="extremely important"))
anes2024$sepPowers<-as.ordered(anes2024$sepPowers) 

#recode facts------------
#V241327
anes2024$facts[anes2024$facts<0]<-NA
anes2024<-anes2024 %>% 
  mutate(facts=recode_factor(facts,'1'="not important",
                                 '2'="a little important",
                                 '3'="moderately important",
                                 '4'="very important",
                                 '5'="extremely important"))
anes2024$facts<-as.ordered(anes2024$facts) 

#recode compromisePrinciples-----------------
#V241338x
anes2024$compromisePrinciples[anes2024$compromisePrinciples<0]<-NA
anes2024<-anes2024 %>% 
  mutate(compromisePrinciples=recode_factor(compromisePrinciples,'1'="prefers compromise strongly",
                             '2'="prefers compromise",
                             '3'="prefers compromise slightly",
                             '4'="prefers principles slightly",
                             '5'="prefers principles",
                             '6'="prefers principles strongly"))
anes2024$compromisePrinciples<-as.ordered(anes2024$compromisePrinciples)

#recode natPride----------------
#V242266x
anes2024$natPride[anes2024$natPride<0]<-NA
anes2024<-anes2024 %>% 
  mutate(natPride=recode_factor(natPride,'1'="a lot better",
                                            '2'="somewhat better",
                                            '3'="the same",
                                            '4'="somewhat worse",
                                            '5'="a lot worse"))
anes2024$natPride<-as.ordered(anes2024$natPride)

#recode americanDream----------------
#V242311
anes2024$americanDream[anes2024$americanDream<0]<-NA
anes2024<-anes2024 %>% 
  mutate(americanDream=recode_factor(americanDream,'1'="a great deal",
                                     '2'="a lot",'3'="a moderate amount",
                                     '4'="a little",'5'="none"))


#recode feeling thermometers---------------
anes2024<-anes2024 %>%
  mutate_at(vars(starts_with("ft_")), ~replace(., (. <0)|(. >100), NA))

#export data-----------
save(anes2024, file="anes2024.rda")
