#############################
# Michael Strausz
# Reading in ANES 2024 data
# 5/1/2025 : 5/18/2025
# Importing and simplifying data from
# https://electionstudies.org/data-center/2024-time-series-study/
#############################

#libraries----------------------------
library(tidyverse)

#simplifying and renaming variables--------------------
#read in the whole dataset
anes2024<-read.csv("anes_timeseries_2024_csv_20250430.csv") 

#select the variables I want-------------------------------------
anes2024<-anes2024 %>% 
  select(V241004,V241106x,V241108,V241118,V241119,V241120,
         V241177,V241211,V241229,V241230,V241234,V241245,
         V241248,V241258,V241287x,V241290x,V241308x,V241319x,V241324,V241325,
         V241327,V241338x,V241366x,V241385x,V241400x,V241403x,V241421,
         V241422,V241440,V241458x,V241459,V241465x,V241470,V241497,
         V241501x,V241521,V241525,V241528,V241537,V241550,V241552,V241553,
         V241567x,V241569,V241583,V241601r,V241612,V241614,V242025,
         V242054x,V242096x,V242095x,V242125,V242126,V242134,V242135,V242136,
         V242137,V242138,V242139,V242140,V242141,V242142,V242143,V242144,
         V242145,V242146,V242147,V242148,V242149,V242150,V242151,V242152,
         V242153,V242154,V242155,V242156,V242266x,V242311,V242321,V242325,
         V242420,V242422,V242423,V241227x)

         #go here V241613

#rename variables-----------------------
anes2024<-anes2024 %>% 
  rename(attention=V241004,
         v2020=V241106x,
         v2016=V241108,
         hope=V241118,
         fear=V241119,
         outrage=V241120,
         ideology=V241177,
         presPredict=V241211,
         trustGov=V241229,
         trustCourts=V241230,
         trustPeople=V241234,
         healthPolicy=V241245,
         abortion=V241248,
         envBusiness=V241258,
         univApproval=V241287x,
         deiApproval=V241290x,
         deathPenalty=V241308x,
         voterID=V241319x,
         pressFreedom=V241324,
         sepPowers=V241325,
         facts=V241327,
         compromisePrinciples=V241338x,
         climateChangeAction=V241366x,
         gayMarriage=V241385x,
         armUkraine=V241400x,
         armIsrael=V241403x,
         bible=V241421,
         relig=V241422,
         religAttn=V241440,
         age=V241458x,
         marital=V241459,
         education=V241465x,
         mil=V241470,
         union=V241497,
         race=V241501x,
         children=V241521,
         smartphone=V241528,
         landline=V241525,
         passport=V241537,
         female=V241550,
         trans=V241552,
         gayLesbian=V241553,
         income=V241567x,
         studentLoans=V241569,
         guns=V241583,
         ncis=V241601r,
         senateTerm=V241612,
         houseKnowledge=V241614,
         polTalk=V242025,
         firstReg=V242054x,
         v2024=V242096x,
         turnout=V242095x,
         ft_harris=V242125,
         ft_trump=V242126,
         ft_waltz=V242134,
         ft_vance=V242135,
         ft_biden=V242136,
         ft_fund=V242137,
         ft_feminists=V242138,
         ft_liberals=V242139,
         ft_unions=V242140,
         ft_business=V242141,
         ft_conservatives=V242142,
         ft_ussc=V242143,
         ft_gaylesbian=V242144,
         ft_congress=V242145,
         ft_muslims=V242146,
         ft_christians=V242147,
         ft_maga=V242148,
         ft_jews=V242149,
         ft_police=V242150,
         ft_trans=V242151,
         ft_blm=V242152,
         ft_nra=V242153,
         ft_fbi=V242154,
         ft_farmers=V242155,
         ft_pparenthood=V242156,
         natPride=V242266x,
         americanDream=V242311,
         climateChangeEffect=V242321,
         gunCon=V242325,
         trustSci=V242420,
         trustMSM=V242422,
         trustSocialMedia=V242423,
         partyID=V241227x)
         
glimpse(anes2024)

anes2024<-anes2024 %>% select(age,female,marital,mil,union,race,children,landline,smartphone,
                    passport,trans,gayLesbian,income,guns,ncis,polTalk,
relig,bible,religAttn,
education,studentLoans,senateTerm,houseKnowledge,presPredict,
v2024,v2020,v2016,firstReg,turnout,
hope,fear,outrage,attention,
trustGov,trustCourts,trustPeople,trustSci,trustMSM,trustSocialMedia,
healthPolicy,abortion,univApproval,deiApproval,voterID,gayMarriage,armUkraine,
armIsrael,gunCon,
envBusiness,climateChangeAction,climateChangeEffect,
ideology,partyID,pressFreedom,sepPowers,facts,compromisePrinciples,natPride,
americanDream,
ft_harris,ft_trump,ft_waltz,ft_vance,ft_biden,ft_fund,ft_feminists,ft_liberals,
ft_unions,ft_business,ft_conservatives,ft_ussc,ft_gaylesbian,ft_congress,
ft_muslims,ft_christians,ft_maga,ft_jews,ft_police,ft_trans,ft_blm,ft_nra,ft_fbi,
ft_farmers,ft_pparenthood)


  
