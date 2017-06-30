library(e1071)
library(grid) 
library(lattice)
library(latticeExtra)
library(HH)
library(mvtnorm)
library(BDgraph)
library(devtools)
library(likert)

##########################################################################################################################################################
##########################################################################################################################################################     

## Year 1: Daily Survey -- Summaries for each person 

#xtabs(~daily_final$AvgThirst+daily_final$bdg+daily_final$phase)

#Temperature Impact - Daily Activities

xtabs(~daily_y1$TempImpact_DailyActivities+daily_y1$bdg+daily_y1$phase)

plot.likert(daily_y1$TempImpact_DailyActivities,)
likert(daily_y1$TempImpact_DailyActivities)

#      , , daily_y1$phase = 1
#      
#                                            daily_y1$bdg
#      daily_y1$TempImpact_DailyActivities        1              2
#                                            1    35/61= 57.4%   31/55= 56.4%
#                                            2    20/61= 32.8%   4/55=  7.3%
#                                            3    4/61=  6.6%    20/55= 36.4%
#                                            4    2/61=  3.3%    0/55=  0%
#                                            5    0/61=  0%      0/55=  0%
#                                       TOTAL     61             55             = 116
#      , , daily_y1$phase = 2
#      
#                                            daily_y1$bdg
#      daily_y1$TempImpact_DailyActivities        1                   2
#                                            1    67/97= 69.1%        76/118= 64.4%
#                                            2    17/97= 17.5%        15/118= 12.7%
#                                            3    11/97= 11.3%        21/118= 17.8%
#                                            4    1/97=  1.3%         6/118=  5.1%
#                                            5    1/97=  1.3%         0/118=  0%
#                                       TOTAL     97                  118       = 215
#      , , daily_y1$phase = 3
#      
#                                            daily_y1$bdg
#      daily_y1$TempImpact_DailyActivities        1                    2
#                                            1    74/106=  69.8%       63/120= 52.5%
#                                            2    19/106=  17.9%       17/120= 14.2%
#                                            3    12/106=  11.3%       32/120= 26.7%
#                                            4    1/106=   0.94%       7/120=  5.8%
#                                            5    0/106=   0%          1/120=  0.83%
#                                       TOTAL     106                  120            =226

xtabs(~daily_y1$TempImpact_DailyActivities+daily_y1$phase+daily_y1$pid.x)

tempimpact.table<-table(daily_y1$phase[daily_y1$bdg==1],daily_y1$TempImpact_DailyActivities[daily_y1$bdg==1])
ftable(tempimpact.table)
prop.table(tempimpact.table,1)     

#Building 1
#         1           2           3           4           5
#      1 0.573770492 0.327868852 0.065573770 0.032786885 0.000000000
#      2 0.690721649 0.175257732 0.113402062 0.010309278 0.010309278
#      3 0.698113208 0.179245283 0.113207547 0.009433962 0.000000000

tempimpact.table<-table(daily_y1$phase[daily_y1$bdg==2],daily_y1$TempImpact_DailyActivities[daily_y1$bdg==2])
ftable(tempimpact.table)
prop.table(tempimpact.table,1) 

#Building 2
#        1           2           3           4           5
#      1 0.563636364 0.072727273 0.363636364 0.000000000 0.000000000
#      2 0.644067797 0.127118644 0.177966102 0.050847458 0.000000000
#      3 0.525000000 0.141666667 0.266666667 0.058333333 0.008333333

tempimpact.activities.y1<-ddply(daily_y1,.(pid.x,phase),summarise,mean.impact.activities=mean(TempImpact_DailyActivities,na.rm=TRUE),max.impact.activities=max(TempImpact_DailyActivities,na.rm=TRUE),sd.impact.activities=sd(TempImpact_DailyActivities,na.rm=TRUE))
tempimpact.activities.y1$bdg<-with(tempimpact.activities.y1,ifelse(pid.x>=39,1,2))
tempimpact.activities.y1$bdg<-with(tempimpact.activities.y1,ifelse(pid.x>=70,2,bdg))

#collpase into 3 categories instead of 5
daily_y1$TempImpact_DailyActivities_collapsed<-daily_y1$TempImpact_DailyActivities
daily_y1$TempImpact_DailyActivities_collapsed<-ifelse(daily_y1$TempImpact_DailyActivities_collapsed==2,1,daily_y1$TempImpact_DailyActivities_collapsed)
daily_y1$TempImpact_DailyActivities_collapsed<-ifelse(daily_y1$TempImpact_DailyActivities_collapsed==4,5,daily_y1$TempImpact_DailyActivities_collapsed)

tempimpact.table<-table(daily_y1$phase[daily_y1$bdg==1],daily_y1$TempImpact_DailyActivities_collapsed[daily_y1$bdg==1])
ftable(tempimpact.table)
prop.table(tempimpact.table,1)   


tempimpact.table<-table(daily_y1$phase[daily_y1$bdg==2],daily_y1$TempImpact_DailyActivities_collapsed[daily_y1$bdg==2])
ftable(tempimpact.table)
prop.table(tempimpact.table,1) 


ggplot(tempimpact.activities.y1)+geom_line(aes(x=phase,y=mean.impact.activities,group=pid.x,color=factor(bdg)))

ggplot(tempimpact.activities.y1)+geom_line(aes(x=phase,y=mean.impact.activities,group=pid.x,color=factor(bdg)))

#Temperature Impact - Relaxing 

xtabs(~daily_y1$TempImpact_Relaxing+daily_y1$bdg+daily_y1$phase)

#      , , daily_y1$phase = 1
#      
#                                       daily_y1$bdg
#      daily_y1$TempImpact_Relaxing     1                   2
#                                  1    40/60= 66.7%        29/53= 54.7%
#                                  2    11/60= 18.3%        7/53=  13.2%
#                                  3    6/60=  10.0%        17/53= 32.1%
#                                  4    3/60=  5.0%         0/53=  0%
#                                  5    0/60=  0%           0/53=  0%
#                             TOTAL     60                  53                  =113
#      , , daily_y1$phase = 2
#      
#                                       daily_y1$bdg
#      daily_y1$TempImpact_Relaxing     1                   2
#                                  1    63/93= 67.7%        72/120= 60.0%
#                                  2    19/93= 20.4%        16/120= 13.3%
#                                  3    7/93=  7.5%         25/120= 20.8%
#                                  4    3/93=  3.2%         5/120= 4.2%
#                                  5    1/93=  1.1%         2/120= 1.7%
#                             TOTAL     93                  120                 =213
#      , , daily_y1$phase = 3
#      
#                                       daily_y1$bdg
#      daily_y1$TempImpact_Relaxing     1                    2
#                                  1    76/106= 71.7%        57/116= 49.1%
#                                  2    19/106= 17.9%        27/116= 23.3%
#                                  3    10/106= 9.4%         27/116= 23.3%
#                                  4     1/106= 0.94%        4/116=  3.4%
#                                  5     0/106= 0%           1/116=  0.86%
#                             TOTAL     106                  116                = 222

xtabs(~daily_y1$TempImpact_Relaxing+daily_y1$phase+daily_y1$pid.x)

tempimpact.relax.table<-table(daily_y1$phase[daily_y1$bdg==1],daily_y1$TempImpact_Relaxing[daily_y1$bdg==1])
ftable(tempimpact.relax.table)
prop.table(tempimpact.relax.table,1)     

#Building 1
#        1           2           3           4           5
#      1 0.666666667 0.183333333 0.100000000 0.050000000 0.000000000
#      2 0.677419355 0.204301075 0.075268817 0.032258065 0.010752688
#      3 0.716981132 0.179245283 0.094339623 0.009433962 0.000000000

tempimpact.relax.table<-table(daily_y1$phase[daily_y1$bdg==2],daily_y1$TempImpact_Relaxing[daily_y1$bdg==2])
ftable(tempimpact.relax.table)
prop.table(tempimpact.relax.table,1)     

#Building 2
#        1          2          3          4          5
#      1 0.54716981 0.13207547 0.32075472 0.00000000 0.00000000
#      2 0.60000000 0.13333333 0.20833333 0.04166667 0.01666667
#      3 0.49137931 0.23275862 0.23275862 0.03448276 0.00862069

tempimpact.relax.y1<-ddply(daily_y1,.(pid.x,phase),summarise,mean.impact.relax=mean(TempImpact_Relaxing,na.rm=TRUE),max.impact.relax=max(TempImpact_Relaxing,na.rm=TRUE),sd.impact.relax=sd(TempImpact_Relaxing,na.rm=TRUE))
tempimpact.relax.y1$bdg<-with(tempimpact.relax.y1,ifelse(pid.x>=39,1,2))
tempimpact.relax.y1$bdg<-with(tempimpact.relax.y1,ifelse(pid.x>=70,2,bdg))

ggplot(tempimpact.relax.y1)+geom_line(aes(x=phase,y=mean.impact.relax,group=pid.x,color=factor(bdg)))

#Temperature Impact - Sleeping 

xtabs(~daily_y1$TempImpact_Sleeping+daily_y1$bdg+daily_y1$phase)

#      , , daily_y1$phase = 1
#      
#                                       daily_y1$bdg
#      daily_y1$TempImpact_Sleeping     1                   2
#                                  1    38/60= 63.3%        29/57= 50.9%
#                                  2    15/60= 25%          7/57=  12.3%
#                                  3    4/60=  6.7%         18/57= 31.6%
#                                  4    3/60=  5.0%         3/57=  5.3%
#                                  5    0/60=  0%           0/57=  0%
#                             TOTAL     60                  57                  =117
#      , , daily_y1$phase = 2
#      
#                                       daily_y1$bdg
#      daily_y1$TempImpact_Sleeping     1                   2
#                                  1    64/93= 68.8%        67/118= 56.%
#                                  2    19/93= 20.4%        17/118= 14.4%
#                                  3    5/93=  5.4%         22/118= 18.6%
#                                  4    3/93=  3.2%         7/118=  5.9%
#                                  5    2/93=  2.2%         5/118=  4.2%
#                             TOTAL     93                  118                 = 211
#      , , daily_y1$phase = 3
#      
#                                       daily_y1$bdg
#      daily_y1$TempImpact_Sleeping     1                   2
#                                  1    71/104= 68.3%       59/116= 50.9%      
#                                  2    21/104= 20.2%       17/116= 14.7%
#                                  3    11/104= 10.6%       33/116= 28.4%
#                                  4    1/104=  0.96%       3/116=  2.6%
#                                  5    0/104=  0%          4/116=  3.4%
#                             TOTAL     104                 116                 = 220

xtabs(~daily_y1$TempImpact_Sleeping+daily_y1$phase+daily_y1$pid.x)


####### TRY COLLAPSED 1, 2, 3+



tempimpact.sleep.table<-table(daily_y1$phase[daily_y1$bdg==1],daily_y1$TempImpact_Sleeping[daily_y1$bdg==1])
ftable(tempimpact.sleep.table)
prop.table(tempimpact.sleep.table,1)     

#Building 1
#        1           2           3           4           5
#      1 0.633333333 0.250000000 0.066666667 0.050000000 0.000000000
#      2 0.688172043 0.204301075 0.053763441 0.032258065 0.021505376
#      3 0.682692308 0.201923077 0.105769231 0.009615385 0.000000000

tempimpact.sleep.table<-table(daily_y1$phase[daily_y1$bdg==2],daily_y1$TempImpact_Sleeping[daily_y1$bdg==2])
ftable(tempimpact.sleep.table)
prop.table(tempimpact.sleep.table,1)

#Building 2
#        1          2          3          4          5
#      1 0.50877193 0.12280702 0.31578947 0.05263158 0.00000000
#      2 0.56779661 0.14406780 0.18644068 0.05932203 0.04237288
#      3 0.50862069 0.14655172 0.28448276 0.02586207 0.03448276

tempimpact.sleep.y1<-ddply(daily_y1,.(pid.x,phase),summarise,mean.impact.sleep=mean(TempImpact_Sleeping,na.rm=TRUE),max.impact.sleep=max(TempImpact_Sleeping,na.rm=TRUE),sd.impact.sleep=sd(TempImpact_Sleeping,na.rm=TRUE))
tempimpact.sleep.y1$bdg<-with(tempimpact.sleep.y1,ifelse(pid.x>=39,1,2))
tempimpact.sleep.y1$bdg<-with(tempimpact.sleep.y1,ifelse(pid.x>=70,2,bdg))

#collpase into 3 categories instead of 5
daily_y1$TempImpact_Sleeping_collapsed<-daily_y1$TempImpact_Sleeping
daily_y1$TempImpact_Sleeping_collapsed<-ifelse(daily_y1$TempImpact_Sleeping_collapsed==2,1,daily_y1$TempImpact_Sleeping_collapsed)
daily_y1$TempImpact_Sleeping_collapsed<-ifelse(daily_y1$TempImpact_Sleeping_collapsed==4,5,daily_y1$TempImpact_Sleeping_collapsed)

tempimpact.sleep.table<-table(daily_y1$phase[daily_y1$bdg==1],daily_y1$TempImpact_Sleeping_collapsed[daily_y1$bdg==1])
ftable(tempimpact.sleep.table)
prop.table(tempimpact.sleep.table,1)   


tempimpact.sleep.table<-table(daily_y1$phase[daily_y1$bdg==2],daily_y1$TempImpact_Sleeping_collapsed[daily_y1$bdg==2])
ftable(tempimpact.sleep.table)
prop.table(tempimpact.sleep.table,1) 


ggplot(tempimpact.sleep.y1)+geom_line(aes(x=phase,y=mean.impact.sleep,group=pid.x,color=factor(bdg)))



###########################################################################################################################################################
############################################################################################################################################################

#####          daily perception 

#Temperature too hot

xtabs(~daily_y1$Apt_TempTooHot+daily_y1$bdg+daily_y1$phase)
xtabs(~daily_y1$Apt_TempTooHot+daily_y1$phase+daily_y1$pid.x)

perc.temptoohot.table<-table(daily_y1$phase[daily_y1$bdg==1],daily_y1$Apt_TempTooHot[daily_y1$bdg==1])
ftable(perc.temptoohot.table)
prop.table(perc.temptoohot.table,1)     
#Building 1
#     1
# 1   7/65 = 10.8%
# 2  23/103 = 22.3%
# 3  39/113 = 34.5%


perc.temptoohot.table<-table(daily_y1$phase[daily_y1$bdg==2],daily_y1$Apt_TempTooHot[daily_y1$bdg==2])
ftable(perc.temptoohot.table)
prop.table(perc.temptoohot.table,1)     
#Building 2
#    1
# 
# 1  17/59 = 28.8%
# 2  54/123 = 43.9%
# 3  57/127 = 44.9%

perc.temptoohot.y1<-ddply(daily_y1,.(pid.x,phase),summarise,max.perc.toohot=max(Apt_TempTooHot,na.rm=TRUE))
perc.temptoohot.y1$bdg<-with(perc.temptoohot.y1,ifelse(pid.x>=39,1,2))
perc.temptoohot.y1$bdg<-with(perc.temptoohot.y1,ifelse(pid.x>=70,2,bdg))

#for chi-square, used online calculator because of the way these are coded :-/

     #by preexisting condition 
     xtabs(~daily_y1$Apt_TempTooHot[daily_y1$bdg==1]+daily_y1$phase[daily_y1$bdg==1])


############################################################################################################################################################

#Temperature too cold

xtabs(~daily_y1$Apt_TempTooCold+daily_y1$bdg+daily_y1$phase)
xtabs(~daily_y1$Apt_TempTooCold+daily_y1$phase+daily_y1$pid.x)

perc.temptoocold.table<-table(daily_y1$phase[daily_y1$bdg==1],daily_y1$Apt_TempTooCold[daily_y1$bdg==1])
ftable(perc.temptoocold.table)
prop.table(perc.temptoocold.table,1)     
#Building 1
# 1
# 
# 1  19/65 = 29.2%       46
# 2  19/103 = 18.4%      84
# 3  30/113 = 26.5%      83

perc.temptoocold.table<-table(daily_y1$phase[daily_y1$bdg==2],daily_y1$Apt_TempTooCold[daily_y1$bdg==2])
ftable(perc.temptoocold.table)
prop.table(perc.temptoocold.table,1)     
#Building 2
# 1
# 
# 1  7/59 = 11.9%        52
# 2  6/123 = 4.9%        117
# 3  6/127 = 4.7%        121

perc.temptoocold.y1<-ddply(daily_y1,.(pid.x,phase),summarise,max.perc.toocold=max(Apt_TempTooCold,na.rm=TRUE))
perc.temptoocold.y1$bdg<-with(perc.temptoocold.y1,ifelse(pid.x>=39,1,2))
perc.temptoocold.y1$bdg<-with(perc.temptoocold.y1,ifelse(pid.x>=70,2,bdg))

#for chi-square, used online calculator because of the way these are coded :-/

############################################################################################################################################################

#Too much air 

xtabs(~daily_y1$Apt_TooMuchAir+daily_y1$bdg+daily_y1$phase)
xtabs(~daily_y1$Apt_TooMuchAir+daily_y1$phase+daily_y1$pid.x)

perc.toomuchair.table<-table(daily_y1$phase[daily_y1$bdg==1],daily_y1$Apt_TooMuchAir[daily_y1$bdg==1])
ftable(perc.toomuchair.table)
prop.table(perc.toomuchair.table,1)     
#Building 1
# 1
# 
# 1  15/65 = 23.1%       50
# 2  11/103 = 10.7%      92
# 3  13/113 = 11.5%      100

perc.toomuchair.table<-table(daily_y1$phase[daily_y1$bdg==2],daily_y1$Apt_TooMuchAir[daily_y1$bdg==2])
ftable(perc.toomuchair.table)
prop.table(perc.toomuchair.table,1)     
#Building 2
# 1
# 
# 1  1/59 = 1.7%         58
# 2  3/123 = 2.4%        120
# 3  7/127 = 5.5%        120

perc.toomuchair.y1<-ddply(daily_y1,.(pid.x,phase),summarise,max.perc.toomuchair=max(Apt_TooMuchAir,na.rm=TRUE))
perc.toomuchair.y1$bdg<-with(perc.toomuchair.y1,ifelse(pid.x>=39,1,2))
perc.toomuchair.y1$bdg<-with(perc.toomuchair.y1,ifelse(pid.x>=70,2,bdg))

############################################################################################################################################################

#Too little air

xtabs(~daily_y1$Apt_TooLittleAir+daily_y1$bdg+daily_y1$phase)
xtabs(~daily_y1$Apt_TooLittleAir+daily_y1$phase+daily_y1$pid.x)

perc.toolittleair.table<-table(daily_y1$phase[daily_y1$bdg==1],daily_y1$Apt_TooLittleAir[daily_y1$bdg==1])
ftable(perc.toolittleair.table)
prop.table(perc.toolittleair.table,1)     
#Building 1
# 1
# 
# 1   9/65 = 13.8%       56
# 2  14/103 = 13.6%      89
# 3  15/113 = 13.3%      98

perc.toolittleair.table<-table(daily_y1$phase[daily_y1$bdg==2],daily_y1$Apt_TooLittleAir[daily_y1$bdg==2])
ftable(perc.toolittleair.table)
prop.table(perc.toolittleair.table,1)     
#Building 2
# 1
# 
# 1  25/59 = 42.4%       34
# 2  28/123 = 22.8%      95
# 3  37/127 = 29.1%      90

perc.toolittleair.y1<-ddply(daily_y1,.(pid.x,phase),summarise,max.perc.toolittleair=max(Apt_TooLittleAir,na.rm=TRUE))
perc.toolittleair.y1$bdg<-with(perc.toolittleair.y1,ifelse(pid.x>=39,1,2))
perc.toolittleair.y1$bdg<-with(perc.toolittleair.y1,ifelse(pid.x>=70,2,bdg))

############################################################################################################################################################

#Too humid

xtabs(~daily_y1$Apt_TooHumid+daily_y1$bdg+daily_y1$phase)
xtabs(~daily_y1$Apt_TooHumid+daily_y1$phase+daily_y1$pid.x)

perc.toohumid.table<-table(daily_y1$phase[daily_y1$bdg==1],daily_y1$Apt_TooHumid[daily_y1$bdg==1])
ftable(perc.toohumid.table)
prop.table(perc.toohumid.table,1)     
#Building 1
#    1
# 
# 1  10/65 = 15.4%       55
# 2  11/103 = 10.7%      92
# 3  25/113 = 22.1%      88

perc.toohumid.table<-table(daily_y1$phase[daily_y1$bdg==2],daily_y1$Apt_TooHumid[daily_y1$bdg==2])
ftable(perc.toohumid.table)
prop.table(perc.toohumid.table,1)     
#Building 2
#    1
# 
# 1  23/59 = 39.0%       36
# 2  40/123 = 32.5%      83
# 3  42/127 = 33.1%      85

perc.toohumid.y1<-ddply(daily_y1,.(pid.x,phase),summarise,max.perc.toohumid=max(Apt_TooHumid,na.rm=TRUE))
perc.toohumid.y1$bdg<-with(perc.toohumid.y1,ifelse(pid.x>=39,1,2))
perc.toohumid.y1$bdg<-with(perc.toohumid.y1,ifelse(pid.x>=70,2,bdg))

############################################################################################################################################################

#Too dry

xtabs(~daily_y1$Apt_TooDry+daily_y1$bdg+daily_y1$phase)
xtabs(~daily_y1$Apt_TooDry+daily_y1$phase+daily_y1$pid.x)

perc.toodry.table<-table(daily_y1$phase[daily_y1$bdg==1],daily_y1$Apt_TooDry[daily_y1$bdg==1])
ftable(perc.toodry.table)
prop.table(perc.toodry.table,1)     
#Building 1
# 1
# 
# 1   7/65 = 10.8%       58
# 2  11/103 = 10.7%      92
# 3   5/113 = 4.4%       108

perc.toodry.table<-table(daily_y1$phase[daily_y1$bdg==2],daily_y1$Apt_TooDry[daily_y1$bdg==2])
ftable(perc.toodry.table)
prop.table(perc.toodry.table,1)     
#Building 2
# 1
# 
# 1  17/59 = 28.8%       42
# 2  18/123 = 14.6%      105
# 3  17/127 = 13.4%      110

perc.toodry.y1<-ddply(daily_y1,.(pid.x,phase),summarise,max.perc.toodry=max(Apt_TooDry,na.rm=TRUE))
perc.toodry.y1$bdg<-with(perc.toodry.y1,ifelse(pid.x>=39,1,2))
perc.toodry.y1$bdg<-with(perc.toodry.y1,ifelse(pid.x>=70,2,bdg))


############################################################################################################################################################
############################################################################################################################################################

#    Daily Satisfaction - do not have any satisfaction variables 


































