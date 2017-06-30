
#netatmo data: temperature distribution
head(ntx_0721)
ntx_0721$studyday<-format(ntx_0721$datetime,"%m/%d")

summary.by.day<-ddply(ntx_0721,.(pid,bdg,studyday),summarise,max.temp=max(temp.f,na.rm=TRUE),
                      mean.temp=mean(temp.f,na.rm=TRUE),min.temp=min(temp.f,na.rm=TRUE))

library(reshape)

summary.by.day<-melt(summary.by.day,id.vars=c("pid","bdg","studyday"))

ggplot(summary.by.day)+geom_density(aes(value,color=variable))+facet_grid(.~bdg)

ddply(ntx_0721,.(bdg,cphase.def),summarise,sd(temp.f,na.rm=TRUE))


#Categorical symptom graphs

graph.total.symp<-na.omit(ddply(subset(daily721, !pid%in%pid.out&!studyday%in%days.out),.
                        (bdg,phase),summarise,sd.symp=sd(total.symp,na.rm=TRUE),mean.symp=mean(total.symp,na.rm=TRUE),n=length(bdg)))

graph.total.symp$sem<-with(graph.total.symp,sd.symp/sqrt(n))

symp.limits<-with(graph.total.symp, aes(ymax=mean.symp+1.96*sem, ymin=mean.symp-1.96*sem))
dodge <- position_dodge(width=0)

days.out<-c("07/08","07/09","07/15","07/28","08/03")


ggplot(graph.total.symp,aes(x=factor(phase),y=mean.symp,group=factor(bdg)))+
  geom_line(aes(linetype=factor(bdg)),position=dodge)+
  geom_point(position=dodge)+
  geom_errorbar(symp.limits,width=0.3,position=dodge)+
  #scale_linetype_manual(values=c(1,2),labels=c("No AC","AC"))+
  theme_classic(base_family="Arial",base_size=16)+labs(y="Total number of symptoms",x="")+
  theme(axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"))+
  theme(legend.title=element_blank(),legend.position ="none")+
  theme(legend.key = element_blank())


daily721$total.catf<-rowSums(daily721[,c(155,156,169:173)])

graph.total.catf<-na.omit(ddply(subset(daily721, !pid%in%pid.out),.
                                (bdg,phase),summarise,sd.symp=sd(total.catf,na.rm=TRUE),mean.symp=mean(total.catf,na.rm=TRUE),n=length(bdg)))

graph.total.catf$sem<-with(graph.total.catf,sd.symp/sqrt(n))

symp.limits<-with(graph.total.catf, aes(ymax=mean.symp+1.96*sem, ymin=mean.symp-1.96*sem))
dodge <- position_dodge(width=0)

days.out<-c("07/08","07/09","07/15","07/28","08/03")


ggplot(subset(graph.total.catf,!phase%in%days.out),aes(x=phase,y=mean.symp,group=factor(bdg)))+
  geom_line(aes(linetype=factor(bdg)),position=dodge)+
  geom_point(position=dodge)+
  geom_errorbar(symp.limits,width=0.3,position=dodge)+
  #scale_linetype_manual(values=c(1,2),labels=c("No AC","AC"))+
  theme_classic(base_family="Arial",base_size=16)+labs(y="Total number of symptoms",x="")+
  theme(axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"))+
  theme(legend.title=element_blank(),legend.position ="none")+
  theme(legend.key = element_blank())

#plot number of symptops vs number of diagnosed pre-conditions

cols.int<-c("pid.x","SurveyDate","bdg","phase","total.catf","total.symp",colnames(dailyjun17)[307:329])

summary(as.factor(dailyjun17$Diagnosis_Depression))
rmnans<-function(x){x<-ifelse(is.na(x),0,x)}

dailyjun17[,307:329]<-sapply(dailyjun17[,307:329],function(x)rmnans(x))

testdf<-dailyjun17[,colnames(dailyjun17)%in%cols.int]
testdf<-melt(testdf,id.vars=c("pid.x","phase","SurveyDate","bdg","total.catf"))

testdf2<-ddply(testdf,.(bdg,phase,variable,value),summarise,mean.symp=mean(total.symp,na.rm=TRUE),sd.symp=sd(total.catf,na.rm=TRUE),n=length(phase))




testdf2$sem<-with(testdf2,sd.symp/sqrt(n))

symp.limits<-with(graph.total.catf, aes(ymax=mean.symp+1.96*sem, ymin=mean.symp-1.96*sem))
dodge <- position_dodge(width=0)

days.out<-c("07/08","07/09","07/15","07/28","08/03")

preconditions<-unique(testdf2$variable)


ggplot(subset(testdf2,variable%in%preconditions[8:10]),aes(x=phase,y=mean.symp,group=factor(value),color=factor(value)))+
  geom_line(position=dodge)+
  geom_point(position=dodge)+
  geom_errorbar(symp.limits,width=0.3,position=dodge)+
  #scale_linetype_manual(values=c(1,2),labels=c("No AC","AC"))+
  theme_classic(base_family="Arial",base_size=16)+labs(y="Total number of symptoms",x="")+
  theme(axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"))+
  theme(legend.title=element_blank(),legend.position ="bottom")+
  theme(legend.key = element_blank())+facet_wrap(~bdg)+facet_grid(variable~bdg)

#check hives, check immune

#treatment vars
tnum<-seq(356,400,2)
tvars<-colnames(dailyjun17[tnum])

cols.int<-c("pid.x","SurveyDate","bdg","phase","total.catf",tvars)


testdf<-dailyjun17[,colnames(dailyjun17)%in%cols.int]
testdf<-melt(testdf,id.vars=c("pid.x","phase","SurveyDate","bdg","total.catf"))

testdf2<-ddply(testdf,.(bdg,phase,variable,value),summarise,mean.symp=mean(total.catf,na.rm=TRUE),sd.symp=sd(total.catf,na.rm=TRUE),n=length(phase))




testdf2$sem<-with(testdf2,sd.symp/sqrt(n))

symp.limits<-with(graph.total.catf, aes(ymax=mean.symp+1.96*sem, ymin=mean.symp-1.96*sem))
dodge <- position_dodge(width=0)

tvars<-colnames(dailyjun17[tnum])

ggplot(subset(testdf2,variable%in%tvars[7:10]),aes(x=phase,y=mean.symp,group=factor(value),color=factor(value)))+
  geom_line(position=dodge)+
  geom_point(position=dodge)+
  geom_errorbar(symp.limits,width=0.3,position=dodge)+
  #scale_linetype_manual(values=c(1,2),labels=c("No AC","AC"))+
  theme_classic(base_family="Arial",base_size=16)+labs(y="Total number of symptoms",x="")+
  theme(axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"))+
  theme(legend.title=element_blank(),legend.position ="bottom")+
  theme(legend.key = element_blank())+facet_wrap(~bdg)+facet_grid(variable~bdg)