a<-read.xlsx("C:\\GGT data.xlsx")
#Table 1####
library(tableone)
myVars<-c("followuptime","AGE","SEX","EDU","INCOME","SMOKE","DRINK","totall_MET","BMI","DMTIME","KFY","YDS","JZY","XZ","CHD","STROKE")
catVars<-c("SEX","EDU","INCOME","SMOKE","DRINK","KFY","YDS","JZY","XZ","CHD","STROKE")
tab<-CreateTableOne(vars = myVars,data=a,factorVars = catVars)
print(tab,showAllLevels=TRUE,catDigits = 1,contDigits = 1)
tab<-CreateTableOne(vars = myVars,data=a,factorVars = catVars,strata="logggt1")
print(tab,showAllLevels=TRUE,catDigits = 1,contDigits = 1)

#Table 2####
library(survival)
library(survminer)
#death
table(a$death,a$logggt1)
model1<-coxph(Surv(time,death)~logggt1+AGE+SEX,data=a) %>% summary() 
model1
model2<-coxph(Surv(time,death)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+JZY+CHD+STROKE,data=a) %>% summary()
model2
model3<-coxph(Surv(time,death)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+JZY+CHD+STROKE+DMTIME+KFY+YDS,data=a) %>% summary()
model3
#CVD
table(a$CVDdeath,a$logggt1)
model1<-coxph(Surv(time,CVDdeath)~logggt1+AGE+SEX,data=a)%>% summary()
model1
model2<-coxph(Surv(time,CVDdeath)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+JZY+CHD+STROKE,data=a) %>% summary()
model2
model3<-coxph(Surv(time,CVDdeath)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+JZY+CHD+STROKE+DMTIME+KFY+YDS,data=a)%>% summary()
model3
#cancer
table(a$cancerdeath,a$logggt1)
model1<-coxph(Surv(time,cancerdeath)~logggt1+AGE+SEX,data=a)%>% summary()
model1
model2<-coxph(Surv(time,cancerdeath)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+JZY,data=a) %>% summary()
model2
model3<-coxph(Surv(time,cancerdeath)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+JZY+DMTIME+KFY+YDS,data=a)%>% summary()
model3

#Supplementary Table 1####
b<-a[a$time>730,]
#death
table(b$death,b$logggt1)
model1<-coxph(Surv(time,death)~logggt1+AGE+SEX,data=b) %>% summary()
model1
model2<-coxph(Surv(time,death)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+JZY+CHD+STROKE,data=b) %>% summary()
model2
model3<-coxph(Surv(time,death)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+JZY+CHD+STROKE+DMTIME+KFY+YDS,data=b) %>% summary()
model3
#CVD
table(b$CVDdeath,b$logggt1)
model1<-coxph(Surv(time,CVDdeath)~logggt1+AGE+SEX,data=b) %>% summary()
model1
model2<-coxph(Surv(time,CVDdeath)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+JZY+CHD+STROKE,data=b) %>% summary()
model2
model3<-coxph(Surv(time,CVDdeath)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+JZY+CHD+STROKE+DMTIME+KFY+YDS,data=b) %>% summary()
model3
#cancer
table(b$cancerdeath,b$logggt1)
model1<-coxph(Surv(time,cancerdeath)~logggt1+AGE+SEX,data=b) %>% summary()
model1
model2<-coxph(Surv(time,cancerdeath)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+JZY,data=b) %>% summary()
model2
model3<-coxph(Surv(time,cancerdeath)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+JZY+DMTIME+KFY+YDS,data=b) %>% summary()
model3

#Supplementary table 2####
c<-a[a$accidentaldeath==0,]
#death
table(c$death,c$logggt1)
model1<-coxph(Surv(time,death)~logggt1+AGE+SEX,data=c) %>% summary()
model1
model2<-coxph(Surv(time,death)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+JZY+CHD+STROKE,data=c) %>% summary()
model2
model3<-coxph(Surv(time,death)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+JZY+CHD+STROKE+DMTIME+KFY+YDS,data=c) %>% summary()
model3
#CVD
table(c$CVDdeath,c$logggt1)
model1<-coxph(Surv(time,CVDdeath)~logggt1+AGE+SEX,data=c) %>% summary()
model1
model2<-coxph(Surv(time,CVDdeath)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+JZY+CHD+STROKE,data=c) %>% summary()
model2
model3<-coxph(Surv(time,CVDdeath)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+JZY+CHD+STROKE+DMTIME+KFY+YDS,data=c) %>% summary()
model3
#cancer
table(c$cancerdeath,c$logggt1)
model1<-coxph(Surv(time,cancerdeath)~logggt1+AGE+SEX,data=c) %>% summary()
model1
model2<-coxph(Surv(time,cancerdeath)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+JZY,data=c) %>% summary()
model2
model3<-coxph(Surv(time,cancerdeath)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+JZY+DMTIME+KFY+YDS,data=c) %>% summary()
model3

#Supplementary table 3####
d<-a[a$JZY==0,]
#death
table(d$death,d$logggt1)
model1<-coxph(Surv(time,death)~logggt1+AGE+SEX,data=d) %>% summary()
model1
model2<-coxph(Surv(time,death)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+CHD+STROKE,data=d) %>% summary()
model2
model3<-coxph(Surv(time,death)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+CHD+STROKE+DMTIME+KFY+YDS,data=d) %>% summary()
model3
#CVD
table(d$CVDdeath,d$logggt1)
model1<-coxph(Surv(time,CVDdeath)~logggt1+AGE+SEX,data=d) %>% summary()
model1
model2<-coxph(Surv(time,CVDdeath)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+CHD+STROKE,data=d) %>% summary()
model2
model3<-coxph(Surv(time,CVDdeath)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+CHD+STROKE+DMTIME+KFY+YDS,data=d) %>% summary()
model3
#cancer
table(d$cancerdeath,d$logggt1)
model1<-coxph(Surv(time,cancerdeath)~logggt1+AGE+SEX,data=d) %>% summary()
model1
model2<-coxph(Surv(time,cancerdeath)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET,data=d) %>% summary()
model2
model3<-coxph(Surv(time,cancerdeath)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+DMTIME+KFY+YDS,data=d) %>% summary()
model3

#Supplementary table 4####
e<-a[a$BMI>28,]
#death
table(e$death,e$logggt1)
model1<-coxph(Surv(time,death)~logggt1+AGE+SEX,data=e) %>% summary()
model1
model2<-coxph(Surv(time,death)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+CHD+STROKE,data=e) %>% summary()
model2
model3<-coxph(Surv(time,death)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+CHD+STROKE+DMTIME+KFY+YDS,data=e) %>% summary()
model3
#CVD
table(e$CVDdeath,e$logggt1)
model1<-coxph(Surv(time,CVDdeath)~logggt1+AGE+SEX,data=e) %>% summary()
model1
model2<-coxph(Surv(time,CVDdeath)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+CHD+STROKE,data=e) %>% summary()
model2
model3<-coxph(Surv(time,CVDdeath)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+CHD+STROKE+DMTIME+KFY+YDS,data=e) %>% summary()
model3
#cancer
table(e$cancerdeath,e$logggt1)
model1<-coxph(Surv(time,cancerdeath)~logggt1+AGE+SEX,data=e) %>% summary()
model1
model2<-coxph(Surv(time,cancerdeath)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET,data=e) %>% summary()
model2
model3<-coxph(Surv(time,cancerdeath)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+DMTIME+KFY+YDS,data=e) %>% summary()
model3

#Supplementary table 5####
f<-read.xlsx("C:\\Supplementary table5.xlsx")
#death
table(f$death,f$logggt1)
model1<-coxph(Surv(time,death)~logggt1+AGE+SEX,data=f) %>% summary()
model1
model2<-coxph(Surv(time,death)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+CHD+STROKE,data=f) %>% summary()
model2
model3<-coxph(Surv(time,death)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+CHD+STROKE+DMTIME+KFY+YDS,data=f) %>% summary()
model3
#CVD
table(f$CVDdeath,f$logggt1)
model1<-coxph(Surv(time,CVDdeath)~logggt1+AGE+SEX,data=f) %>% summary()
model1
model2<-coxph(Surv(time,CVDdeath)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+CHD+STROKE,data=f) %>% summary()
model2
model3<-coxph(Surv(time,CVDdeath)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+CHD+STROKE+DMTIME+KFY+YDS,data=f) %>% summary()
model3
#cancer
table(f$cancerdeath,f$logggt1)
model1<-coxph(Surv(time,cancerdeath)~logggt1+AGE+SEX,data=f) %>% summary()
model1
model2<-coxph(Surv(time,cancerdeath)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET,data=f) %>% summary()
model2
model3<-coxph(Surv(time,cancerdeath)~logggt1+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+DMTIME+KFY+YDS,data=f) %>% summary()
model3


#Figure 1 ####
#all-cause RCS
library(Hmisc)
library(rms)
dd<-datadist(a)
options(datadist="dd")
fit<-cph(Surv(time,death)~rcs(logggt,4)+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+DMTIME+KFY+CHD+STROKE+JZY,data =a)
dd$limits$logggt[2]<-1.9
fit<-update(fit)
anova(fit)
hr<-Predict(fit,logggt,fun = exp,ref.zero = T)
P1<-ggplot()+geom_line(data=hr, aes(logggt,yhat),linetype="solid",size=1,alpha =1.1 ,color="red")+geom_ribbon(data=hr, aes(logggt,ymin = lower, ymax = upper),alpha = 0.1,linetype = 5,fill="white",color="black")+theme_classic()+geom_hline(yintercept=1, linetype=2,size=1)+labs( x="Log-transformed GGT levels", y="HR (95%CI) of all-cause mortality")+scale_x_continuous(limits=c(1.9,6.5), breaks=seq(2,6.5,0.5))+scale_y_continuous(limits=c(0,6), breaks=seq(0,9,1))
P1
#CVD RCS
fit<-cph(Surv(time,CVDdeath)~rcs(logggt,4)+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+DMTIME+KFY+CHD+STROKE+JZY,data =a)
dd$limits$logggt[2]<-1.9
fit<-update(fit)
anova(fit)
hr<-Predict(fit,logggt,fun = exp,ref.zero = T)
P2<-ggplot()+geom_line(data=hr, aes(logggt,yhat),linetype="solid",size=1,alpha =1.1 ,color="red")+geom_ribbon(data=hr, aes(logggt,ymin = lower, ymax = upper),alpha = 0.1,linetype = 5,fill="white",color="black")+theme_classic()+geom_hline(yintercept=1, linetype=2,size=1)+labs( x="Log-transformed GGT levels", y="HR (95%CI) of CVD mortality")+scale_x_continuous(limits=c(1.9,6.5), breaks=seq(2,6.5,0.5))+scale_y_continuous(limits=c(0,8), breaks=seq(0,9,1))
P2

#cancer RCS
fit<-cph(Surv(time,cancerdeath)~rcs(logggt,4)+AGE+SEX+EDU+INCOME+SMOKE+DRINK+BMI+totall_MET+DMTIME+KFY+JZY,data =a)
dd$limits$logggt[2]<-1.9
fit<-update(fit)
anova(fit)
hr<-Predict(fit,logggt,fun = exp,ref.zero = T)
P3<-ggplot()+geom_line(data=hr, aes(logggt,yhat),linetype="solid",size=1,alpha =1.1 ,color="red")+geom_ribbon(data=hr, aes(logggt,ymin = lower, ymax = upper),alpha = 0.1,linetype = 5,fill="white",color="black")+theme_classic()+geom_hline(yintercept=1, linetype=2,size=1)+labs( x="Log-transformed GGT levels", y="HR (95%CI) of cancer mortality")+scale_x_continuous(limits=c(1.9,6.5), breaks=seq(2,6.5,0.5))+scale_y_continuous(limits=c(0,8), breaks=seq(0,9,1))
P3

#Figuer 2####
library(forestplot)
m<-read.xlsx("C:\\Figure 2.xlsx")
forestplot(labeltext=as.matrix(m[,c(1:2,6)]),mean=m$v3,lower=m$v4,upper=m$v5,is.summary=c(T,T,rep(F,10),T,rep(F,10),T,rep(F,10),T,rep(F,10),T,rep(F,10),T,rep(F,10)),
           graph.pos=3,graphwidth=unit(70,"mm"),boxsize=0.2,line.margin=unit(0.5,"mm"),lineheight=unit(5,"mm"),
           col=fpColors(box = "grey0",lines = "grey0",zero="black"),colgap=unit(5,"mm"),txt_gp = fpTxtGp(title=gpar(cex=1.2),ticks=gpar(cex=1)),
           title = "all-cause mortality",zero=1,xticks=c(0.5,1.0,1.5,2.0,2.5),ci.vertices=T)

forestplot(labeltext=as.matrix(m[,c(1:2,10)]),mean=m$v7,lower=m$v8,upper=m$v9,is.summary=c(T,T,rep(F,10),T,rep(F,10),T,rep(F,10),T,rep(F,10),T,rep(F,10),T,rep(F,10)),
           graph.pos=3,graphwidth=unit(70,"mm"),boxsize=0.2,line.margin=unit(0.5,"mm"),lineheight=unit(5,"mm"),
           col=fpColors(box = "grey0",lines = "grey0",zero="black"),colgap=unit(5,"mm"),txt_gp = fpTxtGp(title=gpar(cex=1.2),ticks=gpar(cex=1)),
           title = "CVD mortality",zero=1,xticks=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5),ci.vertices=T)

forestplot(labeltext=as.matrix(m[,c(1:2,14)]),mean=m$v11,lower=m$v12,upper=m$v13,is.summary=c(T,T,rep(F,10),T,rep(F,10),T,rep(F,10),T,rep(F,10),T,rep(F,10),T,rep(F,10)),
           graph.pos=3,graphwidth=unit(70,"mm"),boxsize=0.2,line.margin=unit(0.5,"mm"),lineheight=unit(5,"mm"),
           col=fpColors(box = "grey0",lines = "grey0",zero="black"),colgap=unit(5,"mm"),txt_gp = fpTxtGp(title=gpar(cex=1.2),ticks=gpar(cex=1)),
           title = "cancer mortality",zero=1,xticks=c(0.5,1.0,1.5,2.0,2.5,3.0),ci.vertices=T)

#Supplementary Figure 1####
#GGT
hist(a$GGT,freq =F,breaks =500,xlim =c(0,200),ylim = c(0,0.04),xlab = "Serum GGT,(U/L)" )
xfit<-seq(min(a$GGT),max(a$GGT),by=0.1)
yfit<-dnorm(xfit,mean(a$GGT),sd(a$GGT))
lines(xfit,yfit,lwd=2)

#logggt
hist(a$logggt,freq =F,breaks = 30,ylim = c(0,0.8),main = "",xlab = "Log-transformed GGT")
xfit<-seq(min(a$logggt),max(a$logggt),by=0.1)
yfit<-dnorm(xfit,mean(a$logggt),sd(a$logggt))
lines(xfit,yfit,lwd=2)

#Supplementary Figure 2####
l<-read.xlsx("C:\\Supplementary figure 2.xlsx")
forestplot(labeltext=as.matrix(l[,c(1:2)]),mean=l$v3,lower=l$v4,upper=l$v5,is.summary=c(T,T,T,rep(F,10),T,T,rep(F,30)),
           graph.pos=3,graphwidth=unit(100,"mm"),boxsize=0.2,line.margin=unit(0.5,"mm"),lineheight=unit(6,"mm"),
           col=fpColors(box = "grey0",lines = "grey0",zero="black"),colgap=unit(5,"mm"),txt_gp = fpTxtGp(title=gpar(cex=1.2),ticks=gpar(cex=1)),
           title = "cancer mortality",zero=1,xticks=c(0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0),ci.vertices=T)



