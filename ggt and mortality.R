

#Table 1####
library(tableone)
myVars<-c("followuptime","AGE","SEX","EDU","INCOME","SMOKE","DRINK","totall_MET","BMI","DMTIME","KFY","YDS","JZY","XZ","CHD","STROKE")
catVars<-c("SEX","EDU","INCOME","SMOKE","DRINK","KFY","YDS","JZY","XZ","CHD","STROKE")
tab<-CreateTableOne(vars = myVars,data=a,factorVars = catVars)
print(tab,showAllLevels=TRUE,catDigits = 1,contDigits = 1)
tab<-CreateTableOne(vars = myVars,data=a,factorVars = catVars,strata="logggt1")
print(tab,showAllLevels=TRUE,catDigits = 1,contDigits = 1)


#Table 2####
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



