############################## Code for Figure 1 ##############################

### Plot (left) for Figure 1 ###
x<-seq(-4.5,4.5,0.001)
plot(x,pnorm(x,mean=0), type="l",lwd=2,xlab="x",ylab="",cex.lab=1.3,axes=F,xlim=c(-4.2,4.2))
points(x,pnorm(x,mean=2), type="l",lwd=2)
points(x,pnorm(x,1,sqrt(1/2)),type="l",lwd=2)
axis(1,labels=FALSE)
axis(2,labels=TRUE,font=2)
H<-expression(H(x))
Fj<-expression(F[j](x))
Fs<-expression(F[s](x))
pj<-expression(p[j]<p[s])
text(2.2,0.8,H,cex=1.3)
text(-1.1,0.4,Fj,cex=1.3)
text(2.7,0.4,Fs,cex=1.3)
text(-4,0.95,"(a)",cex=1.3)
text(-3,0.95,pj,cex=1.6)

### Plot (right) for Figure 1 ###
x<-seq(-4.5,4.5,0.001)
plot(x,pnorm(x,mean=0), type="l",lwd=2,xlab="x",ylab="",cex.lab=1.3,axes=F,xlim=c(-4.2,4.2))
points(x,pnorm(x,mean=2), type="l",lwd=2)
points(x,pnorm(x,1,sqrt(1/2)),type="l",lwd=2)
axis(1,labels=FALSE)
axis(2,labels=TRUE,font=2)
H<-expression(H(x))
Fj<-expression(F[j](x))
Fs<-expression(F[s](x))
pj<-expression(p[j]>p[s])
text(2.2,0.8,H,cex=1.3)
text(-1.1,0.4,Fs,cex=1.3)
text(2.6,0.4,Fj,cex=1.3)
text(-4,0.95,"(b)",cex=1.3)
text(-3,0.95,pj,cex=1.6)

####################### Opening the Package #######################

library("nparLD")

############################ Example 1 ############################

data("dental")

### Box plot for Figure 2 ###
boxplot(resp~time, data=dental, lwd=2, xlab="time",
font.lab=2,cex.lab=2, main="Box Plots")

### 95% confidence interval plot for Figure 2 ###
ex.f1np<-nparLD(resp~time, data=dental,
subject="subject", description=FALSE)
plot(ex.f1np)

### Summary of the statistical analysis ###
summary(ex.f1np)

### Multiple comparisons as in Table 4 ###
### 810 is for ages 8 vs 10 ###
### 812 is for ages 8 vs 12 ###
### 814 is for ages 8 vs 14 ###

m810<-which(((dental$time==8)+(dental$time==10))==1)
m812<-which(((dental$time==8)+(dental$time==12))==1)
m814<-which(((dental$time==8)+(dental$time==14))==1)

ex.f1np810<-nparLD(resp~time, data=dental[m810,],
subject="subject", description=FALSE)
ex.f1np812<-nparLD(resp~time, data=dental[m812,],
subject="subject", description=FALSE)
ex.f1np814<-nparLD(resp~time, data=dental[m814,],
subject="subject", description=FALSE)

summary(ex.f1np810)
summary(ex.f1np812)
summary(ex.f1np814)

### Statistical analysis using the nlme package ###
library("nlme")
ex.f1lme<-lme(resp ~ time, data = dental, random = ~ 1 | subject)
summary(ex.f1lme)

### Multiple comparisons using nlme ###
ex.f1lme810<-lme(resp ~ time, data = dental[m810,],
random = ~ 1 | subject)
ex.f1lme812<-lme(resp ~ time, data = dental[m810,],
random = ~ 1 | subject)
ex.f1lme814<-lme(resp ~ time, data = dental[m810,],
random = ~ 1 | subject)

summary(ex.f1lme810)
summary(ex.f1lme812)
summary(ex.f1lme814)

############################ Example 2 ############################

data("rat")

### Box plot for Figure 3 ###
boxplot(resp~group*time, data=rat,
names=FALSE,col=c("grey",2,3),lwd=2)
axis(1,at=2,labels="Time 0",font=2,cex=2)
axis(1,at=5,labels="Time 1",font=2,cex=2)
axis(1,at=8,labels="Time 2",font=2,cex=2)
axis(1,at=11,labels="Time 3",font=2,cex=2)
axis(1,at=14,labels="Time 4",font=2,cex=2)
legend(2,190,c("Control","Thiour","Thyrox"),
lwd=c(3,3,3),col=c("grey",2,3),cex=2)

### 95% confidence interval plot for Figure 3 ###
ex.f1f1np<-nparLD(resp~time*group, data=rat,
subject="subject", description=FALSE)
plot(ex.f1f1np)

### Summary of the statistical analysis ###
summary(ex.f1f1np)

############################ Example 3 ############################

data("respiration")
par(mfrow=c(1,2))

### Box plot (left) for Figure 4 ###
boxplot(resp~treatment*time,data=respiration[which(center==1),],
names=FALSE,ylim=c(-1,5),col=c("grey",2),lwd=2,main="Center 1")
axis(1,at=2,labels="Time 1",font=2,cex=2)
axis(1,at=5,labels="Time 2",font=2,cex=2)
axis(1,at=8,labels="Time 3",font=2,cex=2)
axis(1,at=11,labels="Time 4",font=2,cex=2)
axis(1,at=14,labels="Time 5",font=2,cex=2)
legend(2,5,c("Treat A","Treat P"),
lwd=c(2,2),col=c("grey",2),cex=1.2)

### Box plot (right) for Figure 4 ###
boxplot(resp~treatment*time,data=respiration[which(center==2),],
names=FALSE,ylim=c(-1,5),col=c("grey",2),lwd=2,main="Center 2")
axis(1,at=2,labels="Time 1",font=2,cex=2)
axis(1,at=5,labels="Time 2",font=2,cex=2)
axis(1,at=8,labels="Time 3",font=2,cex=2)
axis(1,at=11,labels="Time 4",font=2,cex=2)
axis(1,at=14,labels="Time 5",font=2,cex=2)
legend(2,5,c("Treat A","Treat P"),
lwd=c(2,2),col=c("grey",2),cex=1.2)

### 95% confidence interval plot for Figure 4 ###
ex.f2f1np<-nparLD(resp~time*center*treatment,
data=respiration, subject="patient", description=FALSE)
plot(ex.f2f1np)

### Summary of the statistical analysis ###
print(ex.f2f1np)

### Statistical analysis using the nlme package ###
library("nlme")

ex.f2f1lme <- lme(resp ~ time*treatment*center,
data = respiration,random = ~ 1 | patient)
summary(ex.f2f1lme)
