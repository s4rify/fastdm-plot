#figure 3.1 # nf<-layout(matrix(c(2,0,1,3),2,2,byrow=TRUE), c(2,1), c(1,2),TRUE)
plot(pulse~heights,xlab="Heights (cm)", ylab="Pulse rate (bpm)")
reg<-lm(pulse~heights)
abline(reg)
pred<-predict(reg,se.fit=TRUE)
fitval<-pred$fit
se<-pred$se.
fitindex<-order(heights)
y<-fitval[index]
se<-se[index]
yu<-y+1.96*se
yl<-y-1.96*se
lines(heights[index],yu,lty=2)
lines(heights[index],yl,lty=2)
hist(heights,ylab="Frequency",xlab="Heights",main="")
boxplot(pulse)

