#---------- exercise 3

#question 1
klm=scan("/Users/sarasal/Desktop/Assignment2/Data Files/klm.txt")
qqnorm(klm,main="QQ-plot of KLM")
m=sum(klm>31)
n= length(klm)
binom.test(m,n,p=0.5)

#question 2
m=sum(klm>72)
n= length(klm)
binom.test(m,n,p=0.1)


#---------- exercise 6

#question 1
run=read.table("/Users/sarasal/Desktop/Assignment2/Data Files/run.txt")
run
par(mfrow=c(1,2))
qqnorm(soft_drink[,1],main = 'QQ-Plot Before Soft Drink')
qqnorm(soft_drink[,2],main = 'QQ-Plot After Soft Drink')
hist(soft_drink[,1],main = 'Before Soft Drink')
hist(soft_drink[,2],main = 'After Soft Drink')
boxplot(soft_drink[,1],main = 'Before Soft Drink')
boxplot(soft_drink[,2],main = 'After Soft Drink')

qqnorm(energy[,1],main = 'QQ-Plot Before Energy')
qqnorm(energy[,2],main = 'QQ-Plot After Energy')
hist(energy[,1],main = 'Before Energy')
hist(energy[,2],main = 'After Energy')
boxplot(energy[,1],main = 'Before Energy')
boxplot(energy[,2],main = 'After Energy')

#question 2
soft_drink=run[run$drink=='lemo', 1:2]
energy=run[run$drink=='energy', 1:2]
mystat=function(x,y) {mean(x-y)}
B=1000


#soft drink
#t.test(soft_drink[,1],soft_drink[,2],paired = TRUE)

soft_drink_tstar=numeric(B)
for (i in 1:B){
  soft_drink_star=t(apply(cbind(soft_drink[,1],soft_drink[,2]),1,sample))
  soft_drink_tstar[i]=mystat(soft_drink_star[,1],soft_drink_star[,2])
  }
soft_drink_myt=mystat(soft_drink_star[,1],soft_drink_star[,2])
hist(soft_drink_tstar, main = 'Soft Drink T*')
pl=sum(soft_drink_tstar<soft_drink_myt)/B
pr=sum(soft_drink_tstar>soft_drink_myt)/B
p_soft_drink=2*min(pl,pr)
p_soft_drink


#energy
#t.test(energy[,1],energy[,2],paired = TRUE)

energy_tstar=numeric(B)
for (i in 1:B){
  energy_star=t(apply(cbind(energy[,1],energy[,2]),1,sample))
  energy_tstar[i]=mystat(energy_star[,1],energy_star[,2])
}
energy_myt=mystat(energy_star[,1],energy_star[,2])
hist(energy_tstar, main = 'Energy T*')
pl=sum(energy_tstar<energy_myt)/B
pr=sum(energy_tstar>energy_myt)/B
p_energy=2*min(pl,pr)
p_energy

#question 3
n=nrow(run)
diff_time <- data.frame(numeric(n), character(n))
diff_time[,1]= run[,2]- run[,1]
diff_time[,2]= run[,3]
wilcox.test(diff_time[1:12,1],diff_time[13:24,1])
#diff_time

#question 6
par(mfrow=c(1,2))
qqnorm(diff_time[1:12,1],main = 'QQ-Plot of Residual lemo')
qqnorm(diff_time[13:24,1],main = 'QQ-Plot of Residual soft')
t.test(diff_time[1:12,1],diff_time[13:24,1],paired = TRUE)

#---------- exercise 7

#question 1
dogs=read.table("/Users/sarasal/Desktop/Assignment2/Data Files/dogs.txt",header = TRUE)
dogs
par(mfrow=c(1,3))
boxplot(dogs[,1], main = "V1: isofluorane")
boxplot(dogs[,2], main = "V2: halothane")
boxplot(dogs[,3], main = "V3: cyclopropane")

qqnorm(dogs[,1], main = "QQ-plot V1: isofluorane")
qqnorm(dogs[,2], main = "QQ-plot V2: halothane")
qqnorm(dogs[,3], main = "QQ-plot V3: cyclopropane")

#question 2
dogsframe= data.frame(plasma=as.vector(as.matrix(dogs)), drugs=factor(rep(1:3,each=10)))
dogsaov= lm(plasma~drugs,data = dogsframe)
anova(dogsaov)
summary(dogsaov)

drug1 = 0.4340
drug2 = drug1 - 0.0350
drug3 = drug1 - 0.4190
drug1; drug2; drug3

#question 3
attach(dogsframe)
kruskal.test(plasma,drugs)
