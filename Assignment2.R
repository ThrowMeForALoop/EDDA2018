# Exercise 1

setwd("C:/Uni_Projects/EDDA/Assignment 2");
data <- scan(file = "telephone_exe1.txt")  # Before running this command edit the input file removing the header and the
                                      #empty line.
#Item 1.1
#Answer: The resolution is twofold.
#Two Bootstrat tests will be executed to demonstrate whether the data could stem from an exponential distribution
#with lambda between 0.01 and 0.1.
#
#Having a lambda between 0.01 and 0.1 in a exponential distribution means that the mean for this population
#would be contained in the interval 1/(0.1) -> 10 and 1/(0.01) -> 100 [10;100].
#
#Thus, the first test will consist of a null hypothesis that lambda = 0.1 (lowerBound ==> mean = 10) with alternative 
#hypothesis that lambda < 0.1 (mean > 10), the idea is to demonstrate that given this alternative hypothesis 
#we reject the null hypothesis in favor of the alternative hypothesis (lambda < 0.1).
#
#The second test will use null hypothesis that lambda = 0.01 (upperBound) with alternative hypothesis that lambda > 0.01 (mean <= 100)
# the idea is to demonstrate that given this alternative hypothesis we reject the null hypothesis in favor of the alternative
#hypothesis (lambda > 0.01).

t=median(data) # The statistic to be used is the median - t = median for the given population's sample

B=1000; # Number of simulations
tstarUpperBound=numeric(B)
tstarLowerBound=numeric(B)
n=length(data)

#First test
# Generating the simulations, creating simulated samples Xstars and computing the statistic over that simulation Tstars
# considering H0 a distribution with lambda = 0.1 (mean = 10) and H1 lambda < 0.1 (mean > 10).
# Lambda = 0.1 means that the exp. distribution has an average 1/0.1 = 10 with a sd. = sqr(1/(0.1^2) = 10.

for (i in 1:B){
  xstarLower=rexp(n,0.1) # generating simulated samples
  tstarLowerBound[i]=median(xstarLower) #generating surrogated ts
}

# Calculating p-value for this test.
pLowerBound=sum(tstarLowerBound>t)/B # P-value upon which we would reject (or fail to reject) the null hypothesis.
                           # This is the probability of computing the statistic with values higher than the observed. 
pLowerBound
# As can be seen, p is very low (0), so we reject the null hypothesis in favor of the alternative hypothesis.

#Second test
# Now generating another simulation but considering different null and alternative hypothesis:
# H0 distribution is a exp distribution with lambda = 0.01 (mean =100) and H1 lambda is > 0.01 (mean < 100)
# Lambda = 0.01 means that the exp. distribution has an average 1/0.01 = 100 with a sd. = sqr(1/(0.01^2) = 100.
for (i in 1:B){
  xstarUpper=rexp(n,0.01) # generating simulated samples
  tstarUpperBound[i]=median(xstarUpper) #generating surrogated ts
}

pUpperBound=sum(tstarUpperBound<t)/B # P-value upon which we would reject the null hypothesis.
# This is the probability of computing the statistic with values higher than the observed. 

pUpperBound
# As can be seen, p is very low(0), so we reject the null hypothesis in favor of the alternative hypothesis.

#Conclusion: Given the two tests above is very plausible that lambda is located between 0.01 and 0.1.

# Item 1.2:
# We generate the bootstrap interval, using the median as location estimator.

TstarBootstrapInterval = numeric(B)
for(i in 1:B){
  Xstar = sample(data,replace=TRUE) #Generates a ramdon permutation of 'size of data' elements
  TstarBootstrapInterval[i]=mean(Xstar) # Computes the statistic for the sample.
}
Tstar25=quantile(TstarBootstrapInterval,0.025)
Tstar975=quantile(TstarBootstrapInterval,0.975)

T1 = mean(data)
c(2*T1-Tstar975,2*T1-Tstar25) # ==> mean is in this interval

#    97.5%     2.5% 
# 38.12714 48.77792 Bootstrap interval for the mean of this population with 95% confidence.

par(mfrow=c(1,3))
hist(data, prob=T)
hist(TstarBootstrapInterval)
boxplot(TstarBootstrapInterval)
# Conclusion: Considering the boostrap confidence interval, the mean consumption for this population falls in the
#interval between 38.12714 and 48.77792, so the marketing manager could potentially focus on consumers within this
# range.


### Exercise 2
light1879_dataframe=read.table("light1879.txt", header = FALSE)
light1882_dataframe=read.table("light1882.txt", fill = TRUE)
light_dataframe= read.table("light.txt", header = FALSE)

light1879_vec = unlist(light1879_dataframe, use.names = FALSE)
light1879_vec = light1879_vec + 299000

light1882_vec = unlist(light1882_dataframe, use.names = FALSE)
light1882_vec <- light1882_vec[!is.na(light1882_vec)]
light1882_vec = light1882_vec + 299000

light_vec = unlist(light_dataframe, use.names = FALSE)
light_vec = 7.442 / (((light_vec/1000) + 24.8)/1000000)

# Question 2.1
par(mfrow=c(1,2))

# Answer: Given the boxplot and histogram of light1879 dataset, we can observe some outliers and the qqline is not straight
# so we can presume that this dataset doesn't follow a normal distribution.
hist(light1879_vec, xlab = "Light velocity 1879")
boxplot(light1879_vec)
qqnorm(light1879_vec)
qqline(light1879_vec)

# Answer: Similary, we can observe some outliers in the boxplot diagram
# and the qqline is not straight in the histogram of 1882 dataset
# We can consider that this dataset is not in normal distribution

hist(light1882_vec, xlab = "Light velocity 1882")
boxplot(light1882_vec)
qqnorm(light1882_vec)
qqline(light1882_vec)

# Answer: As we can see from the histogram of light.txt dataset, the points are approximately on a straight line
# then this dataset can be assumed to be sampled from a normal distribution
hist(light_vec, xlab = "Light velocity")
boxplot(light_vec)
qqnorm(light_vec)
qqline(light_vec)

# Question 2.2
# Dataset 1879
#### MEAN 
B=1000
Tstar=numeric(B)
for(i in 1:B) {
    Xstar=sample(light1879_vec, replace=TRUE)
    Tstar[i]=mean(Xstar)
}
Tstar25=quantile(Tstar,0.025)
Tstar975=quantile(Tstar,0.975)
Tmean= mean(light1879_vec)
c(2*Tmean-Tstar975,2*Tmean-Tstar25)

#### Median
Tstar=numeric(B)
for(i in 1:B) {
  Xstar=sample(light1879_vec, replace=TRUE)
  Tstar[i]=median(Xstar)
}
Tstar25=quantile(Tstar,0.025)
Tstar975=quantile(Tstar,0.975)
Tmedian = median(light1879_vec)
c(2*Tmedian-Tstar975,2*Tmedian-Tstar25)

# Dataset 1882
#### MEAN 
B=1000
Tstar=numeric(B)
for(i in 1:B) {
  Xstar=sample(light1882_vec, replace=TRUE)
  Tstar[i]=mean(Xstar)
}
Tstar25=quantile(Tstar,0.025)
Tstar975=quantile(Tstar,0.975)
Tmean= mean(light1882_vec)
c(2*Tmean-Tstar975,2*Tmean-Tstar25)

#### Median
Tstar=numeric(B)
for(i in 1:B) {
  Xstar=sample(light1882_vec, replace=TRUE)
  Tstar[i]=median(Xstar)
}
Tstar25=quantile(Tstar,0.025)
Tstar975=quantile(Tstar,0.975)
Tmedian = median(light1882_vec)
c(2*Tmedian-Tstar975,2*Tmedian-Tstar25)

# Dataset light.txt
#### MEAN 
B=1000
Tstar=numeric(B)
for(i in 1:B) {
  Xstar=sample(light_vec, replace=TRUE)
  Tstar[i]=mean(Xstar)
}
Tstar25=quantile(Tstar,0.025)
Tstar975=quantile(Tstar,0.975)
Tmean= mean(light_vec)
c(2*Tmean-Tstar975,2*Tmean-Tstar25)

#### Median
Tstar=numeric(B)
for(i in 1:B) {
  Xstar=sample(light_vec, replace=TRUE)
  Tstar[i]=median(Xstar)
}
Tstar25=quantile(Tstar,0.025)
Tstar975=quantile(Tstar,0.975)
Tmedian = median(light_vec)
c(2*Tmedian-Tstar975,2*Tmedian-Tstar25)

# The dataset light.txt follows the normal distribution so mean and median are equal.
# The confidence interval for μ with 95% confidence is measured as below
sd_light=sd(light_vec)
len_sample=length(light_vec)
c(Tmean-2*sd_light/sqrt(len_sample), Tmean+2*sd_light/sqrt(len_sample))

# Question 2.3
# The confidence interval of 
#
#

# Question 2.4
# The p-value of t-test is greater than 0.05 and therefore we can not reject the null hypothesis that
# the value of speed measured by Michelson and Newcomb is equal to the currently most accurate value, 299792.458 km/s
t.test(light_vec, mu= 299792.458)

### Exercise 3
# Question 3.1
klm=scan("klm.txt")
qqnorm(klm,main="QQ-plot of KLM")
m=sum(klm>31)
n= length(klm)
binom.test(m,n,p=0.5)

# Question 3.2
m=sum(klm>72)
n= length(klm)
binom.test(m,n,p=0.1)

### Exercise 4
# Question 4.1
par(mfrow=c(1,4))
cloud_data = read.table("clouds.txt", header=TRUE)

summary(cloud_data$seeded)
sd(cloud_data$seeded)
hist(cloud_data$seeded)
qqnorm(cloud_data$seeded)
qqline(cloud_data$seeded)

summary(cloud_data$unseeded)
sd(cloud_data$unseeded)
hist(cloud_data$unseeded)
qqnorm(cloud_data$unseeded)
qqline(cloud_data$unseeded)

# Comment: Given the histograms and qq-plots of seed and unseed cloud data, we can't assume that they are in a normal distribution
# As a result, we can't apply the two samples t-test in this case because the assumption of the normal distribution in t-test was violated
# The Mann- Whitney test and the Kolmogorov-Smirnov test can be adopted in this case for the reason that both don't assume
# observations are from normal distribution 

t.test(cloud_data$seeded, cloud_data$unseeded)
#  Wilcoxon signed rank test: p-value = 0.01383 < 0.05, we can conclude that Ho of equal means is rejected
wilcox.test(cloud_data$seeded, cloud_data$unseeded)
# Kolmogorov-Smirnov test: p-value = 0.01905 < 0.05, we can conclude that Ho of equal means is rejected
ks.test(cloud_data$seeded, cloud_data$unseeded)

# Question 4.2
par(mfrow=c(1,4))
square_root_data = sqrt(cloud_data)

summary(square_root_data$seeded)
sd(square_root_data$seeded)
hist(square_root_data$seeded)
qqnorm(square_root_data$seeded)
qqline(square_root_data$seeded)

summary(square_root_data$unseeded)
sd(square_root_data$unseeded)
hist(square_root_data$unseeded)
qqnorm(square_root_data$unseeded)
qqline(square_root_data$unseeded)

# Comment: Similarly, we can't assume that the square root of seed and unseed cloud data are in a normal distribution


# The assumption of the normal distribution in the two samples t-test was violated so we shouldn't apply t-test to the data.
# The Mann- Whitney test and the Kolmogorov-Smirnov test can be adopted in this case for the reason that both don't assume
# observations are from normal distribution 
t.test(square_root_data$seeded, square_root_data$unseeded)
#  Wilcoxon signed rank test: p-value = 0.01383 < 0.05, we can conclude that Ho of equal means is rejected
wilcox.test(square_root_data$seeded, square_root_data$unseeded)
#  Kolmogorov-Smirnov test: p-value = 0.01905 < 0.05, we can conclude that Ho of equal means is rejected
ks.test(square_root_data$seeded, square_root_data$unseeded)

# Question 4.3
par(mfrow=c(1,4))

# Comment: After transformed by square root of the square root of the values, seed clouds data don't still 
# follow the normal probability distribution, which can be observed from the histogram and qq-lot
square_root_of_square_root_data = sqrt(square_root_data)
summary(square_root_of_square_root_data$seeded)
sd(square_root_of_square_root_data$seeded)
hist(square_root_of_square_root_data$seeded)
qqnorm(square_root_of_square_root_data$seeded)
qqline(square_root_of_square_root_data$seeded)

# Comment: After transformed by square root of the square root of the values, unseed clouds data can be considered to
# follow the normal probability distribution, which can be observed from the histogram and qq-lot
summary(square_root_of_square_root_data$unseeded)
sd(square_root_of_square_root_data$unseeded)
hist(square_root_of_square_root_data$unseeded)
qqnorm(square_root_of_square_root_data$unseeded)
qqline(square_root_of_square_root_data$unseeded)


# Square root of the square root of the values in seed clouds doesn't follow the normal distribution and we can't use the two samples t-test
# in this situtation.
# The Mann- Whitney test and the Kolmogorov-Smirnov test can be apply again in this case due to the fact that both don't assume
# observations are from normal distribution 

t.test(square_root_of_square_root_data$seeded, square_root_of_square_root_data$unseeded)

#  Wilcoxon signed rank test: p-value = 0.01383 < 0.05, we can conclude that Ho of equal means is rejected
wilcox.test(square_root_of_square_root_data$seeded, square_root_of_square_root_data$unseeded)
#  Kolmogorov-Smirnov test: p-value = 0.01905 < 0.05, we can conclude that Ho of equal means is rejected
ks.test(square_root_of_square_root_data$seeded, square_root_of_square_root_data$unseeded)

###Exercise 5

# Question 5.1
peruvians=read.table("peruvians.txt",header=TRUE)
peruvians = peruvians[,-c(5,6,7)]
pairs(peruvians, upper.panel=NULL)
# Answer: Based on the pairs is possible to see a potential correlation in the following pairs:
# migration x age, migration x weight, migration x wrist
# This conclusion is based on the fact that the plot from these pairs resembles a straight line passing through the
# orign.

# Question 5.2
# Tests will be conducted using Spearma's rank correlation test which doesn't assume normality between the two
# variables.
attach(peruvians) # Keeping the dataset in memory so r functions will access information based on column's
                              # names
# Test 5.2.1 (migration x age)
peruvians[,c(1,2)]
cor.test(migration, age,method="spearman")
# Answer: As can be seen from the test, p-value = 0.0021, which leds us to reject the null hypothesis that rho is equal
# to 0. In fact, the calculated rho based on the samples is 0.4760.

# Test 5.2.2 (migration x weight)
cor.test(migration, weight,method="spearman")
# Answer: As can be seen from the test, p-value = 0.02861, which leds us to reject the null hypothesis that rho is equal
# to 0. In fact, the calculated rho based on the samples is 0.3506.

# Test 5.2.3 (migration x length)
cor.test(migration, length,method="spearman")
# Answer: As can be seen from the test, p-value = 0.6087, which leds us to fail to reject the null hypothesis that rho is equal
# to 0 - considering a o.05 confidence level (despite the fact that R's output states that H0 can be rejected).
# The calculated rho based on the samples is 0.0845 which is very close to 0 -> So it is indeed possible to conclude 
# that these variables are not correlated to each other.

# Test 5.2.4 (migration x wrist)
cor.test(migration, wrist,method="spearman")
# Answer: As can be seen from the test, p-value = 0.1797, which leds us to fail to reject the null hypothesis that rho is equal
# to 0 - considering a o.05 confidence level (despite the fact that R's output states that H0 can be rejected).
# However, the calculated rho for the sample is different from 0 (in fact rho = 0.2193).

# Test 5.2.4 (migration x wrist)
cor.test(migration, wrist,method="spearman")
# Answer: As can be seen from the test, p-value = 0.1797, which leds us to fail to reject the null hypothesis that rho is equal
# to 0 - considering a o.05 confidence level (despite the fact that R's output states that H0 can be rejected).
# However, the calculated rho for the sample is different from 0 (in fact rho = 0.2193).

# Test 5.2.5 (migration x diastolic)
cor.test(migration, diastolic,method="spearman")
# Answer: As can be seen from the test, p-value = 0.6494, which leds us to fail to reject the null hypothesis that rho is equal
# to 0 - considering a o.05 confidence level (despite the fact that R's output states that H0 can be rejected).
# However, the calculated rho for the sample is different from 0 (in fact rho = 0.0751).

### Exercise 6
# Question 6.1
run=read.table("run.txt")
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

# Question 6.2
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

# Question 6.3
n=nrow(run)
diff_time <- data.frame(numeric(n), character(n))
diff_time[,1]= run[,2]- run[,1]
diff_time[,2]= run[,3]
wilcox.test(diff_time[1:12,1],diff_time[13:24,1])
#diff_time

# Question 6.4 
# In Doc File

# Question 6.5 
# In Doc File

# Question 6.6
par(mfrow=c(1,2))
qqnorm(diff_time[1:12,1],main = 'QQ-Plot of Residual lemo')
qqnorm(diff_time[13:24,1],main = 'QQ-Plot of Residual soft')
t.test(diff_time[1:12,1],diff_time[13:24,1],paired = TRUE)

### Exercise 7

# Question 7.1
dogs=read.table("dogs.txt",header = TRUE)
dogs
par(mfrow=c(1,3))
boxplot(dogs[,1], main = "V1: isofluorane")
boxplot(dogs[,2], main = "V2: halothane")
boxplot(dogs[,3], main = "V3: cyclopropane")

qqnorm(dogs[,1], main = "QQ-plot V1: isofluorane")
qqnorm(dogs[,2], main = "QQ-plot V2: halothane")
qqnorm(dogs[,3], main = "QQ-plot V3: cyclopropane")

# Question 7.2
dogsframe= data.frame(plasma=as.vector(as.matrix(dogs)), drugs=factor(rep(1:3,each=10)))
dogsaov= lm(plasma~drugs,data = dogsframe)
anova(dogsaov)
summary(dogsaov)

drug1 = 0.4340
drug2 = drug1 - 0.0350
drug3 = drug1 - 0.4190
drug1; drug2; drug3

# Question 7.3
attach(dogsframe)
kruskal.test(plasma,drugs)
