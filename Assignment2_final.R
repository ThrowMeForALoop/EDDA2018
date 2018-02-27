### Exercise 1

# Question 1.1
data <- read.table(file = "telephone.txt", header = TRUE)
data = unlist(data, use.names = FALSE) 

# The statistic to be used is the median - t = median for the given population's sample
t=median(data) 

numLambdas=50
B=1000; 
tstar=numeric(B)
n=length(data)

#Initially the interval for lambda will be split into equidistant values for lambda. Afterwards, the p-value
#will be calculated for each of them.

#Spliting the lambda interval in 50 different points:
lambdas=numeric(numLambdas)
pvalues=numeric(numLambdas)

lambdas=seq(0.01,0.1,length=50)

for (j in 1:numLambdas) {
  # Creating Xstars and surrogate Tstars
  tstar=numeric(B) #Creating a Tstar vector to be used in the Bootstrap test
  for (i in 1:B){
    xstar=rexp(n,lambdas[j]) # generating simulated samples
    tstar[i]=median(xstar) #generating surrogated ts
  }

  pl=sum(tstar<t)/B
  pr=sum(tstar>t)/B
  pvalues[j]=2*min(pl,pr)
}

par(mfrow=c(1,2))
plot(lambdas,pvalues, main="Lambda x p-value")
qqplot(data, rexp(n,0.02653), main="QQPlot - Data x Exp(0.02653)")

# Question 1.2:
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

par(mfrow=c(1,2))
hist(data, prob=T)
boxplot(data)

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
par(mfrow=c(1,3))

hist(light1879_vec, xlab = "Speed of light 1879")
hist(light1882_vec, xlab = "Speed of light 1882")
hist(light_vec, xlab = "Speed of Light")

par(mfrow=c(1,3))

boxplot(light1879_vec, main = "Boxplot of light1879 dataset")
boxplot(light1882_vec, main = "Boxplot of light1882 dataset")
boxplot(light_vec, main = "Boxplot of light dataset")

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

# Question 2.4
# 1879 dataset is in normal distribution
accurate_light_velocity =  299792.458
t.test(light1879_vec, mu=accurate_light_velocity, conf.level=0.95)

# 1882 and light.txt is not in normal distribution
wilcox.test(light1882_vec, mu= accurate_light_velocity)
wilcox.test(light_vec, mu= accurate_light_velocity)
?wilcox.test()

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
cloud_data = read.table("clouds.txt", header=TRUE)

par(mfrow=c(1,3))
hist(cloud_data$seeded, main= "Histogram of seeded cloud", xlab = "precipitation")
boxplot(cloud_data$seeded, main= "Boxplot of seeded cloud")
qqnorm(cloud_data$seeded, main = "QQPlot of seeded cloud")

par(mfrow=c(1,3))
hist(cloud_data$unseeded,  main= "Histogram of unseeded cloud", xlab = "precipitation")
boxplot(cloud_data$unseeded, main= "Boxplot of unseeded cloud")
qqnorm(cloud_data$unseeded, main= "QQPlot of unseeded cloud")

t.test(cloud_data$seeded, cloud_data$unseeded)
wilcox.test(cloud_data$seeded, cloud_data$unseeded)
ks.test(cloud_data$seeded, cloud_data$unseeded)

# Question 4.2
square_root_data = sqrt(cloud_data)

par(mfrow=c(1,3))
hist(square_root_data$seeded, main= "Histogram of square of seeded cloud", xlab = "precipitation")
boxplot(square_root_data$seeded, main= "Boxplot of square of seeded cloud")
qqnorm(square_root_data$seeded, main= "QQPlot of square of seeded cloud")

par(mfrow=c(1,3))
hist(square_root_data$unseeded, main= "Histogram of square of unseeded cloud", xlab = "precipitation")
boxplot(square_root_data$unseeded, main= "Boxplot of square of unseeded cloud")
qqnorm(square_root_data$unseeded, main= "QQPlot of square of unseeded cloud")

t.test(square_root_data$seeded, square_root_data$unseeded)
wilcox.test(square_root_data$seeded, square_root_data$unseeded)
ks.test(square_root_data$seeded, square_root_data$unseeded)

# Question 4.3
square_root_of_square_root_data = sqrt(square_root_data)

par(mfrow=c(1,3))
hist(square_root_of_square_root_data$seeded, main= "Histogram- square of square seeded cloud", xlab = "precipitation")
boxplot(square_root_of_square_root_data$seeded, main= "Boxplot- square of square seeded cloud")
qqnorm(square_root_of_square_root_data$seeded, main= "QQPlot- square of square seeded cloud")



hist(square_root_of_square_root_data$unseeded, main= "Histogram- square of square unseeded cloud", xlab = "precipitation")
boxplot(square_root_of_square_root_data$unseeded, main= "Boxplot- square of square unseeded cloud")
qqnorm(square_root_of_square_root_data$unseeded, main= "QQPlot- square of square unseeded cloud", xlab = "precipitation")

t.test(square_root_of_square_root_data$seeded, square_root_of_square_root_data$unseeded)
wilcox.test(square_root_of_square_root_data$seeded, square_root_of_square_root_data$unseeded)
ks.test(square_root_of_square_root_data$seeded, square_root_of_square_root_data$unseeded)

###Exercise 5

# Question 5.1
peruvians=read.table("peruvians.txt",header=TRUE)
peruvians = peruvians[,-c(5,6,7)]
pairs(peruvians, upper.panel=NULL)

# Question 5.2
attach(peruvians) 

# Test 5.2.1 (migration x age)
peruvians[,c(1,2)]
cor.test(migration, age,method="spearman")

# Test 5.2.2 (migration x weight)
cor.test(migration, weight,method="spearman")

# Test 5.2.3 (migration x length)
cor.test(migration, length,method="spearman")

# Test 5.2.4 (migration x wrist)
cor.test(migration, wrist,method="spearman")

# Test 5.2.5 (migration x diastolic)
cor.test(migration, diastolic,method="spearman")

### Exercise 6

# Question 6.1
run=read.table("run.txt")
soft_drink=run[run$drink=='lemo', 1:2]
energy=run[run$drink=='energy', 1:2]

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


#energy drink
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


# Question 6.6
par(mfrow=c(1,2))
as.matrix(diff_time)
diff_frame= data.frame(time_diff=as.vector(diff_time[,1]), drink_type=factor(rep(1:2,each=12)))
diff_time_aov= lm(time_diff~drink_type,data = diff_frame)
time_residual=residuals(diff_time_aov)

qqnorm(time_residual[1:12],main = 'QQ-Plot of Residual lemo')
qqnorm(time_residual[13:24],main = 'QQ-Plot of Residual soft')
#t.test(diff_time[1:12,1],diff_time[13:24,1],paired = TRUE)


### Exercise 7

# Question 7.1
dogs=read.table("dogs.txt",header = TRUE)
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
drug2 = drug1 + 0.0350
drug3 = drug1 + 0.4190
drug1; drug2; drug3

# Question 7.3
attach(dogsframe)
kruskal.test(plasma,drugs)

