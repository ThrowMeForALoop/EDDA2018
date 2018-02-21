# Ex 2
light1879_dataframe=read.table("light1879.txt", header = FALSE)
light1882_dataframe=read.table("light1882.txt", fill = TRUE)
light_dataframe= read.table("light.txt", header = FALSE)

light1879_vec = unlist(light1879_dataframe, use.names = FALSE)
light1879_vec = light1879_vec + 299000

light1882_vec = unlist(light1882_dataframe, use.names = FALSE)
light1882_vec <- light1882_vec[!is.na(light1882_vec)]
light1882_vec = light1882_vec + 299000

light_vec = unlist(light_dataframe, use.names = FALSE)
light_vec = 7442 / (((light_vec/1000) + 24.8)/1000000)

#Question 2.1
par(mfrow=c(1,2))
hist(light1879_vec, xlab = "Light velocity 1879")
boxplot(light1879_vec)
qqnorm(light1879_vec)
qqline(light1879_vec)

hist(light1882_vec, xlab = "Light velocity 1882")
boxplot(light1882_vec)
qqnorm(light1882_vec)
qqline(light1882_vec)

hist(light_vec, xlab = "Light velocity")
boxplot(light_vec)
qqnorm(light_vec)
qqline(light_vec)

qnorm(0.025)
qnorm(0.975)
mean(light1879_vec)

#Question 2.2
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

# Compare median result with wilcoxon (only for median)
wilcox.test(light1879_vec, conf.int = TRUE, conf.level = 0.95)

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

# Compare median result with wilcoxon (only for median)
wilcox.test(light1882_vec, conf.int = TRUE, conf.level = 0.95)

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

# Compare median result with wilcoxon (only for median)
wilcox.test(light_vec, conf.int = TRUE, conf.level = 0.95)

# Question 4.4
t.test(light_vec, mu= 299792458)

### Ex 4
# Question 4.1
par(mfrow=c(1,4))
cloud_data = read.table("clouds.txt", header=TRUE)

summary(cloud_data$seeded)
sd(cloud_data$seeded)
vector1 = cloud_data$seeded
hist(cloud_data$seeded)
qqnorm(cloud_data$seeded)

summary(cloud_data$unseeded)
sd(cloud_data$unseeded)
vector2 = cloud_data$unseeded
hist(cloud_data$unseeded)
qqnorm(cloud_data$unseeded)

t.test(cloud_data$seeded, cloud_data$unseeded)
?wilcox.test
wilcox.test(vector1, vector2)
ks.test(cloud_data$seeded, cloud_data$unseeded)

# Question 4.2
par(mfrow=c(1,4))
square_root_data = sqrt(cloud_data)

summary(square_root_data$seeded)
sd(square_root_data$seeded)
hist(square_root_data$seeded)
qqnorm(square_root_data$seeded)

summary(square_root_data$unseeded)
sd(square_root_data$unseeded)
hist(square_root_data$unseeded)
qqnorm(square_root_data$unseeded)

t.test(square_root_data$seeded, square_root_data$unseeded)
?wilcox.test
wilcox.test(square_root_data$seeded, square_root_data$unseeded)
ks.test(square_root_data$seeded, square_root_data$unseeded)

# Question 4.3
par(mfrow=c(1,4))
square_root_of_square_root_data = sqrt(square_root_data)
summary(square_root_of_square_root_data$seeded)
sd(square_root_of_square_root_data$seeded)
hist(square_root_of_square_root_data$seeded)
qqnorm(square_root_of_square_root_data$seeded)

summary(square_root_of_square_root_data$unseeded)
sd(square_root_of_square_root_data$unseeded)
hist(square_root_of_square_root_data$unseeded)
qqnorm(square_root_of_square_root_data$unseeded)

t.test(square_root_of_square_root_data$seeded, square_root_of_square_root_data$unseeded)
?wilcox.test
wilcox.test(square_root_of_square_root_data$seeded, square_root_of_square_root_data$unseeded)
ks.test(square_root_of_square_root_data$seeded, square_root_of_square_root_data$unseeded)

#Exercise 5

#Item 1
peruvians=read.table("peruvians.txt",header=TRUE)
peruvians = peruvians[,-c(5,6,7)]
pairs(peruvians, upper.panel=NULL)
# Answer: Based on the pairs is possible to see a potential correlation in the following pairs:
# migration x age, migration x weight, migration x wrist
# This conclusion is based on the fact that the plot from these pairs resembles a straight line passing through the
# orign.

#Item 2
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