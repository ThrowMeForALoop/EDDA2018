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