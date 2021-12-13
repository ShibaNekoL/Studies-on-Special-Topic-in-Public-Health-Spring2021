# import data
ppi <- read.csv(file.choose())


# boxplot
boxplot(ppi$RSI_drop, ppi$RSI_1_drop , ppi$RSI_2_drop , ppi$RSI_3_drop , ppi$RSI_4_drop , ppi$RSI_5_drop , ppi$RSI_6_drop, ppi$RSI_7_drop , ppi$RSI_8_drop , ppi$RSI_9_drop, main="RSI pretest-posttest",
        names=c("RSI_sum", "RSI_1", "RSI_2", "RSI_3", "RSI_4", "RSI_5", "RSI_6", "RSI_7", "RSI_8", "RSI_9"))

boxplot(ppi$RFS_drop_rater1, ppi$RFS_drop_rater2, main="RFS pretest-posttest",
        names = c("rater 1", "rater 2"))

boxplot(ppi$RFS_SE_drop_rater1,  ppi$RFS_V_drop_rater1,  ppi$RFS_E.H_drop_rater1,  ppi$RFS_VFE_drop_rater1,  ppi$RFS_DLE_drop_rater1,  ppi$RFS_PCH_drop_rater1,  ppi$RFS_G.GT_drop_rater1,  ppi$RFS_TEM_drop_rater1, main="RFS rater 1 pretest-posttest",
        names=c("RFS_SE","RFS_V","RFS_E.H","RFS_VFE" ,"RFS_DLE" ,"RFS_PCH","RFS_G.GT", "RFS_TEM"))
boxplot(ppi$RFS_SE_drop_rater2,  ppi$RFS_V_drop_rater2,  ppi$RFS_E.H_drop_rater2,  ppi$RFS_VFE_drop_rater2,  ppi$RFS_DLE_drop_rater2,  ppi$RFS_PCH_drop_rater2,  ppi$RFS_G.GT_drop_rater2,  ppi$RFS_TEM_drop_rater2, main="RFS rater 2 pretest-posttest",
        names=c("RFS_SE","RFS_V","RFS_E.H","RFS_VFE" ,"RFS_DLE" ,"RFS_PCH","RFS_G.GT", "RFS_TEM"))

# 描述性統計
ppi_n <- subset(ppi, ppi$RSI_sum_pre < 13)
mean(ppi_n$RSI_sum_pre)
sd(ppi_n$RSI_sum_pre)
ppi_n <- subset(ppi, ppi$RSI_sum_pre >= 13)
mean(ppi_n$RSI_sum_pre)
sd(ppi_n$RSI_sum_pre)

ppi_n1 <- subset(ppi, ppi$RSI_sum_post < 13)
mean(ppi_n1$RSI_sum_post)
sd(ppi_n1$RSI_sum_post)
ppi_n1 <- subset(ppi_n1, ppi$RSI_sum_post >= 13)
mean(ppi_n1$RSI_sum_post)
sd(ppi_n1$RSI_sum_post)


## RFS
# 74 60 41 16
ppi_rfs <- ppi_rfs[-16,]


ppi_n <- subset(ppi_rfs, ppi_rfs$RFS_sum_pre_rater1 < 7)
mean(ppi_n$RFS_sum_pre_rater1)
sd(ppi_n$RFS_sum_pre_rater1)
ppi_n <- subset(ppi_rfs, ppi_rfs$RFS_sum_pre_rater1 >= 7)
mean(ppi_n$RFS_sum_pre_rater1)
sd(ppi_n$RFS_sum_pre_rater1)

ppi_n <- subset(ppi_rfs, ppi_rfs$RFS_sum_post_rater1 < 7)
mean(ppi_n$RFS_sum_post_rater1)
sd(ppi_n$RFS_sum_post_rater1)
ppi_n <- subset(ppi_rfs, ppi_rfs$RFS_sum_post_rater1 >= 7)
mean(ppi_n$RFS_sum_post_rater1)
sd(ppi_n$RFS_sum_post_rater1)

ppi_n <- subset(ppi_rfs, ppi_rfs$RFS_sum_pre_rater2 < 7)
mean(ppi_n$RFS_sum_pre_rater2)
sd(ppi_n$RFS_sum_pre_rater2)
ppi_n <- subset(ppi_rfs, ppi_rfs$RFS_sum_pre_rater2 >= 7)
mean(ppi_n$RFS_sum_pre_rater2)
sd(ppi_n$RFS_sum_pre_rater2)

ppi_n <- subset(ppi_rfs, ppi_rfs$RFS_sum_post_rater2 < 7)
mean(ppi_n$RFS_sum_post_rater2)
sd(ppi_n$RFS_sum_post_rater2)
ppi_n <- subset(ppi_rfs, ppi_rfs$RFS_sum_post_rater2 >= 7)
mean(ppi_n$RFS_sum_post_rater2)
sd(ppi_n$RFS_sum_post_rater2)

mean(ppi_rfs$RFS_sum_pre_rater2)


## t-test RFS pre-post
t.test(ppi$RFS_TEM_pre_rater2,ppi$RFS_TEM_post_rater2, paired=T)
t.test(ppi$RSI_9_pre,ppi$RSI_9_post, paired=T)
t.test(ppi_rfs$RFS_sum_pre_rater2,ppi_rfs$RFS_sum_post_rater2, paired=T)


## t-test RFS rater1-2
t.test(ppi$RFS_drop_rater1,ppi$RFS_drop_rater2, paired=T)
t.test(ppi$RFS_TEM_drop_rater1,ppi$RFS_TEM_drop_rater2, paired=T)

## paired t-test RFS-RSI


t.test(ppi$RFS_drop_rater1,ppi$RSI_drop, paired=T)


t.test(ppi$RFS_TEM_drop_rater1,ppi$RFS_TEM_drop_rater2, paired=T)


# 散布圖
par(mfcol=c(2,3))

x <- ppi$RFS_drop_rater1
y <- ppi$RSI_drop
plot(x, y, main="醫師1 RFS與RSI之總分前後測差異散布圖", xlab="RFS_change", ylab="RSI_change")
abline(lm(y~x),col="red")
r <- cor.test(x,y)[[4]]
legend("topleft", legend = c(paste0("r = ", round(r, 2))))

x <- ppi$RFS_drop_rater2
y <- ppi$RSI_drop
plot(x, y, main="醫師2 RFS與RSI之總分前後測差異散布圖", xlab="RFS_change", ylab="RSI_change")
abline(lm(y~x),col="red")
r <- cor.test(x,y)[[4]]
legend("topleft", legend = c(paste0("r = ", round(r, 2))))

x <- ppi$RFS_sum_pre_rater1
y <- ppi$RSI_sum_pre
plot(x, y, main="醫師1 RFS與RSI前測總分散布圖", xlab="RFS_pre", ylab="RSI_pre")
abline(lm(y~x),col="red")
# abline(h=13, v=7)
cor.test(x,y)
legend("topleft", legend = c("r = 0.41"))
r <- cor.test(x,y)[[4]]
legend("topleft", legend = c(paste0("r = ", round(r, 2))))
points(x[x>=7 & y>=13], y[x>=7 & y>=13], col="green4", pch=16)
points(x[x>=7 & y<13], y[x>=7 & y<13], col="blue3", pch=16)
points(x[x<7 & y>=13], y[x<7 & y>=13], col="red3", pch=16)

x <- ppi$RFS_sum_pre_rater2
y <- ppi$RSI_sum_pre
plot(x, y, main="醫師2 RFS與RSI前測總分散布圖", xlab="RFS_pre", ylab="RSI_pre")
abline(lm(y~x),col="red")
r <- cor.test(x,y)[[4]]
legend("topleft", legend = c(paste0("r = ", round(r, 2))))
points(x[x>=7 & y>=13], y[x>=7 & y>=13], col="green4", pch=16)
points(x[x>=7 & y<13], y[x>=7 & y<13], col="blue3", pch=16)
points(x[x<7 & y>=13], y[x<7 & y>=13], col="red3", pch=16)


x <- ppi$RFS_sum_post_rater1
y <- ppi$RSI_sum_post
plot(x, y, main="醫師1 RFS與RSI後測總分散布圖", xlab="RFS_post", ylab="RSI_post")
abline(lm(y~x),col="red")
cor.test(x,y)
r <- cor.test(x,y)[[4]]
legend("topleft", legend = c(paste0("r = ", round(r, 2))))
points(x[x>=7 & y>=13], y[x>=7 & y>=13], col="green4", pch=16)
points(x[x>=7 & y<13], y[x>=7 & y<13], col="blue3", pch=16)
points(x[x<7 & y>=13], y[x<7 & y>=13], col="red3", pch=16)


x <- ppi$RFS_sum_post_rater2
y <- ppi$RSI_sum_post
plot(x, y, main="醫師2 RFS與RSI後測總分散布圖", xlab="RFS_post", ylab="RSI_post")
abline(lm(y~x),col="red")
r <- cor.test(x,y)[[4]]
legend("topleft", legend = c(paste0("r = ", round(r, 2))))
points(x[x>=7 & y>=13], y[x>=7 & y>=13], col="green4", pch=16)
points(x[x>=7 & y<13], y[x>=7 & y<13], col="blue3", pch=16)
points(x[x<7 & y>=13], y[x<7 & y>=13], col="red3", pch=16)



# 散布圖
par(mfcol=c(2,3))

x <- ppi$RFS_drop_rater1
y <- ppi$RSI_drop
plot(x, y, main="Doctor1-RFS & RSI change scatterplot", xlab="RFS_change", ylab="RSI_change")
abline(lm(y~x),col="red")
r <- cor.test(x,y)[[4]]
legend("topleft", legend = c(paste0("r = ", round(r, 2))))

x <- ppi$RFS_drop_rater2
y <- ppi$RSI_drop
plot(x, y, main="Doctor2-RFS & RSI change scatterplot", xlab="RFS_change", ylab="RSI_change")
abline(lm(y~x),col="red")
r <- cor.test(x,y)[[4]]
legend("topleft", legend = c(paste0("r = ", round(r, 2))))

x <- ppi$RFS_sum_pre_rater1
y <- ppi$RSI_sum_pre
plot(x, y, main="Doctor1-RFS & RSI pretest scatterplot", xlab="RFS_pre", ylab="RSI_pre")
abline(lm(y~x),col="red")
# abline(h=13, v=7)
cor.test(x,y)
legend("topleft", legend = c("r = 0.41"))
r <- cor.test(x,y)[[4]]
legend("topleft", legend = c(paste0("r = ", round(r, 2))))
points(x[x>=7 & y>=13], y[x>=7 & y>=13], col="green4", pch=16)
points(x[x>=7 & y<13], y[x>=7 & y<13], col="blue3", pch=16)
points(x[x<7 & y>=13], y[x<7 & y>=13], col="red3", pch=16)

x <- ppi$RFS_sum_pre_rater2
y <- ppi$RSI_sum_pre
plot(x, y, main="Doctor2-RFS & RSI pretest scatterplot", xlab="RFS_pre", ylab="RSI_pre")
abline(lm(y~x),col="red")
r <- cor.test(x,y)[[4]]
legend("topleft", legend = c(paste0("r = ", round(r, 2))))
points(x[x>=7 & y>=13], y[x>=7 & y>=13], col="green4", pch=16)
points(x[x>=7 & y<13], y[x>=7 & y<13], col="blue3", pch=16)
points(x[x<7 & y>=13], y[x<7 & y>=13], col="red3", pch=16)


x <- ppi$RFS_sum_post_rater1
y <- ppi$RSI_sum_post
plot(x, y, main="Doctor1-RFS & RSI posttest scatterplot", xlab="RFS_post", ylab="RSI_post")
abline(lm(y~x),col="red")
cor.test(x,y)
r <- cor.test(x,y)[[4]]
legend("topleft", legend = c(paste0("r = ", round(r, 2))))
points(x[x>=7 & y>=13], y[x>=7 & y>=13], col="green4", pch=16)
points(x[x>=7 & y<13], y[x>=7 & y<13], col="blue3", pch=16)
points(x[x<7 & y>=13], y[x<7 & y>=13], col="red3", pch=16)


x <- ppi$RFS_sum_post_rater2
y <- ppi$RSI_sum_post
plot(x, y, main="Doctor2-RFS & RSI posttest scatterplot", xlab="RFS_post", ylab="RSI_post")
abline(lm(y~x),col="red")
r <- cor.test(x,y)[[4]]
legend("topleft", legend = c(paste0("r = ", round(r, 2))))
points(x[x>=7 & y>=13], y[x>=7 & y>=13], col="green4", pch=16)
points(x[x>=7 & y<13], y[x>=7 & y<13], col="blue3", pch=16)
points(x[x<7 & y>=13], y[x<7 & y>=13], col="red3", pch=16)




# One-Way Repeated Measurement ANOVA

install.packages("car")
library(car)

multmodel <- lm(cbind(ppi$RSI_sum_pre,ppi$RSI_sum_post) ~ 1) # make a multivariate linear model with only the intercept as a predictor for your within-participants observations
Trials <- factor(c("RSI_sum_pre","RSI_sum_post"), ordered=F) # create a factor for your repeatedmeasures variable.
model1 <- Anova(multmodel,idata=data.frame(Trials),idesign=~Trials,type="III") # use the repeated-measures factor as the ‘internal’ part of the design using ‘Anova’ (with a capital A)
summary(model1, multivariate=F)



multmodel <- lm(cbind(ppi$RFS_sum_pre_rater1,ppi$RFS_sum_post_rater1) ~ 1) # make a multivariate linear model with only the intercept as a predictor for your within-participants observations
Trials <- factor(c("RFS_sum_pre_rater1","RFS_sum_post_rater1"), ordered=F) # create a factor for your repeatedmeasures variable.
model1 <- Anova(multmodel,idata=data.frame(Trials),idesign=~Trials,type="III") # use the repeated-measures factor as the ‘internal’ part of the design using ‘Anova’ (with a capital A)
summary(model1, multivariate=F)


multmodel <- lm(cbind(ppi$RFS_sum_pre_rater2,ppi$RFS_sum_post_rater2) ~ 1) # make a multivariate linear model with only the intercept as a predictor for your within-participants observations
Trials <- factor(c("RFS_sum_pre_rater2","RFS_sum_post_rater2"), ordered=F) # create a factor for your repeatedmeasures variable.
model1 <- Anova(multmodel,idata=data.frame(Trials),idesign=~Trials,type="III") # use the repeated-measures factor as the ‘internal’ part of the design using ‘Anova’ (with a capital A)
summary(model1, multivariate=F)

# paired t test

RSI_t.test <- t.test(ppi$RSI_sum_pre,ppi$RSI_sum_post,paired=T)
RSI_t.test

RFS_1_t.test <- t.test(ppi$RFS_sum_pre_rater1,ppi$RFS_sum_post_rater1,paired=T)
RFS_1_t.test

RFS_2_t.test <- t.test(ppi$RFS_sum_pre_rater2,ppi$RFS_sum_post_rater2,paired=T)
RFS_2_t.test

RFS_pre_t.test <- t.test(ppi$RFS_sum_pre_rater1,ppi$RFS_sum_pre_rater2,paired=T)
RFS_pre_t.test

RFS_post_t.test <- t.test(ppi$RFS_sum_post_rater1,ppi$RFS_sum_post_rater2,paired=T)
RFS_post_t.test

# 右尾 paired t test 前-後
RSI_t.test_rightsided <- t.test(ppi$RSI_sum_pre,ppi$RSI_sum_post,paired=T, alt="greater")
RSI_t.test_rightsided

RFS_1_t.test_rightsided <- t.test(ppi$RFS_sum_pre_rater1,ppi$RFS_sum_post_rater1,paired=T, alt="greater")
RFS_1_t.test_rightsided

RFS_2_t.test_rightsided <- t.test(ppi$RFS_sum_pre_rater2,ppi$RFS_sum_post_rater2,paired=T, alt="greater")
RFS_2_t.test_rightsided


# 散布圖
par(mfrow=c(1,2))
x <- ppi$RFS_sum_pre_rater1
y <- ppi$RFS_sum_pre_rater2
plot(x, y, main="RFS醫師1與醫師2評分前測散布圖", xlab="醫師1評分", ylab="醫師2評分")
cor.test(x,y)
abline(lm(y~x),col="red")
abline(0,1,col="black")
legend("topleft", legend = c("r = 0.41"))
points(x[x>=7 & y>=7], y[x>=7 & y>=7], col="green4", pch=16)
points(x[x<7 & y<7], y[x<7 & y<7], col="blue3", pch=16)

x <- ppi$RFS_sum_post_rater1
y <- ppi$RFS_sum_post_rater2
plot(x, y, main="RFS醫師1與醫師2評分後測散布圖", xlab="醫師1評分", ylab="醫師2評分")
cor.test(x,y)
abline(lm(y~x),col="red")
abline(0,1,col="black")
legend("topleft", legend = c("r = 0.37"))
points(x[x>=7 & y>=7], y[x>=7 & y>=7], col="green4", pch=16)
points(x[x<7 & y<7], y[x<7 & y<7], col="blue3", pch=16)


# 散布圖
par(mfrow=c(1,2))
x <- ppi$RFS_sum_pre_rater1
y <- ppi$RFS_sum_pre_rater2
plot(x, y, main="RFS pretest scatterplot", xlab="Doctor 1", ylab="Doctor 2")
cor.test(x,y)
abline(lm(y~x),col="red")
abline(0,1,col="black")
legend("topleft", legend = c("r = 0.41"))
points(x[x>=7 & y>=7], y[x>=7 & y>=7], col="green4", pch=16)
points(x[x<7 & y<7], y[x<7 & y<7], col="blue3", pch=16)

x <- ppi$RFS_sum_post_rater1
y <- ppi$RFS_sum_post_rater2
plot(x, y, main="RFS posttest scatterplot", xlab="Doctor 1", ylab="Doctor 2")
cor.test(x,y)
abline(lm(y~x),col="red")
abline(0,1,col="black")
legend("topleft", legend = c("r = 0.37"))
points(x[x>=7 & y>=7], y[x>=7 & y>=7], col="green4", pch=16)
points(x[x<7 & y<7], y[x<7 & y<7], col="blue3", pch=16)



# bland altman plot

install.packages("devtools")
install.packages("blandr")
devtools::install_github("deepankardatta/blandr")
library(blandr)
library(ggplot2)
# pre test
blandr.draw(ppi$RFS_sum_pre_rater1, ppi$RFS_sum_pre_rater2) + 
    labs(title = "Bland-Altman plot：RFS醫師1與醫師2前測評分")
# post test
blandr.draw(ppi$RFS_sum_post_rater1, ppi$RFS_sum_post_rater2) + 
    labs(title = "Bland-Altman plot：RFS醫師1與醫師2後測評分")


# bland altman plot

install.packages("devtools")
install.packages("blandr")
devtools::install_github("deepankardatta/blandr")
library(blandr)
library(ggplot2)
# pre test
blandr.draw(ppi$RFS_sum_pre_rater1, ppi$RFS_sum_pre_rater2) + 
    labs(title = "Bland-Altman plot: RFS pretest")
# post test
blandr.draw(ppi$RFS_sum_post_rater1, ppi$RFS_sum_post_rater2) + 
    labs(title = "Bland-Altman plot: RFS posttest")




# ICC

install.packages("irr")
library(irr)

# n*m matrix or dataframe, n subjects m raters.
ratings <- cbind(ppi$RFS_sum_pre_rater1, ppi$RFS_sum_pre_rater2)

icc(ratings, model = c("o"), 
    type = c("c"), 
    unit = c("s"), r0 = 0, conf.level = 0.95)

# n*m matrix or dataframe, n subjects m raters.
ratings <- cbind(ppi$RFS_sum_post_rater1, ppi$RFS_sum_post_rater2)

icc(ratings, model = c("o"), 
    type = c("c"), 
    unit = c("s"), r0 = 0, conf.level = 0.95)

# n*m matrix or dataframe, n subjects m raters.
ratings <- cbind(ppi$RFS_sum_post_rater1, ppi$RFS_sum_post_rater2)

icc(ratings, model = c("o"), 
    type = c("c"), 
    unit = c("s"), r0 = 0, conf.level = 0.95)



### Y: RSI_drop_rate
ppi_nobmi <- ppi[-71,]


## sex
# blox plot
par(mfrow=c(1,1))
boxplot(RSI_drop_rate ~ sex, main="RSI percentage change boxplot", xlab="性別", ylab="RSI變化百分比", names=c("女", "男"), data=ppi_nobmi)
# blox plot
par(mfrow=c(1,1))
boxplot(RSI_drop_rate ~ sex, main="RSI percentage change boxplot", xlab="Sex", ylab="RSI percentage change", names=c("female", "male"), data=ppi_nobmi)
# t-test
RSI_sex_t.test <- t.test(ppi_nobmi[which(ppi_nobmi$sex==0), 92], ppi_nobmi[which(ppi_nobmi$sex==1), 92])
RSI_sex_t.test

## age
# 相關係數
x <- ppi_nobmi$age
y <- ppi_nobmi$RSI_drop_rate
plot(x, y, xlab="age", ylab="RSI percentage change")
abline(lm(y~x),col="red")
abline(0,1,col="black")
r <- cor.test(x,y)[[4]]
legend("topleft", legend = c(paste0("r = ", round(r, 2))))
## BMI
# 相關係數
x <- ppi$BMI
y <- ppi$RSI_drop_rate
plot(x, y, xlab="BMI", ylab="RSI percentage change")
abline(lm(y~x),col="red")
abline(0,1,col="black")
r <- cor.test(x,y)[[4]]
legend("topleft", legend = c(paste0("r = ", round(r, 2))))


# Regression 

# Y: RSI_drop_rate
# X: sex+age+BMI
model<-lm(RSI_drop_rate~age, data=ppi)
summary(model)


# variables
ppisex <- subset(ppi_nobmi, ppi_nobmi$sex==1)
sd(ppisex$RSI_drop_rate)

sd(ppi$RSI_drop_rate)


x <- ppi$BMI
y <- ppi$age
plot(x, y, xlab="BMI", ylab="RSI變化百分比")
abline(lm(y~x),col="red")
r <- cor.test(x,y)[[4]]

# ggplot

install.packages("ggplot2")
install.packages("gapminder")
library(ggplot2)
library(gapminder)



install.packages("ggbeeswarm")
library(ggbeeswarm)


#####
# geom_violin
ggplot(ppi_rfs) + geom_quasirandom(aes(x="" , y = RFS_SE_drop_rater1))+
    ylim(-2,2) +
    labs(x = "RFS_SE",
         y = "")
ggplot(ppi_rfs) + geom_quasirandom(aes(x="" , y = RFS_V_drop_rater1))+
    ylim(-2,2) +
    labs(x = "RFS_V",
         y = "")
ggplot(ppi_rfs) + geom_quasirandom(aes(x="" , y = RFS_E.H_drop_rater1))+
    ylim(-2,2) +
    labs(x = "RFS_E.H",
         y = "")
ggplot(ppi_rfs) + geom_quasirandom(aes(x="" , y = RFS_VFE_drop_rater1))+
    ylim(-2,2) +
    labs(x = "RFS_VFE",
         y = "")
ggplot(ppi_rfs) + geom_quasirandom(aes(x="" , y = RFS_DLE_drop_rater1))+
    ylim(-2,2) +
    labs(x = "RFS_DLE",
         y = "")
ggplot(ppi_rfs) + geom_quasirandom(aes(x="" , y = RFS_PCH_drop_rater1))+
    ylim(-2,2) +
    labs(x = "RFS_PCH",
         y = "")
ggplot(ppi_rfs) + geom_quasirandom(aes(x="" , y = RFS_G.GT_drop_rater1))+
    ylim(-2,2) +
    labs(x = "RFS_G.GT",
         y = "")
ggplot(ppi_rfs) + geom_quasirandom(aes(x="" , y = RFS_TEM_drop_rater1))+
    ylim(-2,2) +
    labs(x = "RFS_TEM",
         y = "")


##

ggplot(ppi_rfs) + geom_quasirandom(aes(x="" , y = RFS_SE_drop_rater2))+
    ylim(-2,2) +
    labs(x = "RFS_SE",
         y = "")
ggplot(ppi_rfs) + geom_quasirandom(aes(x="" , y = RFS_V_drop_rater2))+
    ylim(-2,2) +
    labs(x = "RFS_V",
         y = "")
ggplot(ppi_rfs) + geom_quasirandom(aes(x="" , y = RFS_E.H_drop_rater2))+
    ylim(-2,2) +
    labs(x = "RFS_E.H",
         y = "")
ggplot(ppi_rfs) + geom_quasirandom(aes(x="" , y = RFS_VFE_drop_rater2))+
    ylim(-2,2) +
    labs(x = "RFS_VFE",
         y = "")
ggplot(ppi_rfs) + geom_quasirandom(aes(x="" , y = RFS_DLE_drop_rater2))+
    ylim(-2,2) +
    labs(x = "RFS_DLE",
         y = "")
ggplot(ppi_rfs) + geom_quasirandom(aes(x="" , y = RFS_PCH_drop_rater2))+
    ylim(-2,2) +
    labs(x = "RFS_PCH",
         y = "")
ggplot(ppi_rfs) + geom_quasirandom(aes(x="" , y = RFS_G.GT_drop_rater2))+
    ylim(-2,2) +
    labs(x = "RFS_G.GT",
         y = "")
ggplot(ppi_rfs) + geom_quasirandom(aes(x="" , y = RFS_TEM_drop_rater2))+
    ylim(-2,2) +
    labs(x = "RFS_TEM",
         y = "")




#####


