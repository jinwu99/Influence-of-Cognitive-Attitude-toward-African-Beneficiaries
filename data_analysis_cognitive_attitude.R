getwd()
rm(list=ls())
library(psych)
library(dplyr)
library(corrplot)
library(lmerTest)

dfA <- read.csv('survey_A.csv')
dfB <- read.csv('survey_B.csv')

# Only extract likert scores
for(j in 5:27){
  dfA[,j] <- as.numeric(gsub("[^0-9]", "", dfA[,j]))
  dfB[,j] <- as.numeric(gsub("[^0-9]", "", dfB[,j]))
}

map_values <- function(x){
  case_when(
    x == '전혀그렇지않다' ~ 1,
    x == '그렇지않다' ~ 2,
    x == '보통이다' ~ 3,
    x == '그렇다' ~ 4,
    x == '매우그렇다' ~ 5,
    TRUE ~ NA_real_
  )
}

dfA <- dfA %>% mutate(across(all_of(tmp[28:40]),
                             ~map_values(.),
                             .names="{.col}"))
dfB <- dfB %>% mutate(across(all_of(tmp[28:40]),
                             ~map_values(.),
                             .names="{.col}"))

dfA[dfA['gender']=='남자','gender'] <- 'male'
dfA[dfA['gender']=='여자','gender'] <- 'female'
dfA$age <- as.numeric(gsub("[^0-9]", "", dfA$age))
unique(dfA$dep)
dfB[dfB['gender']=='남자','gender'] <- 'male'
dfB[dfB['gender']=='여자','gender'] <- 'female'
dfB$age <- as.numeric(gsub("[^0-9]", "", dfB$age))

dfA[dfA$exp=='없다','exp'] <- 0
dfA[dfA$exp=='일시적으로 기부를 해본 적이 있다.(예: 명절, 연말, 후원방송 ARS 전화기부 등)','exp'] <- 1
dfA[dfA$exp=='정기적인 기부활동을 하고있다.','exp'] <- 2
dfB[dfB$exp=='없다','exp'] <- 0
dfB[dfB$exp=='일시적으로 기부를 해본 적이 있다.(예: 명절, 연말, 후원방송 ARS 전화기부 등)','exp'] <- 1
dfB[dfB$exp=='정기적인 기부활동을 하고있다.','exp'] <- 2

# 학과 종류가 많아 데이터가 sparse해지므로, 학과를 계열별로 축소
map_values <- function(x){
  case_when(
    x == '사회체육' ~ '사과',
    x == '한국어문화' ~ '언어',
    x == '회계' ~ '상경',
    x == '국제문화관광' ~ '상경',
    x == '영상콘텐츠융합' ~ '사과',
    x == '독일어' ~ '언어',
    x == '아랍' ~ '언어',
    x == '중국' ~ '언어',
    x == '아세안' ~ '언어',
    x == '경영' ~ '상경',
    x == '베트남어' ~ '언어',
    x == '국제무역' ~ '상경',
    x == '러시아어' ~ '언어',
    x == '컴퓨터공학' ~ 'IT',
    x == '스마트융합보안' ~ '사과',
    x == '동남아창의융합' ~ '언어',
    x == '일본어' ~ '언어',
    x == '전자로봇공' ~ 'IT',
    x == '외교' ~ '사과',
    x == '국제' ~ '사과',
    x == '스페인어' ~ '언어',
    x == '사회복지' ~ '사과',
    x == '중국어' ~ '언어',
    x == '영어' ~ '언어',
    x == '경제금융' ~ '상경',
    x == '아랍어' ~ '언어',
    x == '글로벌한국' ~ '언어',
    x == '국제학부' ~ '사과',
    x == '한국어교육' ~ '언어',
    x == '국제개발협력' ~ '사과',
    x == '국제개발학과' ~ '사과',
    x == 'G2융합' ~ '언어',
    x == '전자로봇공학' ~ 'IT',
    x == '임베디드IT' ~ 'IT',
    x == '프랑스어/국제개발협력' ~ '언어',
    x == '태국어' ~ '언어',
    FALSE ~ 'NA'
  )
}

dfA <- dfA %>% mutate(dep = map_values(dep))
dfB <- dfB %>% mutate(dep = map_values(dep))

# 현재 하나의 row에 loss/gain framing의 응답이 모두 있기 때문에
# 하나의 row에 하나의 응답만 있게끔 데이터를 변형해야한다.
loss_col <- tmp[c(1:14,15:27,41:45)]
gain_col <- tmp[c(1:14,28:40,41:45)]
dfA_loss <- dfA[,loss_col]
dfA_gain <- dfA[,gain_col]
colnames(dfA_gain) <- colnames(dfA_loss)
dfA_loss$MF <- 0; dfA_gain$MF <- 1
dfA_loss$id <- 1:dim(dfA_loss)[1]; dfA_gain$id <- 1:dim(dfA_gain)[1]

tmp <- colnames(dfB)
loss_col <- tmp[c(1:14,28:40,41:45)] ########## careful
gain_col <- tmp[c(1:14,15:27,41:45)] ########## careful
dfB_loss <- dfB[,loss_col]
dfB_gain <- dfB[,gain_col]
colnames(dfB_loss) <- colnames(dfB_gain) ########## careful
dfB_loss$MF <- 0; dfB_gain$MF <- 1
dfB_loss$id <- (dim(dfA_loss)[1]+1):(dim(dfA_loss)[1]+dim(dfB_loss)[1])
dfB_gain$id <- (dim(dfA_gain)[1]+1):(dim(dfA_gain)[1]+dim(dfB_gain)[1])

# 역코딩
dfA_loss$emp_pos2 <- 6-dfA_loss$emp_pos2_neg
dfA_loss$sent1 <- 6-dfA_loss$sent1_neg
dfA_loss$manip3 <- 6-dfA_loss$manip3_neg
dfA_gain$emp_pos2 <- 6-dfA_gain$emp_pos2_neg
dfA_gain$sent1 <- 6-dfA_gain$sent1_neg
dfA_gain$manip3 <- 6-dfA_gain$manip3_neg

dfB_loss$emp_pos2 <- 6-dfB_loss$emp_pos2_neg
dfB_loss$sent1 <- 6-dfB_loss$sent1_neg
dfB_loss$manip3 <- 6-dfB_loss$manip3_neg
dfB_gain$emp_pos2 <- 6-dfB_gain$emp_pos2_neg
dfB_gain$sent1 <- 6-dfB_gain$sent1_neg
dfB_gain$manip3 <- 6-dfB_gain$manip3_neg

df <- rbind(dfA,dfB)

# 요인분석을 실행하기 이전에 신뢰도계수와 상관계수를 확인한다
cog_alpha <- alpha(df[c(1:68,207:279),2:4])$total$raw_alpha; cog_alpha
iss_alpha <- alpha(df[c(1:68,207:279),5:7])$total$raw_alpha; iss_alpha
pre_alpha <- alpha(df[c(1:68,207:279),8:11])$total$raw_alpha; pre_alpha
emp_alpha <- alpha(df[c(1:68,207:279),c(12,35,14)])$total$raw_alpha; emp_alpha
eff_alpha <- alpha(df[,17:21])$total$raw_alpha; eff_alpha
beh_alpha <- alpha(df[,22:24])$total$raw_alpha; beh_alpha
manip_alpha <- alpha(df[,c(25,26,37)])$total$raw_alpha; manip_alpha

corrplot(cor(df[,c('indep','comp','capb',
                   'iss_inv1','iss_inv2','iss_inv3',
                   'pre_att1','pre_att2','pre_att3','pre_att4',
                   'emp_pos1','emp_pos2','emp_pos3')]),
         order = 'hclust', addrect = 3, method='number')
corrplot(cor(df[,c('manip1','manip2','manip3',
                   'eff1','eff2','eff3','eff4','eff5',
                   'beh1','beh2','beh3')]),
         order = 'hclust', addrect = 3, method='number')
corrplot(cor(df[,c('indep','comp','capb',
                   'iss_inv1','iss_inv2','iss_inv3',
                   'pre_att1','pre_att2','pre_att3','pre_att4',
                   'emp_pos1','emp_pos2','emp_pos3',
                   'manip1','manip2','manip3',
                   'eff1','eff2','eff3','eff4','eff5',
                   'beh1','beh2','beh3')]))

# 요인분석을 하되, 잠재변수 중 반복측정이거나 그렇지 않은 경우 모두 있기 때문에
# 반복측정의 경우 모든 응답을 요인분석에 활용하고
# 그렇지 않은 경우 중복데이터가 있지 않게끔 요인분석을 한다.
loss_cog_att <- fa(rbind(dfA_loss[,c('indep','comp','capb')],
                         dfB_loss[,c('indep','comp','capb')]), nfactors = 1, fm = "ml")
loss_iss_inv <- fa(rbind(dfA_loss[,c('iss_inv1','iss_inv2','iss_inv3')],
                         dfB_loss[,c('iss_inv1','iss_inv2','iss_inv3')]), nfactors = 1, fm = "ml")
loss_pre_att <- fa(rbind(dfA_loss[,c('pre_att1','pre_att2','pre_att3','pre_att4')],
                         dfB_loss[,c('pre_att1','pre_att2','pre_att3','pre_att4')]), nfactors = 1, fm = "ml")
loss_emp_pos <- fa(rbind(dfA_loss[,c('emp_pos1','emp_pos2','emp_pos3')],
                         dfB_loss[,c('emp_pos1','emp_pos2','emp_pos3')]), nfactors = 1, fm = "ml")
dfA_loss$cog_att <- loss_cog_att$scores[1:77]
dfB_loss$cog_att <- loss_cog_att$scores[78:156]
dfA_loss$iss_inv <- loss_iss_inv$scores[1:77]
dfB_loss$iss_inv <- loss_iss_inv$scores[78:156]
dfA_loss$pre_att <- loss_pre_att$scores[1:77]
dfB_loss$pre_att <- loss_pre_att$scores[78:156]
dfA_loss$emp_pos <- loss_emp_pos$scores[1:77]
dfB_loss$emp_pos <- loss_emp_pos$scores[78:156]

gain_cog_att <- fa(rbind(dfA_gain[,c('indep','comp','capb')],
                         dfB_gain[,c('indep','comp','capb')]), nfactors = 1, fm = "ml")
gain_iss_inv <- fa(rbind(dfA_gain[,c('iss_inv1','iss_inv2','iss_inv3')],
                         dfB_gain[,c('iss_inv1','iss_inv2','iss_inv3')]), nfactors = 1, fm = "ml")
gain_pre_att <- fa(rbind(dfA_gain[,c('pre_att1','pre_att2','pre_att3','pre_att4')],
                         dfB_gain[,c('pre_att1','pre_att2','pre_att3','pre_att4')]), nfactors = 1, fm = "ml")
gain_emp_pos <- fa(rbind(dfA_gain[,c('emp_pos1','emp_pos2','emp_pos3')],
                         dfB_gain[,c('emp_pos1','emp_pos2','emp_pos3')]), nfactors = 1, fm = "ml")
dfA_gain$cog_att <- gain_cog_att$scores[1:77]
dfB_gain$cog_att <- gain_cog_att$scores[78:156]
dfA_gain$iss_inv <- gain_iss_inv$scores[1:77]
dfB_gain$iss_inv <- gain_iss_inv$scores[78:156]
dfA_gain$pre_att <- gain_pre_att$scores[1:77]
dfB_gain$pre_att <- gain_pre_att$scores[78:156]
dfA_gain$emp_pos <- gain_emp_pos$scores[1:77]
dfB_gain$emp_pos <- gain_emp_pos$scores[78:156]

dfA <- rbind(dfA_loss,dfA_gain)
dfB <- rbind(dfB_loss,dfB_gain)

dfA$eff <- fa(dfA[,c('eff1','eff2','eff3','eff4','eff5')], nfactors = 1, fm = "ml")$scores
dfA$beh <- fa(dfA[,c('beh1','beh2','beh3')], nfactors = 1, fm = "ml")$scores
dfA$manip <- fa(dfA[,c('manip1','manip2','manip3')], nfactors = 1, fm = "ml")$scores

dfB$eff <- fa(dfB[,c('eff1','eff2','eff3','eff4','eff5')], nfactors = 1, fm = "ml")$scores
dfB$beh <- fa(dfB[,c('beh1','beh2','beh3')], nfactors = 1, fm = "ml")$scores
dfB$manip <- fa(dfB[,c('manip1','manip2','manip3')], nfactors = 1, fm = "ml")$scores

df <- rbind(dfA,dfB)
# write.csv(df,file='survey_AB.csv',row.names=FALSE)

df <- read.csv('survey_AB.csv')
df <- df[complete.cases(df),]

# preprocessing
df$MF <- as.factor(df$MF)
df$exp <- as.factor(df$exp)
df$id <- as.factor(df$id)
df$dep <- as.factor(df$dep)
for(v in c('age','cog_att','iss_inv','pre_att','emp_pos','eff','beh','manip')){
  print(v)
  df[,v] <- scale(df[,v])
}

# testing whether the survey has done well
df$sent <- (df$sent1 + df$sent2)/2
par(mfrow=c(1,1))
boxplot(df[df$MF==0,'sent'],df[df$MF==1,'sent'])
t.test(df[df$MF==0,'sent'],df[df$MF==1,'sent'],paired=T) # good!

# regression for testing interaction
lm_e <- lmer(eff ~ iss_inv + emp_pos + 
               dep + gender +  
               cog_att*MF + manip + (1|id), data=df)
lm_b <- lmer(beh ~ iss_inv + emp_pos + 
               dep + gender +  
               cog_att*MF + manip + (1|id), data=df)
lm_m <- lmer(manip ~ iss_inv + emp_pos + pre_att +
               dep + gender + exp + age +
               cog_att*MF + (1|id), data=df)
lm_e2 <- lmer(eff ~ iss_inv + emp_pos + pre_att + 
                dep + gender + exp + age +
                cog_att + MF + manip + (1|id), data=df)
lm_b2 <- lmer(beh ~ iss_inv + emp_pos + pre_att + 
                dep + gender + exp + age +
                cog_att + MF + manip + (1|id), data=df)
lm_m2 <- lmer(manip ~ iss_inv + emp_pos + pre_att + 
                dep + gender + exp + age +
                cog_att + MF + (1|id), data=df)
anova(lm_e2,lm_e)
anova(lm_b2,lm_b)
anova(lm_m2,lm_m)
summary(lm_e)
summary(lm_b)
summary(lm_m)
summary(lm_e2)
summary(lm_b2)
summary(lm_m2)
# testing residuals and influential data
rr_e <- residuals(lm_e)
rr_b <- residuals(lm_b)
rr_m <- residuals(lm_m)
rr_e2 <- residuals(lm_e2)
rr_b2 <- residuals(lm_b2)
rr_m2 <- residuals(lm_m2)
par(mfrow=c(3,2))
plot(rr_e,ylab='residual for effectiveness'); qqnorm(rr_e)#; lines(c(-3,3),c(-3,3),type='l',col='red')
plot(rr_b,ylab='residual for behavioral'); qqnorm(rr_b)#; lines(c(-3,3),c(-3,3),type='l',col='red')
plot(rr_m,ylab='residual for pmi'); qqnorm(rr_m)#; lines(c(-3,3),c(-3,3),type='l',col='red')
plot(rr_e2,ylab='residual for effectiveness'); qqnorm(rr_e2)#; lines(c(-3,3),c(-3,3),type='l',col='red')
plot(rr_b2,ylab='residual for behavioral'); qqnorm(rr_b2)#; lines(c(-3,3),c(-3,3),type='l',col='red')
plot(rr_m2,ylab='residual for pmi'); qqnorm(rr_m2)#; lines(c(-3,3),c(-3,3),type='l',col='red')

library(car)
vif(lm_e)
vif(lm_b)
vif(lm_m)
vif(lm_e2)
vif(lm_b2)
vif(lm_m2)

par(mfcol=c(3,1))
cooksd <- cooks.distance(lm_e)
plot(sort(cooksd), pch = 20, main = "Cook's distance plot for effectiveness")
influ_e <- which(cooksd >= 0.20)
influ_e <- which(cooksd >= 0.15)
influ_e <- which(cooksd >= 0.10)
influ_e <- which(cooksd >= 3*mean(cooksd))
points(sort(influ_e),sort(cooksd[influ_e]),col='red')

cooksd <- cooks.distance(lm_b)
plot(sort(cooksd), pch = 20, main = "Cook's distance plot for behavioral")
influ_b <- which(cooksd >= 1)
influ_b <- which(cooksd >= 0.8)
influ_b <- which(cooksd >= 0.5)
influ_b <- which(cooksd >= 3*mean(cooksd))
points(influ_b,cooksd[influ_b],col='red')

cooksd <- cooks.distance(lm_m)
plot(sort(cooksd), pch = 20, main = "Cook's distance plot for pmi")
influ_m <- which(cooksd >= 1)
influ_m <- which(cooksd >= 0.18)
influ_m <- which(cooksd >= 0.30)
influ_m <- which(cooksd >= 3*mean(cooksd))
points(sort(influ_m),sort(cooksd[influ_m]),col='red')

cooksd <- cooks.distance(lm_e2)
plot(sort(cooksd), pch = 20, main = "Cook's distance plot for effectiveness")
influ_e <- which(cooksd >= 1)
influ_e <- which(cooksd >= 0.10)
influ_e <- which(cooksd >= 0.12)
influ_e <- which(cooksd >= 3*mean(cooksd))
points(sort(influ_e),sort(cooksd[influ_e]),col='red')

cooksd <- cooks.distance(lm_b2)
plot(sort(cooksd), pch = 20, main = "Cook's distance plot for behavioral")
influ_b <- which(cooksd >= 1)
influ_b <- which(cooksd >= 0.8)
influ_b <- which(cooksd >= 0.5)
influ_b <- which(cooksd >= 0.4)
influ_b <- which(cooksd >= 0.3)
influ_b <- which(cooksd >= 3*mean(cooksd))
points(influ_b,cooksd[influ_b],col='red')

cooksd <- cooks.distance(lm_m2)
plot(sort(cooksd), pch = 20, main = "Cook's distance plot for pmi")
influ_m <- which(cooksd >= 1)
influ_m <- which(cooksd >= 0.18)
influ_m <- which(cooksd >= 0.30)
influ_m <- which(cooksd >= 3*mean(cooksd))
points(sort(influ_m),sort(cooksd[influ_m]),col='red')

df <- df[-influ_e,]
df <- df[-influ_b,]
df <- df[-influ_m,]

length(unique(df$id))

# function form analysis for final model :
col_name <- c('issue involement',
              'dispositional empathetic concern',
              'age',
              'perceived manipulative intent')
par(mfrow=c(2,2))
idx <- 1
for(v in c('iss_inv','emp_pos','manip','age')){
  # plot(df[,v],rr_e,xlab=col_name[idx],ylab='residual',
  #      main='Functional form for effectviness')
  # lines(lowess(df[,v],rr_e),col='red')
  plot(df[,v],rr_e2,xlab=col_name[idx],ylab='residual',
       main='Functional form for effectviness')
  lines(lowess(df[,v],rr_e2),col='red')
  idx <- idx+1
}

par(mfrow=c(2,2))
idx <- 1
for(v in c('iss_inv','emp_pos','manip','age')){
  # plot(df[,v],rr_b,xlab=col_name[idx],ylab='residual',
  #      main='Functional form for behavioral')
  # lines(lowess(df[,v],rr_b),col='red')
  plot(df[,v],rr_b2,xlab=col_name[idx],ylab='residual',
       main='Functional form for behavioral')
  lines(lowess(df[,v],rr_b2),col='red')
  idx <- idx+1
}

par(mfrow=c(2,2))
idx <- 1
for(v in c('iss_inv','emp_pos','age')){
  # plot(df[,v],rr_m,xlab=col_name[idx],ylab='residual',
  #      main='Functional form for pmi')
  # lines(lowess(df[,v],rr_m),col='red')
  plot(df[,v],rr_m2,xlab=col_name[idx],ylab='residual',
       main='Functional form for pmi')
  lines(lowess(df[,v],rr_m2),col='red')
  idx <- idx+1
}

# final model
# testing for random effect
lm_e <- lmer(eff ~ iss_inv + emp_pos + pre_att +
               dep + gender + exp + age +
               cog_att + MF + manip + (1|id), data=df)
lm_b <- lmer(beh ~ iss_inv + emp_pos + pre_att +
               dep + gender + exp + age +
               cog_att + MF + manip + (1|id), data=df)
lm_m <- lmer(manip ~ iss_inv + emp_pos + pre_att +
               dep + gender + exp + age +
               cog_att + MF + (1|id), data=df)
lm_e2 <- lm(eff ~ iss_inv + emp_pos + pre_att +
              dep + gender + exp + age +
              cog_att + MF + manip, data=df)
lm_b2 <- lm(beh ~ iss_inv + emp_pos + pre_att +
              dep + gender + exp + age +
              cog_att + MF + manip, data=df)
lm_m2 <- lm(manip ~ iss_inv + emp_pos + pre_att +
              dep + gender + exp + age +
              cog_att + MF, data=df)
# random effects do good work!
anova(lm_e, lm_e2)
anova(lm_b, lm_b2)
anova(lm_m, lm_m2)
summary(lm_e)
summary(lm_b)
summary(lm_m)
# testing for model themselves
lm_e0 <- lmer(eff ~ (1|id), data=df)
lm_b0 <- lmer(beh ~ (1|id), data=df)
lm_m0 <- lmer(manip ~ (1|id), data=df)
anova(lm_e, lm_e0)
anova(lm_b, lm_b0)
anova(lm_m, lm_m0)

# mediation analysis
# testing correlation between random effects
rand_e <- ranef(lm_e)$id
rand_b <- ranef(lm_b)$id
rand_m <- ranef(lm_m)$id
par(mfrow=c(1,2))
plot(unlist(rand_m),unlist(rand_e),
     xlab='Random effects for pmi',
     ylab='Random effects for effectiveness')
corr <- paste('cor :',round(cor(rand_e,rand_m),2))
legend(0.4,0.7,corr,cex=0.8)
plot(unlist(rand_m),unlist(rand_b),
     xlab='Random effects for pmi',
     ylab='Random effects for behavioral')
corr <- paste('cor :',round(cor(rand_b,rand_m),2))
legend(0.4,1.3,corr,cex=0.8)
# icc
icc_e = as.data.frame(VarCorr(lm_e))
icc_b = as.data.frame(VarCorr(lm_b))
icc_m = as.data.frame(VarCorr(lm_m))
icc_e$vcov[1]/(icc_e$vcov[1]+icc_e$vcov[2])
icc_b$vcov[1]/(icc_b$vcov[1]+icc_b$vcov[2])
icc_m$vcov[1]/(icc_m$vcov[1]+icc_m$vcov[2])

IDE_eff_int <- c(); IDE_eff_cog <- c(); IDE_eff_MF <- c()
IDE_beh_int <- c(); IDE_beh_cog <- c(); IDE_beh_MF <- c()
SIM <- 1000
for(s in 1:SIM){
  if(s%%10==0){
    print(s)
  }
  df_sim <- df[sample(nrow(df), replace = TRUE),]
  
  lm_e <- lmer(eff ~ iss_inv + emp_pos + pre_att +
                 dep + gender + exp + age +
                 cog_att + MF + manip + (1|id), data=df_sim)
  lm_b <- lmer(beh ~ iss_inv + emp_pos + pre_att +
                 dep + gender + exp + age +
                 cog_att + MF + manip + (1|id), data=df_sim)
  lm_m <- lmer(manip ~ iss_inv + emp_pos + pre_att +
                 dep + gender + exp + age +
                 cog_att + MF + (1|id), data=df_sim)
  
  IDE_eff_int[s] <- fixef(lm_m)['cog_att:MF1']*fixef(lm_e)['manip']
  IDE_eff_cog[s] <- fixef(lm_m)['cog_att']*fixef(lm_e)['manip']
  IDE_eff_MF[s] <- fixef(lm_m)['MF1']*fixef(lm_e)['manip']
  IDE_beh_int[s] <- fixef(lm_m)['cog_att:MF1']*fixef(lm_b)['manip']
  IDE_beh_cog[s] <- fixef(lm_m)['cog_att']*fixef(lm_b)['manip']
  IDE_beh_MF[s] <- fixef(lm_m)['MF1']*fixef(lm_b)['manip']
}
# IDEs
p_eff_int <- 2*min(sum(IDE_eff_int>=0)/length(IDE_eff_int),sum(IDE_eff_int<0)/length(IDE_eff_int))
p_eff_cog <- 2*min(sum(IDE_eff_cog>=0)/length(IDE_eff_cog),sum(IDE_eff_cog<0)/length(IDE_eff_cog))
p_eff_MF <- 2*min(sum(IDE_eff_MF>=0)/length(IDE_eff_MF),sum(IDE_eff_MF<0)/length(IDE_eff_MF))
p_beh_int <- 2*min(sum(IDE_beh_int>=0)/length(IDE_beh_int),sum(IDE_beh_int<0)/length(IDE_beh_int))
p_beh_cog <- 2*min(sum(IDE_beh_cog>=0)/length(IDE_beh_cog),sum(IDE_beh_cog<0)/length(IDE_beh_cog))
p_beh_MF <- 2*min(sum(IDE_beh_MF>=0)/length(IDE_beh_MF),sum(IDE_beh_MF<0)/length(IDE_beh_MF))

IDE <- c(mean(IDE_eff_cog),sd(IDE_eff_cog),p_eff_cog,
         mean(IDE_eff_MF),sd(IDE_eff_MF),p_eff_MF,
         mean(IDE_beh_cog),sd(IDE_beh_cog),p_beh_cog,
         mean(IDE_beh_MF),sd(IDE_beh_MF),p_beh_MF)
IDE_results <- matrix(IDE,ncol=3,byrow=T)
colnames(IDE_results) <- c('mean','sd','p-value')
rownames(IDE_results) <- c('Eff_Cog','Eff_MF',
                           'Beh_Cog','Beh_MF')
IDE_results
