library(MuMIn)

load("2_pipeline/store/models/AMRE_predCor_to_forestAge.rData")




test<-emmeans(AMRE_predCor_to_forestAge)





load("2_pipeline/store/AMRE_fa_co_df.rData")


test<-gather(AMRE_fa_co_df,  "lag", "cor", -forest_age)
test1<-test[complete.cases(test),]


lm_fa_00to01 <- lm(cor ~ poly(forest_age,2), data=filter(test))
m1<-lmer(cor ~ poly(forest_age,2) + (1|lag), data=filter(test))

summary(lm_fa_00to01)
t<-ggpredict(lm_fa_00to01, terms="forest_age [all]", data=test)




lm_fa_00to02 <- lm(cor ~ forest_age, data=filter(test, lag=="predCor_0_02"))
lm_fa_00to03 <- lm(cor ~ forest_age, data=filter(test, lag=="predCor_0_03"))

AMRE_predCor_to_forestAge <-list(lm_fa_00to01, lm_fa_00to02, lm_fa_00to03)

names(AMRE_predCor_to_forestAge) <- c("lm_fa_00to01", "lm_fa_00to02", "lm_fa_00to03")

avg<-model.avg(AMRE_predCor_to_forestAge)




lm_fa_00to02 <- lm(predCor_0_02 ~ forest_age, data=AMRE_fa_co_df)
                   



stocks <- tibble(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)



g<-filter(test, lag=="predCor_0_01")
