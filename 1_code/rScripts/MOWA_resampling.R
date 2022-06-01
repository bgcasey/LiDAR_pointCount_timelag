## http://zevross.com/blog/2017/09/19/predictive-modeling-and-machine-learning-in-r-with-the-caret-package/

library(caret)

library(MLmetrics)

load("2_pipeline/tmp/sdm_d.rData")
d<-sdm_d


d_0<-sdm_d%>%filter(SS_lidar_timelag==0)

d_0$MOWA<- factor(d_0$MOWA, 
                  levels = c(0,1), 
                  labels = c("Zero","One"))


fit.control <- trainControl(
              #method = "repeatedcv",
              method = "boot",
              #repeats=3,
              #verboseIter = TRUE,
              summaryFunction = twoClassSummary, 
              classProbs = TRUE)

set.seed(1) 
lm1 <- train(MOWA ~  canopy_relief_ratio +  elev_maximum + elev_stddev + total_all_returns, 
             data = d_0,
             offset=d_0$MOWA_OFF,
             method = "glm", 
             metric="ROC",
             family = "binomial",
             tuneLength = 30,
             trControl=fit.control)


lm1




rpartProbs <- predict(lm1, d_0, type="prob")
head(rpartProbs
     )




predicted <- predict(lm1, type="response")

roc.glmBoostModel <- pROC::roc(d_0$MOWA, lm1)

auc_d_1<-auc(d_0$MOWA, predicted)

pred.glmBoostModel <- as.vector(predict(lm1, newdata=d_0, type="prob")[,"yes"])


############################################

#stats function
AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response")
  AUC<-as.numeric(auc(filter(d, SS_lidar_timelag==-1)$MOWA, pred))
  }

#test
AUCFun(m_1)

test<-bootMer(m_1, nsim = 100, seed = 101, FUN=AUCFun,
        type = "semiparametric",
        verbose = TRUE, .progress = "none",
        use.u = TRUE)





