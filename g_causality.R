library(vars)

MyData <- read.csv(file="testdata.csv")
head(MyData)
attach(MyData)

loan.d<-loansinflation
medrent<-rentinflation
upp_bound<-uppboundinflation
low_bound<-lowboundinflation

loan.d<-diff(log(loan.d))
medrent<-diff(log(medrent))
upp_bound<-diff(log(upp_bound))
low_bound<-diff(log(low_bound))

medbind<-cbind(loan.d, medrent)
uppbind<-cbind(loan.d, upp_bound)
lowbind<-cbind(loan.d, low_bound)

##fit median rent to VAR
med_VAR<-VAR(medbind,type="none",ic="AIC")
summary(med_VAR)

##fit median rent + upper bound MOE to VAR
upp_VAR<-VAR(uppbind,type="none",ic="AIC")
summary(upp_VAR)

##fit median rent + lower bound MOE to VAR
low_VAR<-VAR(lowbind,type="none",ic="AIC")
summary(low_VAR)

##determine optimal lags
select.lags<-function(x,y,max.lag=2) {
  y<-as.numeric(y)
  y.lag<-embed(y,max.lag+1)[,-1,drop=FALSE]
  x.lag<-embed(x,max.lag+1)[,-1,drop=FALSE]
  
  t<-tail(seq_along(y),nrow(y.lag))
  
  ms=lapply(1:max.lag,function(i) lm(y[t]~y.lag[,1:i]+x.lag[,1:i]))
  
  pvals<-mapply(function(i) anova(ms[[i]],ms[[i-1]])[2,"Pr(>F)"],max.lag:2)
  ind<-which(pvals<0.05)[1]
  ftest<-ifelse(is.na(ind),1,max.lag-ind+1)
  
  aic<-as.numeric(lapply(ms,AIC))
  bic<-as.numeric(lapply(ms,BIC))
  structure(list(ic=cbind(aic=aic,bic=bic),pvals=pvals,
                 selection=list(aic=which.min(aic),bic=which.min(bic),ftest=ftest)))
}

s<-select.lags(loan.d,medrent,2)

t(s$selection)

##Granger-casuality test of loan demand on median rent and vice-versa  
causality(med_VAR, cause="loan.d")$Granger
causality(med_VAR, cause="medrent")$Granger

##Granger-casuality test of loan demand on median rent + MOE and vice-versa
causality(upp_VAR, cause="loan.d")$Granger
causality(upp_VAR, cause="upp_bound")$Granger

##Granger-casuality test of loan demand on median rent - MOE and vice-versa
causality(low_VAR, cause="loan.d")$Granger
causality(low_VAR, cause="low_bound")$Granger

