#데이터 불러오기 및 파악
final <- read.csv("C:/Users/Lee/Desktop/이병관/학회/겨울방학 세션/Toy Project/final.csv")
str(final)
summary(final)
install.packages("dplyr")
library(dplyr)

#연속형 변수 Correlation Plot
library('corrplot')
par(mfrow = c(1,1))
corrplot(cor(final[,c('Weight', 'Length', 'Width', 'Depth', 'Price')]), method = 'circle', addCoef.col = 'white')

#다중회귀분석 > Fluorescence p-value 0.05를 넘어 탈락
lmPrice <-lm((Price~as.numeric(factor(Shape))+Weight+as.numeric(factor(Clarity))+as.numeric(factor(Colour))+as.numeric(factor(Cut))+as.numeric(factor(Polish))+as.numeric(factor(Symmetry))+as.numeric(factor(Fluorescence))+Length+Width+Depth), data=final)
summary(lmPrice)

lmPrice <-lm((Price~as.numeric(factor(Shape))+Weight+as.numeric(factor(Clarity))+as.numeric(factor(Colour))+as.numeric(factor(Cut))+as.numeric(factor(Polish))+as.numeric(factor(Symmetry))+Length+Width+Depth), data=final)
summary(lm((Price~as.numeric(factor(Shape))+Weight+as.numeric(factor(Clarity))+as.numeric(factor(Colour))+as.numeric(factor(Cut))+as.numeric(factor(Polish))+as.numeric(factor(Symmetry))+Length+Width+Depth), data=final))

# 다중공선성(Multicollinerity) [VIF] > Depth, Width, Length 값이 높아 탈락
install.packages('car')
library('car')
vif(lmPrice)

lmPrice <-lm(Price~as.numeric(factor(Shape))+Weight+as.numeric(factor(Clarity))+as.numeric(factor(Colour))+as.numeric(factor(Cut))+as.numeric(factor(Polish))+as.numeric(factor(Symmetry)), data=final)
vif(lmPrice)

# 회귀분석 p value가 높아 Cut, Polish 탈락
summary(lmPrice)
lmPrice <-lm(Price~as.numeric(factor(Shape))+Weight+as.numeric(factor(Clarity))+as.numeric(factor(Colour))+as.numeric(factor(Symmetry)), data=final)
summary(lmPrice)

#잔차의 독립성 > 1.3 정도, 2 근처로 가야함 (0~4), 적당히 봐줄만 한 정도
durbinWatsonTest(residuals(lmPrice))

#최적모형 선택
install.packages('leaps')
library(leaps)
regsubsets(Price~as.numeric(factor(Shape))+Weight+as.numeric(factor(Clarity))+as.numeric(factor(Colour))+as.numeric(factor(Symmetry)), data=final, nbest = 3)
regsubsets(Price~as.numeric(factor(Shape))+Weight+as.numeric(factor(Clarity))+as.numeric(factor(Colour))+as.numeric(factor(Symmetry)), data=final, nbest = 3) %>% summary() #*이 사용되면 최적의 변수 

par(mfrow = c(1,1)) 
regsubsets(Price~as.numeric(factor(Shape))+Weight+as.numeric(factor(Clarity))+as.numeric(factor(Colour))+as.numeric(factor(Symmetry)), data=final, nbest = 3) %>% plot(scale='adjr2')
regsubsets(Price~as.numeric(factor(Shape))+Weight+as.numeric(factor(Clarity))+as.numeric(factor(Colour))+as.numeric(factor(Symmetry)), data=final, nbest = 3) %>% plot(scale='bic')
regsubsets(Price~as.numeric(factor(Shape))+Weight+as.numeric(factor(Clarity))+as.numeric(factor(Colour))+as.numeric(factor(Symmetry)), data=final, nbest = 3) %>% plot(scale='r2')

#회귀분석 시 중요도
relweights <- function(fit,...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta ^ 2)
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  import <- as.data.frame(import)
  row.names(import) <- names(fit$model[2:nvar])
  names(import) <- "Weights"
  import <- import[order(import),1, drop=FALSE]
  dotchart(import$Weights, labels=row.names(import),
           xlab="% of R-Square", pch=19,
           main="Relative Importance of Predictor Variables",
           sub=paste("Total R-Square=", round(rsquare, digits=3)),
           ...)
  return(import)
}         
summary(lmPrice)
relweights(lmPrice, col='blue')
