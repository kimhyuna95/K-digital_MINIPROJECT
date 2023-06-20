# 데이터 준비
divorcedata= read.csv("D:\\EXAM_R\\data\\2021_이혼.csv")[-1]
divorcedata[, c("남편결혼연령", "아내결혼연령", "남편이혼연령", "아내이혼연령")] <- 
  round(divorcedata[, c("남편결혼연령", "아내결혼연령", "남편이혼연령", "아내이혼연령")], 0)
source("D:\\EXAM_R\\utils\\util.r") 
# 데이터 기본 정보 확인--------------------------------
divorcedata <- divorcedata[sample(1:101673, 1000), ]
divorcedata

# (1) 수치값으로 확인
displayInfo(divorcedata)

# 전처리 ----------------------------------------------
# (1) 결측치 체크
checkNa(divorcedata)
# (2) 최빈값 체크
checkMode(divorcedata)

# (3) 상관관계계 확인
# 컬럼과 컬럼 사이의 관계
plot(divorcedata)
cor(divorcedata)

# 정규성 확인 >> qqplot()/qqline()/hist()/lines()
par(mfrow=c(2, 5))

for(col in colnames(divorcedata)[-1])
{
  qqnorm(divorcedata[,col], main=paste(col, "Q-Q"))
  qqline(divorcedata[,col], col='red', lwd=2)
}

for(col in colnames(divorcedata)[-1])
{
  hist(divorcedata[,col], freq=FALSE, main=paste(col, "HIST"))
  lines(density(divorcedata[,col]), col='red', lwd=2)
}


# (3) 통계함수 수치 값 확인
# 컬럼과 컬럼 사이의 관계 => cor.test(변수1,변수2)
# 아내결혼연령&남편결혼연령
# 남편이혼연&아내이혼연령+동거기간년수
cor.test(divorcedata$남편결혼연령,divorcedata$아내결혼연령)
cor.test(divorcedata$남편이혼연령,divorcedata$아내이혼연령)
cor.test(divorcedata$남편이혼연령,divorcedata$동거기간년수)

# ---------------------------------------------------------------------------
# petal_length, petal_width의 선형회귀분석 구현
# - 독립변수 : 아내결혼연령,아내이혼연령,동거기간년수수
# - 종속변수 : petal_width
# - 분석방식 : 단순선형회귀,다중회귀귀
# - lm(종속~독립,data=데이터)
# ---------------------------------------------------------------------------
# (1) 단순선형회귀 생성
marryMD=lm(divorcedata$남편결혼연령~divorcedata$아내결혼연령,data=divorcedata)
marryMD
divorceMD=lm(divorcedata$남편이혼연령~divorcedata$아내이혼연령+divorcedata$동거기간년수,
           data=divorcedata)


# (3) 단순선형회쉬 보고서(설명서)

summary(marryMD)
summary(divorceMD)





# ---------------------------------------------------------------------------
# 잔차 검정
# - 선행성
# - 정규성: shapiro.test()
# - 등분산성: car 패키지 ncvTest()
# - 독립성 : lmtest 패키지 > dwtest()
# - lm(종속~독립, data=데이터)
# ---------------------------------------------------------------------------
# (1) 모델에 대한 수치값 검정
par(mfrow=c(2, 4))
plot(marryMD)
plot(divorceMD)
# (2) 함수기반 수치값 검정
# (2-1) 정규성
shapiro.test(resid(marryMD)) # p-value가 0.8, 정규성을 가짐 
shapiro.test(resid(divorceMD))


