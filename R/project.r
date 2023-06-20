install.packages("dplyr")
install.packages("ggplot2")
library("dplyr")
library("ggplot2")



# 외부 데이터 파일 로딩----------------------------------------------
taxidata= read.csv("")
# 데이터 확인--------------------------------------------------------
# (1) 구조 및 정보 출력
str(taxidata)
# (2) 행과 컬럼 갯수 출력
dim(taxidata)
# (3-1) 컬럼명 출력
colnames(taxidata)
head(taxidata)
# 결측치 확인 
complete.cases(taxidata)
sum(is.na(taxidata))
# 공백 지우기
index=which(taxidata$payment=="")
index
taxidata = taxidata[-c(8,446,492,546,622,771,914,954,1208,1373, 1567, 1691, 1705, 1738, 1852,
1861, 1930, 1942, 2354, 2445, 3110, 3373, 3794, 3804, 3984, 4231, 4385, 4475, 4516, 4563,
4747, 4802, 4843, 4916, 4956, 5047, 5069, 5208, 5285, 5587, 5810, 5822, 6170, 6312), ]

index2=which(taxidata$pickup_borough=="")
index2
taxidata = taxidata[-c(42,  603,  618,  667,  708,  963, 1101, 1944, 2120, 2723, 3066, 3239, 3623, 3866, 4094,
4103, 4256, 4388, 4742, 4908, 5226, 5455, 5585, 5599, 6042), ]

index3=which(taxidata$dropoff_borough=="")
index3
taxidata = taxidata[-c(703,  730, 1098, 1215, 1767, 2064, 2154, 2205, 2375, 2543, 2876, 3210, 3467, 3627, 4012,
4179, 4422, 4765, 5551, 5585, 6052, 6246, 6288), ]

index4=which(taxidata$distance==0)
index4
taxidata = taxidata[-c( 119,  663, 1065, 1462, 1883, 2363, 3054, 3179, 3196, 3332, 4207, 4421, 4487, 4599, 4900,
5356, 5467, 5468, 5595, 5600, 5614, 5638, 5713, 5872, 5995, 6031, 6042, 6156, 6265, 6266,
6281, 6302), ]

taxidata$pickup_zone=NULL
taxidata$dropoff_zone=NULL
str(taxidata)
dim(taxidata)
summary(taxidata)

# 택시 이용 지역 빈도수 비율
pickup_borough=table(taxidata$pickup_borough)
pickup_prop=prop.table(pickup_borough)
pickup_prop*100

dropoff_borough=table(taxidata$dropoff_borough)
dropoff_prop=prop.table(dropoff_borough)
dropoff_prop*100

# pi 그림
pickup_se <- prop.table(pickup_borough)
label <- paste(names(pickup_se),'\n',round(pickup_se*100,1),'%',sep='')

pie(pickup_se,col=c('#f0f0f0','#63a3f7',"skyblue","gray"),labels=label,border='white',main='탑승 지역')

dropoff_se <- prop.table(dropoff_borough)
label <- paste(names(dropoff_se),'\n',round(dropoff_se*100,1),'%',sep='')
pie(dropoff_se,col=c('#f0f0f0','#63a3f7',"skyblue","gray"),labels=label,border='white',main='하차 지역')

# 거리당 요금 계산
distance_fare=round(taxidata$fare/taxidata$distance,1)
distance_fare
# 데이터 프레임 거리당 요금 컬럼 추가
taxidata$distance_fare=distance_fare
str(taxidata)


# 지역별로 평균요금 그룹핑
areadata<-taxidata %>% 
group_by(pickup_borough)%>% 
summarize(mean_distance_fare = mean(distance_fare))
areadata

# 그래프 그리기
ggplot(areadata, aes(x=pickup_borough, y=mean_distance_fare)) +
  geom_bar(stat="identity") +
  labs(title="픽업 지역별 평균 요금", x="픽업 지역", y="평균 요금")

# 가설 1: 맨해튼과 퀸즈의 평균 요금이 같지 않다
# 귀무가설: 맨해튼과 퀸즈의 평균 요금이 같다.
# 대립가설: 맨해튼과 퀸즈의 평균 요금이 같지 않다.

# 이상치 제외한 taxi 데이터 생성
taxidata_no_outliers <- taxidata %>%
  filter(distance_fare >= quantile(distance_fare, 0.01) &
         distance_fare <= quantile(distance_fare, 0.99))
# distance_fare 변수 생성
taxidata_no_outliers <- taxidata_no_outliers %>%
  mutate(distance_fare = round(fare / distance, 1))

# 맨해튼과 퀸즈 지역 평균 요금 비교를 위한 t-test
manhattan_data <- taxidata_no_outliers %>%
  filter(pickup_borough == "Manhattan")

queens_data <- taxidata_no_outliers %>%
  filter(pickup_borough == "Queens")

t_test_result <- t.test(manhattan_data$distance_fare, queens_data$distance_fare)
t_test_result

# 시각화
taxidata_no_outliers %>%
  filter(pickup_borough %in% c("Manhattan", "Queens")) %>%
  ggplot(aes(x = pickup_borough, y = distance_fare, fill = pickup_borough)) +
  geom_boxplot() +
  ggtitle("Distribution of Distance Fare by Pickup Borough") +
  xlab("Pickup Borough") +
  ylab("Distance Fare")

#  0.05보다 작기 때문에, 맨해탄과 퀸즈의 평균 요금은 같지 않다는 가설을 기각할 수 있습니다.



bars <- tapply(health_EU$Life_Expectancy, health_EU$Country, mean)
lower <- tapply(health_EU$Life_Expectancy, health_EU$Country, function(x) t.test(x)$conf.int[1])
upper <- tapply(health_EU$Life_Expectancy, health_EU$Country, function(x) t.test(x)$conf.int[2])

library(gplots)
barplot2(bars, space=0.4, plot.ci=TRUE, ci.l=lower, ci.u=upper, ci.color = 'maroon',ci.lwd=4,
         names.arg = c('Germany', 'France'), col=c('lightblue','darkkhaki'),
         ylab='평균 기대 수명', main= '유럽내 평균 기대 수명')







