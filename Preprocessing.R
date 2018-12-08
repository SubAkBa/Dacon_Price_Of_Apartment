# train.csv ----
# key : primary key
# apartment_id : 아파트 ID
# city : Seoul - 1, Busan - 0
# transaction_year_month : 실거래가 발생 년월 - yyyymm
# transaction_date : 실거래가 발생일 - dd ~ dd
# year_of_completion : 아파트 준공 년도, 입주년
# exclusive_use_area : 전용면적(현관문을 열고 들어가는 가족들의 전용 생활공간,
#                               발코니는 전용면적에서 제외)
#                      60㎡이하(소형) 60㎡초과 85㎡이하(중소형) 85㎡초과 
#                      135㎡이하(중대형) 135㎡초과 (대형)별로 구분 -> 실거래가 조사
# floor : 층
# latitude : 위도
# longitude : 경도
# address_by_law : 법정동코드(앞2자리 : 시/도, 앞5자리 : 시/군/구, 앞8자리 : 읍/면/동,
#                             앞10자리 : 시도(2) + 시군구(3) + 읍면동(3) + 리(2))
# total_parking_capacity_in_site : 아파트 단지내 총 주차대수
# total_household_count_in_sites : 아파트 단지내 총 세대수
# apartment_building_count_in_sites : 아파트 단지내 총 동수
# tallest_building_in_sites : 아파트 단지내 최고층
# lowest_building_in_sites : 아파트 단지내 최저층 동의 층수
# heat_type : 난방방식 (개별난방[individual], 중앙난방[central], 지역난방[district])
# heat_fuel : 난방연료 (도시가스[gas], 열병합[cogeneration])
# room_id : 평형 ID(평형 = (전용면적m^2 + 주거공용면적m^2) * 0.3025)
#                  (1평형 = 3.3058m^2)
# supply_area : 공급면적(= 전용면적 + 주거공용면적)
#                        주거공용면적 - 아파트 등 공동주택에 거주하는 
#                                       다른 세대와 공동으로 사용하는 공간(계단, 복도, ...)
# total_household_count_of_area_type : 평형의 세대수
# room_count : 방 수
# bathroom_count : 욕실 수
# front_door_structure : 현관구조(복도식[corridor], 계단식[stairway], 복합식[mixed])
# transaction_real_price : 실거래가(단위 : 만원)

# Subways.csv ----
# station_id : 지하철역 ID
# latitude : 위도
# longitude : 경도
# subway_line : 지하철 노선(서울 : 1 ~ 9[1호선 ~ 9호선], KJ[경의중앙선],
#                           US[우의신경전철], AP[공항철도], KC[경춘선],
#                           DL[분당선], ND[신분당선],
#                           부산 : B1 ~ B4[1호선 ~ 4호선], BD[동해선], BK[부산김해경전철],
#                           복수일 경우 : 환승역)
# address_by_law : 법정동코드(앞2자리 : 시/도, 앞5자리 : 시/군/구, 앞8자리 : 읍/면/동)

# Schools.csv ----
# school_code : 학교 코드
# latitude : 위도
# longitude : 경도
# school_class : 초등학교[elementary school], 중학교[middle school], 고등학교[high school]
# operation_type : 운영주체(국립[national], 공립[public], 사립[private])
# highschool_type : 고등학교 구분(일반고등학교[general], 자율고등학교[autonomous],
#                                 특성화고등학교[specialized], 특수목적고등학교[objective])
# gender : 남학교[male], 여학교[female], 남녀공[both = coeducation]
# foundation_date : 설립일
# address_by_law : 법정동코드(앞2자리 : 시/도, 앞5자리 : 시/군/구, 앞8자리 : 읍/면/동)

# Start ----
library(ggplot2)
library(dplyr)
library(Hmisc)
library(lubridate)
library(foreach)
library(data.table)

# 데이터 읽어 오기
submisson <- read.csv("submission.csv")
test <- read.csv("test.csv")
train <- fread("train.csv", stringsAsFactors = T, data.table = F)
school <- read.csv("Schools.csv")
subway <- read.csv("Subways.csv")
cp_train <- train
options(scipen = 1)

# address_by_law를 통해서 서울 / 부산 알 수 있으므로 city column 제거
cp_train$city <- NULL

# 실거래가 발생 날짜가 가격에 영향을 미치는지 확인
# 날짜와 가격 이상치와 결측치 확인하기
attach(cp_train)
describe(transaction_year) # NA 없음
describe(transaction_month) # NA 없음
describe(transaction_date) # NA 없음
table(is.na(transaction_real_price)) # NA 없음
summary(transaction_real_price) # 가장 높은 곳 : 용산구 한남동 1117013100
detach(cp_train)
cp_train %>% filter(transaction_real_price == max(transaction_real_price))
# 날짜를 합쳐서 하나의 변수로 통합
cp_train <- cp_train %>% mutate(transaction_fulldate =
                                  paste(transaction_year, transaction_month, 
                                        transaction_date, sep = "-"))
cp_train$transaction_year_month <- NULL
cp_train$transaction_date <- NULL
train_date_price <- cp_train %>% group_by(transaction_fulldate) %>% 
  summarise(price = mean(transaction_real_price), n = n())
train_date_price$transaction_fulldate <- as.factor(train_date_price$transaction_fulldate)
plot(train_date_price$transaction_fulldate, train_date_price$price)
# 시간 지날수록 가격이 올라가나 18년도에서 급격하게 감소 (데이터수가 얼마없기때문이라고 판단)
tail(train_date_price)


# 준공년도와 실거래가 발생 날짜와의 관계 파악
describe(cp_train$year_of_completion) # NA 없음
cp_train$transaction_fulldate <- as.factor(cp_train$transaction_fulldate)
cp_train$year_of_completion <- as.factor(cp_train$year_of_completion)
cp_train %>% group_by(year_of_completion, transaction_fulldate) %>% 
  summarise(n = n()) %>% View() # fulldate로는 갯수가 고루 분포되어있음.
train_year_year <- cp_train %>% group_by(year_of_completion, year(transaction_fulldate)) %>% 
  summarise(n = n()) # 연도로 그룹화
mosaicplot(year_of_completion ~ `year(transaction_fulldate)`, data = train_year_year)
View(train_year_year)가
train_year_count <- train_year_year %>% group_by(year_of_completion) %>% 
  summarise(n = sum(n)) # 준공년도에 따른 아파트의 갯수
plot(train_year_count$year_of_completion, train_year_count$n) # 1990년대부터 급상승 후 2008년대부터 급하강
head(cp_train)


# 준공년도와 실거래가 관계 파악
train_year_price <- cp_train %>% group_by(year_of_completion) %>% 
  summarise(price = mean(transaction_real_price), n = n()) 
plot(train_year_price$year_of_completion, train_year_price$price) # 이상치 발견
train_year_price[which(train_year_price$price == max(train_year_price$price)), ] # 1973년도
boxplot(transaction_real_price ~ year_of_completion, data = cp_train) # boxplot을 통해 분포 파악
View(train_year_price)


# Same apartment type (테스트 중)
uniquedeletefunction <- function(dataframe){
  
  deletecolumn <- foreach(i = 1 : dim(dataframe)[2], .combine = c) %do% {
    if(length(unique(dataframe[, i])) == 1){
      return (i)
    }
  }
  dataframe[, deletcolumn] <- NULL
  
  return (dataframe)
}
cp_train$city <- NULL
apart_5584 <- cp_train %>% filter(apartment_id == 5584) # bathroom_count에 따라 가격이 달라짐
apart_5584 <- uniquedeletefunction(apart_5584)
head(apart_5584, 10)

apart_5584 %>% arrange(exclusive_use_area, floor) %>% head(10) # 실거래가 날짜 영향을 미침
describe(apart_5584)


apart_2816 <- cp_train %>% filter(apartment_id == 2816) # bathroom_count에 따라 가격이 달라짐
apart_2816 <- uniquedeletefunction(apart_2816)

describe(apart_2816)
head(apart_2816, 10)
length(unique(apart_2816[, 23]))


# 12.08
cp_train$city <- NULL
cp_train$year <- substr(cp_train$transaction_year_month, 1, 4)
cp_train$month <- substr(cp_train$transaction_year_month, 5, 6)
cp_train$transaction_year_month <- NULL
table(cp_train$front_door_structure)
cp_train[cp_train == ""] <- NA
table(cp_train$front_door_structure, useNA = "always")
train_front_bar <- cp_train %>% filter(front_door_structure == "-")
cp_train %>% filter(apartment_id == "6198") %>% select(front_door_structure) %>% table(useNA = "always")
cp_train %>% filter(apartment_id == "6198") %>% arrange(front_door_structure) %>% tail(10)
cp_train[which(cp_train$apartment_id == "6198" & cp_train$front_door_structure %in% c(NA, "-")), 
         "front_door_structure"] <- "corridor" # 5개를 제외하고 모두 corridor. 
cp_train %>% filter(apartment_id == "34724") %>% nrow() # 해당 아파트는 전부 front_door_structure가
                                                        # '-'로 입력되어있다. 삭제
cp_train <- cp_train %>% filter(front_door_structure != "-")
cp_train$front_door_structure <- as.factor(cp_train$front_door_structure)
boxplot(transaction_real_price ~ front_door_structure, data = cp_train, notch = T) # 안그려진다.
table(cp_train$front_door_structure, useNA = "always")
head(cp_train)
str(cp_train)
describe(cp_train)

# Subway - 보통 역세권이면 가격이 높다.
# train과 Subway를 조인할 수 있는 방법 : 경도, 위도 / 법정동코드
describe(subway) # NA 9개 모두 address_by_law에 위치
subway_na <- subway[which(is.na(subway$address_by_law)), ]
# address_by_law를 채워넣기(경도, 위도를 찾아서)
cp_train <- cp_train %>% arrange(desc(latitude), longitude)
subway_na <- subway_na %>% arrange(desc(latitude), longitude)
summary(cp_train$longitude)
i <- 1
subway_train <- foreach(j = 1 : dim(cp_train)[1], .combine = rbind) %do% { # 채워넣어줘야 함
  if(i == (dim(subway_na)[1] + 1))
    break;
  if(cp_train[j, "latitude"] == subway_na[i, "latitude"] &
     cp_train[j, "longitude"] == subway_na[i, "longitude"]){
    i <<- i + 1
    return (cp_train[j, ])
  }
}
