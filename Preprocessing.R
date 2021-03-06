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
#                      135㎡이하(중대형) 135㎡초과 (대형)별로 구분 -> 실거래가 조사 / 국민평수
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
#             url = https://angelahyejin.blog.me/221052837874
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

rm(list = ls()); gc(reset = T)
# Start ----
library(caret)
library(plyr)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(lubridate)
library(foreach)
library(data.table)
library(fastDummies)

# 데이터 읽어 오기
submission <- read.csv("submission.csv")
test <- read.csv("test.csv")
train <- fread("train.csv", stringsAsFactors = T, data.table = F)
school <- read.csv("Schools.csv")
subway <- read.csv("Subways_rmNA.csv")
cp_train <- train
options(scipen = 10)

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


# 12.08 ----
cp_train$city <- NULL
# divide year - month
cp_train$transaction_year <- substr(cp_train$transaction_year_month, 1, 4)
cp_train$transaction_month <- substr(cp_train$transaction_year_month, 5, 6)
cp_train$transaction_year_month <- NULL
colnames(cp_train)
cp_train <- cp_train[, c(1, 2, 24, 25, 3 : 23)]
table(cp_train$front_door_structure)
table(is.na(cp_train))
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
# 구글맵 이용 url : https://www.google.co.kr/maps/
# subway_na(순서대로) - id 300 도봉역(서울특별시 도봉구 도봉동) : 1132010800, 
#                       id 135 망우역(서울특별시 중랑구 상봉동) : 1126010200, 
#                       id 324 개화산역(서울특별시 강서구 방화동) : 1150010900, 
#                       id 6   동대문역(서울특별시 종로구 창신동) : 1111017400,
#                       id 342 청구역(서울특별시 중구 신당동) : 1114016200,
#                       id 115 대방역(서울특별시 영등포구 신길동) : 1156013200, 
#                       id 413 장승배기역(서울특별시 동작구 상도동) : 1159010200, 
#                       id 32  교대역(서울특별시 서초구 서초동) : 1165010800, 
#                       id 624 남산역(부산광역시 금정구 남산동) : 2641010400
subway_na$address_by_law <- c(1132010800, 1126010200, 1150010900, 1111017400, 
                              1114016200, 1156013200, 1159010200, 1165010800, 2641010400)
str(subway_na)
subway_na[, c(2 : 4)] <- NULL
subway_na <- subway_na %>% arrange(station_id)
j <- 1
for(i in 1 : dim(subway)[1]){
  if(subway_na[j, "station_id"] == subway[i, "station_id"]){
    subway[i, "address_by_law"] <- subway_na[j, "address_by_law"]
    j <<- j + 1
  }
  if(j == (nrow(subway_na) + 1))
    break
}
describe(subway)
write.csv(subway, "Subways_rmNA.csv", row.names = F)


# 12.15
head(cp_train)
# latitude, longitude 사용하지 않고 address_by_law 사용
cp_train$latitude <- NULL
cp_train$longitude <- NULL
cp_train[cp_train == ""] <- NA
colnames(cp_train)
describe(cp_train)
xtabs(~ as.factor(heat_fuel) + as.factor(heat_type), data = cp_train)
# bathroom / room count
table(which(is.na(cp_train$bathroom_count)) == which(is.na(cp_train$room_count))) # NA자리 동일
cp_train$transaction_real_price <- cp_train$transaction_real_price / 10000 # 크기가 커서 만원단위로
str(cp_train)
table(cp_train$room_count)
table(cp_train$bathroom_count)
room_price_train <- cp_train[-which(is.na(cp_train$room_count)), ]
nrow(room_price_train)
tapply(room_price_train$transaction_real_price,
       as.factor(room_price_train$room_count), summary)
boxplot(transaction_real_price ~ as.factor(room_count), data = room_price_train)
                                                       # 방 갯수가 늘어날수록 가격이 높아진다.
boxplot(transaction_real_price ~ as.factor(bathroom_count), data = room_price_train)
                                                       # 화장실 갯수가 늘어날수록 가격이 높아진다.
                                                       # NA들 삭제
train2 <- room_price_train
describe(train2)

# lowest / tallest
table(which(is.na(train2$lowest_building_in_sites)) 
      == which(is.na(train2$tallest_building_in_sites))) # NA자리 동일
na_low_tal_train <- train2[which(is.na(train2$lowest_building_in_sites)), ]
                    # heat_type, fuel도 NA, apartmentid : 36339
train2 %>% filter(apartment_id == 36339) %>% head()
                  # apartment_id : 36339인 아파트는 모두 NA, 1121510400 / 서울특별시 광진구 광장동
train2 %>% filter(address_by_law == 1121510400) %>% nrow() # 6950개, 총 데이터 갯수가 적지 않아 삭제
train2 <- train2[-which(is.na(train2$lowest_building_in_sites)), ]
describe(train2)

# convert character to factor
train2 <- train2 %>% mutate_if(sapply(train2, is.character), as.factor)
str(train2)

# front_door_structure
train3 <- train2
table(train3$front_door_structure)
train3 %>% filter(front_door_structure == "-") # apartment_id   : 34724 - 20개, 6198 - 1개
                                               # address_by_law : 2617010200  , 1156011000
                                               # 부산광역시 동구 수정동, 서울특별시 영등포구 여의도동
train3 %>% filter(apartment_id == 34724) # 이 아파트를 통해 알 수 있는것
                                         # transaction_year, month, date, floor를 제외하고 모두 동일.
train3 %>% filter(apartment_id == 34724) %>% 
  select(transaction_year, transaction_month, transaction_date,
         floor, transaction_real_price) %>% arrange(desc(transaction_real_price),
                                                    desc(transaction_year),
                                                    desc(transaction_month), desc(transaction_date))
                                        # 결과로 봤을때 transaction_year, month, date는 
                                        # 가격에 영향을 미친다.(데이터가 적은 것 고려해야함.)
                                        # why?) ① 시간이 지날수록 돈의 가치가 하락.
                                        #       ② 아파트 주변에 집세가 오를만한 조건들이 생성.
train3 %>% filter(address_by_law == 2617010200) %>% nrow() # 226개
train3 %>% filter(address_by_law == 2617010200) %>% select(apartment_id) %>% table()
                                                    # 11100 : 184, 16725 : 22, 34724 : 20
boxplot(transaction_real_price ~ front_door_structure, data = train3)
                                 # count : mixed < corridor < stairway
                                 #         26085     415191    1146041
                                 # corridor : 복도식(70 ~ 80년대 대규모로 지어진 주공아파트들)
                                 #            오피스텔은 중복도식
                                 # stairway : 승강기를 중심으로 마주보는 현관구조
                                 # mixed : 같은 층의 가구 이상이 승강기를 중심으로 배치.
                                 #         타워형 아파트 설계 시 채택.
summary(transaction_real_price ~ front_door_structure, data = train3) # NA가 있다. 결측치 처리
door_na_train <- train3 %>% filter(is.na(front_door_structure))
head(door_na_train)
nrow(door_na_train)
describe(door_na_train)
plot(door_na_train$room_id) # 특정 구간들에 몰려있다.
plot(door_na_train$apartment_id)
head(unique(door_na_train$apartment_id)) # 5863 4047 4331 4321 2720 12188
train3 %>% filter(apartment_id == 5863) %>% nrow() # 176개
all_5863 <- train3 %>% filter(apartment_id == 5863)
describe(all_5863) # 5863 -> address_by_law = 1130510100 서울특별시 강북구 미아동
View(all_5863) # apartmend_id = 5863중 room_id = 93250인 것들만 front_door_structure = NA
               # room_id = 93250을 가진 다른 아파트를 찾아보자.
door_na_train %>% filter(apartment_id == 5863) %>% nrow() # 73개
onlyna_5863 <- door_na_train %>% filter(apartment_id == 5863)
View(onlyna_5863)
train3 %>% filter(room_id == 93250) %>% nrow() # 위의 아파트를 제외하고 없다. 73개
                                               # 동일한 apartment_id내 다른 
                                               # front_door_structure를 가질수 있나?
train3 %>% group_by(apartment_id, room_id, front_door_structure) %>% 
  dplyr::summarise(n = n()) %>% View() # 가질 수 있다.
                                       # 다른 apartment_id를 가지면서 동일한 room_id를 가질 수 있나?
train3 %>% group_by(apartment_id, room_id) %>% dplyr::summarise(n = n()) %>% View()
                                       # 가지지 않는다.
                                       # front_door_structure 영향을 미치는 변수이며
                                       # 결측치를 채울 수 있는 방법이 없다고 판단되어 삭제
train4 <- train3 %>% filter(!is.na(front_door_structure))
describe(train4) # 결측치 있는 column : total_parking_capacity_in_site / heat_type / heat_fuel
type_na_train <- train4 %>% filter(is.na(heat_type))
describe(type_na_train)
head(type_na_train) # apartment_id = 33185
all_33185 <- train4 %>% filter(apartment_id == 33185)
View(all_33185)
describe(all_33185) # apartment_id = 33185는 heat_type과 heat_fuel이 모두 NA
                    # 일단 결측치를 모두 제거한 데이터를 가지고 회귀분석을 돌려보자.
nona_train <- train4 %>% filter(!is.na(heat_fuel))
nona_train <- nona_train %>% filter(!is.na(total_parking_capacity_in_site))
table(is.na(nona_train))

# Regression
set.seed(137)
str(nona_train)
nona_train$address_by_law <- as.factor(nona_train$address_by_law)
final_train <- nona_train[, c(2 : 9, 15, 16, 18, 20, 21, 22, 23)]
final_train$room_count <- as.integer(final_train$room_count)
final_train$bathroom_count <- as.integer(final_train$bathroom_count)
final_train$transaction_real_price <- as.integer(final_train$transaction_real_price)
final_train$address_by_law <- NULL
str(final_train)
datas <- dummy_cols(final_train, select_columns = c("transaction_year", "transaction_month", 
                                           "transaction_date", "heat_type", "heat_fuel", 
                                           "front_door_structure"),
                    remove_first_dummy = T)
str(datas)
datas[, c(2 : 4, 8, 9, 13)] <- NULL
datas <- datas %>% filter(`heat_fuel_-` == 0, `front_door_structure_-` == 0)
colnames(datas)
sample_idx <- createDataPartition(datas$apartment_id, p = 0.3)$Resample1
apart_train <- datas[-sample_idx, ]
apart_test <- datas[sample_idx, ]
str(apart_train)
rm(onlyna, onlyna_5863, type_na_train, sample_idx, all, all_33185, all_5863, crime,
   door_na_train, room_price_train, train, train2, train3, train4, cp_train, 
   final_train, nona_train); gc(reset = T)
reg_model <- lm(transaction_real_price ~ -apartment_id + ., data = apart_train)
summary(reg_model)
reg_pre <- predict(reg_model, newdata = apart_train)
fitted(reg_model)
(reg_rmse <- RMSE(reg_pre, apart_train$transaction_real_price)) # 23940.65 * 10000
                                                  # 다음에 해야 될 것.
                                                  # 1. 처음부터 다시 돌아가서 결측치 및 이상치 처리
                                                  # 2. School과 Subway 데이터 이용하기
                                                  # 3. column 선정

# 12.26 / 1.11
# 결측치와 이상치 다루기
# 1. 결측치 다루기 (subway 데이터는 위에서 처리 완료)
head(cp_train)
summary(cp_train)
cp_train$transaction_real_price <- cp_train$transaction_real_price / 10000 # 만원단위로 변경
cp_train$city <- NULL # address_by_law column으로 충분히 구분 가능하다.

# (1) tallest_building_in_sites / lowest_building_in_sites
which(is.na(cp_train$tallest_building_in_sites)) == which(is.na(cp_train$lowest_building_in_sites))
sites_train <- cp_train[which(is.na(cp_train$tallest_building_in_sites)), ]
cp_train <- cp_train[which(!is.na(cp_train$tallest_building_in_sites)), ]

# (2) room_count / bathroom_count
table(which(is.na(cp_train$room_count)) == which(is.na(cp_train$bathroom_count)))
cp_train <- cp_train[which(!is.na(cp_train$room_count)), ]

# (3) heat_type / heat_fuel
table(cp_train$heat_type)
table(cp_train$heat_fuel)
xtabs(~ heat_type + heat_fuel, data = cp_train)
which(cp_train$heat_type == "")
xtabs(~ heat_type + year_of_completion, data = cp_train) # 준공년도가 빠를 수록 중앙집중형?
type_comple <- cp_train %>% group_by(year_of_completion, heat_type) %>% summarise(n = n())
ggplot(type_comple, aes(x = year_of_completion, y = n, colour = heat_type)) + geom_line(cex = 1.5)
                            # central은 거의 없다. individual은 갈수록 커진다.
cp_train <- cp_train %>% filter(heat_type != "")
cp_train <- cp_train %>% filter(!heat_fuel %in% c("", "-"))
cp_train$heat_type <- factor(cp_train$heat_type)
cp_train$heat_fuel <- factor(cp_train$heat_fuel)

# (4) front_door_structure
cp_train <- cp_train %>% filter(!front_door_structure %in% c("", "-"))
cp_train$front_door_structure <- factor(cp_train$front_door_structure)

# (5) total_parking_capacity_in_site
cp_train <- cp_train %>% filter(!is.na(total_parking_capacity_in_site))
summary(cp_train)

# 2. 이상치 다루기
area_type <- boxplot(cp_train$total_household_count_of_area_type)
max(cp_train$total_household_count_of_area_type)
train_2960 <- cp_train %>% filter(total_household_count_of_area_type == 2960) # 2960이 다른것들보다 높다. 이상치?
describe(train_2960) # 날짜와 층, 가격만 다르다. 다른것은 모두 동일 / 2638010600 : 부산광역시 사하구 다대동
train_2960 <- train_2960 %>% arrange(transaction_real_price)
View(train_2960) # 이상치 No
write.csv(cp_train, "train_1.csv", row.names = F)