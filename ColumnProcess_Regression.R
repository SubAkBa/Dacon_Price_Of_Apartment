library(data.table)
library(dplyr)
library(ggplot2)
library(measures)

# 1.13
train <- fread("train_1.csv", stringsAsFactors = T, data.table = F)
subway <- read.csv("Subways_rmNA.csv")
cp_train <- train

# date에 따라서 가격이 다를까?
cp_train %>% ggplot(aes(x = transaction_date, y = transaction_real_price)) + 
  geom_boxplot() + scale_y_log10() # 큰차이가 없다. 날짜 분류를 3분류로 나누기(월말을 합치기)
levels(cp_train$transaction_date)[3 : 6] <- "21~"
# factor, character 형변환

# test / train 나눈 후 회귀 돌리기
sample_idx <- cp_train %>% sample_frac(0.7) %>% rownames() %>% as.numeric()
train_of_train <- cp_train[sample_idx, ]
test_of_train <- cp_train[-sample_idx, ]
reg_model1 <- lm(transaction_real_price ~ front_door_structure + bathroom_count + room_count, data = train_of_train)
summary(reg_model1)
reg_model1_pred <- predict(reg_model1, newdata = test_of_train)
RMSE(reg_model1_pred, test_of_train$transaction_real_price) * 10000 # 293,760,531

# supply_area : 공급면적 / exclusive_use_area : 전용면적
cp_train$transaction_real_price <- cp_train$transaction_real_price / 10000
area_sample <- cp_train %>% sample_n(10000)

ggplot(area_sample, aes(x = supply_area, y = transaction_real_price)) + 
  geom_point(alpha = .3) + geom_smooth(method = "lm")
ggplot(area_sample, aes(x = exclusive_use_area, y = transaction_real_price)) + 
  geom_point(alpha = .3) + geom_smooth(method = "lm")

# exclusive_use_area -> factor 형식으로 ( ~ <= 60 / 60 < <= 85 / 85 <  <= 135 / 135 <)
cp_train$exclusive_use_area <- cut(cp_train$exclusive_use_area, 
                                   breaks = c(0, 60, 85, 135, max(cp_train$exclusive_use_area)),
                                   right = T, labels = c("소형", "중소형", "중대형", "대형"))
ggplot(cp_train, aes(x = exclusive_use_area, y = transaction_real_price)) + geom_boxplot() + scale_y_log10()

# latitude, longitude, exclusive_use_area
position_seoul_sample <- cp_train %>% filter(latitude > 37) %>% sample_n(100000)
position_busan_sample <- cp_train %>% filter(latitude < 37) %>% sample_n(100000)
ggplot(position_seoul_sample, aes(x = latitude, y = longitude, colour = exclusive_use_area)) + geom_point()
ggplot(position_busan_sample, aes(x = latitude, y = longitude, colour = exclusive_use_area)) + geom_point()

# address_by_law 바탕으로 근처 역의 갯수
cp_subway <- subway
subway_count <- cp_subway %>% group_by(address_by_law) %>% summarise(n = n())
cp_train <- merge(cp_train, as.data.frame(subway_count), by = "address_by_law", all.x = T)

# latitude, longtitude 값차이를 통해 학교와 역 숫자
# latitude(위도)간 거리 : 1분 * 1.853km(1853m), 1초 * 30.887m
# longitude(경도)간 거리 : 1분 * 1.480km(1480m), 1초 * 24.668m
cp_train[1, c("latitude", "longitude")] - subway[1, c("latitude", "longitude")]
cp_train[1, c("latitude", "longitude")] - subway[2, c("latitude", "longitude")]
test_longitude <- round(abs(cp_train$longitude - subway$longitude[1]), 2)

test_latitude <- data.frame(lati_diff = round(abs(cp_train$latitude - subway$latitude[1]), 2))
test_latitude <- test_latitude %>% mutate(lati_boon = floor(lati_diff),
                                          lati_cho = lati_diff - lati_boon,
                                          lati_dist_m = round(lati_boon * 1853 + lati_cho * 30.887))
View(test_latitude)
table(test_latitude$lati_dist_m)

Dist_Function <- function(data1, data2){
  dist_m <- matrix(nrow = nrow(data1), ncol = nrow(data2))
  
  for(i in 1 : nrow(data2)){
    dist <- data.frame(lati_diff = round(abs(cp_train$latitude - subway$latitude[i]), 2),
                       long_diff = round(abs(cp_train$longitude - subway$longitude[i]), 2))
    dist <- dist %>% mutate(lati_boon = floor(lati_diff),
                            lati_cho = lati_diff - lati_boon,
                            lati_dist_m = round(lati_boon * 1853 + lati_cho * 30.887),
                            long_boon = floor(long_diff),
                            long_cho = long_diff - long_boon,
                            long_dist_m = round(long_boon * 1480 + long_cho * 24.668),
                            dist_result = sqrt(lati_dist_m^2 + long_dist_m^2),
                            dist_result_factor = dist_result %/% 100)
    dist_m[, i] <- dist$dist_result_factor
    print(paste0(i, "번째"))
    rm(dist); gc(reset = T)
  }
  
  return (dist_m)
}
test_dist <- Dist_Function(cp_train[, c("latitude", "longitude")], subway[, c("latitude", "longitude")])
