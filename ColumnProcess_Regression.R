library(data.table)
library(dplyr)
library(ggplot2)
library(measures)

# 1.13
train <- fread("train_1.csv", stringsAsFactors = T, data.table = F)
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

ggplot(area_sample, aes(x = supply_area, y = transaction_real_price)) + geom_point() + geom_smooth(method = "lm")
ggplot(area_sample, aes(x = exclusive_use_area, y = transaction_real_price)) + 
  geom_point(alpha = .3) + geom_smooth(method = "lm")

# exclusive_use_area -> factor 형식으로 ( ~ <= 60 / 60 < <= 85 / 85 <  <= 135 / 135 <)
cp_train$exclusive_use_area <- cut(cp_train$exclusive_use_area, breaks = c(0, 60, 85, 135, 200),
                                   right = T, labels = c("소형", "중소형", "중대형", "대형"))
