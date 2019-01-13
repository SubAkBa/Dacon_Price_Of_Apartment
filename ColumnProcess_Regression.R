library(data.table)
library(dplyr)
library(ggplot2)

# 1.13
train <- fread("train_1.csv", stringsAsFactors = T, data.table = F)
cp_train <- train
gc(reset = T)
# date에 따라서 가격이 다를까?
cp_train %>% ggplot(aes(x = transaction_date, y = transaction_real_price)) + 
  geom_boxplot() + scale_y_log10() # 큰차이가 없다. 날짜 분류를 3분류로 나누기(월말을 합치기)
levels(cp_train$transaction_date)[3 : 6] <- "21~"
# factor, character 형변환

# test / train 나눈 후 회귀 돌리기
sample_idx <- cp_train %>% sample_frac(0.7) %>% rownames() %>% as.numeric()
train_of_train <- cp_train[sample_idx, ]
test_of_train <- cp_train[-sample_idx, ]
