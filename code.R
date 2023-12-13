##### Config #####

library(mlr3verse)
library(tidyverse)

data = read_csv("day.csv")

ind_test = sample(data$instant, round(nrow(data)/5))
ind_val  = sample(setdiff(data$instant, ind_test), round((nrow(data) - length(ind_test))/5))
data_test_raw  = data[ind_test,  ]
data_val_raw   = data[ind_val,   ]
data_train_raw = data[-c(ind_test, ind_val), ]

pre_process = function(data, test = F) {
  if (!test) {
    data %>% 
      rowwise() %>% 
      mutate(count_so_far = list(1:cnt), 
             label = list(c(rep(0, cnt-1), 1))) %>% 
      unnest() %>% 
      select(-dteday, -casual, -registered)
  } else {
    
    unfold = function (ele_) {
      if(ele_ > 4000) {rep(0, 4000)}
      else {c(rep(0, ele_ - 1), rep(1, 4001 - ele_))}
    }
    
    data %>% 
      rowwise() %>% 
      mutate(count_so_far = list(1:4000), 
             label = list(unfold(cnt))
             ) %>% unnest() %>% 
      select(-dteday, -casual, -registered)
  }
}

pre_of = function(vec_) {
  pmin(1-exp(-Reduce(`+`, vec_, accumulate = T)), 1)
}

crps_of = function(truth_, response_, id_) {
  
  samples_ = unique(id_)
  days_ = length(truth_)/length(samples_)
  
  truth_    = unlist(map(samples_, ~ pmin(cumsum(truth_[id_ == .x]), 1)))
  response_ = unlist(map(samples_, ~ pmin(cumsum(response_[id_ == .x]), 1)))
  
  sum((truth_ - response_)**2) / (length(samples_) * days_)
}

##### ReadinAndProcess #####

data_test  = pre_process(data_test_raw, test = T)
data_val   = pre_process(data_val_raw)
data_train = pre_process(data_train_raw)

actual_val = bind_rows(data_train, data_val)

task_train = as_task_regr(bind_rows(data_train, actual_val)[, -1], "label")
task_test  = as_task_regr(data_test[, -1], "label")

task_train$set_row_roles(1:nrow(actual_val) + nrow(data_train), "test")

##### Train #####

learner = lrn("regr.lightgbm")
learner$param_set$set_values(
  num_iterations = 1000, 
  objective = "poisson",
  num_leaves = 30, 
  learning_rate =  0.001,
  feature_fraction =  0.8,
  bagging_fraction =  0.9,
  bagging_seed =  33,
  poisson_max_delta_step =  0.8,
  metric = "poisson", 
  early_stopping = T, 
  early_stopping_rounds = 400
)

learner$train(task_train)

##### PredictAndEval #####

prediction = learner$predict(task_test)

result = prediction |> 
  as.data.table() |> 
  mutate(id = data_test$instant) |> 
  group_by(id) |> 
  mutate(cdf = pre_of(response), 
         days = 1:4000)

plot_of = function(id_) {
  result |> 
    filter(id == id_) |> 
    ggplot() +
    geom_line(aes(x = days, y = cdf)) +
    geom_line(aes(x = days, y = truth))
}

plot_of(659)

map(unique(filter(result, truth == 1)$id)[1:10], plot_of)  

##### cal_crps #####

with(result, crps_of(truth, response, id))