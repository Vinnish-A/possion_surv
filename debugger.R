library(tidyverse)

cal_crps = function(truth_, response_, id_) {
  
  samples_ = unique(id_)
  days_ = length(truth_)/length(samples_)
  
  truth_    = unlist(map(samples_, ~ pmin(cumsum(truth_[id_ == .x]), 1)))
  response_ = unlist(map(samples_, ~ pmin(cumsum(response_[id_ == .x]), 1)))
  
  sum((truth_ - response_)**2) / (length(samples_) * days_)
}


a = (1:6) / 8
b = seq(13, 3, -2) / 14
id = rep(c(1, 2), each = 3)
cal_crps(a, b, id)
