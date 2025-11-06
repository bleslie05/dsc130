library(tidyverse)
library(tidymodels)

?cells

set.seed(123)
cell_split <- initial_split(cells %>% select(-case), 
                            strata = class)

cell_train <- training(cell_split)
cell_test <- testing(cell_split)


mod <- nearest_neighbor(neighbors = 5) %>% 
  set_engine("kknn") %>% 
  set_mode("classification")

fit <- 
  mod %>% 
  fit(class ~ ., data = cell_train)


training_pred <- fit %>%  predict(cell_train, type = "prob")
train_results <- bind_cols(training_pred, cell_train %>% 
              select(class))


train_results %>%                # training set predictions
 roc_auc(truth = class, .pred_PS)

  
  test_pred <- fit %>%  predict(cell_test, type = "prob")
  test_results <- bind_cols(test_pred, cell_test %>% 
                               select(class))
  
  test_results %>%                # training set predictions
    roc_auc(truth = class, .pred_PS)
  
  
  test_pred %>%                   # test set predictions
    roc_auc(truth = class, .pred_PS)
  
  
  ### CV
  
  
  cv_splits <- vfold_cv(cell_train, v = 5)
  
  cv_splits

  
  cv_fits <- fit_resamples(mod, class ~ ., cv_splits)
  
  
  cv_fits %>% collect_metrics()


  
  
  
  
  

  