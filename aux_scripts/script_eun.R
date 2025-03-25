library(tourr)
library(PPtreeExt)
library(PPtreeViz)
A = list.files("./input/aux")
set.seed(143)
keepALL = NULL
for(i in c(1:9,11:23,25:55,58:60,62:66,68,70:83)){
  print(i)
  
  DATA = read.csv(here::here("./input/aux",A[i]))  
  DATA$Y = as.character(DATA$Y)
  TT = table(DATA$Y)
  DATA = DATA %>% filter(Y %in% names(TT)[which(TT>5)])
  DATA$Y = factor(DATA$Y)
  groupID = unique(DATA$Y)
  train_id = NULL
  for(g in groupID){
    sel = which(DATA$Y==g)
    train_id <- c(train_id,
                  sample(sel, round(length(sel)*.7)))
  }
  train <- data.frame(DATA[train_id, ])
  test <- data.frame(DATA[-train_id, ])
  #MOD1
  m_mod1 <- PPTreeclass_MOD(Y~., data = train, PPmethod="LDA")
  
  pred_mod1 <- PPclassify_MOD(m_mod1, test.data = test[,-1], true.class =
                                test[,1])
  
  #pred_mod1$predict.error/nrow(test)
  
  
  
  #MOD2 ERROR
  m_mod2 <- PPtree_splitMOD(Y~., data = train, PPmethod = "PDA", entro =
                              TRUE, entroindiv = FALSE)
  
  pred_mod2 <- predict(m_mod2, newdata = test[,-1])
  
  # Mod3 
  
  m_mod3 <- PPTreeclass_MOD(Y~., data = train, PPmethod = "PDA", entro =
                              FALSE, entroindiv = TRUE)
  
  pred_mod3 <- PPclassify_MOD(m_mod3, test.data = test[, names(test) != "Y"], true.class = test$Y, Rule = 1)
  
  
  #MODoriginal
  
  m1_org <- PPTreeclass(Y~., data = train, PPmethod = "LDA")
  
  pred_org <-predict(m1_org, newdata= test[,-1])
  
  
  
  keepALL = rbind(keepALL,c(A[i],mean(pred_mod1[[2]] != test$Y),
                            mean(as.numeric(as.character(pred_mod2)) != 
                                   as.numeric(as.factor(test$Y))),
                            mean(pred_org != test$Y), mean(pred_mod3$predict.class != test$Y)))
}

write.csv(keepALL, "test-result-2024-07-04.csv")