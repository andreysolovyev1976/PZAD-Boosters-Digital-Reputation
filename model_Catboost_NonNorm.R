library(catboost)
library(dplyr)

### ---------------------------------------------------------------------------
### For meta algorithm
### ---------------------------------------------------------------------------
meta_algorithm_train = sample(3711, 1000)
feature_importance = function(models_list){
    feature_importance = c()
    for (model_idx in 1:length(models_list)) {
        temp = 
            catboost.get_feature_importance(models_list[[model_idx]], 
                                            pool = NULL, 
                                            type = 'FeatureImportance',
                                            thread_count = -1)
        
        feature_importance = cbind(feature_importance, temp)
    }
    
    fi = apply(feature_importance, 1, mean, na.rm = TRUE)
    plot(fi, type = "h")
    return (fi)
}
make_CB_models = function(){
    
    result = list()
    
    for (y_index in 1:5){
        
        train_pool <- catboost.load_pool(data=trainData[,6:ncol(trainData)], 
                                         label=trainData[,y_index])
        
        test_pool <- catboost.load_pool(data=testData[,6:ncol(testData)], 
                                        label=testData[,y_index])
        
        model = catboost.train(train_pool,  test_pool,
                               params = list(
                                   loss_function = 'CrossEntropy'
                                   , eval_metric = 'AUC'
                                   # , eval_metric = c('F1', 'AUC')
                                   # , l2_leaf_reg == DEFAULT VALUE
                                   # depth = 7,
                                   # max_leaves = 1000,
                                   , depth = 10
                                   , iterations = 1200
                                   , use_best_model = TRUE
                                   , metric_period = 10)
        )
        
        result[[y_index]] = model
    }#end of for
    
    return (result)
}#end of func

### ---------------------------------------------------------------------------

trainData = cbind.data.frame(trainDataSet[-meta_algorithm_train,1:5], X1[-meta_algorithm_train,2:ncol(X1)])
trainData = trainData[-y_zeros,]

testData  = sample_n(trainData, 750)

trainData = as.matrix(trainData)
testData = as.matrix(testData)


# CB_models_X1_list = list()
# CB_models_X1_list = make_CB_models()

str(CB_models_list)

predictions = c()
for (model_idx in 1:5){
    real_pool <- catboost.load_pool(X1_test[,2:ncol(X1_test)])
    prediction <- catboost.predict(CB_models_X1_list[[model_idx]], real_pool)
    
    predictions = cbind(predictions, prediction)
}

head(predictions)
range(predictions)
###
# predictions_X1_non_norm = predictions

feature_importance_X1 = c()
for (model_idx in 1:5) {
    temp = 
        catboost.get_feature_importance(CB_models_X1_list[[model_idx]], 
                                        pool = NULL, 
                                        type = 'FeatureImportance',
                                        thread_count = -1)
    
    feature_importance_X1 = cbind(feature_importance_X1, temp)
}

fi_X1 = apply(feature_importance_X1, 1, mean, na.rm = TRUE)
plot(fi_X1, type = "h")



### ---------------------------------------------------------------------------
### On X2 NON_NORM
### ---------------------------------------------------------------------------
library(catboost)
library(dplyr)


trainData = cbind.data.frame(trainDataSet[-meta_algorithm_train,1:5], X2_NON_NORM[-meta_algorithm_train,1:ncol(X2_NON_NORM)])
trainData = trainData[-y_zeros,]

testData  = sample_n(trainData, 1000)

trainData = as.matrix(trainData)
testData = as.matrix(testData)


# CB_models_list_X2 = list()
# CB_models_list_X2 = make_CB_models()


predictions = c()
for (model_idx in 1:5){
    real_pool <- catboost.load_pool(X2_test_NON_NORM[,1:ncol(X2_test_NON_NORM)])
    # real_pool <- catboost.load_pool(X2_NON_NORM[,2:ncol(X2_NON_NORM)])
    prediction <- catboost.predict(CB_models_list_X2[[model_idx]], real_pool)
    
    predictions = cbind(predictions, prediction)
}

head(predictions)
range(predictions)

# predictions_X2_non_norm = predictions

feature_importance_X2 = c()
for (model_idx in 1:5) {
    temp = 
        catboost.get_feature_importance(CB_models_list_X2[[model_idx]], 
                                        pool = NULL, 
                                        type = 'FeatureImportance',
                                        thread_count = -1)
    
    feature_importance_X2 = cbind(feature_importance_X2, temp)
}

fi_X2 = apply(feature_importance_X2, 1, mean, na.rm = TRUE)
plot(fi_X2, type = "h")




### ---------------------------------------------------------------------------
# On the most important features
### ---------------------------------------------------------------------------

library(catboost)
library(dplyr)


X1_adj = X1[,-1]
X2_adj = X2_NON_NORM
colnames(X2_adj)[15] = "median_X2"
colnames(X2_adj)[16] = "sd_X2"
colnames(X2_adj)[17] = "size_X2"


trainData = cbind.data.frame(
    X1_adj[,which(fi_X1 > 1)],
    X2_adj[,which(fi_X2 > 3)],
    x3_statistics[,3:4]
    )

trainData = trainData[-y_zeros,]
trainData = cbind.data.frame(trainLabels_, trainData)
trainData = trainData[-meta_algorithm_train,]

testData  = sample_n(trainData, 1500)

trainData = as.matrix(trainData)
testData = as.matrix(testData)


# CB_models_list_IF_X = list()
# CB_models_list_IF_X = make_CB_models()

X1_test_adj =  X1_test[,-1]

X_test_IF = cbind.data.frame(
    X1_test_adj[,which(fi_X1 > 1)],
    X2_test_NON_NORM[,which(fi_X2 > 3)],
    x3_test_statistics[,3:4]
    
)

# here
predictions = c()
for (model_idx in 1:5){
    real_pool <- catboost.load_pool(X_test_IF[,1:ncol(X_test_IF)])
    prediction <- catboost.predict(CB_models_list_IF_X[[model_idx]], real_pool)
    
    predictions = cbind(predictions, prediction)
}

head(predictions)
range(predictions)

# predictions_X_IF_non_norm = predictions

feature_importance_X3 = c()
for (model_idx in 1:5) {
    temp = 
        catboost.get_feature_importance(CB_models_list_IF_X[[model_idx]], 
                                        pool = NULL, 
                                        type = 'FeatureImportance',
                                        thread_count = -1)
    
    feature_importance_X3 = cbind(feature_importance_X3, temp)
}

fi_X3 = apply(feature_importance_X2, 1, mean, na.rm = TRUE)
plot(fi_X3, type = "h")



### ---------------------------------------------------------------------------
# Meta Algorithm
### ---------------------------------------------------------------------------

library(catboost)
library(dplyr)


predictions = c()
for (model_idx in 1:5){
    real_pool <- catboost.load_pool(X1_test[meta_algorithm_train,2:ncol(X1_test)])
    prediction <- catboost.predict(CB_models_X1_list[[model_idx]], real_pool)
    
    predictions = cbind(predictions, prediction)
}

head(predictions)
range(predictions)

# predictions_X1_CB_meta = predictions
###



predictions = c()
for (model_idx in 1:5){
    real_pool <- catboost.load_pool(X2_test_NON_NORM[meta_algorithm_train,2:ncol(X2_test_NON_NORM)])
    prediction <- catboost.predict(CB_models_list_X2[[model_idx]], real_pool)
    
    predictions = cbind(predictions, prediction)
}

head(predictions)
range(predictions)
# predictions_X2_CB_meta = predictions


# 3rd
# 

predictions = c()
for (model_idx in 1:5){
    real_pool <- catboost.load_pool(X_test_IF[meta_algorithm_train,1:ncol(X_test_IF)])
    prediction <- catboost.predict(CB_models_list_IF_X[[model_idx]], real_pool)
    
    predictions = cbind(predictions, prediction)
}

head(predictions)
range(predictions)

# predictions_X_IF_CB_meta = predictions

##############
##############
##############

colnames(predictions_X1_CB_meta) = c("X1_1","X1_2","X1_3","X1_4","X1_5")
colnames(predictions_X2_CB_meta) = c("X2_1","X2_2","X2_3","X2_4","X2_5")
colnames(predictions_X_IF_CB_meta) = c("X3_1","X3_2","X3_3","X3_4","X3_5")


predictions_X1_CB_meta = dataScaleMinMax(predictions_X1_CB_meta, center = FALSE)
predictions_X2_CB_meta  = dataScaleMinMax(predictions_X2_CB_meta, center = FALSE)
predictions_X_IF_CB_meta = dataScaleMinMax(predictions_X_IF_CB_meta, center = FALSE)


trainData = cbind.data.frame(
    predictions_X1_CB_meta,
    predictions_X2_CB_meta,
    predictions_X_IF_CB_meta
)

trainLabels_meta = trainDataSet[meta_algorithm_train,1:5]

trainData = cbind.data.frame(trainLabels_meta, trainData)
testData  = sample_n(trainData, 200)

trainData = as.matrix(trainData)
testData = as.matrix(testData)


# CB_models_list_META = list()
# CB_models_list_META = make_CB_models()

#charting AUC
predictions = c()
for (model_idx in 1:5){
    real_pool <- catboost.load_pool(trainData[,6:ncol(trainData)])
    prediction <- catboost.predict(CB_models_list_META[[model_idx]], real_pool)
    
    predictions = cbind(predictions, prediction)
}

head(predictions)
range(predictions)

# predictions_META_on_Train = predictions



#prediction Test


predictions_X_1_test_CB_meta = dataScaleMinMax(predictions_X1_non_norm, center = FALSE)
predictions_X_2_test_CB_meta = dataScaleMinMax(predictions_X2_non_norm, center = FALSE)
predictions_X_3_test_CB_meta = dataScaleMinMax(predictions_X_IF_non_norm, center = FALSE)

X_test_predictions = cbind(
    predictions_X_1_test_CB_meta,
    predictions_X_2_test_CB_meta,
    predictions_X_3_test_CB_meta
)


predictions = c()
for (model_idx in 1:5){
    real_pool <- catboost.load_pool(X_test_predictions)
    prediction <- catboost.predict(CB_models_list_META[[model_idx]], real_pool)
    
    predictions = cbind(predictions, prediction)
}

head(predictions)
range(predictions)

predictions_META_on_Test = predictions

