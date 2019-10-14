

length(which(is.na(x1_normalized)))
length(which(is.na(x2_statistics_normalized)))
length(which(is.na(x2_pca_variables)))
length(which(is.na(x3_statistics_normalized)))


length(which(is.infinite(as.matrix(x1_normalized))))
length(which(is.infinite(as.matrix(x2_statistics_normalized))))
length(which(is.infinite(x2_pca_variables)))
length(which(is.infinite(x3_statistics_normalized)))


# x2_nans = which(is.na(x2_statistics_normalized))%%nrow(x2_statistics_normalized)
# 
# for (i in 1:ncol(x2_statistics_normalized))
#     for (j in x2_nans)
#         if (is.na(x2_statistics_normalized[j,i])) x2_statistics_normalized[j,i] = 0.0



head(x1_normalized)

dataSet = cbind(x1_normalized, x2_statistics_normalized, x2_pca_variables, x3_statistics_normalized)
class(dataSet)
nrow(dataSet)
ncol(dataSet)
head(dataSet)



trainDataSet = cbind(Y, dataSet[,-1])
colnames(trainDataSet)[2] = "Y1"
colnames(trainDataSet)[3] = "Y2"
colnames(trainDataSet)[4] = "Y3"
colnames(trainDataSet)[5] = "Y4"
colnames(trainDataSet)[6] = "Y5"

head(trainDataSet)

colnames(trainDataSet) = c("id", 
                           "Y1","Y2","Y3","Y4","Y5",
                           "X1","X2","X3","X10","X11","X12","X13","X14","X15","X16","X17","X18","X19","X20","X21","X22","X23","X24","X4","X5","X6","X7","X8","X9",
                           "x2_mean", "x2_median","x2_sd","x2_sample_size","x2_mean_log","x2_median_log","x2_sd_log","PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","PC11","PC12","PC13","PC14","PC15","PC16","PC17","PC18","PC19","PC20",
                           "x3_mean","x3_sd","x3_sample_size")
trainDataSet = trainDataSet[,-1]



trainDataSet_clean = trainDataSet[-y_zeros,]
length(y_zeros)
nrow(trainDataSet_clean)


cols_to_delete = 34:57
trainDataSet_clean = trainDataSet_clean[,-cols_to_delete]
head(trainDataSet_clean)

