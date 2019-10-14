x1_feature_type = c("id",  "cat", "cat", "cat","real","real", "real", "real", "real", "real", 
                    "cat", "cat", "cat", "cat","cat",  "cat",  "cat",  "cat",  "cat",  "cat",
                    "cat","cat","cat","cat","cat", "cat")


x1_statistics = dataCharacteristics(X1)
head(x1_statistics)

x1_real_variables_normalized = dataScaleMinMax(X1[,which(x1_feature_type == "real")], FALSE)
x1_normalized = cbind(X1$id, X1[,which(x1_feature_type == "cat")], x1_real_variables_normalized)

head(x1_normalized)


