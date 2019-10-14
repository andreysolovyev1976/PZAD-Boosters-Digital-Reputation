#
#transforming that
process_X2 = function(){
    temp_x = X2[2,2]
    X2_modified = data.frame()
    for (i in 2:nrow(X2)) {
        if (X2[i,1] == X2[i-1,1]) {
            temp_x = cbind(temp_x, X2[i,2])  
        } 
        else {
            while (length(temp_x) < 1000) temp_x = cbind(temp_x, 0)  
            row_to_add = c(X2[i-1,1], temp_x)
            X2_modified = rbind(X2_modified, row_to_add)
            temp_x = X2[i,2]
        }
        
        if (i == nrow(X2)) {
            while (length(temp_x) < 1000) temp_x = cbind(temp_x, 0)  
            row_to_add = c(X2[i,1], temp_x)
            X2_modified = rbind(X2_modified, row_to_add)
        }
    }
    
    x2_col_names = c("id")
    for (i in 1:(ncol(X2_modified)-1)) x2_col_names = c(x2_col_names, i)
    colnames(X2_modified) = x2_col_names
    rm(x2_col_names)
}




#making the statistics for each id
statistics_X2 = function(){
    statistics = c()
    for (i in 1:nrow(X2_modified)){
        temp_data = as.numeric(X2_modified[i, which(X2_modified[i,]!=0)])
        temp_data = temp_data[-1]
        temp_data = temp_data[!is.na(temp_data)]
        
        statistics = rbind(statistics, c(
            mean(temp_data, na.rm = TRUE),median(temp_data, na.rm = TRUE), 
            sd(temp_data, na.rm = TRUE), length(temp_data),
            mean(log(temp_data), na.rm = TRUE),median(log(temp_data), na.rm = TRUE), 
            sd(log(temp_data), na.rm = TRUE) 
        ))
    }
    
    rm(temp_data)
    colnames(statistics) = c("mean", "median", "sd", "sample_size",
                             "mean(log)", "median(log)", "sd(log)" )
    return (statistics)
}
x2_statistics = statistics_X2()
nrow(x2_statistics)



#normalizing statistics, collected for each id
x2_statistics_description = dataCharacteristics(x2_statistics)
x2_statistics_normalized = dataScaleMinMax(x2_statistics, FALSE)

head(x2_statistics_normalized)
if (nrow(x2_statistics_normalized) == nrow(X2_modified)) print ("correct number of id's") else print ("CHECK NEEDED!!!")




#pca 
#it is worth of mentioning that normalizing X2 by rows is not a viable idea, 
#is it will produce levels per id, those won't be compared to others, so it 
#will be just a corridor of values, measured by their own volatility

x2.pca = prcomp( X2_modified[,-1], center = TRUE, scale = TRUE)
pca_summary = summary(x2.pca)

str(pca_summary)

screeplot(x2.pca, type = "l", npcs = 25, main = "Screeplot of the first 10 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

cumpro <- cumsum(x2.pca$sdev^2 / sum(x2.pca$sdev^2))
plot(cumpro[0:25], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 20, col="blue", lty=5)
abline(h = 0.95, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC20, 95% of variance explained"),
       col=c("blue"), lty=5, cex=0.6)


x2_pca_variables = x2.pca$x[,1:20]
head(x2_pca_variables)
if (nrow(x2_pca_variables) == nrow(X2_modified)) print ("correct number of id's") else print ("CHECK NEEDED!!!")





#pca 
#it is worth of mentioning that normalizing X2 by rows is not a viable idea, 
#is it will produce levels per id, those won't be compared to others, so it 
#will be just a corridor of values, measured by their own volatility

x2.pca_NON_NORM = prcomp( X2_modified[,-1], center = FALSE, scale = FALSE)
pca_summary = summary(x2.pca_NON_NORM)

str(pca_summary)

screeplot(x2.pca_NON_NORM, type = "l", npcs = 15, main = "Screeplot of the first 15 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

cumpro <- cumsum(x2.pca_NON_NORM$sdev^2 / sum(x2.pca_NON_NORM$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 13, col="blue", lty=5)
abline(h = 0.95, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC13, 95% of variance explained"),
       col=c("blue"), lty=5, cex=0.6)


x2_pca_variables_NON_NORM = x2.pca_NON_NORM$x[,1:13]
head(x2_pca_variables_NON_NORM)
if (nrow(x2_pca_variables_NON_NORM) == nrow(X2_modified)) print ("correct number of id's") else print ("CHECK NEEDED!!!")


X2_NON_NORM = cbind.data.frame(x2_pca_variables_NON_NORM, x2_statistics[,1:4])
