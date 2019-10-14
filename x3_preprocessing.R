
library(ggplot2)


liners = function (vector_1) {
    
    vector_1 = X3[7,]
    length(which(vector_1 != 0))
    
    # create data
    xValue = 1:(length(vector_1)-1)
    yValue = as.numeric(vector_1[-1])
    data = data.frame(xValue,yValue)
    
    # Plot
    g = ggplot(data, aes(x=xValue, y=yValue)) +
        geom_point( color="#69b3a2", size=0.9, alpha=0.9) +
        theme_ipsum(
        ) +
        ggtitle("Evolution of something")+
        xlab("observation") + 
        ylab("value") 
    
    g
    
    
    # 
    # g = ggplot(to_ggplot, 
    #            aes(
    #                x=to_ggplot[,1], 
    #                y=to_ggplot[,2], 
    #                colour = to_ggplot[,1])
    # ) + 
    #     geom_point(
    #         size=2, 
    #         color="steelblue",
    #         fill="#69b3a2",
    #         shape=20,
    #         alpha=0.5,
    #         stroke = 1
    #     ) +
    #     geom_jitter(
    #         #position = "jitter", 
    #         width = 0.5, height = 0.5,
    #         colour = "steelblue", 
    #         inherit.aes = TRUE) +
    #     
    #     # ggtitle("Mean and Sample Size") +
    #     
    #     #    geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)+
    #     theme(
    #         axis.text = element_text( 
    #             angle = 0, 
    #             color="darkblue", 
    #             size=10, 
    #             face=1)
    #         , axis.title = element_text( color="darkblue", size=10, face=2)
    #         , legend.position = "none"
    #         # , plot.title=element_text( hjust=1, vjust=0.5, face='bold')
    #         
    #     ) +
    #     xlab(name_1) + 
    #     ylab(name_2) 
    
    return (g)
}

non_empty_qty = c()
max_numbers = 0
for (single_row in 1:nrow(X3)) non_empty_qty = rbind(non_empty_qty, length(which(X3[single_row,] != 0)))


non_empty_qty_2 = c()
for (single_row in 1:nrow(X2_modified)) non_empty_qty_2 = rbind(non_empty_qty_2, length(which(X2_modified[single_row,] != 0)))

plot(density(non_empty_qty))
lines(density(non_empty_qty_2))

plot(non_empty_qty_2[300:500], type = "l", lty = 1)
lines(non_empty_qty[300:500], type = "l", lty = 1, col = "steelblue")

cor(non_empty_qty, non_empty_qty_2, use = "pairwise.complete.obs", method = "pearson")


which(non_empty_qty == 190)
which(non_empty_qty_2 == 1001)

g = pair_scatters(log(non_empty_qty), log(non_empty_qty_2), "x3", "x2")
g


# which(sd(col[X3]) ==0 )
# X23 X266 X410 X426 
# 23  266  410  426 

# which(x3_stat_rows ==0 )
# [1]  125  160  785 1163 1730 1912 2214 2331 2677 2878 2923 3035 3121 3180 3220 3382 3504 3872 3918 3966 3995


x3_stat_cols = apply(X3[,-1], 2, sd, na.rm = TRUE)
x3_stat_rows = apply(X3[,-1], 1, sd, na.rm = TRUE)

length(which(x3_stat_cols ==0 ))
length(which(x3_stat_rows ==0 ))


x2_stat_cols = apply(X2_modified[,-1], 2, sd, na.rm = TRUE)
x2_stat_rows = apply(X2_modified[,-1], 1, sd, na.rm = TRUE)

length(which(x2_stat_cols ==0 ))
length(which(x2_stat_rows ==0 ))


X3_modified = X3
for (i in 2:ncol(X3_modified)) 
    if (sd(X3_modified[,i]) == 0) X3_modified = X3_modified[,-i]



x3_sum_columns = apply(X3_modified, 2, sum, na.rm = TRUE)
x3_sum_rows = apply(X3_modified[,-1], 1, sum, na.rm = TRUE)

plot(x3_sum_rows)


statistics_X3 = function(){
    statistics = c()
    for (i in 1:nrow(X3_modified)){
        temp_data = as.numeric(X3_modified[i,])
        temp_data = temp_data[-1]
        temp_data = temp_data[!is.na(temp_data)]
        
        statistics = rbind(statistics, c(
            mean(temp_data, na.rm = TRUE),median(temp_data, na.rm = TRUE), 
            sd(temp_data, na.rm = TRUE), length(which(temp_data !=0 ))
        ))
    }
    
    rm(temp_data)
    colnames(statistics) = c("mean", "median", "sd", "sample_size")
    return (statistics)
}
x3_statistics = statistics_X3()
nrow(x3_statistics)

x3_statistics_description = dataCharacteristics(x3_statistics)
x3_statistics_normalized = dataScaleMinMax(x3_statistics, FALSE)

head(x3_statistics_normalized)

to_ggplot = data.frame(x3_statistics_normalized)

p <- ggplot(to_ggplot, 
            aes(
                x=to_ggplot[,2], 
                y=to_ggplot[,3], 
                colour = to_ggplot[,3]),
            size=to_ggplot[,3]) +
    geom_point() +
    # geom_jitter(
    #     #position = "jitter", 
    #     width = 0.1, height = 0.1,
    #     colour = "steelblue", 
    #     inherit.aes = TRUE) +
    theme_ipsum(
    )+
        
    # theme(
    #     axis.text = element_text( 
    #         angle = 0, 
    #         color="steelblue", 
    #         size=10, 
    #         face=1),
    #     axis.title = element_text( color="darkblue", size=10, face=2),
    #     legend.position = "none"
    #     # legend.position = c(.95, .95),
    #     # legend.justification = c("right", "top"),
    #     # legend.box.just = "right",
    #     # legend.margin = margin(6, 6, 6, 6),
    #     # legend.key = element_rect(fill = "white", colour = "black"),
    #     # legend.text = element_text(size = 8, colour = "red"),
    #     # legend.title = element_text(face = "bold")
    # ) +
    xlab("sd") + 
    ylab("sample size") +  

p
p5 <- ggMarginal(p, type="densigram")

p5

rm(data, g, p3, single_row, my_colors, xValue, yValue, c.ccf, statistics, p, p5, vector_1)


x3.pca = prcomp( X3_modified[,-1], center = TRUE, scale = TRUE)
x3_pca_summary = summary(x3.pca)

x3_pca_summary

screeplot(x3.pca, type = "l", npcs = 25, main = "Screeplot of the first 25 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

cumpro <- cumsum(x3.pca$sdev^2 / sum(x3.pca$sdev^2))
plot(cumpro[0:200], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot X3_train")
# abline(v = 20, col="blue", lty=5)
# abline(h = 0.95, col="blue", lty=5)
legend("topleft", legend=c("95% of variance explained - unachievable"),
       col=c("blue"), lty=5, cex=0.6)


