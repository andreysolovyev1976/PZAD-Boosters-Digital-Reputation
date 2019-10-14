
dataCheck = function (data, byColumns = TRUE){
    #data check!!!
    
    if (byColumns) row_cols = 2 else row_cols = 1
    
    col_sdData_ = apply(data, row_cols, sd, na.rm = TRUE)
    data_toRemove_ = which(col_sdData_ == 0)
    if (length(data_toRemove_) != 0) data = data[,-data_toRemove_]
    
    return (data)
} 

dataScaleMinMax = function (data, center, byColumns = TRUE){

    if (byColumns) row_cols = 2 else row_cols = 1
    
    #check the data there is no sd == 0
    #making output container
    datanorm_ = dataCheck(data)

    if (center) {    
        # ## nuts and bolts
        col_meanData = apply(datanorm_, row_cols, mean, na.rm = TRUE)
        # #centering data
        for (a in 1:ncol(datanorm_)) datanorm_[ ,a] = data.temp[ ,a] - col_meanData[a]
    }

    ## nuts and bolts
    col_maxmin_ = c()
    col_minData = apply(datanorm_, row_cols, min, na.rm = TRUE)
    col_maxData = apply(datanorm_, row_cols, max, na.rm = TRUE)

    #making min - mac vector
    for (a in 1:ncol(datanorm_)) 
        col_maxmin_ = c(     
            col_maxmin_, 
            (-col_minData[a] / (col_maxData[a] - col_minData[a]) ) 
        )
    
    #scaling data on min-max
    for (a in 1:ncol(datanorm_)) 
        datanorm_[ ,a] = datanorm_[ ,a] / (col_maxData[a] - col_minData[a]) + col_maxmin_[a]
    

    #--------------------------------------
    
    for (a in 1:ncol(datanorm_)) print(range(datanorm_[,a], na.rm = TRUE))
    
    #--------------------------------------
    
    return (datanorm_)
} 

dataScaleSD = function (data, byColumns = TRUE){

    if (byColumns) row_cols = 2 else row_cols = 1
        
    #check the data there is no sd == 0
    data = dataCheck(data)
    
    ## nuts and bolts
    col_sdData = apply(data, row_cols, sd)
    
    #output container
    datanorm_ = data
    
    #centering data
    for (a in 1:ncol(data))   datanorm_[ ,a] = data[ ,a] - col_meanData[a]
    
    #scaling data on sd
    for (a in 1:ncol(data)) 
        datanorm_[ ,a] = datanorm_[ ,a] / col_sdData[a]
    
    #--------------------------------------
    
    for (a in 1:ncol(datanorm_)) print(range(datanorm_[,a]))
    
    #--------------------------------------
    
    return (datanorm_)
} 

dataCharacteristics = function (data, byColumns = TRUE){
    
    if (byColumns) row_cols = 2 else row_cols = 1
    
    #check the data there is no sd == 0
    data = dataCheck(data)
        
    ## nuts and bolts
    col_maxmin_ = c()
    col_minData = apply(data, row_cols, min, na.rm = TRUE)
    col_maxData = apply(data, row_cols, max, na.rm = TRUE)
    col_meanData = apply(data, row_cols, mean, na.rm = TRUE)
    col_sdData = apply(data, row_cols, sd, na.rm = TRUE)
    
    #data check!!!
    for (a in 1:ncol(data)) 
        col_maxmin_   = c(     
            col_maxmin_, 
            (-col_minData[a] / (col_maxData[a] - col_minData[a]) ) 
        )
    
    output = data.frame(col_minData, col_maxData, col_meanData, col_maxmin_, col_sdData)
    colnames(output) = c("min","max","mean","maxmin","sd")
    return (output)
} 


