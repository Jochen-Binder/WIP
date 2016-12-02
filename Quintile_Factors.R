######################################################################################
# CREATE A FACTOR WITH N LEVELS BASED ON THE N-TILES OF A METRIC VARIABLE
######################################################################################

N_TILES <- function(data, variables, n.split = 2){

    ###########
    # 
    k <- n.split
    df <- data.frame(data[ , c(variables)])
    
    names(df) <- paste(n.split,"-tiles_",names(df), sep = "")
    
    
    ############################################
    # Function for cutting vector in n quantiles

    APPLYQUINTILES <- function(x) {
        
        b <- c(unique(quantile(x, probs = seq(0, 1, 1/k), na.rm=T)))
        l <- c(1:(length(b)-1))
        
        return(cut(x, breaks = b, include.lowest = T, labels = l))
    }
    
    ##################
    # 
    return(cbind(data, apply(df, 2, APPLYQUINTILES)))
}