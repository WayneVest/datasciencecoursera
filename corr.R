corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    # Determine the number of files in the specified directory
    num_files <- length(list.files(directory))
    
    # Loop through files, checking the volume of complete.cases in each, and record if above threshold
    # Create numeric vector to hold results
    cases <- numeric()
    
    # Loop through files checking volume of completed.cases
    for(i in 1:num_files){
        var <- complete(directory = directory, id = i)
        if(var$nobs >= threshold){
            
            # Format file to be read in
            id <- if(i < 10){paste("00", i, sep="")} else if(i < 100){paste("0", i, sep="")} else {as.character(i)}
            
            # Read in file
            data <- read.csv(file.path(directory, paste(id, ".csv", sep="")))
            
            # Do correlation only clena data only
            cases <- c(cases, cor(data[,"sulfate"], data[,"nitrate"], use = 'na.or.complete'))
        }
    }
    
    # Return
    cases
}