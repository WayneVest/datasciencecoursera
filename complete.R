complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    # Create a matrix to hold results
    cc <- matrix(nrow = length(id), ncol = 2, dimnames = list(1:length(id), c("id", "nobs")))
    
    # Create a var to hold filenames
    nums <- numeric()
    
    # Loop over all the ids, creating the required filename, reading in the required file, counting completed.cases and updating the matrix
    for(i in 1:length(id)){
        # Fix up the filename
        if(id[i] < 100 && id[i] > 9){
            nums[i] <- paste("0", id[i], sep = "")
        } else if(id[i] < 10) {
            nums[i] <- paste("00", id[i], sep = "")
        } else {
            nums[i] <- as.character(id[i])
        }
        
        # Read in the file
        data <- read.csv(file.path(directory, c(paste(nums[i], ".csv", sep = ""))))
        
        # Update the matrix
        cc[i,1] <- id[i]
        cc[i,2] <- nrow(data[complete.cases(data),])
    }
    
    # After all the looping, return the final matrix
    as.data.frame(cc)
}