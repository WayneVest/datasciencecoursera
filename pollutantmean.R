pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    # Sort filenames
    nums <- numeric()
    for(i in 1:length(id)){
        if(id[i] < 100 && id[i] > 9){
            nums[i] <- paste("0", id[i], sep = "")
        } else if(id[i] < 10) {
            nums[i] <- paste("00", id[i], sep = "")
        } else {
            nums[i] <- as.character(id[i])
        }
    }
    
    files <- file.path(directory, c(paste(nums, ".csv", sep = "")))
    
    # Read in raw data
    raw_data <- sapply(files, read.csv)
    
    # Extract pollutant data and reformat
    sub_data <- unlist(raw_data[pollutant, ])
    
    # Clean for NA values, calculate mean & return
    mean(sub_data[!is.na(sub_data)])
}