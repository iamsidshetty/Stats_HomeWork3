#---------------------------------------------------------------------------#
# Statistics Project 3, Question 2
#---------------------------------------------------------------------------#

sample_2 <- function(file1, file2, conf_level){
        
        # get the data from the files
        data1 <- as.numeric(scan(file1, what = "", sep = "\t"))
        data2 <- as.numeric(scan(file2, what = "", sep = "\t"))
        
        # get the number of data points
        n <- length(data1)
        m <- length(data2)
        print(paste("Sample Size of sample 1:", n))
        print(paste("Sample Size of sample 2:", m))
        
        
        # get the value of alpha
        alpha <- (1 - (conf_level/100))
        print(paste("Alpha:", alpha))
        
        # get the Z value
        z <- abs(qnorm(alpha/2))
        
        # get the mean of the for data
        mean1 <- mean(data1)
        mean2 <- mean(data2)
        
        # print std dev and mean
        print(paste("Sample Mean of sample 1:", mean1))
        print(paste("Sample Mean of sample 2:", mean2))
        print(paste("Standard Deviation of sample 1:", sd(data1)))
        print(paste("Standard Deviation of sample 2:", sd(data2)))
        print(paste("Difference of Sample Mean:", mean1 - mean2))
        
        # get the confidence interval
        CI_a <- round((mean1 - mean2) - (z * sqrt((((sd(data1)^2)/n) + ((sd(data2)^2)/m)))), digits = 2)
        CI_b <- round((mean1 - mean2) + (z * sqrt((((sd(data1)^2)/n) + ((sd(data2)^2)/m)))), digits = 2)
        print(paste("Confidence Interval:[", CI_a, ",", CI_b, "]"))
}
