#---------------------------------------------------------------------------#
# Statistics Project 3, Question 1
# Team Members:
        # 1. Sudarshan
        # 2. Sri Hari Priya
        # 3. Keerthana
        # 4. Prathyusha
#---------------------------------------------------------------------------#

sample_1 <- function(filename, std_dev, conf_level){
        
        # get the data
        data <- as.numeric(scan(filename, what = "", sep = "\t"))
        
        # get the number of data points (n)
        n <- length(data)
        print(paste("Sample Size:", n))
        
        # get the value of alpha
        alpha <- (1 - (conf_level/100))
        print(paste("Alpha:", alpha))
        
        # get the Z value
        z <- abs(qnorm(alpha/2))
        # print(paste("Standard Norman Quantile:", z))
        
        # print std dev and mean
        print(paste("Sample Mean:", mean(data)))
        print(paste("Standard Deviation:", std_dev))
        
        # calculate the confidence interval
        CI_a <- round((mean(data) - (z * (std_dev/sqrt(n)))), digits = 2)
        CI_b <- round((mean(data) + (z * (std_dev/sqrt(n)))), digits = 2)
        print(paste("Confidence Interval:[", CI_a, ",", CI_b, "]"))
}