#---------------------------------------------------------------------------#
# Statistics Project 3, Question 3
#---------------------------------------------------------------------------#

sample_3 <- function(filename, conf_level){   
        # get the data
        data <- as.numeric(scan(filename, what = "", sep = "\t"))
        
        # get the number of data points (n)
        n <- length(data)
        print(paste("Sample Size:", n))
        
        # get the value of alpha
        alpha <- (1 - (conf_level/100))
        print(paste("Alpha:", alpha))
        
        # get the value of t
        t <- abs(qt(alpha/2, n - 1))
        print(paste("t value:", t))
        
        # print std dev and mean
        print(paste("Sample Mean:", mean(data)))
        print(paste("Standard Deviation:", sd(data)))
        
        # calculate the confidence interval
        CI_a <- round((mean(data) - (t * (sd(data)/sqrt(n)))), digits = 2)
        CI_b <- round((mean(data) + (t * (sd(data)/sqrt(n)))), digits = 2)
        print(paste("Confidence Interval:[", CI_a, ",", CI_b, "]"))
}
