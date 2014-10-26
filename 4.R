#---------------------------------------------------------------------------#
# Statistics Project 3, Question 4
# Team Members:
        # 1. Sudarshan
        # 2. Sri Hari Priya
        # 3. Keerthana
        # 4. Prathyusha
#---------------------------------------------------------------------------#

sample_4 <- function(file1, file2, conf_level){
        
        # get the data
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
        
        # get the degrees of freedom
        v = round(((((sd(data1)^2)/n) + ((sd(data2)^2)/m))^2) / (((sd(data1)^4)/((n^2) * (n-1))) + ((sd(data2)^4)/((m^2) * (m-1)))))
        print(paste("Degrees of Freedom:", v))
        
        # get the value of t
        t <- abs(qt(alpha/2, v))
        print(paste("t value:", t))
        
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
        CI_a <- round((mean1 - mean2) - (t * sqrt((((sd(data1)^2)/n) + ((sd(data2)^2)/m)))), digits = 2)
        CI_b <- round((mean1 - mean2) + (t * sqrt((((sd(data1)^2)/n) + ((sd(data2)^2)/m)))), digits = 2)
        print(paste("Confidence Interval:[", CI_a, ",", CI_b, "]"))
}