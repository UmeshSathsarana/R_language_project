install.packages("dplyr")
install.packages("DescTools")
install.packages("ggplot2")
install.packages("modeest")




View(EduStates_New)
library(Rcmdr)
library(ggplot2)
 
#Task 3

#--------min value-----------

min_value(EduStates_New$dollars)
print(min_value)

#----------max value----------
max_value(EduStates_New$dollars)
print(max_value)

#-------mean Value-----------

mean_value(EduStates_New$dollars)
print(mean_value)

#-----------median Value---------------

median_value(EduStates_New$dollars)
print(median_value)

#---------------mode------------


mode_value(EduStates_New$dollars)
print(mode_value)



#Task 4 out summary statistics of state spending on public education per student

summary(EduStates_New$dollars)


# -----------------Task 5--------------------

#----SAT exam--------

mean_percent = mean(EduStates_New$percent, na.rm = TRUE)
print(mean_percent)

median_percent = median(EduStates_New$percent, na.rm = TRUE)
print(median_percent)

mode_percent = as.numeric(names(sort(table(EduStates_New$percent), decreasing = TRUE)[1]))
print(mode_percent)


#histogram for Percent
hist(EduStates_New$percent, prob = TRUE, main = "percent distribution",xlab = "percent", ylab = "Density")
curve(dnorm(x,mean = mean(EduStates_New$percent) , sd = sd(EduStates_New$percent)),add = TRUE,col = "blue")

hist(EduStates_New$percent, prob = TRUE, main = "Percent Distribution", xlab = "Percent", ylab = "Density", na.rm = TRUE)
curve(dnorm(x, mean = mean(EduStates_New$percent, na.rm = TRUE), sd = sd(EduStates_New$percent, na.rm = TRUE)), add = TRUE, col = "blue",na.rm = TRUE)

#----------education per student--------------------

mean_dollars = mean(EduStates_New$dollars, na.rm = TRUE)
print(mean_dollars)

median_dollars = median(EduStates_New$dollars, na.rm = TRUE)
print(median_dollars)

mode_dollars = as.numeric(names(sort(table(EduStates_New$dollars), decreasing = TRUE)[1]))
print(mode_dollars)

#------bell curve------------

hist(EduStates_New$dollars, prob = TRUE, main = "Dollars distribution",xlab = "dollares", ylab = "Density")
curve(dnorm(x,mean = mean(EduStates_New$dollars) , sd = sd(EduStates_New$dollars)),add = TRUE,col = "blue")



#----average teacher's salary------

mean_pay = mean(EduStates_New$pay, na.rm = TRUE)
print(mean_pay)

median_pay = median(EduStates_New$pop, na.rm = TRUE)
print(median_pay)

mode_pay = as.numeric(names(sort(table(EduStates_New$pop), decreasing = TRUE)[1]))
print(mode_pay)


#----------bell curve
hist(EduStates_New$pay, prob = TRUE, main = "pay distribution",xlab = "pay", ylab = "Density")
curve(dnorm(x,mean = mean(EduStates_New$pay) , sd = sd(EduStates_New$pay)),add = TRUE,col = "blue")


#---------------population----------

mean_pop = mean(EduStates_New$pop, na.rm = TRUE)
print(mean_pop)

median_pop = median(EduStates_New$pop, na.rm = TRUE)
print(median_pop)

mode_pop = as.numeric(names(sort(table(EduStates_New$pop), decreasing = TRUE)[1]))
print(mode_pop)

#----------bell curve-------

hist(EduStates_New$pop, prob = TRUE, main = "pop distribution",xlab = "pop", ylab = "Density")
curve(dnorm(x,mean = mean(EduStates_New$pop) , sd = sd(EduStates_New$pop)),add = TRUE,col = "blue")




#----------task 6--------------------
#Took the R commder


#----------------task 7-------------------------

#----------SAT exam----------

#Anderson-Darling normality test

ad.test(EduStates_New$percent)

#Lilliefors (Kolmogorov-Smirnov) normality test

lillie.test(EduStates_New$percent)

#Shapiro-Wilk normality test

shapiro.test(EduStates_New$percent)


#---state spending on public education per student---

 #Anderson-Darling normality test

ad.test(EduStates_New$dollars)

#Lilliefors (Kolmogorov-Smirnov) normality test

lillie.test(EduStates_New$dollars)

#Shapiro-Wilk normality test

shapiro.test(EduStates_New$dollars)


#-----------correlation Test------------------
cor.test(EduStates_New$percent, EduStates_New$dollars, method = "pearson")




#-------------task 8-----------------------

#-----SAT Participation Rate-----

#Anderson-Darling normality test

ad.test(EduStates_New$percent)


#Lilliefors (Kolmogorov-Smirnov) normality test 
lillie.test(EduStates_New$percent)


#Shapiro-Wilk normality test
shapiro.test(EduStates_New$percent)



#-----------Average teacher's salary in the state-----------------------

#---Anderson-Darling normality test----- 
ad.test(EduStates_New$pay)


#------Lilliefors (Kolmogorov-Smirnov) normality test----- 
lillie.test(EduStates_New$pay)


#------Shapiro-Wilk normality test--------- 
shapiro.test(EduStates_New$pay)



# ------Pearson's product-moment correlation-------

cor.test(EduStates_New$percent, EduStates_New$pay, method = "pearson")





#-----------Task 9-----------

#Anderson-Darling normality test for SAT Participation Rate
ad.test(EduStates_New$percent)


#Lilliefors (Kolmogorov-Smirnov) normality test SAT Participation Rate
lillie.test(EduStates_New$percent)


#Shapiro-Wilk normality test SAT Participation Rate
shapiro.test(EduStates_New$percent)


#--------population-------------- 
#Anderson-Darling normality test 
ad.test(EduStates_New$pop)


#Lilliefors (Kolmogorov-Smirnov) normality test
lillie.test(EduStates_New$pop)


#Shapiro-Wilk normality test 
shapiro.test(EduStates_New$pop)




#-------correlation Test---------

cor.test(EduStates_New$percent, EduStates_New$pop, method = "pearson")


