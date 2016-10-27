install.packages("matlab")

detach("package:matlab") #unattach after use as it can screw regular functions up.

library(matlab)

magic4 = magic(4)

magic

rowSums(magic4)
apply(magic4, 1, sum) #same as rowSums

apply(magic4, 2, toString)


(baldwins <- data.frame(
  name = c("Alec", "Daniel", "Billy", "Stephen"),
  date_of_birth = c(
    "1958-Apr-03", "1960-Oct-05", "1963-Feb-21", "1966-May-12"
  ),
  n_spouses = c(2, 3, 1, 1),
  n_children = c(1, 5, 3, 2),
  stringsAsFactors = FALSE
))


apply(baldwins,1,toString)

sapply(baldwins,range)



x = matrix(1:6,nr=3)
x

#Week_8_presentation_w_Brisbin 
?apply 
apply(x,1,sum) #row sums (second argument 1 indicates row)
apply(x,2,sum) #column sums (second argument 2 indicates columns)






#diamonds practice set 
#need to use read.table NOT read.csv as it is not a CSV file 
# read.table also does not assume a header add argument (header=T)
mydata = read.table("C:/Users/dmarkham/Code/UWEC/Programming/Week8/Week8_Practice_R/diamonds.txt",header=T)

attach(mydata)

counts = table(Clarity)

mytest = chisq.test(counts) #assumes all categories are equally likely, no need for probability parameter

#Chi-squared test for given probabilities

#data:  counts
# X-squared = 18.2013, df = 4, p-value = 0.001127

#Based on the p-value 0.001127, we can reject the null hypothesis 
# that this data set could be a random sample from a population in 
#which each level of clarity is equally likely. 
pval = mytest$p.value

if(pval < .05){
  print("not plausible")}else{
    print("plausible")
  }


###FUNCTION FOR CHI SQUARE ###
EqualityTest <- function(x){
    #use a chi-square GOF test to test whether x could be a random sample
    #from a population in which all 5 values are equally likely
  counts = table(x)
  mytest = chisq.test(counts) #assumes all categories are equally likely, no need for probability parameter
  pval = mytest$p.value #extracting p-value
  if(pval < .05){print("not plausible")}
  else{print("plausible")
    }
} #end EqualityTest


EqualityTest(Clarity) #without sig level


###FUNCTION FOR CHI SQUARE ###
EqualityTest2 <- function(x, alpha = .05){
  #use a chi-square GOF test to test whether x could be a random sample
  #from a population in which all 5 values are equally likely
  #alpha = significance level
  counts = table(x)
  mytest = chisq.test(counts) #assumes all categories are equally likely, no need for probability parameter
  pval = mytest$p.value #extracting p-value
  if(pval < alpha){print("not plausible")}
  else{print("plausible")
  }
} #end EqualityTest


EqualityTest2(Clarity,.001)
EqualityTest2(Clarity,.01)


ls() #shows all the variables in memory


#Start of WebWork Problems
#----------------------------------------------

#apply
x = matrix( c(1,3,2, 6,NA,4), nr = 3 )
x

# [,1] [,2]
# [1,]    1    6
# [2,]    3   NA
# [3,]    2    4


apply(x, 2, max) #column maxes with NULLs
# ?max

apply(x, 2, max, na.rm=T) #column maxes without NULLS 

apply(x,1,min,na.rm=T)#row maxes 


#sort 

x = matrix( c(1,3,2, 6,NA,4), nr = 3 )

apply(x, 2, sort)


M <- matrix(seq(1,16), 4, 4)

M <- array( seq(32), dim = c(4,4,2))

M

x <- 1:20

y <- factor(rep(letters[1:5], each = 4))

print(factor)

tapply(x, y, sum) 


