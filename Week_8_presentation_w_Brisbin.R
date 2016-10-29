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

#best description of tapply 
attach(iris)


tapply(iris$Petal.Length,Species,mean)

iris$Petal.Length[1:5]

head(iris)


#Ames Housing Examples---------------------

ames = read.csv("AmesHousing.csv")

attach(ames)

medianAmes= tapply(ames$SalePrice,Land.Slope,median)

medianAmes
# 
# Gtl    Mod    Sev 
# 159950 188000 206975 

# Columns 45 to 54 of AmesHousing.csv all contain quantitative variables. 
# Create a new data frame, ames2, consisting of just columns 45 to 54.
ames2 = ames[ , 45:54]

head(ames2)

# 3*(mean(x) - median(x)) / sd(x) Create a function that measure Pearson Skew


y = c( 1, 1, 2, 10 )

PearsonSkew = function(x){
  3*(mean(x,na.rm=T) - median(x,na.rm=T)) / sd(x,na.rm=T)
} #added na.rm parameter for mean, median, and mode to ignore Null vals
PearsonSkew(y)

# A. Bsmt.Full.Bath 
# B. X1st.Flr.SF 
# C. Bedroom.AbvGr 
# D. Full.Bath

# a coefficient > 0 indicates right skewness, and 
# a coefficient < 0 indicates left skewness.)
PearsonSkew(ames2$Bsmt.Full.Bath) #most right skewed
PearsonSkew(ames2$X1st.Flr.SF)
PearsonSkew(ames2$Bedroom.AbvGr)
PearsonSkew(ames2$Full.Bath) #most left skewed

# Mark and Bernard want to buy a house. The most important factors 
# to them are the neighborhood, the size of the house, and its price. 
# They decide on a scoring system to help them prioritize which houses to consider:

#   1 point per 1000 square feet of Gr.Liv.Area 
# (for example, a 1,500 square foot house would get 1.5 points)

# 3 points for a SalePrice less than $300,000, 
# plus 2 additional points for a SalePrice less than $200,000

# 3 points for a house in the Northridge (NoRidge) neighborhood, 
# or 2 points for a house in the Northridge Heights neighborhood (NridgHt).
#use colnames function to find index numbers for column names 
colnames(ames)

HouseScore<-function(x){
  score = as.numeric(x[48])/1000
  if(as.numeric(x[82]) < 200000){ score = score + 5 }
  else if(as.numeric(x[82]) < 300000){ score = score + 3 }
  if(x[14] == "NoRidge"){ score = score + 3 }
  if(x[14] == "NridgHt"){ score = score + 2 }
  return(score)
} # end of function HouseScore

myScores = apply( ames, 1, HouseScore )
max( myScores )

which.max(myScores)