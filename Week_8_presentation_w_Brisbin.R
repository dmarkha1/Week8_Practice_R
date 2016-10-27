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

