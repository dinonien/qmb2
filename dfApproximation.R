dfApproximation <- function (s1,s2,n1,n2) {
        numerator <-  (s1^2/n1+s2^2/n2)^2
        denominator <- (1/(n1-1)*(s1^2/n1)^2) + (1/(n2-1)*(s2^2/n2)^2)
        return(numerator/denominator)
}