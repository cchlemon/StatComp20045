
#' @importFrom Rcpp evalCpp
#' @useDynLib StatComp20045


#' @title  a function to compute probability
#' @name dice
#' @description when there are n dices and all number of the dices are k,what is the probability?
#' @param n there are n dices
#' @param k all number of the dices are k
#' @return a probability
#' @examples
#' \dontrun{
#' dice(5,18)}
#' @export
dice<-function(n,k){
  x<-matrix(numeric((k+1)*(k+1)),ncol = k+1,nrow = k+1,byrow = TRUE)
  j<- 0
  while(j<=k){
    for(i in 0:n){
      if(6*i+j+n==k){x[i+1,j+1]<-(-1)^i*choose(n,i)*choose(n-1+j,n-1)/(6^n)}
    }
    j<-j+1
  }
  y<-sum(apply(x,1,sum))
  return(y)
}


#' @title  sensitive question
#' @name sq
#' @description a function to compute the pribability of sensitive question
#' @param N investigate N persons
#' @param n there are n persons to say yes among N persons
#' @param p the probability of saying yes about a easy and public question
#' @param m1 the number of white balls
#' @param m2 the number of black balls
#' @return a probability
#' @examples
#' \dontrun{
#' sq(1583,389,0.5,20,30)
#' }
#' @export
sq<-function(N,n,p,m1,m2){
  y1<- n/N
  y2<- m1*p/(m1+m2)
  y3<- m2/(m1+m2)
  y<- (y1-y2)/y3
  return(y)
}