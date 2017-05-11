#***************************************************************************************************************************************
#' @author Kirkwood Donavin
# Comments:																*		*		*		*		*		*		*		*
#***************************************************************************************************************************************

#' @name chaos
#' @details a function to demonstrate chaos, how small variations in starting values produce
#' large variation in interated results, using a non-linear equation. This function was inspired
#' by John Gribbins explanation of chaos theory in 'Deep Simplicity', p. 61.
#' @param x a single numeric value between -1 and 1.
#' @param n number of iterations.
chaos <- function(x, n = 10){
    if(x > 1 | x < -1){
        print("Error: 'x' must be between -1 and 1")
    } else{
        rtn = NULL
        for(i in 1:n){
            x = 2*x^2 -1
            rtn = c(rtn,x)
        }
        return(rtn)
    }
}

#' @details increment takes an integer object or a numeric object and increments the value of that object.
#' The object will be of type 'integer' after increment() is executed, which may cause precision loss in
#' a numeric object.
#' @param int_obj - an integer object or numeric object
#' @param env - the environment in which int_obj lives.
increment<-function(int_obj, env = parent.frame()){
    if(!is.integer(int_obj) & !is.numeric(int_obj)){
        print("increment() works with integers or numeric only.")
        return()
    }
    obj_name<-deparse(substitute(int_obj))
    value<-as.integer(get(obj_name, envir = env))
    assign(obj_name, as.integer(value + 1), envir = env)
}

#' @name in.na
#' @details is 'x' in 'y'?: Returning missing elements for missing elements in 'x'.
#' @param x vector with elements to check for in y. NA values are returned in kind.
#' @param y vector of elements that x's elements are compared to.
#' @return a logical vector, who's elements are true for elements of x that are in y,
#' false for other non-NA elements in x, and NA for NA elements in x.
in.na <- function(x, y){
    z <- logical(length(x))
    for(i in 1:length(x)){
        if(is.na(x[i])){
            z[i]<- NA
        } else if(x[i] %in% y){
            z[i] <- TRUE
        } else{
            z[i] <- FALSE
        }
    }
    return(z)
}

#' @title The Birthday Problem
#' @details calculates the probability of at least a pair of folks in a room sharing a birthday, given a number of people, 'people'.
#' @param people number of folks in the room.
#' @return probability of at least two folks in the room sharing a birthday.
birth_prob<-function(people){
  prob <-1
  for(i in 1:people-1){
    prob <- prob*(365-i)/365
  }
  return(1-prob)
}
  # sample size converge
    sample_size_converge<-function(n,diff,MSE,lin_combo=c(1,1),alpha=0.05,power=0.8,groups=1, terminate=50){
      t_a <- qt(p=1-alpha/2,df=n*groups-groups)
      t_b <- qt(p=power,df=n*groups-groups)
      n_star<-ceiling((t_a + t_b)^2*MSE*sum(lin_combo^2)/diff^2)
      if(n_star==n){
        return(c(n_star,terminate,paste(n_star," - returned after `terminate' - ",terminate,sep="")))
      } else if(terminate==0){
        return(c(n_star,"ERROR: Sample size did not converge"))
      } else {
        terminate=terminate-1
        sample_size_converge(n=n_star,diff=diff,MSE=MSE,lin_combo=lin_combo,alpha=alpha,power=power,groups=groups,terminate=terminate)
      }
    }
  # s_p
    sp<-function(x,group,variance=FALSE){
      numerator<-as.numeric(0)
      denominator<-as.numeric(-length(levels(group)))
      for(val in levels(group)){
        assign("n",length(x[as.character(group)==val]))
        assign("v",var(x[as.character(group)==val]))
        term<-(n-1)*v
        numerator<-sum(term,numerator)
        denominator<-sum(denominator,n)
      }
      if(variance==FALSE){
        return(sqrt(numerator/denominator))
      } else if(variance==TRUE){
        return(numerator/denominator)
      }
    }

  # Stolen functions
    bmatrix = function(x, digits=NULL, ...) {
      library(xtable)
      default_args = list(include.colnames=FALSE, only.contents=TRUE,
                          include.rownames=FALSE, hline.after=NULL, comment=FALSE,
                          print.results=FALSE)
      passed_args = list(...)
      calling_args = c(list(x=xtable(x, digits=digits)),
                       c(passed_args,
                         default_args[setdiff(names(default_args), names(passed_args))]))
      cat("\\begin{bmatrix}\n",
          do.call(print.xtable, calling_args),
          "\\end{bmatrix}\n")
    }
