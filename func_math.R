#***************************************************************************************************************************************
# Author: Kirkwood Donavin											*		*		*		*		*		*		*		*
# Comments:																*		*		*		*		*		*		*		*
#***************************************************************************************************************************************
# Functions
#' @details converts a numeric vector or dataframe of numeric vectors to an easy-to-read dollar
#' format including '$'. It contains suppression for warnings for formatC() conversions, which sometimes occur with
#' (labeled?) numeric vectors. If an element in x is negative, a '-' is placed before '$'
#' @param x a numeric or integer vector
#' @param MM whether to condense the format into millions of dollars followed by 'MM'.
#' @param B whether to condense the format into billions of dollars followed by 'B'.
#' @param cents - whether to include cents in the dollor format. Defaults to FALSE
#' @return a character vector or dataframe of character vectors in dollar format.
dollars<-function(x, MM = FALSE, B=FALSE, cents = FALSE, digits = 0){
    if(is.numeric(x)){
        negative = logical(length(x)) #For correct negative formatting
        for(i in 1:length(x)){
            if(x[i]<0){
                negative[i] <- TRUE
                x[i] <- abs(x[i])
            }
        }

    #Formatting
        if(MM==TRUE){
            suppressWarnings(x<-formatC(x/1.0e+6,digits=digits,format="f",big.mark=","))
            for(i in 1:length(x)){
                if(negative[i]){
                    x[i]<-paste("-$",x[i],"MM",sep="")
                } else{
                    x[i]<-paste("$",x[i],"MM",sep="")
                }
            }
        } else if(B==TRUE){
            suppressWarnings(x<-formatC(x/1.0e+9,digits=digits,format="f",big.mark=","))
            for(i in 1:length(x)){
                if(negative[i]){
                    x[i]<-paste("-$",x[i],"B",sep="")
                } else{
                    x[i]<-paste("$",x[i],"B",sep="")
                }
            }
        } else{
            if(cents == TRUE){
                suppressWarnings(x<-formatC(x,digits=2,format="f",big.mark=","))
            } else{
                suppressWarnings(x<-formatC(x,digits=digits,format="f",big.mark=","))
            }
            for(i in 1:length(x)){
                if(negative[i]){
                    x[i]<-paste("-$",x[i],sep="")
                } else{
                    x[i]<-paste("$",x[i],sep="")
                }
            }
        }
        return(x)
    } else if(is.data.frame(x)){
        for(i in 1:length(x)){
            x[,i]<-dollars(x[,i],MM = MM, B = B, cents = cents, digits = digits)
        }
        return(x)
    } else{
        print("ERROR: Input must be a numeric vector or dataframe of numeric vectors.")
    }
}
#' @name mode
# Mode (statistical)
  mode <- function(x){
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
#' @name percent
#' @details Converts a one or more numeric vectors to percent format
#' @param x a numeric vector to convert to percent format
#' @param digits the number of decimal places to report past 1-100ths
#' @return a character vector in percent format
percent<-function(x, digits=0){
    if(is.numeric(x)){
        x<-round(x*100,digits = digits)
        y<-character(length = length(x))
        for(i in 1:length(x)){
            y[i]<-paste(sprintf(paste('%.',digits,'f',sep=""),x[i]),"%",sep="")
        }
        return(y)
    } else if(is.data.frame(x)){ #for multiple vectors in a dataframe objects
        y<-data.frame(array(dim= c(nrow(x),length(x))))
        names(y) <- names(x)
        for(i in 1:length(x)){
            y[,i]<-percent(x[,i], digits = digits)#Recursive call per numeric vector
        }
        return(y)
    }
}
  #'@name p.Val.Simple
  #'@details  reports P values in legible character strings that indicate significance level
  #'@param p a numeric vector of values between 0 and 1 (exclusive)
  #'@return a character vector simply indicating significance level
    p.Val.Simple<-function(p){
        s<-character(length(p))
        for(i in 1:length(p)){
            if(p[i]>=0.1){
              s[i]<-"p > 0.1"
            } else if(p[i]<0.001){
              s[i]<-"p < 0.001"
            } else if(p[i]<0.01){
              s[i]<-"p < 0.01"
            } else if(p[i]<0.1){
              s[i]<-"p < 0.1"
            }
        }
        return(s)
    }
  #'@name - p.Val.Star
  #'@details  reports p values in legible character strings that indicate significance level with stars
  #'@param p a numeric vector of values between 0 and 1 (exclusive)
  #'@return a character of p values indicating significance level with stars
  p.Val.Star<-function(p,digits = 3){
      s<-character(length(p))
      for(i in 1:length(p)){
          if(p[i]>=0.1){
              s[i]<-paste(round(p[i],digits = digits))
          } else if(p[i]<0.01){
              s[i]<-paste(format(p[i],digits = digits),"***")
          } else if(p[i]<0.05){
              s[i]<-paste(round(p[i],digits = digits),"**")
          } else if(p[i]<0.1){
              s[i]<-paste(round(p[i],digits = digits),"*")
          }
      }
      return(s)
  }
    #'@name sciNote
    #'@details sciNote takes a numeric vector and converts it into traditional, readable scientific
    #'notation format for markdown.
    #'@param s - a numeric value
    #'@param digits - An integer value of digits to report
    #'@param small.decimal.max - An integer value indicating the number of decimal places before the first significant
    #'(non-zero) digit required in order to convert the number to scientific notation. The number is otherwise
    #'truncated to (small.decimal.max - 1) decimal places.
    #'@param large.digit.min - The minimum base-ten places in order to be converted to scientific notation. The number is
    #'otherwise reported as is.
    #'@return a string value that is in scientific notation
    sci.Note<-function(s, digits = 1, small.decimal.max = 4, large.digit.min = 7){
        r = character(length(s))
        for(i in 1:length(s)){
            count = 0
            s_duplicate = s[i]
            if(s[i] < 1 & s[i] > -1){
                while(s_duplicate < 1 & s_duplicate > -1){
                    count = count - 1
                    s_duplicate = s_duplicate*10
                }
            }else{
                while(abs(s_duplicate) >= 10){
                    count = count + 1
                    s_duplicate = s_duplicate/10
                }
            }
            if(count <= -small.decimal.max | count >= large.digit.min){
                s[i] = s[i]/(10^count)
                r[i] = paste(round(s[i],digits = digits)," x 10^",count,"^",sep = "")
            } else if(count <= 0){
                r[i] = as.character(round(s[i],digits = abs(count)))
            } else{
                r[i] = as.character(s[i])
            }
        }
        return(r)
    }
