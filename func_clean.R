#***************************************************************************************************************************************
#' @title Cleaning Functions
#' @author Kirkwood Donavin
#***************************************************************************************************************************************
# Outside Libraries
    library(Hmisc)
# Cleaning Functions
  #' @details Detaches all "other" packages. This does *not* include attached 'base' packages (i.e., base, methods,
  #' datasets, utils, grDevices, graphics and stats).
  detach_other<-function(){
    sesh<-sessionInfo()
    for(pack in names(sesh[["otherPkgs"]])){
      exp<-paste("detach(package:",pack,")",sep="")
      eval(parse(text=exp))
    }
  }

#' @name Progress Bar
#' @details a function that prints a graphical progress bar based on provided index and total.
#' Default barLength is 20 characters.
#' @param index - the index position which may represent a position within a vector or dataframe.
#' @param total - the total length or number of actions to be computed, which may be the length of 
#' a vector or the nrow() of a data.frame.
#' @param barLength - the length of the progress bar in character spaces
progBar<-function(index,total,barLength = 20){
  barLength<-round(barLength)
  progNum<-round(barLength*(index/total))
  progress<-"|"
  for(number in 1:progNum){
    if(progNum == 0){
      next
    }
    progress<-paste(progress,"=",sep="")
  }
  remainder<-barLength-progNum
  for(number in 1:remainder){
    if(remainder == 0){
      next
    }
    progress<-paste(progress,"-",sep="")
  }
  percentage<-paste(round((index/total)*100),"%",sep="")
  progress<-paste(progress,"| ",percentage,sep ="")
  cat("\r",progress) #Note: carriage return
}

#' @name relationshipBalance
#' @details Fills in missing explicit relationships using an ID variable and a relationship variable.
#' First, this function checks this award's list of related-awards' related-awards (i.e. meta-related awards) to ensure
#' the related awards' lists contain this award's contract number/ID as a related award. Second, it inspects this list for
#' relationships it does not have to explicitly add to this awards list of related-awards. It recursively checks
#' added relationship's meta-related awards as necessary.
#' @param IDVar - The name of the variable in a dataset called setName (see setName) that identifies awards (related or not)
#' @param setName - character string name of the dataset to be manipulated
#' @param seperator - separator string between IDs for string-vector conversions
#' @param env - the environment in which setName may be found
relationshipBalance<-function(IDVar = "Economic.Impact.ID", relationVar = "Other.Award.IDs",
                              setName = "sbir", separator = ", ", env = parent.frame()){
    dataset<-get(setName, envir = env) #pull in dataframe
    if(class(dataset[,relationVar]) != "character"){
        dataset[,relationVar]<-as.character(dataset[,relationVar])
        cat(paste("WARNING: ", relationVar, " has been converted to a character vector.\n",sep = ""))
    }
    #Private recursion function
    .relationshipFillingRec<-function(e){
        change <- FALSE
        for(number in e$relationList){
            metaList <- strToVec(e$dataset[e$dataset[,e$IDVar] == number,e$relationVar], separator = e$separator)
            for(metaNumber in metaList){
                if(metaNumber != e$ID & metaNumber %nin% e$relationList){
                    e$relationList<-c(e$relationList,metaNumber)
                    change <- TRUE
                }
            }
        }
        if(change){
            .relationshipFillingRec(e)
        }
    }
    # One-way-Fill
    for(i in 1:nrow(dataset)){
        if(dataset[i, relationVar] != ""){
            ID<-as.character(dataset[i,IDVar]) #may be a factor in original data
            relationList<-strToVec(as.character(dataset[i,relationVar]), sep = separator)
            for(number in relationList){
                metaList <- strToVec(as.character(dataset[dataset[,IDVar] == number,c(relationVar)]),separator = separator)
                if(ID %nin% metaList){
                    metaList<-c(metaList,ID)
                }
                metaString<-vecToStr(metaList, separator = separator)
                dataset[dataset[,IDVar] == number, relationVar] <- metaString
            }
        }
    }
    # Recursive Fill
    cat("PLEASE WAIT: Recursion in Progress\n")
    for(i in 1:nrow(dataset)){
        progBar(index = i, total = nrow(dataset))
        ID<-as.character(dataset[i,IDVar])
        relationList<-strToVec(as.character(dataset[i,relationVar]),separator = separator)
        .relationshipFillingRec(e=environment()) #private function
        dataset[i,relationVar]<-vecToStr(relationList, separator = separator)
    }
    assign(setName, dataset, envir = env) #export revised data
}

#' Remove Values ----
#' @details A function that removes all objects in the the .GlobalEnv. May be used to clean up at the
#' end of a script file.
rm_values<-function(){
    objs <- objects(name = '.GlobalEnv')
    for(object_name in objs){
      obj <- get(object_name, envir = as.environment('.GlobalEnv'))
      if(!(is.data.frame(obj)) & !(is.function(obj))){
        remove(list=(object_name), envir = as.environment('.GlobalEnv'))
      }
    }
}

#' @details python_boolean directly coverts python boolean values to R logical values, and vice versa,
#' within a dataset.
python_boolean_translation <-function(data, envir = parent.frame()){
    data_name = deparse(substitute(data))
    for(varname in names(data)){
        if('False' %in% data[,varname] | 'True' %in% data[,varname]){
            data[,varname]<-as.logical(data[,varname])
        } else if(is.logical(data[,varname])){
            data[,varname]<-ifelse(data[,varname],'True','False')
        }
    }
    assign(data_name, data, envir = envir)
}

#' @name sort_variables
#' @details sorts the columns of a data.frame object, excluding 'head_variables' to be in the front
#' in the given order.
#' @param data - data.frame object whose columns are to be sorted
#' @param head_variables - a character vectors of the names of variables to place at the 'head' of
#' the sorted data. That is, these will be excluded from sort.
sort_variables<- function(data, head_variables, env = parent.frame()){
    data_name <-deparse(substitute(data))
    head <- data.frame(data[,head_variables])
    names(head)<-head_variables
    tail <- data[,sort(names(data))[which(!(sort(names(data)) %in% head_variables))]]
    data <- data.frame(head,tail)
    assign(x = data_name, value = data, envir = env)
}
