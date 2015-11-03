#################################################
# start: RT storage and analysis                #
# helper functions                              #
# (c) 2015, Wouter Weeda, Leiden University     #
#################################################

#helper functions
#n2n
#npq
#bin
#recurSwitch
#makelevels
#makeconditionarray

n2n <- function(dat,ind)
#return the column number(s) matching to (a) column name(s)
{

  if(!is.data.frame(dat)) stop('[n2n] Data must be a data.frame')
  if( !(is.numeric(ind) | is.character(ind)) ) stop('[n2n] Indicator must be a character or numeric')

  if(is.character(ind)) ind = grep(paste0('^',ind,'$'),names(dat))

  return(ind)
}


npq <- function(x,quant)
#return n per quantile
{
    q = quantile(x,quant)
    n = length(which(x<q))
    return(n)
}

bin <- function(x)
#return width of bins
{
    y = numeric(length(x))

    y[1] = x[1]
    for(i in 2:length(x)) y[i] = x[i]-x[i-1]

    return(y)
}

recurSwitch <- function(object,funcall,argslist)
#apply a function recursively to the rtdata objects if the object is of class subjects
{

  if(class(object)=='subjects') {
    #get objects into the current environment
    for(i in 1:length(argslist)) {
      assign(argslist[i],get(paste(argslist[i]),envir=sys.frames()[[sys.nframe()-1]]))
    }

    #run over subjects
    for(i in 1:length(object@valid)) {
      if(object@valid[i]) {

        #make the correct function call, now with the rtdata object
        cl = paste0(argslist[1],'=',argslist[1])
        if(length(argslist)>1) {
          for(j in 2:(length(argslist))) {
            cl = paste0(cl,',',argslist[j],'=',argslist[j])
          }
        }

        #set the rtdat object to the rtdata object and evaluate
        rtdat = object@rtdata[[i]]
        object@rtdata[[i]] = eval(parse(text=paste0(funcall[[1]],'(',cl,')')))
      } else {
        #object not valid
        #do nothing for now
      }

    }
  } else {
    if(class(object)!='rtdata') stop('Input is not of class rtdata or subjects.')
  }

  return(object)
}

makelevels <- function(conditionvec,dat)
  #get all levels from an experiment
{
  lv = vector('list',length(conditionvec))

  for(i in 1:length(conditionvec)) {
    lv[[i]] = eval(parse(text=paste('levels(as.factor(dat$',conditionvec[i],'))',sep='')))
  }

  return(lv)

}


makeconditionarray <- function(conditionvec,lv)
{
  #make condition array
  nc = 1
  for(condition in 1:length(lv)) {
    if(length(lv[[condition]])>0) nc = nc * length(lv[[condition]])
  }

  sv = nc
  condmat = rep(lv[[1]],each=sv/length(lv[[1]]))
  sv = sv / length(lv[[1]])
  if(length(lv)>=2) {
    for(i in 2:length(lv)) {
      sw = 1
      for(j in 1:(i-1)) sw = sw*length(lv[[j]])
      condmat = cbind(condmat,rep(rep(lv[[i]],each=sv/length(lv[[i]])),times=sw))
      sv = sv / length(lv[[i]])
    }
  }

  if(!is.matrix(condmat)) condmat = as.matrix(condmat)

  conditionvector = character(nrow(condmat))
  for(i in 1:nrow(condmat)) {
    conditionvector[i] = paste(condmat[i,],collapse='_')
  }

  dimnames(condmat) = list(conditionvector,conditionvec)
  return(condmat)
}
