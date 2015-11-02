#################################################
# start: RT storage and analysis                #
# import functions                              #
# (c) 2015, Wouter Weeda, Leiden University     #
#################################################

#importFunctions
#importMultiple

readRTfile <- function(filename,import.control)
#read a single file based on import.control settings
{
  stopflag = FALSE

  #set filename to importcontrol frame
  fn = strsplit(strsplit(filename,'\\.')[[1]],'\\/')[[1]]
  import.control@filename = fn[length(fn)]

  #read in the file according to the import.control settings
  dat = try(read.table(filename,sep=import.control@separator,header=import.control@header,skip=import.control@skip,stringsAsFactors=F,fill=T,dec=import.control@decimal),silen=T)

  if(class(dat)=='try-error') {
    warning(paste0('[readRTfile] Unable to read file ',filename))
    stopflag = TRUE
  }

  if(stopflag) return(import.control)

  #change names of columns to numbers when necessary
  import.control@rt.col = n2n(dat,import.control@rt.col)
  import.control@cic.col = n2n(dat,import.control@cic.col)
  import.control@con.col = n2n(dat,import.control@con.col)

  #import the reaction time and correct/incorrect vectors, and the conditions
  rtvec = dat[,import.control@rt.col]
  cicvec = dat[,import.control@cic.col]
  conditions = dat[,import.control@con.col]

  wrn = character(0)

  #checks:

  #rtvec
  if(length(rtvec)<=0) {
    wrn = c(wrn,'[readRTfile] no lines to read in.')
    import.control@remarks = wrn
    warning(wrn)
    stopflag = TRUE
  }

  if(stopflag) return(import.control)

  if(!is.numeric(rtvec)) {
    wrn = c(wrn,'[readRTfile] rtvec is not numeric.')
    import.control@remarks = wrn
    warning(wrn)
    stopflag = TRUE
  }

  if(stopflag) return(import.control)

  #correct incorrect
  if(!is.logical(cicvec)) {
    if(range(cicvec,na.rm=T)[1]!=0 | range(cicvec,na.rm=T)[2]!=1) {
      if(length(table(cicvec,useNA='no'))==2) {
        #force dichotomous to logical
        ncic = rep(FALSE,length(cicvec))
        ncic[which(cicvec==names(table(cicvec,useNA='no'))[1])]=TRUE
        wrn = c(wrn,paste0('[readRTfile] Forcing cicvec to logical with ',names(table(cicvec,useNA='no'))[1],'=TRUE and ',names(table(cicvec,useNA='no'))[2],'=FALSE.'))
        cicvec = ncic
      } else {
        if(length(table(cicvec,useNA='no'))==1) {
          #no determination, set all to TRUE
          cicvec = as.logical(cicvec)
          wrn = c(wrn,'[readRTfile] Constant in correct/incorrect, assuming perfect score (forcing as.logical).')
        } else {
          #no determination, set all to FALSE
          wrn = c(wrn,'[readRTfile] Cannot determine correct/incorrect!')
          import.control@remarks = wrn
          stopflag = TRUE
        }
      }
    } else {
      #binary 0/1 to FALSE/TRUE
      cicvec = as.logical(cicvec)
      wrn = c(wrn,'[readRTfile] Forcing 0/1 to FALSE/TRUE.')
    }
  }

  if(stopflag) return(import.control)

  #conditions
  conditions = as.data.frame(conditions)
  for(i in 1:ncol(conditions)) conditions[,i] = as.factor(conditions[,i])

  #create a new rtdata object
  rtdat = new('rtdata')

  #fill the new object
  #rt
  rtdat@rt = rtvec
  rtdat@rt.units = import.control@rt.units

  #correct/incorrect
  rtdat@correct = cicvec

  #conditions
  rtdat@conditions = conditions
  clist = list(ncol(conditions))
  for(i in 1:ncol(conditions)) clist[[i]] = levels(conditions[,i])
  rtdat@condition.levels = clist

  #valid (set to TRUE)
  rtdat@valid = rep(TRUE,length(rtvec))

  #import.control and remarks
  import.control@remarks = c(import.control@remarks,wrn)
  rtdat@import  = import.control

  #show warnings
  warnings(wrn)

  #return the rtdat object
  return(rtdat)

}

readMultiple <- function(path,import.control,patt=NULL,bsinfo=NULL)
#read in multiple files and return subjects object
{
  #list.files
  fns = list.files(path,pattern=patt,full.names=T)
  nsub = length(fns)
  if(nsub<=0) stop('[readMultiple] No files in path.')
  fnstrp = list.files(path,pattern=patt,full.names=F)

  wrn = character(0)

  #make new subjects object
  sdat = new('subjects')
  sdat@valid = rep(TRUE,nsub)
  sdat@rtdata = vector('list',nsub)

  #loop over subjects
  for(i in 1:nsub) {

    #read in rtdata
    rtdat = try(suppressWarnings(readRTfile(fns[i],import.control)),silen=T)
    if(class(rtdat)=='import') {
      rtdat2 = new('rtdata')
      rtdat2@import = rtdat
      rtdat = rtdat2
      sdat@valid[i] = FALSE
    }
    sdat@rtdata[[i]] = rtdat
  }

  #match bsinfo to filenames
  ord = match(fnstrp,bsinfo[,1])
  bsframe = data.frame(fileID=fnstrp,bsinfo[ord,])
  sdat@variables = bsframe

  #add variable levels
  clist = list(ncol(bsframe))
  for(i in 1:ncol(bsframe)) {
    if(is.factor(bsframe[,i])) clist[[i]] = levels(bsframe[,i])
  }
  sdat@variable.levels = clist

  #return subjects object with rtdata
  return(sdat)

}

