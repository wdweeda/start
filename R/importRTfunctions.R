#################################################
# start: RT storage and analysis                #
# (c) 2015, Wouter Weeda, Leiden University     #
#################################################

#importFunctions

readRTfile <- function(filename,import.control)
#read a single file based on import.control settings
{
  #read in the file according to the import.control settings
  dat = read.table(filename,sep=import.control@separator,header=import.control@header,skip=import.control@skip,stringsAsFactors=F,fill=T)

  #change names of columns to numbers when necessary
  import.control@rt.col = n2n(dat,import.control@rt.col)
  import.control@cic.col = n2n(dat,import.control@cic.col)
  import.control@con.col = n2n(dat,import.control@con.col)

  #import the reaction time and correct/incorrect vectors, and the conditions
  rtvec = dat[,import.control@rt.col]
  cicvec = dat[,import.control@cic.col]
  conditions = dat[,import.control@con.col]

  #checks:
    #rtvec  = numeric
    #cicvec = logical
    #conditions = data.frame with factors.

  #create a new rtdata object
  rtdat = new('rtdata')

  #fill the new object
  rtdat@RT = rtvec

  #return the rtdat object
  return(rtdat)

}



