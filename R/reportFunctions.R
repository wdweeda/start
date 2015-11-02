#################################################
# start: RT storage and analysis                #
# report functions                              #
# (c) 2015, Wouter Weeda, Leiden University     #
#################################################


importReport <- function(object)
# report on imported data
{

  if(class(object)=='subjects') {

    #print table header
    cat(paste0('\nImport report for ',object@experimentname,' generated ',Sys.time(),'\n'))
    cat(rep('-',120),'\n',sep='')
    cat(sprintf('%20s %10s %s\n\n','[filename]','[nimports]','[remarks]'))

    #loop over all subjects
    tdat = character(0)
    for(i in 1:length(object@rtdata)) {
      if(object@valid[i]) {
        #call importReport on rtdata object
        tdat = rbind(tdat,importReport(object@rtdata[[i]]))
      } else {
        #if object is not valid get the filename and remarks
        tdat = rbind(tdat,c(object@rtdata[[i]]@import@filename,NA,object@rtdata[[i]]@import@remarks))
      }
      #print row of tdat
      cat(sprintf('%20s %10i %0s\n',tdat[i,1],as.integer(tdat[i,2]),tdat[i,3]))
    }
    cat('\n',rep('-',120),'\n',sep='')

    #return the data.frame containing report data invisibly
    return(invisible(tdat))
  }

  if(class(object)=='rtdata') {
    #return a dataframe with filename, nimport, and remarks if object is of class rtdata
    rem = object@import@remarks
    if(length(rem)==0) rem=''
    return(data.frame(filename=object@import@filename,nimport=length(object@rt),remarks=rem,stringsAsFactors=F))
  }

}
