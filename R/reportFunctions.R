#################################################
# start: RT storage and analysis                #
# report functions                              #
# (c) 2015, Wouter Weeda, Leiden University     #
#################################################


importReport <- function(object)
# report on imported data
{

  if(class(object)=='subjects') {
    tdat = character(0)
    cat(sprintf('%20s %10s %s\n','[filename]','[nimports]','[remarks]'))
    for(i in 1:length(object@rtdata)) {
      if(object@valid[i]) {
        tdat = rbind(tdat,importReport(object@rtdata[[i]]))
      } else {
        #if object is not valid
        tdat = rbind(tdat,c(object@rtdata[[i]]@import@filename,NA,object@rtdata[[i]]@import@remarks))
      }
      cat(sprintf('%20s %10i %0s\n',tdat[i,1],as.integer(tdat[i,2]),tdat[i,3]))
    }
    return(invisible(tdat))
  }

  if(class(object)=='rtdata') {
    rem = object@import@remarks
    if(length(rem)==0) rem=''
    #browser()
    return(data.frame(filename=object@import@filename,nimport=length(object@rt),remarks=rem,stringsAsFactors=F))
  }

}
