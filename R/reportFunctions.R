#################################################
# start: RT storage and analysis                #
# report functions                              #
# (c) 2015, Wouter Weeda, Leiden University     #
#################################################


importReport <- function(object)
# report on imported data
{

  if(class(object)=='subjects') {
    for(i in 1:length(object@rtdata)) {
      if(object@valid[i]) {
        importReport(object@rtdata[[i]])
      } else {
        os = c(paste0('File    : ',object@variables[i,1],'\n'),
               paste0('  error in read in. check remarks\n'))
        cat(os)
      }

    }
    cat(c(paste0('[overall remarks]\n'),paste0(object@remarks)))
  }

  if(class(object)=='rtdata') {
    #get number of rows,
    os = c(paste0('File    : ',object@import@filename,'\n'),
    paste0('import : ',length(object@rt),'\n'),
    paste0('remarks: ','\n'),
    paste0(object@import@remarks))
    cat(os)
  }

}
