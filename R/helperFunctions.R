#################################################
# start: RT storage and analysis                #
# helper functions                              #
# (c) 2015, Wouter Weeda, Leiden University     #
#################################################

#helper functions

n2n <- function(dat,ind)
#return the column number(s) matching to (a) column name(s)
{

  if(!is.data.frame(dat)) stop('[n2n] Data must be a data.frame')
  if( !(is.numeric(ind) | is.character(ind)) ) stop('[n2n] Indicator must be a character or numeric')

  if(is.character(ind)) ind = grep(paste0('^',ind,'$'),names(dat))

  return(ind)
}
