#############################################
# RTanalyze S4 CLASS DEFINITIONS			#
# Copyright(c) 2011 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################



#fitDiffusionModel
#doexp
#makelevels
#makeconditionarray
#getdepends
#getestimates
#getsamples

fitDiffusionModel <- function(rtdat,fdmobject,ID,onlyreadoutput=F,removeAfterUse=T,usingWin=F,runstring='c:/fast-dm-29-win32/')
#fit diffusion model on a set of subjects
{
	if(missing(ID)) {
		ID = 0
		warning('')
		if(removeAfterUse==FALSE) {
			ID = 666
			warning('ID is missing, using 666. Files will be overwritten when attempting to run more subjects without ID.\n')
		}
	}

	#get condition names
	which.within=.fastdm.conditions(fdmobject)

	cvec = character(0)

	#make cvec
	eval(parse(text=paste('cvec = as.character(rtdat$`',which.within[1],'`[.rtdata.valid(rtdat)==TRUE])',sep='')))
	if(length(which.within)>1) {
		for(j in 2:length(which.within)) {
			eval(parse(text=paste('cvec = cbind(cvec,as.character(rtdat$`',which.within[j],'`[.rtdata.valid(rtdat)==TRUE]))',sep='')))
		}
	}

	if(.rtdata.rt.units(rtdat)=='ms') rt = .rtdata.rt(rtdat)[.rtdata.valid(rtdat)==TRUE]/1000 else rt = .rtdata.rt(rtdat)[.rtdata.valid(rtdat)==TRUE]

	rtvec = cbind(rt,as.numeric(rtdat@correct[rtdat$valid==TRUE]),cvec)


	#set correct outputdirs
	path = .fastdm.datadir(fdmobject)
	subject = ID
	fnt = strsplit(.fastdm.dataname(fdmobject),'\\*')[[1]]
	fn = paste(fnt[1],subject,fnt[2],sep='')
	write.table(rtvec,file=paste(path,'/',fn,sep=''),quote=F,row.names=F,col.names=F,sep='\t')
	#do actual experiment
	if(onlyreadoutput==F) runfdm = T else runfdm = F
	fdmdata = doexp(fdmobject,subject,bootstrapnum=.fastdm.bootstrap.num(fdmobject),runfdm=runfdm,removeAfterUse=removeAfterUse,usingWin=usingWin,runstring=runstring)


	return(fdmdata)
}


doexp <- function(fdmex,subject.indicator=NULL,bootstrapnum=1,runfdm=T,removeAfterUse=T,usingWin=F,runstring='c:/fast-dm-30-win32/')
{
	cat('[fast-dm] Fitting subject',subject.indicator,'...\n')

	#make experiment file based on fastdm object
	writestring = character(0)

	#set method to KS as default (TODO make an option for method selection).
	meth = tolower(fdmex@method)
	writestring = c(writestring,paste('method ',meth,'\n',sep=''))

	if(length(fdmex@set)!=0) {
		for(i in 1:length(fdmex@set)) {
			writestring = c(writestring,paste('set ',fdmex@set[[i]][1],' ',fdmex@set[[i]][2],'\n',sep=''))
		}
	}

	if(length(fdmex@depends)!=0) {
		for(i in 1:length(fdmex@depends)) {
			writestring = c(writestring,paste('depends ',fdmex@depends[[i]][1],' ',fdmex@depends[[i]][2],'\n',sep=''))
		}
	}

	fmstring = 'format '
	for(i in 1:length(fdmex@format)) {
		fmstring = paste(fmstring,fdmex@format[i],' ',sep='')
	}
	fmstring = paste(fmstring,'\n')
	writestring = c(writestring,fmstring)

	if(!is.null(subject.indicator)) {
		sp = strsplit(fdmex@dataname,'\\*')
		datname = paste(sp[[1]][1],subject.indicator,sp[[1]][2],sep='')
		writestring = c(writestring,paste('load \"',datname,'\"\n',sep=''))

		sp = strsplit(fdmex@outputname,'\\*')
		outname = paste(sp[[1]][1],subject.indicator,sp[[1]][2],sep='')
		writestring = c(writestring,paste('save \"',outname,'\"\n',sep=''))
	} else {
		outname = fdmex@outputname
		datname = fdmex@dataname

		writestring = c(writestring,paste('load \"',datname,'\"\n',sep=''))
		writestring = c(writestring,paste('save \"',outname,'\"\n',sep=''))
	}

	#run the actual fast-dm analysis


	if(runfdm) {
		fn = paste(fdmex@datadir,'/experiment.ctl',sep='')
		write.table(writestring,file=fn,row.names=F,col.names=F,quote=F)

		if(usingWin==TRUE) {
			owd=getwd()
			setwd(runstring)
			output = system(paste(runstring,'fast-dm-30.exe',sep=''))
			setwd(owd)
		} else {
			output = try(system(paste('cd ',fdmex@datadir,'\n',fdmex@appdir,'/fast-dm ',fn,sep=''),intern=FALSE))
		}

		if(removeAfterUse) {
			file.remove(fn)
		}
	}

	if(class(output)!='try-error' & file.exists(paste(fdmex@datadir,'/',outname,sep=''))) {

		#format the outputparameters
		dfn = paste(fdmex@datadir,'/',datname,sep='')
		dat = read.table(dfn,header=F,fill=T)

		names(dat) = fdmex@format
		ofn = paste(fdmex@datadir,'/',outname,sep='')
		out = read.table(ofn,header=F,fill=T,stringsAsFactors=F)

		#fix fit index readin
		rmon = which(out$V2=='index')
		if(length(rmon)>0) {
			out$V1[rmon]='fit_index'
			out$V2[rmon]='='
			out$V3[rmon]=out$V1[rmon+1]
			out=out[-(rmon+1),]
		}

		#set method to numeric
		m = out$V3[out$V1=='method']
		if(m=='ML') out$V3[out$V1=='method']=1
		if(m=='KS') out$V3[out$V1=='method']=2
		if(m=='CS') out$V3[out$V1=='method']=3
		out$V3 = as.numeric(out$V3)

		out = out[,-2]
		outframe = data.frame(t(out[,2]))
		names(outframe) = out[,1]

		if(removeAfterUse) {
			file.remove(dfn)
			file.remove(ofn)
		}


		#get all estimates in the right order and estimate forward data
		dependent = getdepends(fdmex,dat,out)
		estimates = getestimates(fdmex,out,dependent$outmat,dependent$levelmat)
		if(bootstrapnum!=0) sampledata = getsamples(fdmex,dat,estimates,bootstrapnum,T,subject.indicator,runfdm) else sampledata=numeric(0)

		fdmout = new('fdmoutput')
		fdmout@ID = subject.indicator
		fdmout@fdmex = fdmex
		fdmout@data = dat
		fdmout@parameters = t(outframe)
		fdmout@estimates = estimates
		fdmout@bootstrapdata = sampledata
		fdmout@outputlog = output

		cat('[fast-dm] Finished\n')

		return(fdmout)

	} else {
		cat('[fast-dm] Finished with ERRORS.\n')

		return(NULL)
	}


}

getdepends <- function(fdmex,dat,out,parameters=c('v','a','t0','zr','szr','st0','sv','d'))
{
	totlev = makelevels(fdmex@conditions,dat)
	totmat = makeconditionarray(fdmex@conditions,totlev)

	depmat = matrix(unlist(fdmex@depends),,2,byrow=T)
	outmat = matrix('',nrow(totmat),length(parameters),dimnames=list(rownames(totmat),parameters))

	for(parnum in 1:length(parameters)) {

		for(cond in 1:nrow(totmat)) {

			outmat[cond,parnum] = parameters[parnum]
			dep = grep(parameters[parnum],depmat[,1])
			depends = depmat[dep,2]

			for(fact in 1:ncol(totmat)) {

				if(length(depends)>0) {
					dpvec = logical(length(depends))
					for(dp in 1:length(depends)) {
						if(colnames(totmat)[fact]==depends[dp]) dpvec[dp]=TRUE
					}
					if(any(dpvec)) {
						outmat[cond,parnum] = paste(outmat[cond,parnum],'_',totmat[cond,fact],sep='')
					}
				}
			}
		}
	}

	return(list(outmat=outmat,levelmat=data.frame(totmat)))
}


getestimates <- function(fdmex,out,dependsmatrix,levelmat)
{
	estmatrix = matrix(NA,nrow(dependsmatrix),ncol(dependsmatrix),dimnames=list(rownames(dependsmatrix),colnames(dependsmatrix)))
	paramvec = colnames(dependsmatrix)

	for(cond in 1:nrow(estmatrix)) {
		for(parnum in 1:ncol(estmatrix)) {

			sets = matrix(unlist(fdmex@set),,2,byrow=T)
			setval = which(sets[,1]==paramvec[parnum])

			if(length(setval)>0) {
				estmatrix[cond,parnum] = sets[setval,2]
			} else {
				estmatrix[cond,parnum] = out[grep(paste('\\<',dependsmatrix[cond,parnum],'\\>',sep=''),as.character(out[,1])),2]
			}
		}
	}

	outmat = cbind(levelmat,estmatrix)

	return(outmat)

}


getsamples <- function(fdmex,dat,estimatematrix,bootstraps=1,deterministic=F,subID,runfdm,sampledatname='_sampledata',sampledatext='.txt',removeAfterUse=T)
{
	sampledat = vector('list',nrow(estimatematrix))
	paramvec = colnames(estimatematrix)

	totlev = makelevels(fdmex@conditions,dat)
	totmat = makeconditionarray(fdmex@conditions,totlev)

	samplen = numeric(0)

	for(cond in 1:length(sampledat))
	{

		totselect = character(0)
		for(i in 1:ncol(totmat)) {
			totselect = c(totselect,paste('dat$',colnames(totmat)[i],'==','\'',totmat[cond,i],'\'',sep=''))
		}
		eval(parse(text=paste('samplen = sum(',paste(totselect,collapse=' & '),')',sep='')))

		if(samplen>0) {
			#browser()
			bssample = array(NA,dim=c(samplen,2,bootstraps),dimnames=list(seq(1,samplen),c('correct','RT'),seq(1,bootstraps)))

			if(!deterministic) {
				if(runfdm) {
					outlog = system(paste('cd ',fdmex@datadir,'\n',fdmex@appdir,'/construct-samples ','-a',estimatematrix[cond,which(paramvec=='a')],' -z',estimatematrix[cond,which(paramvec=='zr')],' -v',estimatematrix[cond,which(paramvec=='v')],' -t',estimatematrix[cond,which(paramvec=='t0')],' -d',estimatematrix[cond,which(paramvec=='d')],' -Z',estimatematrix[cond,which(paramvec=='szr')],' -V',estimatematrix[cond,which(paramvec=='sv')],' -T',estimatematrix[cond,which(paramvec=='st0')],' -n',samplen,' -r',' -N',bootstraps,' -o',path.expand(fdmex@datadir),'/',subID,'_cond',cond,sampledatname,'%d',sampledatext,sep=''),intern=FALSE)
				}

				for(bs in 1:bootstraps) {
					fn=paste(fdmex@datadir,'/',subID,'_cond',cond,sampledatname,bs-1,sampledatext,sep='')

					rbs = try(read.table(file=fn))
					if(class(rbs)!='try-error') {

						bssample[,,bs] = as.matrix(rbs)
						if(removeAfterUse) file.remove(fn)
					} else {
						cat('[fast-dm] BOOTSTRAP ERROR\n')
					}

				}

				sampledat[[cond]] = bssample


			} else {
				bs = 1
				if(runfdm) {
					outlog = system(paste('cd ',fdmex@datadir,'\n',fdmex@appdir,'/construct-samples ','-a',estimatematrix[cond,which(paramvec=='a')],' -z',estimatematrix[cond,which(paramvec=='zr')],' -v',estimatematrix[cond,which(paramvec=='v')],' -t',estimatematrix[cond,which(paramvec=='t0')],' -d',estimatematrix[cond,which(paramvec=='d')],' -Z',estimatematrix[cond,which(paramvec=='szr')],' -V',estimatematrix[cond,which(paramvec=='sv')],' -T',estimatematrix[cond,which(paramvec=='st0')],' -n',samplen,' -N',bootstraps,' -o',path.expand(fdmex@datadir),'/',subID,'_cond',cond,sampledatname,'%d',sampledatext,sep=''),intern=FALSE)
				}

				fn = paste(fdmex@datadir,'/',subID,'_cond',cond,sampledatname,bs-1,sampledatext,sep='')
				rbs = try(read.table(file=fn))
				if(class(rbs)!='try-error') {
					sampledat[[cond]] = as.matrix(rbs)
					if(removeAfterUse) file.remove(fn)
				} else {
					cat('[fast-dm] BOOTSTRAP ERROR\n')
				}

			}

		} else {

			sampledat[[cond]]=NA
		}



	}

	return(sampledat)
}

simulatesamples <-
function(fdmdata,fixedlist=NULL,bootstrapnum=1)
{
	#load in data, estimates, and fdm options
	dat = fdmdata@data
	estimates = fdmdata@estimates
	fdmex = fdmdata@fdmex
	subject.indicator = fdd@ID

	#run through the fixedlist (named list [1]=parameter, [2]=condition, [3]=value), eg. list(c('a','switch',0.984),c('v','switch',2.34))
	if(!is.null(fixedlist)) {
		for(i in 1:length(fixedlist)) {
			row = grep(paste('\\<',fixedlist[[i]][2],'\\>',sep=''),dimnames(estimates)[[1]])
			col = grep(paste('\\<',fixedlist[[i]][1],'\\>',sep=''),dimnames(estimates)[[2]])
			if(length(row)==0 | length(col)==0) warning('No correct selection found in fixedlist, no values fixed in estimates matrix.')
			estimates[row,col] = as.numeric(fixedlist[[i]][3])
		}
	}

	sampledata = getsamples(fdmex,dat,estimates,bootstrapnum,T,subject.indicator,TRUE,'_simdata','.txt')

	fdmout = new('fdmoutput')
	fdmout@ID = subject.indicator
	fdmout@fdmex = fdmex
	fdmout@data = dat
	fdmout@parameters = fdmdata@parameters
	fdmout@estimates = estimates
	fdmout@bootstrapdata = sampledata

	return(fdmout)

}


makefitarray <- function(fdmdata,FUN)
#make a fitarray to plot easy
{
	totlev = makelevels(fdmdata@fdmex@conditions,fdmdata@data)
	totmat = makeconditionarray(fdmdata@fdmex@conditions,totlev)

	condnames = colnames(totmat)
	dmcondnames = rownames(totmat)
	funname = paste(match.call()$FUN,c('TRUE','FALSE','Pc'),sep='')
	outmat = modmat = as.data.frame(matrix(NA,length(dmcondnames),3,dimnames=list(dmcondnames,funname)))

	data = fdmdata@data
	dat = numeric(0)

	for(dmcond in 1:dim(totmat)[1])	{
		evstring = paste('dat = data[data$`',condnames[1],'`==\'',totmat[dmcond,1],'\'',sep='')

		if(length(condnames)>1) {
			for(i in 2:length(condnames)) {
				evstring = paste(evstring,' & data$`',condnames[i],'`==\'',totmat[dmcond,i],'\'',sep='')
			}
		}

		evstring = paste(evstring,',]',sep='')
		eval(parse(text=evstring))

		outmat[dmcond,1] = apply(as.matrix(dat$TIME[dat$RESPONSE==1]),2,FUN)
		outmat[dmcond,2] = apply(as.matrix(dat$TIME[dat$RESPONSE==0]),2,FUN)
		outmat[dmcond,3] = length(dat$TIME[dat$RESPONSE==1]) / (length(dat$TIME[dat$RESPONSE==1])+length(dat$TIME[dat$RESPONSE==0]))

		mod = as.data.frame(fdmdata@bootstrapdata[[dmcond]])

		modmat[dmcond,1] = apply(as.matrix(mod$V2[mod$V1==1]),2,FUN)
		modmat[dmcond,2] = apply(as.matrix(mod$V2[mod$V1==0]),2,FUN)
		modmat[dmcond,3] = length(mod$V2[mod$V1==1]) / (length(mod$V2[mod$V1==1])+length(mod$V2[mod$V1==0]))

	}

	return(list(data=outmat,model=modmat))

}

summarize.diffmod <- function(subjectdata)
#get all diffusionmodel estimates
{
	subs = which(.subjects.valid(subjectdata)==TRUE)

	summary.dif = numeric(0)

	for(i in 1:length(subs)) {

		cc = try(.subjects.fdmdata(subjectdata)[[subs[i]]],silen=T)

		if(class(cc)!='try-error') {
			if(!is.null(.subjects.fdmdata(subjectdata)[[subs[i]]])) {
				sumdif = .fdmoutput.estimates(.subjects.fdmdata(subjectdata)[[subs[i]]])

				sdat = .subjects.variables(subjectdata)[subs[i],]
				if(nrow(sumdif)>1) {
					for(j in 2:nrow(sumdif)) {
						sdat = rbind(sdat,.subjects.variables(subjectdata)[subs[i],])
					}
				}

				sdat = cbind(sdat,sumdif)
				summary.dif = rbind(summary.dif,sdat)
			} else {
				#summary.dif = rbind(summary.dif,NA)
			}
		} else {
			#summary.dif = rbind(summary.dif,NA)
		}

	}

	return(summary.dif)

}

readFDM <- function(wd=getwd(),expname='experiment.ctl',outname='fdmestimates.txt')
{
	twd = getwd()
	setwd(wd)

	dat = read.table(expname,header=F,sep='',fill=T,stringsAsFactors=F)

	cat('Reading',expname,'from',wd,'\n')

	np = dat[grep('save',tolower(dat[,1])),2]
	np2 = strsplit(np,'\\*')[[1]][1]

	fl = list.files(wd,np2)

	if(length(fl)>0) {
		cat('Found',length(fl),'output files. Fetching estimates...')

		tmp = read.table(fl[1],sep='',header=F,stringsAsFactors=F,fill=T)
		out = matrix(NA,length(fl),nrow(tmp),dimnames=list(fl,tmp[order(tmp[,1]),1]))

		if(sum(colnames(out)==tmp[order(tmp[,1]),1])==length(colnames(out)))	out[1,] = tmp[order(tmp[,1]),3] else stop('Parameter names mismatch\n')

		if(length(fl)>1) {
			for(i in 2:length(fl)) {
				tmp = read.table(fl[i],sep='',header=F,stringsAsFactors=F)
				if(sum(colnames(out)==tmp[order(tmp[,1]),1])==length(colnames(out))) out[i,] = tmp[order(tmp[,1]),3] else stop('Parameter names mismatch\n')
			}
		}
		cat('done.\n')

	} else {
		stop(paste('There are no filenames matching `',np2,'`in directory ',wd,'. Please check ',expname,'.',sep=''))
	}

	#write it down
	out = as.data.frame(out)
	out = cbind(rownames(out),out)
	names(out)[1]='filename'

	checkname <- function(outname) {
		x = strsplit(outname,'\\.')
		outname = paste(x[[1]][1],'0.',x[[1]][2],sep='')
		return(outname)
	}

	while(file.exists(outname)) {
		outname = checkname(outname)
	}

	write.table(out,file=outname,row.names=F,col.names=T,sep='\t',na='')
	cat('Written output to',outname,'\n')

	setwd(twd)
	return(invisible(out))
}



link.fdmdata <- function(subjectdata,fdmdata,subjIDname='ppID')
#link a list of fdmdata objects to matching subjects data
{
	sv = which(.subjects.valid(subjectdata)==TRUE)
	.subjects.fdmdata(subjectdata) = vector('list',length(.subjects.valid(subjectdata)))

	for(i in 1:length(fdmdata)) {

		#check match and fill if all is well
		if(.subjects.variables(subjectdata)[,grep(paste('^',subjIDname,'$',sep=''),names(.subjects.variables(subjectdata)))][sv[i]]==.fdmoutput.ID(fdmdata[[i]])) {
			.subjects.fdmdata(subjectdata)[[sv[i]]] = fdmdata[[i]]
		} else {
			cat(.subjects.variables(subjectdata)[,grep(paste('^',subjIDname,'$',sep=''),names(.subjects.variables(subjectdata)))][sv[i]],.fdmoutput.ID(fdmdata[[i]]),'\n')
			warning('No ppID match')
		}
	}

	return(subjectdata)
}





constructsample <- function(v,a,t0,d,zr,szr,sv,st0,samplen,fdmdata,removeAfterUse=T)
#wrapper for constructsample routine of fast-dm (more direct than getsamples/simulatesamples)
{
	fdmex = fdmdata@fdmex

	outlog = system(paste('cd ',fdmex@datadir,'\n',fdmex@appdir,'/construct-samples ','-a',a,' -z',zr,' -v',v,' -t',t0,' -d',d,' -Z',szr,' -V',sv,' -T',st0,' -n',samplen,' -N',1,' -o',path.expand(fdmex@datadir),'/','constsample.txt',sep=''),intern=FALSE)

	if(file.exists(paste(path.expand(fdmex@datadir),'/','constsample.txt',sep=''))) {
		dat = read.table(paste(path.expand(fdmex@datadir),'/','constsample.txt',sep=''))

		if(removeAfterUse) {
			file.remove(paste(path.expand(fdmex@datadir),'/','constsample.txt',sep=''))
		}
	} else {
		dat = NULL
	}


	return(dat)
}

spotdiff_test <- function()
{
	#spotdiffusion(			double *a, 		double *t0, 	double *P, 	double *sda, 	double *rd, 	double *st0, double *sz, double *eta, double *cond, 	int *ntrials, 		int *maxstep, double *RTerr, double *RTcorr)
	out = .C('spotdiffusion',as.double(.9),as.double(.1),as.double(.2),as.double(.2),as.double(.2),as.double(0),as.double(0),as.double(0),as.double(1),as.integer(50000),as.integer(1500),as.double(vector('numeric',50000)),as.double(vector('numeric',50000)))
	browser()
}


spotdiff <- function(a,ter,P,sda,rd,fl_cond,ster=0,sz=0,eta=0,ntrials=50000,maxstep=1500)
{
	#spotdiffusion(			double *a, 		double *t0, 	double *P, 	double *sda, 	double *rd, 	double *st0, double *sz, double *eta, double *cond, 	int *ntrials, 		int *maxstep, double *RTerr, double *RTcorr)
	out = .C('spotdiffusion',as.double(a),as.double(ter),as.double(P),as.double(sda),as.double(rd),as.double(fl_cond),as.double(ster),as.double(sz),as.double(eta),as.integer(ntrials),as.integer(maxstep),as.double(vector('numeric',ntrials)),as.double(vector('numeric',ntrials)))

	RTerr = out[[12]]
	RTcorr = out[[13]]

	RTerr=RTerr[-which(RTerr==0)]
	RTcorr=RTcorr[-which(RTcorr==0)]

	return(list(TRcorrect=RTcorr,TRerror=RTerr))

}
