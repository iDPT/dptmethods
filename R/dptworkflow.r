## Filename: dptworkflow.r
## Author: Yaomin Xu
## Date: Created on Sep 11, 2010, Modified on Oct 1, 2012
## Notes: setup iDPT workspace and iDPT workflow
## =============================================================================
.dptworkflow <- expression({
  .First <- function(){
    vflag <- F
    require(MCMCpack,quietly=vflag)
    require(snowfall,quietly=vflag)
    require(nlme,quietly=vflag)
    require(gmodels,quietly=vflag)
    require(MASS,quietly=vflag)
    require(plyr,quietly=vflag)
    require(preprocessCore,quietly=vflag)
    require(methods,quietly=vflag)
    require(inline,quietly=vflag)
    require(Rcpp,quietly=vflag)
    require(IRanges,quietly=vflag)
    require(Biostrings,quietly=vflag)
    require(mmap,quietly=vflag)
    require(qvalue, quietly=vflag)
  }
  .dump <- .First()
  
  dptws.initexpr <- expression({load(".RData"); .First()})
  dpt.options <- list(
                      ws = list(
                        ws.root = opt$ws.root,
                        out.path = opt$output,
                        in.path = opt$input,
                        analysis.path = opt$output,
                        mixpos.path = "MixPoisson",
                        pattrecog.path = "PattRecog",
                        difftest.path = "DiffTest",
                        preprocess.path="Preprocess",
                        log.path = "logs",
                        postprocess.path="Postprocess",
                        src.path = "src",
                        tmp.path = ".tmp"),
                      ## ws paths for major modules and utilities
                      output = list(
                        preprocess = "joinsample",
                        joinSample="joinsample",
                        mixPoisson = "mixpoi",
                        pattRecog = "pattrecog",
                        diffTest = "difftest",
                        report = 'report',
                        analysis = opt$analysis)
                    ## output elements for tasks and filenames
                      )
  dpt.options$logs <- dpt.options$output
  if(opt$verbose>0) print(dpt.options)
## logs with files named after tasks in output
                    
## get.src <- function(string, base, ws.options){

##  if(!missing(base)) {
##    source(file.path(base, string), echo=F)
##  } else {
##    if(file.exists(file.path(ws.options$src.path,
##                             string))){
##      source(file.path(ws.options$src.path,
##                       string),
##             echo=F)
##    } else if(file.exists(file.path(ws.options$ws.root,
##                                    ws.options$src.path,
##                                    string))) {
##      source(file.path(ws.options$ws.root,
##                       ws.options$src.path,
##                       string), echo=F)
##    } else {stop("Can't allocate the DPT file")}
##  }
## }


## #... Load experiment configuration file
## get.src(string="dpt.methods.R",
##        base=file.path("http://dl.dropbox.com/u/10421230",
##          "dpt",
##          dpt.version),
##        ws.options=dpt.options[['ws']]
##        )  ## to be replaced by R package dptmethods

  ###... Samples
  sample.sheet <- opt$samplesheet
  samplesheet <- read.table(sample.sheet, as.is=T, header=F, strip.white=T, sep=",")
  mappedFiles <- samplesheet$V1
  sample.label.full <- samplesheet$V2
  subject.label.full <- samplesheet$V3
  stopifnot(all(samplesheet$V4%in%c("Y","N")))
  sample.select <- samplesheet$V4=="Y"
  sample.label <- sample.label.full[sample.select]
  subject.label <- subject.label.full[sample.select]

###... Load iDPT method parameters
## Probability mapping patterns
  events.res <- event.allPossiblePatterns(sample.label)
## Patterns to scan and identify
  events.wins <- event.patternsToScan(events.res,
                                      exclude=NULL,
                                      noZeroPatt=T)
  
###... Differential testing
  events.ctr <- construct.contrasts(events.wins)
  contrast.objs <- construct.contrast.objs(events.wins, events.ctr)

## scan parameters
  search.sites.cut <- rbind(matrix(rep(c(5e4,2e4), each=ncol(events.wins)-1), ncol=2),
                          c(2500, 2000))
  search.sites.cut.2 <- c(5000,2000)

###... call iDPT modules  
  call.js <- expression({
    .expr <- expression({
      logfile <- file.path(dpt.options[[c('ws','log.path')]],
                           paste(dpt.options[[c('logs','joinSample')]],'log',sep="."))
      logfile.con <- file(logfile, open="wt")
      sink(logfile.con)
      sink(logfile.con, type="message")
      merge.samples(file.path(get.ws.path("root"), ".."), mappedFiles)
      sink(type="message")
      sink()
    })
    cat("starting joinSamples", "\n")
    .trym <- try(eval(.expr), TRUE)
    if(is(.trym, "try-error")) {
      sink(type="message")
      sink()
      cat("error in joingSamples", "\n")
      if(opt$verbose>0) cat(.trym, "\n")
      return("failed")
    } else {
      cat("finishing joingSamples", "\n")
      return(NULL)
    }
  })

  call.mp <- expression({
    .expr <- expression({
      logfile <- file.path(dpt.options[[c('ws','log.path')]],
                           paste(dpt.options[[c('logs','mixPoisson')]],chr,'log',sep="."))
      logfile.con <- file(logfile, open="wt")
      sink(logfile.con)
      sink(logfile.con, type="message")
      ##chr <- get.str.chr()
      cat("read in data from Preprocess ...")
      count.table <- get.mappedCountTab()
      if(initfilter.TF) {
        init.filter.cutoff <- get.initfilter.cutoff(count.table, 0.5)
      } else {
        init.filter.cutoff <- initfilter.cutoff
      }
      
      get.reads.fromJoinedSamples(get.wsoption.path("joinSample"),
                                  chr,
                                  threshold=init.filter.cutoff,
                                  sample.select=sample.select)
      cat("\tdone\n")
      cat("mix-poisson deconvolution ...\n")
      controls <- list(msize=6000, burnin=1000, thin=1);
      priors.e <- list(d=c(0.3, 0.3, 0.3),
                       lambda.prior=list(a=1, b=1/20),
                       signal.prior=list(q=0.1,s=0.1))
      
      ncore <- min(ncore, length(s.cols))
      
      para.mixPois_eWin <- function(i.smp, priors, controls) {
        cat("start sample", i.smp, "\n")
        y1 <- reads[, i.smp+1]
        
        ## Run 2 (w/o initials)
        n <- length(y1)
        p0<- c(sum(y1==0)/n, sum(y1>0&y1<=4)/n, sum(y1>4)/n)
        lm0 <- c(mean(y1[which(y1>0&y1<=4)]), mean(y1[which(y1>4)]))
        signal0 <- y1*(1+1/25)
        
        initials.e <- list(p=p0, lambda=lm0[1], signal=signal0, r=1/25)
        res.i <- mixPois_eWin(y=y1,
                              initials=initials.e,
                              para.priors=priors,
                              controls=controls)
        return(res.i)
      }
      
      start.para(ncore, dptws.initexpr)
      sfExport(list=ls())
      
      res <- sfLapply(seq(along=s.cols),
                      para.mixPois_eWin,
                      priors=priors.e,
                      controls=controls)
      res <- res[!unlist(lapply(res, is.null))]
      
      stop.para()
      cat("mix-poisson deconvolution done\n")
      cat("cross-sample normalization based on posterior mean estimates ...") 
      compute.pmeans(res, reads.poi, reads.names, count.table)
      cat("\tdone\n")
      cat("save results to workspace ...")
      save.ws(ws="mixPoisson",
              what=c("chr", "res",
                "reads","reads.poi","reads.names",
                "pmeans","pmeans.norm","pmeans.bgnorm.norm","pmeans.bgcorrect.norm"),
              chr=chr)
      cat("\tdone\n")
      sink(type="message")
      sink()
    })
    cat("starting mixPoi:", chr, "\n")
    .trym <- try(eval(.expr), TRUE)
    if(is(.trym, "try-error")) {
      sink(type="message")
      sink()
      cat("error in mixPoi:", chr, "\n")
      if(opt$verbose>0) cat(.trym, "\n")
      return(chr)
    } else {
      cat("finishing mixPoi:", chr, "\n")  
      return(NULL)
    }
  })
  
  call.pr <- expression({
    .expr <- expression({
      logfile <- file.path(dpt.options[[c('ws','log.path')]],
                           paste(dpt.options[[c('logs','pattRecog')]],chr,'log',sep="."))
      logfile.con <- file(logfile, open="wt")
      sink(logfile.con)
      sink(logfile.con, type="message")
      
      
      ## Select which pmeans to use
      if(bg.process == "bgcorrect") {
        pmeans.norm.str <- "pmeans.bgcorrect.norm"
      } else if(bg.process == "bgnorm") {
        pmeans.norm.str <- "pmeans.bgnorm.norm"
      } else if(bg.process == "norm") {
        pmeans.norm.str <- "pmeans.norm"
      } else if(bg.process == "none") {
        pmeans.norm.str <- NULL
      }
      if(!is.null(pmeans.norm.str)) {
        get.ws(ws="mixPoisson",what=c("pmeans", "res", "reads.poi",pmeans.norm.str ))
        assign("pmeans.norm", get(pmeans.norm.str))
      } else {
        get.ws(ws="mixPoisson",what=c("pmeans", "res", "reads.poi"))
        pmeans.norm <- pmeans$mean
      }
      
      ## Parallel process
      start.para(ncore, dptws.initexpr)
      cat("calculate event scores ...\n")
      e.res <- events.score(res, events.res, reads.poi, sample.label,
                            npart=ncore,
                            which.score=which.escore,
                            para.mode=2)
      
      e.cut <- choose.events.cutoff(res,
                                    e.res,
                                    events.cut,
                                    events.cutoff.method)
      
      wins.sel <- win.select(e.res, e.cut,
                             exclude.zeroPattern=T, zeroPattern.cutoff=events.cut+cleanZeroPattern.cutoff)
      cat("scan sites ...\n")
      sites <- search.sites.v2(wins.sel, events.wins, chr, winsize,
                               search.sites.cut, cut.1st=search.sites.cut.2)
      
      ## Joining, shearing, and cleaning
      ranged.sites <- rangedSites(sites, winsize)
      gaps.site <- site.gaps(ranged.sites)
      cat("postprocess found sites ...\n")
      sites.s <-disjoin.sites(disjoin.sites(ranged.sites$sites.site,
                                            gaps.site$within,
                                            type="two.ends"),
                              gaps.site$within[width(gaps.site$within)>shear.gap])
      
      sites.j <- lapply(sites.s, reduce, min.gapwidth=join.gap+1)
      
      ## Clean the sites based on their pattern consistancy
      
      e.TF <- ranged.e.TF(e.res, winsize, cutoff=events.cut)
      
      sites.cleaned.1 <- sfLapply(names(sites.j),
                                  clean.sitePatt,
                                  sites=sites.j,
                                  e.TF=e.TF,
                                  cutoff=cleansite.cutoff)
      
      stop.para()
      
      names(sites.cleaned.1) <- names(sites.j)
      cat("cleanup sites with non-unique pattern ...\n")
      sites.cleaned <- IRangesList(lapply(clean.nonuniqPatt(sites.cleaned.1),
                                          function(x) ranges(x)[[1]]))
      
      
      wins.full <- ranges(e.TF)[[1]]
      sites.js <- format.sites4diffTest(wins.full, sites.cleaned, chr)
      
      ## Postprocess
      
      if(!is.null(bf.cutoff)) {
        bf.filtered <- bf.filter(lapply(pattern.events(e.res, events.wins),c),
                                 data.frame(sites.js, pattern.win=match.pattern.win(wins.sel, sites.js)),
                                 winsize,
                                 bf.cutoff,
                                 reads.poi,
                                 pmeans.norm,
                                 events.wins,
                                 which.score=bf.which.score)
        sites.js.ex <- bf.filtered$sites.js.ex
      } else {
        bf.filtered <- NULL
        sites.js.ex <- data.frame(sites.js, pattern.win=match.pattern.win(wins.sel, sites.js))
      }
      attr(sites.js.ex, 'patterns') <- events.wins
      
      ## Save workspace
      
      save.ws(ws="pattRecog",
              what=c("chr","sample.label","winsize",
                "sites", "sites.js.ex", "wins.sel", "e.res", "e.cut", "events.res", "events.wins",
                ##"pmeans", "pmeans.norm", "bf.filtered")
                "bf.filtered"),
              chr=chr
              )
      sink(type="message")
      sink()
    })
    cat("starting pattRecog:", chr, "\n")
    .trym <- try(eval(.expr), TRUE)
    if(is(.trym, "try-error")) {
      sink(type="message")
      sink()
      cat("error in pattRecog:", chr, "\n")
      if(opt$verbose>0) cat(.trym, "\n")
      return(chr)
    } else {
      cat("finishing pattRecog:", chr, "\n")
      return(NULL)
    }
  })
  
  call.dt <- expression({
    .expr <- expression({
      logfile <- file.path(dpt.options[[c('ws','log.path')]],
                           paste(dpt.options[[c('logs','diffTest')]],chr,'log',sep="."))
      logfile.con <- file(logfile, open="wt")
      sink(logfile.con)
      sink(logfile.con, type="message")
      testmode.rep <- anyDuplicated(subject.label)>0
      cat("testmode.rep=",testmode.rep,"\n")
      
      if(bg.process == "bgcorrect") {
        pmeans.norm.str <- "pmeans.bgcorrect.norm"
      } else if(bg.process == "bgnorm") {
        pmeans.norm.str <- "pmeans.bgnorm.norm"
      } else if(bg.process == "norm") {
        pmeans.norm.str <- "pmeans.norm"
      } else if(bg.process == "none") {
        pmeans.norm.str <- NULL
      }
      if(!is.null(pmeans.norm.str)) {
        get.ws(ws="mixPoisson",what=c("pmeans", "res", "reads.poi",pmeans.norm.str ))
        assign("pmeans.norm", get(pmeans.norm.str))
      } else {
        get.ws(ws="mixPoisson",what=c("pmeans", "res", "reads.poi"))
        pmeans.norm <- pmeans$mean
      }
      
      new.names <- paste("s", seq(ncol(pmeans$mean)), sep="")
      names(pmeans$mean) <- names(pmeans$var) <- new.names
      names(pmeans.norm) <- new.names
      
      get.ws("pattRecog",c("sites.js.ex", "events.wins"))
      
      ## Computation
      ##-- Parallel start
      start.para(ncore, dptws.initexpr)
      T.results <- process.diffTest(sites.js.ex,
                                    events.wins,
                                    events.ctr,
                                    testmode.rep,
                                    pmeans.norm,
                                    pmeans$var,
                                    ncore,
                                    sample.label,
                                    subject.label)
      attach(T.results)
      
      ##-- Parallel stop
      stop.para()
      ##--
      
      ##. Save workspace
      save.ws(ws="diffTest",
              what=c("chr",
                "pmeans", "pmeans.norm",
                "wins.test", "wins.test.report",
                "events.ctr", "events.wins"),
              chr=chr)
      sink(type="message")
      sink()
    })
    cat("starting diffTest:",chr, "\n")
    .trym <- try(eval(.expr), TRUE)
    if(is(.trym, "try-error")) {
      sink(type="message")
      sink()
      cat("error in diffTest:", chr, "\n")
      if(opt$verbose>0) cat(.trym, "\n")
      return(chr)
    } else {
      cat("finishing diffTest:", chr, "\n")
      return(NULL)
    }
  })
  
  call.rp <- expression({
    ##. Setup parameters
    .expr <- expression({
      logfile <- file.path(dpt.options[[c('ws','log.path')]],
                           paste(dpt.options[[c('logs','report')]],'log',sep="."))
      logfile.con <- file(logfile, open="wt")
      sink(logfile.con)
      sink(logfile.con, type="message")
      ##. Computation
      if(fragsizefilter.TF) {
        test.report.flat <- subset(get.difftest.flatReport(winsize=winsize),
                                   subset=end-start>=fragsizefilter.cutoff)
      } else test.report.flat <- get.difftest.flatReport(winsize=winsize)
      
      test.report.sigs <- select.sig.sites(test.report.flat,
                                           method.p.sel=diffTest.cutoff.method,
                                           cutoff.p.sel=diffTest.cutoff,
                                           cutoff.v.sel=NA,
                                           winsize=winsize,
                                           pvalue.var="p.value.1",
                                           value.var="value.1")
      
      ##. Save workspace
      
      save.ws(ws="report",
              what=c("test.report.flat", "test.report.sigs"))
      sink(type="message")
      sink()
    })
    cat("starting report","\n")
    .trym <- try(eval(.expr), TRUE)
    if(is(.trym, "try-error")) {
      sink(type="message")
      sink()
      cat("error in report","\n")
      if(opt$verbose>0) cat(.trym, "\n")
      return("failed")
    } else {
      cat("finishing report","\n")
      return(NULL)
    }
  })
  
  call.methyl <- expression({
    .expr <- expression({
      logfile <- file.path(dpt.options[[c('ws','log.path')]],
                           paste(dpt.options[[c('output','analysis')]],'log',sep="."))
      logfile.con <- file(logfile, open="wt")
      sink(logfile.con)
      sink(logfile.con, type="message")
      output.path <- dpt.options[[c('ws','analysis.path')]]
      
      get.ws("report", c("test.report.sigs"))
      cat("num of test.report.sigs:",dim(test.report.sigs)[1], "\n")
      print(with(test.report.sigs, table(pattern, contrast)))
      test.report.signs <- append.effectsign(test.report.sigs,
                                             col="value.1",
                                             colname="MethylType",
                                             signvalues=c("Hyper","Hypo"))
      
      print(table(test.report.signs[,c("pattern", "contrast", "MethylType")]))
      
      ##.. region identification
      
      test.regions.1 <- map.region(test.report.signs, gap=region.gap)
      
      ##.. futher correction to 0/1 decision 
      test.regions.2 <- cbind(test.regions.1, calculate.gmeans(test.regions.1))
      test.regions <- cleanup.on.gmeans(test.regions.2)
      test.regions$id <- paste(test.regions$chr,"s",seq(nrow(test.regions)), sep="_")
      table(test.regions[,c("pattern", "contrast", "MethylType")])
      report.file.prefix <- dpt.options[[c("output","analysis")]]
      report.file.postfix <- gsub("-", "",gsub(":","",gsub(" ","",Sys.time())))
      cat("### output site report\n")
      sel.vars <- c("chr","start","end","width",
                    "pattern","contrast","value.1","p.value.1","qvalue",
                    "id","r.id")
      sel.vars.rnm <- c("chr","start","end","width",
                        "pattern","contrast","ratio","p.value","qvalue",
                        "site.id","region.id")
      test.regions.short <- subset(test.regions, select=sel.vars)
      names(test.regions.short) <- sel.vars.rnm
      write.csv(test.regions.short,
                file=file.path(output.path,
                  paste(report.file.prefix,
                        report.file.postfix,
                        "sites",
                        "csv",
                        sep=".")),
                row.names=F)
      cat("### collect reads data for sites\n")
      ##. Reads data
      chrs <- get.ws.chrs('diffTest')
      attach.reads.worker <- function(i) {
        load.mixPoiData(chr=chrs[i])
        if(bg.process == "bgcorrect") {
          pmeans.norm.str <- "pmeans.bgcorrect.norm"
        } else if(bg.process == "bgnorm") {
          pmeans.norm.str <- "pmeans.bgnorm.norm"
        } else if(bg.process == "norm") {
          pmeans.norm.str <- "pmeans.norm"
        } else if(bg.process == "none") {
          pmeans.norm.str <- NULL
        }
        if(!is.null(pmeans.norm.str)) {
          assign("pmeans.norm", get(pmeans.norm.str))
        } else {
          pmeans.norm <- pmeans$mean
        }
        
        reads.pmeansn <- cbind(reads, pmeans.norm)
        names(reads.pmeansn) <- c(names(reads),
                                  paste('p', names(pmeans.norm),
                                        sep="."))
        test.regions.thischr <- test.regions[test.regions$chr==chrs[i],]
        reads.chr.df <- NULL
        if(nrow(test.regions.thischr)>0) {
          reads.chr.list <- apply(test.regions.thischr, 1, function(x) transform(s.id=x['id'],
                                                                                 get.reads(reads.pmeansn,
                                                                                           x['start'],
                                                                                           x['end'])))
          for(i in seq(along=reads.chr.list)) {
            reads.chr.df <- rbind(reads.chr.df, reads.chr.list[[i]])
          }
        }
        reads.chr.df 
      }
      
      start.para(ncore, dptws.initexpr)
      sfExport(list=c('chrs','test.regions'))
      reads.report.list <- sfLapply(seq(chrs), attach.reads.worker)
      stop.para()
      
      reads.report.df <- select.reads(ldply(reads.report.list, function(x) x))
      reads.report <- cbind(chr=sapply(as.character(reads.report.df$s.id),
                              function(x) strsplit(x, split="_")[[1]][1]),
                            reads.report.df[,c("poi", grep("^s",names(reads.report.df),v=T))])
      cat("### output site reads\n")
      
      write.csv(reads.report,
                file=file.path(output.path,
                  paste(report.file.prefix,
                        report.file.postfix,
                        "reads",
                        "csv",
                        sep=".")),
                row.names=F)
      save.ws(ws="analysis",
              what=c("test.report.signs",
                "test.regions.1","test.regions.2","test.regions",
                "chrs",
                "reads.report.df",
                "reads.report",
                "report.file.prefix",
                "report.file.postfix"),
              outpath=get.ws.path("report"))
      
      sink(type="message")
      sink()
    })
    cat("starting diffMethyl analysis ...", "\n")
    .trym <- try(eval(.expr), TRUE)
    if(is(.trym, "try-error")) {
      sink(type="message")
      sink()
      cat("error in diffMethyl analysis", "\n")
      if(opt$verbose>0) cat(.trym, "\n")
      return("failed")
    } else {
      cat("finishing diffMethyl analysis", "\n")
      return(NULL)
    }
  })
                                        #return(list(call.js, call.mp, call.pr,call.dt, call.rp, call.methyl, dpt.options))
})

