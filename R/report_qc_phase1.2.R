sink.txt=function(x, file, method=print, append){sink(file, append=append); method(x); sink()}

#' @importFrom utils read.csv
runQC_Phase1.2_report=function(dir.input, dir.output, select.all.cohorts=F, site.nm){
  set.seed(1234)
  ### read the data
  dat.DailyCounts=read.csv(paste0(dir.input,"/DailyCounts-",site.nm,".csv"))
  dat.ClinicalCourse=read.csv(paste0(dir.input,"/ClinicalCourse-",site.nm,".csv"))
  dat.AgeSex=read.csv(paste0(dir.input,"/AgeSex-",site.nm,".csv"))
  dat.DiagProcMed=read.csv(paste0(dir.input,"/DiagProcMed-",site.nm,".csv"))
  dat.Labs=read.csv(paste0(dir.input,"/Labs-",site.nm,".csv"))
  dat.LabCodes=read.csv(paste0(dir.input,"/LabCodes-",site.nm,".csv"))
  dat.RaceByLocalCode=read.csv(paste0(dir.input,"/RaceByLocalCode-",site.nm,".csv"))
  dat.RaceBy4CECode=read.csv(paste0(dir.input,"/RaceBy4CECode-",site.nm,".csv"))

  file.nm1 = paste0(dir.output, "/QC_report_phase1.2_", site.nm, "_", Sys.Date(), ".txt")

  race.list.all = c('asian','black','no_information','other','white','american_indian','hawaiian_pacific_islander')
  cohort.cat = c('PosAdm','U071Adm','NegAdm','PosNotAdm','U071NotAdm','NegNotAdm')
  quarter.cat = c('2020Q1','2020Q2','2020Q3','2020Q4','2021Q1','2021Q2')
  cohort.list.all = NULL
  for (c.cat in cohort.cat){
    for(q.cat in quarter.cat){
      cohort.list.all = c(cohort.list.all, paste0(c.cat,q.cat))
    }
  }
  cohort.all = Reduce(intersect, list(unique(dat.AgeSex$cohort),
                                      unique(dat.DailyCounts$cohort),
                                      unique(dat.ClinicalCourse$cohort),
                                      unique(dat.DiagProcMed$cohort),
                                      unique(dat.Labs$cohort),
                                      unique(dat.RaceByLocalCode$cohort),
                                      unique(dat.RaceBy4CECode$cohort)))
  cohort.all.PosAdm = cohort.all[which(substr(cohort.all,1,6)=='NegAdm')]
  cohort.all.rest = cohort.all[which(substr(cohort.all,1,6)!='NegAdm')]

  cohort.sample = tryCatch(c(sample(x=cohort.all.PosAdm, size =1), sample(x=cohort.all.rest, size=2, replace=F)), error=function(e) NA)

  sink.txt("\n\n", file=file.nm1, cat, append=T)
  tryCatch(sink.txt(paste0("\n\nPhase1.2 QC Report ",site.nm, "\n"), file=file.nm1, cat, append=F), error=function(e) NA)
  tryCatch(sink.txt(paste0("Generated ",Sys.Date(), "\n"), file=file.nm1, cat, append=T), error=function(e) NA)
  sink.txt(paste0("\n________________________________________________________________________________________________\n\n"), file=file.nm1, cat, append=T)

  sink.txt("Missing cohorts:\n",file=file.nm1, cat, append=T)
  cohort.miss = setdiff(cohort.list.all, cohort.all)
  if (is.null(cohort.miss)){sink.txt("no cohort missing\n",file=file.nm1, cat, append=T)}else{
    sink.txt(cohort.miss, file=file.nm1, cat, append=T)
  }
  sink.txt(paste0("\n\n________________________________________________________________________________________________\n\n"), file=file.nm1, cat, append=T)

  if (select.all.cohorts == T){set.cohort = cohort.all}else{set.cohort = cohort.sample}
  for (cohort.nm in set.cohort){
    dat.DailyCounts.c=dat.DailyCounts%>%filter(cohort==as.character(cohort.nm))
    dat.ClinicalCourse.c=dat.ClinicalCourse%>%filter(cohort==as.character(cohort.nm))
    dat.AgeSex.c=dat.AgeSex%>%filter(cohort==as.character(cohort.nm))
    dat.DiagProcMed.c=dat.DiagProcMed%>%filter(cohort==as.character(cohort.nm))
    dat.Labs.c=dat.Labs%>%filter(cohort==as.character(cohort.nm))
    dat.RaceByLocalCode.c=dat.RaceByLocalCode%>%filter(cohort==as.character(cohort.nm))
    dat.RaceBy4CECode.c=dat.RaceBy4CECode%>%filter(cohort==as.character(cohort.nm))

    print(as.character(cohort.nm))

    tryCatch(sink.txt(paste0('Cohort: ', cohort.nm), file=file.nm1, cat, append=T), error=function(e) NA)

    qc.res=qc_site(dat.DailyCounts.c, dat.ClinicalCourse.c, dat.AgeSex.c, dat.DiagProcMed.c,
                   dat.Labs.c, dat.RaceByLocalCode.c, dat.RaceBy4CECode.c, dat.LabCodes,
                   site.nm, cohort.list.all, race.list.all)


    colnames(qc.res$qc.grp$err.report)=
      colnames(qc.res$qc.col$err.report)=
      colnames(qc.res$qc.cros$err.report)=
      colnames(qc.res$qc.as$err.report)=
      colnames(qc.res$qc.cc$err.report)=
      colnames(qc.res$qc.dc$err.report)=
      colnames(qc.res$qc.dt$err.report)=
      colnames(qc.res$qc.dpm$err.report)=
      colnames(qc.res$qc.lab$err.report)=
      colnames(qc.res$qc.lab.val$err.report)=
      #colnames(qc.res$qc.lab.prev$err.report)=
      colnames(qc.res$qc.rc$err.report)=
      #colnames(qc.res$qc.rc.mis$err.report)=
      c("SiteID", "Possible Issues")


    tryCatch(sink.txt("\n\nColumn Names:\n", file=file.nm1, cat, append=T), error=function(e) NA)
    if(dim(qc.res$qc.col$err.report)[1]!=0){
      tryCatch(sink.txt(as.data.frame(qc.res$qc.col$err.report), file=file.nm1, print, append=T), error=function(e) NA)}else{
        sink.txt("no issue identified\n", file=file.nm1, cat, append=T)
      }
    tryCatch(sink.txt("\n\nCrossover:\n", file=file.nm1, cat, append=T), error=function(e) NA)
    if(dim(qc.res$qc.cros$err.report)[1]!=0){
      tryCatch(sink.txt(as.data.frame(qc.res$qc.cros$err.report), file=file.nm1, print, append=T), error=function(e) NA)}else{
        sink.txt("no issue identified\n", file=file.nm1, cat, append=T)
      }
    tryCatch(sink.txt("\n\nAgeSex:\n", file=file.nm1, cat, append=T), error=function(e) NA)
    if(dim(qc.res$qc.as$err.report)[1]!=0){
      tryCatch(sink.txt(as.data.frame(qc.res$qc.as$err.report), file=file.nm1, print, append=T), error=function(e) NA)}else{
        sink.txt("no issue identified\n", file=file.nm1, cat, append=T)
      }
    tryCatch(sink.txt("\n\nClinicalCourse:\n", file=file.nm1, cat, append=T), error=function(e) NA)
    if(dim(qc.res$qc.cc$err.report)[1]!=0){
      tryCatch(sink.txt(as.data.frame(qc.res$qc.cc$err.report), file=file.nm1, print, append=T), error=function(e) NA)}else{
        sink.txt("no issue identified\n", file=file.nm1, cat, append=T)
      }
    tryCatch(sink.txt("\n\nDailyCount:\n", file=file.nm1, cat, append=T), error=function(e) NA)
    if(dim(qc.res$qc.dc$err.report)[1]!=0){
      tryCatch(sink.txt(as.data.frame(qc.res$qc.dc$err.report), file=file.nm1, print, append=T), error=function(e) NA)}else{
        sink.txt("no issue identified\n", file=file.nm1, cat, append=T)
      }
    tryCatch(sink.txt("\n\nDailyCountDate:\n", file=file.nm1, cat, append=T), error=function(e) NA)
    if(dim(qc.res$qc.dt$err.report)[1]!=0){
      tryCatch(sink.txt(as.data.frame(qc.res$qc.dt$err.report), file=file.nm1, print, append=T), error=function(e) NA)}else{
        sink.txt("no issue identified\n", file=file.nm1, cat, append=T)
      }
    tryCatch(sink.txt("\n\nDiagProcMed:\n", file=file.nm1, cat, append=T), error=function(e) NA)
    if(dim(qc.res$qc.dpm$err.report)[1]!=0){
      tryCatch(sink.txt(as.data.frame(qc.res$qc.dpm$err.report), file=file.nm1, print, append=T), error=function(e) NA)}else{
        sink.txt("no issue identified\n", file=file.nm1, cat, append=T)
      }
    tryCatch(sink.txt("\n\nLabs:\n", file=file.nm1, cat, append=T))
    if(dim(qc.res$qc.lab$err.report)[1]!=0){
      tryCatch(sink.txt(as.data.frame(qc.res$qc.lab$err.report), file=file.nm1, print, append=T), error=function(e) NA)}else{
        sink.txt("no issue identified\n", file=file.nm1, cat, append=T)
      }
    tryCatch(sink.txt("\n\nLabs missing:\n", file=file.nm1, cat, append=T))
    tryCatch(sink.txt(as.data.frame(qc.res$qc.lab.mis$err.report), file=file.nm1, print, append=T), error=function(e) NA)

    tryCatch(sink.txt("\n\nLess than 33% of all patients have some of the important labs:\n", file=file.nm1, cat, append=T))
    tryCatch(sink.txt(as.data.frame(qc.res$qc.lab.prev$err.report), file=file.nm1, print, append=T), error=function(e) NA)

    tryCatch(sink.txt("\n\nLab units:\n",file=file.nm1, cat, append=T))
    if(dim(qc.res$qc.lab.val$err.report)[1]!=0 ){
      tryCatch(sink.txt(as.data.frame(qc.res$qc.lab.val$err.report), file=file.nm1, print, append=T), error=function(e) NA)}else{
        sink.txt("no issue identified\n", file=file.nm1, cat, append=T)
      }
    tryCatch(sink.txt("\n\nRace:\n",file=file.nm1, cat, append=T))
    if(dim(qc.res$qc.rc$err.report)[1]!=0){
      tryCatch(sink.txt(as.data.frame(qc.res$qc.rc$err.report), file=file.nm1, print, append=T), error=function(e) NA)}else{
        sink.txt("no issue identified\n", file=file.nm1, cat, append=T)
      }
    # tryCatch(sink.txt("\n\nMissing Race Category:\n",file=file.nm1, cat, append=T))
    # if(dim(qc.res$qc.rc.mis$err.report)[1]!=0 ){
    #   tryCatch(sink.txt(as.data.frame(qc.res$qc.rc.mis$err.report), file=file.nm1, print, append=T), error=function(e) NA)}else{
    #     sink.txt("no issue identified\n", file=file.nm1, cat, append=T)
    #   }
    sink.txt(paste0("\n\n ________________________________________________________________________________________________\n\n"), file=file.nm1, cat, append=T)

  }

  #qc.res
}
