#' @import data.table
#' @import dplyr
#' @import utils
#' @import stringr
runQC_Phase2.2_report=function(dir.input1.2, dir.input2.2, dir.output, select.all.cohorts=F, site.nm){
  set.seed(1234)
  output.dir = dir.output
  file.nm2=paste0(dir.output, "/QC_report_phase2.2_", site.nm, "_", Sys.Date(), ".txt")

  phase1.DailyCounts=read.csv(paste0(dir.input1.2,"/DailyCounts-",site.nm,".csv"))
  phase1.ClinicalCourse=read.csv(paste0(dir.input1.2,"/ClinicalCourse-",site.nm,".csv"))
  phase1.AgeSex=read.csv(paste0(dir.input1.2,"/AgeSex-",site.nm,".csv"))
  phase1.DiagProcMed=read.csv(paste0(dir.input1.2,"/DiagProcMed-",site.nm,".csv"))
  phase1.Labs=read.csv(paste0(dir.input1.2,"/Labs-",site.nm,".csv"))
  phase1.LabCodes=read.csv(paste0(dir.input1.2,"/LabCodes-",site.nm,".csv"))
  phase1.RaceByLocalCode=read.csv(paste0(dir.input1.2,"/RaceByLocalCode-",site.nm,".csv"))
  #phase1.RaceBy4CECode=read.csv(paste0(dir.input1.2,"/RaceBy4CECode-",site.nm,".csv"))

  phase2.ClinicalCourse=read.csv(paste0(dir.input2.2, "/LocalPatientClinicalCourse.csv"))
  phase2.Observations=read.csv(paste0(dir.input2.2, "/LocalPatientObservations.csv"))
  phase2.Summary=read.csv(paste0(dir.input2.2, "/LocalPatientSummary.csv"))
  #phase2.Race=tryCatch(read.csv(paste0(dir.input2.2, "/LocalPatientRace.csv")), error=function(e) NA)

  colnames(phase1.DailyCounts)=tolower(colnames(phase1.DailyCounts))
  colnames(phase1.ClinicalCourse)=tolower(colnames(phase1.ClinicalCourse))
  colnames(phase1.AgeSex)=tolower(colnames(phase1.AgeSex))
  colnames(phase1.DiagProcMed)=tolower(colnames(phase1.DiagProcMed))
  colnames(phase1.Labs)=tolower(colnames(phase1.Labs))
  colnames(phase1.LabCodes)=tolower(colnames(phase1.LabCodes))
  #colnames(phase1.RaceByLocalCode)=tolower(colnames(phase1.RaceByLocalCode))
  #colnames(phase1.RaceBy4CECode)=tolower(colnames(phase1.RaceBy4CECode))

  colnames(phase2.ClinicalCourse)=tolower(colnames(phase2.ClinicalCourse))
  colnames(phase2.Observations)=tolower(colnames(phase2.Observations))
  colnames(phase2.Summary)=tolower(colnames(phase2.Summary))
  #colnames(phase2.Race)=tolower(colnames(phase2.Race))

  sink.txt("\n\n", file=file.nm2, cat, append=T)
  tryCatch(sink.txt(paste0("\n\nPhase2.2 QC Report ",site.nm, "\n"), file=file.nm2, cat, append=F), error=function(e) NA)
  tryCatch(sink.txt(paste0("Generated ",Sys.Date(), "\n"), file=file.nm2, cat, append=T), error=function(e) NA)
  sink.txt(paste0("\n________________________________________________________________________________________________\n\n"), file=file.nm2, cat, append=T)

  Phase2QC_colnames=err_report_colnames_site.phase2(phase2.ClinicalCourse, phase2.Observations, phase2.Summary, phase2.Race=NA, site.nm)

  tryCatch(sink.txt("Column names:\n", file=file.nm2, cat, append=T), error=function(e) NA)
  if(dim(Phase2QC_colnames$err.report)[1]!=0){
    tryCatch(sink.txt(paste(as.data.frame(Phase2QC_colnames$err.report)$label, collapse=";\n"), file=file.nm2, cat, append=T), error=function(e) NA)
  }else{
    sink.txt("no issue identified\n\n", cat, file=file.nm2,append=T)
  }

  cohort.cat = c('PosAdm','U071Adm','NegAdm','PosNotAdm','U071NotAdm','NegNotAdm')
  quarter.cat = c('2020Q1','2020Q2','2020Q3','2020Q4','2021Q1','2021Q2', '2021Q3', '2021Q4', '2022Q1', '2022Q2')
  cohort.list.all = NULL
  for (c.cat in cohort.cat){
    for(q.cat in quarter.cat){
      cohort.list.all = c(cohort.list.all, paste0(c.cat,q.cat))
    }
  }

  cohort.all = Reduce(intersect, list(unique(phase2.ClinicalCourse$cohort),
                                      unique(phase2.Observations$cohort),
                                      unique(phase2.Summary$cohort)))
                                      #unique(phase2.Race$cohort)))
  cohort.all.PosAdm = cohort.all[which(substr(cohort.all,1,6)=='PosAdm')]
  cohort.all.rest = cohort.all[which(substr(cohort.all,1,6)!='PosAdm')]

  cohort.sample = tryCatch(c(sample(x=cohort.all.PosAdm, size =1), sample(x=cohort.all.rest, size=2, replace=F)), error=function(e) NA)

  sink.txt("Missing cohorts:\n",file=file.nm2, cat, append=T)
  cohort.miss = setdiff(cohort.list.all, cohort.all)
  if (is.null(cohort.miss)){sink.txt("no cohort missing",file=file.nm2, cat, append=T)
    }else{
    cohort.miss.summary = setdiff(unique(sub("20.*", "", cohort.miss)), unique(sub("20.*", "", cohort.all)))
    sink.txt(cohort.miss.summary, file=file.nm2, cat, append=T)
    sink.txt("\n\nMissing cohorts detail:\n",file=file.nm2, cat, append=T)
    sink.txt(cohort.miss, file=file.nm2, cat, append=T)
  }
  # if (is.na(phase2.Race)==TRUE){
  #   sink.txt("\n\nThe file LocalPatientRace.csv is missing",file=file.nm2, cat, append=T)
  # }
  #sink.txt("\n\nChecking for presence of NA, NaN, Inf in the data\n",file=file.nm2, cat, append=T)
  #Phase2QC_NA = run_qc_na.phase2(phase2.ClinicalCourse, phase2.Observations, phase2.Summary, output.dir)
  sink.txt(paste0("\n\n________________________________________________________________________________________________\n\n"), file=file.nm2, cat, append=T)
  Phase2QC_Tab_frequency = run_qc_tab_frequency.phase2(file.nm2, phase2.Observations, phase1.AgeSex, select.all.cohorts=TRUE, output.dir)
  sink.txt(paste0("\n\n________________________________________________________________________________________________\n\n"), file=file.nm2, cat, append=T)

  if (select.all.cohorts == T){set.cohort = cohort.all}else{set.cohort = cohort.sample}
  for (cohort.nm in set.cohort){
    print(cohort.nm)
    phase1.DailyCounts.c=phase1.DailyCounts%>%filter(cohort==as.character(cohort.nm))
    phase1.ClinicalCourse.c=phase1.ClinicalCourse%>%filter(cohort==as.character(cohort.nm))
    phase1.AgeSex.c=phase1.AgeSex%>%filter(cohort==as.character(cohort.nm))
    phase1.DiagProcMed.c=phase1.DiagProcMed%>%filter(cohort==as.character(cohort.nm))
    phase1.Labs.c=phase1.Labs%>%filter(cohort==as.character(cohort.nm))
    #phase1.RaceByLocalCode.c=phase1.RaceByLocalCode%>%filter(cohort==as.character(cohort.nm))
    #phase1.RaceBy4CECode.c=phase1.RaceBy4CECode%>%filter(cohort==as.character(cohort.nm))

    phase2.ClinicalCourse.c=phase2.ClinicalCourse%>%filter(cohort==as.character(cohort.nm))
    phase2.Observations.c=phase2.Observations%>%filter(cohort==as.character(cohort.nm))
    phase2.Summary.c=phase2.Summary%>%filter(cohort==as.character(cohort.nm))
    #phase2.Race.c=tryCatch(phase2.Race%>%filter(cohort==as.character(cohort.nm)), error=function(e) NA)

    tryCatch(sink.txt(paste0('Cohort: ', cohort.nm, '\n\n'), file=file.nm2, cat, append=T), error=function(e) NA)
    sink.txt(paste0("\n\n....................................................................................................\n\n"), file=file.nm2, cat, append=T)

    Phase2QC_Tab_Demographic=runQC_tab_dem(file.nm2, phase2.Summary.c, phase2.Observations.c, phase1.AgeSex.c, output.dir)
    sink.txt(paste0("\n\n....................................................................................................\n\n"), file=file.nm2, cat, append=T)
    #sink.txt(paste0("________________________________________________\n"), file=file.nm2, cat, append=T)

    Phase2QC_Tab_Labs=runQC_tab_lab(file.nm2, phase2.ClinicalCourse.c, phase2.Observations.c, phase1.Labs.c, output.dir)
    sink.txt(paste0("\n\n....................................................................................................\n\n"), file=file.nm2, cat, append=T)
    #sink.txt(paste0("________________________________________________\n"), file=file.nm2, cat, append=T)

    Phase2QC_Tab_Medications=tryCatch(runQC_tab_med(file.nm2, phase2.ClinicalCourse.c, phase2.Observations.c, phase1.DiagProcMed.c, output.dir), error=function(e) NA)
    sink.txt(paste0("\n\n....................................................................................................\n\n"), file=file.nm2,cat, append=T)
    #sink.txt(paste0("________________________________________________\n"), file=file.nm2,cat, append=T)

    Phase2QC_Tab_Diagnoses=runQC_tab_diag(file.nm2, phase2.ClinicalCourse.c, phase2.Observations.c, phase1.DiagProcMed.c, output.dir)
    sink.txt(paste0("\n\n....................................................................................................\n\n"), file=file.nm2, cat, append=T)
    #sink.txt(paste0("________________________________________________\n"), file=file.nm2, cat, append=T)

    Phase2QC_Tab_ClinicalCourse=runQC_tab_cc(file.nm2, phase2.ClinicalCourse.c, phase1.ClinicalCourse.c, output.dir)
    #sink.txt(paste0("\n\n....................................................................................................\n\n"), file=file.nm2, cat, append=T)
    #sink.txt(paste0("________________________________________________\n"), file=file.nm2, cat, append=T)

    # Phase2QC_Tab_frequency=run_qc_tab_frequency.phase2(file.nm2,phase2.Observations.c, phase1.AgeSex.c, select.all.cohorts = FALSE, output.dir)
    # sink.txt(paste0("\n\n________________________________________________________________________________________________\n\n"), file=file.nm2, cat, append=T)
    # #Phase2QC_Tab_Labs+Phase2QC_Tab_Medications+Phase2QC_Tab_Diagnoses+Phase2QC_Tab_Demographic+Phase2QC_Tab_ClinicalCourse

  }
}
