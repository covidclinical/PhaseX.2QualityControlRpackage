#' @import data.table
#' @import dplyr
runQC_tab_diag <- function(file.nm2, phase2.ClinicalCourse, phase2.Observations, phase1.DiagProcMed, output.dir) {
  #print("Checking Phase2.2 Diagnoses")
  res=tab_compare_diag(phase2.Observations, phase2.ClinicalCourse, phase1.DiagProcMed)
  nm.duplicated=res[duplicated(res[,"diag-icd"]),c("diag-icd")]
  tryCatch(sink.txt("4. Diagnoses\n\n", file=file.nm2, cat, append=T), error=function(e) NA)
  tryCatch(sink.txt("Checking duplicated rows:\n", file=file.nm2, cat, append=T), error=function(e) NA)
  if(length(nm.duplicated)!=0){
    #print(paste0("Duplicated rows for: ", nm.duplicated))
    tryCatch(sink.txt(paste0(paste(nm.duplicated,collapse=";"), "\n"), file=file.nm2, cat, append=T), error=function(e) NA)}else{
      sink.txt("no issue identified", file=file.nm2, cat, append=T)
    }
  sink.txt("\n\n", file=file.nm2, cat, append=T)

  tryCatch(sink.txt(paste0("Checking differences between Phase1.2 and Phase2.2:\n\n"), file=file.nm2, cat, append=T), error=function(e) NA)

  nm.diag.all=NULL
  for (nm in c("n_all_before", "n_all_since", "n_severe_before", "n_severe_since")){
    nm1=paste0("p1.", nm)
    if(grepl("before", nm)){nm2=paste0("p2.", nm, 0)}
    if(grepl("since", nm)){nm2=paste0("p2.", nm, 2)}

    nm2.1=paste0("p2.", nm,1)
    nm2.2=paste0("p2.", nm,2)

    nm.check=paste0("nm.diff.",nm)
    range.LB=pmin(res[,nm2.1]*0.975,res[,nm2.1]-5)
    range.UB=pmax(res[,nm2.2]*1.025, res[,nm2.2]+5)
    id.issue=which(res[,nm1]<range.LB|res[,nm1]>range.UB)

     if(length(id.issue)!=0){
      res.print=data.frame(res[id.issue, c("diag-icd", nm1, nm2)], range.LB[id.issue], range.UB[id.issue])
      colnames(res.print)=c("icd", "phase1", "phase2", "phase2.rangeL", "phase2.rangeU")
      tryCatch(sink.txt(paste0("Diagnosis codes with different ", nm, " between Phase1.2 and Phase2.2:\n"), file=file.nm2, cat, append=T), error=function(e) NA)

      #print(res.print)
      tryCatch(sink.txt(paste(apply(res.print,1, function(ll) paste(paste0(colnames(res.print), "=", ll),collapse="; ")), collapse="\n"), file=file.nm2, cat, append=T), error=function(e) NA)
    }else{sink.txt("no issue identified", file=file.nm2, cat, append=T)}
    sink.txt("\n\n", file=file.nm2, cat, append=T)

    nm.diag.all=c(nm.diag.all, res[id.issue,"diag-icd"])
  }
  is.error=length(c(nm.diag.all,nm.duplicated))!=0
  is.error
}

tab_compare_diag=function(phase2.Observations, phase2.ClinicalCourse, phase1.DiagProcMed){
  dat=phase2.Observations[phase2.Observations$concept_type%in%c("DIAG-ICD10", "DIAG-ICD9"),]
  dat.1=phase1.DiagProcMed[phase1.DiagProcMed$concept_type%in%c("DIAG-ICD10", "DIAG-ICD9"),]
  dat$concept_code=as.character(dat$concept_code)
  patient_severe=phase2.ClinicalCourse[which(phase2.ClinicalCourse$severe==1),"patient_num"]
  nm.diag=sort(unique(dat$concept_code))

  tmp.all.before0=dat[which(dat$days_since_admission<= (-15) & dat$days_since_admission>=-365),c("patient_num", "concept_code")]
  tmp.all.before0=tmp.all.before0[duplicated(tmp.all.before0)!=1,]
  n_all_before0=table(tmp.all.before0[,"concept_code"])
  n_all_before0=data.frame(n_all_before0)
  n_all_before0=suppressMessages(left_join(data.frame(Var1=nm.diag), n_all_before0, by="Var1"))

  tmp.all.before1=dat[which(dat$days_since_admission<= (-16) & dat$days_since_admission>=-364),c("patient_num", "concept_code")]
  tmp.all.before1=tmp.all.before1[duplicated(tmp.all.before1)!=1,]
  n_all_before1=table(tmp.all.before1[,"concept_code"])
  n_all_before1=data.frame(n_all_before1)
  n_all_before1=suppressMessages(left_join(data.frame(Var1=nm.diag), n_all_before1, by="Var1"))

  tmp.all.before2=dat[which(dat$days_since_admission<= (-14) & dat$days_since_admission>=-366),c("patient_num", "concept_code")]
  tmp.all.before2=tmp.all.before2[duplicated(tmp.all.before2)!=1,]
  n_all_before2=table(tmp.all.before2[,"concept_code"])
  n_all_before2=data.frame(n_all_before2)
  n_all_before2=suppressMessages(left_join(data.frame(Var1=nm.diag), n_all_before2, by="Var1"))


  tmp.all.since1=dat[dat$days_since_admission>0,c("patient_num", "concept_code")]
  tmp.all.since1=tmp.all.since1[duplicated(tmp.all.since1)!=1,]
  n_all_since1=table(tmp.all.since1[,"concept_code"])
  n_all_since1=data.frame(n_all_since1)
  n_all_since1=suppressMessages(left_join(data.frame(Var1=nm.diag), n_all_since1, by="Var1"))

  tmp.all.since2=dat[dat$days_since_admission>=0,c("patient_num", "concept_code")]
  tmp.all.since2=tmp.all.since2[duplicated(tmp.all.since2)!=1,]
  n_all_since2=table(tmp.all.since2[,"concept_code"])
  n_all_since2=data.frame(n_all_since2)
  n_all_since2=suppressMessages(left_join(data.frame(Var1=nm.diag), n_all_since2, by="Var1"))

  tmp.severe.before0=dat[which(dat$days_since_admission<= (-15) & dat$days_since_admission>=-365 & dat$patient_num%in%patient_severe),c("patient_num", "concept_code")]
  tmp.severe.before0=tmp.severe.before0[duplicated(tmp.severe.before0)!=1,]
  n_severe_before0=table(tmp.severe.before0[,"concept_code"])
  n_severe_before0=data.frame(n_severe_before0)
  n_severe_before0=suppressMessages(left_join(data.frame(Var1=nm.diag), n_severe_before0, by="Var1"))

  tmp.severe.before1=dat[which(dat$days_since_admission<= (-16) & dat$days_since_admission>=-364 & dat$patient_num%in%patient_severe),c("patient_num", "concept_code")]
  tmp.severe.before1=tmp.severe.before1[duplicated(tmp.severe.before1)!=1,]
  n_severe_before1=table(tmp.severe.before1[,"concept_code"])
  n_severe_before1=data.frame(n_severe_before1)
  n_severe_before1=suppressMessages(left_join(data.frame(Var1=nm.diag), n_severe_before1, by="Var1"))

  tmp.severe.before2=dat[which(dat$days_since_admission<= (-14) & dat$days_since_admission>=-366 & dat$patient_num%in%patient_severe),c("patient_num", "concept_code")]
  tmp.severe.before2=tmp.severe.before2[duplicated(tmp.severe.before2)!=1,]
  n_severe_before2=table(tmp.severe.before2[,"concept_code"])
  n_severe_before2=data.frame(n_severe_before2)
  n_severe_before2=suppressMessages(left_join(data.frame(Var1=nm.diag), n_severe_before2, by="Var1"))


  tmp.severe.since1=dat[dat$days_since_admission>0 & dat$patient_num%in%patient_severe,c("patient_num", "concept_code")]
  tmp.severe.since1=tmp.severe.since1[duplicated(tmp.severe.since1)!=1,]
  n_severe_since1=table(tmp.severe.since1[,"concept_code"])
  n_severe_since1=data.frame(n_severe_since1)
  n_severe_since1=suppressMessages(left_join(data.frame(Var1=nm.diag), n_severe_since1, by="Var1"))

  tmp.severe.since2=dat[dat$days_since_admission>=0& dat$patient_num%in%patient_severe,c("patient_num", "concept_code")]
  tmp.severe.since2=tmp.severe.since2[duplicated(tmp.severe.since2)!=1,]
  n_severe_since2=table(tmp.severe.since2[,"concept_code"])
  n_severe_since2=data.frame(n_severe_since2)
  n_severe_since2=suppressMessages(left_join(data.frame(Var1=nm.diag), n_severe_since2, by="Var1"))


  res.p2=cbind(n_all_before0,n_all_before1[,2],n_all_before2[,2], n_all_since1[,2], n_all_since2[,2],n_severe_before0[,2],n_severe_before1[,2],n_severe_before2[,2], n_severe_since1[,2], n_severe_since2[,2])
  colnames(res.p2)=c("diag-icd","p2.n_all_before0","p2.n_all_before1","p2.n_all_before2","p2.n_all_since1","p2.n_all_since2","p2.n_severe_before0","p2.n_severe_before1","p2.n_severe_before2","p2.n_severe_since1","p2.n_severe_since2")
  res.p2[is.na(res.p2)]=0

  #res.p1=phase1.Diagnoses[, -c(1,3)]
  res.p1=dat.1 %>% select(concept_code, pts_all_before_adm, pts_all_since_adm,
                          pts_ever_severe_before_adm, pts_ever_severe_since_adm)
  colnames(res.p1)=c("diag-icd", "p1.n_all_before", "p1.n_all_since", "p1.n_severe_before", "p1.n_severe_since")
  res=suppressMessages(left_join(res.p1, res.p2, by="diag-icd"))
  res=res[,c("diag-icd",
             "p1.n_all_before","p2.n_all_before0","p2.n_all_before1","p2.n_all_before2",
             "p1.n_all_since","p2.n_all_since1","p2.n_all_since2",
             "p1.n_severe_before","p2.n_severe_before0","p2.n_severe_before1","p2.n_severe_before2",
             "p1.n_severe_since","p2.n_severe_since1","p2.n_severe_since2"
  )]
  res
}
