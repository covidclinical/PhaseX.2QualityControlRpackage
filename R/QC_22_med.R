#' @import data.table
#' @import dplyr
runQC_tab_med <- function(file.nm2, phase2.ClinicalCourse, phase2.Observations, phase1.DiagProcMed, output.dir) {
  #print("Checking Phase2.2 Medications ...")
  junk=tab_compare_med(phase2.Observations, phase2.ClinicalCourse, phase1.DiagProcMed)
  res=junk$res

  nm.duplicated=res[duplicated(res[,"medclass"]),c("medclass")]
  tryCatch(sink.txt("3. Medications\n\n", file=file.nm2, cat, append=T), error=function(e) NA)
  tryCatch(sink.txt("Checking duplicated rows:\n", file=file.nm2, cat, append=T), error=function(e) NA)
  if(length(nm.duplicated)!=0){
    #print(paste0("The following medications have duplicated rows: ", paste(nm.duplicated, collapse="; ")))
    tryCatch(sink.txt(paste(paste0(paste(nm.duplicated,collapse=";"), "\n"), collapse=""), file=file.nm2, cat, append=T), error=function(e) NA)}else{
      sink.txt("no issue identified", file=file.nm2, cat, append=T)
    }
  sink.txt("\n\n", file=file.nm2, cat, append=T)


  nm.medclass.all=NULL
  tryCatch(sink.txt(paste0("Checking differences between Phase1.2 and Phase2.2:\n\n"), file=file.nm2, cat, append=T), error=function(e) NA)

  for (nm in c("n_all_before", "n_all_since", "n_severe_before", "n_severe_since")){
    nm1=paste0("p1.", nm)
    if(grepl("before", nm)){nm2=paste0("p2.",nm, "0")}
    if(grepl("since", nm)){nm2=paste0("p2.",nm, "2")}

    nm2.1=paste0("p2.", nm, "1")
    nm2.2=paste0("p2.", nm, "2")

    nm.check=paste0("nm.diff.",nm)

    range.LB=pmin(res[,nm2.1]*0.975, res[,nm2.1]-5)
    range.UB=pmax(res[,nm2.2]*1.025, res[,nm2.2]+5)

    id.issue=which(res[,nm1]<range.LB|res[,nm1]>range.UB)

    tryCatch(sink.txt(paste0("Medication class with different ", nm, " between Phase1.2 and Phase2.2:\n"), file=file.nm2, cat, append=T), error=function(e) NA)
    if(length(id.issue)!=0){
      res.print=data.frame(res[id.issue,c("medclass", nm1, nm2)], range.LB[id.issue], range.UB[id.issue])
      colnames(res.print)=c("medclass", "Phase1.2", "Phase2.2", "Phase2.2.rangeL", "Phase2.2.rangeU")
      #print(paste0("Medication class with different ", nm, " between Phase1.2 and Phase2.2: "))
      #print(res.print)
      max.print <- getOption('max.print')
      options(max.print=nrow(res.print) * ncol(res.print))
      sink(file=file.nm2, append=T)
      print(noquote(as.matrix(res.print)))
      options(max.print=max.print)
          }else{
      sink.txt("no issue identified", file=file.nm2, cat, append=T)
    }
    sink.txt("\n\n", file=file.nm2, cat, append=T)
    nm.medclass.all=c(nm.medclass.all, res[id.issue,"medclass"])
  }
  is.error=length(c(nm.medclass.all,nm.duplicated))!=0
  is.error
}

#' @import data.table
#' @import dplyr
tab_compare_med=function(phase2.Observations, phase2.ClinicalCourse, phase1.DiagProcMed){
  dat=phase2.Observations[phase2.Observations$concept_type=="MED-CLASS",]
  dat.1=phase1.DiagProcMed[phase1.DiagProcMed$concept_type=="MED-CLASS",]
  dat.1$concept_code=as.character(dat.1$concept_code)
  dat$concept_code=as.character(dat$concept_code)
  nm.med.add=dat$concept_code[dat$concept_code%in%dat.1$concept_code!=1]
  patient_severe=phase2.ClinicalCourse[which(phase2.ClinicalCourse$severe==1),"patient_num"]
  nm.med=sort(unique(dat$concept_code))
  res.p2=NULL
  for(nm in nm.med[8]){
    n_all_before0=length(unique(dat[which(dat$days_since_admission<= (-15)   & dat$days_since_admission>= -365 & dat$concept_code==nm),"patient_num"]))
    n_severe_before0=length(unique(dat[which(dat$days_since_admission<= (-15)   & dat$days_since_admission>= -365 & dat$concept_code==nm & dat$patient_num%in%patient_severe),"patient_num"]))

    n_all_before1=length(unique(dat[which(dat$days_since_admission<= (-16)   & dat$days_since_admission>= -364 & dat$concept_code==nm),"patient_num"]))
    n_severe_before1=length(unique(dat[which(dat$days_since_admission<= (-16)   & dat$days_since_admission>= -364 & dat$concept_code==nm & dat$patient_num%in%patient_severe),"patient_num"]))

    n_all_before2=length(unique(dat[which(dat$days_since_admission<= (-14)   & dat$days_since_admission>= -366 & dat$concept_code==nm),"patient_num"]))
    n_severe_before2=length(unique(dat[which(dat$days_since_admission<= (-14)   & dat$days_since_admission>= -366 & dat$concept_code==nm & dat$patient_num%in%patient_severe),"patient_num"]))

    n_all_since1=length(unique(dat[which(dat$days_since_admission>0 & dat$concept_code==nm),"patient_num"]))
    n_severe_since1=length(unique(dat[which(dat$days_since_admission>0 & dat$concept_code==nm & dat$patient_num%in%patient_severe),"patient_num"]))

    n_all_since2=length(unique(dat[which(dat$days_since_admission>=0 & dat$concept_code==nm),"patient_num"]))
    n_severe_since2=length(unique(dat[which(dat$days_since_admission>=0 & dat$concept_code==nm & dat$patient_num%in%patient_severe),"patient_num"]))

    res.p2=rbind(res.p2, data.frame(medclass=nm,n_all_before0,n_all_before1,n_all_before2, n_all_since1, n_all_since2, n_severe_before0, n_severe_before1, n_severe_before2, n_severe_since1, n_severe_since2))
  }
  colnames(res.p2)[-1]=paste0("p2.", colnames(res.p2)[-1])
  res.p1=NULL

  for(nm in nm.med[8]){
    tmp=dat.1[which(dat.1$concept_code==nm), ]
    res.p1=rbind(res.p1, data.frame(medclass=nm,
                                    n_all_before=tmp[,"pts_all_before_adm"],
                                    n_all_since=tmp[,"pts_all_since_adm"],
                                    n_severe_before=tmp[,"pts_ever_severe_before_adm"],
                                    n_severe_since=tmp[,"pts_ever_severe_since_adm"]))
  }

  colnames(res.p1)=c("medclass", "p1.n_all_before", "p1.n_all_since", "p1.n_severe_before", "p1.n_severe_since")
  res=suppressMessages(left_join(res.p1, res.p2, by="medclass"))
  res=res[,c("medclass",
             "p1.n_all_before","p2.n_all_before0","p2.n_all_before1","p2.n_all_before2",
             "p1.n_all_since","p2.n_all_since1","p2.n_all_since2",
             "p1.n_severe_before","p2.n_severe_before0","p2.n_severe_before1","p2.n_severe_before2",
             "p1.n_severe_since","p2.n_severe_since1","p2.n_severe_since2"
  )]
  list(res=res, nm.med.add=nm.med.add)
}
