#' @import data.table
#' @import dplyr
runQC_tab_cc <- function(file.nm2, phase2.ClinicalCourse, phase1.ClinicalCourse, output.dir) {
  #print("Checking Phase2.2 ClinicalCourse ...")
  res=tab_compare_cc(phase2.ClinicalCourse, phase1.ClinicalCourse)
  nm.duplicated=res[duplicated(res[,"days_since_admission"]),c("days_since_admission")]
  tryCatch(sink.txt("5. ClinicalCourse\n\n", file=file.nm2, cat, append=T), error=function(e) NA)
  tryCatch(sink.txt("Checking duplicated rows:\n", file=file.nm2, cat, append=T), error=function(e) NA)
  if(length(nm.duplicated)!=0){
    tryCatch(sink.txt(paste0(paste(nm.duplicated,collapse=";"), "\n"), file=file.nm2, cat, append=T), error=function(e) NA)}else{
      sink.txt("no issue identified", file=file.nm2, cat, append=T)
    }
  sink.txt("\n\n", file=file.nm2, cat, append=T)

  tryCatch(sink.txt(paste0("Checking differences between Phase1.2 and Phase2.2:\n\n"), file=file.nm2, cat, append=T), error=function(e) NA)

  for (nm in c("num_patients_all_still_in_hospital",
               "num_patients_ever_severe_still_in_hospital")){
    nm1=paste0("p1.", nm)
    nm2=paste0("p2.", nm)
    nm.check=paste0("nm.diff.",nm)
    #nm.day0=(res[res$days_since_admission==0,nm1]<res[res$days_since_admission==0,nm2]-5) * (res[res$days_since_admission==0,nm1]>res[res$days_since_admission==0,nm2]+5)
    nm.day0=1
    if(nm=="num_patients_all_still_in_hospital"){nm.print="total numbers of patient"}
    if(nm=="num_patients_ever_severe_still_in_hospital"){nm.print="total numbers of ever severe patient"}

    tryCatch(sink.txt(paste0("ClinicalCourse with different ",nm.print," between Phase1.1 and Phase2.1:\n"), file=file.nm2, cat, append=T), error=function(e) NA)
    if(nm.day0!=0){
      res.print=res[res$days_since_admission==0, c(nm1, nm2)]
      colnames(res.print)=c("phase1", "phase2")
      #print(res.print)
      tryCatch(sink.txt(paste(apply(res.print,1, function(ll) paste0(paste(paste0(colnames(res.print), "=", ll),collapse="; "), "\n\n")), collapse=""), file=file.nm2, cat, append=T), error=function(e) NA)}else{
        sink.txt("no issue identified\n", file=file.nm2, cat, append=T)
      }
  }

  is.error=(length(c(nm.duplicated))+nm.day0)!=0
  is.error
}

tab_compare_cc=function(phase2.ClinicalCourse, phase1.ClinicalCourse){
  res.p2=NULL
  patient_ever_severe=unique(phase2.ClinicalCourse[phase2.ClinicalCourse$severe==1,"patient_num"])
  for(myday in phase1.ClinicalCourse[,"days_since_admission"]){
    num_patients_all_still_in_hospital=length(unique(phase2.ClinicalCourse[which(phase2.ClinicalCourse[,"days_since_admission"]==myday & phase2.ClinicalCourse[,"in_hospital"]==1),"patient_num"]))
    num_patients_ever_severe_still_in_hospital=length(unique(phase2.ClinicalCourse[which(phase2.ClinicalCourse[,"days_since_admission"]==myday & phase2.ClinicalCourse[,"in_hospital"]==1 & phase2.ClinicalCourse[,"patient_num"]%in%patient_ever_severe==1),"patient_num"]))
    res.p2=rbind(res.p2,c(num_patients_all_still_in_hospital,num_patients_ever_severe_still_in_hospital))
  }
  res=cbind(phase1.ClinicalCourse[,c("days_since_admission",
                                     "pts_all_in_hosp",
                                     "pts_ever_severe_in_hosp")], res.p2)
  colnames(res)[-1]=c("p1.num_patients_all_still_in_hospital","p1.num_patients_ever_severe_still_in_hospital",
                      "p2.num_patients_all_still_in_hospital","p2.num_patients_ever_severe_still_in_hospital")
  res=res[,c("days_since_admission",
             "p1.num_patients_all_still_in_hospital","p2.num_patients_all_still_in_hospital",
             "p1.num_patients_ever_severe_still_in_hospital","p2.num_patients_ever_severe_still_in_hospital")]
  res
}


