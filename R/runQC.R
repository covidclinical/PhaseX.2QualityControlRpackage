#' Conducts QC for Phase2 Data and Generates QC Reports
#'
#' @keywords 4CE Phase2 Project
#' @param dir.input1.2 path that contains phase 1.2 data
#' @param dir.input2.2 path that contains phase 2.2 data, NA if 2.2 data is not available
#' @param dir.output path to save the QC report
#' @param site.nm 4CE site ID (string)
#' @return QC file
#' @export
runQC <- function(dir.input1.2, dir.input2.2=NA, dir.output, site.nm){

  x=dir.input1.2
  # QC for Phase1.2
  # print("Generating Phase 1.2 QC report ...")
  # runQC_Phase1.2_report(dir.input1.2, dir.output, site.nm)

  # QC for Phase2.2
  if(is.na(dir.input2.2)==FALSE){
    print("Generating Phase 2.2 QC report ...")
    runQC_Phase2.2_report(dir.input1.2, dir.input2.2, dir.output, site.nm)
    }else{print("Phase 2.2 data is not provided")}

}
