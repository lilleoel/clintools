# library(xml2)
# iscus <- function(filename){
#
#    #Import file
#    xmlobj <- read_xml(filename)
#    vars <- NULL
#
#    #Extract as text
#    extract_vars <- function(x,y){
#       z <- xml_text(xml_find_all(x, y))
#       z[identical(z,character(0))] <- ""
#       return(z)
#    }
#
#    #General variables
#    vars$UniqueID <- extract_vars(xmlobj, ".//d1:UniqueID")
#    vars$Machine <- extract_vars(xmlobj, ".//d1:Machine")
#    vars$AdmissionDate <- extract_vars(xmlobj, ".//d1:AdmissionDate")
#    vars$AdmissionEndDate <- extract_vars(xmlobj, ".//d1:AdmissionEndDate")
#    vars$AdmissionNote <- extract_vars(xmlobj, ".//d1:AdmissionNote")
#
#    #Patient
#    vars$PatientID <- extract_vars(xmlobj, ".//d1:Patient/d1:PatientID")
#    vars$FirstName <- extract_vars(xmlobj, ".//d1:Patient/d1:FirstName")
#    vars$LastName <- extract_vars(xmlobj, ".//d1:Patient/d1:LastName")
#
#    #Numbers
#    xml_rec <- xml_find_all( xmlobj, ".//d1:Recording" )
#
#    df_results <- NULL
#
#    if(length(xml_rec) == 0) return(df_results)
#
#    rec <- NULL
#    for(i in c(1:length(xml_rec))){
#
#       rec$CatheterLocation <- xml_text(xml_find_all(xml_rec[i],".//d1:CatheterLocation"))
#
#       rec$AnalyteCode <- extract_vars(xml_rec[i],".//d1:AnalyteCode")
#       rec$Start <- extract_vars(xml_rec[i],".//d1:Start")
#       rec$Unit <- xml_attr(xml_rec[i], "Unit" )
#
#       rec$no <- sum(1*(xml_name(xml_children(xml_rec[i])) == "Measurement"))
#       rec$var <- unique(xml_name(xml_children(xml_find_all(xml_rec, ".//d1:Measurement"))))
#
#       measures_df <- NULL
#       for(j in c(1:rec$no)){
#
#          measures_vars <- NULL
#          for(k in measures_var){
#             xml_measure <- xml_find_all(xml_rec[i],".//d1:Measurement")[j]
#             measures_temp <- xml_text(xml_find_all(xml_measure, paste0(".//d1:",k)))
#
#             if(!identical(measures_temp, character(0))){
#                measures_vars <- c(measures_vars, measures_temp)
#             }else{
#                measures_vars <- c(measures_vars,NA)
#             }
#          }
#
#          measures_df <- as.data.frame(rbind(measures_df, measures_vars))
#
#       }
#       colnames(measures_df) <- rec$var
#
#       temp_df <- cbind(rec$CatheterLocation, rec$Start, rec$AnalyteCode, rec$Unit, measures_df)
#       colnames(temp_df)[1:4] <- c("CatheterLocation","Start","AnalyteCode","Unit")
#
#       df_results <- rbind(df_results,temp_df)
#    }
#
#    df_results <- cbind(vars$PatientID, vars$FirstName, vars$LastName,
#                        vars$UniqueID,vars$Machine,vars$AdmissionDate,vars$AdmissionEndDate,
#                        vars$AdmissionNote,
#                        df_results)
#    colnames(df_results) <- gsub("vars\\$","",colnames(df_results))
#
#    do_date <- function(x){
#       x <- as.POSIXct(gsub("T"," ",x), format="%Y-%m-%d %H:%M:%S")
#    }
#
#    df_results$AdmissionDate <- do_date(df_results$AdmissionDate)
#    df_results$AdmissionEndDate <- do_date(df_results$AdmissionEndDate)
#    df_results$Start <- do_date(df_results$Start)
#    df_results$TimeStamp <- do_date(df_results$TimeStamp)
#
#    if(is.null(df_results$Status)){ df_results$Status <- NA }
#    df_results <- df_results[,!is.na(colnames(df_results))]
#
#    rownames(df_results) <- c(1:nrow(df_results))
#
#    return(df_results)
#
# }
