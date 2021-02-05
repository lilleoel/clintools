setwd("C:/Oel/Artikler/PhD - CPP crisis/Data/MD/Data")
library(xml2)
library(dplyr)

results <- NULL
dirs <- list.dirs(full.names = FALSE)[-1]
for(dir in dirs){

   files <- list.files(path=paste0(getwd(),"/",dir))
   for(file in files){

      xmlobj <- read_xml(paste0(getwd(),"/",dir,"/", file))
      vars <- NULL

      #General variables
      vars$id <- xml_find_all( xmlobj, ".//d1:Patient/d1:PatientID" ) %>% xml_text()
      vars$firstname <- xml_find_all( xmlobj, ".//d1:Patient/d1:FirstName" ) %>% xml_text()
      vars$lastname <- xml_find_all( xmlobj, ".//d1:Patient/d1:LastName" ) %>% xml_text()
      vars$AdmissionDate <- xml_find_all( xmlobj, ".//d1:AdmissionDate" ) %>% xml_text()

      xml_rec <- xml_find_all( xmlobj, ".//d1:Recording" )

      if(length(xml_rec) == 0) next

      df_results <- NULL
      for(i in c(1:length(xml_rec))){

         vars$CatheterLocation <- xml_find_all( xml_rec[i], ".//d1:CatheterLocation" ) %>% xml_text()
         vars$AnalyteCode <- xml_find_all( xml_rec[i], ".//d1:AnalyteCode" ) %>% xml_text()
         vars$Start <- xml_find_all( xml_rec[i], ".//d1:Start" ) %>% xml_text()
         vars$Unit <- xml_attr(xml_rec[i], "Unit" )

         measures_no <- sum(1*(xml_name(xml_children(xml_rec[i])) == "Measurement"))
         measures_var <- unique(xml_name(xml_children(xml_find_all(xml_rec, ".//d1:Measurement"))))


         measures_df <- NULL
         for(j in c(1:measures_no)){

            measures_vars <- NULL
            for(k in measures_var){

               xml_measure <- xml_find_all( xml_rec[i], ".//d1:Measurement")[j]
               measures_temp <- xml_find_all( xml_measure, paste0(".//d1:",k) ) %>% xml_text()

               if(!identical(measures_temp, character(0))){
                  measures_vars <- c(measures_vars, measures_temp)
               }else{
                  measures_vars <- c(measures_vars,NA)
               }

            }

            measures_df <- rbind(measures_df, measures_vars)

         }
         measures_df <- as.data.frame(measures_df)
         colnames(measures_df) <- measures_var

         temp_df <- cbind(vars$CatheterLocation, vars$Start, vars$AnalyteCode, vars$Unit, measures_df)
         colnames(temp_df)[1:4] <- c("CatheterLocation","Start","AnalyteCode","Unit")

         df_results <- rbind(df_results,temp_df)
      }

      df_results <- cbind(dir,file,vars$id,vars$firstname,vars$lastname,vars$AdmissionDate,df_results)

      if(is.null(df_results$Status)){ df_results$Status <- NA }

      results <- rbind(results,df_results)
   }

   print(dir)

}

colnames(results)[1:6] <- c("dir","file","id","firstname","lastname","admissiondate")

write.csv2(results, "MD.csv", row.names = F)
