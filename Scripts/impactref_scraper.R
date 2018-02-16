## ---- webscraper to trawl first 10 items of each institution's case studies on Impact Ref: impact.ref.ac.uk/


# function to do the harvest. the function returns Titles, abstracts, subject areas, unit of assessment and the full case study text for each CS. The first two are taken from the institution page, while the rest are taken from the specific case study page - the latter is what the second for loop does.
# the funtion can be started and finished at any specific institutional number (as recognised in the Impact Ref database)
# it is also possible in the function to specify the particular impact type that you want the output to be based on.
# the output will be stored as a .csv in a folder called "impactRef_data"

# dependencies
library(rvest)
library(fuzzyjoin)
library(zoo)

impactRef_harvest = function(from, to) {

  


  dir.create("../impactRef_data")

  url_base = "http://impact.ref.ac.uk/CaseStudies/"
  url_uni_base = "Results.aspx?HEI="
  
  # total_unis = to-from

  urls = data.frame(paste0(url_base, url_uni_base, seq(from = from, to = to, by = 1)), stringsAsFactors = FALSE)
  
  impact.ref100 = data.frame(as.character(), as.character(), stringsAsFactors = FALSE)
  uni_casestudy = data.frame(as.character(), as.character(), as.character(), as.character(), stringsAsFactors = FALSE)
  
  page = 1
  
  for (page in 1:nrow(urls)){

    webpage = read_html(urls[page, 1])
    html_check = html_nodes(webpage, 'h1')
    check = html_text(html_check)
    
    if (length(check) == 0){ ## length 0 suggests that there is a record for this page number. if there is not, then it will be length(1)

      html_Titles=html_nodes(webpage,'#MainContent_divResults .loading-dial')
      Titles = data.frame(html_text(html_Titles), stringsAsFactors = FALSE)
      html_Abstracts_para = html_nodes(webpage, "#MainContent_divResults p")
      Abstracts_para = data.frame(html_text(html_Abstracts_para), stringsAsFactors = FALSE)
      htmlTitleAbstract = html_nodes(webpage, '#MainContent_divResults p , #MainContent_divResults .loading-dial, #MainContent_divResults li')
      TitleAbstract = html_text(htmlTitleAbstract) 
      TitleAbstract = data.frame(TitleAbstract, stringsAsFactors = FALSE)
      table = data.frame(as.character(), stringsAsFactors = FALSE)
      names(Titles) = "Titles"
      names(TitleAbstract) = "TitleAbstract"
      join = TitleAbstract %>%
        stringdist_left_join(Titles, by = c(TitleAbstract = "Titles"))
      join = data.frame(join$TitleAbstract, na.locf(join$Titles), stringsAsFactors = F)
 
 
      # For loop to sort out Titles and Abstracts. Sometimes an abstract takes up multiple rows  
      for (i in as.numeric(rownames(join[!duplicated(join$na.locf.join.Titles.),]))){
        x = grep(join[i,2], join$na.locf.join.Titles.)
        if(length(x)>2 & i < (nrow(join)-2)){
          sub_join = join[join$na.locf.join.Titles.==join[i,2],1]
          record_abstract = paste(sub_join[2:length(sub_join)], collapse = " ")
          join[i,1] = record_abstract
          #print(paste0(i, " joined"))
        } else{
            join[i,1] = join[i+1, 1]
          }
      }
      
  
      
      TitleAbstracts = join[!duplicated(join$na.locf.join.Titles.),]
      TitleAbstracts = TitleAbstracts[,c(2,1)]
      
      Titles = Titles[(Titles$Titles %in% TitleAbstracts$na.locf.join.Titles.),]
      
      duplicatedTitles <- anyDuplicated(Titles)
      
      TitleAbstracts = TitleAbstracts[!duplicated(TitleAbstracts$na.locf.join.Titles.),]

    
      extensions_casestudy = html_attr(html_Titles, "href")
      
      if(duplicatedTitles > 0){
        extensions_casestudy = extensions_casestudy[-duplicatedTitles]
      }
    
      extension = 1
      for (extension in 1:length(extensions_casestudy)){
        url_casestudy = paste0(url_base, extensions_casestudy[extension])
        webpage_casestudy = read_html(url_casestudy)
        html_Subject = html_nodes(webpage_casestudy, '.col-sm-8 span')
        Subject = data.frame(html_text(html_Subject), stringsAsFactors = FALSE)
        Subject = paste(Subject[,1], collapse = " ")
        
        
        html_webpage_casestudy = html_nodes(webpage_casestudy, '#MainContent_divCaseStudy > p')
        webpage_casestudy_text = data.frame(html_text(html_webpage_casestudy), stringsAsFactors = FALSE)
        webpage_casestudy_text = paste(webpage_casestudy_text[,1], collapse = " ")
        
        html_uoa = html_nodes(webpage_casestudy, '.pill-uoa')
        uoa = data.frame(html_text(html_uoa), stringsAsFactors = FALSE)
        
        html_impactType = html_nodes(webpage_casestudy, '.pill-pathway')
        impactType = data.frame(html_text(html_impactType), stringsAsFactors = FALSE)
        
        subject_casestudy = cbind(uoa, Subject, impactType, webpage_casestudy_text, urls[page, 1], extensions_casestudy[extension])

        uni_casestudy = rbind(uni_casestudy, subject_casestudy)
      }
      print(paste0("Completed record ", urls[page,]))

      impact.ref100 = rbind(impact.ref100, TitleAbstracts)
      print(paste0("Web record ", page, " has ", nrow(impact.ref100), " entries"))
      print(paste0("... and ", nrow(uni_casestudy), " institutional case studies")) 

      } else {
      print(paste0("Don't know how to handle web record ", urls[page,]))
    }




  }
  

  impact.ref100 = cbind(impact.ref100, uni_casestudy)
  
  impact.ref100 = impact.ref100[,c(7,8,1:6)]
  
  names(impact.ref100) = c("Institution_URL", "CaseStudy_URLExt", "Title", "Abstract", "UnitOfAssessment", "Subject", "ImpactType", "Casestudy")
  
  
  for(i in 1:ncol(impact.ref100)){
    impact.ref100[,i] = gsub("\n","", impact.ref100[,i])
    impact.ref100[,i] = gsub("\r     ","", impact.ref100[,i])
  }
  

    write.csv(impact.ref100, paste0("../impactRef_data/impactData_", from, "-", to, ".csv"), row.names = FALSE)
   
  
}


args = commandArgs(trailingOnly = TRUE)

from = args[1]
to = args[2]
#impact_area = args[3] #may be unused


#from = 1
#to = 2
#impact_area = NULL
impactRef_harvest(from, to)



