rm(list = ls())
library(DT)
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinydashboard)
library("shinythemes")
library("highcharter")
library(zipfR)
library(adehabitatLT)
library("magrittr")
library("dplyr")
library(datasets)
library(scales)

# This function will create the buttons for the datatable, they will be unique
shinyInput <- function(FUN, len, id, ...) {inputs <- character(len)
for (i in seq_len(len)) {
  inputs[i] <- as.character(FUN(paste0(id, i), ...))}
inputs
}

mainbody <- div(tabItems(
  tabItem(tabName = "About",
          h4(tags$b('2x2: A Comprehensive Calculator for Two-Way Contingency Tables')),
          HTML('<p align="justify"> <b> Definition: </b> 
Most of the researches in the health field require 2x2 cross tables to perform categorical data analysis. 
               With these tables for solving a large number of problems such as investigating the performance of diagnostic tests 
               to determine the presence or absence of a disease, making risk estimates in terms of variables thought to be of 
               interest to diseases, investigating the cause-and-effect relationship between two clinical variables in two categories,
               investigating the compatibility between two evaluators, evaluating the performance of classification methods, 
               calculating distance-similarity measures  can be done statistical calculations. Although there are a lot of software 
               to do calculations with 2x2 tables in literature, these software have been developed for the solution of certain problems 
               and make a limited number of calculations. In this program, it is aimed to develop a comprehensive web software which 
               is able to calculate the necessary statistics for solving a large number of problems and has the ability to interpret 
               the calculated measures which are not found in any software related to the subject, and which is easily accessible and 
               usable.This tool includes analysis procedures for 2x2 contingency table. Each procedure includes following features: </p>'),
          
  HTML('<p align="justify"> <b> Diagnostic Measures:</b>Sensitivity, Sensitivity of a random test, Quality index of sensitivity...</p>'),
  HTML('<p align="justify"> <b> Association Measures:</b>Dice\'s Index (Czekanowski-Sorenson), Yules Q (Gamma)...</p>'),
  HTML('<p align="justify"> <b> Agreement Measures:</b> Cohen\'s Kappa, Observed Agreement, Chance Agreement...</p>'),
  HTML('<p align="justify"> <b> Statistics and Hypothesis Tests:</b>Pearson Chi-Square Analysis, Pearson Chi-Square Analysis with Yate\'s correction (Continuity Correction)...</p>'),
  HTML('<p align="justify"> <b> Smilarity and Distance Measures:</b>Jaccard (Similarity), Dice (Similarity), Czekanowski (Similarity)...</p>'),
  HTML('<p align="justify"> <b> Risk Measures:</b>Prevalence, Detection Rate, Detection Prevalence...</p>'),
  
  HTML('<p><div align = "center"><table cellpadding="0" cellspacing="0"><tr><td><img src="images/measure1.jpg" width="300" height="200" ></td><td><img src="images/cont_table.png" width="300" height="200"></td><td><img src="images/measure2.gif" width="300" height="200"></td></tr></table></div></p>'),
  
  HTML('<p align="justify"> All source codes are in <a href="https://github.com/gokmenzararsiz/2x2 " target="_blank"><b>GitHub</b></a>. 
       Please see the <a href="help/help.html" target="_blank"> <b>help page</b></a> for more detailed information.</p>')
  ),

tabItem(tabName = "Analysis", fluidRow( column(width=3,
                                               tabBox(width = 12,
                                                  tabPanel(title = 'Table Input:', '\n', '2x2 Table Cells',solidHeader = T,status = 'primary',
                                                   label = 'View Data',
                                                   fluidRow (column(4, h5("True Positive:")),
                                                             column(4, textInput("text1", label = "", value = "44"))),
                                                   fluidRow (column(4, h5("False Negative:")),
                                                             column(4, textInput("text2", label = "", value = "22"))),
                                                   fluidRow (column(4, h5("False Positive:")),
                                                             column(4, textInput("text3", label = "", value = "81"))),
                                                   fluidRow (column(4, h5("True Negative:")),
                                                             column(4, textInput("text4", label = "", value = "473"))),
                                                   h5("Confidence Interval (%):"),
                                                   textInput("text5", label = "", value = "95")
                                                   
                                                   
                                               ),
                                               tabPanel(title = 'File Input:', '\n', 'Data Upload',solidHeader = T,status = 'primary',
                                                        label = 'View Data2',
                                               checkboxInput("verigir_kontrol","Please check if you want to use the sample dataset or enter your own data",
                                                             FALSE),
                                               radioButtons("dataInput", "", list("Upload a file" = 2, "Load example data" = 1), selected=NULL),
                                               ## Lachman data https://www.researchgate.net/profile/William_Robertson/publication/51618260_Reliability_and_Diagnostic_Accuracy_of_the_Lachman_Test_Performed_in_a_Prone_Position/links/569987c308ae748dfaff825b.pdf
                                               ## Lachman data https://joannabriggs.org/assets/docs/sumari/Reviewers-Manual_The-systematic-review-of-studies-of-diagnostic-test-accuracy.pdf
                                               ##   https://mysite.du.edu/~jcalvert/econ/twobytwo.htm
                                               ##   https://books.google.com.tr/books?id=RiDCAgAAQBAJ&pg=PA270&dq=Langley+cholera+inoculation+study+818+people&hl=tr&sa=X&ved=0ahUKEwi-4YXL9ObWAhWqDcAKHSbnB4AQ6AEIJjAA#v=onepage&q=Langley%20cholera%20inoculation%20study%20818%20people&f=false
                                               conditionalPanel(condition="input.dataInput=='1'",
                                                                h5("Load example data:"),
                                                                radioButtons("sampleData", "", list("Lachman data"=1, "Cholera data"=2), selected=1)
                                               ),
                                               conditionalPanel(condition="input.dataInput=='2'",
                                                                h5("Upload a delimited text file: "),
                                                                
                                                                fileInput("upload", "", multiple = FALSE),
                                                                
                                                                radioButtons("fileSepDF", "Delimiter:", list("Comma"=1,"Tab"=2,"Semicolon"=3,"Space"=4),selected=2),
                                                                conditionalPanel(condition="input.fileSepDF!='1'",
                                                                                 checkboxInput(inputId = "decimal", label = "Use comma as decimal", value = FALSE)),
                                                                HTML('<p>You can upload your data as separated by comma, tab, semicolon or space.</p>'),
                                                                HTML('<p>Note: First row must be header.</p>')
                                               ),
                                               h5("Confidence Interval (%):"),
                                               textInput("text5_2", label = "", value = "95")))),
  tabBox(width = 9,
         tabPanel(title = 'Data',#icon = icon('sort-amount-asc'),
                  textOutput(outputId = "desc"),DT::dataTableOutput('firstdata')
         ),
         tabPanel(title = 'Diagnostic Measures',#icon = icon('sort-amount-asc'),
                  DT::dataTableOutput('dataUpload0'),uiOutput("popup0"),
                  highcharter::highchartOutput('plot0'),highcharter::highchartOutput('plot01')
         ),
         tabPanel(title = 'Association Measures',
                  dataTableOutput('dataUpload1'),uiOutput("popup1"),
                  highcharter::highchartOutput('plot1'),highcharter::highchartOutput('plot11')
         ),
         tabPanel(title = 'Agreement Measures',
                  dataTableOutput('dataUpload2'),uiOutput("popup2"),
                  highcharter::highchartOutput('plot2'),highcharter::highchartOutput('plot21')
         ),
         tabPanel(title = 'Statistics and Hypothesis Tests',
                  dataTableOutput('dataUpload3'), uiOutput("popup3")
         ),
         tabPanel(title = 'Similarity and Distance Measures',
                  dataTableOutput('dataUpload4'),uiOutput("popup4"),
                  highcharter::highchartOutput('plot4'),highcharter::highchartOutput('plot41')
         ),
         tabPanel(title = 'Risk Measures',
                  dataTableOutput('dataUpload5'), uiOutput("popup5")
                  ,highcharter::highchartOutput('plot5'),highcharter::highchartOutput('plot51')
         )
        
  )
  )),
  tabItem(tabName = "Authors",
          HTML('<p><b>Ahu Durmuscelebi</b></a><p>'),
          HTML('<p>Erciyes University Faculty of Medicine <a href="http://www.biostatistics.hacettepe.edu.tr" target="_blank"> Department of Biostatistics</a><p>'),
          HTML('<p><a href="mailto:ahudurmuscelebi.87@gmail.com" target="_blank">ahudurmuscelebi.87@gmail.com</a><p>'),
          HTML('<p><a href="http://gokmenzararsiz.simplesite.com" target="_blank"> <b>Gokmen Zararsiz</b></a><p>'),
          HTML('<p>Erciyes University Faculty of Medicine <a href="http://www.biostatistics.hacettepe.edu.tr" target="_blank"> Department of Biostatistics</a><p>'),
          HTML('<p><a href="mailto:gokmenzararsiz@hotmail.com" target="_blank">gokmenzararsiz@hotmail.com</a><p>') ,
          HTML('<p><a href="http://yunus.hacettepe.edu.tr/~selcuk.korkmaz/" target="_blank"> <b>Selcuk Korkmaz</b></a><p>'),
          HTML('<p>Hacettepe University Faculty of Medicine <a href="http://www.biostatistics.hacettepe.edu.tr" target="_blank"> Department of Biostatistics</a><p>'),
          HTML('<p><a href="mailto:selcukorkmaz@gmail.com" target="_blank">selcukorkmaz@gmail.com</a><p>'),
          HTML('<p><a href="http://yunus.hacettepe.edu.tr/~dincer.goksuluk" target="_blank"> <b>Dincer Goksuluk</b></a><p>'),
          HTML('<p>Hacettepe University Faculty of Medicine <a href="http://www.biostatistics.hacettepe.edu.tr" target="_blank"> Department of Biostatistics</a><p>'),
          HTML('<p><a href="mailto:dincer.goksuluk@gmail.com" target="_blank">dincer.goksuluk@gmail.com</a><p>')
           )
))


ui <- bootstrapPage(useShinyjs(),
                    # Add custom CSS & Javascript;
                    tagList(tags$head(
                      tags$link(rel="stylesheet", type="text/css",href="style.css"),
                      tags$script(type="text/javascript", src = "busy.js")
                      #lapply(1:length(mdls),function(i) modelCSS(mdls[i],pal[i]))
                      
                    )),
  dashboardPage(
  
  dashboardHeader(title='2x2: A Comprehensive Calculator for Two-Way Contingency Tables',titleWidth = 800),
  
  dashboardSidebar(sidebarMenuOutput("menu")),
  
  dashboardBody(uiOutput("body"))
  
))

## iconlar icin adres http://fontawesome.io/icons/  ve  https://getbootstrap.com/docs/3.3/components/
server <- function(input, output, session) {
  output$menu <- renderUI({
    sidebarMenu(
      menuItem("About", tabName="About", icon = icon("info-circle")),
      menuItem("Analysis", tabName="Analysis", icon = icon("th-large"), selected=TRUE),
      menuItem("Authors & News", tabName="Authors", icon = icon("group"))
    )
  })
  
  output$body <- renderUI({
     mainbody
  })
#### Data Upload ##
  tabloveri <- reactive({  ## Data input.
    if(input$dataInput==1){  ## Load example data.
      
      if(input$sampleData == 1){
        
        data <- read.table("Lachman_data.txt", header=TRUE, sep = "\t")
        
      }
      
      else if(input$sampleData == 2){
        
        data <- read.table("Cholera_data.txt", header=TRUE, sep = "\t")
        
      }
    }
    
    else if(input$dataInput==2){  ## Upload data.
      
      inFile <- input$upload
      
      mySep <- switch(input$fileSepDF, '1'=",",'2'="\t",'3'=";", '4'="")
      
      if (is.null(input$upload))  {return(NULL)}
      
      if (file.info(inFile$datapath)$size <= 10485800){
        data <- read.table(inFile$datapath, sep=mySep, header=TRUE, fill=TRUE, dec = ifelse(input$decimal, ",", "."))
      } else print("File is bigger than 10MB and will not be uploaded.") 
      
    }
    
    #ind = complete.cases(data)
    #return(data[ind,])
    return(data) 
    #else {
    #    data[,dim(data)[2]] = as.factor(data[,dim(data)[2]])
    #    ind = complete.cases(data)
    #    return(data[ind,])
    #}
  })
 
  output$desc <- renderText({
    if (input$verigir_kontrol == TRUE & NROW(tabloveri())!=0) 
    { TP = 0
    FN = 0
    FP = 0
    TN = 0
    i = 1
    for (i in 1:NROW(tabloveri()))
    {
      if (tabloveri()[i,1]==1 & tabloveri()[i,2]==1)
      {TP = TP+1}
      else if (tabloveri()[i,1]==1 & tabloveri()[i,2]==2)
      {FN = FN+1}
      else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==1)
      {FP = FP+1}
      else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==2)
      {TN = TN+1}
      next
    }
    
    paste('TP = ',TP, '\n', 'FN = ',FN, '\n', 'FP = ',FP, '\n', 'TN = ', TN)}
    else
      paste("No data entry.")
  })
  
  output$firstdata <- DT::renderDataTable({
    
    datatable(tabloveri(), extensions = 'ColReorder', options = list(colReorder = TRUE))
    
  })

## Diagnostic Measures ##
  dataM0 <- reactive({  

    if (is.null(trimws(input$text3)) && is.null(trimws(input$text4)))  {return(NULL)}
    else {
      if (input$verigir_kontrol == TRUE){ 
      TP = 0
      FN = 0
      FP = 0
      TN = 0
      for (i in 1:nrow(tabloveri()))
      {
        if (tabloveri()[i,1]==1 & tabloveri()[i,2]==1)
        {TP = TP+1}
        else if (tabloveri()[i,1]==1 & tabloveri()[i,2]==2)
        {FN = FN+1}
        else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==1)
        {FP = FP+1}
        else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==2)
        {TN = TN+1}
        next
      }
      CI = as.numeric(trimws(input$text5_2))
      }
      else{
      TP = as.numeric(trimws(input$text1)) ### x
      FN = as.numeric(trimws(input$text2)) ### y
      FP = as.numeric(trimws(input$text3)) ### z
      TN = as.numeric(trimws(input$text4)) ### t
      CI = as.numeric(trimws(input$text5))
      }
      
      satirtop1 = TP + FN
      satirtop2 = FP + TN
      sutuntop1 = TP + FP
      sutuntop2 = FN + TN
      toplam = satirtop1 + satirtop2
      #table1 <- matrix(c(TP,FN,satirtop1,FP,TN,satirtop2,sutuntop1,sutuntop2,toplam),nrow=3,ncol=3,byrow=TRUE,dimnames=list(c("1.satir","2.satir","Sutun toplamlari"),c("1.kolon","2.kolon","Satir toplamlari")))
      #table1
      ## Duyarlilik 
      sensitivity_estimate = TP / satirtop1
      sensitivity_se = round((sqrt(sensitivity_estimate * (1 - sensitivity_estimate)/satirtop1)), 4)
      sensitivity_low = 1 - Rbeta.inv(((1 + CI/100)/2), (satirtop1+1-TP), TP)
      sensitivity_upp = Rbeta.inv(((1 + CI/100)/2), (TP+1), (satirtop1-TP))
      sensitivity_randomtest = sutuntop1 / toplam
      qualityindex_sensitivity = (sensitivity_estimate - sensitivity_randomtest) / (1 - sensitivity_randomtest)
      ## Secicilik
      specificity = TN / satirtop2
      specificity_se = sqrt(specificity * (1 - specificity) / satirtop2)
      specificity_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop2+1-TN),TN)
      specificity_upp = Rbeta.inv((1 + CI/100)/2, (TN+1), (satirtop2-TN))
      specificity_randomtest = (1 - sensitivity_randomtest)
      qualityindex_specificity = (specificity - 1 + sensitivity_randomtest) / sensitivity_randomtest
      ## Gain in Certainty
      gain_in_certainty = sensitivity_estimate + specificity
      ## Etkililik, yeterlilik ayni zamanda overall accuracy (toplam-genel dogruluk orani)
      efficiency = (TP + TN) / toplam
      efficiency_se = sqrt(efficiency * (1 - efficiency) / toplam)
      efficiency_low = 1-Rbeta.inv((1 + CI/100)/2, (toplam+1-TP-TN), (TP+TN))
      efficiency_upp = Rbeta.inv((1 + CI/100)/2, (TP+TN+1), (toplam-TP-TN))
      efficiency_randomtest = (satirtop1 / toplam) * sensitivity_randomtest + (1 - (satirtop1 / toplam)) * (1 - sensitivity_randomtest)
      mis_efficiency = 1 - efficiency
      ## Quality indeks
      quality_index = (efficiency - efficiency_randomtest) / (1 - efficiency_randomtest)
      quality_index_se = sqrt((efficiency*(1-efficiency)/(1-efficiency_randomtest)^2+2*(1-efficiency)*(2*efficiency*efficiency_randomtest-((TP*((satirtop1 / toplam)+sensitivity_randomtest)+TN*(2-(satirtop1 / toplam)-sensitivity_randomtest))/toplam))/(1-efficiency_randomtest)^3+((1-efficiency)^2)*((TP*((satirtop1 / toplam)+sensitivity_randomtest)^2+FN*(-(satirtop1 / toplam)+1+sensitivity_randomtest)^2+FP*(1+(satirtop1 / toplam)-sensitivity_randomtest)^2+TN*(2-(satirtop1 / toplam)-sensitivity_randomtest)^2)/toplam-4*efficiency_randomtest^2)/(1-efficiency_randomtest)^4)/toplam)
      quality_index_low = quality_index + qnorm((1 - CI/100)/2)*quality_index_se
      quality_index_upp = quality_index - qnorm((1 - CI/100)/2)*quality_index_se
      ## Youden indeksi
      youdens_index = sensitivity_estimate + specificity - 1
      youdens_index_se = sqrt(sensitivity_se^2+specificity_se^2)
      youdens_index_low = youdens_index + qnorm((1 - CI/100)/2)*youdens_index_se
      youdens_index_upp = youdens_index - qnorm((1 - CI/100)/2)*youdens_index_se
      
      number_needed_to_diagnose = 1 / youdens_index
      number_needed_to_misdiagnose = 1 / ( 1 - efficiency ) 
      ## Pozitif kestirim degeri
      predictivevalue_positivetest = TP / sutuntop1
      predictivevalue_positivetest_se = sqrt(predictivevalue_positivetest*(1-predictivevalue_positivetest)/sutuntop1)
      predictivevalue_positivetest_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop1+1-TP),TP)
      predictivevalue_positivetest_upp = Rbeta.inv((1 + CI/100)/2, (TP+1),(sutuntop1-TP))
      predvalue_positiverandomtest = (satirtop1 / toplam)
      predvalue_positiverandomtest_mar_ol = predvalue_positiverandomtest*sensitivity_estimate+(1-predvalue_positiverandomtest)*(1-specificity)
      ## Negatif kestirim degeri
      predictivevalue_negativetest = TN / sutuntop2
      predictivevalue_negativetest_se = sqrt(predictivevalue_negativetest*(1-predictivevalue_negativetest)/sutuntop2)
      predictivevalue_negativetest_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop2+1-TN),TN)
      predictivevalue_negativetest_upp = Rbeta.inv((1 + CI/100)/2, (TN+1), (sutuntop2-TN))
      predvalue_negativerandomtest = 1 - (satirtop1 / toplam)
      predvalue_negativerandomtest_mar_ol = 1 - predvalue_positiverandomtest_mar_ol
      ## Predictive Summary Index (PSI)
      predictive_summary_index = predictivevalue_positivetest + predictivevalue_negativetest - 1
      ## Yanlis pozitif orani
      false_positiverate = FP / satirtop2
      false_positiverate_se = sqrt(false_positiverate*(1-false_positiverate)/satirtop2)
      false_positiverate_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop2+1-FP),FP)
      false_positiverate_upp = Rbeta.inv((1 + CI/100)/2, (FP+1), (satirtop2-FP))
      ## Yansis negatif orani
      false_negativerate = FN / satirtop1
      false_negativerate_se = sqrt(false_negativerate*(1-false_negativerate)/satirtop1)
      false_negativerate_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop1+1-FN),FN)
      false_negativerate_upp = Rbeta.inv((1 + CI/100)/2, (FN+1), (satirtop1-FN))
      ## False Omission Rate (FOR) https://www.ncss.com/software/ncss/diagnostic-tests-in-ncss/
      false_omission_rate = FN/(sutuntop2)
      false_omission_rate_se = sqrt(false_omission_rate*(1-false_omission_rate)/sutuntop2)
      false_omission_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop2+1-FN),FN)
      false_omission_rate_upp = Rbeta.inv((1 + CI/100)/2, (FN+1), (sutuntop2-FN))
      ## False Discovery Rate (FDR) https://www.ncss.com/software/ncss/diagnostic-tests-in-ncss/
      false_discovery_rate = FP/(sutuntop1)
      false_discovery_rate_se = sqrt(false_discovery_rate*(1-false_discovery_rate)/sutuntop1)
      false_discovery_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop1+1-FP),FP)
      false_discovery_rate_upp = Rbeta.inv((1 + CI/100)/2, (FP+1), (sutuntop1-FP))
      ## Dogru pozitif orani
      true_positiverate = TP / (satirtop1)
      ## Yanlis siniflandirma orani
      misclassification_rate = (FN + FP) / toplam
      misclassification_rate_se = sqrt(misclassification_rate*(1-misclassification_rate)/toplam)
      misclassification_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-FP-FN), (FP+FN))
      misclassification_rate_upp = Rbeta.inv((1 + CI/100)/2, (FP+FN+1), (toplam-FP-FN))
      ## Prevelans
      prevalence = (satirtop1 / toplam)
      prevalence_se = sqrt((satirtop1 / toplam)*(1-(satirtop1 / toplam))/toplam)
      prevalence_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-satirtop1), satirtop1)
      prevalence_upp = Rbeta.inv((1 + CI/100)/2, (satirtop1+1), (toplam-satirtop1))
      
      ##caret paketinden olculer
      detection_rate = TP/toplam
      detection_prevalence = (TP+FP)/toplam
      balanced_accuracy = (sensitivity_estimate + specificity)/2
      lift = (TP/(TP+FN))/((TP+FP)/toplam)  ## https://gerardnico.com/wiki/data_mining/error_rate
      precision = TP/(TP+FP)
      recall = TP/(TP+FN)
      ## http://michael.hahsler.net/research/recomm_lnai2002/lnai2002.pdf
      conviction = (1-((TP+FP)/toplam))/(1-(TP/(TP+FN)))
      matthews_correlation_coefficient = (TP * TN + FN * FP) / ((TP+FP)*(FN+TN)*(FP+TN)*(TP+FN))^(1/2)
      ## Goreli Risk Orani (Relative risk)
      ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
      # https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20ratio.htm
      relative_risk = (TP/sutuntop1)/(FN/sutuntop2)
      relative_risk_se = sqrt((1/TP)+(1/FN)-(1/(TP+FP))-(1/(FN+TN)))
      relative_risk_low = exp(log(relative_risk)+qnorm((1-CI/100)/2)*relative_risk_se)
      relative_risk_upp = exp(log(relative_risk)-qnorm((1-CI/100)/2)*relative_risk_se)
      difference_in_proportion = (TP/sutuntop1) - (FN/sutuntop2)
      ## Leverage
      ## https://www3.nd.edu/~tweninge/pubs/AHBCW_ANNIE08.pdf
      leverage = (TP/sutuntop1)-((TP+FP)/toplam) 
      ## http://www.ebm.med.ualberta.ca/TherapyCalc.html
      control_event_rate = FN/sutuntop2
      experimental_event_rate = TP/sutuntop1
      ## Absolute risk reduction
      absolute_risk_reduction = -difference_in_proportion
      absolute_risk_reduction_se = sqrt((TP/sutuntop1)*(1-(TP/sutuntop1))/sutuntop1+(FN/sutuntop2)*(1-(FN/sutuntop2))/sutuntop2)
      absolute_risk_reduction_low = absolute_risk_reduction - qnorm((1-CI/100)/2)*absolute_risk_reduction_se
      absolute_risk_reduction_upp = absolute_risk_reduction + qnorm((1-CI/100)/2)*absolute_risk_reduction_se
      ## Relative risk reduction
      relative_risk_reduction = absolute_risk_reduction / (FN/sutuntop2)
      relative_risk_reduction_low = 1 - exp(log(relative_risk)-qnorm((1-CI/100)/2)*relative_risk_se)
      relative_risk_reduction_upp = 1 - exp(log(relative_risk)+qnorm((1-CI/100)/2)*relative_risk_se)
      ##Number needed to treat
      number_needed_to_treat = 1 / abs(difference_in_proportion)
      number_needed_to_treat_low = 1 /absolute_risk_reduction_upp
      number_needed_to_treat_upp = 1 /absolute_risk_reduction_low
      ## Test duzeyi
      test_level = sensitivity_randomtest
      test_level_se = sqrt(test_level*(1-test_level)/toplam)
      test_level_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-sutuntop1),sutuntop1)
      test_level_upp = Rbeta.inv((1 + CI/100)/2, (sutuntop1+1), (toplam-sutuntop1))
      ## Pre-test odds
      pretest_odds = prevalence / (1 - prevalence)
      ## Pozitif Olabilirlik Orani
      likelihoodratio_positivetest = sensitivity_estimate/(1-specificity)
      likelihoodratio_positivetest_se = exp(sqrt((1-sensitivity_estimate)/(sensitivity_estimate*satirtop1) +specificity/((1-specificity)*satirtop2)))
      likelihoodratio_positivetest_low = exp(log(likelihoodratio_positivetest) + qnorm((1 - CI/100)/2)*log(likelihoodratio_positivetest_se))
      likelihoodratio_positivetest_upp = exp(log(likelihoodratio_positivetest) - qnorm((1 - CI/100)/2)*log(likelihoodratio_positivetest_se))
      ## post-test odds
      posttest_odds = pretest_odds * likelihoodratio_positivetest
      bayes_ppv = (sensitivity_estimate * prevalence)/((sensitivity_estimate * prevalence) + (1 - specificity) * (1 - prevalence))
      ## Post-test probability (positive test result)
      posttest_probability = (pretest_odds * likelihoodratio_positivetest) / (1 + (pretest_odds * likelihoodratio_positivetest))
      ## Negatif Olabilirlik Orani
      likelihoodratio_negativetest = (1-sensitivity_estimate)/specificity
      likelihoodratio_negativetest_se = exp(sqrt(sensitivity_estimate/((1-sensitivity_estimate)*satirtop1) +(1-specificity)/(specificity*satirtop2)))
      likelihoodratio_negativetest_low = exp(log(likelihoodratio_negativetest) + qnorm((1-CI/100)/2)*log(likelihoodratio_negativetest_se))
      likelihoodratio_negativetest_upp = exp(log(likelihoodratio_negativetest) - qnorm((1-CI/100)/2)*log(likelihoodratio_negativetest_se))
      bayes_npv = (sensitivity_estimate * (1 - prevalence))/((sensitivity_estimate * (1 - prevalence)) + (1 - specificity) * prevalence)
      ## Post-test probability (negative test result)
      posttest_probability_neg = (pretest_odds * likelihoodratio_negativetest) / (1 + (pretest_odds * likelihoodratio_negativetest))
      ## Ters negatif olabilirlik orani
      inverse_likelihoodratio_negativetest = specificity/(1-sensitivity_estimate)
      inverse_likelihoodratio_negativetest_se = exp(sqrt(sensitivity_estimate/((1-sensitivity_estimate)*satirtop1) +(1-specificity)/(specificity*satirtop2)))
      inverse_likelihoodratio_negativetest_low = exp(log(inverse_likelihoodratio_negativetest) + qnorm((1-CI/100)/2)*log(inverse_likelihoodratio_negativetest_se))
      inverse_likelihoodratio_negativetest_upp = exp(log(inverse_likelihoodratio_negativetest) - qnorm((1-CI/100)/2)*log(inverse_likelihoodratio_negativetest_se))
      ## Diagnostic Odds Ratio (Tani Odds Orani)
      odds_ratio = (TP/FN)/(FP/TN)
      odds_ratio_se = exp(sqrt(1/TP+1/FP+1/TN+1/FN))
      odds_ratio_low = exp(log(odds_ratio) + qnorm((1-CI/100)/2)*log(odds_ratio_se))
      odds_ratio_upp = exp(log(odds_ratio) - qnorm((1-CI/100)/2)*log(odds_ratio_se))
      odds_ratio_Haldane = ((TP+0.5)/(FN+0.5))/((FP+0.5)/(TN+0.5))
      odds_ratio_Haldane_se = exp(sqrt(1/(TP+0.5)+1/(FP+0.5)+1/(TN+0.5)+1/(FN+0.5)))
      odds_ratio_Haldane_low = exp(log(odds_ratio_Haldane) + qnorm((1-CI/100)/2)*log(odds_ratio_Haldane_se))
      odds_ratio_Haldane_upp = exp(log(odds_ratio_Haldane) - qnorm((1-CI/100)/2)*log(odds_ratio_Haldane_se))
      error_odds_ratio = (sensitivity_estimate/(1-sensitivity_estimate))/(specificity/(1-specificity))
      ## Rate ratio
      ## http://omarkasule-05.tripod.com/id52.html
      ## http://sphweb.bumc.bu.edu/otlt/mph-modules/ep/ep713_randomerror/ep713_randomerror4.html
      rate_ratio = (TP/sutuntop1)/(TP/sutuntop2)
      rate_ratio_low = exp(log(relative_risk)+qnorm((1-CI/100)/2)*sqrt(1/TP+1/FP))
      rate_ratio_upp = exp(log(relative_risk)-qnorm((1-CI/100)/2)*sqrt(1/TP+1/FP))
      ## Risk Difference
      ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
      risk_difference = (TP/sutuntop1) - (FN/sutuntop2)
      risk_difference_se = (((TP/sutuntop1)*(1-(TP/sutuntop1)))/sutuntop1)+(((FN/sutuntop2)*(1-(FN/sutuntop2)))/sutuntop2)
      risk_difference_low = risk_difference + qnorm((1-CI/100)/2)*sqrt(risk_difference_se)
      risk_difference_upp = risk_difference - qnorm((1-CI/100)/2)*sqrt(risk_difference_se)
      ## Nitelenebilen risk
      ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
      attributable_risk = (TP/sutuntop1)-(FN/sutuntop2)
      attributable_risk_se = sqrt((satirtop1/toplam)*(1-(satirtop1/toplam))*(1/sutuntop1+1/sutuntop2))
      attributable_risk_low = attributable_risk + qnorm((1-CI/100)/2)*attributable_risk_se
      attributable_risk_upp = attributable_risk - qnorm((1-CI/100)/2)*attributable_risk_se
      ## Attributable risk percent
      ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
      attributable_risk_percent = ((relative_risk-1)/relative_risk)*100
      attributable_risk_percent_low = attributable_risk_percent - (qnorm((1-CI/100)/2)*attributable_risk_se)/attributable_risk
      attributable_risk_percent_upp = attributable_risk_percent + (qnorm((1-CI/100)/2)*attributable_risk_se)/attributable_risk
      ## Population Attributable Risk
      ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
      population_attributable_risk = (satirtop1/toplam)-(FN/sutuntop2)
      ## Population Attributable Risk Percent
      population_attributable_risk_percent = (population_attributable_risk / (satirtop1/toplam))*100
      ## Kohen's Kappa
      cohens_kappa = (efficiency-efficiency_randomtest)/(1-efficiency_randomtest)
      cohens_kappa_se = quality_index_se
      cohens_kappa_low = cohens_kappa + qnorm((1-CI/100)/2)*cohens_kappa_se
      cohens_kappa_upp = cohens_kappa - qnorm((1-CI/100)/2)*cohens_kappa_se
      ## Gozlenen Karar
      observed_agreement = efficiency
      observed_agreement_se = efficiency_se
      observed_agreement_low = efficiency_low
      observed_agreement_upp = efficiency_upp
      ## Risk karari,expected agreement
      chance_agreement = efficiency_randomtest
      ## Pozitif karar
      positive_agreement = 2*TP /(satirtop1+sutuntop1)
      positive_agreement_se = sqrt(4*TP*(FP+FN)*(TP+FP+FN))/(sutuntop1+satirtop1)^2
      positive_agreement_low = positive_agreement + qnorm((1-CI/100)/2)*positive_agreement_se
      positive_agreement_upp = positive_agreement - qnorm((1-CI/100)/2)*positive_agreement_se
      ## Negatif karar
      negative_agreement = 2*TN/(satirtop2+sutuntop2)
      negative_agreement_se = sqrt(4*TN*(FP+FN)*(TN+FP+FN))/(sutuntop2+satirtop2)^2
      negative_agreement_low = negative_agreement + qnorm((1-CI/100)/2)*negative_agreement_se
      negative_agreement_upp = negative_agreement - qnorm((1-CI/100)/2)*negative_agreement_se
      ## Rand index
      ## http://www.cs.pomona.edu/classes/cs158/resources/158-10(EM).pdf
      rand_index = (TP+TN)/(TP+FN+FP+TN)
      ## e-measure
      ## http://michael.hahsler.net/research/recomm_lnai2002/lnai2002.pdf
      e_measure = 1/((((1-CI)/100)*(1-precision))+((1-((1-CI)/100))*(1/recall)))
      ## discriminant power 
      ## https://eva.fing.edu.uy/pluginfile.php/69453/mod_resource/content/1/7633-10048-1-PB.pdf
      discriminant_power = (sqrt(3)/pi)*(log (sensitivity_estimate / (1-sensitivity_estimate)) + log (specificity / (1-specificity)))
      ## F1 Score
      ## http://machinelearningmastery.com/classification-accuracy-is-not-enough-more-performance-measures-you-can-use/
      f1_score = 2*(((TP/(TP+FN))*(TP/(TP+FP)))/((TP/(TP+FN))+(TP/(TP+FP))))
      ## Byrt yanlilik indeksi
      byrt_bias_index = (FN-FP)/toplam
      ## Byrt  asimetrik indeks prevelansi
      byrt_prevalence_asymmetry_index = (TN-TP)/toplam
      ## Yanlilik duzeltmeli kappa
      bias_adjusted_kappa = (efficiency-0.25*((satirtop1+sutuntop1)^2+(satirtop2+sutuntop2)^2)/toplam^2)/(1-0.25*((satirtop1+sutuntop1)^2+(satirtop2+sutuntop2)^2)/toplam^2)
      ## Yanlilik duzeltmeli kappa prevelansi
      prevalence_bias_adjusted_kappa = cohens_kappa+(1-cohens_kappa)*(byrt_prevalence_asymmetry_index^2-byrt_bias_index^2)
      ## Dice'in indeksi (Czekanowski)
      dice_index = positive_agreement
      dice_index_se= positive_agreement_se
      dice_index_low = positive_agreement_low
      dice_index_upp = positive_agreement_upp
      ## Yule's Q
      yules_q = (odds_ratio - 1) / (odds_ratio + 1)
      yules_q_se = 0.5*(1-yules_q^2)*sqrt(1/TP+1/FP+1/TN+1/FN)
      yules_q_low = yules_q + qnorm((1-CI/100)/2)*yules_q_se
      yules_q_upp = yules_q - qnorm((1-CI/100)/2)*yules_q_se
      
      equitable_threatscore = (TP-(sutuntop1*satirtop1/toplam))/(TP+FP+FN-(sutuntop1*satirtop1/toplam))
      ## Phi
      phi = (TP*TN-FN*FP) / sqrt(satirtop1 * satirtop2 * sutuntop1 * sutuntop2)
      phi_se = sqrt((1-phi^2+(phi+0.5*phi^3)*((satirtop1-satirtop2)*(sutuntop1-sutuntop2)/sqrt(satirtop1*satirtop2*sutuntop1*sutuntop2))-0.75*phi^2*((satirtop1-satirtop2)^2/(satirtop1*satirtop2)+(sutuntop1-sutuntop2)^2/(sutuntop1*sutuntop2)))/toplam)
      phi_low = phi + qnorm((1-CI/100)/2)*phi_se
      phi_upp = phi - qnorm((1-CI/100)/2)*phi_se
      ## Cramer V katsayisi
      cramer_v = sqrt((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2))/toplam)
      ## Olaganlik katsayisi
      contingency_coefficient = sqrt((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2))/(((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)))+toplam))
      ## Goodman and Kruskal Gamma 
      goodman_and_kruskal_gamma = (TP*TN-FN*FP)/(TP*TN+FN*FP)
      goodman_and_kruskal_gamma_se = 0.5*(1-yules_q^2)*sqrt(1/TP+1/FP+1/TN+1/FN)
      goodman_and_kruskal_gamma_low = yules_q + qnorm((1-CI/100)/2)*yules_q_se
      goodman_and_kruskal_gamma_upp = yules_q - qnorm((1-CI/100)/2)*yules_q_se
      ## Kendall's tau a
      kendalls_tau_a = (TP*TN-FN*FP)/(toplam*(toplam-1)/2)
      ## Kendall's tau b
      ## https://v8doc.sas.com/sashtml/stat/chap28/sect20.htm
      ## http://slideplayer.com/slide/10766029/
      kendalls_tau_b = (TP*TN-FN*FP)/sqrt((TP*TN+FN*FP+(TP*FN+FP*TN))*(TP*TN+FN*FP+(TP*FP+FN*TN)))
      ## Kendall's tau c
      kendalls_tau_c = ((2*2)*(TP*TN-FN*FP)) / (toplam^2 * (2-1))
      ## Somers'd R|C
      somers_d = (TP*TN-FN*FP)/(TP*TN+FN*FP+(TP*FN+FP*TN))
      ## Somers'd C|R
      somers_d_2 = (TP*TN-FP*FN)/(TP*TN+FP*FN+(TP*FP+FN*TN))
      ## Scoot'un karar indeksi
      scotts_agreement_index = bias_adjusted_kappa
      ## Dort-duzeyli Korelasyon
      tetrachoric_correlation = cos(pi/(1+sqrt(TP*TN/FN/FP)))
      tetrachoric_correlation_se = 0.375*(1-tetrachoric_correlation^2)*sqrt((1/(TP))+(1/(FP))+(1/(TN))+(1/(FN)))
      tetrachoric_correlation_low = tetrachoric_correlation + qnorm((1-CI/100)/2)*tetrachoric_correlation_se
      tetrachoric_correlation_upp = tetrachoric_correlation - qnorm((1-CI/100)/2)*tetrachoric_correlation_se
      ## Goodman kruskal tau katsayisi
      goodman_kruskal_tau = (toplam*((TP^2+FP^2)/sutuntop1+(FN^2+TN^2)/sutuntop2)-satirtop1^2-satirtop2^2)/(toplam^2-satirtop1^2-satirtop2^2)
      goodman_kruskal_tau_se = toplam*sqrt(TP*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop1)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*TP/satirtop1-(TP^2+FN^2)/satirtop1^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+FN*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop2)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*FN/satirtop1-(TP^2+FN^2)/satirtop1^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+FP*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop1)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*FP/satirtop2-(FP^2+TN^2)/satirtop2^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+TN*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop2)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*TN/satirtop2-(FP^2+TN^2)/satirtop2^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2)/(toplam^2-sutuntop1^2-sutuntop2^2)^2
      goodman_kruskal_tau_low = goodman_kruskal_tau + qnorm((1-CI/100)/2)*goodman_kruskal_tau_se
      goodman_kruskal_tau_upp = goodman_kruskal_tau - qnorm((1-CI/100)/2)*goodman_kruskal_tau_se
      ## Simetrik Lambda 
      lambda_symmetric = ((max(TP,FN)+max(FP,TN)+max(TP,FP)+max(FN,TN)-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2))/(2*toplam-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2)))
      if (lambda_symmetric==0 || lambda_symmetric==1)
      {lambda_symmetric_se = 0}
      else
      {
        lambda_symmetric_se = sqrt((TP*((if(TP>FN) 1 else 0) + (if(TP>FP) 1 else 0) + ((if(sutuntop1>sutuntop2) 1 else 0)+ (if(satirtop1>satirtop2) 1 else 0))*(lambda_symmetric-1))^2+FN*( (if(TP>FN) 0 else 1)+ (if(FN>TN) 1 else 0) +( (if(sutuntop1>sutuntop2) 0 else 1) + (if(satirtop1>satirtop2) 1 else 0))*(lambda_symmetric-1))^2+FP*( (if(FP>TN) 1 else 0) + (if(TP>FP) 0 else 1) +( (if(sutuntop1>sutuntop2) 1 else 0) + (if(satirtop1<satirtop2) 1 else 0))*(lambda_symmetric-1))^2+TN*((if(FP>TN) 0 else 1)+ (if(FN>TN) 0 else 1) +((if(sutuntop1>sutuntop2) 0 else 1)+ (if(satirtop1<satirtop2) 1 else 0))*(lambda_symmetric-1))^2-4*toplam*lambda_symmetric^2)/(2*toplam-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2))^2)
      }
      
      lambda_symmetric_low = lambda_symmetric + qnorm((1-CI/100)/2)*lambda_symmetric_se
      lambda_symmetric_upp = lambda_symmetric - qnorm((1-CI/100)/2)*lambda_symmetric_se
      ## Lambda Asymmetric R|C
      lambda_criterion = (max(TP,FP)+max(FN,TN)-max(satirtop1,satirtop2))/(toplam-max(satirtop1,satirtop2))
      
      if (lambda_criterion==0 || lambda_criterion==1)
      {lambda_criterion_se = 0}
      else
      {
        lambda_criterion_se = sqrt((toplam-max(TP,FP)-max(FN,TN))*(max(TP,FP)+max(FN,TN)+max(satirtop1,satirtop2)-2*(if(satirtop1>satirtop2) max(TP,FN) else max(FP,TN)))/(toplam-max(satirtop1,satirtop2))^3)
      }
      
      lambda_criterion_low = lambda_criterion + qnorm((1-CI/100)/2)*lambda_criterion_se
      lambda_criterion_upp = lambda_criterion - qnorm((1-CI/100)/2)*lambda_criterion_se
      ## Lambda Asymmetric C|R
      lambda_criterion_2 = (max(TP,FN)+max(FP,TN)-max(sutuntop1,sutuntop2))/(toplam-max(sutuntop1,sutuntop2))
      
      if (lambda_criterion_2==0 || lambda_criterion_2==1)
      {lambda_criterion_se_2 = 0}
      else
      {
        lambda_criterion_se_2 = sqrt((toplam-max(TP,FN)-max(FP,TN))*(max(TP,FN)+max(FP,TN)+max(sutuntop1,sutuntop2)-2*(if(sutuntop1>sutuntop2) max(TP,FP) else max(FN,TN)))/(toplam-max(sutuntop1,sutuntop2))^3)
      }
      
      lambda_criterion_low_2 = lambda_criterion_2 + qnorm((1-CI/100)/2)*lambda_criterion_se_2
      lambda_criterion_upp_2 = lambda_criterion_2 - qnorm((1-CI/100)/2)*lambda_criterion_se_2
      ## belirsizlik katsayisi (coefficient of uncertainty) simetrik
      uncertainty_coefficient_symmetric = 2*(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam)))/(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))
      uncertainty_coefficient_symmetric_se = 2*sqrt(TP*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop1*sutuntop1/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(TP/toplam))^2+FN*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop1*sutuntop2/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(FN/toplam))^2+FP*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop2*sutuntop1/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(FP/toplam))^2+TN*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop2*sutuntop2/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(TN/toplam))^2)/toplam/(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam)^2
      uncertainty_coefficient_symmetric_low = uncertainty_coefficient_symmetric + qnorm((1-CI/100)/2)*uncertainty_coefficient_symmetric_se
      uncertainty_coefficient_symmetric_upp = uncertainty_coefficient_symmetric - qnorm((1-CI/100)/2)*uncertainty_coefficient_symmetric_se
      ## (coefficient of uncertainty) R|C
      uncertainty_coeff_crit = (satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam)))/(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))
      uncertainty_coeff_crit_se = sqrt(TP*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(TP/sutuntop1)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop1/toplam))^2+FN*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(FN/sutuntop2)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop1/toplam))^2+FP*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(FP/sutuntop1)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop2/toplam))^2+TN*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(TN/sutuntop2)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop2/toplam))^2)/toplam/((satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam)^2
      uncertainty_coeff_crit_low = uncertainty_coeff_crit + qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se
      uncertainty_coeff_crit_upp = uncertainty_coeff_crit - qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se
      ## (coefficient of uncertainty) C|R
      uncertainty_coeff_crit_2 = (sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)+satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)-(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam)))/(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))
      uncertainty_coeff_crit_se_2 = sqrt(TP*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(TP/satirtop1)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop1/toplam))^2+FP*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(FP/satirtop2)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop1/toplam))^2+FN*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(FN/satirtop1)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop2/toplam))^2+TN*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(TN/satirtop2)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop2/toplam))^2)/toplam/((sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)^2
      uncertainty_coeff_crit_low_2 = uncertainty_coeff_crit_2 + qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se_2
      uncertainty_coeff_crit_upp_2 = uncertainty_coeff_crit_2 - qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se_2
      ## Yule s Y (Coefficient of colligation)
      yule_y = (sqrt(TP*TN)-sqrt(FN*FP))/(sqrt(TP*TN)+sqrt(FN*FP))
      ## Pearson ki-kare
      pearson_chi_squ = toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)
      pearson_chi_squ_p = dchi(pearson_chi_squ, df=1)
      ## 
      with_yate_cor_for_pearson = toplam*(abs(TP*TN-FN*FP)-0.5*toplam)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)
      with_yate_cor_p_for_pearson = dchi(with_yate_cor_for_pearson, df=1)
      ## Mantel Haenszel chi-square
      ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
      mantel_haenszel = ((toplam-1)*(TP*TN-FN*FP)^2)/(satirtop1*satirtop2*sutuntop1*sutuntop2)
      mantel_haenszel_p = dchi(mantel_haenszel, df=1)
      ## Olasilik orani
      likelih_ratio = 2*(TP*log(TP/(sutuntop1*satirtop1/toplam))+FN*log(FN/(sutuntop2*satirtop1/toplam))+FP*log(FP/(sutuntop1*satirtop2/toplam))+TN*log(TN/(sutuntop2*satirtop2/toplam)))
      likelih_ratio_p =dchi(likelih_ratio, df=1)
      ## Fisher'in exact testi
      ##fisher_exact_test = (TP-(satirtop1*sutuntop1/toplam))^2/(satirtop1*sutuntop1/toplam) + (FN-(satirtop1*sutuntop2/toplam))^2/(satirtop1*sutuntop2/toplam) + (FP-(satirtop2*sutuntop1/toplam))^2/(satirtop2*sutuntop1/toplam) + (TN-(satirtop2*sutuntop2/toplam))^2/(satirtop2*sutuntop2/toplam)
      fisher <- c(TP, FN, FP, TN)
      tab <- t(matrix(fisher, nrow=2,ncol=2))
      fisher_exact_test = fisher.test(tab)$p.value
      minimum_exp_fre = min((sutuntop1*satirtop1/toplam):(sutuntop2*satirtop2/toplam))
      cells_exp_fre_5 = (if((sutuntop1*satirtop1/toplam)<5) 1 else 0)+ (if((sutuntop1*satirtop2/toplam)<5) 1 else 0)+ (if((sutuntop2*satirtop1/toplam) <5) 1 else 0)+ (if((sutuntop2*satirtop2/toplam) <5) 1 else 0)
      cells_exp_fre_1 = (if((sutuntop1*satirtop1/toplam) <1) 1 else 0)+ (if((sutuntop1*satirtop2/toplam) <1) 1 else 0)+ (if((sutuntop2*satirtop1/toplam) <1) 1 else 0)+ (if((sutuntop2*satirtop2/toplam) <1) 1 else 0)
      ## Mc Nemar testi
      mcNemar_test = (FP-FN)^2/(FP+FN)
      mcNemar_test_p = dchi(mcNemar_test, df=1)
      with_Yate_cor_for_mcnamer = (abs(FP-FN)-1)^2/(FP+FN)
      with_Yate_cor_p_for_mcnamer = dchi(with_Yate_cor_for_mcnamer, df=1)
      
      ## Belirsizlik (Entropi)
      forbes_NMI = sensitivity_estimate*predvalue_positiverandomtest*log((sensitivity_estimate/predvalue_positiverandomtest_mar_ol),base=2)+
        (1-specificity)*(1-predvalue_positiverandomtest)*log(((1-specificity)/predvalue_positiverandomtest_mar_ol),base=2)+
        (1-sensitivity_estimate)*predvalue_positiverandomtest*log(((1-sensitivity_estimate)/(1-predvalue_positiverandomtest_mar_ol)),base=2)+
        specificity*(1-predvalue_positiverandomtest)*log((specificity/(1-predvalue_positiverandomtest_mar_ol)),base=2)
      ## satir icin entropy (test icin)
      entropy_hr = - ((sutuntop1/toplam) * log(sutuntop1/toplam, base=2) + (sutuntop2/toplam) * log(sutuntop2/toplam, base=2))
      ## sutun icin entropy (hastalik icin)  
      entropy_hc = - ((satirtop1/toplam)*log(satirtop1/toplam, base=2) + (satirtop2/toplam)*log(satirtop2/toplam, base=2))
      ## birlesik entropi (joint entropy)  
      entropy_hrc = - ( (TP/toplam)*log(TP/toplam, base=2) + (FP/toplam)*log(FP/toplam, base=2) + (FN/toplam)*log(FN/toplam, base=2) + (TN/toplam)*log(TN/toplam, base=2))
      ## bilgi icerigi (mutual information)  
      information_r_c = entropy_hr + entropy_hc - entropy_hrc
      ## kosullu entropi (conditional entropy)  
      a = entropy_hrc - entropy_hr
      c = entropy_hrc - entropy_hc
      sim_r_c = information_r_c / (a + information_r_c + c)
      dis_r_c = (a + c) / (a + information_r_c + c)
      ## goreli entropi (relative entropy, kullback-leibler uzakligi)
      ## pozitif test sonucu icin goreli entropi, Relative Improvement Over Chance (RIOC)
      positive_relative_entropy = (TP/sutuntop1)*log(((TP/sutuntop1)/ prevalence), base=2)+(FP/sutuntop1)*log(((FP/sutuntop1)/(1- prevalence)), base=2)
      ## negatif test sonucu icin goreli entropi 
      negative_relative_entropy = (FN/sutuntop2)*log(((FN/sutuntop2)/ prevalence), base=2)+(TN/sutuntop2)*log(((TN/sutuntop2)/(1- prevalence)), base=2)
      
      
      verim <- data.frame(Estimate = format(round(sensitivity_estimate, digit=3),nsmall=3), EstimatePercent = percent(round(sensitivity_estimate, digit=3)), LowerCI = format(round(sensitivity_low, digit=3),nsmall=3), UpperCI = format(round(sensitivity_upp, digit=3),nsmall=3))
      newRow <- data.frame(Estimate = format(round(sensitivity_randomtest, digit =3),nsmall=3), EstimatePercent = percent(round(sensitivity_randomtest, digit=3)), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(qualityindex_sensitivity, digit =3),nsmall=3), EstimatePercent = percent(round(qualityindex_sensitivity, digit=3)), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(specificity, digit=3),nsmall=3), EstimatePercent = percent(round(specificity, digit=3)), LowerCI = format(round(specificity_low, digit =3),nsmall=3), UpperCI = format(round(specificity_upp, digit =3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(specificity_randomtest, digit =3),nsmall=3), EstimatePercent = percent(round(specificity_randomtest, digit=3)), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)   
      newRow <- data.frame(Estimate = format(round(qualityindex_specificity, digit =3),nsmall=3), EstimatePercent = percent(round(qualityindex_specificity, digit=3)), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(gain_in_certainty, digit =3),nsmall=3), EstimatePercent = percent(round(gain_in_certainty, digit=3)), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(efficiency, digit =3),nsmall=3), EstimatePercent = percent(round(efficiency, digit =3)), LowerCI = format(round(efficiency_low, digit =3),nsmall=3), UpperCI = format(round(efficiency_upp, digit =3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(efficiency_randomtest, digit =3),nsmall=3), EstimatePercent = percent(round(efficiency_randomtest, digit=3)), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow) 
      #### #mis_eff <- data.frame(Symbol = 'M_EFF', Estimate = round(mis_efficiency, digit = 3), se = 0, LowerCI = 0, UpperCI = 0)
      newRow <- data.frame(Estimate = format(round(quality_index, digit=3),nsmall=3), EstimatePercent = percent(round(quality_index, digit=3)), LowerCI = format(round(quality_index_low, digit =3),nsmall=3), UpperCI = format(round(quality_index_upp, digit =3),nsmall=3))
      verim <- rbind(verim,newRow)  
      newRow<- data.frame(Estimate = format(round(youdens_index, digit =3),nsmall=3), EstimatePercent = percent(round(youdens_index, digit=3)), LowerCI = format(round(youdens_index_low, digit =3),nsmall=3), UpperCI = format(round(youdens_index_upp, digit =3),nsmall=3))
      verim <- rbind(verim,newRow) 
      newRow <- data.frame(Estimate = format(round(number_needed_to_diagnose, digit =3),nsmall=3), EstimatePercent = percent(round(number_needed_to_diagnose, digit=3)), LowerCI = 0, UpperCI = 0)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(number_needed_to_misdiagnose, digit =3),nsmall=3), EstimatePercent = percent(round(number_needed_to_misdiagnose, digit=3)), LowerCI = 0, UpperCI = 0)
      verim <- rbind(verim,newRow)
     newRow <- data.frame(Estimate = format(round(predictivevalue_positivetest, digit =3),nsmall=3), EstimatePercent = percent(round(predictivevalue_positivetest, digit =3)), LowerCI = format(round(predictivevalue_positivetest_low, digit =3),nsmall=3), UpperCI = format(round(predictivevalue_positivetest_upp, digit =3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow<- data.frame(Estimate = format(round(predvalue_positiverandomtest, digit =3),nsmall=3), EstimatePercent = percent(round(predvalue_positiverandomtest, digit=3)),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      newRow<- data.frame(Estimate = format(round(predictivevalue_negativetest, digit =3),nsmall=3), EstimatePercent = percent(round(predictivevalue_negativetest, digit =3)), LowerCI = format(round(predictivevalue_negativetest_low, digit =3),nsmall=3), UpperCI = format(round(predictivevalue_negativetest_upp, digit =3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(predvalue_negativerandomtest, digit =3),nsmall=3), EstimatePercent = percent(round(predvalue_negativerandomtest, digit=3)), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(predictive_summary_index, digit =3),nsmall=3), EstimatePercent = percent(round(predictive_summary_index, digit=3)), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(false_positiverate, digit =3),nsmall=3), EstimatePercent = percent(round(false_positiverate, digit=3)), LowerCI = format(round(false_positiverate_low, digit =3),nsmall=3), UpperCI = format(round(false_positiverate_upp, digit =3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow  <- data.frame(Estimate = format(round(false_negativerate, digit =3),nsmall=3),  EstimatePercent = percent(round(false_negativerate, digit=3)), LowerCI = format(round(false_negativerate_low, digit =3),nsmall=3), UpperCI = format(round(false_negativerate_upp, digit =3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow  <- data.frame(Estimate = format(round(false_omission_rate, digit =3),nsmall=3),  EstimatePercent = percent(round(false_omission_rate, digit=3)), LowerCI = format(round(false_omission_rate_low, digit =3),nsmall=3), UpperCI = format(round(false_omission_rate_upp, digit =3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow  <- data.frame(Estimate = format(round(false_discovery_rate, digit =3),nsmall=3),  EstimatePercent = percent(round(false_discovery_rate, digit=3)), LowerCI = format(round(false_discovery_rate_low, digit =3),nsmall=3), UpperCI = format(round(false_discovery_rate_upp, digit =3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(misclassification_rate, digit =3),nsmall=3),  EstimatePercent = percent(round(misclassification_rate, digit=3)), LowerCI = format(round(misclassification_rate_low, digit =3),nsmall=3), UpperCI = format(round(misclassification_rate_upp, digit =3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(rand_index, digit =3),nsmall=3), EstimatePercent = percent(round(rand_index, digit=3)), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(balanced_accuracy, digit=3),nsmall=3), EstimatePercent = percent(round(balanced_accuracy, digit=3)), LowerCI = 0, UpperCI = 0)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(lift, digit=3),nsmall=3), EstimatePercent = percent(round(lift, digit=3)), LowerCI = 0, UpperCI = 0)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(precision, digit=3),nsmall=3),EstimatePercent = percent(round(precision, digit=3)), LowerCI = 0, UpperCI = 0)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(recall, digit=3),nsmall=3),EstimatePercent = percent(round(recall, digit=3)), LowerCI = 0, UpperCI = 0)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(e_measure, digit =3),nsmall=3), EstimatePercent = percent(round(e_measure, digit=3)), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(conviction, digit =3),nsmall=3), EstimatePercent = percent(round(conviction, digit=3)), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(leverage, digit =3),nsmall=3), EstimatePercent = percent(round(leverage, digit=3)), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(discriminant_power, digit=3),nsmall=3),EstimatePercent = percent(round(discriminant_power, digit=3)), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(f1_score, digit=3),nsmall=3),EstimatePercent = percent(round(f1_score, digit=3)), LowerCI = 0, UpperCI = 0)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(matthews_correlation_coefficient, digit=3),nsmall=3),EstimatePercent = percent(round(matthews_correlation_coefficient, digit=3)), LowerCI = 0, UpperCI = 0)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(entropy_hr, digit =3),nsmall=3),  EstimatePercent = percent(round(entropy_hr, digit=3)), LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(entropy_hc, digit =3),nsmall=3),  EstimatePercent = percent(round(entropy_hc, digit=3)), LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(entropy_hrc, digit =3),nsmall=3),  EstimatePercent = percent(round(entropy_hrc, digit=3)), LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(information_r_c, digit =3),nsmall=3), EstimatePercent = percent(round(information_r_c, digit=3)),  LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(a, digit =3),nsmall=3), EstimatePercent = percent(round(a, digit=3)),  LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(c, digit =3),nsmall=3), EstimatePercent = percent(round(c, digit=3)),  LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(sim_r_c, digit =3),nsmall=3), EstimatePercent = percent(round(sim_r_c, digit=3)),  LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(dis_r_c, digit =3),nsmall=3), EstimatePercent = percent(round(dis_r_c, digit=3)),  LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(positive_relative_entropy, digit =3),nsmall=3), EstimatePercent = percent(round(positive_relative_entropy, digit=3)),  LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(negative_relative_entropy, digit =3),nsmall=3), EstimatePercent = percent(round(negative_relative_entropy, digit=3)), LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      
      
      rownames(verim) = c("Sensitivity (True Positive Rate)", "Sensitivity of a random test", "Quality index of sensitivity", 
                          "Specificity (True Negative Rate)"
                          , "Specificity of a random test",
                          "Quality index of specificity","Gain in Certainty (Overall Accuracy)", "Efficiency (Accuracy - Correct classification rate -  Overall Fraction Correct)", "Efficiency of a random test", 
                          "Quality index", "Youden\'s index", "Number Needed to Diagnose", "Number Needed to Misdiagnose",
                          "Predictive value of positive test (PPV)","Pred. value of a positive random test", "Predictive value of negative test (NPV)",
                          "Pred. value of a negative random test","Predictive Summary Index (PSI)", "False positive rate (FPR)",
                          "False negative rate (FNR)", "False Omission Rate (FOR)", "False Discovery Rate (FDR)",
                          "Misclassification rate (Error classification rate - Proportion Incorrectly Classified)", "Rand Index (Rand Measure)", 
                          "Balanced Accuracy (Average Accuracy)", "Lift (Interest)",  "Precision", "Recall","E-Measure","Conviction",
                          "Leverage", "Discriminant Power","F1 Score (F Measure)",
                          "Matthew\'s Correlation Coefficient","Entropy for true", "Entropy for False",
                          "Joint Entropy", "Mutual Information", "Joint Entropy - Entropy for true", "Joint Entropy - Entropy for false",
                          "Similarity of Descriptors True and False", "Distance Between True and False", "Relative Improvement Over Chance (RIOC)", 
                          "Relative Entropy for Negative Test"
      )
      
      #verim = data.frame(verim,row.names = TRUE)
      return(verim[,c(1,2,3,4)])
      
    }
  })

  my_data <- reactive({
    testdata <- dataM0()
    as.data.frame(cbind(Interpretation = shinyInput(actionButton, nrow(testdata),'button_', label = "Interpretation", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' ),testdata))
  })
  
  output$dataUpload0 <- DT::renderDataTable(my_data(),selection = 'single',rownames= TRUE,extensions = c('Buttons','KeyTable', 'Responsive'),
                                            options = list(pageLength = 25,lengthMenu = c(25, 50, 75, 100),
                                                                                          dom = 'Bfrtip',
                                                                                          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),keys = TRUE,
                                                           preDrawCallback = JS("function() { 
                                         Shiny.unbindAll(this.api().table().node()); }"), 
                                                                              drawCallback = JS("function() { 
                                                                                                Shiny.bindAll(this.api().table().node()); } ")),
                                            server = FALSE, escape = FALSE)
  
  
  
  # Here I created a reactive to save which row was clicked which can be stored for further analysis
  SelectedRow <- eventReactive(input$select_button,{
    as.numeric(strsplit(input$select_button, "_")[[1]][2])
  })
  
  # This is needed so that the button is clicked once for modal to show, a bug reported here
  # https://github.com/ebailey78/shinyBS/issues/57
  observeEvent(input$select_button, {
    toggleModal(session, "modalExample", "open")
  })
  
  DataRow <- eventReactive(input$select_button,{
    my_data()[SelectedRow(),2:ncol(my_data())]
  })

  
  output$popup0 <- renderUI({
    if (input$verigir_kontrol == TRUE){ 
      TP = 0
      FN = 0
      FP = 0
      TN = 0
      for (i in 1:nrow(tabloveri()))
      {
        if (tabloveri()$test[i]==1 & tabloveri()$real_situation[i]==1)
        {TP = TP+1}
        else if (tabloveri()$test[i]==1 & tabloveri()$real_situation[i]==2)
        {FN = FN+1}
        else if (tabloveri()$test[i]==2 & tabloveri()$real_situation[i]==1)
        {FP = FP+1}
        else if (tabloveri()$test[i]==2 & tabloveri()$real_situation[i]==2)
        {TN = TN+1}
        next
      }
      CI = as.numeric(trimws(input$text5_2))
    }
    else{
      TP = as.numeric(trimws(input$text1)) ### x
      FN = as.numeric(trimws(input$text2)) ### y
      FP = as.numeric(trimws(input$text3)) ### z
      TN = as.numeric(trimws(input$text4)) ### t
      CI = as.numeric(trimws(input$text5))
    }

    if (rownames(DataRow()) == 'Sensitivity (True Positive Rate)') 
    #if (input$select_button == 'button_1') 
    {
      bsModal("modalExample", paste0("Sensitivity"), "", size = "large",
              h4("Description:"),
              print(paste0("Sensitivity is the proportion of subjects who are actually patient (positive) who have tested the patient (positive).
That is, the sensitivity of the test shows how many of the really sick people are classified correctly (as patients). 
                           It only measures how good the test is when looking at patient subjects.")),
              h4("Interpretation:"),
              h5("The sensitivity test indicates that ", TP, " people of ", TP+FN, " (", DataRow()$EstimatePercent,
                 ") patients were correctly (as patient) identified.  "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow()),
              column(12,                   
                     DT::renderDataTable(DataRow())
                     
              )
     
              
      )}
    

    else if (rownames(DataRow()) == 'Specificity (True Negative Rate)') 
    {
      bsModal("modalExample", paste0("Specificity"), "", size = "large",
              h4("Description:"),
              print(paste0("Specificity is the proportion of subjects who are actually non-patient (negative) who have tested the non-patient (negative). 
That is, the possibility of a negative test result between those who participate in the study and those who are not ill.
The specificity of the test indicates that what is not sick is actually classified correctly (non-sick). 
                           It gives a measure of how good the test is when looking at only non-patient subjects.")),
              h4("Interpretation:"),
              h5("The specificity test indicates that ", TN, " of ", TN+FP, " people (", DataRow()$EstimatePercent,
                 ") who are not really sick are classifying correctly (non-sick). "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow()),
              column(12,                   
                     DT::renderDataTable(DataRow())
                     
              )
 
              
      )}

    else if (rownames(DataRow()) == 'Gain in Certainty (Overall Accuracy)') 
    {
      degeff = ' '
      if (as.numeric(as.character(DataRow()$Estimate)) == 1)
      {degeff = 'The test provides no information.'
      }
      else 
      {degeff = 'closer to 1, so it is a very good score.'
      }
      bsModal("modalExample", paste0("Gain in Certainty"), "", size = "large",
              h4("Description:"),
              print(paste0("The gain in the certainty that a condition is present is the difference between the post-test 
                           probability and the prior probability (the prevalence) when the test is positive. 
                           The gain in certainty that there is no disease is the difference between post-test probability 
                           of no disease and the prior probability of no disease (1-prevalence). Gain in certainty varies 
                           from 0 to 2 and a result of 1 indicates that the diagnostic test does not add to guessing; 
                           that is, the test provides no information; when Gain in  certainty is greatest, the expected gain 
                           is maximized. Expected gain is also related to the receiver operating characteristic curve for a 
                           diagnostic test: the point on the receiver operating characteristic curve at which Gain in 
                           certainty is greatest corresponds to the point at which the distance from the major diagonal is 
                           greatest at which the slope of the receiver operating characteristic curve equals 1.")),
              h4("Interpretation:"),
              h5(" "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow()),
              column(12,                   
                     DT::renderDataTable(DataRow())
                     
              )
              
              
      )}

    else if (rownames(DataRow()) == 'Efficiency (Accuracy - Correct classification rate -  Overall Fraction Correct)') 
    {
      degeff = ' '
      if (as.numeric(as.character(DataRow()$Estimate)) <= 0.5 & as.numeric(as.character(DataRow()$Estimate)) >= 0)
      {degeff = 'closer to 0, so it is not a very good score.'
      }
      else 
      {degeff = 'closer to 1, so it is a very good score.'
      }
      bsModal("modalExample", paste0("Efficiency (Accuracy - Correct classification rate)"), "", size = "large",
              h4("Description:"),
              print(paste0("Accuracy is the ratio of the correct test result to the total number 
of subjects that is the sum of the true negative and true positive results.
Accuracy is the ratio of actual results (true positive or true negative) among subjects participating in the study. 
                           It measures the accuracy of the diagnostic test." )),
              h4("Interpretation:"),
              h5("The likelihood that ", TP+FP, " of the test-result-positive patients were actually one of the ", TP, " patients who were sick is ", DataRow()$EstimatePercent,
                 "."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow()),
              column(12,                   
                     DT::renderDataTable(DataRow())
                     
              )
              
      )}

    else if (rownames(DataRow()) == 'Youden\'s index') 
    {
      bsModal("modalExample", paste0("Youden\'s Index"), "", size = "large",
              h4("Description:"),
              print(paste0("This is an index that summarizes the sensitivity and authenticity of a test.  
                           Youden Index is 0 if the diagnostic test has weak accuracy and 1 if it has strong accuracy. 
                           It is used for the evaluation of overall discriminative power of a diagnostic procedure and for comparison of 
                           this test with other tests. The disadvantage of the Youden Index is that it is not sensitive to differences 
                           in sensitivity and specificity. It is not a bad index as an approximation to the overall performance of the test,
                           but it is not recommended to use it as a single parameter to evaluate any diagnostic test. 
                           Youden\'s index is not affected by the disease prevalence, but it is affected by the spectrum of the disease, 
                           as are also sensitivity specificity, likelihood ratios and odds ratio.")),
              h4("Interpretation:"),
              h5(" "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow()),
              column(12,                   
                     DT::renderDataTable(DataRow())
                     
              )

              
      )}

    else if (rownames(DataRow()) == 'Number Needed to Diagnose') 
    {
      bsModal("modalExample", paste0("Number Needed to Diagnose"), "", size = "large",
              h4("Description:"),
              print(paste0("The number needed to diagnose is defined as the number of paitents that need to be tested to give one correct positive test. 
                           It is used to calculate the number of patients that need to be examined in order to correctly detect one person with the disease.")),
              h4("Interpretation:"),
              h5(" "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow()),
              column(12,                   
                     DT::renderDataTable(DataRow())
                     
              )
              
              
      )}

    else if (rownames(DataRow()) == 'Number Needed to Misdiagnose') 
    {
      bsModal("modalExample", paste0("Number Needed to Misdiagnose"), "", size = "large",
              h4("Description:"),
              print(paste0("The number needed to misdiagnose, defined as the number of patients who need to be tested in order for one to be misdiagnosed (FP or FN) by the test.
                           The higher the NNM of a test, the closer is the test to the gold-standard, hence, a better test.")),
              h4("Interpretation:"),
              h5("One out of ", DataRow()$Estimate ," men tested is misdiagnosed. "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow()),
              column(12,                   
                     DT::renderDataTable(DataRow())
                     
              )
              
              
      )}

    else if (rownames(DataRow()) == 'Predictive value of positive test (PPV)') 
    {
      bsModal("modalExample", paste0("Predictive Value of Positive Test"), "", size = "large",
              h4("Description:"),
              print(paste0("The positive test value is the proportion of patients (positive) 
who are actually within the total number of subjects (positive) as a result of the diagnostic test. 
It is possible that someone who is positive for the test result is sick. ")),
              h4("Interpretation:"),
              h5("The likelihood that ", TP+FP, " of the test-result-positive patients were actually one of the ", TP, 
                 " patients who were sick is ", DataRow()$EstimatePercent,"."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow()),
              column(12,                   
                     DT::renderDataTable(DataRow())
                     
              )
  
              
      )}

    else if (rownames(DataRow()) == 'Predictive value of negative test (NPV)') 
    {
      bsModal("modalExample", paste0("Predictive Value of Negative Test"), "", size = "large",
              h4("Description:"),
              print(paste0("The negative test value is the ratio of those who are truly non-patients (negative) among all subjects who are non-patient (negative) as a result of the diagnostic test. 
The predictive value of negative test of the test result indicates the likelihood that a person known to be negative is not sick.")),
              h4("Interpretation:"),
              h5("The probability that one of the ", FN+TN, " people known to be negative for the test result is one of the ",
                 TN, " ill patients is ", DataRow()$EstimatePercent, " ."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow()),
              column(12,                   
                     DT::renderDataTable(DataRow())
                     
              )
    
              
              )}
    else if (rownames(DataRow()) == 'False positive rate (FPR)') 
    {
      bsModal("modalExample", paste0("False Positive Rate"), "", size = "large",
              h4("Description:"),              
              print(paste0("False Positive (also known as false alarm) are predictions that should be false but were predicted as true. 
                           A false positive occurs when the test reports a positive result (patient) for a person who is disease free (non-patient). 
                           ")),
              h4("Interpretation:"),
              h5("The likelihood that a person who is not actually sick is accidentally positive ", DataRow()$Estimate ,"."),              
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow()),
              column(12,                   
                     DT::renderDataTable(DataRow())
                     
              )
              
              
              )}
    else if (rownames(DataRow()) == 'False negative rate (FNR)') 
    {
      bsModal("modalExample", paste0("False Negative Rate"), "", size = "large",
              h4("Description:"),              
              print(paste0("A false negative occurs when the test reports a negative result (non-patient) for a person who actually has the disease (patient). 
                           In real patients, the test is accidental, not sick. 
                           ")),
              h4("Interpretation:"),
              h5("The likelihood that someone who is actually known to be ill actually accidentally goes out of the test ", DataRow()$Estimate, "."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow()),
              column(12,                   
                     DT::renderDataTable(DataRow())
                     
              )
  
              
              )}
    else if (rownames(DataRow()) == 'Misclassification rate (Error classification rate)') 
    {
      degeff = ' '
      if (as.numeric(as.character(DataRow()$Estimate)) <= 0.5 & as.numeric(as.character(DataRow()$Estimate)) >= 0)
      {degeff = 'closer to 0, so it is not a very good score.'
      }
      else 
      {degeff = 'closer to 1, so it is a very good score.'
      }
      bsModal("modalExample", paste0("Misclassification rate (Error classification rate - Proportion Incorrectly Classified)"), "", size = "large",
              h4("Description:"),              
              print(paste0("The misclassification rate is the proportion of those individuals incorrectly categorized by the test 
                           (those with disease who had a negative test plus those without disease who had a positive test result). This measures the portion of all decisions that were incorrect decisions. 
                           It falls in the range from 0 to 1, with 0 being the best score. "
                           )),
              h4("Interpretation:"),
              h5(DataRow()$Estimate , " is ", degeff, "."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow()),
              column(12,                   
                     DT::renderDataTable(DataRow())
                     
              )
        
              
      )}
    else if (rownames(DataRow()) == 'Rand Index (Rand Measure)') 
    {
      bsModal("modalExample", paste0("Rand index (Rand measure)"), "", size = "large",
              h4("Description:"),              
              print(paste0("The Rand index or Rand measure (named after William M. Rand) in statistics, and in particular in data clustering, is a measure of the 
similarity between two data clusterings. From a mathematical standpoint, Rand index is related to the accuracy, but is applicable even when class labels are not used. The Rand index has a value between 0 and 1, 
                           with 0 indicating that the two data groups do not agree on any pair of points and 1 indicating that the data groups are exactly the same. "
              )),
              h4("Interpretation:"),
              h5(" "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow()),
              column(12,                   
                     DT::renderDataTable(DataRow())
                     
              )
              
              
              )}
    else if (rownames(DataRow()) == 'Balanced Accuracy (Average Accuracy)') 
    {
bsModal("modalExample", paste0("Balanced Accuracy (Average Accuracy)"), "", size = "large",
        h4("Description:"),      
        print(paste0("The balanced accuracy is the average of sensitivity and specificity can be defined also as the average accuracy obtained on either class.")),
        h4("Interpretation:"),
        h5(" "),      
        h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow()),
              column(12,                   
                     DT::renderDataTable(DataRow())
                     
              )
     
              
)}
    else if (rownames(DataRow()) == 'Lift (Interest)') 
    {
      bsModal("modalExample", paste0("Lift (Interest)"), "", size = "large",
              h4("Description:"),      
              print(paste0("Lift is an alternative measure for confidence that takes the frequency of the consequent into account. 
Lift is defined as the relation of the (observed) probability of the co-occurrence of two items to the 
                           probability under the assumption that they occur independently.")),
              h4("Interpretation:"),
              h5(" "),      
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow()),
              column(12,                   
                     DT::renderDataTable(DataRow())
                     
              )
              
              
      )}
    ## precision
    else if (rownames(DataRow()) == 'Precision') 
    {

      bsModal("modalExample", paste0("Precision"), "", size = "large",
              h4("Description:"),
              print(paste0("Precision is the ratio of accurately estimated positives to all predicted positives.
                           It falls in the range from 0 to 1, with 1 being the best score. ")),
              h4("Interpretation:"),
              h5(""),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow()),
              column(12,                   
                     DT::renderDataTable(DataRow())
                     
              )
         
              
              )}
    ## recall
    else if (rownames(DataRow()) == 'Recall') 
    {

      bsModal("modalExample", paste0("Recall"), "", size = "large",
              h4("Description:"),
              print(paste0("Recall is the ratio of accurately estimated positives to all positive observations.
                           It falls in the range from 0 to 1, with 1 being the best score. " )),
              h4("Interpretation:"),
              h5(""),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow()),
              column(12,                   
                     DT::renderDataTable(DataRow())
                     
              )
 
              
      )}
    else if (rownames(DataRow()) == 'E-Measure') 
    {
      
      bsModal("modalExample", paste0("E-Measure"), "", size = "large",
              h4("Description:"),
              print(paste0("To find an optimal trade-off between precision and recall a single-valued measure like the E-measure can be used. " )),
              h4("Interpretation:"),
              h5(""),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow()),
              column(12,                   
                     DT::renderDataTable(DataRow())
                     
              )
              
              
              )}
    else if (rownames(DataRow()) == 'Discriminant Power') 
    {
      degpre = ' '
      if (as.numeric(as.character(DataRow()$Estimate)) <= 1)
      {degpre = ' smaller than 1, evaluates an algorithm distinguishes between positive and negative examples in a poor way.'
      }
      else if (as.numeric(as.character(DataRow()$Estimate)) > 1 && as.numeric(as.character(DataRow()$Estimate)) <= 2) 
      {degpre = ' between 1 and 2, evaluates an algorithm distinguishes between positive and negative examples in a limited way.'
      }
      else if (as.numeric(as.character(DataRow()$Estimate)) > 2 && as.numeric(as.character(DataRow()$Estimate)) <= 3) 
      {degpre = ' between 1 and 2, evaluates an algorithm distinguishes between positive and negative examples in a fair way.'
      }
      else if (as.numeric(as.character(DataRow()$Estimate)) > 3) 
      {degpre = ' between 1 and 2, evaluates an algorithm distinguishes between positive and negative examples in a good way.'
      }
      bsModal("modalExample", paste0("Discriminant power (DP) "), "", size = "large",
              h4("Description:"),
              print(paste0("Discriminant power (DP) is a measure that summarizes sensitivity and specificity."
                           )),
              h4("Interpretation:"),
              h5(DataRow()$Estimate, " is ", degpre),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow()),
              column(12,                   
                     DT::renderDataTable(DataRow())
                     
              )
       
              
              )}
    ##f1 score
    else if (rownames(DataRow()) == 'F1 Score (F Measure)') 
    {
      degpre = ' '
      if (as.numeric(as.character(DataRow()$Estimate)) >= 0 && as.numeric(as.character(DataRow()$Estimate)) <= 0.5)
      {degpre = 'closer to 0, so it is not a very good score.'
      }
      else if (as.numeric(as.character(DataRow()$Estimate)) > 0.5)
      {degpre = 'closer to 1, so it is a very good score.'
      }
      bsModal("modalExample", paste0("F1 Score"), "", size = "large",
              h4("Description:"),
              print(paste0("This measure is the combination of the harmonic averages of precision and recall. 
                           It falls in the range from 0 to 1, with 1 being the best score. ")),
              h4("Interpretation:"),
              h5(DataRow()$Estimate, " is ", degpre),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow()),
              column(12,                   
                     DT::renderDataTable(DataRow())
                     
              )
  
      )}
    else if (rownames(DataRow()) == 'Matthew\'s Correlation Coefficient') 
    {
      
      bsModal("modalExample", paste0("Matthew\'s Correlation Coefficient"), "", size = "large",
              h4("Description:"),
              print(paste0("The Matthews correlation coefficient is used in machine learning as a measure of the quality of binary (two-class) classifications, 
introduced by biochemist Brian W. Matthews in 1975. It takes into account true and false positives and negatives and is generally regarded as 
a balanced measure which can be used even if the classes are of very different sizes. The MCC is in essence a correlation coefficient 
between the observed and predicted binary classifications; it returns a value between -1 and +1. 
A coefficient of +1 represents a perfect prediction,
                           0 no better than random prediction and -1 indicates total disagreement between prediction and observation.  ")),
              h4("Interpretation:"),
              h5(""),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow()),
              column(12,                   
                     DT::renderDataTable(DataRow())
                     
              )
              
              )}
    else
    {
      bsModal("modalExample", paste0("Interpretation: "), "", size = "large",
              print(paste0(rownames(DataRow()) ," estimate is ", DataRow()$Estimate," and its confidence interval is ", DataRow()$LowerCI," - ",DataRow()$UpperCI," . ")),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow()),
              column(12,                   
                     DT::renderDataTable(DataRow())
                     
              )
              
              
      )}
  })
  
  output$plot0 <- highcharter::renderHighchart({
    if (input$verigir_kontrol == TRUE){ 
      TP = 0
      FN = 0
      FP = 0
      TN = 0
      for (i in 1:nrow(tabloveri()))
      {
        if (tabloveri()[i,1]==1 & tabloveri()[i,2]==1)
        {TP = TP+1}
        else if (tabloveri()[i,1]==1 & tabloveri()[i,2]==2)
        {FN = FN+1}
        else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==1)
        {FP = FP+1}
        else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==2)
        {TN = TN+1}
        next
      }
      CI = as.numeric(trimws(input$text5_2))
    }
    else{
      TP = as.numeric(trimws(input$text1)) ### x
      FN = as.numeric(trimws(input$text2)) ### y
      FP = as.numeric(trimws(input$text3)) ### z
      TN = as.numeric(trimws(input$text4)) ### t
      CI = as.numeric(trimws(input$text5))
    }
    
    
    satirtop1 = TP + FN
    satirtop2 = FP + TN
    sutuntop1 = TP + FP
    sutuntop2 = FN + TN
    toplam = satirtop1 + satirtop2
    #table1 <- matrix(c(TP,FN,satirtop1,FP,TN,satirtop2,sutuntop1,sutuntop2,toplam),nrow=3,ncol=3,byrow=TRUE,dimnames=list(c("1.satir","2.satir","Sutun toplamlari"),c("1.kolon","2.kolon","Satir toplamlari")))
    #table1
    ## Duyarlilik 
    sensitivity_estimate = TP / satirtop1
    sensitivity_se = round((sqrt(sensitivity_estimate * (1 - sensitivity_estimate)/satirtop1)), 4)
    sensitivity_low = 1 - Rbeta.inv(((1 + CI/100)/2), (satirtop1+1-TP), TP)
    sensitivity_upp = Rbeta.inv(((1 + CI/100)/2), (TP+1), (satirtop1-TP))
    sensitivity_randomtest = sutuntop1 / toplam
    qualityindex_sensitivity = (sensitivity_estimate - sensitivity_randomtest) / (1 - sensitivity_randomtest)
    ## Secicilik
    specificity = TN / satirtop2
    specificity_se = sqrt(specificity * (1 - specificity) / satirtop2)
    specificity_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop2+1-TN),TN)
    specificity_upp = Rbeta.inv((1 + CI/100)/2, (TN+1), (satirtop2-TN))
    specificity_randomtest = (1 - sensitivity_randomtest)
    qualityindex_specificity = (specificity - 1 + sensitivity_randomtest) / sensitivity_randomtest
    ## Gain in Certainty
    gain_in_certainty = sensitivity_estimate + specificity
    ## Etkililik, yeterlilik ayni zamanda overall accuracy (toplam-genel dogruluk orani)
    efficiency = (TP + TN) / toplam
    efficiency_se = sqrt(efficiency * (1 - efficiency) / toplam)
    efficiency_low = 1-Rbeta.inv((1 + CI/100)/2, (toplam+1-TP-TN), (TP+TN))
    efficiency_upp = Rbeta.inv((1 + CI/100)/2, (TP+TN+1), (toplam-TP-TN))
    efficiency_randomtest = (satirtop1 / toplam) * sensitivity_randomtest + (1 - (satirtop1 / toplam)) * (1 - sensitivity_randomtest)
    mis_efficiency = 1 - efficiency
    ## Quality indeks
    quality_index = (efficiency - efficiency_randomtest) / (1 - efficiency_randomtest)
    quality_index_se = sqrt((efficiency*(1-efficiency)/(1-efficiency_randomtest)^2+2*(1-efficiency)*(2*efficiency*efficiency_randomtest-((TP*((satirtop1 / toplam)+sensitivity_randomtest)+TN*(2-(satirtop1 / toplam)-sensitivity_randomtest))/toplam))/(1-efficiency_randomtest)^3+((1-efficiency)^2)*((TP*((satirtop1 / toplam)+sensitivity_randomtest)^2+FN*(-(satirtop1 / toplam)+1+sensitivity_randomtest)^2+FP*(1+(satirtop1 / toplam)-sensitivity_randomtest)^2+TN*(2-(satirtop1 / toplam)-sensitivity_randomtest)^2)/toplam-4*efficiency_randomtest^2)/(1-efficiency_randomtest)^4)/toplam)
    quality_index_low = quality_index + qnorm((1 - CI/100)/2)*quality_index_se
    quality_index_upp = quality_index - qnorm((1 - CI/100)/2)*quality_index_se
    ## Youden indeksi
    youdens_index = sensitivity_estimate + specificity - 1
    youdens_index_se = sqrt(sensitivity_se^2+specificity_se^2)
    youdens_index_low = youdens_index + qnorm((1 - CI/100)/2)*youdens_index_se
    youdens_index_upp = youdens_index - qnorm((1 - CI/100)/2)*youdens_index_se
    
    number_needed_to_diagnose = 1 / youdens_index
    number_needed_to_misdiagnose = 1 / ( 1 - efficiency ) 
    ## Pozitif kestirim degeri
    predictivevalue_positivetest = TP / sutuntop1
    predictivevalue_positivetest_se = sqrt(predictivevalue_positivetest*(1-predictivevalue_positivetest)/sutuntop1)
    predictivevalue_positivetest_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop1+1-TP),TP)
    predictivevalue_positivetest_upp = Rbeta.inv((1 + CI/100)/2, (TP+1),(sutuntop1-TP))
    predvalue_positiverandomtest = (satirtop1 / toplam)
    predvalue_positiverandomtest_mar_ol = predvalue_positiverandomtest*sensitivity_estimate+(1-predvalue_positiverandomtest)*(1-specificity)
    ## Negatif kestirim degeri
    predictivevalue_negativetest = TN / sutuntop2
    predictivevalue_negativetest_se = sqrt(predictivevalue_negativetest*(1-predictivevalue_negativetest)/sutuntop2)
    predictivevalue_negativetest_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop2+1-TN),TN)
    predictivevalue_negativetest_upp = Rbeta.inv((1 + CI/100)/2, (TN+1), (sutuntop2-TN))
    predvalue_negativerandomtest = 1 - (satirtop1 / toplam)
    predvalue_negativerandomtest_mar_ol = 1 - predvalue_positiverandomtest_mar_ol
    ## Predictive Summary Index (PSI)
    predictive_summary_index = predictivevalue_positivetest + predictivevalue_negativetest - 1
    ## Yanlis pozitif orani
    false_positiverate = FP / satirtop2
    false_positiverate_se = sqrt(false_positiverate*(1-false_positiverate)/satirtop2)
    false_positiverate_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop2+1-FP),FP)
    false_positiverate_upp = Rbeta.inv((1 + CI/100)/2, (FP+1), (satirtop2-FP))
    ## Yansis negatif orani
    false_negativerate = FN / satirtop1
    false_negativerate_se = sqrt(false_negativerate*(1-false_negativerate)/satirtop1)
    false_negativerate_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop1+1-FN),FN)
    false_negativerate_upp = Rbeta.inv((1 + CI/100)/2, (FN+1), (satirtop1-FN))
    ## False Omission Rate (FOR) https://www.ncss.com/software/ncss/diagnostic-tests-in-ncss/
    false_omission_rate = FN/(sutuntop2)
    false_omission_rate_se = sqrt(false_omission_rate*(1-false_omission_rate)/sutuntop2)
    false_omission_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop2+1-FN),FN)
    false_omission_rate_upp = Rbeta.inv((1 + CI/100)/2, (FN+1), (sutuntop2-FN))
    ## False Discovery Rate (FDR) https://www.ncss.com/software/ncss/diagnostic-tests-in-ncss/
    false_discovery_rate = FP/(sutuntop1)
    false_discovery_rate_se = sqrt(false_discovery_rate*(1-false_discovery_rate)/sutuntop1)
    false_discovery_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop1+1-FP),FP)
    false_discovery_rate_upp = Rbeta.inv((1 + CI/100)/2, (FP+1), (sutuntop1-FP))
    ## Dogru pozitif orani
    true_positiverate = TP / (satirtop1)
    ## Yanlis siniflandirma orani
    misclassification_rate = (FN + FP) / toplam
    misclassification_rate_se = sqrt(misclassification_rate*(1-misclassification_rate)/toplam)
    misclassification_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-FP-FN), (FP+FN))
    misclassification_rate_upp = Rbeta.inv((1 + CI/100)/2, (FP+FN+1), (toplam-FP-FN))
    ## Prevelans
    prevalence = (satirtop1 / toplam)
    prevalence_se = sqrt((satirtop1 / toplam)*(1-(satirtop1 / toplam))/toplam)
    prevalence_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-satirtop1), satirtop1)
    prevalence_upp = Rbeta.inv((1 + CI/100)/2, (satirtop1+1), (toplam-satirtop1))
    
    ##caret paketinden olculer
    detection_rate = TP/toplam
    detection_prevalence = (TP+FP)/toplam
    balanced_accuracy = (sensitivity_estimate + specificity)/2
    lift = (TP/(TP+FN))/((TP+FP)/toplam)  ## https://gerardnico.com/wiki/data_mining/error_rate
    precision = TP/(TP+FP)
    recall = TP/(TP+FN)
    ## http://michael.hahsler.net/research/recomm_lnai2002/lnai2002.pdf
    conviction = (1-((TP+FP)/toplam))/(1-(TP/(TP+FN)))
    matthews_correlation_coefficient = (TP * TN + FN * FP) / ((TP+FP)*(FN+TN)*(FP+TN)*(TP+FN))^(1/2)
    ## Goreli Risk Orani (Relative risk)
    ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
    # https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20ratio.htm
    relative_risk = (TP/sutuntop1)/(FN/sutuntop2)
    relative_risk_se = sqrt((1/TP)+(1/FN)-(1/(TP+FP))-(1/(FN+TN)))
    relative_risk_low = exp(log(relative_risk)+qnorm((1-CI/100)/2)*relative_risk_se)
    relative_risk_upp = exp(log(relative_risk)-qnorm((1-CI/100)/2)*relative_risk_se)
    difference_in_proportion = (TP/sutuntop1) - (FN/sutuntop2)
    ## Leverage
    ## https://www3.nd.edu/~tweninge/pubs/AHBCW_ANNIE08.pdf
    leverage = (TP/sutuntop1)-((TP+FP)/toplam)
    ## http://www.ebm.med.ualberta.ca/TherapyCalc.html
    control_event_rate = FN/sutuntop2
    experimental_event_rate = TP/sutuntop1
    ## Absolute risk reduction
    absolute_risk_reduction = -difference_in_proportion
    absolute_risk_reduction_se = sqrt((TP/sutuntop1)*(1-(TP/sutuntop1))/sutuntop1+(FN/sutuntop2)*(1-(FN/sutuntop2))/sutuntop2)
    absolute_risk_reduction_low = absolute_risk_reduction - qnorm((1-CI/100)/2)*absolute_risk_reduction_se
    absolute_risk_reduction_upp = absolute_risk_reduction + qnorm((1-CI/100)/2)*absolute_risk_reduction_se
    ## Relative risk reduction
    relative_risk_reduction = absolute_risk_reduction / (FN/sutuntop2)
    relative_risk_reduction_low = 1 - exp(log(relative_risk)-qnorm((1-CI/100)/2)*relative_risk_se)
    relative_risk_reduction_upp = 1 - exp(log(relative_risk)+qnorm((1-CI/100)/2)*relative_risk_se)
    ##Number needed to treat
    number_needed_to_treat = 1 / abs(difference_in_proportion)
    number_needed_to_treat_low = 1 /absolute_risk_reduction_upp
    number_needed_to_treat_upp = 1 /absolute_risk_reduction_low
    ## Test duzeyi
    test_level = sensitivity_randomtest
    test_level_se = sqrt(test_level*(1-test_level)/toplam)
    test_level_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-sutuntop1),sutuntop1)
    test_level_upp = Rbeta.inv((1 + CI/100)/2, (sutuntop1+1), (toplam-sutuntop1))
    ## Pre-test odds
    pretest_odds = prevalence / (1 - prevalence)
    ## Pozitif Olabilirlik Orani
    likelihoodratio_positivetest = sensitivity_estimate/(1-specificity)
    likelihoodratio_positivetest_se = exp(sqrt((1-sensitivity_estimate)/(sensitivity_estimate*satirtop1) +specificity/((1-specificity)*satirtop2)))
    likelihoodratio_positivetest_low = exp(log(likelihoodratio_positivetest) + qnorm((1 - CI/100)/2)*log(likelihoodratio_positivetest_se))
    likelihoodratio_positivetest_upp = exp(log(likelihoodratio_positivetest) - qnorm((1 - CI/100)/2)*log(likelihoodratio_positivetest_se))
    ## post-test odds
    posttest_odds = pretest_odds * likelihoodratio_positivetest
    bayes_ppv = (sensitivity_estimate * prevalence)/((sensitivity_estimate * prevalence) + (1 - specificity) * (1 - prevalence))
    ## Post-test probability (positive test result)
    posttest_probability = (pretest_odds * likelihoodratio_positivetest) / (1 + (pretest_odds * likelihoodratio_positivetest))
    ## Negatif Olabilirlik Orani
    likelihoodratio_negativetest = (1-sensitivity_estimate)/specificity
    likelihoodratio_negativetest_se = exp(sqrt(sensitivity_estimate/((1-sensitivity_estimate)*satirtop1) +(1-specificity)/(specificity*satirtop2)))
    likelihoodratio_negativetest_low = exp(log(likelihoodratio_negativetest) + qnorm((1-CI/100)/2)*log(likelihoodratio_negativetest_se))
    likelihoodratio_negativetest_upp = exp(log(likelihoodratio_negativetest) - qnorm((1-CI/100)/2)*log(likelihoodratio_negativetest_se))
    bayes_npv = (sensitivity_estimate * (1 - prevalence))/((sensitivity_estimate * (1 - prevalence)) + (1 - specificity) * prevalence)
    ## Post-test probability (negative test result)
    posttest_probability_neg = (pretest_odds * likelihoodratio_negativetest) / (1 + (pretest_odds * likelihoodratio_negativetest))
    ## Ters negatif olabilirlik orani
    inverse_likelihoodratio_negativetest = specificity/(1-sensitivity_estimate)
    inverse_likelihoodratio_negativetest_se = exp(sqrt(sensitivity_estimate/((1-sensitivity_estimate)*satirtop1) +(1-specificity)/(specificity*satirtop2)))
    inverse_likelihoodratio_negativetest_low = exp(log(inverse_likelihoodratio_negativetest) + qnorm((1-CI/100)/2)*log(inverse_likelihoodratio_negativetest_se))
    inverse_likelihoodratio_negativetest_upp = exp(log(inverse_likelihoodratio_negativetest) - qnorm((1-CI/100)/2)*log(inverse_likelihoodratio_negativetest_se))
    ## Diagnostic Odds Ratio (Tani Odds Orani)
    odds_ratio = (TP/FN)/(FP/TN)
    odds_ratio_se = exp(sqrt(1/TP+1/FP+1/TN+1/FN))
    odds_ratio_low = exp(log(odds_ratio) + qnorm((1-CI/100)/2)*log(odds_ratio_se))
    odds_ratio_upp = exp(log(odds_ratio) - qnorm((1-CI/100)/2)*log(odds_ratio_se))
    odds_ratio_Haldane = ((TP+0.5)/(FN+0.5))/((FP+0.5)/(TN+0.5))
    odds_ratio_Haldane_se = exp(sqrt(1/(TP+0.5)+1/(FP+0.5)+1/(TN+0.5)+1/(FN+0.5)))
    odds_ratio_Haldane_low = exp(log(odds_ratio_Haldane) + qnorm((1-CI/100)/2)*log(odds_ratio_Haldane_se))
    odds_ratio_Haldane_upp = exp(log(odds_ratio_Haldane) - qnorm((1-CI/100)/2)*log(odds_ratio_Haldane_se))
    error_odds_ratio = (sensitivity_estimate/(1-sensitivity_estimate))/(specificity/(1-specificity))
    ## Rate ratio
    ## http://omarkasule-05.tripod.com/id52.html
    ## http://sphweb.bumc.bu.edu/otlt/mph-modules/ep/ep713_randomerror/ep713_randomerror4.html
    rate_ratio = (TP/sutuntop1)/(TP/sutuntop2)
    rate_ratio_low = exp(log(relative_risk)+qnorm((1-CI/100)/2)*sqrt(1/TP+1/FP))
    rate_ratio_upp = exp(log(relative_risk)-qnorm((1-CI/100)/2)*sqrt(1/TP+1/FP))
    ## Risk Difference
    ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
    risk_difference = (TP/sutuntop1) - (FN/sutuntop2)
    risk_difference_se = (((TP/sutuntop1)*(1-(TP/sutuntop1)))/sutuntop1)+(((FN/sutuntop2)*(1-(FN/sutuntop2)))/sutuntop2)
    risk_difference_low = risk_difference + qnorm((1-CI/100)/2)*sqrt(risk_difference_se)
    risk_difference_upp = risk_difference - qnorm((1-CI/100)/2)*sqrt(risk_difference_se)
    ## Nitelenebilen risk
    ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
    attributable_risk = (TP/sutuntop1)-(FN/sutuntop2)
    attributable_risk_se = sqrt((satirtop1/toplam)*(1-(satirtop1/toplam))*(1/sutuntop1+1/sutuntop2))
    attributable_risk_low = attributable_risk + qnorm((1-CI/100)/2)*attributable_risk_se
    attributable_risk_upp = attributable_risk - qnorm((1-CI/100)/2)*attributable_risk_se
    ## Attributable risk percent
    ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
    attributable_risk_percent = ((relative_risk-1)/relative_risk)*100
    attributable_risk_percent_low = attributable_risk_percent - (qnorm((1-CI/100)/2)*attributable_risk_se)/attributable_risk
    attributable_risk_percent_upp = attributable_risk_percent + (qnorm((1-CI/100)/2)*attributable_risk_se)/attributable_risk
    ## Population Attributable Risk
    ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
    population_attributable_risk = (satirtop1/toplam)-(FN/sutuntop2)
    ## Population Attributable Risk Percent
    population_attributable_risk_percent = (population_attributable_risk / (satirtop1/toplam))*100
    ## Kohen's Kappa
    cohens_kappa = (efficiency-efficiency_randomtest)/(1-efficiency_randomtest)
    cohens_kappa_se = quality_index_se
    cohens_kappa_low = cohens_kappa + qnorm((1-CI/100)/2)*cohens_kappa_se
    cohens_kappa_upp = cohens_kappa - qnorm((1-CI/100)/2)*cohens_kappa_se
    ## Gozlenen Karar
    observed_agreement = efficiency
    observed_agreement_se = efficiency_se
    observed_agreement_low = efficiency_low
    observed_agreement_upp = efficiency_upp
    ## Risk karari,expected agreement
    chance_agreement = efficiency_randomtest
    ## Pozitif karar
    positive_agreement = 2*TP /(satirtop1+sutuntop1)
    positive_agreement_se = sqrt(4*TP*(FP+FN)*(TP+FP+FN))/(sutuntop1+satirtop1)^2
    positive_agreement_low = positive_agreement + qnorm((1-CI/100)/2)*positive_agreement_se
    positive_agreement_upp = positive_agreement - qnorm((1-CI/100)/2)*positive_agreement_se
    ## Negatif karar
    negative_agreement = 2*TN/(satirtop2+sutuntop2)
    negative_agreement_se = sqrt(4*TN*(FP+FN)*(TN+FP+FN))/(sutuntop2+satirtop2)^2
    negative_agreement_low = negative_agreement + qnorm((1-CI/100)/2)*negative_agreement_se
    negative_agreement_upp = negative_agreement - qnorm((1-CI/100)/2)*negative_agreement_se
    ## Rand index
    ## http://www.cs.pomona.edu/classes/cs158/resources/158-10(EM).pdf
    rand_index = (TP+TN)/(TP+FN+FP+TN)
    ## e-measure
    ## http://michael.hahsler.net/research/recomm_lnai2002/lnai2002.pdf
    e_measure = 1/((((1-CI)/100)*(1-precision))+((1-((1-CI)/100))*(1/recall)))
    ## discriminant power 
    ## https://eva.fing.edu.uy/pluginfile.php/69453/mod_resource/content/1/7633-10048-1-PB.pdf
    discriminant_power = (sqrt(3)/pi)*(log (sensitivity_estimate / (1-sensitivity_estimate)) + log (specificity / (1-specificity)))
    ## F1 Score
    ## http://machinelearningmastery.com/classification-accuracy-is-not-enough-more-performance-measures-you-can-use/
    f1_score = 2*(((TP/(TP+FN))*(TP/(TP+FP)))/((TP/(TP+FN))+(TP/(TP+FP))))
    ## Byrt yanlilik indeksi
    byrt_bias_index = (FN-FP)/toplam
    ## Byrt  asimetrik indeks prevelansi
    byrt_prevalence_asymmetry_index = (TN-TP)/toplam
    ## Yanlilik duzeltmeli kappa
    bias_adjusted_kappa = (efficiency-0.25*((satirtop1+sutuntop1)^2+(satirtop2+sutuntop2)^2)/toplam^2)/(1-0.25*((satirtop1+sutuntop1)^2+(satirtop2+sutuntop2)^2)/toplam^2)
    ## Yanlilik duzeltmeli kappa prevelansi
    prevalence_bias_adjusted_kappa = cohens_kappa+(1-cohens_kappa)*(byrt_prevalence_asymmetry_index^2-byrt_bias_index^2)
    ## Dice'in indeksi (Czekanowski)
    dice_index = positive_agreement
    dice_index_se= positive_agreement_se
    dice_index_low = positive_agreement_low
    dice_index_upp = positive_agreement_upp
    ## Yule's Q
    yules_q = (odds_ratio - 1) / (odds_ratio + 1)
    yules_q_se = 0.5*(1-yules_q^2)*sqrt(1/TP+1/FP+1/TN+1/FN)
    yules_q_low = yules_q + qnorm((1-CI/100)/2)*yules_q_se
    yules_q_upp = yules_q - qnorm((1-CI/100)/2)*yules_q_se
    
    equitable_threatscore = (TP-(sutuntop1*satirtop1/toplam))/(TP+FP+FN-(sutuntop1*satirtop1/toplam))
    ## Phi
    phi = (TP*TN-FN*FP) / sqrt(satirtop1 * satirtop2 * sutuntop1 * sutuntop2)
    phi_se = sqrt((1-phi^2+(phi+0.5*phi^3)*((satirtop1-satirtop2)*(sutuntop1-sutuntop2)/sqrt(satirtop1*satirtop2*sutuntop1*sutuntop2))-0.75*phi^2*((satirtop1-satirtop2)^2/(satirtop1*satirtop2)+(sutuntop1-sutuntop2)^2/(sutuntop1*sutuntop2)))/toplam)
    phi_low = phi + qnorm((1-CI/100)/2)*phi_se
    phi_upp = phi - qnorm((1-CI/100)/2)*phi_se
    ## Cramer V katsayisi
    cramer_v = sqrt((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2))/toplam)
    ## Olaganlik katsayisi
    contingency_coefficient = sqrt((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2))/(((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)))+toplam))
    ## Goodman and Kruskal Gamma 
    goodman_and_kruskal_gamma = (TP*TN-FN*FP)/(TP*TN+FN*FP)
    goodman_and_kruskal_gamma_se = 0.5*(1-yules_q^2)*sqrt(1/TP+1/FP+1/TN+1/FN)
    goodman_and_kruskal_gamma_low = yules_q + qnorm((1-CI/100)/2)*yules_q_se
    goodman_and_kruskal_gamma_upp = yules_q - qnorm((1-CI/100)/2)*yules_q_se
    ## Kendall's tau a
    kendalls_tau_a = (TP*TN-FN*FP)/(toplam*(toplam-1)/2)
    ## Kendall's tau b
    ## https://v8doc.sas.com/sashtml/stat/chap28/sect20.htm
    ## http://slideplayer.com/slide/10766029/
    kendalls_tau_b = (TP*TN-FN*FP)/sqrt((TP*TN+FN*FP+(TP*FN+FP*TN))*(TP*TN+FN*FP+(TP*FP+FN*TN)))
    ## Kendall's tau c
    kendalls_tau_c = ((2*2)*(TP*TN-FN*FP)) / (toplam^2 * (2-1))
    ## Somers'd R|C
    somers_d = (TP*TN-FN*FP)/(TP*TN+FN*FP+(TP*FN+FP*TN))
    ## Somers'd C|R
    somers_d_2 = (TP*TN-FP*FN)/(TP*TN+FP*FN+(TP*FP+FN*TN))
    ## Scoot'un karar indeksi
    scotts_agreement_index = bias_adjusted_kappa
    ## Dort-duzeyli Korelasyon
    tetrachoric_correlation = cos(pi/(1+sqrt(TP*TN/FN/FP)))
    tetrachoric_correlation_se = 0.375*(1-tetrachoric_correlation^2)*sqrt((1/(TP))+(1/(FP))+(1/(TN))+(1/(FN)))
    tetrachoric_correlation_low = tetrachoric_correlation + qnorm((1-CI/100)/2)*tetrachoric_correlation_se
    tetrachoric_correlation_upp = tetrachoric_correlation - qnorm((1-CI/100)/2)*tetrachoric_correlation_se
    ## Goodman kruskal tau katsayisi
    goodman_kruskal_tau = (toplam*((TP^2+FP^2)/sutuntop1+(FN^2+TN^2)/sutuntop2)-satirtop1^2-satirtop2^2)/(toplam^2-satirtop1^2-satirtop2^2)
    goodman_kruskal_tau_se = toplam*sqrt(TP*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop1)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*TP/satirtop1-(TP^2+FN^2)/satirtop1^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+FN*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop2)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*FN/satirtop1-(TP^2+FN^2)/satirtop1^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+FP*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop1)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*FP/satirtop2-(FP^2+TN^2)/satirtop2^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+TN*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop2)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*TN/satirtop2-(FP^2+TN^2)/satirtop2^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2)/(toplam^2-sutuntop1^2-sutuntop2^2)^2
    goodman_kruskal_tau_low = goodman_kruskal_tau + qnorm((1-CI/100)/2)*goodman_kruskal_tau_se
    goodman_kruskal_tau_upp = goodman_kruskal_tau - qnorm((1-CI/100)/2)*goodman_kruskal_tau_se
    ## Simetrik Lambda 
    lambda_symmetric = ((max(TP,FN)+max(FP,TN)+max(TP,FP)+max(FN,TN)-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2))/(2*toplam-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2)))
    if (lambda_symmetric==0 || lambda_symmetric==1)
    {lambda_symmetric_se = 0}
    else
    {
      lambda_symmetric_se = sqrt((TP*((if(TP>FN) 1 else 0) + (if(TP>FP) 1 else 0) + ((if(sutuntop1>sutuntop2) 1 else 0)+ (if(satirtop1>satirtop2) 1 else 0))*(lambda_symmetric-1))^2+FN*( (if(TP>FN) 0 else 1)+ (if(FN>TN) 1 else 0) +( (if(sutuntop1>sutuntop2) 0 else 1) + (if(satirtop1>satirtop2) 1 else 0))*(lambda_symmetric-1))^2+FP*( (if(FP>TN) 1 else 0) + (if(TP>FP) 0 else 1) +( (if(sutuntop1>sutuntop2) 1 else 0) + (if(satirtop1<satirtop2) 1 else 0))*(lambda_symmetric-1))^2+TN*((if(FP>TN) 0 else 1)+ (if(FN>TN) 0 else 1) +((if(sutuntop1>sutuntop2) 0 else 1)+ (if(satirtop1<satirtop2) 1 else 0))*(lambda_symmetric-1))^2-4*toplam*lambda_symmetric^2)/(2*toplam-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2))^2)
    }
    
    lambda_symmetric_low = lambda_symmetric + qnorm((1-CI/100)/2)*lambda_symmetric_se
    lambda_symmetric_upp = lambda_symmetric - qnorm((1-CI/100)/2)*lambda_symmetric_se
    ## Lambda Asymmetric R|C
    lambda_criterion = (max(TP,FP)+max(FN,TN)-max(satirtop1,satirtop2))/(toplam-max(satirtop1,satirtop2))
    
    if (lambda_criterion==0 || lambda_criterion==1)
    {lambda_criterion_se = 0}
    else
    {
      lambda_criterion_se = sqrt((toplam-max(TP,FP)-max(FN,TN))*(max(TP,FP)+max(FN,TN)+max(satirtop1,satirtop2)-2*(if(satirtop1>satirtop2) max(TP,FN) else max(FP,TN)))/(toplam-max(satirtop1,satirtop2))^3)
    }
    
    lambda_criterion_low = lambda_criterion + qnorm((1-CI/100)/2)*lambda_criterion_se
    lambda_criterion_upp = lambda_criterion - qnorm((1-CI/100)/2)*lambda_criterion_se
    ## Lambda Asymmetric C|R
    lambda_criterion_2 = (max(TP,FN)+max(FP,TN)-max(sutuntop1,sutuntop2))/(toplam-max(sutuntop1,sutuntop2))
    
    if (lambda_criterion_2==0 || lambda_criterion_2==1)
    {lambda_criterion_se_2 = 0}
    else
    {
      lambda_criterion_se_2 = sqrt((toplam-max(TP,FN)-max(FP,TN))*(max(TP,FN)+max(FP,TN)+max(sutuntop1,sutuntop2)-2*(if(sutuntop1>sutuntop2) max(TP,FP) else max(FN,TN)))/(toplam-max(sutuntop1,sutuntop2))^3)
    }
    
    lambda_criterion_low_2 = lambda_criterion_2 + qnorm((1-CI/100)/2)*lambda_criterion_se_2
    lambda_criterion_upp_2 = lambda_criterion_2 - qnorm((1-CI/100)/2)*lambda_criterion_se_2
    ## belirsizlik katsayisi (coefficient of uncertainty) simetrik
    uncertainty_coefficient_symmetric = 2*(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam)))/(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))
    uncertainty_coefficient_symmetric_se = 2*sqrt(TP*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop1*sutuntop1/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(TP/toplam))^2+FN*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop1*sutuntop2/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(FN/toplam))^2+FP*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop2*sutuntop1/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(FP/toplam))^2+TN*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop2*sutuntop2/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(TN/toplam))^2)/toplam/(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam)^2
    uncertainty_coefficient_symmetric_low = uncertainty_coefficient_symmetric + qnorm((1-CI/100)/2)*uncertainty_coefficient_symmetric_se
    uncertainty_coefficient_symmetric_upp = uncertainty_coefficient_symmetric - qnorm((1-CI/100)/2)*uncertainty_coefficient_symmetric_se
    ## (coefficient of uncertainty) R|C
    uncertainty_coeff_crit = (satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam)))/(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))
    uncertainty_coeff_crit_se = sqrt(TP*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(TP/sutuntop1)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop1/toplam))^2+FN*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(FN/sutuntop2)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop1/toplam))^2+FP*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(FP/sutuntop1)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop2/toplam))^2+TN*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(TN/sutuntop2)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop2/toplam))^2)/toplam/((satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam)^2
    uncertainty_coeff_crit_low = uncertainty_coeff_crit + qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se
    uncertainty_coeff_crit_upp = uncertainty_coeff_crit - qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se
    ## (coefficient of uncertainty) C|R
    uncertainty_coeff_crit_2 = (sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)+satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)-(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam)))/(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))
    uncertainty_coeff_crit_se_2 = sqrt(TP*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(TP/satirtop1)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop1/toplam))^2+FP*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(FP/satirtop2)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop1/toplam))^2+FN*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(FN/satirtop1)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop2/toplam))^2+TN*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(TN/satirtop2)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop2/toplam))^2)/toplam/((sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)^2
    uncertainty_coeff_crit_low_2 = uncertainty_coeff_crit_2 + qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se_2
    uncertainty_coeff_crit_upp_2 = uncertainty_coeff_crit_2 - qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se_2
    ## Yule s Y (Coefficient of colligation)
    yule_y = (sqrt(TP*TN)-sqrt(FN*FP))/(sqrt(TP*TN)+sqrt(FN*FP))
    ## Pearson ki-kare
    pearson_chi_squ = toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)
    pearson_chi_squ_p = dchi(pearson_chi_squ, df=1)
    ## 
    with_yate_cor_for_pearson = toplam*(abs(TP*TN-FN*FP)-0.5*toplam)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)
    with_yate_cor_p_for_pearson = dchi(with_yate_cor_for_pearson, df=1)
    ## Mantel Haenszel chi-square
    ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
    mantel_haenszel = ((toplam-1)*(TP*TN-FN*FP)^2)/(satirtop1*satirtop2*sutuntop1*sutuntop2)
    mantel_haenszel_p = dchi(mantel_haenszel, df=1)
    ## Olasilik orani
    likelih_ratio = 2*(TP*log(TP/(sutuntop1*satirtop1/toplam))+FN*log(FN/(sutuntop2*satirtop1/toplam))+FP*log(FP/(sutuntop1*satirtop2/toplam))+TN*log(TN/(sutuntop2*satirtop2/toplam)))
    likelih_ratio_p =dchi(likelih_ratio, df=1)
    ## Fisher'in exact testi
    ##fisher_exact_test = (TP-(satirtop1*sutuntop1/toplam))^2/(satirtop1*sutuntop1/toplam) + (FN-(satirtop1*sutuntop2/toplam))^2/(satirtop1*sutuntop2/toplam) + (FP-(satirtop2*sutuntop1/toplam))^2/(satirtop2*sutuntop1/toplam) + (TN-(satirtop2*sutuntop2/toplam))^2/(satirtop2*sutuntop2/toplam)
    fisher <- c(TP, FN, FP, TN)
    tab <- t(matrix(fisher, nrow=2,ncol=2))
    fisher_exact_test = fisher.test(tab)$p.value
    minimum_exp_fre = min((sutuntop1*satirtop1/toplam):(sutuntop2*satirtop2/toplam))
    cells_exp_fre_5 = (if((sutuntop1*satirtop1/toplam)<5) 1 else 0)+ (if((sutuntop1*satirtop2/toplam)<5) 1 else 0)+ (if((sutuntop2*satirtop1/toplam) <5) 1 else 0)+ (if((sutuntop2*satirtop2/toplam) <5) 1 else 0)
    cells_exp_fre_1 = (if((sutuntop1*satirtop1/toplam) <1) 1 else 0)+ (if((sutuntop1*satirtop2/toplam) <1) 1 else 0)+ (if((sutuntop2*satirtop1/toplam) <1) 1 else 0)+ (if((sutuntop2*satirtop2/toplam) <1) 1 else 0)
    ## Mc Nemar testi
    mcNemar_test = (FP-FN)^2/(FP+FN)
    mcNemar_test_p = dchi(mcNemar_test, df=1)
    with_Yate_cor_for_mcnamer = (abs(FP-FN)-1)^2/(FP+FN)
    with_Yate_cor_p_for_mcnamer = dchi(with_Yate_cor_for_mcnamer, df=1)
    
    ## Belirsizlik (Entropi)
    forbes_NMI = sensitivity_estimate*predvalue_positiverandomtest*log((sensitivity_estimate/predvalue_positiverandomtest_mar_ol),base=2)+
      (1-specificity)*(1-predvalue_positiverandomtest)*log(((1-specificity)/predvalue_positiverandomtest_mar_ol),base=2)+
      (1-sensitivity_estimate)*predvalue_positiverandomtest*log(((1-sensitivity_estimate)/(1-predvalue_positiverandomtest_mar_ol)),base=2)+
      specificity*(1-predvalue_positiverandomtest)*log((specificity/(1-predvalue_positiverandomtest_mar_ol)),base=2)
    ## satir icin entropy (test icin)
    entropy_hr = - ((sutuntop1/toplam) * log(sutuntop1/toplam, base=2) + (sutuntop2/toplam) * log(sutuntop2/toplam, base=2))
    ## sutun icin entropy (hastalik icin)  
    entropy_hc = - ((satirtop1/toplam)*log(satirtop1/toplam, base=2) + (satirtop2/toplam)*log(satirtop2/toplam, base=2))
    ## birlesik entropi (joint entropy)  
    entropy_hrc = - ( (TP/toplam)*log(TP/toplam, base=2) + (FP/toplam)*log(FP/toplam, base=2) + (FN/toplam)*log(FN/toplam, base=2) + (TN/toplam)*log(TN/toplam, base=2))
    ## bilgi icerigi (mutual information)  
    information_r_c = entropy_hr + entropy_hc - entropy_hrc
    ## kosullu entropi (conditional entropy)  
    a = entropy_hrc - entropy_hr
    c = entropy_hrc - entropy_hc
    sim_r_c = information_r_c / (a + information_r_c + c)
    dis_r_c = (a + c) / (a + information_r_c + c)
    ## goreli entropi (relative entropy, kullback-leibler uzakligi)
    ## pozitif test sonucu icin goreli entropi, Relative Improvement Over Chance (RIOC)
    positive_relative_entropy = (TP/sutuntop1)*log(((TP/sutuntop1)/ prevalence), base=2)+(FP/sutuntop1)*log(((FP/sutuntop1)/(1- prevalence)), base=2)
    ## negatif test sonucu icin goreli entropi 
    negative_relative_entropy = (FN/sutuntop2)*log(((FN/sutuntop2)/ prevalence), base=2)+(TN/sutuntop2)*log(((TN/sutuntop2)/(1- prevalence)), base=2)
    
    verim <- data.frame(#Estimate=integer(),
                        #LowerCI=integer(),
                        #UpperCI=integer()
      )
    row <- vector(mode="character")
    if (sensitivity_estimate >= 0 && sensitivity_estimate <= 1) 
    {
      verim <- data.frame(Estimate = round(sensitivity_estimate, digit =3),  LowerCI = round(sensitivity_low, digit =3), UpperCI = round(sensitivity_upp, digit =3))
      row <- c("Sensitivity")
    } 
    if (sensitivity_randomtest >= 0 && sensitivity_randomtest <= 1)
    {
      newRow <- data.frame(Estimate = round(sensitivity_randomtest, digit =3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Sensitivity of a random test")
    }
    if (qualityindex_sensitivity >= 0 && qualityindex_sensitivity <= 1)
    {
      newRow <- data.frame(Estimate = round(qualityindex_sensitivity, digit =3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Quality index of sensitivity")
    }
    if (specificity >= 0 && specificity <= 1)
    {
      newRow <- data.frame(Estimate = round(specificity, digit=3), LowerCI = round(specificity_low, digit =3), UpperCI = round(specificity_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Specificity")
    }
    if (specificity_randomtest >= 0 && specificity_randomtest <= 1)
    {
      newRow <- data.frame(Estimate = round(specificity_randomtest, digit =3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Specificity of a random test")
    }
    if (qualityindex_specificity >= 0 && qualityindex_specificity <= 1)
    {
      newRow <- data.frame(Estimate = round(qualityindex_specificity, digit =3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Quality index of specificity")
    }
    if (efficiency >= 0 && efficiency <= 1)
    {
      newRow <- data.frame(Estimate = round(efficiency, digit =3),  LowerCI = round(efficiency_low, digit =3), UpperCI = round(efficiency_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Efficiency (Correct classification rate)")
    }
    if (efficiency_randomtest >= 0 && efficiency_randomtest <= 1)
    {
      newRow <- data.frame(Estimate = round(efficiency_randomtest, digit =3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Efficiency of a random test")
    }
    #### #mis_eff <- data.frame(Symbol = 'M_EFF', Estimate = round(mis_efficiency, digit = 3), se = 0, LowerCI = 0, UpperCI = 0)
    
    if (quality_index >= 0 && quality_index <= 1)
    {
      newRow <- data.frame(Estimate = round(quality_index, digit=3), LowerCI = round(quality_index_low, digit =3), UpperCI = round(quality_index_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Quality index")
    }
    if (youdens_index >= 0 && youdens_index <= 1)
    {
      newRow<- data.frame(Estimate = round(youdens_index, digit =3), LowerCI = round(youdens_index_low, digit =3), UpperCI = round(youdens_index_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Youden\'s index")
    }
    if (number_needed_to_diagnose >= 0 && number_needed_to_diagnose <= 1)
    {
      newRow <- data.frame(Estimate = round(number_needed_to_diagnose, digit =3), LowerCI = 0, UpperCI = 0)
      verim <- rbind(verim,newRow)
      row <- append(row, "Number Needed to Diagnose")
    }
    if (number_needed_to_diagnose >= 0 && number_needed_to_diagnose <= 1)
    {
      newRow <- data.frame(Estimate = round(number_needed_to_diagnose, digit =3), LowerCI = 0, UpperCI = 0)
      verim <- rbind(verim,newRow)
      row <- append(row, "Number Needed to Diagnose")
    }
    if (number_needed_to_misdiagnose >= 0 && number_needed_to_misdiagnose <= 1)
    {
      newRow <- data.frame(Estimate = round(number_needed_to_misdiagnose, digit =3), LowerCI = 0, UpperCI = 0)
      verim <- rbind(verim,newRow)
      row <- append(row, "Number Needed to Misdiagnose")
    }
    if (predictivevalue_positivetest >= 0 && predictivevalue_positivetest <= 1)
    {
      newRow <- data.frame(Estimate = round(predictivevalue_positivetest, digit =3), LowerCI = round(predictivevalue_positivetest_low, digit =3), UpperCI = round(predictivevalue_positivetest_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Predictive value of positive test")
    }
    if (predvalue_positiverandomtest >= 0 && predvalue_positiverandomtest <= 1)
    {
      newRow<- data.frame(Estimate = round(predvalue_positiverandomtest, digit =3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Pred. value of a positive random test")
    }
    if (predictivevalue_negativetest >= 0 && predictivevalue_negativetest <= 1)
    {
      newRow<- data.frame(Estimate = round(predictivevalue_negativetest, digit =3), LowerCI = format(round(predictivevalue_negativetest_low, digit =3),nsmall=3), UpperCI = round(predictivevalue_negativetest_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Predictive value of negative test")
    }
    if (predvalue_negativerandomtest >= 0 && predvalue_negativerandomtest <= 1)
    {
      newRow <- data.frame(Estimate = round(predvalue_negativerandomtest, digit =3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Pred. value of a negative random test")
    }
    if (false_positiverate >= 0 && false_positiverate <= 1)
    {
      newRow <- data.frame(Estimate = round(false_positiverate, digit =3), LowerCI = round(false_positiverate_low, digit =3), UpperCI = round(false_positiverate_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "False positive rate")
    }
    if (false_negativerate >= 0 && false_negativerate <= 1)
    {
      newRow  <- data.frame(Estimate = round(false_negativerate, digit =3),  LowerCI = round(false_negativerate_low, digit =3), UpperCI = round(false_negativerate_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "False negative rate")
    }
    if (true_positiverate >= 0 && true_positiverate <= 1)
    {
      newRow  <- data.frame(Estimate = round(true_positiverate, digit =3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "True positive rate")
    }
    if (misclassification_rate >= 0 && misclassification_rate <= 1)
    {
      newRow <- data.frame(Estimate = round(misclassification_rate, digit =3),  LowerCI = round(misclassification_rate_low, digit =3), UpperCI = round(misclassification_rate_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Misclassification rate")
    }
    if (rand_index >= 0 && rand_index <= 1)
    {
      newRow <- data.frame(Estimate = round(rand_index, digit =3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Rand Index (Rand Measure)")
    }
    if (balanced_accuracy >= 0 && balanced_accuracy <= 1)
    {
      newRow <- data.frame(Estimate = round(balanced_accuracy, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Balanced Accuracy (Average Accuracy)")
    }
    if (lift >= 0 && lift <= 1)
    {
      newRow <- data.frame(Estimate = round(lift, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Lift (Interest)")
    }
    if (precision >= 0 && precision <= 1)
    {
      newRow <- data.frame(Estimate = round(precision, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Precision")
    }
    if (recall >= 0 && recall <= 1)
    {
      newRow <- data.frame(Estimate = round(recall, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Recall")
    }
    if (discriminant_power >= 0 && discriminant_power <= 1)
    {
      newRow <- data.frame(Estimate = round(discriminant_power, digit =3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Discriminant Power")
    }
    if (f1_score >= 0 && f1_score <= 1)
    {
      newRow <- data.frame(Estimate = round(f1_score, digit =3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "F1 Score (F Measure)")
    }
    if (matthews_correlation_coefficient >= 0 && matthews_correlation_coefficient <= 1)
    {
      newRow <- data.frame(Estimate = round(matthews_correlation_coefficient, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Matthew\'s Correlation Coefficient")
    }
    if (entropy_hr >= 0 && entropy_hr <= 1)
    {
      newRow <- data.frame(Estimate = round(entropy_hr, digit =3),  LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      row <- append(row, "Entropy for true")
    }
    if (entropy_hc >= 0 && entropy_hc <= 1)
    {
      newRow <- data.frame(Estimate = round(entropy_hc, digit =3),  LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      row <- append(row, "Entropy for False")
    }
    if (entropy_hrc >= 0 && entropy_hrc <= 1)
    {
      newRow <- data.frame(Estimate = round(entropy_hrc, digit =3),  LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      row <- append(row, "Joint Entropy")
    }
    if (information_r_c >= 0 && information_r_c <= 1)
    {
      newRow <- data.frame(Estimate = round(information_r_c, digit =3),  LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      row <- append(row, "Mutual Information")
    }
    if (a >= 0 && a <= 1)
    {
      newRow <- data.frame(Estimate = round(a, digit =3),  LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      row <- append(row, "Joint Entropy - Entropy for true")
    }
    if (c >= 0 && c <= 1)
    {
      newRow <- data.frame(Estimate = round(c, digit =3),  LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      row <- append(row, "Joint Entropy - Entropy for false")
    }
    if (sim_r_c >= 0 && sim_r_c <= 1)
    {
      newRow <- data.frame(Estimate = round(sim_r_c, digit =3),  LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      row <- append(row, "Similarity of Descriptors True and False")
    }
    if (dis_r_c >= 0 && dis_r_c <= 1)
    {
      newRow <- data.frame(Estimate = round(dis_r_c, digit =3),  LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      row <- append(row, "Distance Between True and False")
    }
    if (positive_relative_entropy >= 0 && positive_relative_entropy <= 1)
    {
      newRow <- data.frame(Estimate = round(positive_relative_entropy, digit =3),  LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      row <- append(row, "Relative Improvement Over Chance (RIOC)")
    }
    if (negative_relative_entropy >= 0 && negative_relative_entropy <= 1)
    {
      newRow <- data.frame(Estimate = round(negative_relative_entropy, digit =3),  LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      row <- append(row, "Relative Entropy for Negative Test")
    }
    
    rownames(verim) = row
    
    highchart() %>% hc_exporting(enabled = TRUE, filename = "Plot0") %>% 
      hc_add_series(name = "Conf. Interval", type = "line", data = verim$`Estimate`) %>%
      hc_add_series(name = "CI", data = as.matrix(cbind(verim$`LowerCI`, verim$`UpperCI`)),type = "errorbar", names = "Limits", showInLegend = FALSE, zIndex = 0, lineWidth = 1.5, linkedTo = "CI"
      ) %>%
      hc_xAxis(categories = rownames(verim),title = "Diagnostic Measures") %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, headerFormat = "<b>Diagnostic Measures: </b>{point.x} <br>") %>%
      hc_plotOptions(line = list(tooltip = list(pointFormat = "<b>{series.name}: </b>{point.y:.3f} ")), 
                     errorbar = list(tooltip = list(pointFormat = "({point.low} - {point.high})"))) %>%
      hc_add_theme(hc_theme_google())

    
   
  })

  output$plot01 <- highcharter::renderHighchart({
    if (input$verigir_kontrol == TRUE){ 
      TP = 0
      FN = 0
      FP = 0
      TN = 0
      for (i in 1:nrow(tabloveri()))
      {
        if (tabloveri()[i,1]==1 & tabloveri()[i,2]==1)
        {TP = TP+1}
        else if (tabloveri()[i,1]==1 & tabloveri()[i,2]==2)
        {FN = FN+1}
        else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==1)
        {FP = FP+1}
        else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==2)
        {TN = TN+1}
        next
      }
      CI = as.numeric(trimws(input$text5_2))
    }
    else{
      TP = as.numeric(trimws(input$text1)) ### x
      FN = as.numeric(trimws(input$text2)) ### y
      FP = as.numeric(trimws(input$text3)) ### z
      TN = as.numeric(trimws(input$text4)) ### t
      CI = as.numeric(trimws(input$text5))
    }
    
    
    satirtop1 = TP + FN
    satirtop2 = FP + TN
    sutuntop1 = TP + FP
    sutuntop2 = FN + TN
    toplam = satirtop1 + satirtop2
    #table1 <- matrix(c(TP,FN,satirtop1,FP,TN,satirtop2,sutuntop1,sutuntop2,toplam),nrow=3,ncol=3,byrow=TRUE,dimnames=list(c("1.satir","2.satir","Sutun toplamlari"),c("1.kolon","2.kolon","Satir toplamlari")))
    #table1
    ## Duyarlilik 
    sensitivity_estimate = TP / satirtop1
    sensitivity_se = round((sqrt(sensitivity_estimate * (1 - sensitivity_estimate)/satirtop1)), 4)
    sensitivity_low = 1 - Rbeta.inv(((1 + CI/100)/2), (satirtop1+1-TP), TP)
    sensitivity_upp = Rbeta.inv(((1 + CI/100)/2), (TP+1), (satirtop1-TP))
    sensitivity_randomtest = sutuntop1 / toplam
    qualityindex_sensitivity = (sensitivity_estimate - sensitivity_randomtest) / (1 - sensitivity_randomtest)
    ## Secicilik
    specificity = TN / satirtop2
    specificity_se = sqrt(specificity * (1 - specificity) / satirtop2)
    specificity_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop2+1-TN),TN)
    specificity_upp = Rbeta.inv((1 + CI/100)/2, (TN+1), (satirtop2-TN))
    specificity_randomtest = (1 - sensitivity_randomtest)
    qualityindex_specificity = (specificity - 1 + sensitivity_randomtest) / sensitivity_randomtest
    ## Gain in Certainty
    gain_in_certainty = sensitivity_estimate + specificity
    ## Etkililik, yeterlilik ayni zamanda overall accuracy (toplam-genel dogruluk orani)
    efficiency = (TP + TN) / toplam
    efficiency_se = sqrt(efficiency * (1 - efficiency) / toplam)
    efficiency_low = 1-Rbeta.inv((1 + CI/100)/2, (toplam+1-TP-TN), (TP+TN))
    efficiency_upp = Rbeta.inv((1 + CI/100)/2, (TP+TN+1), (toplam-TP-TN))
    efficiency_randomtest = (satirtop1 / toplam) * sensitivity_randomtest + (1 - (satirtop1 / toplam)) * (1 - sensitivity_randomtest)
    mis_efficiency = 1 - efficiency
    ## Quality indeks
    quality_index = (efficiency - efficiency_randomtest) / (1 - efficiency_randomtest)
    quality_index_se = sqrt((efficiency*(1-efficiency)/(1-efficiency_randomtest)^2+2*(1-efficiency)*(2*efficiency*efficiency_randomtest-((TP*((satirtop1 / toplam)+sensitivity_randomtest)+TN*(2-(satirtop1 / toplam)-sensitivity_randomtest))/toplam))/(1-efficiency_randomtest)^3+((1-efficiency)^2)*((TP*((satirtop1 / toplam)+sensitivity_randomtest)^2+FN*(-(satirtop1 / toplam)+1+sensitivity_randomtest)^2+FP*(1+(satirtop1 / toplam)-sensitivity_randomtest)^2+TN*(2-(satirtop1 / toplam)-sensitivity_randomtest)^2)/toplam-4*efficiency_randomtest^2)/(1-efficiency_randomtest)^4)/toplam)
    quality_index_low = quality_index + qnorm((1 - CI/100)/2)*quality_index_se
    quality_index_upp = quality_index - qnorm((1 - CI/100)/2)*quality_index_se
    ## Youden indeksi
    youdens_index = sensitivity_estimate + specificity - 1
    youdens_index_se = sqrt(sensitivity_se^2+specificity_se^2)
    youdens_index_low = youdens_index + qnorm((1 - CI/100)/2)*youdens_index_se
    youdens_index_upp = youdens_index - qnorm((1 - CI/100)/2)*youdens_index_se
    
    number_needed_to_diagnose = 1 / youdens_index
    number_needed_to_misdiagnose = 1 / ( 1 - efficiency ) 
    ## Pozitif kestirim degeri
    predictivevalue_positivetest = TP / sutuntop1
    predictivevalue_positivetest_se = sqrt(predictivevalue_positivetest*(1-predictivevalue_positivetest)/sutuntop1)
    predictivevalue_positivetest_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop1+1-TP),TP)
    predictivevalue_positivetest_upp = Rbeta.inv((1 + CI/100)/2, (TP+1),(sutuntop1-TP))
    predvalue_positiverandomtest = (satirtop1 / toplam)
    predvalue_positiverandomtest_mar_ol = predvalue_positiverandomtest*sensitivity_estimate+(1-predvalue_positiverandomtest)*(1-specificity)
    ## Negatif kestirim degeri
    predictivevalue_negativetest = TN / sutuntop2
    predictivevalue_negativetest_se = sqrt(predictivevalue_negativetest*(1-predictivevalue_negativetest)/sutuntop2)
    predictivevalue_negativetest_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop2+1-TN),TN)
    predictivevalue_negativetest_upp = Rbeta.inv((1 + CI/100)/2, (TN+1), (sutuntop2-TN))
    predvalue_negativerandomtest = 1 - (satirtop1 / toplam)
    predvalue_negativerandomtest_mar_ol = 1 - predvalue_positiverandomtest_mar_ol
    ## Predictive Summary Index (PSI)
    predictive_summary_index = predictivevalue_positivetest + predictivevalue_negativetest - 1
    ## Yanlis pozitif orani
    false_positiverate = FP / satirtop2
    false_positiverate_se = sqrt(false_positiverate*(1-false_positiverate)/satirtop2)
    false_positiverate_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop2+1-FP),FP)
    false_positiverate_upp = Rbeta.inv((1 + CI/100)/2, (FP+1), (satirtop2-FP))
    ## Yansis negatif orani
    false_negativerate = FN / satirtop1
    false_negativerate_se = sqrt(false_negativerate*(1-false_negativerate)/satirtop1)
    false_negativerate_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop1+1-FN),FN)
    false_negativerate_upp = Rbeta.inv((1 + CI/100)/2, (FN+1), (satirtop1-FN))
    ## False Omission Rate (FOR) https://www.ncss.com/software/ncss/diagnostic-tests-in-ncss/
    false_omission_rate = FN/(sutuntop2)
    false_omission_rate_se = sqrt(false_omission_rate*(1-false_omission_rate)/sutuntop2)
    false_omission_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop2+1-FN),FN)
    false_omission_rate_upp = Rbeta.inv((1 + CI/100)/2, (FN+1), (sutuntop2-FN))
    ## False Discovery Rate (FDR) https://www.ncss.com/software/ncss/diagnostic-tests-in-ncss/
    false_discovery_rate = FP/(sutuntop1)
    false_discovery_rate_se = sqrt(false_discovery_rate*(1-false_discovery_rate)/sutuntop1)
    false_discovery_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop1+1-FP),FP)
    false_discovery_rate_upp = Rbeta.inv((1 + CI/100)/2, (FP+1), (sutuntop1-FP))
    ## Dogru pozitif orani
    true_positiverate = TP / (satirtop1)
    ## Yanlis siniflandirma orani
    misclassification_rate = (FN + FP) / toplam
    misclassification_rate_se = sqrt(misclassification_rate*(1-misclassification_rate)/toplam)
    misclassification_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-FP-FN), (FP+FN))
    misclassification_rate_upp = Rbeta.inv((1 + CI/100)/2, (FP+FN+1), (toplam-FP-FN))
    ## Prevelans
    prevalence = (satirtop1 / toplam)
    prevalence_se = sqrt((satirtop1 / toplam)*(1-(satirtop1 / toplam))/toplam)
    prevalence_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-satirtop1), satirtop1)
    prevalence_upp = Rbeta.inv((1 + CI/100)/2, (satirtop1+1), (toplam-satirtop1))
    
    ##caret paketinden olculer
    detection_rate = TP/toplam
    detection_prevalence = (TP+FP)/toplam
    balanced_accuracy = (sensitivity_estimate + specificity)/2
    lift = (TP/(TP+FN))/((TP+FP)/toplam)  ## https://gerardnico.com/wiki/data_mining/error_rate
    precision = TP/(TP+FP)
    recall = TP/(TP+FN)
    ## http://michael.hahsler.net/research/recomm_lnai2002/lnai2002.pdf
    conviction = (1-((TP+FP)/toplam))/(1-(TP/(TP+FN)))
    matthews_correlation_coefficient = (TP * TN + FN * FP) / ((TP+FP)*(FN+TN)*(FP+TN)*(TP+FN))^(1/2)
    ## Goreli Risk Orani (Relative risk)
    ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
    # https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20ratio.htm
    relative_risk = (TP/sutuntop1)/(FN/sutuntop2)
    relative_risk_se = sqrt((1/TP)+(1/FN)-(1/(TP+FP))-(1/(FN+TN)))
    relative_risk_low = exp(log(relative_risk)+qnorm((1-CI/100)/2)*relative_risk_se)
    relative_risk_upp = exp(log(relative_risk)-qnorm((1-CI/100)/2)*relative_risk_se)
    difference_in_proportion = (TP/sutuntop1) - (FN/sutuntop2)
    ## Leverage
    ## https://www3.nd.edu/~tweninge/pubs/AHBCW_ANNIE08.pdf
    leverage = (TP/sutuntop1)-((TP+FP)/toplam)
    ## http://www.ebm.med.ualberta.ca/TherapyCalc.html
    control_event_rate = FN/sutuntop2
    experimental_event_rate = TP/sutuntop1
    ## Absolute risk reduction
    absolute_risk_reduction = -difference_in_proportion
    absolute_risk_reduction_se = sqrt((TP/sutuntop1)*(1-(TP/sutuntop1))/sutuntop1+(FN/sutuntop2)*(1-(FN/sutuntop2))/sutuntop2)
    absolute_risk_reduction_low = absolute_risk_reduction - qnorm((1-CI/100)/2)*absolute_risk_reduction_se
    absolute_risk_reduction_upp = absolute_risk_reduction + qnorm((1-CI/100)/2)*absolute_risk_reduction_se
    ## Relative risk reduction
    relative_risk_reduction = absolute_risk_reduction / (FN/sutuntop2)
    relative_risk_reduction_low = 1 - exp(log(relative_risk)-qnorm((1-CI/100)/2)*relative_risk_se)
    relative_risk_reduction_upp = 1 - exp(log(relative_risk)+qnorm((1-CI/100)/2)*relative_risk_se)
    ##Number needed to treat
    number_needed_to_treat = 1 / abs(difference_in_proportion)
    number_needed_to_treat_low = 1 /absolute_risk_reduction_upp
    number_needed_to_treat_upp = 1 /absolute_risk_reduction_low
    ## Test duzeyi
    test_level = sensitivity_randomtest
    test_level_se = sqrt(test_level*(1-test_level)/toplam)
    test_level_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-sutuntop1),sutuntop1)
    test_level_upp = Rbeta.inv((1 + CI/100)/2, (sutuntop1+1), (toplam-sutuntop1))
    ## Pre-test odds
    pretest_odds = prevalence / (1 - prevalence)
    ## Pozitif Olabilirlik Orani
    likelihoodratio_positivetest = sensitivity_estimate/(1-specificity)
    likelihoodratio_positivetest_se = exp(sqrt((1-sensitivity_estimate)/(sensitivity_estimate*satirtop1) +specificity/((1-specificity)*satirtop2)))
    likelihoodratio_positivetest_low = exp(log(likelihoodratio_positivetest) + qnorm((1 - CI/100)/2)*log(likelihoodratio_positivetest_se))
    likelihoodratio_positivetest_upp = exp(log(likelihoodratio_positivetest) - qnorm((1 - CI/100)/2)*log(likelihoodratio_positivetest_se))
    ## post-test odds
    posttest_odds = pretest_odds * likelihoodratio_positivetest
    bayes_ppv = (sensitivity_estimate * prevalence)/((sensitivity_estimate * prevalence) + (1 - specificity) * (1 - prevalence))
    ## Post-test probability (positive test result)
    posttest_probability = (pretest_odds * likelihoodratio_positivetest) / (1 + (pretest_odds * likelihoodratio_positivetest))
    ## Negatif Olabilirlik Orani
    likelihoodratio_negativetest = (1-sensitivity_estimate)/specificity
    likelihoodratio_negativetest_se = exp(sqrt(sensitivity_estimate/((1-sensitivity_estimate)*satirtop1) +(1-specificity)/(specificity*satirtop2)))
    likelihoodratio_negativetest_low = exp(log(likelihoodratio_negativetest) + qnorm((1-CI/100)/2)*log(likelihoodratio_negativetest_se))
    likelihoodratio_negativetest_upp = exp(log(likelihoodratio_negativetest) - qnorm((1-CI/100)/2)*log(likelihoodratio_negativetest_se))
    bayes_npv = (sensitivity_estimate * (1 - prevalence))/((sensitivity_estimate * (1 - prevalence)) + (1 - specificity) * prevalence)
    ## Post-test probability (negative test result)
    posttest_probability_neg = (pretest_odds * likelihoodratio_negativetest) / (1 + (pretest_odds * likelihoodratio_negativetest))
    ## Ters negatif olabilirlik orani
    inverse_likelihoodratio_negativetest = specificity/(1-sensitivity_estimate)
    inverse_likelihoodratio_negativetest_se = exp(sqrt(sensitivity_estimate/((1-sensitivity_estimate)*satirtop1) +(1-specificity)/(specificity*satirtop2)))
    inverse_likelihoodratio_negativetest_low = exp(log(inverse_likelihoodratio_negativetest) + qnorm((1-CI/100)/2)*log(inverse_likelihoodratio_negativetest_se))
    inverse_likelihoodratio_negativetest_upp = exp(log(inverse_likelihoodratio_negativetest) - qnorm((1-CI/100)/2)*log(inverse_likelihoodratio_negativetest_se))
    ## Diagnostic Odds Ratio (Tani Odds Orani)
    odds_ratio = (TP/FN)/(FP/TN)
    odds_ratio_se = exp(sqrt(1/TP+1/FP+1/TN+1/FN))
    odds_ratio_low = exp(log(odds_ratio) + qnorm((1-CI/100)/2)*log(odds_ratio_se))
    odds_ratio_upp = exp(log(odds_ratio) - qnorm((1-CI/100)/2)*log(odds_ratio_se))
    odds_ratio_Haldane = ((TP+0.5)/(FN+0.5))/((FP+0.5)/(TN+0.5))
    odds_ratio_Haldane_se = exp(sqrt(1/(TP+0.5)+1/(FP+0.5)+1/(TN+0.5)+1/(FN+0.5)))
    odds_ratio_Haldane_low = exp(log(odds_ratio_Haldane) + qnorm((1-CI/100)/2)*log(odds_ratio_Haldane_se))
    odds_ratio_Haldane_upp = exp(log(odds_ratio_Haldane) - qnorm((1-CI/100)/2)*log(odds_ratio_Haldane_se))
    error_odds_ratio = (sensitivity_estimate/(1-sensitivity_estimate))/(specificity/(1-specificity))
    ## Rate ratio
    ## http://omarkasule-05.tripod.com/id52.html
    ## http://sphweb.bumc.bu.edu/otlt/mph-modules/ep/ep713_randomerror/ep713_randomerror4.html
    rate_ratio = (TP/sutuntop1)/(TP/sutuntop2)
    rate_ratio_low = exp(log(relative_risk)+qnorm((1-CI/100)/2)*sqrt(1/TP+1/FP))
    rate_ratio_upp = exp(log(relative_risk)-qnorm((1-CI/100)/2)*sqrt(1/TP+1/FP))
    ## Risk Difference
    ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
    risk_difference = (TP/sutuntop1) - (FN/sutuntop2)
    risk_difference_se = (((TP/sutuntop1)*(1-(TP/sutuntop1)))/sutuntop1)+(((FN/sutuntop2)*(1-(FN/sutuntop2)))/sutuntop2)
    risk_difference_low = risk_difference + qnorm((1-CI/100)/2)*sqrt(risk_difference_se)
    risk_difference_upp = risk_difference - qnorm((1-CI/100)/2)*sqrt(risk_difference_se)
    ## Nitelenebilen risk
    ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
    attributable_risk = (TP/sutuntop1)-(FN/sutuntop2)
    attributable_risk_se = sqrt((satirtop1/toplam)*(1-(satirtop1/toplam))*(1/sutuntop1+1/sutuntop2))
    attributable_risk_low = attributable_risk + qnorm((1-CI/100)/2)*attributable_risk_se
    attributable_risk_upp = attributable_risk - qnorm((1-CI/100)/2)*attributable_risk_se
    ## Attributable risk percent
    ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
    attributable_risk_percent = ((relative_risk-1)/relative_risk)*100
    attributable_risk_percent_low = attributable_risk_percent - (qnorm((1-CI/100)/2)*attributable_risk_se)/attributable_risk
    attributable_risk_percent_upp = attributable_risk_percent + (qnorm((1-CI/100)/2)*attributable_risk_se)/attributable_risk
    ## Population Attributable Risk
    ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
    population_attributable_risk = (satirtop1/toplam)-(FN/sutuntop2)
    ## Population Attributable Risk Percent
    population_attributable_risk_percent = (population_attributable_risk / (satirtop1/toplam))*100
    ## Kohen's Kappa
    cohens_kappa = (efficiency-efficiency_randomtest)/(1-efficiency_randomtest)
    cohens_kappa_se = quality_index_se
    cohens_kappa_low = cohens_kappa + qnorm((1-CI/100)/2)*cohens_kappa_se
    cohens_kappa_upp = cohens_kappa - qnorm((1-CI/100)/2)*cohens_kappa_se
    ## Gozlenen Karar
    observed_agreement = efficiency
    observed_agreement_se = efficiency_se
    observed_agreement_low = efficiency_low
    observed_agreement_upp = efficiency_upp
    ## Risk karari,expected agreement
    chance_agreement = efficiency_randomtest
    ## Pozitif karar
    positive_agreement = 2*TP /(satirtop1+sutuntop1)
    positive_agreement_se = sqrt(4*TP*(FP+FN)*(TP+FP+FN))/(sutuntop1+satirtop1)^2
    positive_agreement_low = positive_agreement + qnorm((1-CI/100)/2)*positive_agreement_se
    positive_agreement_upp = positive_agreement - qnorm((1-CI/100)/2)*positive_agreement_se
    ## Negatif karar
    negative_agreement = 2*TN/(satirtop2+sutuntop2)
    negative_agreement_se = sqrt(4*TN*(FP+FN)*(TN+FP+FN))/(sutuntop2+satirtop2)^2
    negative_agreement_low = negative_agreement + qnorm((1-CI/100)/2)*negative_agreement_se
    negative_agreement_upp = negative_agreement - qnorm((1-CI/100)/2)*negative_agreement_se
    ## Rand index
    ## http://www.cs.pomona.edu/classes/cs158/resources/158-10(EM).pdf
    rand_index = (TP+TN)/(TP+FN+FP+TN)
    ## e-measure
    ## http://michael.hahsler.net/research/recomm_lnai2002/lnai2002.pdf
    e_measure = 1/((((1-CI)/100)*(1-precision))+((1-((1-CI)/100))*(1/recall)))
    ## discriminant power 
    ## https://eva.fing.edu.uy/pluginfile.php/69453/mod_resource/content/1/7633-10048-1-PB.pdf
    discriminant_power = (sqrt(3)/pi)*(log (sensitivity_estimate / (1-sensitivity_estimate)) + log (specificity / (1-specificity)))
    ## F1 Score
    ## http://machinelearningmastery.com/classification-accuracy-is-not-enough-more-performance-measures-you-can-use/
    f1_score = 2*(((TP/(TP+FN))*(TP/(TP+FP)))/((TP/(TP+FN))+(TP/(TP+FP))))
    ## Byrt yanlilik indeksi
    byrt_bias_index = (FN-FP)/toplam
    ## Byrt  asimetrik indeks prevelansi
    byrt_prevalence_asymmetry_index = (TN-TP)/toplam
    ## Yanlilik duzeltmeli kappa
    bias_adjusted_kappa = (efficiency-0.25*((satirtop1+sutuntop1)^2+(satirtop2+sutuntop2)^2)/toplam^2)/(1-0.25*((satirtop1+sutuntop1)^2+(satirtop2+sutuntop2)^2)/toplam^2)
    ## Yanlilik duzeltmeli kappa prevelansi
    prevalence_bias_adjusted_kappa = cohens_kappa+(1-cohens_kappa)*(byrt_prevalence_asymmetry_index^2-byrt_bias_index^2)
    ## Dice'in indeksi (Czekanowski)
    dice_index = positive_agreement
    dice_index_se= positive_agreement_se
    dice_index_low = positive_agreement_low
    dice_index_upp = positive_agreement_upp
    ## Yule's Q
    yules_q = (odds_ratio - 1) / (odds_ratio + 1)
    yules_q_se = 0.5*(1-yules_q^2)*sqrt(1/TP+1/FP+1/TN+1/FN)
    yules_q_low = yules_q + qnorm((1-CI/100)/2)*yules_q_se
    yules_q_upp = yules_q - qnorm((1-CI/100)/2)*yules_q_se
    
    equitable_threatscore = (TP-(sutuntop1*satirtop1/toplam))/(TP+FP+FN-(sutuntop1*satirtop1/toplam))
    ## Phi
    phi = (TP*TN-FN*FP) / sqrt(satirtop1 * satirtop2 * sutuntop1 * sutuntop2)
    phi_se = sqrt((1-phi^2+(phi+0.5*phi^3)*((satirtop1-satirtop2)*(sutuntop1-sutuntop2)/sqrt(satirtop1*satirtop2*sutuntop1*sutuntop2))-0.75*phi^2*((satirtop1-satirtop2)^2/(satirtop1*satirtop2)+(sutuntop1-sutuntop2)^2/(sutuntop1*sutuntop2)))/toplam)
    phi_low = phi + qnorm((1-CI/100)/2)*phi_se
    phi_upp = phi - qnorm((1-CI/100)/2)*phi_se
    ## Cramer V katsayisi
    cramer_v = sqrt((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2))/toplam)
    ## Olaganlik katsayisi
    contingency_coefficient = sqrt((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2))/(((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)))+toplam))
    ## Goodman and Kruskal Gamma 
    goodman_and_kruskal_gamma = (TP*TN-FN*FP)/(TP*TN+FN*FP)
    goodman_and_kruskal_gamma_se = 0.5*(1-yules_q^2)*sqrt(1/TP+1/FP+1/TN+1/FN)
    goodman_and_kruskal_gamma_low = yules_q + qnorm((1-CI/100)/2)*yules_q_se
    goodman_and_kruskal_gamma_upp = yules_q - qnorm((1-CI/100)/2)*yules_q_se
    ## Kendall's tau a
    kendalls_tau_a = (TP*TN-FN*FP)/(toplam*(toplam-1)/2)
    ## Kendall's tau b
    ## https://v8doc.sas.com/sashtml/stat/chap28/sect20.htm
    ## http://slideplayer.com/slide/10766029/
    kendalls_tau_b = (TP*TN-FN*FP)/sqrt((TP*TN+FN*FP+(TP*FN+FP*TN))*(TP*TN+FN*FP+(TP*FP+FN*TN)))
    ## Kendall's tau c
    kendalls_tau_c = ((2*2)*(TP*TN-FN*FP)) / (toplam^2 * (2-1))
    ## Somers'd R|C
    somers_d = (TP*TN-FN*FP)/(TP*TN+FN*FP+(TP*FN+FP*TN))
    ## Somers'd C|R
    somers_d_2 = (TP*TN-FP*FN)/(TP*TN+FP*FN+(TP*FP+FN*TN))
    ## Scoot'un karar indeksi
    scotts_agreement_index = bias_adjusted_kappa
    ## Dort-duzeyli Korelasyon
    tetrachoric_correlation = cos(pi/(1+sqrt(TP*TN/FN/FP)))
    tetrachoric_correlation_se = 0.375*(1-tetrachoric_correlation^2)*sqrt((1/(TP))+(1/(FP))+(1/(TN))+(1/(FN)))
    tetrachoric_correlation_low = tetrachoric_correlation + qnorm((1-CI/100)/2)*tetrachoric_correlation_se
    tetrachoric_correlation_upp = tetrachoric_correlation - qnorm((1-CI/100)/2)*tetrachoric_correlation_se
    ## Goodman kruskal tau katsayisi
    goodman_kruskal_tau = (toplam*((TP^2+FP^2)/sutuntop1+(FN^2+TN^2)/sutuntop2)-satirtop1^2-satirtop2^2)/(toplam^2-satirtop1^2-satirtop2^2)
    goodman_kruskal_tau_se = toplam*sqrt(TP*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop1)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*TP/satirtop1-(TP^2+FN^2)/satirtop1^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+FN*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop2)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*FN/satirtop1-(TP^2+FN^2)/satirtop1^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+FP*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop1)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*FP/satirtop2-(FP^2+TN^2)/satirtop2^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+TN*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop2)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*TN/satirtop2-(FP^2+TN^2)/satirtop2^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2)/(toplam^2-sutuntop1^2-sutuntop2^2)^2
    goodman_kruskal_tau_low = goodman_kruskal_tau + qnorm((1-CI/100)/2)*goodman_kruskal_tau_se
    goodman_kruskal_tau_upp = goodman_kruskal_tau - qnorm((1-CI/100)/2)*goodman_kruskal_tau_se
    ## Simetrik Lambda 
    lambda_symmetric = ((max(TP,FN)+max(FP,TN)+max(TP,FP)+max(FN,TN)-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2))/(2*toplam-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2)))
    if (lambda_symmetric==0 || lambda_symmetric==1)
    {lambda_symmetric_se = 0}
    else
    {
      lambda_symmetric_se = sqrt((TP*((if(TP>FN) 1 else 0) + (if(TP>FP) 1 else 0) + ((if(sutuntop1>sutuntop2) 1 else 0)+ (if(satirtop1>satirtop2) 1 else 0))*(lambda_symmetric-1))^2+FN*( (if(TP>FN) 0 else 1)+ (if(FN>TN) 1 else 0) +( (if(sutuntop1>sutuntop2) 0 else 1) + (if(satirtop1>satirtop2) 1 else 0))*(lambda_symmetric-1))^2+FP*( (if(FP>TN) 1 else 0) + (if(TP>FP) 0 else 1) +( (if(sutuntop1>sutuntop2) 1 else 0) + (if(satirtop1<satirtop2) 1 else 0))*(lambda_symmetric-1))^2+TN*((if(FP>TN) 0 else 1)+ (if(FN>TN) 0 else 1) +((if(sutuntop1>sutuntop2) 0 else 1)+ (if(satirtop1<satirtop2) 1 else 0))*(lambda_symmetric-1))^2-4*toplam*lambda_symmetric^2)/(2*toplam-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2))^2)
    }
    
    lambda_symmetric_low = lambda_symmetric + qnorm((1-CI/100)/2)*lambda_symmetric_se
    lambda_symmetric_upp = lambda_symmetric - qnorm((1-CI/100)/2)*lambda_symmetric_se
    ## Lambda Asymmetric R|C
    lambda_criterion = (max(TP,FP)+max(FN,TN)-max(satirtop1,satirtop2))/(toplam-max(satirtop1,satirtop2))
    
    if (lambda_criterion==0 || lambda_criterion==1)
    {lambda_criterion_se = 0}
    else
    {
      lambda_criterion_se = sqrt((toplam-max(TP,FP)-max(FN,TN))*(max(TP,FP)+max(FN,TN)+max(satirtop1,satirtop2)-2*(if(satirtop1>satirtop2) max(TP,FN) else max(FP,TN)))/(toplam-max(satirtop1,satirtop2))^3)
    }
    
    lambda_criterion_low = lambda_criterion + qnorm((1-CI/100)/2)*lambda_criterion_se
    lambda_criterion_upp = lambda_criterion - qnorm((1-CI/100)/2)*lambda_criterion_se
    ## Lambda Asymmetric C|R
    lambda_criterion_2 = (max(TP,FN)+max(FP,TN)-max(sutuntop1,sutuntop2))/(toplam-max(sutuntop1,sutuntop2))
    
    if (lambda_criterion_2==0 || lambda_criterion_2==1)
    {lambda_criterion_se_2 = 0}
    else
    {
      lambda_criterion_se_2 = sqrt((toplam-max(TP,FN)-max(FP,TN))*(max(TP,FN)+max(FP,TN)+max(sutuntop1,sutuntop2)-2*(if(sutuntop1>sutuntop2) max(TP,FP) else max(FN,TN)))/(toplam-max(sutuntop1,sutuntop2))^3)
    }
    
    lambda_criterion_low_2 = lambda_criterion_2 + qnorm((1-CI/100)/2)*lambda_criterion_se_2
    lambda_criterion_upp_2 = lambda_criterion_2 - qnorm((1-CI/100)/2)*lambda_criterion_se_2
    ## belirsizlik katsayisi (coefficient of uncertainty) simetrik
    uncertainty_coefficient_symmetric = 2*(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam)))/(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))
    uncertainty_coefficient_symmetric_se = 2*sqrt(TP*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop1*sutuntop1/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(TP/toplam))^2+FN*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop1*sutuntop2/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(FN/toplam))^2+FP*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop2*sutuntop1/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(FP/toplam))^2+TN*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop2*sutuntop2/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(TN/toplam))^2)/toplam/(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam)^2
    uncertainty_coefficient_symmetric_low = uncertainty_coefficient_symmetric + qnorm((1-CI/100)/2)*uncertainty_coefficient_symmetric_se
    uncertainty_coefficient_symmetric_upp = uncertainty_coefficient_symmetric - qnorm((1-CI/100)/2)*uncertainty_coefficient_symmetric_se
    ## (coefficient of uncertainty) R|C
    uncertainty_coeff_crit = (satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam)))/(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))
    uncertainty_coeff_crit_se = sqrt(TP*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(TP/sutuntop1)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop1/toplam))^2+FN*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(FN/sutuntop2)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop1/toplam))^2+FP*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(FP/sutuntop1)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop2/toplam))^2+TN*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(TN/sutuntop2)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop2/toplam))^2)/toplam/((satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam)^2
    uncertainty_coeff_crit_low = uncertainty_coeff_crit + qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se
    uncertainty_coeff_crit_upp = uncertainty_coeff_crit - qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se
    ## (coefficient of uncertainty) C|R
    uncertainty_coeff_crit_2 = (sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)+satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)-(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam)))/(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))
    uncertainty_coeff_crit_se_2 = sqrt(TP*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(TP/satirtop1)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop1/toplam))^2+FP*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(FP/satirtop2)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop1/toplam))^2+FN*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(FN/satirtop1)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop2/toplam))^2+TN*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(TN/satirtop2)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop2/toplam))^2)/toplam/((sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)^2
    uncertainty_coeff_crit_low_2 = uncertainty_coeff_crit_2 + qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se_2
    uncertainty_coeff_crit_upp_2 = uncertainty_coeff_crit_2 - qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se_2
    ## Yule s Y (Coefficient of colligation)
    yule_y = (sqrt(TP*TN)-sqrt(FN*FP))/(sqrt(TP*TN)+sqrt(FN*FP))
    ## Pearson ki-kare
    pearson_chi_squ = toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)
    pearson_chi_squ_p = dchi(pearson_chi_squ, df=1)
    ## 
    with_yate_cor_for_pearson = toplam*(abs(TP*TN-FN*FP)-0.5*toplam)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)
    with_yate_cor_p_for_pearson = dchi(with_yate_cor_for_pearson, df=1)
    ## Mantel Haenszel chi-square
    ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
    mantel_haenszel = ((toplam-1)*(TP*TN-FN*FP)^2)/(satirtop1*satirtop2*sutuntop1*sutuntop2)
    mantel_haenszel_p = dchi(mantel_haenszel, df=1)
    ## Olasilik orani
    likelih_ratio = 2*(TP*log(TP/(sutuntop1*satirtop1/toplam))+FN*log(FN/(sutuntop2*satirtop1/toplam))+FP*log(FP/(sutuntop1*satirtop2/toplam))+TN*log(TN/(sutuntop2*satirtop2/toplam)))
    likelih_ratio_p =dchi(likelih_ratio, df=1)
    ## Fisher'in exact testi
    ##fisher_exact_test = (TP-(satirtop1*sutuntop1/toplam))^2/(satirtop1*sutuntop1/toplam) + (FN-(satirtop1*sutuntop2/toplam))^2/(satirtop1*sutuntop2/toplam) + (FP-(satirtop2*sutuntop1/toplam))^2/(satirtop2*sutuntop1/toplam) + (TN-(satirtop2*sutuntop2/toplam))^2/(satirtop2*sutuntop2/toplam)
    fisher <- c(TP, FN, FP, TN)
    tab <- t(matrix(fisher, nrow=2,ncol=2))
    fisher_exact_test = fisher.test(tab)$p.value
    minimum_exp_fre = min((sutuntop1*satirtop1/toplam):(sutuntop2*satirtop2/toplam))
    cells_exp_fre_5 = (if((sutuntop1*satirtop1/toplam)<5) 1 else 0)+ (if((sutuntop1*satirtop2/toplam)<5) 1 else 0)+ (if((sutuntop2*satirtop1/toplam) <5) 1 else 0)+ (if((sutuntop2*satirtop2/toplam) <5) 1 else 0)
    cells_exp_fre_1 = (if((sutuntop1*satirtop1/toplam) <1) 1 else 0)+ (if((sutuntop1*satirtop2/toplam) <1) 1 else 0)+ (if((sutuntop2*satirtop1/toplam) <1) 1 else 0)+ (if((sutuntop2*satirtop2/toplam) <1) 1 else 0)
    ## Mc Nemar testi
    mcNemar_test = (FP-FN)^2/(FP+FN)
    mcNemar_test_p = dchi(mcNemar_test, df=1)
    with_Yate_cor_for_mcnamer = (abs(FP-FN)-1)^2/(FP+FN)
    with_Yate_cor_p_for_mcnamer = dchi(with_Yate_cor_for_mcnamer, df=1)
    
    ## Belirsizlik (Entropi)
    forbes_NMI = sensitivity_estimate*predvalue_positiverandomtest*log((sensitivity_estimate/predvalue_positiverandomtest_mar_ol),base=2)+
      (1-specificity)*(1-predvalue_positiverandomtest)*log(((1-specificity)/predvalue_positiverandomtest_mar_ol),base=2)+
      (1-sensitivity_estimate)*predvalue_positiverandomtest*log(((1-sensitivity_estimate)/(1-predvalue_positiverandomtest_mar_ol)),base=2)+
      specificity*(1-predvalue_positiverandomtest)*log((specificity/(1-predvalue_positiverandomtest_mar_ol)),base=2)
    ## satir icin entropy (test icin)
    entropy_hr = - ((sutuntop1/toplam) * log(sutuntop1/toplam, base=2) + (sutuntop2/toplam) * log(sutuntop2/toplam, base=2))
    ## sutun icin entropy (hastalik icin)  
    entropy_hc = - ((satirtop1/toplam)*log(satirtop1/toplam, base=2) + (satirtop2/toplam)*log(satirtop2/toplam, base=2))
    ## birlesik entropi (joint entropy)  
    entropy_hrc = - ( (TP/toplam)*log(TP/toplam, base=2) + (FP/toplam)*log(FP/toplam, base=2) + (FN/toplam)*log(FN/toplam, base=2) + (TN/toplam)*log(TN/toplam, base=2))
    ## bilgi icerigi (mutual information)  
    information_r_c = entropy_hr + entropy_hc - entropy_hrc
    ## kosullu entropi (conditional entropy)  
    a = entropy_hrc - entropy_hr
    c = entropy_hrc - entropy_hc
    sim_r_c = information_r_c / (a + information_r_c + c)
    dis_r_c = (a + c) / (a + information_r_c + c)
    ## goreli entropi (relative entropy, kullback-leibler uzakligi)
    ## pozitif test sonucu icin goreli entropi, Relative Improvement Over Chance (RIOC)
    positive_relative_entropy = (TP/sutuntop1)*log(((TP/sutuntop1)/ prevalence), base=2)+(FP/sutuntop1)*log(((FP/sutuntop1)/(1- prevalence)), base=2)
    ## negatif test sonucu icin goreli entropi 
    negative_relative_entropy = (FN/sutuntop2)*log(((FN/sutuntop2)/ prevalence), base=2)+(TN/sutuntop2)*log(((TN/sutuntop2)/(1- prevalence)), base=2)
    
    verim <- data.frame(#Estimate=integer(),
      #LowerCI=integer(),
      #UpperCI=integer()
    )
    row <- vector(mode="character")
    if (sensitivity_estimate >= 0 && sensitivity_estimate <= 1) 
    {
      verim <- data.frame(Estimate = round(sensitivity_estimate, digit =3),  LowerCI = round(sensitivity_low, digit =3), UpperCI = round(sensitivity_upp, digit =3))
      row <- c("Sensitivity")
    } 
    if (sensitivity_randomtest >= 0 && sensitivity_randomtest <= 1)
    {
      newRow <- data.frame(Estimate = round(sensitivity_randomtest, digit =3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Sensitivity of a random test")
    }
    if (qualityindex_sensitivity >= 0 && qualityindex_sensitivity <= 1)
    {
      newRow <- data.frame(Estimate = round(qualityindex_sensitivity, digit =3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Quality index of sensitivity")
    }
    if (specificity >= 0 && specificity <= 1)
    {
      newRow <- data.frame(Estimate = round(specificity, digit=3), LowerCI = round(specificity_low, digit =3), UpperCI = round(specificity_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Specificity")
    }
    if (specificity_randomtest >= 0 && specificity_randomtest <= 1)
    {
      newRow <- data.frame(Estimate = round(specificity_randomtest, digit =3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Specificity of a random test")
    }
    if (qualityindex_specificity >= 0 && qualityindex_specificity <= 1)
    {
      newRow <- data.frame(Estimate = round(qualityindex_specificity, digit =3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Quality index of specificity")
    }
    if (efficiency >= 0 && efficiency <= 1)
    {
      newRow <- data.frame(Estimate = round(efficiency, digit =3),  LowerCI = round(efficiency_low, digit =3), UpperCI = round(efficiency_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Efficiency (Correct classification rate)")
    }
    if (efficiency_randomtest >= 0 && efficiency_randomtest <= 1)
    {
      newRow <- data.frame(Estimate = round(efficiency_randomtest, digit =3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Efficiency of a random test")
    }
    #### #mis_eff <- data.frame(Symbol = 'M_EFF', Estimate = round(mis_efficiency, digit = 3), se = 0, LowerCI = 0, UpperCI = 0)
    
    if (quality_index >= 0 && quality_index <= 1)
    {
      newRow <- data.frame(Estimate = round(quality_index, digit=3), LowerCI = round(quality_index_low, digit =3), UpperCI = round(quality_index_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Quality index")
    }
    if (youdens_index >= 0 && youdens_index <= 1)
    {
      newRow<- data.frame(Estimate = round(youdens_index, digit =3), LowerCI = round(youdens_index_low, digit =3), UpperCI = round(youdens_index_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Youden\'s index")
    }
    if (number_needed_to_diagnose >= 0 && number_needed_to_diagnose <= 1)
    {
      newRow <- data.frame(Estimate = round(number_needed_to_diagnose, digit =3), LowerCI = 0, UpperCI = 0)
      verim <- rbind(verim,newRow)
      row <- append(row, "Number Needed to Diagnose")
    }
    if (number_needed_to_diagnose >= 0 && number_needed_to_diagnose <= 1)
    {
      newRow <- data.frame(Estimate = round(number_needed_to_diagnose, digit =3), LowerCI = 0, UpperCI = 0)
      verim <- rbind(verim,newRow)
      row <- append(row, "Number Needed to Diagnose")
    }
    if (number_needed_to_misdiagnose >= 0 && number_needed_to_misdiagnose <= 1)
    {
      newRow <- data.frame(Estimate = round(number_needed_to_misdiagnose, digit =3), LowerCI = 0, UpperCI = 0)
      verim <- rbind(verim,newRow)
      row <- append(row, "Number Needed to Misdiagnose")
    }
    if (predictivevalue_positivetest >= 0 && predictivevalue_positivetest <= 1)
    {
      newRow <- data.frame(Estimate = round(predictivevalue_positivetest, digit =3), LowerCI = round(predictivevalue_positivetest_low, digit =3), UpperCI = round(predictivevalue_positivetest_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Predictive value of positive test")
    }
    if (predvalue_positiverandomtest >= 0 && predvalue_positiverandomtest <= 1)
    {
      newRow<- data.frame(Estimate = round(predvalue_positiverandomtest, digit =3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Pred. value of a positive random test")
    }
    if (predictivevalue_negativetest >= 0 && predictivevalue_negativetest <= 1)
    {
      newRow<- data.frame(Estimate = round(predictivevalue_negativetest, digit =3), LowerCI = format(round(predictivevalue_negativetest_low, digit =3),nsmall=3), UpperCI = round(predictivevalue_negativetest_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Predictive value of negative test")
    }
    if (predvalue_negativerandomtest >= 0 && predvalue_negativerandomtest <= 1)
    {
      newRow <- data.frame(Estimate = round(predvalue_negativerandomtest, digit =3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Pred. value of a negative random test")
    }
    if (false_positiverate >= 0 && false_positiverate <= 1)
    {
      newRow <- data.frame(Estimate = round(false_positiverate, digit =3), LowerCI = round(false_positiverate_low, digit =3), UpperCI = round(false_positiverate_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "False positive rate")
    }
    if (false_negativerate >= 0 && false_negativerate <= 1)
    {
      newRow  <- data.frame(Estimate = round(false_negativerate, digit =3),  LowerCI = round(false_negativerate_low, digit =3), UpperCI = round(false_negativerate_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "False negative rate")
    }
    if (true_positiverate >= 0 && true_positiverate <= 1)
    {
      newRow  <- data.frame(Estimate = round(true_positiverate, digit =3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "True positive rate")
    }
    if (misclassification_rate >= 0 && misclassification_rate <= 1)
    {
      newRow <- data.frame(Estimate = round(misclassification_rate, digit =3),  LowerCI = round(misclassification_rate_low, digit =3), UpperCI = round(misclassification_rate_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Misclassification rate")
    }
    if (rand_index >= 0 && rand_index <= 1)
    {
      newRow <- data.frame(Estimate = round(rand_index, digit =3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Rand Index (Rand Measure)")
    }
    if (balanced_accuracy >= 0 && balanced_accuracy <= 1)
    {
      newRow <- data.frame(Estimate = round(balanced_accuracy, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Balanced Accuracy (Average Accuracy)")
    }
    if (lift >= 0 && lift <= 1)
    {
      newRow <- data.frame(Estimate = round(lift, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Lift")
    }
    if (precision >= 0 && precision <= 1)
    {
      newRow <- data.frame(Estimate = round(precision, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Precision")
    }
    if (recall >= 0 && recall <= 1)
    {
      newRow <- data.frame(Estimate = round(recall, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Recall")
    }
    if (discriminant_power >= 0 && discriminant_power <= 1)
    {
      newRow <- data.frame(Estimate = round(discriminant_power, digit =3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Discriminant Power")
    }
    if (f1_score >= 0 && f1_score <= 1)
    {
      newRow <- data.frame(Estimate = round(f1_score, digit =3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "F1 Score (F Measure)")
    }
    if (matthews_correlation_coefficient >= 0 && matthews_correlation_coefficient <= 1)
    {
      newRow <- data.frame(Estimate = round(matthews_correlation_coefficient, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Matthew\'s Correlation Coefficient")
    }
    if (entropy_hr >= 0 && entropy_hr <= 1)
    {
      newRow <- data.frame(Estimate = round(entropy_hr, digit =3),  LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      row <- append(row, "Entropy for true")
    }
    if (entropy_hc >= 0 && entropy_hc <= 1)
    {
      newRow <- data.frame(Estimate = round(entropy_hc, digit =3),  LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      row <- append(row, "Entropy for False")
    }
    if (entropy_hrc >= 0 && entropy_hrc <= 1)
    {
      newRow <- data.frame(Estimate = round(entropy_hrc, digit =3),  LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      row <- append(row, "Joint Entropy")
    }
    if (information_r_c >= 0 && information_r_c <= 1)
    {
      newRow <- data.frame(Estimate = round(information_r_c, digit =3),  LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      row <- append(row, "Mutual Information")
    }
    if (a >= 0 && a <= 1)
    {
      newRow <- data.frame(Estimate = round(a, digit =3),  LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      row <- append(row, "Joint Entropy - Entropy for true")
    }
    if (c >= 0 && c <= 1)
    {
      newRow <- data.frame(Estimate = round(c, digit =3),  LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      row <- append(row, "Joint Entropy - Entropy for false")
    }
    if (sim_r_c >= 0 && sim_r_c <= 1)
    {
      newRow <- data.frame(Estimate = round(sim_r_c, digit =3),  LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      row <- append(row, "Similarity of Descriptors True and False")
    }
    if (dis_r_c >= 0 && dis_r_c <= 1)
    {
      newRow <- data.frame(Estimate = round(dis_r_c, digit =3),  LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      row <- append(row, "Distance Between True and False")
    }
    if (positive_relative_entropy >= 0 && positive_relative_entropy <= 1)
    {
      newRow <- data.frame(Estimate = round(positive_relative_entropy, digit =3),  LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      row <- append(row, "Relative Improvement Over Chance (RIOC)")
    }
    if (negative_relative_entropy >= 0 && negative_relative_entropy <= 1)
    {
      newRow <- data.frame(Estimate = round(negative_relative_entropy, digit =3),  LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      row <- append(row, "Relative Entropy for Negative Test")
    }
    
    rownames(verim) = row
    

    
    highchart() %>% hc_exporting(enabled = TRUE, filename = "Plot01") %>% 
      hc_add_series(name = "Conf. Interval", type = "bar", data = verim$`Estimate`) %>%
      hc_add_series(name = "CI", data = as.matrix(cbind(verim$`LowerCI`, verim$`UpperCI`)),type = "errorbar", names = "Limits", showInLegend = FALSE, zIndex = 0, lineWidth = 1.5, linkedTo = "CI"
      ) %>%
      hc_xAxis(categories = rownames(verim),title = "Diagnostic Measures") %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, headerFormat = "<b>Diagnostic Measures: </b>{point.x} <br>") %>%
      hc_plotOptions(line = list(tooltip = list(pointFormat = "<b>{series.name}: </b>{point.y:.3f} ")), 
                     errorbar = list(tooltip = list(pointFormat = "({point.low} - {point.high})"))) %>%
      hc_add_theme(hc_theme_google())
    
    
  })

  
  ## Association Measures
  dataM1 <- reactive({   
    if (is.null(trimws(input$text3)) && is.null(trimws(input$text4)))  {return(NULL)}
    else {
      if (input$verigir_kontrol == TRUE){ 
        TP = 0
        FN = 0
        FP = 0
        TN = 0
        for (i in 1:nrow(tabloveri()))
        {
          if (tabloveri()[i,1]==1 & tabloveri()[i,2]==1)
          {TP = TP+1}
          else if (tabloveri()[i,1]==1 & tabloveri()[i,2]==2)
          {FN = FN+1}
          else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==1)
          {FP = FP+1}
          else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==2)
          {TN = TN+1}
          next
        }
        CI = as.numeric(trimws(input$text5_2))
      }
      else{
        TP = as.numeric(trimws(input$text1)) ### x
        FN = as.numeric(trimws(input$text2)) ### y
        FP = as.numeric(trimws(input$text3)) ### z
        TN = as.numeric(trimws(input$text4)) ### t
        CI = as.numeric(trimws(input$text5))
      }
      
      
      satirtop1 = TP + FN
      satirtop2 = FP + TN
      sutuntop1 = TP + FP
      sutuntop2 = FN + TN
      toplam = satirtop1 + satirtop2
      #table1 <- matrix(c(TP,FN,satirtop1,FP,TN,satirtop2,sutuntop1,sutuntop2,toplam),nrow=3,ncol=3,byrow=TRUE,dimnames=list(c("1.satir","2.satir","Sutun toplamlari"),c("1.kolon","2.kolon","Satir toplamlari")))
      #table1
      ## Duyarlilik 
      sensitivity_estimate = TP / satirtop1
      sensitivity_se = round((sqrt(sensitivity_estimate * (1 - sensitivity_estimate)/satirtop1)), 4)
      sensitivity_low = 1 - Rbeta.inv(((1 + CI/100)/2), (satirtop1+1-TP), TP)
      sensitivity_upp = Rbeta.inv(((1 + CI/100)/2), (TP+1), (satirtop1-TP))
      sensitivity_randomtest = sutuntop1 / toplam
      qualityindex_sensitivity = (sensitivity_estimate - sensitivity_randomtest) / (1 - sensitivity_randomtest)
      ## Secicilik
      specificity = TN / satirtop2
      specificity_se = sqrt(specificity * (1 - specificity) / satirtop2)
      specificity_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop2+1-TN),TN)
      specificity_upp = Rbeta.inv((1 + CI/100)/2, (TN+1), (satirtop2-TN))
      specificity_randomtest = (1 - sensitivity_randomtest)
      qualityindex_specificity = (specificity - 1 + sensitivity_randomtest) / sensitivity_randomtest
      ## Gain in Certainty
      gain_in_certainty = sensitivity_estimate + specificity
      ## Etkililik, yeterlilik ayni zamanda overall accuracy (toplam-genel dogruluk orani)
      efficiency = (TP + TN) / toplam
      efficiency_se = sqrt(efficiency * (1 - efficiency) / toplam)
      efficiency_low = 1-Rbeta.inv((1 + CI/100)/2, (toplam+1-TP-TN), (TP+TN))
      efficiency_upp = Rbeta.inv((1 + CI/100)/2, (TP+TN+1), (toplam-TP-TN))
      efficiency_randomtest = (satirtop1 / toplam) * sensitivity_randomtest + (1 - (satirtop1 / toplam)) * (1 - sensitivity_randomtest)
      mis_efficiency = 1 - efficiency
      ## Quality indeks
      quality_index = (efficiency - efficiency_randomtest) / (1 - efficiency_randomtest)
      quality_index_se = sqrt((efficiency*(1-efficiency)/(1-efficiency_randomtest)^2+2*(1-efficiency)*(2*efficiency*efficiency_randomtest-((TP*((satirtop1 / toplam)+sensitivity_randomtest)+TN*(2-(satirtop1 / toplam)-sensitivity_randomtest))/toplam))/(1-efficiency_randomtest)^3+((1-efficiency)^2)*((TP*((satirtop1 / toplam)+sensitivity_randomtest)^2+FN*(-(satirtop1 / toplam)+1+sensitivity_randomtest)^2+FP*(1+(satirtop1 / toplam)-sensitivity_randomtest)^2+TN*(2-(satirtop1 / toplam)-sensitivity_randomtest)^2)/toplam-4*efficiency_randomtest^2)/(1-efficiency_randomtest)^4)/toplam)
      quality_index_low = quality_index + qnorm((1 - CI/100)/2)*quality_index_se
      quality_index_upp = quality_index - qnorm((1 - CI/100)/2)*quality_index_se
      ## Youden indeksi
      youdens_index = sensitivity_estimate + specificity - 1
      youdens_index_se = sqrt(sensitivity_se^2+specificity_se^2)
      youdens_index_low = youdens_index + qnorm((1 - CI/100)/2)*youdens_index_se
      youdens_index_upp = youdens_index - qnorm((1 - CI/100)/2)*youdens_index_se
      
      number_needed_to_diagnose = 1 / youdens_index
      number_needed_to_misdiagnose = 1 / ( 1 - efficiency ) 
      ## Pozitif kestirim degeri
      predictivevalue_positivetest = TP / sutuntop1
      predictivevalue_positivetest_se = sqrt(predictivevalue_positivetest*(1-predictivevalue_positivetest)/sutuntop1)
      predictivevalue_positivetest_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop1+1-TP),TP)
      predictivevalue_positivetest_upp = Rbeta.inv((1 + CI/100)/2, (TP+1),(sutuntop1-TP))
      predvalue_positiverandomtest = (satirtop1 / toplam)
      predvalue_positiverandomtest_mar_ol = predvalue_positiverandomtest*sensitivity_estimate+(1-predvalue_positiverandomtest)*(1-specificity)
      ## Negatif kestirim degeri
      predictivevalue_negativetest = TN / sutuntop2
      predictivevalue_negativetest_se = sqrt(predictivevalue_negativetest*(1-predictivevalue_negativetest)/sutuntop2)
      predictivevalue_negativetest_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop2+1-TN),TN)
      predictivevalue_negativetest_upp = Rbeta.inv((1 + CI/100)/2, (TN+1), (sutuntop2-TN))
      predvalue_negativerandomtest = 1 - (satirtop1 / toplam)
      predvalue_negativerandomtest_mar_ol = 1 - predvalue_positiverandomtest_mar_ol
      ## Predictive Summary Index (PSI)
      predictive_summary_index = predictivevalue_positivetest + predictivevalue_negativetest - 1
      ## Yanlis pozitif orani
      false_positiverate = FP / satirtop2
      false_positiverate_se = sqrt(false_positiverate*(1-false_positiverate)/satirtop2)
      false_positiverate_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop2+1-FP),FP)
      false_positiverate_upp = Rbeta.inv((1 + CI/100)/2, (FP+1), (satirtop2-FP))
      ## Yansis negatif orani
      false_negativerate = FN / satirtop1
      false_negativerate_se = sqrt(false_negativerate*(1-false_negativerate)/satirtop1)
      false_negativerate_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop1+1-FN),FN)
      false_negativerate_upp = Rbeta.inv((1 + CI/100)/2, (FN+1), (satirtop1-FN))
      ## False Omission Rate (FOR) https://www.ncss.com/software/ncss/diagnostic-tests-in-ncss/
      false_omission_rate = FN/(sutuntop2)
      false_omission_rate_se = sqrt(false_omission_rate*(1-false_omission_rate)/sutuntop2)
      false_omission_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop2+1-FN),FN)
      false_omission_rate_upp = Rbeta.inv((1 + CI/100)/2, (FN+1), (sutuntop2-FN))
      ## False Discovery Rate (FDR) https://www.ncss.com/software/ncss/diagnostic-tests-in-ncss/
      false_discovery_rate = FP/(sutuntop1)
      false_discovery_rate_se = sqrt(false_discovery_rate*(1-false_discovery_rate)/sutuntop1)
      false_discovery_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop1+1-FP),FP)
      false_discovery_rate_upp = Rbeta.inv((1 + CI/100)/2, (FP+1), (sutuntop1-FP))
      ## Dogru pozitif orani
      true_positiverate = TP / (satirtop1)
      ## Yanlis siniflandirma orani
      misclassification_rate = (FN + FP) / toplam
      misclassification_rate_se = sqrt(misclassification_rate*(1-misclassification_rate)/toplam)
      misclassification_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-FP-FN), (FP+FN))
      misclassification_rate_upp = Rbeta.inv((1 + CI/100)/2, (FP+FN+1), (toplam-FP-FN))
      ## Prevelans
      prevalence = (satirtop1 / toplam)
      prevalence_se = sqrt((satirtop1 / toplam)*(1-(satirtop1 / toplam))/toplam)
      prevalence_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-satirtop1), satirtop1)
      prevalence_upp = Rbeta.inv((1 + CI/100)/2, (satirtop1+1), (toplam-satirtop1))
      
      ##caret paketinden olculer
      detection_rate = TP/toplam
      detection_prevalence = (TP+FP)/toplam
      balanced_accuracy = (sensitivity_estimate + specificity)/2
      lift = (TP/(TP+FN))/((TP+FP)/toplam)  ## https://gerardnico.com/wiki/data_mining/error_rate
      precision = TP/(TP+FP)
      recall = TP/(TP+FN)
      ## http://michael.hahsler.net/research/recomm_lnai2002/lnai2002.pdf
      conviction = (1-((TP+FP)/toplam))/(1-(TP/(TP+FN)))
      matthews_correlation_coefficient = (TP * TN + FN * FP) / ((TP+FP)*(FN+TN)*(FP+TN)*(TP+FN))^(1/2)
      ## Goreli Risk Orani (Relative risk)
      ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
      # https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20ratio.htm
      relative_risk = (TP/sutuntop1)/(FN/sutuntop2)
      relative_risk_se = sqrt((1/TP)+(1/FN)-(1/(TP+FP))-(1/(FN+TN)))
      relative_risk_low = exp(log(relative_risk)+qnorm((1-CI/100)/2)*relative_risk_se)
      relative_risk_upp = exp(log(relative_risk)-qnorm((1-CI/100)/2)*relative_risk_se)
      difference_in_proportion = (TP/sutuntop1) - (FN/sutuntop2)
      ## Leverage
      ## https://www3.nd.edu/~tweninge/pubs/AHBCW_ANNIE08.pdf
      leverage = (TP/sutuntop1)-((TP+FP)/toplam)
      ## http://www.ebm.med.ualberta.ca/TherapyCalc.html
      control_event_rate = FN/sutuntop2
      experimental_event_rate = TP/sutuntop1
      ## Absolute risk reduction
      absolute_risk_reduction = -difference_in_proportion
      absolute_risk_reduction_se = sqrt((TP/sutuntop1)*(1-(TP/sutuntop1))/sutuntop1+(FN/sutuntop2)*(1-(FN/sutuntop2))/sutuntop2)
      absolute_risk_reduction_low = absolute_risk_reduction - qnorm((1-CI/100)/2)*absolute_risk_reduction_se
      absolute_risk_reduction_upp = absolute_risk_reduction + qnorm((1-CI/100)/2)*absolute_risk_reduction_se
      ## Relative risk reduction
      relative_risk_reduction = absolute_risk_reduction / (FN/sutuntop2)
      relative_risk_reduction_low = 1 - exp(log(relative_risk)-qnorm((1-CI/100)/2)*relative_risk_se)
      relative_risk_reduction_upp = 1 - exp(log(relative_risk)+qnorm((1-CI/100)/2)*relative_risk_se)
      ##Number needed to treat
      number_needed_to_treat = 1 / abs(difference_in_proportion)
      number_needed_to_treat_low = 1 /absolute_risk_reduction_upp
      number_needed_to_treat_upp = 1 /absolute_risk_reduction_low
      ## Test duzeyi
      test_level = sensitivity_randomtest
      test_level_se = sqrt(test_level*(1-test_level)/toplam)
      test_level_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-sutuntop1),sutuntop1)
      test_level_upp = Rbeta.inv((1 + CI/100)/2, (sutuntop1+1), (toplam-sutuntop1))
      ## Pre-test odds
      pretest_odds = prevalence / (1 - prevalence)
      ## Pozitif Olabilirlik Orani
      likelihoodratio_positivetest = sensitivity_estimate/(1-specificity)
      likelihoodratio_positivetest_se = exp(sqrt((1-sensitivity_estimate)/(sensitivity_estimate*satirtop1) +specificity/((1-specificity)*satirtop2)))
      likelihoodratio_positivetest_low = exp(log(likelihoodratio_positivetest) + qnorm((1 - CI/100)/2)*log(likelihoodratio_positivetest_se))
      likelihoodratio_positivetest_upp = exp(log(likelihoodratio_positivetest) - qnorm((1 - CI/100)/2)*log(likelihoodratio_positivetest_se))
      ## post-test odds
      posttest_odds = pretest_odds * likelihoodratio_positivetest
      bayes_ppv = (sensitivity_estimate * prevalence)/((sensitivity_estimate * prevalence) + (1 - specificity) * (1 - prevalence))
      ## Post-test probability (positive test result)
      posttest_probability = (pretest_odds * likelihoodratio_positivetest) / (1 + (pretest_odds * likelihoodratio_positivetest))
      ## Negatif Olabilirlik Orani
      likelihoodratio_negativetest = (1-sensitivity_estimate)/specificity
      likelihoodratio_negativetest_se = exp(sqrt(sensitivity_estimate/((1-sensitivity_estimate)*satirtop1) +(1-specificity)/(specificity*satirtop2)))
      likelihoodratio_negativetest_low = exp(log(likelihoodratio_negativetest) + qnorm((1-CI/100)/2)*log(likelihoodratio_negativetest_se))
      likelihoodratio_negativetest_upp = exp(log(likelihoodratio_negativetest) - qnorm((1-CI/100)/2)*log(likelihoodratio_negativetest_se))
      bayes_npv = (sensitivity_estimate * (1 - prevalence))/((sensitivity_estimate * (1 - prevalence)) + (1 - specificity) * prevalence)
      ## Post-test probability (negative test result)
      posttest_probability_neg = (pretest_odds * likelihoodratio_negativetest) / (1 + (pretest_odds * likelihoodratio_negativetest))
      ## Ters negatif olabilirlik orani
      inverse_likelihoodratio_negativetest = specificity/(1-sensitivity_estimate)
      inverse_likelihoodratio_negativetest_se = exp(sqrt(sensitivity_estimate/((1-sensitivity_estimate)*satirtop1) +(1-specificity)/(specificity*satirtop2)))
      inverse_likelihoodratio_negativetest_low = exp(log(inverse_likelihoodratio_negativetest) + qnorm((1-CI/100)/2)*log(inverse_likelihoodratio_negativetest_se))
      inverse_likelihoodratio_negativetest_upp = exp(log(inverse_likelihoodratio_negativetest) - qnorm((1-CI/100)/2)*log(inverse_likelihoodratio_negativetest_se))
      ## Diagnostic Odds Ratio (Tani Odds Orani)
      odds_ratio = (TP/FN)/(FP/TN)
      odds_ratio_se = exp(sqrt(1/TP+1/FP+1/TN+1/FN))
      odds_ratio_low = exp(log(odds_ratio) + qnorm((1-CI/100)/2)*log(odds_ratio_se))
      odds_ratio_upp = exp(log(odds_ratio) - qnorm((1-CI/100)/2)*log(odds_ratio_se))
      odds_ratio_Haldane = ((TP+0.5)/(FN+0.5))/((FP+0.5)/(TN+0.5))
      odds_ratio_Haldane_se = exp(sqrt(1/(TP+0.5)+1/(FP+0.5)+1/(TN+0.5)+1/(FN+0.5)))
      odds_ratio_Haldane_low = exp(log(odds_ratio_Haldane) + qnorm((1-CI/100)/2)*log(odds_ratio_Haldane_se))
      odds_ratio_Haldane_upp = exp(log(odds_ratio_Haldane) - qnorm((1-CI/100)/2)*log(odds_ratio_Haldane_se))
      error_odds_ratio = (sensitivity_estimate/(1-sensitivity_estimate))/(specificity/(1-specificity))
      ## Rate ratio
      ## http://omarkasule-05.tripod.com/id52.html
      ## http://sphweb.bumc.bu.edu/otlt/mph-modules/ep/ep713_randomerror/ep713_randomerror4.html
      rate_ratio = (TP/sutuntop1)/(TP/sutuntop2)
      rate_ratio_low = exp(log(relative_risk)+qnorm((1-CI/100)/2)*sqrt(1/TP+1/FP))
      rate_ratio_upp = exp(log(relative_risk)-qnorm((1-CI/100)/2)*sqrt(1/TP+1/FP))
      ## Risk Difference
      ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
      risk_difference = (TP/sutuntop1) - (FN/sutuntop2)
      risk_difference_se = (((TP/sutuntop1)*(1-(TP/sutuntop1)))/sutuntop1)+(((FN/sutuntop2)*(1-(FN/sutuntop2)))/sutuntop2)
      risk_difference_low = risk_difference + qnorm((1-CI/100)/2)*sqrt(risk_difference_se)
      risk_difference_upp = risk_difference - qnorm((1-CI/100)/2)*sqrt(risk_difference_se)
      ## Nitelenebilen risk
      ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
      attributable_risk = (TP/sutuntop1)-(FN/sutuntop2)
      attributable_risk_se = sqrt((satirtop1/toplam)*(1-(satirtop1/toplam))*(1/sutuntop1+1/sutuntop2))
      attributable_risk_low = attributable_risk + qnorm((1-CI/100)/2)*attributable_risk_se
      attributable_risk_upp = attributable_risk - qnorm((1-CI/100)/2)*attributable_risk_se
      ## Attributable risk percent
      ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
      attributable_risk_percent = ((relative_risk-1)/relative_risk)*100
      attributable_risk_percent_low = attributable_risk_percent - (qnorm((1-CI/100)/2)*attributable_risk_se)/attributable_risk
      attributable_risk_percent_upp = attributable_risk_percent + (qnorm((1-CI/100)/2)*attributable_risk_se)/attributable_risk
      ## Population Attributable Risk
      ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
      population_attributable_risk = (satirtop1/toplam)-(FN/sutuntop2)
      ## Population Attributable Risk Percent
      population_attributable_risk_percent = (population_attributable_risk / (satirtop1/toplam))*100
      ## Kohen's Kappa
      cohens_kappa = (efficiency-efficiency_randomtest)/(1-efficiency_randomtest)
      cohens_kappa_se = quality_index_se
      cohens_kappa_low = cohens_kappa + qnorm((1-CI/100)/2)*cohens_kappa_se
      cohens_kappa_upp = cohens_kappa - qnorm((1-CI/100)/2)*cohens_kappa_se
      ## Gozlenen Karar
      observed_agreement = efficiency
      observed_agreement_se = efficiency_se
      observed_agreement_low = efficiency_low
      observed_agreement_upp = efficiency_upp
      ## Risk karari,expected agreement
      chance_agreement = efficiency_randomtest
      ## Pozitif karar
      positive_agreement = 2*TP /(satirtop1+sutuntop1)
      positive_agreement_se = sqrt(4*TP*(FP+FN)*(TP+FP+FN))/(sutuntop1+satirtop1)^2
      positive_agreement_low = positive_agreement + qnorm((1-CI/100)/2)*positive_agreement_se
      positive_agreement_upp = positive_agreement - qnorm((1-CI/100)/2)*positive_agreement_se
      ## Negatif karar
      negative_agreement = 2*TN/(satirtop2+sutuntop2)
      negative_agreement_se = sqrt(4*TN*(FP+FN)*(TN+FP+FN))/(sutuntop2+satirtop2)^2
      negative_agreement_low = negative_agreement + qnorm((1-CI/100)/2)*negative_agreement_se
      negative_agreement_upp = negative_agreement - qnorm((1-CI/100)/2)*negative_agreement_se
      ## Rand index
      ## http://www.cs.pomona.edu/classes/cs158/resources/158-10(EM).pdf
      rand_index = (TP+TN)/(TP+FN+FP+TN)
      ## e-measure
      ## http://michael.hahsler.net/research/recomm_lnai2002/lnai2002.pdf
      e_measure = 1/((((1-CI)/100)*(1-precision))+((1-((1-CI)/100))*(1/recall)))
      ## discriminant power 
      ## https://eva.fing.edu.uy/pluginfile.php/69453/mod_resource/content/1/7633-10048-1-PB.pdf
      discriminant_power = (sqrt(3)/pi)*(log (sensitivity_estimate / (1-sensitivity_estimate)) + log (specificity / (1-specificity)))
      ## F1 Score
      ## http://machinelearningmastery.com/classification-accuracy-is-not-enough-more-performance-measures-you-can-use/
      f1_score = 2*(((TP/(TP+FN))*(TP/(TP+FP)))/((TP/(TP+FN))+(TP/(TP+FP))))
      ## Byrt yanlilik indeksi
      byrt_bias_index = (FN-FP)/toplam
      ## Byrt  asimetrik indeks prevelansi
      byrt_prevalence_asymmetry_index = (TN-TP)/toplam
      ## Yanlilik duzeltmeli kappa
      bias_adjusted_kappa = (efficiency-0.25*((satirtop1+sutuntop1)^2+(satirtop2+sutuntop2)^2)/toplam^2)/(1-0.25*((satirtop1+sutuntop1)^2+(satirtop2+sutuntop2)^2)/toplam^2)
      ## Yanlilik duzeltmeli kappa prevelansi
      prevalence_bias_adjusted_kappa = cohens_kappa+(1-cohens_kappa)*(byrt_prevalence_asymmetry_index^2-byrt_bias_index^2)
      ## Dice'in indeksi (Czekanowski)
      dice_index = positive_agreement
      dice_index_se= positive_agreement_se
      dice_index_low = positive_agreement_low
      dice_index_upp = positive_agreement_upp
      ## Yule's Q
      yules_q = (odds_ratio - 1) / (odds_ratio + 1)
      yules_q_se = 0.5*(1-yules_q^2)*sqrt(1/TP+1/FP+1/TN+1/FN)
      yules_q_low = yules_q + qnorm((1-CI/100)/2)*yules_q_se
      yules_q_upp = yules_q - qnorm((1-CI/100)/2)*yules_q_se
      
      equitable_threatscore = (TP-(sutuntop1*satirtop1/toplam))/(TP+FP+FN-(sutuntop1*satirtop1/toplam))
      ## Phi
      phi = (TP*TN-FN*FP) / sqrt(satirtop1 * satirtop2 * sutuntop1 * sutuntop2)
      phi_se = sqrt((1-phi^2+(phi+0.5*phi^3)*((satirtop1-satirtop2)*(sutuntop1-sutuntop2)/sqrt(satirtop1*satirtop2*sutuntop1*sutuntop2))-0.75*phi^2*((satirtop1-satirtop2)^2/(satirtop1*satirtop2)+(sutuntop1-sutuntop2)^2/(sutuntop1*sutuntop2)))/toplam)
      phi_low = phi + qnorm((1-CI/100)/2)*phi_se
      phi_upp = phi - qnorm((1-CI/100)/2)*phi_se
      ## Cramer V katsayisi
      cramer_v = sqrt((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2))/toplam)
      ## Olaganlik katsayisi
      contingency_coefficient = sqrt((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2))/(((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)))+toplam))
      ## Goodman and Kruskal Gamma 
      goodman_and_kruskal_gamma = (TP*TN-FN*FP)/(TP*TN+FN*FP)
      goodman_and_kruskal_gamma_se = 0.5*(1-yules_q^2)*sqrt(1/TP+1/FP+1/TN+1/FN)
      goodman_and_kruskal_gamma_low = yules_q + qnorm((1-CI/100)/2)*yules_q_se
      goodman_and_kruskal_gamma_upp = yules_q - qnorm((1-CI/100)/2)*yules_q_se
      ## Kendall's tau a
      kendalls_tau_a = (TP*TN-FN*FP)/(toplam*(toplam-1)/2)
      ## Kendall's tau b
      ## https://v8doc.sas.com/sashtml/stat/chap28/sect20.htm
      ## http://slideplayer.com/slide/10766029/
      kendalls_tau_b = (TP*TN-FN*FP)/sqrt((TP*TN+FN*FP+(TP*FN+FP*TN))*(TP*TN+FN*FP+(TP*FP+FN*TN)))
      ## Kendall's tau c
      kendalls_tau_c = ((2*2)*(TP*TN-FN*FP)) / (toplam^2 * (2-1))
      ## Somers'd R|C
      somers_d = (TP*TN-FN*FP)/(TP*TN+FN*FP+(TP*FN+FP*TN))
      ## Somers'd C|R
      somers_d_2 = (TP*TN-FP*FN)/(TP*TN+FP*FN+(TP*FP+FN*TN))
      ## Scoot'un karar indeksi
      scotts_agreement_index = bias_adjusted_kappa
      ## Dort-duzeyli Korelasyon
      tetrachoric_correlation = cos(pi/(1+sqrt(TP*TN/FN/FP)))
      tetrachoric_correlation_se = 0.375*(1-tetrachoric_correlation^2)*sqrt((1/(TP))+(1/(FP))+(1/(TN))+(1/(FN)))
      tetrachoric_correlation_low = tetrachoric_correlation + qnorm((1-CI/100)/2)*tetrachoric_correlation_se
      tetrachoric_correlation_upp = tetrachoric_correlation - qnorm((1-CI/100)/2)*tetrachoric_correlation_se
      ## Goodman kruskal tau katsayisi
      goodman_kruskal_tau = (toplam*((TP^2+FP^2)/sutuntop1+(FN^2+TN^2)/sutuntop2)-satirtop1^2-satirtop2^2)/(toplam^2-satirtop1^2-satirtop2^2)
      goodman_kruskal_tau_se = toplam*sqrt(TP*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop1)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*TP/satirtop1-(TP^2+FN^2)/satirtop1^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+FN*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop2)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*FN/satirtop1-(TP^2+FN^2)/satirtop1^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+FP*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop1)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*FP/satirtop2-(FP^2+TN^2)/satirtop2^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+TN*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop2)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*TN/satirtop2-(FP^2+TN^2)/satirtop2^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2)/(toplam^2-sutuntop1^2-sutuntop2^2)^2
      goodman_kruskal_tau_low = goodman_kruskal_tau + qnorm((1-CI/100)/2)*goodman_kruskal_tau_se
      goodman_kruskal_tau_upp = goodman_kruskal_tau - qnorm((1-CI/100)/2)*goodman_kruskal_tau_se
      ## Simetrik Lambda 
      lambda_symmetric = ((max(TP,FN)+max(FP,TN)+max(TP,FP)+max(FN,TN)-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2))/(2*toplam-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2)))
      if (lambda_symmetric==0 || lambda_symmetric==1)
      {lambda_symmetric_se = 0}
      else
      {
        lambda_symmetric_se = sqrt((TP*((if(TP>FN) 1 else 0) + (if(TP>FP) 1 else 0) + ((if(sutuntop1>sutuntop2) 1 else 0)+ (if(satirtop1>satirtop2) 1 else 0))*(lambda_symmetric-1))^2+FN*( (if(TP>FN) 0 else 1)+ (if(FN>TN) 1 else 0) +( (if(sutuntop1>sutuntop2) 0 else 1) + (if(satirtop1>satirtop2) 1 else 0))*(lambda_symmetric-1))^2+FP*( (if(FP>TN) 1 else 0) + (if(TP>FP) 0 else 1) +( (if(sutuntop1>sutuntop2) 1 else 0) + (if(satirtop1<satirtop2) 1 else 0))*(lambda_symmetric-1))^2+TN*((if(FP>TN) 0 else 1)+ (if(FN>TN) 0 else 1) +((if(sutuntop1>sutuntop2) 0 else 1)+ (if(satirtop1<satirtop2) 1 else 0))*(lambda_symmetric-1))^2-4*toplam*lambda_symmetric^2)/(2*toplam-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2))^2)
      }
      
      lambda_symmetric_low = lambda_symmetric + qnorm((1-CI/100)/2)*lambda_symmetric_se
      lambda_symmetric_upp = lambda_symmetric - qnorm((1-CI/100)/2)*lambda_symmetric_se
      ## Lambda Asymmetric R|C
      lambda_criterion = (max(TP,FP)+max(FN,TN)-max(satirtop1,satirtop2))/(toplam-max(satirtop1,satirtop2))
      
      if (lambda_criterion==0 || lambda_criterion==1)
      {lambda_criterion_se = 0}
      else
      {
        lambda_criterion_se = sqrt((toplam-max(TP,FP)-max(FN,TN))*(max(TP,FP)+max(FN,TN)+max(satirtop1,satirtop2)-2*(if(satirtop1>satirtop2) max(TP,FN) else max(FP,TN)))/(toplam-max(satirtop1,satirtop2))^3)
      }
      
      lambda_criterion_low = lambda_criterion + qnorm((1-CI/100)/2)*lambda_criterion_se
      lambda_criterion_upp = lambda_criterion - qnorm((1-CI/100)/2)*lambda_criterion_se
      ## Lambda Asymmetric C|R
      lambda_criterion_2 = (max(TP,FN)+max(FP,TN)-max(sutuntop1,sutuntop2))/(toplam-max(sutuntop1,sutuntop2))
      
      if (lambda_criterion_2==0 || lambda_criterion_2==1)
      {lambda_criterion_se_2 = 0}
      else
      {
        lambda_criterion_se_2 = sqrt((toplam-max(TP,FN)-max(FP,TN))*(max(TP,FN)+max(FP,TN)+max(sutuntop1,sutuntop2)-2*(if(sutuntop1>sutuntop2) max(TP,FP) else max(FN,TN)))/(toplam-max(sutuntop1,sutuntop2))^3)
      }
      
      lambda_criterion_low_2 = lambda_criterion_2 + qnorm((1-CI/100)/2)*lambda_criterion_se_2
      lambda_criterion_upp_2 = lambda_criterion_2 - qnorm((1-CI/100)/2)*lambda_criterion_se_2
      ## belirsizlik katsayisi (coefficient of uncertainty) simetrik
      uncertainty_coefficient_symmetric = 2*(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam)))/(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))
      uncertainty_coefficient_symmetric_se = 2*sqrt(TP*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop1*sutuntop1/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(TP/toplam))^2+FN*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop1*sutuntop2/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(FN/toplam))^2+FP*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop2*sutuntop1/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(FP/toplam))^2+TN*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop2*sutuntop2/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(TN/toplam))^2)/toplam/(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam)^2
      uncertainty_coefficient_symmetric_low = uncertainty_coefficient_symmetric + qnorm((1-CI/100)/2)*uncertainty_coefficient_symmetric_se
      uncertainty_coefficient_symmetric_upp = uncertainty_coefficient_symmetric - qnorm((1-CI/100)/2)*uncertainty_coefficient_symmetric_se
      ## (coefficient of uncertainty) R|C
      uncertainty_coeff_crit = (satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam)))/(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))
      uncertainty_coeff_crit_se = sqrt(TP*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(TP/sutuntop1)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop1/toplam))^2+FN*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(FN/sutuntop2)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop1/toplam))^2+FP*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(FP/sutuntop1)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop2/toplam))^2+TN*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(TN/sutuntop2)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop2/toplam))^2)/toplam/((satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam)^2
      uncertainty_coeff_crit_low = uncertainty_coeff_crit + qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se
      uncertainty_coeff_crit_upp = uncertainty_coeff_crit - qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se
      ## (coefficient of uncertainty) C|R
      uncertainty_coeff_crit_2 = (sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)+satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)-(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam)))/(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))
      uncertainty_coeff_crit_se_2 = sqrt(TP*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(TP/satirtop1)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop1/toplam))^2+FP*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(FP/satirtop2)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop1/toplam))^2+FN*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(FN/satirtop1)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop2/toplam))^2+TN*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(TN/satirtop2)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop2/toplam))^2)/toplam/((sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)^2
      uncertainty_coeff_crit_low_2 = uncertainty_coeff_crit_2 + qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se_2
      uncertainty_coeff_crit_upp_2 = uncertainty_coeff_crit_2 - qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se_2
      ## Yule s Y (Coefficient of colligation)
      yule_y = (sqrt(TP*TN)-sqrt(FN*FP))/(sqrt(TP*TN)+sqrt(FN*FP))
      ## Pearson ki-kare
      pearson_chi_squ = toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)
      pearson_chi_squ_p = dchi(pearson_chi_squ, df=1)
      ## 
      with_yate_cor_for_pearson = toplam*(abs(TP*TN-FN*FP)-0.5*toplam)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)
      with_yate_cor_p_for_pearson = dchi(with_yate_cor_for_pearson, df=1)
      ## Mantel Haenszel chi-square
      ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
      mantel_haenszel = ((toplam-1)*(TP*TN-FN*FP)^2)/(satirtop1*satirtop2*sutuntop1*sutuntop2)
      mantel_haenszel_p = dchi(mantel_haenszel, df=1)
      ## Olasilik orani
      likelih_ratio = 2*(TP*log(TP/(sutuntop1*satirtop1/toplam))+FN*log(FN/(sutuntop2*satirtop1/toplam))+FP*log(FP/(sutuntop1*satirtop2/toplam))+TN*log(TN/(sutuntop2*satirtop2/toplam)))
      likelih_ratio_p =dchi(likelih_ratio, df=1)
      ## Fisher'in exact testi
      ##fisher_exact_test = (TP-(satirtop1*sutuntop1/toplam))^2/(satirtop1*sutuntop1/toplam) + (FN-(satirtop1*sutuntop2/toplam))^2/(satirtop1*sutuntop2/toplam) + (FP-(satirtop2*sutuntop1/toplam))^2/(satirtop2*sutuntop1/toplam) + (TN-(satirtop2*sutuntop2/toplam))^2/(satirtop2*sutuntop2/toplam)
      fisher <- c(TP, FN, FP, TN)
      tab <- t(matrix(fisher, nrow=2,ncol=2))
      fisher_exact_test = fisher.test(tab)$p.value
      minimum_exp_fre = min((sutuntop1*satirtop1/toplam):(sutuntop2*satirtop2/toplam))
      cells_exp_fre_5 = (if((sutuntop1*satirtop1/toplam)<5) 1 else 0)+ (if((sutuntop1*satirtop2/toplam)<5) 1 else 0)+ (if((sutuntop2*satirtop1/toplam) <5) 1 else 0)+ (if((sutuntop2*satirtop2/toplam) <5) 1 else 0)
      cells_exp_fre_1 = (if((sutuntop1*satirtop1/toplam) <1) 1 else 0)+ (if((sutuntop1*satirtop2/toplam) <1) 1 else 0)+ (if((sutuntop2*satirtop1/toplam) <1) 1 else 0)+ (if((sutuntop2*satirtop2/toplam) <1) 1 else 0)
      ## Mc Nemar testi
      mcNemar_test = (FP-FN)^2/(FP+FN)
      mcNemar_test_p = dchi(mcNemar_test, df=1)
      with_Yate_cor_for_mcnamer = (abs(FP-FN)-1)^2/(FP+FN)
      with_Yate_cor_p_for_mcnamer = dchi(with_Yate_cor_for_mcnamer, df=1)
      
      ## Belirsizlik (Entropi)
      forbes_NMI = sensitivity_estimate*predvalue_positiverandomtest*log((sensitivity_estimate/predvalue_positiverandomtest_mar_ol),base=2)+
        (1-specificity)*(1-predvalue_positiverandomtest)*log(((1-specificity)/predvalue_positiverandomtest_mar_ol),base=2)+
        (1-sensitivity_estimate)*predvalue_positiverandomtest*log(((1-sensitivity_estimate)/(1-predvalue_positiverandomtest_mar_ol)),base=2)+
        specificity*(1-predvalue_positiverandomtest)*log((specificity/(1-predvalue_positiverandomtest_mar_ol)),base=2)
      ## satir icin entropy (test icin)
      entropy_hr = - ((sutuntop1/toplam) * log(sutuntop1/toplam, base=2) + (sutuntop2/toplam) * log(sutuntop2/toplam, base=2))
      ## sutun icin entropy (hastalik icin)  
      entropy_hc = - ((satirtop1/toplam)*log(satirtop1/toplam, base=2) + (satirtop2/toplam)*log(satirtop2/toplam, base=2))
      ## birlesik entropi (joint entropy)  
      entropy_hrc = - ( (TP/toplam)*log(TP/toplam, base=2) + (FP/toplam)*log(FP/toplam, base=2) + (FN/toplam)*log(FN/toplam, base=2) + (TN/toplam)*log(TN/toplam, base=2))
      ## bilgi icerigi (mutual information)  
      information_r_c = entropy_hr + entropy_hc - entropy_hrc
      ## kosullu entropi (conditional entropy)  
      a = entropy_hrc - entropy_hr
      c = entropy_hrc - entropy_hc
      sim_r_c = information_r_c / (a + information_r_c + c)
      dis_r_c = (a + c) / (a + information_r_c + c)
      ## goreli entropi (relative entropy, kullback-leibler uzakligi)
      ## pozitif test sonucu icin goreli entropi, Relative Improvement Over Chance (RIOC)
      positive_relative_entropy = (TP/sutuntop1)*log(((TP/sutuntop1)/ prevalence), base=2)+(FP/sutuntop1)*log(((FP/sutuntop1)/(1- prevalence)), base=2)
      ## negatif test sonucu icin goreli entropi 
      negative_relative_entropy = (FN/sutuntop2)*log(((FN/sutuntop2)/ prevalence), base=2)+(TN/sutuntop2)*log(((TN/sutuntop2)/(1- prevalence)), base=2)
      
     
      verim <- data.frame(Estimate = format(round(dice_index, digit =3),nsmall=3), EstimatePercent = percent(round(dice_index, digit =3)), LowerCI = format(round(dice_index_low, digit =3),nsmall=3), UpperCI = format(round(dice_index_upp, digit =3),nsmall=3))
      newRow <- data.frame(Estimate = format(round(yules_q, digit =3),nsmall=3), EstimatePercent = percent(round(yules_q, digit =3)), LowerCI = format(round(yules_q_low, digit =3),nsmall=3), UpperCI = format(round(yules_q_upp, digit =3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(cramer_v, digit =3),nsmall=3), EstimatePercent =percent(round(cramer_v, digit =3)) ,LowerCI = format(round(yules_q_low, digit =3),nsmall=3), UpperCI = format(round(yules_q_upp, digit =3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(equitable_threatscore, digit =3),nsmall=3),EstimatePercent = percent(round(equitable_threatscore, digit =3)), LowerCI = 0, UpperCI = 0)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(phi, digit =3),nsmall=3),EstimatePercent = percent(round(phi, digit =3)),  LowerCI = format(round(phi_low, digit =3),nsmall=3), UpperCI = format(round(phi_upp, digit =3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(contingency_coefficient, digit =3),nsmall=3), EstimatePercent = percent(round(contingency_coefficient, digit =3)), LowerCI = NA, UpperCI = NA)
      ## ?????? Adjusted Contingency Coefficient
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(goodman_and_kruskal_gamma, digit =3),nsmall=3),EstimatePercent = percent(round(goodman_and_kruskal_gamma, digit =3)), LowerCI = format(round(goodman_and_kruskal_gamma_low, digit =3),nsmall=3), UpperCI = format(round(goodman_and_kruskal_gamma_upp, digit =3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(kendalls_tau_a, digit =3),nsmall=3),EstimatePercent = percent(round(kendalls_tau_a, digit =3)), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(kendalls_tau_b, digit =3),nsmall=3),EstimatePercent = percent(round(kendalls_tau_b, digit =3)), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(kendalls_tau_c, digit =3),nsmall=3),EstimatePercent = percent(round(kendalls_tau_c, digit =3)), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(somers_d, digit =3),nsmall=3),EstimatePercent = percent(round(somers_d, digit =3)), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(somers_d_2, digit =3),nsmall=3),EstimatePercent = percent(round(somers_d_2, digit =3)), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(scotts_agreement_index, digit =3),nsmall=3),EstimatePercent = percent(round(scotts_agreement_index, digit =3)), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(tetrachoric_correlation, digit =3),nsmall=3),EstimatePercent = percent(round(tetrachoric_correlation, digit =3)),  LowerCI = format(round(tetrachoric_correlation_low, digit =3),nsmall=3), UpperCI = format(round(tetrachoric_correlation_upp, digit =3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(goodman_kruskal_tau, digit =3),nsmall=3),EstimatePercent = percent(round(goodman_kruskal_tau, digit =3)),  LowerCI = format(round(goodman_kruskal_tau_low, digit =3),nsmall=3), UpperCI = format(round(goodman_kruskal_tau_upp, digit =3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(lambda_symmetric, digit =3),nsmall=3),EstimatePercent = percent(round(lambda_symmetric, digit =3)) ,  LowerCI = format(round(lambda_symmetric_low, digit =3),nsmall=3), UpperCI = format(round(lambda_symmetric_upp, digit =3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(lambda_criterion, digit =3),nsmall=3),EstimatePercent = percent(round(lambda_criterion, digit =3)),  LowerCI = format(round(lambda_criterion_low, digit =3),nsmall=3), UpperCI = format(round(lambda_criterion_upp, digit =3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(lambda_criterion_2, digit =3),nsmall=3),EstimatePercent = percent(round(lambda_criterion_2, digit =3)),  LowerCI = format(round(lambda_criterion_low_2, digit =3),nsmall=3), UpperCI = format(round(lambda_criterion_upp_2, digit =3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(uncertainty_coefficient_symmetric, digit =3),nsmall=3),EstimatePercent = percent(round(uncertainty_coefficient_symmetric, digit =3)),  LowerCI = format(round(uncertainty_coefficient_symmetric_low, digit =3),nsmall=3), UpperCI = format(round(uncertainty_coefficient_symmetric_upp, digit =3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(uncertainty_coeff_crit, digit =3),nsmall=3),EstimatePercent = percent(round(uncertainty_coeff_crit, digit =3)),  LowerCI = format(round(uncertainty_coeff_crit_low, digit =3),nsmall=3), UpperCI = format(round(uncertainty_coeff_crit_upp, digit =3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(uncertainty_coeff_crit_2, digit =3),nsmall=3),EstimatePercent = percent(round(uncertainty_coeff_crit_2, digit =3)),  LowerCI = format(round(uncertainty_coeff_crit_low_2, digit =3),nsmall=3), UpperCI = format(round(uncertainty_coeff_crit_upp_2, digit =3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(yule_y, digit =3),nsmall=3),EstimatePercent = percent(round(yule_y, digit =3)),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      
      rownames(verim) = c("Dice\'s Index (Czekanowski-Sorenson)", "Yules Q (Gamma)", "Cramer\'s V","Equitable Threatscore (Gilbert Skill Score)", "Phi", "Contingency Coefficient", 
                          "Goodman & Kruskal Gamma (Gamma Statistic-Gamma Coefficient)","Kendall\'s Tau-a" ,"Kendall\'s Tau-b" ,"Kendall\'s (Stuart\'s) Tau-c" , 
                          "Somers\' Delta (Somers\' d) R|C", "Somers\' Delta (Somers\' d) C|R",
                          "Scotts agreement index", "Tetrachoric (Polychoric) Correlation", "Goodman & Kruskals tau (Crit. dep.)", 
                          "Lambda Symmetric", "Lambda Asymmetric R|C","Lambda Asymmetric C|R" ,"Uncertainty Coefficient (Theil\'s U) Symmetric ", 
                          "Uncertainty Coefficient (Theil\'s U) R|C ", "Uncertainty Coefficient (Theil\'s U) C|R", "Yule\'s Y (Coefficient of Colligation)")
      
      
      return(verim[,c(1,2,3,4)])

    }
    })
  

  my_data1 <- reactive({
    testdata <- dataM1()
    as.data.frame(cbind(Interpretation = shinyInput(actionButton, nrow(testdata),'button1_', label = "Interpretation", onclick = 'Shiny.onInputChange(\"select_button1\",  this.id)' ),testdata))
  })  
  output$dataUpload1 <- DT::renderDataTable(my_data1(),selection = 'single', extensions = c('Buttons','KeyTable', 'Responsive'),
                                            options = list(pageLength = 25,lengthMenu = c(25, 50, 75, 100),
                                                           dom = 'Bfrtip',
                                                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),keys = TRUE,
                                                           preDrawCallback = JS("function() { 
                                        Shiny.unbindAll(this.api().table().node()); }"), 
                                                                              drawCallback = JS("function() { 
                                                                                                Shiny.bindAll(this.api().table().node()); } ")),
                                            server = FALSE, escape = FALSE)
  
  
  # Here I created a reactive to save which row was clicked which can be stored for further analysis
  SelectedRow1 <- eventReactive(input$select_button1,{
    as.numeric(strsplit(input$select_button1, "_")[[1]][2])
  })
  
  # This is needed so that the button is clicked once for modal to show, a bug reported here
  # https://github.com/ebailey78/shinyBS/issues/57
  observeEvent(input$select_button1, {
    toggleModal(session, "modalExample1", "open")
  })
  
  DataRow1 <- eventReactive(input$select_button1,{
    my_data1()[SelectedRow1(),2:ncol(my_data1())]
  })
  
  output$popup1 <- renderUI({
    if (row.names(DataRow1()) == 'Dice\'s Index (Czekanowski-Sorenson)') 
    {
      degdice = ' '
      if (as.numeric(as.character(DataRow1()$Estimate)) <= 0.5 & as.numeric(as.character(DataRow1()$Estimate)) >= 0)
      {degdice = 'closer to 0, so it is not a very good score.'
      }
      else 
      {degdice = 'closer to 1, so it is a very good score.'
      }
      ## file:///C:/Users/User/Downloads/chapter022-small.pdf
      bsModal("modalExample1", paste0("Dice s Index"), "", size = "large",
              h4("Description:"),
              print(paste0("The Dice score is often used to quantify the performance of image segmentation methods. 
                           There you annotate some ground truth region in your image and then make an automated algorithm to do it. 
                           You validate the algorithm by calculating the Dice score, which is a measure of how similar the objects are. 
                           So it is the size of the overlap of the two segmentations divided by the total size of the two objects.
                           The Dice index is based on the harmonic mean.
                           Dice (1945) proposed an index which is the conditional probability that one (of two)
                           randomly chosen raters classifies an item as positive given that the other rater classified
                           the item as positive. The Dice and Jaccard coefficients are the most commonly used
                           measures of spatial overlap for binary labels. In both cases, the values for the
                           coefficients range from zero (no overlap) to one (perfect agreement). This is also sometimes known as the relative overlap measure. As all these
                           measures are related to each other, typically only one or the other is calculated. The Dice coefficient has been shown to be a special case of the kappa coef-
                           ficient, a measure commonly used to evaluate inter-observer agreement.
                           As defined, both of these measures are symmetric, in that over- or undersegmentation
                           errors are weighted equally.
                           ")),
              h4("Interpretation:"),
              h5("Dice\'s index  is ", DataRow1()$Estimate, " and ", degdice ),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow1()),
              column(12,                   
                     DT::renderDataTable(DataRow1())
                     
              )
              
              
              )}
      
    else if (row.names(DataRow1()) == 'Yules Q (Gamma)') 
    {
      ## https://www.angelo.edu/faculty/ljones/gov3301/block14/objective3.htm
      degyule = ' '
      if ((as.numeric(as.character(DataRow1()$Estimate))<=1) & (as.numeric(as.character(DataRow1()$Estimate))>=0.70))
      {degyule = 'there is very strong positive relationship.'
      }
      else if ((as.numeric(as.character(DataRow1()$Estimate)) <= 0.69) & (as.numeric(as.character(DataRow1()$Estimate)) >= 0.50))
      {degyule = 'there is substantial positive relationship.'
      }
      else if ((as.numeric(as.character(DataRow1()$Estimate)) <= 0.49) & (as.numeric(as.character(DataRow1()$Estimate)) >= 0.30))
      {degyule = 'there is moderate positive relationship.'
      }
      else if ((as.numeric(as.character(DataRow1()$Estimate)) <= 0.29) & (as.numeric(as.character(DataRow1()$Estimate)) >= 0.10))
      {degyule = 'there is low positive relationship.'
      }
      else if ((as.numeric(as.character(DataRow1()$Estimate)) <= 0.09) & (as.numeric(as.character(DataRow1()$Estimate)) >= 0.01))
      {degyule = 'there is negligible positive relationship.'
      }
      else if ((as.numeric(as.character(DataRow1()$Estimate)) == 0))
      {degyule = 'there is no relationship.'
      }
      else if ((as.numeric(as.character(DataRow1()$Estimate)) >= -0.09) & (as.numeric(as.character(DataRow1()$Estimate)) <= -0.01))
      {degyule = 'there is negligible negative relationship.'
      }
      else if ((as.numeric(as.character(DataRow1()$Estimate)) >= -0.29) & (as.numeric(as.character(DataRow1()$Estimate)) <= -0.10))
      {degyule = 'there is low negative relationship.'
      }
      else if ((as.numeric(as.character(DataRow1()$Estimate)) >= -0.49) & (as.numeric(as.character(DataRow1()$Estimate)) <= -0.30))
      {degyule = 'there is moderate negative relationship.'
      }
      else if ((as.numeric(as.character(DataRow1()$Estimate)) >= -0.69) & (as.numeric(as.character(DataRow1()$Estimate)) <= -0.50))
      {degyule = 'there is substantial negative relationship.'
      }
      if ((as.numeric(as.character(DataRow1()$Estimate)) >= -1) & (as.numeric(as.character(DataRow1()$Estimate)) <= -0.70))
      {degyule = 'there is very strong negative relationship.'
      }
      ## file:///C:/Users/User/Downloads/chapter022-small.pdf
      ## http://maxwellsci.com/print/rjms/v3-82-90.pdf
      bsModal("modalExample1", paste0("Yule's Q"), "", size = "large",
              h4("Description:"),
              print(paste0("This index is a special case of Goodman and Kruskal's gamma. It is a function of the cross-ratio and is independent of the marginal totals. It has a range of -1 to 1.
The Yule s Q is a nominal level measure of
                           association that could be used to determine the association
                           or relationship between variables (Baddie and Fred, 1995;
                           Kolawole, 2001). Yule originated this measure of
                           association for variables which have two and only two
                           values. It is used with 2 x 2 tables, each variable being
                           expressed as a dichotomy. Yule s Q is equivalent to Goodman and Kruskal s Gamma. Yule s Q appropriate only for 2x2 tables (2 rows, 2 columns).
                           Yule s Q  captures that in a measure 0 (no association) and   -1, +1 (strong association).
                           Rule of Thumb for interpreting Yule s Q: 0 to 0.24 (virtually no relationship), 0.25 to 0.49 (weak relationship),
                           0.50 to 0.74 (moderate relationship), 0.75 to 1 (strong relationship).
                           ")),
              h4("Interpretation:"),
              h5("Yule s Q coefficient is ", DataRow1()$Estimate, " and ", degyule ),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow1()),
              column(12,                   
                     DT::renderDataTable(DataRow1())
                     
              )
              
              
              )}

    else if (row.names(DataRow1()) == 'Equitable Threatscore (Gilbert Skill Score)') 
    {
      degequi = ' '
      if ((as.numeric(as.character(DataRow1()$Estimate)) <= 0.5) & (as.numeric(as.character(DataRow1()$Estimate)) >= 0))
      {degequi = 'closer to 0, so it is not a very good score.'
      }
      else 
      {degequi = 'closer to 1, so it is a very good score.'
      }
      ## http://www.bom.gov.au/water/newEvents/presentations/waterbriefing2011/hobart/water_forecasting_dj.pdf
      bsModal("modalExample1", paste0("Equitable Threatscore"), "", size = "large",
              h4("Description:"),
              print(paste0("Measures the fraction of observed and/or forecast events that were correctly predicted, adjusted
for hits associated with random chance (for example, it is easier to correctly forecast rain occurrence in a wet
                           climate than in a dry climate). The ETS is often used in the verification of rainfall in NWP models because its
                           equitability allows scores to be compared more fairly across different regimes. Sensitive to hits. Because it
                           penalises both misses and false alarms in the same way, it does not distinguish the source of forecast error. 
                           Range is -1/3 to 1, 0 indicates no skill. Perfect score is 1.
                           ")),
              h4("Interpretation:"),
              h5("Equitable Threatscore is ", DataRow1()$Estimate, " and ", degequi ),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow1()),
              column(12,                   
                     DT::renderDataTable(DataRow1())
                     
              )
              
              
              )}

    else if (row.names(DataRow1()) == 'Phi') 
    {
      ## https://www.angelo.edu/faculty/ljones/gov3301/block14/objective3.htm
      degphi = ' '
      if ((as.numeric(as.character(DataRow1()$Estimate))<=1) & (as.numeric(as.character(DataRow1()$Estimate))>=0.70))
      {degphi = 'there is very strong positive relationship.'
      }
      else if ((as.numeric(as.character(DataRow1()$Estimate)) <= 0.69) & (as.numeric(as.character(DataRow1()$Estimate)) >= 0.50))
      {degphi = 'there is substantial positive relationship.'
      }
      else if ((as.numeric(as.character(DataRow1()$Estimate)) <= 0.49) & (as.numeric(as.character(DataRow1()$Estimate)) >= 0.30))
      {degphi = 'there is moderate positive relationship.'
      }
      else if ((as.numeric(as.character(DataRow1()$Estimate)) <= 0.29) & (as.numeric(as.character(DataRow1()$Estimate)) >= 0.10))
      {degphi = 'there is low positive relationship.'
      }
      else if ((as.numeric(as.character(DataRow1()$Estimate)) <= 0.09) & (as.numeric(as.character(DataRow1()$Estimate)) >= 0.01))
      {degphi = 'there is negligible positive relationship.'
      }
      else if ((as.numeric(as.character(DataRow1()$Estimate)) == 0))
      {degphi = 'there is no relationship.'
      }
      else if ((as.numeric(as.character(DataRow1()$Estimate)) >= -0.09) & (as.numeric(as.character(DataRow1()$Estimate)) <= -0.01))
      {degphi = 'there is negligible negative relationship.'
      }
      else if ((as.numeric(as.character(DataRow1()$Estimate)) >= -0.29) & (as.numeric(as.character(DataRow1()$Estimate)) <= -0.10))
      {degphi = 'there is low negative relationship.'
      }
      else if ((as.numeric(as.character(DataRow1()$Estimate)) >= -0.49) & (as.numeric(as.character(DataRow1()$Estimate)) <= -0.30))
      {degphi = 'there is moderate negative relationship.'
      }
      else if ((as.numeric(as.character(DataRow1()$Estimate)) >= -0.69) & (as.numeric(as.character(DataRow1()$Estimate)) <= -0.50))
      {degphi = 'there is substantial negative relationship.'
      }
      if ((as.numeric(as.character(DataRow1()$Estimate)) >= -1) & (as.numeric(as.character(DataRow1()$Estimate)) <= -0.70))
      {degphi = 'there is very strong negative relationship.'
      }

      ## file:///C:/Users/User/Downloads/chapter022-small.pdf
      bsModal("modalExample1", paste0("Phi"), "", size = "large",
              h4("Description:"),
              print(paste0("Phi is a chi-square-based measure of association that involves dividing the chi-square statistic 
by the sample size and taking the square root of the result.
                           The Phi Coefficient is nominal measure of association for nominal variables
                           (Kolawole, 2001). It measures the degree of association
                           between two variables that are expressed as a dichotomy. Phi is very similar to Yule s Q.It is only for 2x2 table, 
                           ranges from -1 to 1, 0 (no assoc.). Close to 0 it shows little association between variables. Close to 1, it indicates a strong positive association. 
                           Close to -1 it shows a strong negative correlation.
                           ")),
              h4("Interpretation:"),
              h5("Phi coefficient is ", DataRow1()$Estimate, " and ", degphi ),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow1()),
              column(12,                   
                     DT::renderDataTable(DataRow1())
                     
              )
              
              
              )}
 
    else if (row.names(DataRow1()) == 'Contingency Coefficient') 
    {
      ## http://www.statisticssolutions.com/nominal-variable-association/
      degcont = ' '
      if ((as.numeric(as.character(DataRow1()$Estimate)) >= 0.31) )
      {degcont = 'there is strong relationship.'
      }
      else if ((as.numeric(as.character(DataRow1()$Estimate)) >= 0.11) & (as.numeric(as.character(DataRow1()$Estimate)) <= 0.30))
      {degcont = 'there is moderate relationship.'
      }
      else if ((as.numeric(as.character(DataRow1()$Estimate)) <= 0.10))
      {degcont = 'there is weak relationship.'
      }
      ## file:///C:/Users/User/Downloads/chapter022-small.pdf
      bsModal("modalExample1", paste0("Contingency coefficient"), "", size = "large",
              h4("Description:"),
              print(paste0("Contingency coefficient is another chi square based measure of association, and one that also
adjusts for different sample sizes. When there is no relationship between two variables, Contingency coefficient = 0.
                           The contingency coefficient cannot exceed the value Contingency coefficient = 1. The contingency coefficient may be less than
                           1 even when two variables are perfectly related to each other. This means that it is not as desirable a measure of association as 
                           those which have the range 0 to 1. The maximum value possible depends on the number of rows and columns in a table.
                           ")),
              h4("Interpretation:"),
              h5("Contingency coefficient is ", DataRow1()$Estimate, " and ", degcont ),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow1()),
              column(12,                   
                     DT::renderDataTable(DataRow1())
                     
              )
              
              
              )}
 
    else if (row.names(DataRow1()) == 'Goodman & Kruskal Gamma (Gamma Statistic-Gamma Coefficient)') 
    {
      ## https://us.sagepub.com/sites/default/files/upm-binaries/67534_Gau_Chapter_10.pdf
      degkrus = ' '
      if (as.numeric(as.character(DataRow1()$Estimate)) == 1)
      {degkrus = 'there is perfect positive correlation.'
      }
      else if (as.numeric(as.character(DataRow1()$Estimate)) == -1)
      {degkrus = 'there is perfect inverse  correlation.'
      }
      else if (as.numeric(as.character(DataRow1()$Estimate)) == 0)
      {degkrus = 'there is no relationship.'
      }
      else if ((as.numeric(as.character(DataRow1()$Estimate)) > 0) & (as.numeric(as.character(DataRow1()$Estimate)) <= 0.19))
      {degkrus = 'there is weak relationship.'
      }
      else if ((as.numeric(as.character(DataRow1()$Estimate)) >= 0.20) & (as.numeric(as.character(DataRow1()$Estimate)) <= 0.39))
      {degkrus = 'there is moderate relationship.'
      }
      else if ((as.numeric(as.character(DataRow1()$Estimate)) >= 0.40) & (as.numeric(as.character(DataRow1()$Estimate)) <= 0.59))
      {degkrus = 'there is strong relationship.'
      }
      else if ((as.numeric(as.character(DataRow1()$Estimate)) >= 0.60) & (as.numeric(as.character(DataRow1()$Estimate)) < 1))
      {degkrus = 'there is very strong relationship.'
      }
      ## file:///C:/Users/User/Downloads/chapter022-small.pdf
      bsModal("modalExample1", paste0("Goodman and Kruskal Gamma"), "", size = "large",
              h4("Description:"),
              print(paste0("The Goodman s and Kruskal s Gamma g is an
ordinal measure of association between two variables
                           (Adeyemi, 1998). It measures the degree of agreement or
                           association between two ordinal-level data. Gamma is P(concordant) - P(discordant), ignoring comparisons that are tied.
Unfortunately, this often excludes a number of pairwise comparisons. Gamma g can
                           be analyzed in two ways. The first is when no ties occur
                           in the rankings while the second way is when there are
                           ties in the rankings (Aghenta, 2000).  A symmetric measure of association between two ordinal variables that ranges between -1 and 1. 
                           Values close to an absolute value of 1 indicate a strong relationship between the two variables. 
                           Values close to 0 indicate little or no relationship. For 2-way tables, zero-order gammas are displayed. 
                           For 3-way to n-way tables, conditional gammas are displayed.
                           In 2x2 tables, gamma equals yule\'s q.")),
              h4("Interpretation:"),
              h5("Goodman and Kruskal Gamma is ", DataRow1()$Estimate, " and ", degkrus ),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow1()),
              column(12,                   
                     DT::renderDataTable(DataRow1())
                     
              )
              
              
              )}

    else if (row.names(DataRow1()) == 'Kendall\'s Tau-a') 
    {
## http://lori.academicdirect.org/works/?f=84
      bsModal("modalExample1", paste0("Kendall's Tau-a"), "", size = "large",
              h4("Description:"),
              print(paste0("Kendall-tau is a non-parametric correlation coefficient that can be used to assess and
test correlations between non-interval scaled ordinal variables. 
                           The Kendall tau correlation coefficient is considered to be equivalent to the Spearman
                           rank correlation coefficient. While Spearman rank correlation coefficient is like the Pearson
                           correlation coefficient but computed from ranks, the Kendall tau correlation rather represents
                           a probability. Tau defines error as the misclassification of a case, and cases are classified into
category j with probability equal to the observed frequency of category j. 
                           There are three Kendall\'s tau correlation coefficient known as tau-a, tau-b, and tau-c. If the agreement between the two rankings is perfect and the two rankings are the same, the
coefficient has value 1.If the disagreement between the two rankings is perfect and one ranking is the reverse of
                           the other, the coefficient has value -1.For all other arrangements the value lies between -1 and 1, and increasing values imply
                           increasing agreement between the rankings.If the rankings are independent, the coefficient has value 0. ")),
              h4("Interpretation:"),
              h5(" "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow1()),
              column(12,                   
                     DT::renderDataTable(DataRow1())
                     
              )
              
              
      )}
 
    else if (row.names(DataRow1()) == 'Kendall\'s Tau-b') 
    {
      ## http://lori.academicdirect.org/works/?f=84
      bsModal("modalExample1", paste0("Kendall's Tau-b"), "", size = "large",
              h4("Description:"),
              print(paste0("Kendall-tau is a non-parametric correlation coefficient that can be used to assess and
                           test correlations between non-interval scaled ordinal variables. 
                           The Kendall tau correlation coefficient is considered to be equivalent to the Spearman
                           rank correlation coefficient. While Spearman rank correlation coefficient is like the Pearson
                           correlation coefficient but computed from ranks, the Kendall tau correlation rather represents
                           a probability. Tau defines error as the misclassification of a case, and cases are classified into
category j with probability equal to the observed frequency of category j. 
                           There are three Kendall\'s tau correlation coefficient known as tau-a, tau-b, and tau-c. If the agreement between the two rankings is perfect and the two rankings are the same, the
                           coefficient has value 1.If the disagreement between the two rankings is perfect and one ranking is the reverse of
                           the other, the coefficient has value -1.For all other arrangements the value lies between -1 and 1, and increasing values imply
                           increasing agreement between the rankings.If the rankings are independent, the coefficient has value 0. 
Kendall tau-b is a nonparametric measure of correlation that take ties into account. Tau-b also accounts for comparisons tied on one variable but does not have a
simple interpretation. It decreases with respect to gamma as the number of
                           comparisons tied on one variable increases. 
                           ")),
              h4("Interpretation:"),
              h5(" "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow1()),
              column(12,                   
                     DT::renderDataTable(DataRow1())
                     
              )
              
              
      )}

    else if (row.names(DataRow1()) == 'Kendall\'s (Stuart\'s) Tau-c') 
    {
      ## http://lori.academicdirect.org/works/?f=84
      bsModal("modalExample1", paste0("Kendall's Tau-c"), "", size = "large",
              h4("Description:"),
              print(paste0("Kendall-tau is a non-parametric correlation coefficient that can be used to assess and
                           test correlations between non-interval scaled ordinal variables. 
                           The Kendall tau correlation coefficient is considered to be equivalent to the Spearman
                           rank correlation coefficient. While Spearman rank correlation coefficient is like the Pearson
                           correlation coefficient but computed from ranks, the Kendall tau correlation rather represents
                           a probability. Tau defines error as the misclassification of a case, and cases are classified into
category j with probability equal to the observed frequency of category j. 
                           There are three Kendall\'s tau correlation coefficient known as tau-a, tau-b, and tau-c. If the agreement between the two rankings is perfect and the two rankings are the same, the
                           coefficient has value 1.If the disagreement between the two rankings is perfect and one ranking is the reverse of
                           the other, the coefficient has value -1.For all other arrangements the value lies between -1 and 1, and increasing values imply
                           increasing agreement between the rankings.If the rankings are independent, the coefficient has value 0. 
                           Kendall tau-c is a nonparametric measure of correlation that ignores ties. Tau-c also accounts for comparisons tied on one variable and does not have a
simple interpretation. Tau-c may be preferable to tau-b when the numbers of
categories in the row and column variables are not equal. 
 ")),
              h4("Interpretation:"),
              h5(" "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow1()),
              column(12,                   
                     DT::renderDataTable(DataRow1())
                     
              )
              
              
      )}

    else if (row.names(DataRow1()) == 'Somers\' Delta (Somers\' d) R|C') 
    {
      ## http://lori.academicdirect.org/works/?f=84
      ## http://www.schwarzpartners.ch/Applied_Data_Analysis/Lecture%2005_EN_2014%20Contingency%20Analysis.pdf
      bsModal("modalExample1", paste0("Somers' D R|C"), "", size = "large",
              h4("Description:"),
              print(paste0("Somers' D is an asymmetric extension of gamma that differs only in the inclusion of the number of pairs
not tied on the independent variable. Somer's d is a modification of gamma that accounts for comparisons that are tied on
one variable. Somer's d decreases with respect to gamma as the number of
                           comparisons tied on one variable increases. 
                           ")),
              h4("Interpretation:"),
              h5("There is a ", DataRow1()$EstimatePercent ," reduction in misclassification in the test, given the real situation. "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow1()),
              column(12,                   
                     DT::renderDataTable(DataRow1())
                     
              )
              
              
      )}

    else if (row.names(DataRow1()) == 'Somers\' Delta (Somers\' d) C|R') 
    {
      ## http://lori.academicdirect.org/works/?f=84
      bsModal("modalExample1", paste0("Somers' D C|R"), "", size = "large",
              h4("Description:"),
              print(paste0("Somers' D is an asymmetric extension of gamma that differs only in the inclusion of the number of pairs
not tied on the independent variable. Somer's d is a modification of gamma that accounts for comparisons that are tied on
                           one variable. Somer's d decreases with respect to gamma as the number of
                           comparisons tied on one variable increases. ")),
              h4("Interpretation:"),
              h5("There is a ", DataRow1()$EstimatePercent ," reduction in the misclassification in the real case when the test case is taken into consideration. "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow1()),
              column(12,                   
                     DT::renderDataTable(DataRow1())
                     
              )
              
              
      )}

    else if (row.names(DataRow1()) == 'Tetrachoric (Polychoric) Correlation') 
    {
      degtetra = ' '
      if (as.numeric(DataRow1()$Estimate) <= 0.5 & as.numeric(DataRow1()$Estimate) >= 0)
      {degtetra = 'closer to 0, so it is not a very good score.'
      }
      else 
      {degtetra = 'closer to 1, so it is a very good score.'
      }
      ## http://www.statisticshowto.com/tetrachoric-correlation/
      ## file:///C:/Users/User/Downloads/v43n1a15.pdf
      ## http://geofizika-journal.gfz.hr/vol_23/No1/juras.pdf
      bsModal("modalExample1", paste0("Tetrachoric Correlation Coefficient"), "", size = "large",
              h4("Description:"),
              print(paste0("The tetrachoric correlation coefficient (Pearson, 1900)
estimates the relationship between two dichotomous
                           variables assuming an underlying bivariate normal
                           distribution. An example of such variables is a pair of
                           True/False test items in an achievement test. The tetrachoric
                           coefficient is potentially applicable to many situations and
                           plays a key role in some important analysis, such as the
                           Factor Analysis of binary items or the inter-rater agreement
                           measurement. A measure of association that indicates the proportional reduction in error when values of one variable are used to predict values of the 
                           other variable. This measure
relies on two assumptions: 1) there exist continuous latent variables underlying
                           the contingency table and 2) joint distribution of corresponding standard
                           normal deviates is bivariate normal. It is shown that, in practice, the
                           tetrachoric (polychoric) correlation coefficient is an estimate of Pearson correlation
                           coefficient between the latent variables. Consequently, these measures
                           do not depend on bias nor on marginal frequencies of the table, which
                           implies a natural and convenient partition of information (carried by the
                           contingency table), between association, bias and probability of the event and
                           subsequently enables the analysis of how other scores depend on bias and
                           marginal frequencies.", DataRow1()$Estimate, " is ", degtetra)),
              h4("Interpretation:"),
              h5("A value of ", DataRow1()$Estimate ,"indicates that knowledge of one variable reduces error in predicting values of the other variable 
                           by ", DataRow1()$EstimatePercent, "."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow1()),
              column(12,                   
                     DT::renderDataTable(DataRow1())
                     
              )
              
              
      )}

    else if (row.names(DataRow1()) == 'Lambda Symmetric') 
    {
     ## https://arifkamarbafadal.files.wordpress.com/2011/09/ebook-045-tutorial-spss-cross-tabulations.pdf
      bsModal("modalExample1", paste0("Lambda Symmetric"), "", size = "large",
              h4("Description:"),
              print(paste0("Lambda defines error as the misclassification of cases, and cases are classified
according to the modal (most frequent) category. 
                           ")),
              h4("Interpretation:"),
              h5(" "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow1()),
              column(12,                   
                     DT::renderDataTable(DataRow1())
                     
              )
              
              
      )}

    else if (row.names(DataRow1()) == 'Lambda Asymmetric R|C') 
    {
      ## https://arifkamarbafadal.files.wordpress.com/2011/09/ebook-045-tutorial-spss-cross-tabulations.pdf
      bsModal("modalExample1", paste0("Lambda Asymmetric R|C"), "", size = "large",
              h4("Description:"),
              print(paste0("Lambda defines error as the misclassification of cases, and cases are classified
according to the modal (most frequent) category. 
                           ")),
              h4("Interpretation:"),
              h5(" "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow1()),
              column(12,                   
                     DT::renderDataTable(DataRow1())
                     
              )
              
              
      )}

    else if (row.names(DataRow1()) == 'Lambda Asymmetric C|R') 
    {
      ## https://arifkamarbafadal.files.wordpress.com/2011/09/ebook-045-tutorial-spss-cross-tabulations.pdf
      bsModal("modalExample1", paste0("Lambda Asymmetric C|R"), "", size = "large",
              h4("Description:"),
              print(paste0("Lambda defines error as the misclassification of cases, and cases are classified
                           according to the modal (most frequent) category. 
                           ")),
              h4("Interpretation:"),
              h5(" "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow1()),
              column(12,                   
                     DT::renderDataTable(DataRow1())
                     
              )
              
              
      )}

    else if (row.names(DataRow1()) == 'Uncertainty Coefficient (Theil\'s U) Symmetric ') 
    {
      ## https://arifkamarbafadal.files.wordpress.com/2011/09/ebook-045-tutorial-spss-cross-tabulations.pdf
      bsModal("modalExample1", paste0("Uncertainty Coefficient Symmetric"), "", size = "large",
              h4("Description:"),
              print(paste0("The uncertainty coefficient defines error as the entropy, or P(category j) *
ln(P(category j)) summed over the categories of the variable.  
                           ")),
              h4("Interpretation:"),
              h5(" "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow1()),
              column(12,                   
                     DT::renderDataTable(DataRow1())
                     
              )
              
              
      )}

    else if (input$select_button1 == 'Uncertainty Coefficient (Theil\'s U) R|C ') 
    {
      ## https://arifkamarbafadal.files.wordpress.com/2011/09/ebook-045-tutorial-spss-cross-tabulations.pdf
      bsModal("modalExample1", paste0("Uncertainty Coefficient Asymmetric R|C"), "", size = "large",
              h4("Description:"),
              print(paste0("The uncertainty coefficient defines error as the entropy, or P(category j) *
                           ln(P(category j)) summed over the categories of the variable.  
                           ")),
              h4("Interpretation:"),
              h5(" "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow1()),
              column(12,                   
                     DT::renderDataTable(DataRow1())
                     
              )
              
              
      )}

    else if (row.names(DataRow1()) == 'Uncertainty Coefficient (Theil\'s U) C|R') 
    {
      ## https://arifkamarbafadal.files.wordpress.com/2011/09/ebook-045-tutorial-spss-cross-tabulations.pdf
      bsModal("modalExample1", paste0("Uncertainty Coefficient Asymmetric C|R"), "", size = "large",
              h4("Description:"),
              print(paste0("The uncertainty coefficient defines error as the entropy, or P(category j) *
                           ln(P(category j)) summed over the categories of the variable.  
                           ")),
              h4("Interpretation:"),
              h5(" "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow1()),
              column(12,                   
                     DT::renderDataTable(DataRow1())
                     
              )
              
              
      )}
    else if (row.names(DataRow1()) == 'Yule\'s Y (Coefficient of Colligation)') 
    {
      ## https://arifkamarbafadal.files.wordpress.com/2011/09/ebook-045-tutorial-spss-cross-tabulations.pdf
      bsModal("modalExample1", paste0("Yule\'s Y (Coefficient of Colligation):"), "", size = "large",
              h4("Description:"),
              print(paste0("In statistics, Yule s Y, also known as the coefficient of colligation, is a measure of 
association between two binary variables. The measure was developed by George Udny Yule in 1912,and should not be confused 
with Yule s coefficient for measuring skewness based on quartiles. Yule s Y varies from -1 to +1. -1 reflects total negative 
correlation, +1 reflects perfect positive association while 0 reflects no association at all. These correspond to the values 
for the more common Pearson correlation.  
                           ")),
              h4("Interpretation:"),
              h5(" "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow1()),
              column(12,                   
                     DT::renderDataTable(DataRow1())
                     
              )
              
              
              )}
    else
    {
    bsModal("modalExample1", paste0("Interpretation: "), "", size = "large",
            print(paste0(rownames(DataRow1())," estimate is ", DataRow1()$Estimate," and its confidence interval is ", DataRow1()$LowerCI," - ",DataRow1()$UpperCI," . ")),
            h5("**************************************************************************************************************************************************"),
            h5("Data for Row Number:",SelectedRow1()),
            column(12,                   
                   DT::renderDataTable(DataRow1())
                   
            )
            
            
    )
    }
  })
  
  output$plot1 <- highcharter::renderHighchart({
    if (input$verigir_kontrol == TRUE){ 
      TP = 0
      FN = 0
      FP = 0
      TN = 0
      for (i in 1:nrow(tabloveri()))
      {
        if (tabloveri()[i,1]==1 & tabloveri()[i,2]==1)
        {TP = TP+1}
        else if (tabloveri()[i,1]==1 & tabloveri()[i,2]==2)
        {FN = FN+1}
        else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==1)
        {FP = FP+1}
        else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==2)
        {TN = TN+1}
        next
      }
      CI = as.numeric(trimws(input$text5_2))
    }
    else{
      TP = as.numeric(trimws(input$text1)) ### x
      FN = as.numeric(trimws(input$text2)) ### y
      FP = as.numeric(trimws(input$text3)) ### z
      TN = as.numeric(trimws(input$text4)) ### t
      CI = as.numeric(trimws(input$text5))
    }
    
    
    satirtop1 = TP + FN
    satirtop2 = FP + TN
    sutuntop1 = TP + FP
    sutuntop2 = FN + TN
    toplam = satirtop1 + satirtop2
    #table1 <- matrix(c(TP,FN,satirtop1,FP,TN,satirtop2,sutuntop1,sutuntop2,toplam),nrow=3,ncol=3,byrow=TRUE,dimnames=list(c("1.satir","2.satir","Sutun toplamlari"),c("1.kolon","2.kolon","Satir toplamlari")))
    #table1
    ## Duyarlilik 
    sensitivity_estimate = TP / satirtop1
    sensitivity_se = round((sqrt(sensitivity_estimate * (1 - sensitivity_estimate)/satirtop1)), 4)
    sensitivity_low = 1 - Rbeta.inv(((1 + CI/100)/2), (satirtop1+1-TP), TP)
    sensitivity_upp = Rbeta.inv(((1 + CI/100)/2), (TP+1), (satirtop1-TP))
    sensitivity_randomtest = sutuntop1 / toplam
    qualityindex_sensitivity = (sensitivity_estimate - sensitivity_randomtest) / (1 - sensitivity_randomtest)
    ## Secicilik
    specificity = TN / satirtop2
    specificity_se = sqrt(specificity * (1 - specificity) / satirtop2)
    specificity_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop2+1-TN),TN)
    specificity_upp = Rbeta.inv((1 + CI/100)/2, (TN+1), (satirtop2-TN))
    specificity_randomtest = (1 - sensitivity_randomtest)
    qualityindex_specificity = (specificity - 1 + sensitivity_randomtest) / sensitivity_randomtest
    ## Gain in Certainty
    gain_in_certainty = sensitivity_estimate + specificity
    ## Etkililik, yeterlilik ayni zamanda overall accuracy (toplam-genel dogruluk orani)
    efficiency = (TP + TN) / toplam
    efficiency_se = sqrt(efficiency * (1 - efficiency) / toplam)
    efficiency_low = 1-Rbeta.inv((1 + CI/100)/2, (toplam+1-TP-TN), (TP+TN))
    efficiency_upp = Rbeta.inv((1 + CI/100)/2, (TP+TN+1), (toplam-TP-TN))
    efficiency_randomtest = (satirtop1 / toplam) * sensitivity_randomtest + (1 - (satirtop1 / toplam)) * (1 - sensitivity_randomtest)
    mis_efficiency = 1 - efficiency
    ## Quality indeks
    quality_index = (efficiency - efficiency_randomtest) / (1 - efficiency_randomtest)
    quality_index_se = sqrt((efficiency*(1-efficiency)/(1-efficiency_randomtest)^2+2*(1-efficiency)*(2*efficiency*efficiency_randomtest-((TP*((satirtop1 / toplam)+sensitivity_randomtest)+TN*(2-(satirtop1 / toplam)-sensitivity_randomtest))/toplam))/(1-efficiency_randomtest)^3+((1-efficiency)^2)*((TP*((satirtop1 / toplam)+sensitivity_randomtest)^2+FN*(-(satirtop1 / toplam)+1+sensitivity_randomtest)^2+FP*(1+(satirtop1 / toplam)-sensitivity_randomtest)^2+TN*(2-(satirtop1 / toplam)-sensitivity_randomtest)^2)/toplam-4*efficiency_randomtest^2)/(1-efficiency_randomtest)^4)/toplam)
    quality_index_low = quality_index + qnorm((1 - CI/100)/2)*quality_index_se
    quality_index_upp = quality_index - qnorm((1 - CI/100)/2)*quality_index_se
    ## Youden indeksi
    youdens_index = sensitivity_estimate + specificity - 1
    youdens_index_se = sqrt(sensitivity_se^2+specificity_se^2)
    youdens_index_low = youdens_index + qnorm((1 - CI/100)/2)*youdens_index_se
    youdens_index_upp = youdens_index - qnorm((1 - CI/100)/2)*youdens_index_se
    
    number_needed_to_diagnose = 1 / youdens_index
    number_needed_to_misdiagnose = 1 / ( 1 - efficiency ) 
    ## Pozitif kestirim degeri
    predictivevalue_positivetest = TP / sutuntop1
    predictivevalue_positivetest_se = sqrt(predictivevalue_positivetest*(1-predictivevalue_positivetest)/sutuntop1)
    predictivevalue_positivetest_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop1+1-TP),TP)
    predictivevalue_positivetest_upp = Rbeta.inv((1 + CI/100)/2, (TP+1),(sutuntop1-TP))
    predvalue_positiverandomtest = (satirtop1 / toplam)
    predvalue_positiverandomtest_mar_ol = predvalue_positiverandomtest*sensitivity_estimate+(1-predvalue_positiverandomtest)*(1-specificity)
    ## Negatif kestirim degeri
    predictivevalue_negativetest = TN / sutuntop2
    predictivevalue_negativetest_se = sqrt(predictivevalue_negativetest*(1-predictivevalue_negativetest)/sutuntop2)
    predictivevalue_negativetest_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop2+1-TN),TN)
    predictivevalue_negativetest_upp = Rbeta.inv((1 + CI/100)/2, (TN+1), (sutuntop2-TN))
    predvalue_negativerandomtest = 1 - (satirtop1 / toplam)
    predvalue_negativerandomtest_mar_ol = 1 - predvalue_positiverandomtest_mar_ol
    ## Predictive Summary Index (PSI)
    predictive_summary_index = predictivevalue_positivetest + predictivevalue_negativetest - 1
    ## Yanlis pozitif orani
    false_positiverate = FP / satirtop2
    false_positiverate_se = sqrt(false_positiverate*(1-false_positiverate)/satirtop2)
    false_positiverate_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop2+1-FP),FP)
    false_positiverate_upp = Rbeta.inv((1 + CI/100)/2, (FP+1), (satirtop2-FP))
    ## Yansis negatif orani
    false_negativerate = FN / satirtop1
    false_negativerate_se = sqrt(false_negativerate*(1-false_negativerate)/satirtop1)
    false_negativerate_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop1+1-FN),FN)
    false_negativerate_upp = Rbeta.inv((1 + CI/100)/2, (FN+1), (satirtop1-FN))
    ## False Omission Rate (FOR) https://www.ncss.com/software/ncss/diagnostic-tests-in-ncss/
    false_omission_rate = FN/(sutuntop2)
    false_omission_rate_se = sqrt(false_omission_rate*(1-false_omission_rate)/sutuntop2)
    false_omission_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop2+1-FN),FN)
    false_omission_rate_upp = Rbeta.inv((1 + CI/100)/2, (FN+1), (sutuntop2-FN))
    ## False Discovery Rate (FDR) https://www.ncss.com/software/ncss/diagnostic-tests-in-ncss/
    false_discovery_rate = FP/(sutuntop1)
    false_discovery_rate_se = sqrt(false_discovery_rate*(1-false_discovery_rate)/sutuntop1)
    false_discovery_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop1+1-FP),FP)
    false_discovery_rate_upp = Rbeta.inv((1 + CI/100)/2, (FP+1), (sutuntop1-FP))
    ## Dogru pozitif orani
    true_positiverate = TP / (satirtop1)
    ## Yanlis siniflandirma orani
    misclassification_rate = (FN + FP) / toplam
    misclassification_rate_se = sqrt(misclassification_rate*(1-misclassification_rate)/toplam)
    misclassification_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-FP-FN), (FP+FN))
    misclassification_rate_upp = Rbeta.inv((1 + CI/100)/2, (FP+FN+1), (toplam-FP-FN))
    ## Prevelans
    prevalence = (satirtop1 / toplam)
    prevalence_se = sqrt((satirtop1 / toplam)*(1-(satirtop1 / toplam))/toplam)
    prevalence_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-satirtop1), satirtop1)
    prevalence_upp = Rbeta.inv((1 + CI/100)/2, (satirtop1+1), (toplam-satirtop1))
    
    ##caret paketinden olculer
    detection_rate = TP/toplam
    detection_prevalence = (TP+FP)/toplam
    balanced_accuracy = (sensitivity_estimate + specificity)/2
    lift = (TP/(TP+FN))/((TP+FP)/toplam)  ## https://gerardnico.com/wiki/data_mining/error_rate
    precision = TP/(TP+FP)
    recall = TP/(TP+FN)
    ## http://michael.hahsler.net/research/recomm_lnai2002/lnai2002.pdf
    conviction = (1-((TP+FP)/toplam))/(1-(TP/(TP+FN)))
    matthews_correlation_coefficient = (TP * TN + FN * FP) / ((TP+FP)*(FN+TN)*(FP+TN)*(TP+FN))^(1/2)
    ## Goreli Risk Orani (Relative risk)
    ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
    # https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20ratio.htm
    relative_risk = (TP/sutuntop1)/(FN/sutuntop2)
    relative_risk_se = sqrt((1/TP)+(1/FN)-(1/(TP+FP))-(1/(FN+TN)))
    relative_risk_low = exp(log(relative_risk)+qnorm((1-CI/100)/2)*relative_risk_se)
    relative_risk_upp = exp(log(relative_risk)-qnorm((1-CI/100)/2)*relative_risk_se)
    difference_in_proportion = (TP/sutuntop1) - (FN/sutuntop2)
    ## Leverage
    ## https://www3.nd.edu/~tweninge/pubs/AHBCW_ANNIE08.pdf
    leverage = (TP/sutuntop1)-((TP+FP)/toplam)
    ## http://www.ebm.med.ualberta.ca/TherapyCalc.html
    control_event_rate = FN/sutuntop2
    experimental_event_rate = TP/sutuntop1
    ## Absolute risk reduction
    absolute_risk_reduction = -difference_in_proportion
    absolute_risk_reduction_se = sqrt((TP/sutuntop1)*(1-(TP/sutuntop1))/sutuntop1+(FN/sutuntop2)*(1-(FN/sutuntop2))/sutuntop2)
    absolute_risk_reduction_low = absolute_risk_reduction - qnorm((1-CI/100)/2)*absolute_risk_reduction_se
    absolute_risk_reduction_upp = absolute_risk_reduction + qnorm((1-CI/100)/2)*absolute_risk_reduction_se
    ## Relative risk reduction
    relative_risk_reduction = absolute_risk_reduction / (FN/sutuntop2)
    relative_risk_reduction_low = 1 - exp(log(relative_risk)-qnorm((1-CI/100)/2)*relative_risk_se)
    relative_risk_reduction_upp = 1 - exp(log(relative_risk)+qnorm((1-CI/100)/2)*relative_risk_se)
    ##Number needed to treat
    number_needed_to_treat = 1 / abs(difference_in_proportion)
    number_needed_to_treat_low = 1 /absolute_risk_reduction_upp
    number_needed_to_treat_upp = 1 /absolute_risk_reduction_low
    ## Test duzeyi
    test_level = sensitivity_randomtest
    test_level_se = sqrt(test_level*(1-test_level)/toplam)
    test_level_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-sutuntop1),sutuntop1)
    test_level_upp = Rbeta.inv((1 + CI/100)/2, (sutuntop1+1), (toplam-sutuntop1))
    ## Pre-test odds
    pretest_odds = prevalence / (1 - prevalence)
    ## Pozitif Olabilirlik Orani
    likelihoodratio_positivetest = sensitivity_estimate/(1-specificity)
    likelihoodratio_positivetest_se = exp(sqrt((1-sensitivity_estimate)/(sensitivity_estimate*satirtop1) +specificity/((1-specificity)*satirtop2)))
    likelihoodratio_positivetest_low = exp(log(likelihoodratio_positivetest) + qnorm((1 - CI/100)/2)*log(likelihoodratio_positivetest_se))
    likelihoodratio_positivetest_upp = exp(log(likelihoodratio_positivetest) - qnorm((1 - CI/100)/2)*log(likelihoodratio_positivetest_se))
    ## post-test odds
    posttest_odds = pretest_odds * likelihoodratio_positivetest
    bayes_ppv = (sensitivity_estimate * prevalence)/((sensitivity_estimate * prevalence) + (1 - specificity) * (1 - prevalence))
    ## Post-test probability (positive test result)
    posttest_probability = (pretest_odds * likelihoodratio_positivetest) / (1 + (pretest_odds * likelihoodratio_positivetest))
    ## Negatif Olabilirlik Orani
    likelihoodratio_negativetest = (1-sensitivity_estimate)/specificity
    likelihoodratio_negativetest_se = exp(sqrt(sensitivity_estimate/((1-sensitivity_estimate)*satirtop1) +(1-specificity)/(specificity*satirtop2)))
    likelihoodratio_negativetest_low = exp(log(likelihoodratio_negativetest) + qnorm((1-CI/100)/2)*log(likelihoodratio_negativetest_se))
    likelihoodratio_negativetest_upp = exp(log(likelihoodratio_negativetest) - qnorm((1-CI/100)/2)*log(likelihoodratio_negativetest_se))
    bayes_npv = (sensitivity_estimate * (1 - prevalence))/((sensitivity_estimate * (1 - prevalence)) + (1 - specificity) * prevalence)
    ## Post-test probability (negative test result)
    posttest_probability_neg = (pretest_odds * likelihoodratio_negativetest) / (1 + (pretest_odds * likelihoodratio_negativetest))
    ## Ters negatif olabilirlik orani
    inverse_likelihoodratio_negativetest = specificity/(1-sensitivity_estimate)
    inverse_likelihoodratio_negativetest_se = exp(sqrt(sensitivity_estimate/((1-sensitivity_estimate)*satirtop1) +(1-specificity)/(specificity*satirtop2)))
    inverse_likelihoodratio_negativetest_low = exp(log(inverse_likelihoodratio_negativetest) + qnorm((1-CI/100)/2)*log(inverse_likelihoodratio_negativetest_se))
    inverse_likelihoodratio_negativetest_upp = exp(log(inverse_likelihoodratio_negativetest) - qnorm((1-CI/100)/2)*log(inverse_likelihoodratio_negativetest_se))
    ## Diagnostic Odds Ratio (Tani Odds Orani)
    odds_ratio = (TP/FN)/(FP/TN)
    odds_ratio_se = exp(sqrt(1/TP+1/FP+1/TN+1/FN))
    odds_ratio_low = exp(log(odds_ratio) + qnorm((1-CI/100)/2)*log(odds_ratio_se))
    odds_ratio_upp = exp(log(odds_ratio) - qnorm((1-CI/100)/2)*log(odds_ratio_se))
    odds_ratio_Haldane = ((TP+0.5)/(FN+0.5))/((FP+0.5)/(TN+0.5))
    odds_ratio_Haldane_se = exp(sqrt(1/(TP+0.5)+1/(FP+0.5)+1/(TN+0.5)+1/(FN+0.5)))
    odds_ratio_Haldane_low = exp(log(odds_ratio_Haldane) + qnorm((1-CI/100)/2)*log(odds_ratio_Haldane_se))
    odds_ratio_Haldane_upp = exp(log(odds_ratio_Haldane) - qnorm((1-CI/100)/2)*log(odds_ratio_Haldane_se))
    error_odds_ratio = (sensitivity_estimate/(1-sensitivity_estimate))/(specificity/(1-specificity))
    ## Rate ratio
    ## http://omarkasule-05.tripod.com/id52.html
    ## http://sphweb.bumc.bu.edu/otlt/mph-modules/ep/ep713_randomerror/ep713_randomerror4.html
    rate_ratio = (TP/sutuntop1)/(TP/sutuntop2)
    rate_ratio_low = exp(log(relative_risk)+qnorm((1-CI/100)/2)*sqrt(1/TP+1/FP))
    rate_ratio_upp = exp(log(relative_risk)-qnorm((1-CI/100)/2)*sqrt(1/TP+1/FP))
    ## Risk Difference
    ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
    risk_difference = (TP/sutuntop1) - (FN/sutuntop2)
    risk_difference_se = (((TP/sutuntop1)*(1-(TP/sutuntop1)))/sutuntop1)+(((FN/sutuntop2)*(1-(FN/sutuntop2)))/sutuntop2)
    risk_difference_low = risk_difference + qnorm((1-CI/100)/2)*sqrt(risk_difference_se)
    risk_difference_upp = risk_difference - qnorm((1-CI/100)/2)*sqrt(risk_difference_se)
    ## Nitelenebilen risk
    ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
    attributable_risk = (TP/sutuntop1)-(FN/sutuntop2)
    attributable_risk_se = sqrt((satirtop1/toplam)*(1-(satirtop1/toplam))*(1/sutuntop1+1/sutuntop2))
    attributable_risk_low = attributable_risk + qnorm((1-CI/100)/2)*attributable_risk_se
    attributable_risk_upp = attributable_risk - qnorm((1-CI/100)/2)*attributable_risk_se
    ## Attributable risk percent
    ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
    attributable_risk_percent = ((relative_risk-1)/relative_risk)*100
    attributable_risk_percent_low = attributable_risk_percent - (qnorm((1-CI/100)/2)*attributable_risk_se)/attributable_risk
    attributable_risk_percent_upp = attributable_risk_percent + (qnorm((1-CI/100)/2)*attributable_risk_se)/attributable_risk
    ## Population Attributable Risk
    ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
    population_attributable_risk = (satirtop1/toplam)-(FN/sutuntop2)
    ## Population Attributable Risk Percent
    population_attributable_risk_percent = (population_attributable_risk / (satirtop1/toplam))*100
    ## Kohen's Kappa
    cohens_kappa = (efficiency-efficiency_randomtest)/(1-efficiency_randomtest)
    cohens_kappa_se = quality_index_se
    cohens_kappa_low = cohens_kappa + qnorm((1-CI/100)/2)*cohens_kappa_se
    cohens_kappa_upp = cohens_kappa - qnorm((1-CI/100)/2)*cohens_kappa_se
    ## Gozlenen Karar
    observed_agreement = efficiency
    observed_agreement_se = efficiency_se
    observed_agreement_low = efficiency_low
    observed_agreement_upp = efficiency_upp
    ## Risk karari,expected agreement
    chance_agreement = efficiency_randomtest
    ## Pozitif karar
    positive_agreement = 2*TP /(satirtop1+sutuntop1)
    positive_agreement_se = sqrt(4*TP*(FP+FN)*(TP+FP+FN))/(sutuntop1+satirtop1)^2
    positive_agreement_low = positive_agreement + qnorm((1-CI/100)/2)*positive_agreement_se
    positive_agreement_upp = positive_agreement - qnorm((1-CI/100)/2)*positive_agreement_se
    ## Negatif karar
    negative_agreement = 2*TN/(satirtop2+sutuntop2)
    negative_agreement_se = sqrt(4*TN*(FP+FN)*(TN+FP+FN))/(sutuntop2+satirtop2)^2
    negative_agreement_low = negative_agreement + qnorm((1-CI/100)/2)*negative_agreement_se
    negative_agreement_upp = negative_agreement - qnorm((1-CI/100)/2)*negative_agreement_se
    ## Rand index
    ## http://www.cs.pomona.edu/classes/cs158/resources/158-10(EM).pdf
    rand_index = (TP+TN)/(TP+FN+FP+TN)
    ## e-measure
    ## http://michael.hahsler.net/research/recomm_lnai2002/lnai2002.pdf
    e_measure = 1/((((1-CI)/100)*(1-precision))+((1-((1-CI)/100))*(1/recall)))
    ## discriminant power 
    ## https://eva.fing.edu.uy/pluginfile.php/69453/mod_resource/content/1/7633-10048-1-PB.pdf
    discriminant_power = (sqrt(3)/pi)*(log (sensitivity_estimate / (1-sensitivity_estimate)) + log (specificity / (1-specificity)))
    ## F1 Score
    ## http://machinelearningmastery.com/classification-accuracy-is-not-enough-more-performance-measures-you-can-use/
    f1_score = 2*(((TP/(TP+FN))*(TP/(TP+FP)))/((TP/(TP+FN))+(TP/(TP+FP))))
    ## Byrt yanlilik indeksi
    byrt_bias_index = (FN-FP)/toplam
    ## Byrt  asimetrik indeks prevelansi
    byrt_prevalence_asymmetry_index = (TN-TP)/toplam
    ## Yanlilik duzeltmeli kappa
    bias_adjusted_kappa = (efficiency-0.25*((satirtop1+sutuntop1)^2+(satirtop2+sutuntop2)^2)/toplam^2)/(1-0.25*((satirtop1+sutuntop1)^2+(satirtop2+sutuntop2)^2)/toplam^2)
    ## Yanlilik duzeltmeli kappa prevelansi
    prevalence_bias_adjusted_kappa = cohens_kappa+(1-cohens_kappa)*(byrt_prevalence_asymmetry_index^2-byrt_bias_index^2)
    ## Dice'in indeksi (Czekanowski)
    dice_index = positive_agreement
    dice_index_se= positive_agreement_se
    dice_index_low = positive_agreement_low
    dice_index_upp = positive_agreement_upp
    ## Yule's Q
    yules_q = (odds_ratio - 1) / (odds_ratio + 1)
    yules_q_se = 0.5*(1-yules_q^2)*sqrt(1/TP+1/FP+1/TN+1/FN)
    yules_q_low = yules_q + qnorm((1-CI/100)/2)*yules_q_se
    yules_q_upp = yules_q - qnorm((1-CI/100)/2)*yules_q_se
    
    equitable_threatscore = (TP-(sutuntop1*satirtop1/toplam))/(TP+FP+FN-(sutuntop1*satirtop1/toplam))
    ## Phi
    phi = (TP*TN-FN*FP) / sqrt(satirtop1 * satirtop2 * sutuntop1 * sutuntop2)
    phi_se = sqrt((1-phi^2+(phi+0.5*phi^3)*((satirtop1-satirtop2)*(sutuntop1-sutuntop2)/sqrt(satirtop1*satirtop2*sutuntop1*sutuntop2))-0.75*phi^2*((satirtop1-satirtop2)^2/(satirtop1*satirtop2)+(sutuntop1-sutuntop2)^2/(sutuntop1*sutuntop2)))/toplam)
    phi_low = phi + qnorm((1-CI/100)/2)*phi_se
    phi_upp = phi - qnorm((1-CI/100)/2)*phi_se
    ## Cramer V katsayisi
    cramer_v = sqrt((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2))/toplam)
    ## Olaganlik katsayisi
    contingency_coefficient = sqrt((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2))/(((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)))+toplam))
    ## Goodman and Kruskal Gamma 
    goodman_and_kruskal_gamma = (TP*TN-FN*FP)/(TP*TN+FN*FP)
    goodman_and_kruskal_gamma_se = 0.5*(1-yules_q^2)*sqrt(1/TP+1/FP+1/TN+1/FN)
    goodman_and_kruskal_gamma_low = yules_q + qnorm((1-CI/100)/2)*yules_q_se
    goodman_and_kruskal_gamma_upp = yules_q - qnorm((1-CI/100)/2)*yules_q_se
    ## Kendall's tau a
    kendalls_tau_a = (TP*TN-FN*FP)/(toplam*(toplam-1)/2)
    ## Kendall's tau b
    ## https://v8doc.sas.com/sashtml/stat/chap28/sect20.htm
    ## http://slideplayer.com/slide/10766029/
    kendalls_tau_b = (TP*TN-FN*FP)/sqrt((TP*TN+FN*FP+(TP*FN+FP*TN))*(TP*TN+FN*FP+(TP*FP+FN*TN)))
    ## Kendall's tau c
    kendalls_tau_c = ((2*2)*(TP*TN-FN*FP)) / (toplam^2 * (2-1))
    ## Somers'd R|C
    somers_d = (TP*TN-FN*FP)/(TP*TN+FN*FP+(TP*FN+FP*TN))
    ## Somers'd C|R
    somers_d_2 = (TP*TN-FP*FN)/(TP*TN+FP*FN+(TP*FP+FN*TN))
    ## Scoot'un karar indeksi
    scotts_agreement_index = bias_adjusted_kappa
    ## Dort-duzeyli Korelasyon
    tetrachoric_correlation = cos(pi/(1+sqrt(TP*TN/FN/FP)))
    tetrachoric_correlation_se = 0.375*(1-tetrachoric_correlation^2)*sqrt((1/(TP))+(1/(FP))+(1/(TN))+(1/(FN)))
    tetrachoric_correlation_low = tetrachoric_correlation + qnorm((1-CI/100)/2)*tetrachoric_correlation_se
    tetrachoric_correlation_upp = tetrachoric_correlation - qnorm((1-CI/100)/2)*tetrachoric_correlation_se
    ## Goodman kruskal tau katsayisi
    goodman_kruskal_tau = (toplam*((TP^2+FP^2)/sutuntop1+(FN^2+TN^2)/sutuntop2)-satirtop1^2-satirtop2^2)/(toplam^2-satirtop1^2-satirtop2^2)
    goodman_kruskal_tau_se = toplam*sqrt(TP*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop1)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*TP/satirtop1-(TP^2+FN^2)/satirtop1^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+FN*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop2)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*FN/satirtop1-(TP^2+FN^2)/satirtop1^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+FP*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop1)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*FP/satirtop2-(FP^2+TN^2)/satirtop2^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+TN*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop2)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*TN/satirtop2-(FP^2+TN^2)/satirtop2^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2)/(toplam^2-sutuntop1^2-sutuntop2^2)^2
    goodman_kruskal_tau_low = goodman_kruskal_tau + qnorm((1-CI/100)/2)*goodman_kruskal_tau_se
    goodman_kruskal_tau_upp = goodman_kruskal_tau - qnorm((1-CI/100)/2)*goodman_kruskal_tau_se
    ## Simetrik Lambda 
    lambda_symmetric = ((max(TP,FN)+max(FP,TN)+max(TP,FP)+max(FN,TN)-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2))/(2*toplam-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2)))
    if (lambda_symmetric==0 || lambda_symmetric==1)
    {lambda_symmetric_se = 0}
    else
    {
      lambda_symmetric_se = sqrt((TP*((if(TP>FN) 1 else 0) + (if(TP>FP) 1 else 0) + ((if(sutuntop1>sutuntop2) 1 else 0)+ (if(satirtop1>satirtop2) 1 else 0))*(lambda_symmetric-1))^2+FN*( (if(TP>FN) 0 else 1)+ (if(FN>TN) 1 else 0) +( (if(sutuntop1>sutuntop2) 0 else 1) + (if(satirtop1>satirtop2) 1 else 0))*(lambda_symmetric-1))^2+FP*( (if(FP>TN) 1 else 0) + (if(TP>FP) 0 else 1) +( (if(sutuntop1>sutuntop2) 1 else 0) + (if(satirtop1<satirtop2) 1 else 0))*(lambda_symmetric-1))^2+TN*((if(FP>TN) 0 else 1)+ (if(FN>TN) 0 else 1) +((if(sutuntop1>sutuntop2) 0 else 1)+ (if(satirtop1<satirtop2) 1 else 0))*(lambda_symmetric-1))^2-4*toplam*lambda_symmetric^2)/(2*toplam-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2))^2)
    }
    
    lambda_symmetric_low = lambda_symmetric + qnorm((1-CI/100)/2)*lambda_symmetric_se
    lambda_symmetric_upp = lambda_symmetric - qnorm((1-CI/100)/2)*lambda_symmetric_se
    ## Lambda Asymmetric R|C
    lambda_criterion = (max(TP,FP)+max(FN,TN)-max(satirtop1,satirtop2))/(toplam-max(satirtop1,satirtop2))
    
    if (lambda_criterion==0 || lambda_criterion==1)
    {lambda_criterion_se = 0}
    else
    {
      lambda_criterion_se = sqrt((toplam-max(TP,FP)-max(FN,TN))*(max(TP,FP)+max(FN,TN)+max(satirtop1,satirtop2)-2*(if(satirtop1>satirtop2) max(TP,FN) else max(FP,TN)))/(toplam-max(satirtop1,satirtop2))^3)
    }
    
    lambda_criterion_low = lambda_criterion + qnorm((1-CI/100)/2)*lambda_criterion_se
    lambda_criterion_upp = lambda_criterion - qnorm((1-CI/100)/2)*lambda_criterion_se
    ## Lambda Asymmetric C|R
    lambda_criterion_2 = (max(TP,FN)+max(FP,TN)-max(sutuntop1,sutuntop2))/(toplam-max(sutuntop1,sutuntop2))
    
    if (lambda_criterion_2==0 || lambda_criterion_2==1)
    {lambda_criterion_se_2 = 0}
    else
    {
      lambda_criterion_se_2 = sqrt((toplam-max(TP,FN)-max(FP,TN))*(max(TP,FN)+max(FP,TN)+max(sutuntop1,sutuntop2)-2*(if(sutuntop1>sutuntop2) max(TP,FP) else max(FN,TN)))/(toplam-max(sutuntop1,sutuntop2))^3)
    }
    
    lambda_criterion_low_2 = lambda_criterion_2 + qnorm((1-CI/100)/2)*lambda_criterion_se_2
    lambda_criterion_upp_2 = lambda_criterion_2 - qnorm((1-CI/100)/2)*lambda_criterion_se_2
    ## belirsizlik katsayisi (coefficient of uncertainty) simetrik
    uncertainty_coefficient_symmetric = 2*(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam)))/(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))
    uncertainty_coefficient_symmetric_se = 2*sqrt(TP*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop1*sutuntop1/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(TP/toplam))^2+FN*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop1*sutuntop2/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(FN/toplam))^2+FP*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop2*sutuntop1/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(FP/toplam))^2+TN*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop2*sutuntop2/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(TN/toplam))^2)/toplam/(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam)^2
    uncertainty_coefficient_symmetric_low = uncertainty_coefficient_symmetric + qnorm((1-CI/100)/2)*uncertainty_coefficient_symmetric_se
    uncertainty_coefficient_symmetric_upp = uncertainty_coefficient_symmetric - qnorm((1-CI/100)/2)*uncertainty_coefficient_symmetric_se
    ## (coefficient of uncertainty) R|C
    uncertainty_coeff_crit = (satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam)))/(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))
    uncertainty_coeff_crit_se = sqrt(TP*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(TP/sutuntop1)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop1/toplam))^2+FN*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(FN/sutuntop2)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop1/toplam))^2+FP*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(FP/sutuntop1)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop2/toplam))^2+TN*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(TN/sutuntop2)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop2/toplam))^2)/toplam/((satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam)^2
    uncertainty_coeff_crit_low = uncertainty_coeff_crit + qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se
    uncertainty_coeff_crit_upp = uncertainty_coeff_crit - qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se
    ## (coefficient of uncertainty) C|R
    uncertainty_coeff_crit_2 = (sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)+satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)-(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam)))/(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))
    uncertainty_coeff_crit_se_2 = sqrt(TP*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(TP/satirtop1)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop1/toplam))^2+FP*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(FP/satirtop2)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop1/toplam))^2+FN*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(FN/satirtop1)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop2/toplam))^2+TN*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(TN/satirtop2)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop2/toplam))^2)/toplam/((sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)^2
    uncertainty_coeff_crit_low_2 = uncertainty_coeff_crit_2 + qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se_2
    uncertainty_coeff_crit_upp_2 = uncertainty_coeff_crit_2 - qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se_2
    ## Yule s Y (Coefficient of colligation)
    yule_y = (sqrt(TP*TN)-sqrt(FN*FP))/(sqrt(TP*TN)+sqrt(FN*FP))
    ## Pearson ki-kare
    pearson_chi_squ = toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)
    pearson_chi_squ_p = dchi(pearson_chi_squ, df=1)
    ## 
    with_yate_cor_for_pearson = toplam*(abs(TP*TN-FN*FP)-0.5*toplam)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)
    with_yate_cor_p_for_pearson = dchi(with_yate_cor_for_pearson, df=1)
    ## Mantel Haenszel chi-square
    ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
    mantel_haenszel = ((toplam-1)*(TP*TN-FN*FP)^2)/(satirtop1*satirtop2*sutuntop1*sutuntop2)
    mantel_haenszel_p = dchi(mantel_haenszel, df=1)
    ## Olasilik orani
    likelih_ratio = 2*(TP*log(TP/(sutuntop1*satirtop1/toplam))+FN*log(FN/(sutuntop2*satirtop1/toplam))+FP*log(FP/(sutuntop1*satirtop2/toplam))+TN*log(TN/(sutuntop2*satirtop2/toplam)))
    likelih_ratio_p =dchi(likelih_ratio, df=1)
    ## Fisher'in exact testi
    ##fisher_exact_test = (TP-(satirtop1*sutuntop1/toplam))^2/(satirtop1*sutuntop1/toplam) + (FN-(satirtop1*sutuntop2/toplam))^2/(satirtop1*sutuntop2/toplam) + (FP-(satirtop2*sutuntop1/toplam))^2/(satirtop2*sutuntop1/toplam) + (TN-(satirtop2*sutuntop2/toplam))^2/(satirtop2*sutuntop2/toplam)
    fisher <- c(TP, FN, FP, TN)
    tab <- t(matrix(fisher, nrow=2,ncol=2))
    fisher_exact_test = fisher.test(tab)$p.value
    minimum_exp_fre = min((sutuntop1*satirtop1/toplam):(sutuntop2*satirtop2/toplam))
    cells_exp_fre_5 = (if((sutuntop1*satirtop1/toplam)<5) 1 else 0)+ (if((sutuntop1*satirtop2/toplam)<5) 1 else 0)+ (if((sutuntop2*satirtop1/toplam) <5) 1 else 0)+ (if((sutuntop2*satirtop2/toplam) <5) 1 else 0)
    cells_exp_fre_1 = (if((sutuntop1*satirtop1/toplam) <1) 1 else 0)+ (if((sutuntop1*satirtop2/toplam) <1) 1 else 0)+ (if((sutuntop2*satirtop1/toplam) <1) 1 else 0)+ (if((sutuntop2*satirtop2/toplam) <1) 1 else 0)
    ## Mc Nemar testi
    mcNemar_test = (FP-FN)^2/(FP+FN)
    mcNemar_test_p = dchi(mcNemar_test, df=1)
    with_Yate_cor_for_mcnamer = (abs(FP-FN)-1)^2/(FP+FN)
    with_Yate_cor_p_for_mcnamer = dchi(with_Yate_cor_for_mcnamer, df=1)
    
    ## Belirsizlik (Entropi)
    forbes_NMI = sensitivity_estimate*predvalue_positiverandomtest*log((sensitivity_estimate/predvalue_positiverandomtest_mar_ol),base=2)+
      (1-specificity)*(1-predvalue_positiverandomtest)*log(((1-specificity)/predvalue_positiverandomtest_mar_ol),base=2)+
      (1-sensitivity_estimate)*predvalue_positiverandomtest*log(((1-sensitivity_estimate)/(1-predvalue_positiverandomtest_mar_ol)),base=2)+
      specificity*(1-predvalue_positiverandomtest)*log((specificity/(1-predvalue_positiverandomtest_mar_ol)),base=2)
    ## satir icin entropy (test icin)
    entropy_hr = - ((sutuntop1/toplam) * log(sutuntop1/toplam, base=2) + (sutuntop2/toplam) * log(sutuntop2/toplam, base=2))
    ## sutun icin entropy (hastalik icin)  
    entropy_hc = - ((satirtop1/toplam)*log(satirtop1/toplam, base=2) + (satirtop2/toplam)*log(satirtop2/toplam, base=2))
    ## birlesik entropi (joint entropy)  
    entropy_hrc = - ( (TP/toplam)*log(TP/toplam, base=2) + (FP/toplam)*log(FP/toplam, base=2) + (FN/toplam)*log(FN/toplam, base=2) + (TN/toplam)*log(TN/toplam, base=2))
    ## bilgi icerigi (mutual information)  
    information_r_c = entropy_hr + entropy_hc - entropy_hrc
    ## kosullu entropi (conditional entropy)  
    a = entropy_hrc - entropy_hr
    c = entropy_hrc - entropy_hc
    sim_r_c = information_r_c / (a + information_r_c + c)
    dis_r_c = (a + c) / (a + information_r_c + c)
    ## goreli entropi (relative entropy, kullback-leibler uzakligi)
    ## pozitif test sonucu icin goreli entropi, Relative Improvement Over Chance (RIOC)
    positive_relative_entropy = (TP/sutuntop1)*log(((TP/sutuntop1)/ prevalence), base=2)+(FP/sutuntop1)*log(((FP/sutuntop1)/(1- prevalence)), base=2)
    ## negatif test sonucu icin goreli entropi 
    negative_relative_entropy = (FN/sutuntop2)*log(((FN/sutuntop2)/ prevalence), base=2)+(TN/sutuntop2)*log(((TN/sutuntop2)/(1- prevalence)), base=2)
    
    verim <- data.frame(#Estimate=integer(),
      #LowerCI=integer(),
      #UpperCI=integer()
    )
    row <- vector(mode="character")
    if (dice_index >= 0 && dice_index <= 1) 
    {
      verim <- data.frame(Estimate = round(dice_index, digit =3),  LowerCI = round(dice_index_low, digit =3), UpperCI = round(dice_index_upp, digit =3))
      row <- c("Dice\'s Index (Czekanowski)")
    } 
    if (yules_q >= 0 && yules_q <= 1)
    {
      newRow <- data.frame(Estimate = round(yules_q, digit =3),  LowerCI = round(yules_q_low, digit =3), UpperCI = round(yules_q_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Yules Q (Gamma)")
    }
    if (equitable_threatscore >= 0 && equitable_threatscore <= 1)
    {
      newRow <- data.frame(Estimate = round(equitable_threatscore, digit =3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Equitable Threatscore")
    }
    if (phi >= 0 && phi <= 1)
    {
      newRow <- data.frame(Estimate = round(phi, digit =3),  LowerCI = round(phi_low, digit =3), UpperCI = round(phi_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Phi")
    }
    
    ## ?????? Adjusted Contingency Coefficient
    if (contingency_coefficient >= 0 && contingency_coefficient <= 1)
    {
      newRow <- data.frame(Estimate = round(contingency_coefficient, digit =3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Contingency Coefficient")
    }
    if (goodman_and_kruskal_gamma >= 0 && goodman_and_kruskal_gamma <= 1)
    {
      newRow <- data.frame(Estimate = round(goodman_and_kruskal_gamma, digit =3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Goodman and Kruskal Gamma (Gamma Statistic-Gamma Coefficient)")
    }
    if (scotts_agreement_index >= 0 && scotts_agreement_index <= 1)
    {
      newRow <- data.frame(Estimate = round(scotts_agreement_index, digit =3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Scotts agreement index")
    }
    if (tetrachoric_correlation >= 0 && tetrachoric_correlation <= 1)
    {
      newRow <- data.frame(Estimate = round(tetrachoric_correlation, digit =3),  LowerCI = round(tetrachoric_correlation_low, digit =3), UpperCI = round(tetrachoric_correlation_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Tetrachoric (Polychoric) Correlation")
    }
    if (goodman_kruskal_tau >= 0 && goodman_kruskal_tau <= 1)
    {
      newRow <- data.frame(Estimate = round(goodman_kruskal_tau, digit =3),  LowerCI = round(goodman_kruskal_tau_low, digit =3), UpperCI = round(goodman_kruskal_tau_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Goodman & Kruskals tau (Crit. dep.)")
    }
    if (lambda_symmetric >= 0 && lambda_symmetric <= 1)
    {
      newRow <- data.frame(Estimate = round(lambda_symmetric, digit =3),  LowerCI = round(lambda_symmetric_low, digit =3), UpperCI = round(lambda_symmetric_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Lambda (Symmetric)")
    }
    if (lambda_criterion >= 0 && lambda_criterion <= 1)
    {
      newRow <- data.frame(Estimate = round(lambda_criterion, digit =3),  LowerCI = round(lambda_criterion_low, digit =3), UpperCI = round(lambda_criterion_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Lambda (Criterion_dep.)")
    }
    if (uncertainty_coefficient_symmetric >= 0 && uncertainty_coefficient_symmetric <= 1)
    {
      newRow <- data.frame(Estimate = round(uncertainty_coefficient_symmetric, digit =3),  LowerCI = round(uncertainty_coefficient_symmetric_low, digit =3), UpperCI = round(uncertainty_coefficient_symmetric_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Uncertainty Coefficient (Symmetric)")
    }
    if (uncertainty_coeff_crit >= 0 && uncertainty_coeff_crit <= 1)
    {
      newRow <- data.frame(Estimate = round(uncertainty_coeff_crit, digit =3),  LowerCI = round(uncertainty_coeff_crit_low, digit =3), UpperCI = round(uncertainty_coeff_crit_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Uncertainty Coeff. (Criterion_dep.)")
    }
    
    rownames(verim) = c("Dice\'s Index (Czekanowski-Sorenson)", "Yules Q (Gamma)", "Equitable Threatscore (Gilbert Skill Score)", "Phi", "Contingency Coefficient", 
                        "Goodman and Kruskal Gamma (Gamma Statistic-Gamma Coefficient)","Scotts agreement index", "Tetrachoric Correlation", 
                        "Goodman & Kruskals tau (Crit. dep.)", 
                        "Lambda (Symmetric)", "Lambda (Criterion_dep.)", "Uncertainty Coefficient (Symmetric)", 
                        "Uncertainty Coeff. (Criterion_dep.)")
    
    
    
    highchart() %>% hc_exporting(enabled = TRUE, filename = "Plot1") %>% 
      hc_add_series(name = "Conf. Interval", type = "line", data = verim$`Estimate`) %>%
      hc_add_series(name = "CI", data = as.matrix(cbind(verim$`LowerCI`, verim$`UpperCI`)),type = "errorbar", names = "Limits", showInLegend = FALSE, zIndex = 0, lineWidth = 1.5, linkedTo = "CI"
                    ) %>%
      hc_xAxis(categories = rownames(verim),title = "Association Measures") %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, headerFormat = "<b>Association Measures: </b>{point.x} <br>") %>%
      hc_plotOptions(line = list(tooltip = list(pointFormat = "<b>{series.name}: </b>{point.y:.3f} ")), 
                     errorbar = list(tooltip = list(pointFormat = "({point.low} - {point.high})"))) %>%
      hc_add_theme(hc_theme_google())
})
  
  output$plot11 <- highcharter::renderHighchart({
    if (input$verigir_kontrol == TRUE){ 
      TP = 0
      FN = 0
      FP = 0
      TN = 0
      for (i in 1:nrow(tabloveri()))
      {
        if (tabloveri()[i,1]==1 & tabloveri()[i,2]==1)
        {TP = TP+1}
        else if (tabloveri()[i,1]==1 & tabloveri()[i,2]==2)
        {FN = FN+1}
        else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==1)
        {FP = FP+1}
        else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==2)
        {TN = TN+1}
        next
      }
      CI = as.numeric(trimws(input$text5_2))
    }
    else{
      TP = as.numeric(trimws(input$text1)) ### x
      FN = as.numeric(trimws(input$text2)) ### y
      FP = as.numeric(trimws(input$text3)) ### z
      TN = as.numeric(trimws(input$text4)) ### t
      CI = as.numeric(trimws(input$text5))
    }
    
    
    satirtop1 = TP + FN
    satirtop2 = FP + TN
    sutuntop1 = TP + FP
    sutuntop2 = FN + TN
    toplam = satirtop1 + satirtop2
    #table1 <- matrix(c(TP,FN,satirtop1,FP,TN,satirtop2,sutuntop1,sutuntop2,toplam),nrow=3,ncol=3,byrow=TRUE,dimnames=list(c("1.satir","2.satir","Sutun toplamlari"),c("1.kolon","2.kolon","Satir toplamlari")))
    #table1
    ## Duyarlilik 
    sensitivity_estimate = TP / satirtop1
    sensitivity_se = round((sqrt(sensitivity_estimate * (1 - sensitivity_estimate)/satirtop1)), 4)
    sensitivity_low = 1 - Rbeta.inv(((1 + CI/100)/2), (satirtop1+1-TP), TP)
    sensitivity_upp = Rbeta.inv(((1 + CI/100)/2), (TP+1), (satirtop1-TP))
    sensitivity_randomtest = sutuntop1 / toplam
    qualityindex_sensitivity = (sensitivity_estimate - sensitivity_randomtest) / (1 - sensitivity_randomtest)
    ## Secicilik
    specificity = TN / satirtop2
    specificity_se = sqrt(specificity * (1 - specificity) / satirtop2)
    specificity_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop2+1-TN),TN)
    specificity_upp = Rbeta.inv((1 + CI/100)/2, (TN+1), (satirtop2-TN))
    specificity_randomtest = (1 - sensitivity_randomtest)
    qualityindex_specificity = (specificity - 1 + sensitivity_randomtest) / sensitivity_randomtest
    ## Gain in Certainty
    gain_in_certainty = sensitivity_estimate + specificity
    ## Etkililik, yeterlilik ayni zamanda overall accuracy (toplam-genel dogruluk orani)
    efficiency = (TP + TN) / toplam
    efficiency_se = sqrt(efficiency * (1 - efficiency) / toplam)
    efficiency_low = 1-Rbeta.inv((1 + CI/100)/2, (toplam+1-TP-TN), (TP+TN))
    efficiency_upp = Rbeta.inv((1 + CI/100)/2, (TP+TN+1), (toplam-TP-TN))
    efficiency_randomtest = (satirtop1 / toplam) * sensitivity_randomtest + (1 - (satirtop1 / toplam)) * (1 - sensitivity_randomtest)
    mis_efficiency = 1 - efficiency
    ## Quality indeks
    quality_index = (efficiency - efficiency_randomtest) / (1 - efficiency_randomtest)
    quality_index_se = sqrt((efficiency*(1-efficiency)/(1-efficiency_randomtest)^2+2*(1-efficiency)*(2*efficiency*efficiency_randomtest-((TP*((satirtop1 / toplam)+sensitivity_randomtest)+TN*(2-(satirtop1 / toplam)-sensitivity_randomtest))/toplam))/(1-efficiency_randomtest)^3+((1-efficiency)^2)*((TP*((satirtop1 / toplam)+sensitivity_randomtest)^2+FN*(-(satirtop1 / toplam)+1+sensitivity_randomtest)^2+FP*(1+(satirtop1 / toplam)-sensitivity_randomtest)^2+TN*(2-(satirtop1 / toplam)-sensitivity_randomtest)^2)/toplam-4*efficiency_randomtest^2)/(1-efficiency_randomtest)^4)/toplam)
    quality_index_low = quality_index + qnorm((1 - CI/100)/2)*quality_index_se
    quality_index_upp = quality_index - qnorm((1 - CI/100)/2)*quality_index_se
    ## Youden indeksi
    youdens_index = sensitivity_estimate + specificity - 1
    youdens_index_se = sqrt(sensitivity_se^2+specificity_se^2)
    youdens_index_low = youdens_index + qnorm((1 - CI/100)/2)*youdens_index_se
    youdens_index_upp = youdens_index - qnorm((1 - CI/100)/2)*youdens_index_se
    
    number_needed_to_diagnose = 1 / youdens_index
    number_needed_to_misdiagnose = 1 / ( 1 - efficiency ) 
    ## Pozitif kestirim degeri
    predictivevalue_positivetest = TP / sutuntop1
    predictivevalue_positivetest_se = sqrt(predictivevalue_positivetest*(1-predictivevalue_positivetest)/sutuntop1)
    predictivevalue_positivetest_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop1+1-TP),TP)
    predictivevalue_positivetest_upp = Rbeta.inv((1 + CI/100)/2, (TP+1),(sutuntop1-TP))
    predvalue_positiverandomtest = (satirtop1 / toplam)
    predvalue_positiverandomtest_mar_ol = predvalue_positiverandomtest*sensitivity_estimate+(1-predvalue_positiverandomtest)*(1-specificity)
    ## Negatif kestirim degeri
    predictivevalue_negativetest = TN / sutuntop2
    predictivevalue_negativetest_se = sqrt(predictivevalue_negativetest*(1-predictivevalue_negativetest)/sutuntop2)
    predictivevalue_negativetest_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop2+1-TN),TN)
    predictivevalue_negativetest_upp = Rbeta.inv((1 + CI/100)/2, (TN+1), (sutuntop2-TN))
    predvalue_negativerandomtest = 1 - (satirtop1 / toplam)
    predvalue_negativerandomtest_mar_ol = 1 - predvalue_positiverandomtest_mar_ol
    ## Predictive Summary Index (PSI)
    predictive_summary_index = predictivevalue_positivetest + predictivevalue_negativetest - 1
    ## Yanlis pozitif orani
    false_positiverate = FP / satirtop2
    false_positiverate_se = sqrt(false_positiverate*(1-false_positiverate)/satirtop2)
    false_positiverate_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop2+1-FP),FP)
    false_positiverate_upp = Rbeta.inv((1 + CI/100)/2, (FP+1), (satirtop2-FP))
    ## Yansis negatif orani
    false_negativerate = FN / satirtop1
    false_negativerate_se = sqrt(false_negativerate*(1-false_negativerate)/satirtop1)
    false_negativerate_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop1+1-FN),FN)
    false_negativerate_upp = Rbeta.inv((1 + CI/100)/2, (FN+1), (satirtop1-FN))
    ## False Omission Rate (FOR) https://www.ncss.com/software/ncss/diagnostic-tests-in-ncss/
    false_omission_rate = FN/(sutuntop2)
    false_omission_rate_se = sqrt(false_omission_rate*(1-false_omission_rate)/sutuntop2)
    false_omission_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop2+1-FN),FN)
    false_omission_rate_upp = Rbeta.inv((1 + CI/100)/2, (FN+1), (sutuntop2-FN))
    ## False Discovery Rate (FDR) https://www.ncss.com/software/ncss/diagnostic-tests-in-ncss/
    false_discovery_rate = FP/(sutuntop1)
    false_discovery_rate_se = sqrt(false_discovery_rate*(1-false_discovery_rate)/sutuntop1)
    false_discovery_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop1+1-FP),FP)
    false_discovery_rate_upp = Rbeta.inv((1 + CI/100)/2, (FP+1), (sutuntop1-FP))
    ## Dogru pozitif orani
    true_positiverate = TP / (satirtop1)
    ## Yanlis siniflandirma orani
    misclassification_rate = (FN + FP) / toplam
    misclassification_rate_se = sqrt(misclassification_rate*(1-misclassification_rate)/toplam)
    misclassification_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-FP-FN), (FP+FN))
    misclassification_rate_upp = Rbeta.inv((1 + CI/100)/2, (FP+FN+1), (toplam-FP-FN))
    ## Prevelans
    prevalence = (satirtop1 / toplam)
    prevalence_se = sqrt((satirtop1 / toplam)*(1-(satirtop1 / toplam))/toplam)
    prevalence_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-satirtop1), satirtop1)
    prevalence_upp = Rbeta.inv((1 + CI/100)/2, (satirtop1+1), (toplam-satirtop1))
    
    ##caret paketinden olculer
    detection_rate = TP/toplam
    detection_prevalence = (TP+FP)/toplam
    balanced_accuracy = (sensitivity_estimate + specificity)/2
    lift = (TP/(TP+FN))/((TP+FP)/toplam)  ## https://gerardnico.com/wiki/data_mining/error_rate
    precision = TP/(TP+FP)
    recall = TP/(TP+FN)
    ## http://michael.hahsler.net/research/recomm_lnai2002/lnai2002.pdf
    conviction = (1-((TP+FP)/toplam))/(1-(TP/(TP+FN)))
    matthews_correlation_coefficient = (TP * TN + FN * FP) / ((TP+FP)*(FN+TN)*(FP+TN)*(TP+FN))^(1/2)
    ## Goreli Risk Orani (Relative risk)
    ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
    # https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20ratio.htm
    relative_risk = (TP/sutuntop1)/(FN/sutuntop2)
    relative_risk_se = sqrt((1/TP)+(1/FN)-(1/(TP+FP))-(1/(FN+TN)))
    relative_risk_low = exp(log(relative_risk)+qnorm((1-CI/100)/2)*relative_risk_se)
    relative_risk_upp = exp(log(relative_risk)-qnorm((1-CI/100)/2)*relative_risk_se)
    difference_in_proportion = (TP/sutuntop1) - (FN/sutuntop2)
    ## Leverage
    ## https://www3.nd.edu/~tweninge/pubs/AHBCW_ANNIE08.pdf
    leverage = (TP/sutuntop1)-((TP+FP)/toplam)
    ## http://www.ebm.med.ualberta.ca/TherapyCalc.html
    control_event_rate = FN/sutuntop2
    experimental_event_rate = TP/sutuntop1
    ## Absolute risk reduction
    absolute_risk_reduction = -difference_in_proportion
    absolute_risk_reduction_se = sqrt((TP/sutuntop1)*(1-(TP/sutuntop1))/sutuntop1+(FN/sutuntop2)*(1-(FN/sutuntop2))/sutuntop2)
    absolute_risk_reduction_low = absolute_risk_reduction - qnorm((1-CI/100)/2)*absolute_risk_reduction_se
    absolute_risk_reduction_upp = absolute_risk_reduction + qnorm((1-CI/100)/2)*absolute_risk_reduction_se
    ## Relative risk reduction
    relative_risk_reduction = absolute_risk_reduction / (FN/sutuntop2)
    relative_risk_reduction_low = 1 - exp(log(relative_risk)-qnorm((1-CI/100)/2)*relative_risk_se)
    relative_risk_reduction_upp = 1 - exp(log(relative_risk)+qnorm((1-CI/100)/2)*relative_risk_se)
    ##Number needed to treat
    number_needed_to_treat = 1 / abs(difference_in_proportion)
    number_needed_to_treat_low = 1 /absolute_risk_reduction_upp
    number_needed_to_treat_upp = 1 /absolute_risk_reduction_low
    ## Test duzeyi
    test_level = sensitivity_randomtest
    test_level_se = sqrt(test_level*(1-test_level)/toplam)
    test_level_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-sutuntop1),sutuntop1)
    test_level_upp = Rbeta.inv((1 + CI/100)/2, (sutuntop1+1), (toplam-sutuntop1))
    ## Pre-test odds
    pretest_odds = prevalence / (1 - prevalence)
    ## Pozitif Olabilirlik Orani
    likelihoodratio_positivetest = sensitivity_estimate/(1-specificity)
    likelihoodratio_positivetest_se = exp(sqrt((1-sensitivity_estimate)/(sensitivity_estimate*satirtop1) +specificity/((1-specificity)*satirtop2)))
    likelihoodratio_positivetest_low = exp(log(likelihoodratio_positivetest) + qnorm((1 - CI/100)/2)*log(likelihoodratio_positivetest_se))
    likelihoodratio_positivetest_upp = exp(log(likelihoodratio_positivetest) - qnorm((1 - CI/100)/2)*log(likelihoodratio_positivetest_se))
    ## post-test odds
    posttest_odds = pretest_odds * likelihoodratio_positivetest
    bayes_ppv = (sensitivity_estimate * prevalence)/((sensitivity_estimate * prevalence) + (1 - specificity) * (1 - prevalence))
    ## Post-test probability (positive test result)
    posttest_probability = (pretest_odds * likelihoodratio_positivetest) / (1 + (pretest_odds * likelihoodratio_positivetest))
    ## Negatif Olabilirlik Orani
    likelihoodratio_negativetest = (1-sensitivity_estimate)/specificity
    likelihoodratio_negativetest_se = exp(sqrt(sensitivity_estimate/((1-sensitivity_estimate)*satirtop1) +(1-specificity)/(specificity*satirtop2)))
    likelihoodratio_negativetest_low = exp(log(likelihoodratio_negativetest) + qnorm((1-CI/100)/2)*log(likelihoodratio_negativetest_se))
    likelihoodratio_negativetest_upp = exp(log(likelihoodratio_negativetest) - qnorm((1-CI/100)/2)*log(likelihoodratio_negativetest_se))
    bayes_npv = (sensitivity_estimate * (1 - prevalence))/((sensitivity_estimate * (1 - prevalence)) + (1 - specificity) * prevalence)
    ## Post-test probability (negative test result)
    posttest_probability_neg = (pretest_odds * likelihoodratio_negativetest) / (1 + (pretest_odds * likelihoodratio_negativetest))
    ## Ters negatif olabilirlik orani
    inverse_likelihoodratio_negativetest = specificity/(1-sensitivity_estimate)
    inverse_likelihoodratio_negativetest_se = exp(sqrt(sensitivity_estimate/((1-sensitivity_estimate)*satirtop1) +(1-specificity)/(specificity*satirtop2)))
    inverse_likelihoodratio_negativetest_low = exp(log(inverse_likelihoodratio_negativetest) + qnorm((1-CI/100)/2)*log(inverse_likelihoodratio_negativetest_se))
    inverse_likelihoodratio_negativetest_upp = exp(log(inverse_likelihoodratio_negativetest) - qnorm((1-CI/100)/2)*log(inverse_likelihoodratio_negativetest_se))
    ## Diagnostic Odds Ratio (Tani Odds Orani)
    odds_ratio = (TP/FN)/(FP/TN)
    odds_ratio_se = exp(sqrt(1/TP+1/FP+1/TN+1/FN))
    odds_ratio_low = exp(log(odds_ratio) + qnorm((1-CI/100)/2)*log(odds_ratio_se))
    odds_ratio_upp = exp(log(odds_ratio) - qnorm((1-CI/100)/2)*log(odds_ratio_se))
    odds_ratio_Haldane = ((TP+0.5)/(FN+0.5))/((FP+0.5)/(TN+0.5))
    odds_ratio_Haldane_se = exp(sqrt(1/(TP+0.5)+1/(FP+0.5)+1/(TN+0.5)+1/(FN+0.5)))
    odds_ratio_Haldane_low = exp(log(odds_ratio_Haldane) + qnorm((1-CI/100)/2)*log(odds_ratio_Haldane_se))
    odds_ratio_Haldane_upp = exp(log(odds_ratio_Haldane) - qnorm((1-CI/100)/2)*log(odds_ratio_Haldane_se))
    error_odds_ratio = (sensitivity_estimate/(1-sensitivity_estimate))/(specificity/(1-specificity))
    ## Rate ratio
    ## http://omarkasule-05.tripod.com/id52.html
    ## http://sphweb.bumc.bu.edu/otlt/mph-modules/ep/ep713_randomerror/ep713_randomerror4.html
    rate_ratio = (TP/sutuntop1)/(TP/sutuntop2)
    rate_ratio_low = exp(log(relative_risk)+qnorm((1-CI/100)/2)*sqrt(1/TP+1/FP))
    rate_ratio_upp = exp(log(relative_risk)-qnorm((1-CI/100)/2)*sqrt(1/TP+1/FP))
    ## Risk Difference
    ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
    risk_difference = (TP/sutuntop1) - (FN/sutuntop2)
    risk_difference_se = (((TP/sutuntop1)*(1-(TP/sutuntop1)))/sutuntop1)+(((FN/sutuntop2)*(1-(FN/sutuntop2)))/sutuntop2)
    risk_difference_low = risk_difference + qnorm((1-CI/100)/2)*sqrt(risk_difference_se)
    risk_difference_upp = risk_difference - qnorm((1-CI/100)/2)*sqrt(risk_difference_se)
    ## Nitelenebilen risk
    ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
    attributable_risk = (TP/sutuntop1)-(FN/sutuntop2)
    attributable_risk_se = sqrt((satirtop1/toplam)*(1-(satirtop1/toplam))*(1/sutuntop1+1/sutuntop2))
    attributable_risk_low = attributable_risk + qnorm((1-CI/100)/2)*attributable_risk_se
    attributable_risk_upp = attributable_risk - qnorm((1-CI/100)/2)*attributable_risk_se
    ## Attributable risk percent
    ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
    attributable_risk_percent = ((relative_risk-1)/relative_risk)*100
    attributable_risk_percent_low = attributable_risk_percent - (qnorm((1-CI/100)/2)*attributable_risk_se)/attributable_risk
    attributable_risk_percent_upp = attributable_risk_percent + (qnorm((1-CI/100)/2)*attributable_risk_se)/attributable_risk
    ## Population Attributable Risk
    ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
    population_attributable_risk = (satirtop1/toplam)-(FN/sutuntop2)
    ## Population Attributable Risk Percent
    population_attributable_risk_percent = (population_attributable_risk / (satirtop1/toplam))*100
    ## Kohen's Kappa
    cohens_kappa = (efficiency-efficiency_randomtest)/(1-efficiency_randomtest)
    cohens_kappa_se = quality_index_se
    cohens_kappa_low = cohens_kappa + qnorm((1-CI/100)/2)*cohens_kappa_se
    cohens_kappa_upp = cohens_kappa - qnorm((1-CI/100)/2)*cohens_kappa_se
    ## Gozlenen Karar
    observed_agreement = efficiency
    observed_agreement_se = efficiency_se
    observed_agreement_low = efficiency_low
    observed_agreement_upp = efficiency_upp
    ## Risk karari,expected agreement
    chance_agreement = efficiency_randomtest
    ## Pozitif karar
    positive_agreement = 2*TP /(satirtop1+sutuntop1)
    positive_agreement_se = sqrt(4*TP*(FP+FN)*(TP+FP+FN))/(sutuntop1+satirtop1)^2
    positive_agreement_low = positive_agreement + qnorm((1-CI/100)/2)*positive_agreement_se
    positive_agreement_upp = positive_agreement - qnorm((1-CI/100)/2)*positive_agreement_se
    ## Negatif karar
    negative_agreement = 2*TN/(satirtop2+sutuntop2)
    negative_agreement_se = sqrt(4*TN*(FP+FN)*(TN+FP+FN))/(sutuntop2+satirtop2)^2
    negative_agreement_low = negative_agreement + qnorm((1-CI/100)/2)*negative_agreement_se
    negative_agreement_upp = negative_agreement - qnorm((1-CI/100)/2)*negative_agreement_se
    ## Rand index
    ## http://www.cs.pomona.edu/classes/cs158/resources/158-10(EM).pdf
    rand_index = (TP+TN)/(TP+FN+FP+TN)
    ## e-measure
    ## http://michael.hahsler.net/research/recomm_lnai2002/lnai2002.pdf
    e_measure = 1/((((1-CI)/100)*(1-precision))+((1-((1-CI)/100))*(1/recall)))
    ## discriminant power 
    ## https://eva.fing.edu.uy/pluginfile.php/69453/mod_resource/content/1/7633-10048-1-PB.pdf
    discriminant_power = (sqrt(3)/pi)*(log (sensitivity_estimate / (1-sensitivity_estimate)) + log (specificity / (1-specificity)))
    ## F1 Score
    ## http://machinelearningmastery.com/classification-accuracy-is-not-enough-more-performance-measures-you-can-use/
    f1_score = 2*(((TP/(TP+FN))*(TP/(TP+FP)))/((TP/(TP+FN))+(TP/(TP+FP))))
    ## Byrt yanlilik indeksi
    byrt_bias_index = (FN-FP)/toplam
    ## Byrt  asimetrik indeks prevelansi
    byrt_prevalence_asymmetry_index = (TN-TP)/toplam
    ## Yanlilik duzeltmeli kappa
    bias_adjusted_kappa = (efficiency-0.25*((satirtop1+sutuntop1)^2+(satirtop2+sutuntop2)^2)/toplam^2)/(1-0.25*((satirtop1+sutuntop1)^2+(satirtop2+sutuntop2)^2)/toplam^2)
    ## Yanlilik duzeltmeli kappa prevelansi
    prevalence_bias_adjusted_kappa = cohens_kappa+(1-cohens_kappa)*(byrt_prevalence_asymmetry_index^2-byrt_bias_index^2)
    ## Dice'in indeksi (Czekanowski)
    dice_index = positive_agreement
    dice_index_se= positive_agreement_se
    dice_index_low = positive_agreement_low
    dice_index_upp = positive_agreement_upp
    ## Yule's Q
    yules_q = (odds_ratio - 1) / (odds_ratio + 1)
    yules_q_se = 0.5*(1-yules_q^2)*sqrt(1/TP+1/FP+1/TN+1/FN)
    yules_q_low = yules_q + qnorm((1-CI/100)/2)*yules_q_se
    yules_q_upp = yules_q - qnorm((1-CI/100)/2)*yules_q_se
    
    equitable_threatscore = (TP-(sutuntop1*satirtop1/toplam))/(TP+FP+FN-(sutuntop1*satirtop1/toplam))
    ## Phi
    phi = (TP*TN-FN*FP) / sqrt(satirtop1 * satirtop2 * sutuntop1 * sutuntop2)
    phi_se = sqrt((1-phi^2+(phi+0.5*phi^3)*((satirtop1-satirtop2)*(sutuntop1-sutuntop2)/sqrt(satirtop1*satirtop2*sutuntop1*sutuntop2))-0.75*phi^2*((satirtop1-satirtop2)^2/(satirtop1*satirtop2)+(sutuntop1-sutuntop2)^2/(sutuntop1*sutuntop2)))/toplam)
    phi_low = phi + qnorm((1-CI/100)/2)*phi_se
    phi_upp = phi - qnorm((1-CI/100)/2)*phi_se
    ## Cramer V katsayisi
    cramer_v = sqrt((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2))/toplam)
    ## Olaganlik katsayisi
    contingency_coefficient = sqrt((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2))/(((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)))+toplam))
    ## Goodman and Kruskal Gamma 
    goodman_and_kruskal_gamma = (TP*TN-FN*FP)/(TP*TN+FN*FP)
    goodman_and_kruskal_gamma_se = 0.5*(1-yules_q^2)*sqrt(1/TP+1/FP+1/TN+1/FN)
    goodman_and_kruskal_gamma_low = yules_q + qnorm((1-CI/100)/2)*yules_q_se
    goodman_and_kruskal_gamma_upp = yules_q - qnorm((1-CI/100)/2)*yules_q_se
    ## Kendall's tau a
    kendalls_tau_a = (TP*TN-FN*FP)/(toplam*(toplam-1)/2)
    ## Kendall's tau b
    ## https://v8doc.sas.com/sashtml/stat/chap28/sect20.htm
    ## http://slideplayer.com/slide/10766029/
    kendalls_tau_b = (TP*TN-FN*FP)/sqrt((TP*TN+FN*FP+(TP*FN+FP*TN))*(TP*TN+FN*FP+(TP*FP+FN*TN)))
    ## Kendall's tau c
    kendalls_tau_c = ((2*2)*(TP*TN-FN*FP)) / (toplam^2 * (2-1))
    ## Somers'd R|C
    somers_d = (TP*TN-FN*FP)/(TP*TN+FN*FP+(TP*FN+FP*TN))
    ## Somers'd C|R
    somers_d_2 = (TP*TN-FP*FN)/(TP*TN+FP*FN+(TP*FP+FN*TN))
    ## Scoot'un karar indeksi
    scotts_agreement_index = bias_adjusted_kappa
    ## Dort-duzeyli Korelasyon
    tetrachoric_correlation = cos(pi/(1+sqrt(TP*TN/FN/FP)))
    tetrachoric_correlation_se = 0.375*(1-tetrachoric_correlation^2)*sqrt((1/(TP))+(1/(FP))+(1/(TN))+(1/(FN)))
    tetrachoric_correlation_low = tetrachoric_correlation + qnorm((1-CI/100)/2)*tetrachoric_correlation_se
    tetrachoric_correlation_upp = tetrachoric_correlation - qnorm((1-CI/100)/2)*tetrachoric_correlation_se
    ## Goodman kruskal tau katsayisi
    goodman_kruskal_tau = (toplam*((TP^2+FP^2)/sutuntop1+(FN^2+TN^2)/sutuntop2)-satirtop1^2-satirtop2^2)/(toplam^2-satirtop1^2-satirtop2^2)
    goodman_kruskal_tau_se = toplam*sqrt(TP*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop1)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*TP/satirtop1-(TP^2+FN^2)/satirtop1^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+FN*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop2)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*FN/satirtop1-(TP^2+FN^2)/satirtop1^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+FP*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop1)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*FP/satirtop2-(FP^2+TN^2)/satirtop2^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+TN*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop2)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*TN/satirtop2-(FP^2+TN^2)/satirtop2^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2)/(toplam^2-sutuntop1^2-sutuntop2^2)^2
    goodman_kruskal_tau_low = goodman_kruskal_tau + qnorm((1-CI/100)/2)*goodman_kruskal_tau_se
    goodman_kruskal_tau_upp = goodman_kruskal_tau - qnorm((1-CI/100)/2)*goodman_kruskal_tau_se
    ## Simetrik Lambda 
    lambda_symmetric = ((max(TP,FN)+max(FP,TN)+max(TP,FP)+max(FN,TN)-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2))/(2*toplam-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2)))
    if (lambda_symmetric==0 || lambda_symmetric==1)
    {lambda_symmetric_se = 0}
    else
    {
      lambda_symmetric_se = sqrt((TP*((if(TP>FN) 1 else 0) + (if(TP>FP) 1 else 0) + ((if(sutuntop1>sutuntop2) 1 else 0)+ (if(satirtop1>satirtop2) 1 else 0))*(lambda_symmetric-1))^2+FN*( (if(TP>FN) 0 else 1)+ (if(FN>TN) 1 else 0) +( (if(sutuntop1>sutuntop2) 0 else 1) + (if(satirtop1>satirtop2) 1 else 0))*(lambda_symmetric-1))^2+FP*( (if(FP>TN) 1 else 0) + (if(TP>FP) 0 else 1) +( (if(sutuntop1>sutuntop2) 1 else 0) + (if(satirtop1<satirtop2) 1 else 0))*(lambda_symmetric-1))^2+TN*((if(FP>TN) 0 else 1)+ (if(FN>TN) 0 else 1) +((if(sutuntop1>sutuntop2) 0 else 1)+ (if(satirtop1<satirtop2) 1 else 0))*(lambda_symmetric-1))^2-4*toplam*lambda_symmetric^2)/(2*toplam-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2))^2)
    }
    
    lambda_symmetric_low = lambda_symmetric + qnorm((1-CI/100)/2)*lambda_symmetric_se
    lambda_symmetric_upp = lambda_symmetric - qnorm((1-CI/100)/2)*lambda_symmetric_se
    ## Lambda Asymmetric R|C
    lambda_criterion = (max(TP,FP)+max(FN,TN)-max(satirtop1,satirtop2))/(toplam-max(satirtop1,satirtop2))
    
    if (lambda_criterion==0 || lambda_criterion==1)
    {lambda_criterion_se = 0}
    else
    {
      lambda_criterion_se = sqrt((toplam-max(TP,FP)-max(FN,TN))*(max(TP,FP)+max(FN,TN)+max(satirtop1,satirtop2)-2*(if(satirtop1>satirtop2) max(TP,FN) else max(FP,TN)))/(toplam-max(satirtop1,satirtop2))^3)
    }
    
    lambda_criterion_low = lambda_criterion + qnorm((1-CI/100)/2)*lambda_criterion_se
    lambda_criterion_upp = lambda_criterion - qnorm((1-CI/100)/2)*lambda_criterion_se
    ## Lambda Asymmetric C|R
    lambda_criterion_2 = (max(TP,FN)+max(FP,TN)-max(sutuntop1,sutuntop2))/(toplam-max(sutuntop1,sutuntop2))
    
    if (lambda_criterion_2==0 || lambda_criterion_2==1)
    {lambda_criterion_se_2 = 0}
    else
    {
      lambda_criterion_se_2 = sqrt((toplam-max(TP,FN)-max(FP,TN))*(max(TP,FN)+max(FP,TN)+max(sutuntop1,sutuntop2)-2*(if(sutuntop1>sutuntop2) max(TP,FP) else max(FN,TN)))/(toplam-max(sutuntop1,sutuntop2))^3)
    }
    
    lambda_criterion_low_2 = lambda_criterion_2 + qnorm((1-CI/100)/2)*lambda_criterion_se_2
    lambda_criterion_upp_2 = lambda_criterion_2 - qnorm((1-CI/100)/2)*lambda_criterion_se_2
    ## belirsizlik katsayisi (coefficient of uncertainty) simetrik
    uncertainty_coefficient_symmetric = 2*(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam)))/(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))
    uncertainty_coefficient_symmetric_se = 2*sqrt(TP*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop1*sutuntop1/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(TP/toplam))^2+FN*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop1*sutuntop2/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(FN/toplam))^2+FP*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop2*sutuntop1/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(FP/toplam))^2+TN*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop2*sutuntop2/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(TN/toplam))^2)/toplam/(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam)^2
    uncertainty_coefficient_symmetric_low = uncertainty_coefficient_symmetric + qnorm((1-CI/100)/2)*uncertainty_coefficient_symmetric_se
    uncertainty_coefficient_symmetric_upp = uncertainty_coefficient_symmetric - qnorm((1-CI/100)/2)*uncertainty_coefficient_symmetric_se
    ## (coefficient of uncertainty) R|C
    uncertainty_coeff_crit = (satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam)))/(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))
    uncertainty_coeff_crit_se = sqrt(TP*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(TP/sutuntop1)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop1/toplam))^2+FN*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(FN/sutuntop2)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop1/toplam))^2+FP*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(FP/sutuntop1)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop2/toplam))^2+TN*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(TN/sutuntop2)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop2/toplam))^2)/toplam/((satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam)^2
    uncertainty_coeff_crit_low = uncertainty_coeff_crit + qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se
    uncertainty_coeff_crit_upp = uncertainty_coeff_crit - qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se
    ## (coefficient of uncertainty) C|R
    uncertainty_coeff_crit_2 = (sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)+satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)-(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam)))/(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))
    uncertainty_coeff_crit_se_2 = sqrt(TP*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(TP/satirtop1)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop1/toplam))^2+FP*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(FP/satirtop2)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop1/toplam))^2+FN*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(FN/satirtop1)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop2/toplam))^2+TN*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(TN/satirtop2)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop2/toplam))^2)/toplam/((sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)^2
    uncertainty_coeff_crit_low_2 = uncertainty_coeff_crit_2 + qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se_2
    uncertainty_coeff_crit_upp_2 = uncertainty_coeff_crit_2 - qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se_2
    ## Yule s Y (Coefficient of colligation)
    yule_y = (sqrt(TP*TN)-sqrt(FN*FP))/(sqrt(TP*TN)+sqrt(FN*FP))
    ## Pearson ki-kare
    pearson_chi_squ = toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)
    pearson_chi_squ_p = dchi(pearson_chi_squ, df=1)
    ## 
    with_yate_cor_for_pearson = toplam*(abs(TP*TN-FN*FP)-0.5*toplam)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)
    with_yate_cor_p_for_pearson = dchi(with_yate_cor_for_pearson, df=1)
    ## Mantel Haenszel chi-square
    ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
    mantel_haenszel = ((toplam-1)*(TP*TN-FN*FP)^2)/(satirtop1*satirtop2*sutuntop1*sutuntop2)
    mantel_haenszel_p = dchi(mantel_haenszel, df=1)
    ## Olasilik orani
    likelih_ratio = 2*(TP*log(TP/(sutuntop1*satirtop1/toplam))+FN*log(FN/(sutuntop2*satirtop1/toplam))+FP*log(FP/(sutuntop1*satirtop2/toplam))+TN*log(TN/(sutuntop2*satirtop2/toplam)))
    likelih_ratio_p =dchi(likelih_ratio, df=1)
    ## Fisher'in exact testi
    ##fisher_exact_test = (TP-(satirtop1*sutuntop1/toplam))^2/(satirtop1*sutuntop1/toplam) + (FN-(satirtop1*sutuntop2/toplam))^2/(satirtop1*sutuntop2/toplam) + (FP-(satirtop2*sutuntop1/toplam))^2/(satirtop2*sutuntop1/toplam) + (TN-(satirtop2*sutuntop2/toplam))^2/(satirtop2*sutuntop2/toplam)
    fisher <- c(TP, FN, FP, TN)
    tab <- t(matrix(fisher, nrow=2,ncol=2))
    fisher_exact_test = fisher.test(tab)$p.value
    minimum_exp_fre = min((sutuntop1*satirtop1/toplam):(sutuntop2*satirtop2/toplam))
    cells_exp_fre_5 = (if((sutuntop1*satirtop1/toplam)<5) 1 else 0)+ (if((sutuntop1*satirtop2/toplam)<5) 1 else 0)+ (if((sutuntop2*satirtop1/toplam) <5) 1 else 0)+ (if((sutuntop2*satirtop2/toplam) <5) 1 else 0)
    cells_exp_fre_1 = (if((sutuntop1*satirtop1/toplam) <1) 1 else 0)+ (if((sutuntop1*satirtop2/toplam) <1) 1 else 0)+ (if((sutuntop2*satirtop1/toplam) <1) 1 else 0)+ (if((sutuntop2*satirtop2/toplam) <1) 1 else 0)
    ## Mc Nemar testi
    mcNemar_test = (FP-FN)^2/(FP+FN)
    mcNemar_test_p = dchi(mcNemar_test, df=1)
    with_Yate_cor_for_mcnamer = (abs(FP-FN)-1)^2/(FP+FN)
    with_Yate_cor_p_for_mcnamer = dchi(with_Yate_cor_for_mcnamer, df=1)
    
    ## Belirsizlik (Entropi)
    forbes_NMI = sensitivity_estimate*predvalue_positiverandomtest*log((sensitivity_estimate/predvalue_positiverandomtest_mar_ol),base=2)+
      (1-specificity)*(1-predvalue_positiverandomtest)*log(((1-specificity)/predvalue_positiverandomtest_mar_ol),base=2)+
      (1-sensitivity_estimate)*predvalue_positiverandomtest*log(((1-sensitivity_estimate)/(1-predvalue_positiverandomtest_mar_ol)),base=2)+
      specificity*(1-predvalue_positiverandomtest)*log((specificity/(1-predvalue_positiverandomtest_mar_ol)),base=2)
    ## satir icin entropy (test icin)
    entropy_hr = - ((sutuntop1/toplam) * log(sutuntop1/toplam, base=2) + (sutuntop2/toplam) * log(sutuntop2/toplam, base=2))
    ## sutun icin entropy (hastalik icin)  
    entropy_hc = - ((satirtop1/toplam)*log(satirtop1/toplam, base=2) + (satirtop2/toplam)*log(satirtop2/toplam, base=2))
    ## birlesik entropi (joint entropy)  
    entropy_hrc = - ( (TP/toplam)*log(TP/toplam, base=2) + (FP/toplam)*log(FP/toplam, base=2) + (FN/toplam)*log(FN/toplam, base=2) + (TN/toplam)*log(TN/toplam, base=2))
    ## bilgi icerigi (mutual information)  
    information_r_c = entropy_hr + entropy_hc - entropy_hrc
    ## kosullu entropi (conditional entropy)  
    a = entropy_hrc - entropy_hr
    c = entropy_hrc - entropy_hc
    sim_r_c = information_r_c / (a + information_r_c + c)
    dis_r_c = (a + c) / (a + information_r_c + c)
    ## goreli entropi (relative entropy, kullback-leibler uzakligi)
    ## pozitif test sonucu icin goreli entropi, Relative Improvement Over Chance (RIOC)
    positive_relative_entropy = (TP/sutuntop1)*log(((TP/sutuntop1)/ prevalence), base=2)+(FP/sutuntop1)*log(((FP/sutuntop1)/(1- prevalence)), base=2)
    ## negatif test sonucu icin goreli entropi 
    negative_relative_entropy = (FN/sutuntop2)*log(((FN/sutuntop2)/ prevalence), base=2)+(TN/sutuntop2)*log(((TN/sutuntop2)/(1- prevalence)), base=2)
    
    verim <- data.frame(#Estimate=integer(),
      #LowerCI=integer(),
      #UpperCI=integer()
    )
    row <- vector(mode="character")
    if (dice_index >= 0 && dice_index <= 1) 
    {
      verim <- data.frame(Estimate = round(dice_index, digit =3),  LowerCI = round(dice_index_low, digit =3), UpperCI = round(dice_index_upp, digit =3))
      row <- c("Dice\'s Index (Czekanowski)")
    } 
    if (yules_q >= 0 && yules_q <= 1)
    {
      newRow <- data.frame(Estimate = round(yules_q, digit =3),  LowerCI = round(yules_q_low, digit =3), UpperCI = round(yules_q_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Yules Q (Gamma)")
    }
    if (equitable_threatscore >= 0 && equitable_threatscore <= 1)
    {
      newRow <- data.frame(Estimate = round(equitable_threatscore, digit =3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Equitable Threatscore")
    }
    if (phi >= 0 && phi <= 1)
    {
      newRow <- data.frame(Estimate = round(phi, digit =3),  LowerCI = round(phi_low, digit =3), UpperCI = round(phi_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Phi")
    }
    
    ## ?????? Adjusted Contingency Coefficient
    if (contingency_coefficient >= 0 && contingency_coefficient <= 1)
    {
      newRow <- data.frame(Estimate = round(contingency_coefficient, digit =3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Contingency Coefficient")
    }
    if (goodman_and_kruskal_gamma >= 0 && goodman_and_kruskal_gamma <= 1)
    {
      newRow <- data.frame(Estimate = round(goodman_and_kruskal_gamma, digit =3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Goodman and Kruskal Gamma (Gamma Statistic-Gamma Coefficient)")
    }
    if (scotts_agreement_index >= 0 && scotts_agreement_index <= 1)
    {
      newRow <- data.frame(Estimate = round(scotts_agreement_index, digit =3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Scotts agreement index")
    }
    if (tetrachoric_correlation >= 0 && tetrachoric_correlation <= 1)
    {
      newRow <- data.frame(Estimate = round(tetrachoric_correlation, digit =3),  LowerCI = round(tetrachoric_correlation_low, digit =3), UpperCI = round(tetrachoric_correlation_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Tetrachoric (Polychoric) Correlation")
    }
    if (goodman_kruskal_tau >= 0 && goodman_kruskal_tau <= 1)
    {
      newRow <- data.frame(Estimate = round(goodman_kruskal_tau, digit =3),  LowerCI = round(goodman_kruskal_tau_low, digit =3), UpperCI = round(goodman_kruskal_tau_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Goodman & Kruskals tau (Crit. dep.)")
    }
    if (lambda_symmetric >= 0 && lambda_symmetric <= 1)
    {
      newRow <- data.frame(Estimate = round(lambda_symmetric, digit =3),  LowerCI = round(lambda_symmetric_low, digit =3), UpperCI = round(lambda_symmetric_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Lambda (Symmetric)")
    }
    if (lambda_criterion >= 0 && lambda_criterion <= 1)
    {
      newRow <- data.frame(Estimate = round(lambda_criterion, digit =3),  LowerCI = round(lambda_criterion_low, digit =3), UpperCI = round(lambda_criterion_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Lambda (Criterion_dep.)")
    }
    if (uncertainty_coefficient_symmetric >= 0 && uncertainty_coefficient_symmetric <= 1)
    {
      newRow <- data.frame(Estimate = round(uncertainty_coefficient_symmetric, digit =3),  LowerCI = round(uncertainty_coefficient_symmetric_low, digit =3), UpperCI = round(uncertainty_coefficient_symmetric_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Uncertainty Coefficient (Symmetric)")
    }
    if (uncertainty_coeff_crit >= 0 && uncertainty_coeff_crit <= 1)
    {
      newRow <- data.frame(Estimate = round(uncertainty_coeff_crit, digit =3),  LowerCI = round(uncertainty_coeff_crit_low, digit =3), UpperCI = round(uncertainty_coeff_crit_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Uncertainty Coeff. (Criterion_dep.)")
    }
    
    rownames(verim) = c("Dice\'s Index (Czekanowski-Sorenson)", "Yules Q (Gamma)", "Equitable Threatscore (Gilbert Skill Score)", "Phi", "Contingency Coefficient", 
                        "Goodman and Kruskal Gamma (Gamma Statistic-Gamma Coefficient)","Scotts agreement index", "Tetrachoric Correlation", 
                        "Goodman & Kruskals tau (Crit. dep.)", 
                        "Lambda (Symmetric)", "Lambda (Criterion_dep.)", "Uncertainty Coefficient (Symmetric)", 
                        "Uncertainty Coeff. (Criterion_dep.)")
    
    
    
    highchart() %>% hc_exporting(enabled = TRUE, filename = "Plot1") %>% 
      hc_add_series(name = "Conf. Interval", type = "bar", data = verim$`Estimate`) %>%
      hc_add_series(name = "CI", data = as.matrix(cbind(verim$`LowerCI`, verim$`UpperCI`)),type = "errorbar", names = "Limits", showInLegend = FALSE, zIndex = 0, lineWidth = 1.5, linkedTo = "CI"
      ) %>%
      hc_xAxis(categories = rownames(verim),title = "Association Measures") %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, headerFormat = "<b>Association Measures: </b>{point.x} <br>") %>%
      hc_plotOptions(line = list(tooltip = list(pointFormat = "<b>{series.name}: </b>{point.y:.3f} ")), 
                     errorbar = list(tooltip = list(pointFormat = "({point.low} - {point.high})"))) %>%
      hc_add_theme(hc_theme_google())
  })
  
  ## Agreement  
  dataM2 <- reactive({   
    if (is.null(trimws(input$text3)) && is.null(trimws(input$text4)))  {return(NULL)}
    else {
      if (input$verigir_kontrol == TRUE){ 
        TP = 0
        FN = 0
        FP = 0
        TN = 0
        for (i in 1:nrow(tabloveri()))
        {
          if (tabloveri()[i,1]==1 & tabloveri()[i,2]==1)
          {TP = TP+1}
          else if (tabloveri()[i,1]==1 & tabloveri()[i,2]==2)
          {FN = FN+1}
          else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==1)
          {FP = FP+1}
          else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==2)
          {TN = TN+1}
          next
        }
        CI = as.numeric(trimws(input$text5_2))
      }
      else{
        TP = as.numeric(trimws(input$text1)) ### x
        FN = as.numeric(trimws(input$text2)) ### y
        FP = as.numeric(trimws(input$text3)) ### z
        TN = as.numeric(trimws(input$text4)) ### t
        CI = as.numeric(trimws(input$text5))
      }
      
      
      satirtop1 = TP + FN
      satirtop2 = FP + TN
      sutuntop1 = TP + FP
      sutuntop2 = FN + TN
      toplam = satirtop1 + satirtop2
      #table1 <- matrix(c(TP,FN,satirtop1,FP,TN,satirtop2,sutuntop1,sutuntop2,toplam),nrow=3,ncol=3,byrow=TRUE,dimnames=list(c("1.satir","2.satir","Sutun toplamlari"),c("1.kolon","2.kolon","Satir toplamlari")))
      #table1
      ## Duyarlilik 
      sensitivity_estimate = TP / satirtop1
      sensitivity_se = round((sqrt(sensitivity_estimate * (1 - sensitivity_estimate)/satirtop1)), 4)
      sensitivity_low = 1 - Rbeta.inv(((1 + CI/100)/2), (satirtop1+1-TP), TP)
      sensitivity_upp = Rbeta.inv(((1 + CI/100)/2), (TP+1), (satirtop1-TP))
      sensitivity_randomtest = sutuntop1 / toplam
      qualityindex_sensitivity = (sensitivity_estimate - sensitivity_randomtest) / (1 - sensitivity_randomtest)
      ## Secicilik
      specificity = TN / satirtop2
      specificity_se = sqrt(specificity * (1 - specificity) / satirtop2)
      specificity_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop2+1-TN),TN)
      specificity_upp = Rbeta.inv((1 + CI/100)/2, (TN+1), (satirtop2-TN))
      specificity_randomtest = (1 - sensitivity_randomtest)
      qualityindex_specificity = (specificity - 1 + sensitivity_randomtest) / sensitivity_randomtest
      ## Gain in Certainty
      gain_in_certainty = sensitivity_estimate + specificity
      ## Etkililik, yeterlilik ayni zamanda overall accuracy (toplam-genel dogruluk orani)
      efficiency = (TP + TN) / toplam
      efficiency_se = sqrt(efficiency * (1 - efficiency) / toplam)
      efficiency_low = 1-Rbeta.inv((1 + CI/100)/2, (toplam+1-TP-TN), (TP+TN))
      efficiency_upp = Rbeta.inv((1 + CI/100)/2, (TP+TN+1), (toplam-TP-TN))
      efficiency_randomtest = (satirtop1 / toplam) * sensitivity_randomtest + (1 - (satirtop1 / toplam)) * (1 - sensitivity_randomtest)
      mis_efficiency = 1 - efficiency
      ## Quality indeks
      quality_index = (efficiency - efficiency_randomtest) / (1 - efficiency_randomtest)
      quality_index_se = sqrt((efficiency*(1-efficiency)/(1-efficiency_randomtest)^2+2*(1-efficiency)*(2*efficiency*efficiency_randomtest-((TP*((satirtop1 / toplam)+sensitivity_randomtest)+TN*(2-(satirtop1 / toplam)-sensitivity_randomtest))/toplam))/(1-efficiency_randomtest)^3+((1-efficiency)^2)*((TP*((satirtop1 / toplam)+sensitivity_randomtest)^2+FN*(-(satirtop1 / toplam)+1+sensitivity_randomtest)^2+FP*(1+(satirtop1 / toplam)-sensitivity_randomtest)^2+TN*(2-(satirtop1 / toplam)-sensitivity_randomtest)^2)/toplam-4*efficiency_randomtest^2)/(1-efficiency_randomtest)^4)/toplam)
      quality_index_low = quality_index + qnorm((1 - CI/100)/2)*quality_index_se
      quality_index_upp = quality_index - qnorm((1 - CI/100)/2)*quality_index_se
      ## Youden indeksi
      youdens_index = sensitivity_estimate + specificity - 1
      youdens_index_se = sqrt(sensitivity_se^2+specificity_se^2)
      youdens_index_low = youdens_index + qnorm((1 - CI/100)/2)*youdens_index_se
      youdens_index_upp = youdens_index - qnorm((1 - CI/100)/2)*youdens_index_se
      
      number_needed_to_diagnose = 1 / youdens_index
      number_needed_to_misdiagnose = 1 / ( 1 - efficiency ) 
      ## Pozitif kestirim degeri
      predictivevalue_positivetest = TP / sutuntop1
      predictivevalue_positivetest_se = sqrt(predictivevalue_positivetest*(1-predictivevalue_positivetest)/sutuntop1)
      predictivevalue_positivetest_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop1+1-TP),TP)
      predictivevalue_positivetest_upp = Rbeta.inv((1 + CI/100)/2, (TP+1),(sutuntop1-TP))
      predvalue_positiverandomtest = (satirtop1 / toplam)
      predvalue_positiverandomtest_mar_ol = predvalue_positiverandomtest*sensitivity_estimate+(1-predvalue_positiverandomtest)*(1-specificity)
      ## Negatif kestirim degeri
      predictivevalue_negativetest = TN / sutuntop2
      predictivevalue_negativetest_se = sqrt(predictivevalue_negativetest*(1-predictivevalue_negativetest)/sutuntop2)
      predictivevalue_negativetest_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop2+1-TN),TN)
      predictivevalue_negativetest_upp = Rbeta.inv((1 + CI/100)/2, (TN+1), (sutuntop2-TN))
      predvalue_negativerandomtest = 1 - (satirtop1 / toplam)
      predvalue_negativerandomtest_mar_ol = 1 - predvalue_positiverandomtest_mar_ol
      ## Predictive Summary Index (PSI)
      predictive_summary_index = predictivevalue_positivetest + predictivevalue_negativetest - 1
      ## Yanlis pozitif orani
      false_positiverate = FP / satirtop2
      false_positiverate_se = sqrt(false_positiverate*(1-false_positiverate)/satirtop2)
      false_positiverate_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop2+1-FP),FP)
      false_positiverate_upp = Rbeta.inv((1 + CI/100)/2, (FP+1), (satirtop2-FP))
      ## Yansis negatif orani
      false_negativerate = FN / satirtop1
      false_negativerate_se = sqrt(false_negativerate*(1-false_negativerate)/satirtop1)
      false_negativerate_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop1+1-FN),FN)
      false_negativerate_upp = Rbeta.inv((1 + CI/100)/2, (FN+1), (satirtop1-FN))
      ## False Omission Rate (FOR) https://www.ncss.com/software/ncss/diagnostic-tests-in-ncss/
      false_omission_rate = FN/(sutuntop2)
      false_omission_rate_se = sqrt(false_omission_rate*(1-false_omission_rate)/sutuntop2)
      false_omission_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop2+1-FN),FN)
      false_omission_rate_upp = Rbeta.inv((1 + CI/100)/2, (FN+1), (sutuntop2-FN))
      ## False Discovery Rate (FDR) https://www.ncss.com/software/ncss/diagnostic-tests-in-ncss/
      false_discovery_rate = FP/(sutuntop1)
      false_discovery_rate_se = sqrt(false_discovery_rate*(1-false_discovery_rate)/sutuntop1)
      false_discovery_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop1+1-FP),FP)
      false_discovery_rate_upp = Rbeta.inv((1 + CI/100)/2, (FP+1), (sutuntop1-FP))
      ## Dogru pozitif orani
      true_positiverate = TP / (satirtop1)
      ## Yanlis siniflandirma orani
      misclassification_rate = (FN + FP) / toplam
      misclassification_rate_se = sqrt(misclassification_rate*(1-misclassification_rate)/toplam)
      misclassification_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-FP-FN), (FP+FN))
      misclassification_rate_upp = Rbeta.inv((1 + CI/100)/2, (FP+FN+1), (toplam-FP-FN))
      ## Prevelans
      prevalence = (satirtop1 / toplam)
      prevalence_se = sqrt((satirtop1 / toplam)*(1-(satirtop1 / toplam))/toplam)
      prevalence_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-satirtop1), satirtop1)
      prevalence_upp = Rbeta.inv((1 + CI/100)/2, (satirtop1+1), (toplam-satirtop1))
      
      ##caret paketinden olculer
      detection_rate = TP/toplam
      detection_prevalence = (TP+FP)/toplam
      balanced_accuracy = (sensitivity_estimate + specificity)/2
      lift = (TP/(TP+FN))/((TP+FP)/toplam)  ## https://gerardnico.com/wiki/data_mining/error_rate
      precision = TP/(TP+FP)
      ## http://michael.hahsler.net/research/recomm_lnai2002/lnai2002.pdf
      conviction = (1-((TP+FP)/toplam))/(1-(TP/(TP+FN)))
      recall = TP/(TP+FN)
      matthews_correlation_coefficient = (TP * TN + FN * FP) / ((TP+FP)*(FN+TN)*(FP+TN)*(TP+FN))^(1/2)
      ## Goreli Risk Orani (Relative risk)
      ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
      # https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20ratio.htm
      relative_risk = (TP/sutuntop1)/(FN/sutuntop2)
      relative_risk_se = sqrt((1/TP)+(1/FN)-(1/(TP+FP))-(1/(FN+TN)))
      relative_risk_low = exp(log(relative_risk)+qnorm((1-CI/100)/2)*relative_risk_se)
      relative_risk_upp = exp(log(relative_risk)-qnorm((1-CI/100)/2)*relative_risk_se)
      difference_in_proportion = (TP/sutuntop1) - (FN/sutuntop2)
      ## Leverage
      ## https://www3.nd.edu/~tweninge/pubs/AHBCW_ANNIE08.pdf
      leverage = (TP/sutuntop1)-((TP+FP)/toplam)
      ## http://www.ebm.med.ualberta.ca/TherapyCalc.html
      control_event_rate = FN/sutuntop2
      experimental_event_rate = TP/sutuntop1
      ## Absolute risk reduction
      absolute_risk_reduction = -difference_in_proportion
      absolute_risk_reduction_se = sqrt((TP/sutuntop1)*(1-(TP/sutuntop1))/sutuntop1+(FN/sutuntop2)*(1-(FN/sutuntop2))/sutuntop2)
      absolute_risk_reduction_low = absolute_risk_reduction - qnorm((1-CI/100)/2)*absolute_risk_reduction_se
      absolute_risk_reduction_upp = absolute_risk_reduction + qnorm((1-CI/100)/2)*absolute_risk_reduction_se
      ## Relative risk reduction
      relative_risk_reduction = absolute_risk_reduction / (FN/sutuntop2)
      relative_risk_reduction_low = 1 - exp(log(relative_risk)-qnorm((1-CI/100)/2)*relative_risk_se)
      relative_risk_reduction_upp = 1 - exp(log(relative_risk)+qnorm((1-CI/100)/2)*relative_risk_se)
      ##Number needed to treat
      number_needed_to_treat = 1 / abs(difference_in_proportion)
      number_needed_to_treat_low = 1 /absolute_risk_reduction_upp
      number_needed_to_treat_upp = 1 /absolute_risk_reduction_low
      ## Test duzeyi
      test_level = sensitivity_randomtest
      test_level_se = sqrt(test_level*(1-test_level)/toplam)
      test_level_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-sutuntop1),sutuntop1)
      test_level_upp = Rbeta.inv((1 + CI/100)/2, (sutuntop1+1), (toplam-sutuntop1))
      ## Pre-test odds
      pretest_odds = prevalence / (1 - prevalence)
      ## Pozitif Olabilirlik Orani
      likelihoodratio_positivetest = sensitivity_estimate/(1-specificity)
      likelihoodratio_positivetest_se = exp(sqrt((1-sensitivity_estimate)/(sensitivity_estimate*satirtop1) +specificity/((1-specificity)*satirtop2)))
      likelihoodratio_positivetest_low = exp(log(likelihoodratio_positivetest) + qnorm((1 - CI/100)/2)*log(likelihoodratio_positivetest_se))
      likelihoodratio_positivetest_upp = exp(log(likelihoodratio_positivetest) - qnorm((1 - CI/100)/2)*log(likelihoodratio_positivetest_se))
      ## post-test odds
      posttest_odds = pretest_odds * likelihoodratio_positivetest
      bayes_ppv = (sensitivity_estimate * prevalence)/((sensitivity_estimate * prevalence) + (1 - specificity) * (1 - prevalence))
      ## Post-test probability (positive test result)
      posttest_probability = (pretest_odds * likelihoodratio_positivetest) / (1 + (pretest_odds * likelihoodratio_positivetest))
      ## Negatif Olabilirlik Orani
      likelihoodratio_negativetest = (1-sensitivity_estimate)/specificity
      likelihoodratio_negativetest_se = exp(sqrt(sensitivity_estimate/((1-sensitivity_estimate)*satirtop1) +(1-specificity)/(specificity*satirtop2)))
      likelihoodratio_negativetest_low = exp(log(likelihoodratio_negativetest) + qnorm((1-CI/100)/2)*log(likelihoodratio_negativetest_se))
      likelihoodratio_negativetest_upp = exp(log(likelihoodratio_negativetest) - qnorm((1-CI/100)/2)*log(likelihoodratio_negativetest_se))
      bayes_npv = (sensitivity_estimate * (1 - prevalence))/((sensitivity_estimate * (1 - prevalence)) + (1 - specificity) * prevalence)
      ## Post-test probability (negative test result)
      posttest_probability_neg = (pretest_odds * likelihoodratio_negativetest) / (1 + (pretest_odds * likelihoodratio_negativetest))
      ## Ters negatif olabilirlik orani
      inverse_likelihoodratio_negativetest = specificity/(1-sensitivity_estimate)
      inverse_likelihoodratio_negativetest_se = exp(sqrt(sensitivity_estimate/((1-sensitivity_estimate)*satirtop1) +(1-specificity)/(specificity*satirtop2)))
      inverse_likelihoodratio_negativetest_low = exp(log(inverse_likelihoodratio_negativetest) + qnorm((1-CI/100)/2)*log(inverse_likelihoodratio_negativetest_se))
      inverse_likelihoodratio_negativetest_upp = exp(log(inverse_likelihoodratio_negativetest) - qnorm((1-CI/100)/2)*log(inverse_likelihoodratio_negativetest_se))
      ## Diagnostic Odds Ratio (Tani Odds Orani)
      odds_ratio = (TP/FN)/(FP/TN)
      odds_ratio_se = exp(sqrt(1/TP+1/FP+1/TN+1/FN))
      odds_ratio_low = exp(log(odds_ratio) + qnorm((1-CI/100)/2)*log(odds_ratio_se))
      odds_ratio_upp = exp(log(odds_ratio) - qnorm((1-CI/100)/2)*log(odds_ratio_se))
      odds_ratio_Haldane = ((TP+0.5)/(FN+0.5))/((FP+0.5)/(TN+0.5))
      odds_ratio_Haldane_se = exp(sqrt(1/(TP+0.5)+1/(FP+0.5)+1/(TN+0.5)+1/(FN+0.5)))
      odds_ratio_Haldane_low = exp(log(odds_ratio_Haldane) + qnorm((1-CI/100)/2)*log(odds_ratio_Haldane_se))
      odds_ratio_Haldane_upp = exp(log(odds_ratio_Haldane) - qnorm((1-CI/100)/2)*log(odds_ratio_Haldane_se))
      error_odds_ratio = (sensitivity_estimate/(1-sensitivity_estimate))/(specificity/(1-specificity))
      ## Rate ratio
      ## http://omarkasule-05.tripod.com/id52.html
      ## http://sphweb.bumc.bu.edu/otlt/mph-modules/ep/ep713_randomerror/ep713_randomerror4.html
      rate_ratio = (TP/sutuntop1)/(TP/sutuntop2)
      rate_ratio_low = exp(log(relative_risk)+qnorm((1-CI/100)/2)*sqrt(1/TP+1/FP))
      rate_ratio_upp = exp(log(relative_risk)-qnorm((1-CI/100)/2)*sqrt(1/TP+1/FP))
      ## Risk Difference
      ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
      risk_difference = (TP/sutuntop1) - (FN/sutuntop2)
      risk_difference_se = (((TP/sutuntop1)*(1-(TP/sutuntop1)))/sutuntop1)+(((FN/sutuntop2)*(1-(FN/sutuntop2)))/sutuntop2)
      risk_difference_low = risk_difference + qnorm((1-CI/100)/2)*sqrt(risk_difference_se)
      risk_difference_upp = risk_difference - qnorm((1-CI/100)/2)*sqrt(risk_difference_se)
      ## Nitelenebilen risk
      ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
      attributable_risk = (TP/sutuntop1)-(FN/sutuntop2)
      attributable_risk_se = sqrt((satirtop1/toplam)*(1-(satirtop1/toplam))*(1/sutuntop1+1/sutuntop2))
      attributable_risk_low = attributable_risk + qnorm((1-CI/100)/2)*attributable_risk_se
      attributable_risk_upp = attributable_risk - qnorm((1-CI/100)/2)*attributable_risk_se
      ## Attributable risk percent
      ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
      attributable_risk_percent = ((relative_risk-1)/relative_risk)*100
      attributable_risk_percent_low = attributable_risk_percent - (qnorm((1-CI/100)/2)*attributable_risk_se)/attributable_risk
      attributable_risk_percent_upp = attributable_risk_percent + (qnorm((1-CI/100)/2)*attributable_risk_se)/attributable_risk
      ## Population Attributable Risk
      ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
      population_attributable_risk = (satirtop1/toplam)-(FN/sutuntop2)
      ## Population Attributable Risk Percent
      population_attributable_risk_percent = (population_attributable_risk / (satirtop1/toplam))*100
      ## percent agreement
      percent_agreement = ((TP + TN) / toplam)*100
      ## proportion of expected agreement
      prop_expected_agreement = ((TP+FP)*(TP+FN)+(TN+FP)*(TN+FN))/toplam
      ## proportion of pozitif agreement
      prop_pozitif_agreement = 2*TP/(toplam+TP-TN)
      ### proportion of negatif agreement
      prop_negatif_agreement = 2*TN/(toplam-TP+TN)
      ## Kohen's Kappa
      cohens_kappa = (efficiency-efficiency_randomtest)/(1-efficiency_randomtest)
      cohens_kappa_se = quality_index_se
      cohens_kappa_low = cohens_kappa + qnorm((1-CI/100)/2)*cohens_kappa_se
      cohens_kappa_upp = cohens_kappa - qnorm((1-CI/100)/2)*cohens_kappa_se
      ## Gozlenen Karar
      observed_agreement = efficiency
      observed_agreement_se = efficiency_se
      observed_agreement_low = efficiency_low
      observed_agreement_upp = efficiency_upp
      ## Risk karari,expected agreement
      chance_agreement = efficiency_randomtest
      ## Pozitif karar
      positive_agreement = 2*TP /(satirtop1+sutuntop1)
      positive_agreement_se = sqrt(4*TP*(FP+FN)*(TP+FP+FN))/(sutuntop1+satirtop1)^2
      positive_agreement_low = positive_agreement + qnorm((1-CI/100)/2)*positive_agreement_se
      positive_agreement_upp = positive_agreement - qnorm((1-CI/100)/2)*positive_agreement_se
      ## Negatif karar
      negative_agreement = 2*TN/(satirtop2+sutuntop2)
      negative_agreement_se = sqrt(4*TN*(FP+FN)*(TN+FP+FN))/(sutuntop2+satirtop2)^2
      negative_agreement_low = negative_agreement + qnorm((1-CI/100)/2)*negative_agreement_se
      negative_agreement_upp = negative_agreement - qnorm((1-CI/100)/2)*negative_agreement_se
      ## Rand index
      ## http://www.cs.pomona.edu/classes/cs158/resources/158-10(EM).pdf
      rand_index = (TP+TN)/(TP+FN+FP+TN)
      ## e-measure
      ## http://michael.hahsler.net/research/recomm_lnai2002/lnai2002.pdf
      e_measure = 1/((((1-CI)/100)*(1-precision))+((1-((1-CI)/100))*(1/recall)))
      ## discriminant power 
      ## https://eva.fing.edu.uy/pluginfile.php/69453/mod_resource/content/1/7633-10048-1-PB.pdf
      discriminant_power = (sqrt(3)/pi)*(log (sensitivity_estimate / (1-sensitivity_estimate)) + log (specificity / (1-specificity)))
      ## F1 Score
      ## http://machinelearningmastery.com/classification-accuracy-is-not-enough-more-performance-measures-you-can-use/
      f1_score = 2*(((TP/(TP+FN))*(TP/(TP+FP)))/((TP/(TP+FN))+(TP/(TP+FP))))
      ## Byrt yanlilik indeksi
      byrt_bias_index = (FN-FP)/toplam
      ## Byrt  asimetrik indeks prevelansi
      byrt_prevalence_asymmetry_index = (TN-TP)/toplam
      ## Yanlilik duzeltmeli kappa
      bias_adjusted_kappa = (efficiency-0.25*((satirtop1+sutuntop1)^2+(satirtop2+sutuntop2)^2)/toplam^2)/(1-0.25*((satirtop1+sutuntop1)^2+(satirtop2+sutuntop2)^2)/toplam^2)
      ## Yanlilik duzeltmeli kappa prevelansi
      prevalence_bias_adjusted_kappa = cohens_kappa+(1-cohens_kappa)*(byrt_prevalence_asymmetry_index^2-byrt_bias_index^2)
      ## Dice'in indeksi (Czekanowski)
      dice_index = positive_agreement
      dice_index_se= positive_agreement_se
      dice_index_low = positive_agreement_low
      dice_index_upp = positive_agreement_upp
      ## Yule's Q
      yules_q = (odds_ratio - 1) / (odds_ratio + 1)
      yules_q_se = 0.5*(1-yules_q^2)*sqrt(1/TP+1/FP+1/TN+1/FN)
      yules_q_low = yules_q + qnorm((1-CI/100)/2)*yules_q_se
      yules_q_upp = yules_q - qnorm((1-CI/100)/2)*yules_q_se
      
      equitable_threatscore = (TP-(sutuntop1*satirtop1/toplam))/(TP+FP+FN-(sutuntop1*satirtop1/toplam))
      ## Phi
      phi = (TP*TN-FN*FP) / sqrt(satirtop1 * satirtop2 * sutuntop1 * sutuntop2)
      phi_se = sqrt((1-phi^2+(phi+0.5*phi^3)*((satirtop1-satirtop2)*(sutuntop1-sutuntop2)/sqrt(satirtop1*satirtop2*sutuntop1*sutuntop2))-0.75*phi^2*((satirtop1-satirtop2)^2/(satirtop1*satirtop2)+(sutuntop1-sutuntop2)^2/(sutuntop1*sutuntop2)))/toplam)
      phi_low = phi + qnorm((1-CI/100)/2)*phi_se
      phi_upp = phi - qnorm((1-CI/100)/2)*phi_se
      ## Cramer V katsayisi
      cramer_v = sqrt((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2))/toplam)
      ## Olaganlik katsayisi
      contingency_coefficient = sqrt((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2))/(((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)))+toplam))
      ## Goodman and Kruskal Gamma 
      goodman_and_kruskal_gamma = (TP*TN-FN*FP)/(TP*TN+FN*FP)
      goodman_and_kruskal_gamma_se = 0.5*(1-yules_q^2)*sqrt(1/TP+1/FP+1/TN+1/FN)
      goodman_and_kruskal_gamma_low = yules_q + qnorm((1-CI/100)/2)*yules_q_se
      goodman_and_kruskal_gamma_upp = yules_q - qnorm((1-CI/100)/2)*yules_q_se
      ## Kendall's tau a
      kendalls_tau_a = (TP*TN-FN*FP)/(toplam*(toplam-1)/2)
      ## Kendall's tau b
      ## https://v8doc.sas.com/sashtml/stat/chap28/sect20.htm
      ## http://slideplayer.com/slide/10766029/
      kendalls_tau_b = (TP*TN-FN*FP)/sqrt((TP*TN+FN*FP+(TP*FN+FP*TN))*(TP*TN+FN*FP+(TP*FP+FN*TN)))
      ## Kendall's tau c
      kendalls_tau_c = ((2*2)*(TP*TN-FN*FP)) / (toplam^2 * (2-1))
      ## Somers'd R|C
      somers_d = (TP*TN-FN*FP)/(TP*TN+FN*FP+(TP*FN+FP*TN))
      ## Somers'd C|R
      somers_d_2 = (TP*TN-FP*FN)/(TP*TN+FP*FN+(TP*FP+FN*TN))
      ## Scoot'un karar indeksi
      scotts_agreement_index = bias_adjusted_kappa
      ## Dort-duzeyli Korelasyon
      tetrachoric_correlation = cos(pi/(1+sqrt(TP*TN/FN/FP)))
      tetrachoric_correlation_se = 0.375*(1-tetrachoric_correlation^2)*sqrt((1/(TP))+(1/(FP))+(1/(TN))+(1/(FN)))
      tetrachoric_correlation_low = tetrachoric_correlation + qnorm((1-CI/100)/2)*tetrachoric_correlation_se
      tetrachoric_correlation_upp = tetrachoric_correlation - qnorm((1-CI/100)/2)*tetrachoric_correlation_se
      ## Goodman kruskal tau katsayisi
      goodman_kruskal_tau = (toplam*((TP^2+FP^2)/sutuntop1+(FN^2+TN^2)/sutuntop2)-satirtop1^2-satirtop2^2)/(toplam^2-satirtop1^2-satirtop2^2)
      goodman_kruskal_tau_se = toplam*sqrt(TP*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop1)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*TP/satirtop1-(TP^2+FN^2)/satirtop1^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+FN*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop2)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*FN/satirtop1-(TP^2+FN^2)/satirtop1^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+FP*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop1)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*FP/satirtop2-(FP^2+TN^2)/satirtop2^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+TN*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop2)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*TN/satirtop2-(FP^2+TN^2)/satirtop2^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2)/(toplam^2-sutuntop1^2-sutuntop2^2)^2
      goodman_kruskal_tau_low = goodman_kruskal_tau + qnorm((1-CI/100)/2)*goodman_kruskal_tau_se
      goodman_kruskal_tau_upp = goodman_kruskal_tau - qnorm((1-CI/100)/2)*goodman_kruskal_tau_se
      ## Simetrik Lambda 
      lambda_symmetric = ((max(TP,FN)+max(FP,TN)+max(TP,FP)+max(FN,TN)-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2))/(2*toplam-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2)))
      if (lambda_symmetric==0 || lambda_symmetric==1)
      {lambda_symmetric_se = 0}
      else
      {
        lambda_symmetric_se = sqrt((TP*((if(TP>FN) 1 else 0) + (if(TP>FP) 1 else 0) + ((if(sutuntop1>sutuntop2) 1 else 0)+ (if(satirtop1>satirtop2) 1 else 0))*(lambda_symmetric-1))^2+FN*( (if(TP>FN) 0 else 1)+ (if(FN>TN) 1 else 0) +( (if(sutuntop1>sutuntop2) 0 else 1) + (if(satirtop1>satirtop2) 1 else 0))*(lambda_symmetric-1))^2+FP*( (if(FP>TN) 1 else 0) + (if(TP>FP) 0 else 1) +( (if(sutuntop1>sutuntop2) 1 else 0) + (if(satirtop1<satirtop2) 1 else 0))*(lambda_symmetric-1))^2+TN*((if(FP>TN) 0 else 1)+ (if(FN>TN) 0 else 1) +((if(sutuntop1>sutuntop2) 0 else 1)+ (if(satirtop1<satirtop2) 1 else 0))*(lambda_symmetric-1))^2-4*toplam*lambda_symmetric^2)/(2*toplam-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2))^2)
      }
      
      lambda_symmetric_low = lambda_symmetric + qnorm((1-CI/100)/2)*lambda_symmetric_se
      lambda_symmetric_upp = lambda_symmetric - qnorm((1-CI/100)/2)*lambda_symmetric_se
      ## Lambda Asymmetric R|C
      lambda_criterion = (max(TP,FP)+max(FN,TN)-max(satirtop1,satirtop2))/(toplam-max(satirtop1,satirtop2))
      
      if (lambda_criterion==0 || lambda_criterion==1)
      {lambda_criterion_se = 0}
      else
      {
        lambda_criterion_se = sqrt((toplam-max(TP,FP)-max(FN,TN))*(max(TP,FP)+max(FN,TN)+max(satirtop1,satirtop2)-2*(if(satirtop1>satirtop2) max(TP,FN) else max(FP,TN)))/(toplam-max(satirtop1,satirtop2))^3)
      }
      
      lambda_criterion_low = lambda_criterion + qnorm((1-CI/100)/2)*lambda_criterion_se
      lambda_criterion_upp = lambda_criterion - qnorm((1-CI/100)/2)*lambda_criterion_se
      ## Lambda Asymmetric C|R
      lambda_criterion_2 = (max(TP,FN)+max(FP,TN)-max(sutuntop1,sutuntop2))/(toplam-max(sutuntop1,sutuntop2))
      
      if (lambda_criterion_2==0 || lambda_criterion_2==1)
      {lambda_criterion_se_2 = 0}
      else
      {
        lambda_criterion_se_2 = sqrt((toplam-max(TP,FN)-max(FP,TN))*(max(TP,FN)+max(FP,TN)+max(sutuntop1,sutuntop2)-2*(if(sutuntop1>sutuntop2) max(TP,FP) else max(FN,TN)))/(toplam-max(sutuntop1,sutuntop2))^3)
      }
      
      lambda_criterion_low_2 = lambda_criterion_2 + qnorm((1-CI/100)/2)*lambda_criterion_se_2
      lambda_criterion_upp_2 = lambda_criterion_2 - qnorm((1-CI/100)/2)*lambda_criterion_se_2
      ## belirsizlik katsayisi (coefficient of uncertainty) simetrik
      uncertainty_coefficient_symmetric = 2*(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam)))/(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))
      uncertainty_coefficient_symmetric_se = 2*sqrt(TP*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop1*sutuntop1/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(TP/toplam))^2+FN*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop1*sutuntop2/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(FN/toplam))^2+FP*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop2*sutuntop1/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(FP/toplam))^2+TN*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop2*sutuntop2/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(TN/toplam))^2)/toplam/(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam)^2
      uncertainty_coefficient_symmetric_low = uncertainty_coefficient_symmetric + qnorm((1-CI/100)/2)*uncertainty_coefficient_symmetric_se
      uncertainty_coefficient_symmetric_upp = uncertainty_coefficient_symmetric - qnorm((1-CI/100)/2)*uncertainty_coefficient_symmetric_se
      ## (coefficient of uncertainty) R|C
      uncertainty_coeff_crit = (satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam)))/(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))
      uncertainty_coeff_crit_se = sqrt(TP*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(TP/sutuntop1)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop1/toplam))^2+FN*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(FN/sutuntop2)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop1/toplam))^2+FP*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(FP/sutuntop1)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop2/toplam))^2+TN*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(TN/sutuntop2)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop2/toplam))^2)/toplam/((satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam)^2
      uncertainty_coeff_crit_low = uncertainty_coeff_crit + qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se
      uncertainty_coeff_crit_upp = uncertainty_coeff_crit - qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se
      ## (coefficient of uncertainty) C|R
      uncertainty_coeff_crit_2 = (sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)+satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)-(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam)))/(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))
      uncertainty_coeff_crit_se_2 = sqrt(TP*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(TP/satirtop1)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop1/toplam))^2+FP*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(FP/satirtop2)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop1/toplam))^2+FN*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(FN/satirtop1)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop2/toplam))^2+TN*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(TN/satirtop2)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop2/toplam))^2)/toplam/((sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)^2
      uncertainty_coeff_crit_low_2 = uncertainty_coeff_crit_2 + qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se_2
      uncertainty_coeff_crit_upp_2 = uncertainty_coeff_crit_2 - qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se_2
      ## Yule s Y (Coefficient of colligation)
      yule_y = (sqrt(TP*TN)-sqrt(FN*FP))/(sqrt(TP*TN)+sqrt(FN*FP))
      ## Pearson ki-kare
      pearson_chi_squ = toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)
      pearson_chi_squ_p = dchi(pearson_chi_squ, df=1)
      ## 
      with_yate_cor_for_pearson = toplam*(abs(TP*TN-FN*FP)-0.5*toplam)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)
      with_yate_cor_p_for_pearson = dchi(with_yate_cor_for_pearson, df=1)
      ## Mantel Haenszel chi-square
      ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
      mantel_haenszel = ((toplam-1)*(TP*TN-FN*FP)^2)/(satirtop1*satirtop2*sutuntop1*sutuntop2)
      mantel_haenszel_p = dchi(mantel_haenszel, df=1)
      ## Olasilik orani
      likelih_ratio = 2*(TP*log(TP/(sutuntop1*satirtop1/toplam))+FN*log(FN/(sutuntop2*satirtop1/toplam))+FP*log(FP/(sutuntop1*satirtop2/toplam))+TN*log(TN/(sutuntop2*satirtop2/toplam)))
      likelih_ratio_p =dchi(likelih_ratio, df=1)
      ## Fisher'in exact testi
      ##fisher_exact_test = (TP-(satirtop1*sutuntop1/toplam))^2/(satirtop1*sutuntop1/toplam) + (FN-(satirtop1*sutuntop2/toplam))^2/(satirtop1*sutuntop2/toplam) + (FP-(satirtop2*sutuntop1/toplam))^2/(satirtop2*sutuntop1/toplam) + (TN-(satirtop2*sutuntop2/toplam))^2/(satirtop2*sutuntop2/toplam)
      fisher <- c(TP, FN, FP, TN)
      tab <- t(matrix(fisher, nrow=2,ncol=2))
      fisher_exact_test = fisher.test(tab)$p.value
      minimum_exp_fre = min((sutuntop1*satirtop1/toplam):(sutuntop2*satirtop2/toplam))
      cells_exp_fre_5 = (if((sutuntop1*satirtop1/toplam)<5) 1 else 0)+ (if((sutuntop1*satirtop2/toplam)<5) 1 else 0)+ (if((sutuntop2*satirtop1/toplam) <5) 1 else 0)+ (if((sutuntop2*satirtop2/toplam) <5) 1 else 0)
      cells_exp_fre_1 = (if((sutuntop1*satirtop1/toplam) <1) 1 else 0)+ (if((sutuntop1*satirtop2/toplam) <1) 1 else 0)+ (if((sutuntop2*satirtop1/toplam) <1) 1 else 0)+ (if((sutuntop2*satirtop2/toplam) <1) 1 else 0)
      ## Mc Nemar testi
      mcNemar_test = (FP-FN)^2/(FP+FN)
      mcNemar_test_p = dchi(mcNemar_test, df=1)
      with_Yate_cor_for_mcnamer = (abs(FP-FN)-1)^2/(FP+FN)
      with_Yate_cor_p_for_mcnamer = dchi(with_Yate_cor_for_mcnamer, df=1)
      
      ## Belirsizlik (Entropi)
      forbes_NMI = sensitivity_estimate*predvalue_positiverandomtest*log((sensitivity_estimate/predvalue_positiverandomtest_mar_ol),base=2)+
        (1-specificity)*(1-predvalue_positiverandomtest)*log(((1-specificity)/predvalue_positiverandomtest_mar_ol),base=2)+
        (1-sensitivity_estimate)*predvalue_positiverandomtest*log(((1-sensitivity_estimate)/(1-predvalue_positiverandomtest_mar_ol)),base=2)+
        specificity*(1-predvalue_positiverandomtest)*log((specificity/(1-predvalue_positiverandomtest_mar_ol)),base=2)
      ## satir icin entropy (test icin)
      entropy_hr = - ((sutuntop1/toplam) * log(sutuntop1/toplam, base=2) + (sutuntop2/toplam) * log(sutuntop2/toplam, base=2))
      ## sutun icin entropy (hastalik icin)  
      entropy_hc = - ((satirtop1/toplam)*log(satirtop1/toplam, base=2) + (satirtop2/toplam)*log(satirtop2/toplam, base=2))
      ## birlesik entropi (joint entropy)  
      entropy_hrc = - ( (TP/toplam)*log(TP/toplam, base=2) + (FP/toplam)*log(FP/toplam, base=2) + (FN/toplam)*log(FN/toplam, base=2) + (TN/toplam)*log(TN/toplam, base=2))
      ## bilgi icerigi (mutual information)  
      information_r_c = entropy_hr + entropy_hc - entropy_hrc
      ## kosullu entropi (conditional entropy)  
      a = entropy_hrc - entropy_hr
      c = entropy_hrc - entropy_hc
      sim_r_c = information_r_c / (a + information_r_c + c)
      dis_r_c = (a + c) / (a + information_r_c + c)
      ## goreli entropi (relative entropy, kullback-leibler uzakligi)
      ## pozitif test sonucu icin goreli entropi, Relative Improvement Over Chance (RIOC)
      positive_relative_entropy = (TP/sutuntop1)*log(((TP/sutuntop1)/ prevalence), base=2)+(FP/sutuntop1)*log(((FP/sutuntop1)/(1- prevalence)), base=2)
      ## negatif test sonucu icin goreli entropi 
      negative_relative_entropy = (FN/sutuntop2)*log(((FN/sutuntop2)/ prevalence), base=2)+(TN/sutuntop2)*log(((TN/sutuntop2)/(1- prevalence)), base=2)
      
      verim <- data.frame(Estimate = format(round(observed_agreement, digit =3),nsmall=3),  LowerCI = format(round(observed_agreement_low, digit =3),nsmall=3), UpperCI = format(round(observed_agreement_upp, digit =3),nsmall=3))
      newRow <- data.frame(Estimate = format(round(chance_agreement, digit =3),nsmall=3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(prop_expected_agreement, digit =3),nsmall=3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(cohens_kappa, digit =3),nsmall=3), LowerCI = format(round(cohens_kappa_low, digit =3),nsmall=3), UpperCI = format(round(cohens_kappa_upp, digit =3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(positive_agreement, digit =3),nsmall=3),  LowerCI = format(round(positive_agreement_low, digit =3),nsmall=3), UpperCI = format(round(positive_agreement_upp, digit =3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(negative_agreement, digit =3),nsmall=3),  LowerCI = format(round(negative_agreement_low, digit =3),nsmall=3), UpperCI = format(round(negative_agreement_upp, digit =3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(byrt_bias_index, digit =3),nsmall=3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(byrt_prevalence_asymmetry_index, digit =3),nsmall=3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(bias_adjusted_kappa, digit =3),nsmall=3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(prevalence_bias_adjusted_kappa, digit =3),nsmall=3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      
      rownames(verim) = c("Observed Agreement",
                          "Chance Agreement",
                          "Proportion of Expected Agreement" ,
                          "Cohen\'s Kappa", "Positive Agreement", "Negative Agreement", "Byrt\'s Bias Index (BI)",
       "Byrt\'s Prevalence Asymmetry Index (PI)", "Bias Adjusted Kappa (BAK)", "Prevalence & Bias Adjusted Kappa (PABAK)")
     
      return(verim)
      
    }
  })
  
  
  my_data2 <- reactive({
    testdata <- dataM2()
    as.data.frame(cbind(Interpretation = shinyInput(actionButton, nrow(testdata),'button2_', label = "Interpretation", onclick = 'Shiny.onInputChange(\"select_button2\",  this.id)' ),testdata))
  })  
  output$dataUpload2 <- DT::renderDataTable(my_data2(),selection = 'single', extensions = c('Buttons','KeyTable', 'Responsive'),
                                            options = list(pageLength = 25,lengthMenu = c(25, 50, 75, 100),
                                                           dom = 'Bfrtip',
                                                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),keys = TRUE,
                                                           preDrawCallback = JS("function() { 
                                                                                Shiny.unbindAll(this.api().table().node()); }"), 
                                                           drawCallback = JS("function() { 
                                                                             Shiny.bindAll(this.api().table().node()); } ")),
                                            server = FALSE, escape = FALSE)
  
  
  # Here I created a reactive to save which row was clicked which can be stored for further analysis
  SelectedRow2 <- eventReactive(input$select_button2,{
    as.numeric(strsplit(input$select_button2, "_")[[1]][2])
  })
  
  # This is needed so that the button is clicked once for modal to show, a bug reported here
  # https://github.com/ebailey78/shinyBS/issues/57
  observeEvent(input$select_button2, {
    toggleModal(session, "modalExample2", "open")
  })
  
  DataRow2 <- eventReactive(input$select_button2,{
    my_data2()[SelectedRow2(),2:ncol(my_data2())]
  })
  

  
  output$popup2 <- renderUI({
    if (rownames(DataRow2()) == 'Observed Agreement') 
    {
      ### http://www.pmean.com/definitions/kappa.htm
      ### https://stats.stackexchange.com/questions/82162/cohens-kappa-in-plain-english
      ## file:///C:/Users/User/Downloads/2748-11862-1-PB.pdf
      bsModal("modalExample2", paste0("Percent Agreement"), "", size = "large",
              h4("Description:"),
              print(paste0("Percent agreement is one of the statistical tests to
measure interrater reliability. A researcher simply
                           -calculates the number of times raters agree on a rating,
                           then divides by the total number of ratings-. Acceptable percent agreement occurs only if the value
is>80%.  
                           ")),
              h4("Interpretation:"),
              h5(" "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow2()),
              column(12,                   
                     DT::renderDataTable(DataRow2())
                     
              )
              
              
              )}

    else if (rownames(DataRow2()) == 'Cohen\'s Kappa') 
    {
      degcap_int = ' '
      if (as.numeric(as.character(DataRow2()$Estimate)) < 0.20)
      {deg_cap = 'Less than 0.20'
       degcap_int = 'poor agreement'}
      else if (as.numeric(as.character(DataRow2()$Estimate)) >= 0.20 && as.numeric(as.character(DataRow2()$Estimate)) <= 0.40)
      {deg_cap = '0.20 to 0.40'
      degcap_int = 'fair agreement'}
      else if (as.numeric(as.character(DataRow2()$Estimate)) >= 0.40 && as.numeric(as.character(DataRow2()$Estimate)) <= 0.60)
      {deg_cap = '0.40 to 0.60'
      degcap_int = 'moderate agreement'}
      else if (as.numeric(as.character(DataRow2()$Estimate)) >= 0.60 && as.numeric(as.character(DataRow2()$Estimate)) <= 0.80)
      {deg_cap = '0.60 to 0.80'
      degcap_int = 'good agreement'}
      else if (as.numeric(as.character(DataRow2()$Estimate)) >= 0.80 && as.numeric(as.character(DataRow2()$Estimate)) <= 1.00)
      {deg_cap = '0.80 to 1.00'
      degcap_int = 'very good agreement'}
      
      ### http://www.pmean.com/definitions/kappa.htm
      ### https://stats.stackexchange.com/questions/82162/cohens-kappa-in-plain-english
      ### http://epiville.ccnmtl.columbia.edu/popup/how_to_calculate_kappa.html
      bsModal("modalExample2", paste0("Cohen's Kappa"), "", size = "large",
              h4("Description:"),
              print(paste0("Kappa statistic can also be used to measure interrater
reliability, beside percent agreement. Kappa was firstly
                           introduced by Jacob Cohen in 1960 as a revision of
                           percent agreement.  The Kappa statistic (or value) is a metric that compares an Observed Accuracy with an Expected Accuracy (random chance). The kappa statistic is used not only to evaluate a single classifier, but also to evaluate classifiers amongst themselves. In addition, it takes into account random chance (agreement with a random classifier), which generally means it is less misleading than simply using accuracy as a metric (an Observed Accuracy of 80% is a lot less impressive with an Expected Accuracy of 75% versus an Expected Accuracy of 50%). Computation of Observed Accuracy and Expected Accuracy is integral to comprehension of the kappa statistic, and is most easily illustrated through use of a confusion matrix.
Kappa is always less than or equal to 1. A value of 1 implies perfect agreement and values less than 1 imply less than perfect agreement.
                           In rare situations, Kappa can be negative. This is a sign that the two observers agreed less than would be expected just by chance.
                           It is rare that we get perfect agreement. Kappa is not computed if the data storage type (string or numeric) is not the same for the two variables. 
                           For string variable, both variables must have the same defined length.Different people have different interpretations as to what is 
                           a good level of agreement. 
                          There are several different interpretations of kappa
coefficient based on different authors, such as Landis and
                           Koch, Fleiss, and Altman. Interpretation of kappa coefficient based on Landis and
                           Koch:
                           No agreement = Less than or equal to 0;
                           None to slight= 0.01 to 0.20;
                           Fair = 0.21 to 0.40;
                           Moderate = 0.41 to 0.60;
                           Substantial = 0.61 to 0.80;
                           Almost perfect agreement = 0.81 to 1.00. Interpretation of kappa coefficient based on Fleiss:
                           Poor agreement = Less than or equal to 0.40;
                           Poor= 0.40 to 0.75;
                           Excellent agreement = Greater than or equal to 0.75. Interpretation of kappa coefficient based on Altman:
                           Poor = Less than or equal to 0.20;
                           Fair= 0.21 to 0.40;
Moderate= 0.41 to 0.60;
Good= 0.61 to 0.80;
Very good= 0.81 to 1.00;
                           ")),
              h4("Interpretation:"),
              h5("The value for Kappa is ", DataRow2()$Estimate, " indicating a ", degcap_int,
                 "The diagnostic test result and real situation had a probability of agreeing who had disease  beyond chance of ", 
                 DataRow2()$Estimate),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow2()),
              column(12,                   
                     DT::renderDataTable(DataRow2())
                     
              )
              
              
              )}
    

    ### http://www.jmood.org/pdf/TR-JMOOD-0fc03dbc.pdf
     else if (rownames(DataRow2()) == 'Observed Accuracy') 
    {
      ### http://www.pmean.com/definitions/kappa.htm
      ### https://stats.stackexchange.com/questions/82162/cohens-kappa-in-plain-english
       bsModal("modalExample2", paste0("Observed Accuracy"), "", size = "large",
               h4("Description:"),
               print(paste0("The observed accuracy is simply the number of instances that were classified correctly throughout the entire confusion 
                           matrix and is the total proportion of matches observed for the two evaluators.
                            ")),
               h4("Interpretation:"),
               h5(" "),
               h5("**************************************************************************************************************************************************"),
               h5("Data for Row Number:",SelectedRow2()),
               column(12,                   
                      DT::renderDataTable(DataRow2())
                      
               )
               
               
               )}
    else if (rownames(DataRow2()) == 'Byrt\'s Bias Index (BI)') 
    {
      ### https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4867103/
      bsModal("modalExample2", paste0("Byrt\'s Bias Index (BI)"), "", size = "large",
              h4("Description:"),
              print(paste0("The bias index (BI) estimates the tendency of two observers to differ in 
how frequently they observe the occurrence of a condition in the same sample; 
a nonzero BI indicates kappa may be inflated by rater bias.
                           ")),
              h4("Interpretation:"),
              h5(" "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow2()),
              column(12,                   
                     DT::renderDataTable(DataRow2())
                     
              )
              
              
      )}
    else if (rownames(DataRow2()) == 'Byrt\'s Prevalence Asymmetry Index (PI)') 
    {
      ### https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4867103/
      bsModal("modalExample2", paste0("Byrt\'s Prevalence Asymmetry Index (PI)"), "", size = "large",
              h4("Description:"),
              print(paste0("The prevalence index (PI) estimates the relative likelihoods of present and absent 
ratings as a function of the prevalence of the condition being rated;
as |PI| increases, kappa decreases.
                           ")),
              h4("Interpretation:"),
              h5(" "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow2()),
              column(12,                   
                     DT::renderDataTable(DataRow2())
                     
              )
              
              
      )}
    else if (rownames(DataRow2()) == 'Bias Adjusted Kappa (BAK)') 
    {
      ### https://books.google.com.tr/books?id=kR9GAAAAQBAJ&pg=PA8&lpg=PA8&dq=Prevalence+Adjusted+Bias+Adjusted+Kappa&source=bl&ots=HCRs0bFnp0&sig=Q4NqnjntWM3YgATJMwtKPXHI9WE&hl=tr&sa=X&ved=0ahUKEwizrKSfrZHVAhWmJZoKHUKTADE4FBDoAQhIMAU#v=onepage&q=Prevalence%20Adjusted%20Bias%20Adjusted%20Kappa&f=false
      bsModal("modalExample2", paste0("Bias Adjusted Kappa (BAK)"), "", size = "large",
              h4("Description:"),
              print(paste0("Bias Adjusted Kappa (BAK) is used for the classic Cohen s kappa coefficient have been proposed to cope with 
                           their problems. BAK allows adjustment of Cohen s kappa coefficient for rater bias, i.e. differences in the 
                           marginal probability distribution of the two raters.
                           ")),
              h4("Interpretation:"),
              h5(" "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow2()),
              column(12,                   
                     DT::renderDataTable(DataRow2())
                     
              )
              )}
    else if (rownames(DataRow2()) == 'Prevalence & Bias Adjusted Kappa (PABAK)') 
    {
### https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2636838/
      ### https://academic.oup.com/ptj/article/85/3/257/2805022
      bsModal("modalExample2", paste0("Prevalence & Bias Adjusted Kappa (PABAK)"), "", size = "large",
              h4("Description:"),
              print(paste0("Because both prevalence and bias play a part in determining the magnitude of the kappa coefficient, 
some statisticians have devised adjustments to take account of these influences. Kappa can be adjusted for high or low 
prevalence by computing the average of cells a and d and substituting this value for the actual values in those cells. 
Similarly, an adjustment for bias is achieved by substituting the mean of cells b and c for those actual cell values. 
The kappa coefficient that results is referred to as PABAK (prevalence-adjusted bias-adjusted kappa). 
                           ")),
              h4("Interpretation:"),
              h5(" "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow2()),
              column(12,                   
                     DT::renderDataTable(DataRow2())
                     
              )
              )}
    

    else
    {
    bsModal("modalExample2", paste0("Interpretation: "), "", size = "large",
            print(paste0(rownames(DataRow2())," estimate is ", DataRow2()$Estimate," and its confidence interval is ", DataRow2()$LowerCI," - ",DataRow2()$UpperCI," . ")),
            h5("**************************************************************************************************************************************************"),
            h5("Data for Row Number:",SelectedRow2()),
            column(12,                   
                   DT::renderDataTable(DataRow2())
                   
            )
            
            
    )
    }
  })

  output$plot2 <- highcharter::renderHighchart({
    if (input$verigir_kontrol == TRUE){ 
      TP = 0
      FN = 0
      FP = 0
      TN = 0
      for (i in 1:nrow(tabloveri()))
      {
        if (tabloveri()[i,1]==1 & tabloveri()[i,2]==1)
        {TP = TP+1}
        else if (tabloveri()[i,1]==1 & tabloveri()[i,2]==2)
        {FN = FN+1}
        else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==1)
        {FP = FP+1}
        else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==2)
        {TN = TN+1}
        next
      }
      CI = as.numeric(trimws(input$text5_2))
    }
    else{
      TP = as.numeric(trimws(input$text1)) ### x
      FN = as.numeric(trimws(input$text2)) ### y
      FP = as.numeric(trimws(input$text3)) ### z
      TN = as.numeric(trimws(input$text4)) ### t
      CI = as.numeric(trimws(input$text5))
    }
    
    
    satirtop1 = TP + FN
    satirtop2 = FP + TN
    sutuntop1 = TP + FP
    sutuntop2 = FN + TN
    toplam = satirtop1 + satirtop2
    #table1 <- matrix(c(TP,FN,satirtop1,FP,TN,satirtop2,sutuntop1,sutuntop2,toplam),nrow=3,ncol=3,byrow=TRUE,dimnames=list(c("1.satir","2.satir","Sutun toplamlari"),c("1.kolon","2.kolon","Satir toplamlari")))
    #table1
    ## Duyarlilik 
    sensitivity_estimate = TP / satirtop1
    sensitivity_se = round((sqrt(sensitivity_estimate * (1 - sensitivity_estimate)/satirtop1)), 4)
    sensitivity_low = 1 - Rbeta.inv(((1 + CI/100)/2), (satirtop1+1-TP), TP)
    sensitivity_upp = Rbeta.inv(((1 + CI/100)/2), (TP+1), (satirtop1-TP))
    sensitivity_randomtest = sutuntop1 / toplam
    qualityindex_sensitivity = (sensitivity_estimate - sensitivity_randomtest) / (1 - sensitivity_randomtest)
    ## Secicilik
    specificity = TN / satirtop2
    specificity_se = sqrt(specificity * (1 - specificity) / satirtop2)
    specificity_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop2+1-TN),TN)
    specificity_upp = Rbeta.inv((1 + CI/100)/2, (TN+1), (satirtop2-TN))
    specificity_randomtest = (1 - sensitivity_randomtest)
    qualityindex_specificity = (specificity - 1 + sensitivity_randomtest) / sensitivity_randomtest
    ## Gain in Certainty
    gain_in_certainty = sensitivity_estimate + specificity
    ## Etkililik, yeterlilik ayni zamanda overall accuracy (toplam-genel dogruluk orani)
    efficiency = (TP + TN) / toplam
    efficiency_se = sqrt(efficiency * (1 - efficiency) / toplam)
    efficiency_low = 1-Rbeta.inv((1 + CI/100)/2, (toplam+1-TP-TN), (TP+TN))
    efficiency_upp = Rbeta.inv((1 + CI/100)/2, (TP+TN+1), (toplam-TP-TN))
    efficiency_randomtest = (satirtop1 / toplam) * sensitivity_randomtest + (1 - (satirtop1 / toplam)) * (1 - sensitivity_randomtest)
    mis_efficiency = 1 - efficiency
    ## Quality indeks
    quality_index = (efficiency - efficiency_randomtest) / (1 - efficiency_randomtest)
    quality_index_se = sqrt((efficiency*(1-efficiency)/(1-efficiency_randomtest)^2+2*(1-efficiency)*(2*efficiency*efficiency_randomtest-((TP*((satirtop1 / toplam)+sensitivity_randomtest)+TN*(2-(satirtop1 / toplam)-sensitivity_randomtest))/toplam))/(1-efficiency_randomtest)^3+((1-efficiency)^2)*((TP*((satirtop1 / toplam)+sensitivity_randomtest)^2+FN*(-(satirtop1 / toplam)+1+sensitivity_randomtest)^2+FP*(1+(satirtop1 / toplam)-sensitivity_randomtest)^2+TN*(2-(satirtop1 / toplam)-sensitivity_randomtest)^2)/toplam-4*efficiency_randomtest^2)/(1-efficiency_randomtest)^4)/toplam)
    quality_index_low = quality_index + qnorm((1 - CI/100)/2)*quality_index_se
    quality_index_upp = quality_index - qnorm((1 - CI/100)/2)*quality_index_se
    ## Youden indeksi
    youdens_index = sensitivity_estimate + specificity - 1
    youdens_index_se = sqrt(sensitivity_se^2+specificity_se^2)
    youdens_index_low = youdens_index + qnorm((1 - CI/100)/2)*youdens_index_se
    youdens_index_upp = youdens_index - qnorm((1 - CI/100)/2)*youdens_index_se
    
    number_needed_to_diagnose = 1 / youdens_index
    number_needed_to_misdiagnose = 1 / ( 1 - efficiency ) 
    ## Pozitif kestirim degeri
    predictivevalue_positivetest = TP / sutuntop1
    predictivevalue_positivetest_se = sqrt(predictivevalue_positivetest*(1-predictivevalue_positivetest)/sutuntop1)
    predictivevalue_positivetest_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop1+1-TP),TP)
    predictivevalue_positivetest_upp = Rbeta.inv((1 + CI/100)/2, (TP+1),(sutuntop1-TP))
    predvalue_positiverandomtest = (satirtop1 / toplam)
    predvalue_positiverandomtest_mar_ol = predvalue_positiverandomtest*sensitivity_estimate+(1-predvalue_positiverandomtest)*(1-specificity)
    ## Negatif kestirim degeri
    predictivevalue_negativetest = TN / sutuntop2
    predictivevalue_negativetest_se = sqrt(predictivevalue_negativetest*(1-predictivevalue_negativetest)/sutuntop2)
    predictivevalue_negativetest_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop2+1-TN),TN)
    predictivevalue_negativetest_upp = Rbeta.inv((1 + CI/100)/2, (TN+1), (sutuntop2-TN))
    predvalue_negativerandomtest = 1 - (satirtop1 / toplam)
    predvalue_negativerandomtest_mar_ol = 1 - predvalue_positiverandomtest_mar_ol
    ## Predictive Summary Index (PSI)
    predictive_summary_index = predictivevalue_positivetest + predictivevalue_negativetest - 1
    ## Yanlis pozitif orani
    false_positiverate = FP / satirtop2
    false_positiverate_se = sqrt(false_positiverate*(1-false_positiverate)/satirtop2)
    false_positiverate_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop2+1-FP),FP)
    false_positiverate_upp = Rbeta.inv((1 + CI/100)/2, (FP+1), (satirtop2-FP))
    ## Yansis negatif orani
    false_negativerate = FN / satirtop1
    false_negativerate_se = sqrt(false_negativerate*(1-false_negativerate)/satirtop1)
    false_negativerate_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop1+1-FN),FN)
    false_negativerate_upp = Rbeta.inv((1 + CI/100)/2, (FN+1), (satirtop1-FN))
    ## False Omission Rate (FOR) https://www.ncss.com/software/ncss/diagnostic-tests-in-ncss/
    false_omission_rate = FN/(sutuntop2)
    false_omission_rate_se = sqrt(false_omission_rate*(1-false_omission_rate)/sutuntop2)
    false_omission_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop2+1-FN),FN)
    false_omission_rate_upp = Rbeta.inv((1 + CI/100)/2, (FN+1), (sutuntop2-FN))
    ## False Discovery Rate (FDR) https://www.ncss.com/software/ncss/diagnostic-tests-in-ncss/
    false_discovery_rate = FP/(sutuntop1)
    false_discovery_rate_se = sqrt(false_discovery_rate*(1-false_discovery_rate)/sutuntop1)
    false_discovery_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop1+1-FP),FP)
    false_discovery_rate_upp = Rbeta.inv((1 + CI/100)/2, (FP+1), (sutuntop1-FP))
    ## Dogru pozitif orani
    true_positiverate = TP / (satirtop1)
    ## Yanlis siniflandirma orani
    misclassification_rate = (FN + FP) / toplam
    misclassification_rate_se = sqrt(misclassification_rate*(1-misclassification_rate)/toplam)
    misclassification_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-FP-FN), (FP+FN))
    misclassification_rate_upp = Rbeta.inv((1 + CI/100)/2, (FP+FN+1), (toplam-FP-FN))
    ## Prevelans
    prevalence = (satirtop1 / toplam)
    prevalence_se = sqrt((satirtop1 / toplam)*(1-(satirtop1 / toplam))/toplam)
    prevalence_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-satirtop1), satirtop1)
    prevalence_upp = Rbeta.inv((1 + CI/100)/2, (satirtop1+1), (toplam-satirtop1))
    
    ##caret paketinden olculer
    detection_rate = TP/toplam
    detection_prevalence = (TP+FP)/toplam
    balanced_accuracy = (sensitivity_estimate + specificity)/2
    lift = (TP/(TP+FN))/((TP+FP)/toplam)  ## https://gerardnico.com/wiki/data_mining/error_rate
    precision = TP/(TP+FP)
    recall = TP/(TP+FN)
    ## http://michael.hahsler.net/research/recomm_lnai2002/lnai2002.pdf
    conviction = (1-((TP+FP)/toplam))/(1-(TP/(TP+FN)))
    matthews_correlation_coefficient = (TP * TN + FN * FP) / ((TP+FP)*(FN+TN)*(FP+TN)*(TP+FN))^(1/2)
    ## Goreli Risk Orani (Relative risk)
    ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
    # https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20ratio.htm
    relative_risk = (TP/sutuntop1)/(FN/sutuntop2)
    relative_risk_se = sqrt((1/TP)+(1/FN)-(1/(TP+FP))-(1/(FN+TN)))
    relative_risk_low = exp(log(relative_risk)+qnorm((1-CI/100)/2)*relative_risk_se)
    relative_risk_upp = exp(log(relative_risk)-qnorm((1-CI/100)/2)*relative_risk_se)
    difference_in_proportion = (TP/sutuntop1) - (FN/sutuntop2)
    ## Leverage
    ## https://www3.nd.edu/~tweninge/pubs/AHBCW_ANNIE08.pdf
    leverage = (TP/sutuntop1)-((TP+FP)/toplam)
    ## http://www.ebm.med.ualberta.ca/TherapyCalc.html
    control_event_rate = FN/sutuntop2
    experimental_event_rate = TP/sutuntop1
    ## Absolute risk reduction
    absolute_risk_reduction = -difference_in_proportion
    absolute_risk_reduction_se = sqrt((TP/sutuntop1)*(1-(TP/sutuntop1))/sutuntop1+(FN/sutuntop2)*(1-(FN/sutuntop2))/sutuntop2)
    absolute_risk_reduction_low = absolute_risk_reduction - qnorm((1-CI/100)/2)*absolute_risk_reduction_se
    absolute_risk_reduction_upp = absolute_risk_reduction + qnorm((1-CI/100)/2)*absolute_risk_reduction_se
    ## Relative risk reduction
    relative_risk_reduction = absolute_risk_reduction / (FN/sutuntop2)
    relative_risk_reduction_low = 1 - exp(log(relative_risk)-qnorm((1-CI/100)/2)*relative_risk_se)
    relative_risk_reduction_upp = 1 - exp(log(relative_risk)+qnorm((1-CI/100)/2)*relative_risk_se)
    ##Number needed to treat
    number_needed_to_treat = 1 / abs(difference_in_proportion)
    number_needed_to_treat_low = 1 /absolute_risk_reduction_upp
    number_needed_to_treat_upp = 1 /absolute_risk_reduction_low
    ## Test duzeyi
    test_level = sensitivity_randomtest
    test_level_se = sqrt(test_level*(1-test_level)/toplam)
    test_level_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-sutuntop1),sutuntop1)
    test_level_upp = Rbeta.inv((1 + CI/100)/2, (sutuntop1+1), (toplam-sutuntop1))
    ## Pre-test odds
    pretest_odds = prevalence / (1 - prevalence)
    ## Pozitif Olabilirlik Orani
    likelihoodratio_positivetest = sensitivity_estimate/(1-specificity)
    likelihoodratio_positivetest_se = exp(sqrt((1-sensitivity_estimate)/(sensitivity_estimate*satirtop1) +specificity/((1-specificity)*satirtop2)))
    likelihoodratio_positivetest_low = exp(log(likelihoodratio_positivetest) + qnorm((1 - CI/100)/2)*log(likelihoodratio_positivetest_se))
    likelihoodratio_positivetest_upp = exp(log(likelihoodratio_positivetest) - qnorm((1 - CI/100)/2)*log(likelihoodratio_positivetest_se))
    ## post-test odds
    posttest_odds = pretest_odds * likelihoodratio_positivetest
    bayes_ppv = (sensitivity_estimate * prevalence)/((sensitivity_estimate * prevalence) + (1 - specificity) * (1 - prevalence))
    ## Post-test probability (positive test result)
    posttest_probability = (pretest_odds * likelihoodratio_positivetest) / (1 + (pretest_odds * likelihoodratio_positivetest))
    ## Negatif Olabilirlik Orani
    likelihoodratio_negativetest = (1-sensitivity_estimate)/specificity
    likelihoodratio_negativetest_se = exp(sqrt(sensitivity_estimate/((1-sensitivity_estimate)*satirtop1) +(1-specificity)/(specificity*satirtop2)))
    likelihoodratio_negativetest_low = exp(log(likelihoodratio_negativetest) + qnorm((1-CI/100)/2)*log(likelihoodratio_negativetest_se))
    likelihoodratio_negativetest_upp = exp(log(likelihoodratio_negativetest) - qnorm((1-CI/100)/2)*log(likelihoodratio_negativetest_se))
    bayes_npv = (sensitivity_estimate * (1 - prevalence))/((sensitivity_estimate * (1 - prevalence)) + (1 - specificity) * prevalence)
    ## Post-test probability (negative test result)
    posttest_probability_neg = (pretest_odds * likelihoodratio_negativetest) / (1 + (pretest_odds * likelihoodratio_negativetest))
    ## Ters negatif olabilirlik orani
    inverse_likelihoodratio_negativetest = specificity/(1-sensitivity_estimate)
    inverse_likelihoodratio_negativetest_se = exp(sqrt(sensitivity_estimate/((1-sensitivity_estimate)*satirtop1) +(1-specificity)/(specificity*satirtop2)))
    inverse_likelihoodratio_negativetest_low = exp(log(inverse_likelihoodratio_negativetest) + qnorm((1-CI/100)/2)*log(inverse_likelihoodratio_negativetest_se))
    inverse_likelihoodratio_negativetest_upp = exp(log(inverse_likelihoodratio_negativetest) - qnorm((1-CI/100)/2)*log(inverse_likelihoodratio_negativetest_se))
    ## Diagnostic Odds Ratio (Tani Odds Orani)
    odds_ratio = (TP/FN)/(FP/TN)
    odds_ratio_se = exp(sqrt(1/TP+1/FP+1/TN+1/FN))
    odds_ratio_low = exp(log(odds_ratio) + qnorm((1-CI/100)/2)*log(odds_ratio_se))
    odds_ratio_upp = exp(log(odds_ratio) - qnorm((1-CI/100)/2)*log(odds_ratio_se))
    odds_ratio_Haldane = ((TP+0.5)/(FN+0.5))/((FP+0.5)/(TN+0.5))
    odds_ratio_Haldane_se = exp(sqrt(1/(TP+0.5)+1/(FP+0.5)+1/(TN+0.5)+1/(FN+0.5)))
    odds_ratio_Haldane_low = exp(log(odds_ratio_Haldane) + qnorm((1-CI/100)/2)*log(odds_ratio_Haldane_se))
    odds_ratio_Haldane_upp = exp(log(odds_ratio_Haldane) - qnorm((1-CI/100)/2)*log(odds_ratio_Haldane_se))
    error_odds_ratio = (sensitivity_estimate/(1-sensitivity_estimate))/(specificity/(1-specificity))
    ## Rate ratio
    ## http://omarkasule-05.tripod.com/id52.html
    ## http://sphweb.bumc.bu.edu/otlt/mph-modules/ep/ep713_randomerror/ep713_randomerror4.html
    rate_ratio = (TP/sutuntop1)/(TP/sutuntop2)
    rate_ratio_low = exp(log(relative_risk)+qnorm((1-CI/100)/2)*sqrt(1/TP+1/FP))
    rate_ratio_upp = exp(log(relative_risk)-qnorm((1-CI/100)/2)*sqrt(1/TP+1/FP))
    ## Risk Difference
    ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
    risk_difference = (TP/sutuntop1) - (FN/sutuntop2)
    risk_difference_se = (((TP/sutuntop1)*(1-(TP/sutuntop1)))/sutuntop1)+(((FN/sutuntop2)*(1-(FN/sutuntop2)))/sutuntop2)
    risk_difference_low = risk_difference + qnorm((1-CI/100)/2)*sqrt(risk_difference_se)
    risk_difference_upp = risk_difference - qnorm((1-CI/100)/2)*sqrt(risk_difference_se)
    ## Nitelenebilen risk
    ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
    attributable_risk = (TP/sutuntop1)-(FN/sutuntop2)
    attributable_risk_se = sqrt((satirtop1/toplam)*(1-(satirtop1/toplam))*(1/sutuntop1+1/sutuntop2))
    attributable_risk_low = attributable_risk + qnorm((1-CI/100)/2)*attributable_risk_se
    attributable_risk_upp = attributable_risk - qnorm((1-CI/100)/2)*attributable_risk_se
    ## Attributable risk percent
    ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
    attributable_risk_percent = ((relative_risk-1)/relative_risk)*100
    attributable_risk_percent_low = attributable_risk_percent - (qnorm((1-CI/100)/2)*attributable_risk_se)/attributable_risk
    attributable_risk_percent_upp = attributable_risk_percent + (qnorm((1-CI/100)/2)*attributable_risk_se)/attributable_risk
    ## Population Attributable Risk
    ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
    population_attributable_risk = (satirtop1/toplam)-(FN/sutuntop2)
    ## Population Attributable Risk Percent
    population_attributable_risk_percent = (population_attributable_risk / (satirtop1/toplam))*100
    ## Kohen's Kappa
    cohens_kappa = (efficiency-efficiency_randomtest)/(1-efficiency_randomtest)
    cohens_kappa_se = quality_index_se
    cohens_kappa_low = cohens_kappa + qnorm((1-CI/100)/2)*cohens_kappa_se
    cohens_kappa_upp = cohens_kappa - qnorm((1-CI/100)/2)*cohens_kappa_se
    ## Gozlenen Karar
    observed_agreement = efficiency
    observed_agreement_se = efficiency_se
    observed_agreement_low = efficiency_low
    observed_agreement_upp = efficiency_upp
    ## Risk karari,expected agreement
    chance_agreement = efficiency_randomtest
    ## Pozitif karar
    positive_agreement = 2*TP /(satirtop1+sutuntop1)
    positive_agreement_se = sqrt(4*TP*(FP+FN)*(TP+FP+FN))/(sutuntop1+satirtop1)^2
    positive_agreement_low = positive_agreement + qnorm((1-CI/100)/2)*positive_agreement_se
    positive_agreement_upp = positive_agreement - qnorm((1-CI/100)/2)*positive_agreement_se
    ## Negatif karar
    negative_agreement = 2*TN/(satirtop2+sutuntop2)
    negative_agreement_se = sqrt(4*TN*(FP+FN)*(TN+FP+FN))/(sutuntop2+satirtop2)^2
    negative_agreement_low = negative_agreement + qnorm((1-CI/100)/2)*negative_agreement_se
    negative_agreement_upp = negative_agreement - qnorm((1-CI/100)/2)*negative_agreement_se
    ## Rand index
    ## http://www.cs.pomona.edu/classes/cs158/resources/158-10(EM).pdf
    rand_index = (TP+TN)/(TP+FN+FP+TN)
    ## e-measure
    ## http://michael.hahsler.net/research/recomm_lnai2002/lnai2002.pdf
    e_measure = 1/((((1-CI)/100)*(1-precision))+((1-((1-CI)/100))*(1/recall)))
    ## discriminant power 
    ## https://eva.fing.edu.uy/pluginfile.php/69453/mod_resource/content/1/7633-10048-1-PB.pdf
    discriminant_power = (sqrt(3)/pi)*(log (sensitivity_estimate / (1-sensitivity_estimate)) + log (specificity / (1-specificity)))
    ## F1 Score
    ## http://machinelearningmastery.com/classification-accuracy-is-not-enough-more-performance-measures-you-can-use/
    f1_score = 2*(((TP/(TP+FN))*(TP/(TP+FP)))/((TP/(TP+FN))+(TP/(TP+FP))))
    ## Byrt yanlilik indeksi
    byrt_bias_index = (FN-FP)/toplam
    ## Byrt  asimetrik indeks prevelansi
    byrt_prevalence_asymmetry_index = (TN-TP)/toplam
    ## Yanlilik duzeltmeli kappa
    bias_adjusted_kappa = (efficiency-0.25*((satirtop1+sutuntop1)^2+(satirtop2+sutuntop2)^2)/toplam^2)/(1-0.25*((satirtop1+sutuntop1)^2+(satirtop2+sutuntop2)^2)/toplam^2)
    ## Yanlilik duzeltmeli kappa prevelansi
    prevalence_bias_adjusted_kappa = cohens_kappa+(1-cohens_kappa)*(byrt_prevalence_asymmetry_index^2-byrt_bias_index^2)
    ## Dice'in indeksi (Czekanowski)
    dice_index = positive_agreement
    dice_index_se= positive_agreement_se
    dice_index_low = positive_agreement_low
    dice_index_upp = positive_agreement_upp
    ## Yule's Q
    yules_q = (odds_ratio - 1) / (odds_ratio + 1)
    yules_q_se = 0.5*(1-yules_q^2)*sqrt(1/TP+1/FP+1/TN+1/FN)
    yules_q_low = yules_q + qnorm((1-CI/100)/2)*yules_q_se
    yules_q_upp = yules_q - qnorm((1-CI/100)/2)*yules_q_se
    
    equitable_threatscore = (TP-(sutuntop1*satirtop1/toplam))/(TP+FP+FN-(sutuntop1*satirtop1/toplam))
    ## Phi
    phi = (TP*TN-FN*FP) / sqrt(satirtop1 * satirtop2 * sutuntop1 * sutuntop2)
    phi_se = sqrt((1-phi^2+(phi+0.5*phi^3)*((satirtop1-satirtop2)*(sutuntop1-sutuntop2)/sqrt(satirtop1*satirtop2*sutuntop1*sutuntop2))-0.75*phi^2*((satirtop1-satirtop2)^2/(satirtop1*satirtop2)+(sutuntop1-sutuntop2)^2/(sutuntop1*sutuntop2)))/toplam)
    phi_low = phi + qnorm((1-CI/100)/2)*phi_se
    phi_upp = phi - qnorm((1-CI/100)/2)*phi_se
    ## Cramer V katsayisi
    cramer_v = sqrt((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2))/toplam)
    ## Olaganlik katsayisi
    contingency_coefficient = sqrt((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2))/(((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)))+toplam))
    ## Goodman and Kruskal Gamma 
    goodman_and_kruskal_gamma = (TP*TN-FN*FP)/(TP*TN+FN*FP)
    goodman_and_kruskal_gamma_se = 0.5*(1-yules_q^2)*sqrt(1/TP+1/FP+1/TN+1/FN)
    goodman_and_kruskal_gamma_low = yules_q + qnorm((1-CI/100)/2)*yules_q_se
    goodman_and_kruskal_gamma_upp = yules_q - qnorm((1-CI/100)/2)*yules_q_se
    ## Kendall's tau a
    kendalls_tau_a = (TP*TN-FN*FP)/(toplam*(toplam-1)/2)
    ## Kendall's tau b
    ## https://v8doc.sas.com/sashtml/stat/chap28/sect20.htm
    ## http://slideplayer.com/slide/10766029/
    kendalls_tau_b = (TP*TN-FN*FP)/sqrt((TP*TN+FN*FP+(TP*FN+FP*TN))*(TP*TN+FN*FP+(TP*FP+FN*TN)))
    ## Kendall's tau c
    kendalls_tau_c = ((2*2)*(TP*TN-FN*FP)) / (toplam^2 * (2-1))
    ## Somers'd R|C
    somers_d = (TP*TN-FN*FP)/(TP*TN+FN*FP+(TP*FN+FP*TN))
    ## Somers'd C|R
    somers_d_2 = (TP*TN-FP*FN)/(TP*TN+FP*FN+(TP*FP+FN*TN))
    ## Scoot'un karar indeksi
    scotts_agreement_index = bias_adjusted_kappa
    ## Dort-duzeyli Korelasyon
    tetrachoric_correlation = cos(pi/(1+sqrt(TP*TN/FN/FP)))
    tetrachoric_correlation_se = 0.375*(1-tetrachoric_correlation^2)*sqrt((1/(TP))+(1/(FP))+(1/(TN))+(1/(FN)))
    tetrachoric_correlation_low = tetrachoric_correlation + qnorm((1-CI/100)/2)*tetrachoric_correlation_se
    tetrachoric_correlation_upp = tetrachoric_correlation - qnorm((1-CI/100)/2)*tetrachoric_correlation_se
    ## Goodman kruskal tau katsayisi
    goodman_kruskal_tau = (toplam*((TP^2+FP^2)/sutuntop1+(FN^2+TN^2)/sutuntop2)-satirtop1^2-satirtop2^2)/(toplam^2-satirtop1^2-satirtop2^2)
    goodman_kruskal_tau_se = toplam*sqrt(TP*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop1)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*TP/satirtop1-(TP^2+FN^2)/satirtop1^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+FN*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop2)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*FN/satirtop1-(TP^2+FN^2)/satirtop1^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+FP*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop1)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*FP/satirtop2-(FP^2+TN^2)/satirtop2^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+TN*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop2)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*TN/satirtop2-(FP^2+TN^2)/satirtop2^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2)/(toplam^2-sutuntop1^2-sutuntop2^2)^2
    goodman_kruskal_tau_low = goodman_kruskal_tau + qnorm((1-CI/100)/2)*goodman_kruskal_tau_se
    goodman_kruskal_tau_upp = goodman_kruskal_tau - qnorm((1-CI/100)/2)*goodman_kruskal_tau_se
    ## Simetrik Lambda 
    lambda_symmetric = ((max(TP,FN)+max(FP,TN)+max(TP,FP)+max(FN,TN)-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2))/(2*toplam-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2)))
    if (lambda_symmetric==0 || lambda_symmetric==1)
    {lambda_symmetric_se = 0}
    else
    {
      lambda_symmetric_se = sqrt((TP*((if(TP>FN) 1 else 0) + (if(TP>FP) 1 else 0) + ((if(sutuntop1>sutuntop2) 1 else 0)+ (if(satirtop1>satirtop2) 1 else 0))*(lambda_symmetric-1))^2+FN*( (if(TP>FN) 0 else 1)+ (if(FN>TN) 1 else 0) +( (if(sutuntop1>sutuntop2) 0 else 1) + (if(satirtop1>satirtop2) 1 else 0))*(lambda_symmetric-1))^2+FP*( (if(FP>TN) 1 else 0) + (if(TP>FP) 0 else 1) +( (if(sutuntop1>sutuntop2) 1 else 0) + (if(satirtop1<satirtop2) 1 else 0))*(lambda_symmetric-1))^2+TN*((if(FP>TN) 0 else 1)+ (if(FN>TN) 0 else 1) +((if(sutuntop1>sutuntop2) 0 else 1)+ (if(satirtop1<satirtop2) 1 else 0))*(lambda_symmetric-1))^2-4*toplam*lambda_symmetric^2)/(2*toplam-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2))^2)
    }
    
    lambda_symmetric_low = lambda_symmetric + qnorm((1-CI/100)/2)*lambda_symmetric_se
    lambda_symmetric_upp = lambda_symmetric - qnorm((1-CI/100)/2)*lambda_symmetric_se
    ## Lambda Asymmetric R|C
    lambda_criterion = (max(TP,FP)+max(FN,TN)-max(satirtop1,satirtop2))/(toplam-max(satirtop1,satirtop2))
    
    if (lambda_criterion==0 || lambda_criterion==1)
    {lambda_criterion_se = 0}
    else
    {
      lambda_criterion_se = sqrt((toplam-max(TP,FP)-max(FN,TN))*(max(TP,FP)+max(FN,TN)+max(satirtop1,satirtop2)-2*(if(satirtop1>satirtop2) max(TP,FN) else max(FP,TN)))/(toplam-max(satirtop1,satirtop2))^3)
    }
    
    lambda_criterion_low = lambda_criterion + qnorm((1-CI/100)/2)*lambda_criterion_se
    lambda_criterion_upp = lambda_criterion - qnorm((1-CI/100)/2)*lambda_criterion_se
    ## Lambda Asymmetric C|R
    lambda_criterion_2 = (max(TP,FN)+max(FP,TN)-max(sutuntop1,sutuntop2))/(toplam-max(sutuntop1,sutuntop2))
    
    if (lambda_criterion_2==0 || lambda_criterion_2==1)
    {lambda_criterion_se_2 = 0}
    else
    {
      lambda_criterion_se_2 = sqrt((toplam-max(TP,FN)-max(FP,TN))*(max(TP,FN)+max(FP,TN)+max(sutuntop1,sutuntop2)-2*(if(sutuntop1>sutuntop2) max(TP,FP) else max(FN,TN)))/(toplam-max(sutuntop1,sutuntop2))^3)
    }
    
    lambda_criterion_low_2 = lambda_criterion_2 + qnorm((1-CI/100)/2)*lambda_criterion_se_2
    lambda_criterion_upp_2 = lambda_criterion_2 - qnorm((1-CI/100)/2)*lambda_criterion_se_2
    ## belirsizlik katsayisi (coefficient of uncertainty) simetrik
    uncertainty_coefficient_symmetric = 2*(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam)))/(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))
    uncertainty_coefficient_symmetric_se = 2*sqrt(TP*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop1*sutuntop1/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(TP/toplam))^2+FN*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop1*sutuntop2/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(FN/toplam))^2+FP*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop2*sutuntop1/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(FP/toplam))^2+TN*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop2*sutuntop2/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(TN/toplam))^2)/toplam/(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam)^2
    uncertainty_coefficient_symmetric_low = uncertainty_coefficient_symmetric + qnorm((1-CI/100)/2)*uncertainty_coefficient_symmetric_se
    uncertainty_coefficient_symmetric_upp = uncertainty_coefficient_symmetric - qnorm((1-CI/100)/2)*uncertainty_coefficient_symmetric_se
    ## (coefficient of uncertainty) R|C
    uncertainty_coeff_crit = (satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam)))/(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))
    uncertainty_coeff_crit_se = sqrt(TP*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(TP/sutuntop1)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop1/toplam))^2+FN*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(FN/sutuntop2)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop1/toplam))^2+FP*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(FP/sutuntop1)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop2/toplam))^2+TN*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(TN/sutuntop2)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop2/toplam))^2)/toplam/((satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam)^2
    uncertainty_coeff_crit_low = uncertainty_coeff_crit + qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se
    uncertainty_coeff_crit_upp = uncertainty_coeff_crit - qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se
    ## (coefficient of uncertainty) C|R
    uncertainty_coeff_crit_2 = (sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)+satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)-(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam)))/(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))
    uncertainty_coeff_crit_se_2 = sqrt(TP*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(TP/satirtop1)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop1/toplam))^2+FP*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(FP/satirtop2)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop1/toplam))^2+FN*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(FN/satirtop1)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop2/toplam))^2+TN*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(TN/satirtop2)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop2/toplam))^2)/toplam/((sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)^2
    uncertainty_coeff_crit_low_2 = uncertainty_coeff_crit_2 + qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se_2
    uncertainty_coeff_crit_upp_2 = uncertainty_coeff_crit_2 - qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se_2
    ## Yule s Y (Coefficient of colligation)
    yule_y = (sqrt(TP*TN)-sqrt(FN*FP))/(sqrt(TP*TN)+sqrt(FN*FP))
    ## Pearson ki-kare
    pearson_chi_squ = toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)
    pearson_chi_squ_p = dchi(pearson_chi_squ, df=1)
    ## 
    with_yate_cor_for_pearson = toplam*(abs(TP*TN-FN*FP)-0.5*toplam)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)
    with_yate_cor_p_for_pearson = dchi(with_yate_cor_for_pearson, df=1)
    ## Mantel Haenszel chi-square
    ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
    mantel_haenszel = ((toplam-1)*(TP*TN-FN*FP)^2)/(satirtop1*satirtop2*sutuntop1*sutuntop2)
    mantel_haenszel_p = dchi(mantel_haenszel, df=1)
    ## Olasilik orani
    likelih_ratio = 2*(TP*log(TP/(sutuntop1*satirtop1/toplam))+FN*log(FN/(sutuntop2*satirtop1/toplam))+FP*log(FP/(sutuntop1*satirtop2/toplam))+TN*log(TN/(sutuntop2*satirtop2/toplam)))
    likelih_ratio_p =dchi(likelih_ratio, df=1)
    ## Fisher'in exact testi
    ##fisher_exact_test = (TP-(satirtop1*sutuntop1/toplam))^2/(satirtop1*sutuntop1/toplam) + (FN-(satirtop1*sutuntop2/toplam))^2/(satirtop1*sutuntop2/toplam) + (FP-(satirtop2*sutuntop1/toplam))^2/(satirtop2*sutuntop1/toplam) + (TN-(satirtop2*sutuntop2/toplam))^2/(satirtop2*sutuntop2/toplam)
    fisher <- c(TP, FN, FP, TN)
    tab <- t(matrix(fisher, nrow=2,ncol=2))
    fisher_exact_test = fisher.test(tab)$p.value
    minimum_exp_fre = min((sutuntop1*satirtop1/toplam):(sutuntop2*satirtop2/toplam))
    cells_exp_fre_5 = (if((sutuntop1*satirtop1/toplam)<5) 1 else 0)+ (if((sutuntop1*satirtop2/toplam)<5) 1 else 0)+ (if((sutuntop2*satirtop1/toplam) <5) 1 else 0)+ (if((sutuntop2*satirtop2/toplam) <5) 1 else 0)
    cells_exp_fre_1 = (if((sutuntop1*satirtop1/toplam) <1) 1 else 0)+ (if((sutuntop1*satirtop2/toplam) <1) 1 else 0)+ (if((sutuntop2*satirtop1/toplam) <1) 1 else 0)+ (if((sutuntop2*satirtop2/toplam) <1) 1 else 0)
    ## Mc Nemar testi
    mcNemar_test = (FP-FN)^2/(FP+FN)
    mcNemar_test_p = dchi(mcNemar_test, df=1)
    with_Yate_cor_for_mcnamer = (abs(FP-FN)-1)^2/(FP+FN)
    with_Yate_cor_p_for_mcnamer = dchi(with_Yate_cor_for_mcnamer, df=1)
    
    ## Belirsizlik (Entropi)
    forbes_NMI = sensitivity_estimate*predvalue_positiverandomtest*log((sensitivity_estimate/predvalue_positiverandomtest_mar_ol),base=2)+
      (1-specificity)*(1-predvalue_positiverandomtest)*log(((1-specificity)/predvalue_positiverandomtest_mar_ol),base=2)+
      (1-sensitivity_estimate)*predvalue_positiverandomtest*log(((1-sensitivity_estimate)/(1-predvalue_positiverandomtest_mar_ol)),base=2)+
      specificity*(1-predvalue_positiverandomtest)*log((specificity/(1-predvalue_positiverandomtest_mar_ol)),base=2)
    ## satir icin entropy (test icin)
    entropy_hr = - ((sutuntop1/toplam) * log(sutuntop1/toplam, base=2) + (sutuntop2/toplam) * log(sutuntop2/toplam, base=2))
    ## sutun icin entropy (hastalik icin)  
    entropy_hc = - ((satirtop1/toplam)*log(satirtop1/toplam, base=2) + (satirtop2/toplam)*log(satirtop2/toplam, base=2))
    ## birlesik entropi (joint entropy)  
    entropy_hrc = - ( (TP/toplam)*log(TP/toplam, base=2) + (FP/toplam)*log(FP/toplam, base=2) + (FN/toplam)*log(FN/toplam, base=2) + (TN/toplam)*log(TN/toplam, base=2))
    ## bilgi icerigi (mutual information)  
    information_r_c = entropy_hr + entropy_hc - entropy_hrc
    ## kosullu entropi (conditional entropy)  
    a = entropy_hrc - entropy_hr
    c = entropy_hrc - entropy_hc
    sim_r_c = information_r_c / (a + information_r_c + c)
    dis_r_c = (a + c) / (a + information_r_c + c)
    ## goreli entropi (relative entropy, kullback-leibler uzakligi)
    ## pozitif test sonucu icin goreli entropi, Relative Improvement Over Chance (RIOC)
    positive_relative_entropy = (TP/sutuntop1)*log(((TP/sutuntop1)/ prevalence), base=2)+(FP/sutuntop1)*log(((FP/sutuntop1)/(1- prevalence)), base=2)
    ## negatif test sonucu icin goreli entropi 
    negative_relative_entropy = (FN/sutuntop2)*log(((FN/sutuntop2)/ prevalence), base=2)+(TN/sutuntop2)*log(((TN/sutuntop2)/(1- prevalence)), base=2)
    
    verim <- data.frame(#Estimate=integer(),
      #LowerCI=integer(),
      #UpperCI=integer()
    )
    row <- vector(mode="character")
    if (cohens_kappa >= 0 && cohens_kappa <= 1) 
    {
      verim <- data.frame(Estimate = round(cohens_kappa, digit =3), LowerCI = round(cohens_kappa_low, digit =3), UpperCI = round(cohens_kappa_upp, digit =3))
      row <- c("Cohen\'s Kappa")
    } 
    if (observed_agreement >= 0 && observed_agreement <= 1)
    {
      newRow <- data.frame(Estimate = round(observed_agreement, digit =3),  LowerCI = round(observed_agreement_low, digit =3), UpperCI = round(observed_agreement_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Observed Agreement")
    }
    if (chance_agreement >= 0 && chance_agreement <= 1)
    {
      newRow <- data.frame(Estimate = round(chance_agreement, digit =3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Chance Agreement")
    }
    if (positive_agreement >= 0 && positive_agreement <= 1)
    {
      newRow <- data.frame(Estimate = round(positive_agreement, digit =3),  LowerCI = round(positive_agreement_low, digit =3), UpperCI = round(positive_agreement_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Positive Agreement")
    }
    if (negative_agreement >= 0 && negative_agreement <= 1)
    {
      newRow <- data.frame(Estimate = round(negative_agreement, digit =3),  LowerCI = round(negative_agreement_low, digit =3), UpperCI = round(negative_agreement_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Negative Agreement")
    }
    if (byrt_bias_index >= 0 && byrt_bias_index <= 1)
    {
      newRow <- data.frame(Estimate = round(byrt_bias_index, digit =3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Byrt\'s Bias Index")
    }
    if (byrt_prevalence_asymmetry_index >= 0 && byrt_prevalence_asymmetry_index <= 1)
    {
      newRow <- data.frame(Estimate = round(byrt_prevalence_asymmetry_index, digit =3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Byrt\'s Prevalence Asymmetry Index")
    }
    if (bias_adjusted_kappa >= 0 && bias_adjusted_kappa <= 1)
    {
      newRow <- data.frame(Estimate = round(bias_adjusted_kappa, digit =3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Bias Adjusted Kappa")
    }
    if (prevalence_bias_adjusted_kappa >= 0 && prevalence_bias_adjusted_kappa <= 1)
    {
      newRow <- data.frame(Estimate = round(prevalence_bias_adjusted_kappa, digit =3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Prevalence & Bias Adjusted Kappa")
    }

    rownames(verim) =  row
  
    highchart() %>% hc_exporting(enabled = TRUE, filename = "Plot2") %>% 
      hc_add_series(name = "Conf. Interval", type = "line", data = verim$`Estimate`) %>%
      hc_add_series(name = "CI", data = as.matrix(cbind(verim$`LowerCI`, verim$`UpperCI`)),type = "errorbar", names = "Limits", showInLegend = FALSE, zIndex = 0, lineWidth = 1.5, linkedTo = "CI"
      ) %>%
      hc_xAxis(categories = rownames(verim),title = "Agreement:") %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, headerFormat = "<b>Agreement: </b>{point.x} <br>") %>%
      hc_plotOptions(line = list(tooltip = list(pointFormat = "<b>{series.name}: </b>{point.y:.3f} ")), 
                     errorbar = list(tooltip = list(pointFormat = "({point.low} - {point.high})"))) %>%
      hc_add_theme(hc_theme_google())
    
 
    })
  
  output$plot21 <- highcharter::renderHighchart({
    if (input$verigir_kontrol == TRUE){ 
      TP = 0
      FN = 0
      FP = 0
      TN = 0
      for (i in 1:nrow(tabloveri()))
      {
        if (tabloveri()[i,1]==1 & tabloveri()[i,2]==1)
        {TP = TP+1}
        else if (tabloveri()[i,1]==1 & tabloveri()[i,2]==2)
        {FN = FN+1}
        else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==1)
        {FP = FP+1}
        else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==2)
        {TN = TN+1}
        next
      }
      CI = as.numeric(trimws(input$text5_2))
    }
    else{
      TP = as.numeric(trimws(input$text1)) ### x
      FN = as.numeric(trimws(input$text2)) ### y
      FP = as.numeric(trimws(input$text3)) ### z
      TN = as.numeric(trimws(input$text4)) ### t
      CI = as.numeric(trimws(input$text5))
    }
    
    
    satirtop1 = TP + FN
    satirtop2 = FP + TN
    sutuntop1 = TP + FP
    sutuntop2 = FN + TN
    toplam = satirtop1 + satirtop2
    #table1 <- matrix(c(TP,FN,satirtop1,FP,TN,satirtop2,sutuntop1,sutuntop2,toplam),nrow=3,ncol=3,byrow=TRUE,dimnames=list(c("1.satir","2.satir","Sutun toplamlari"),c("1.kolon","2.kolon","Satir toplamlari")))
    #table1
    ## Duyarlilik 
    sensitivity_estimate = TP / satirtop1
    sensitivity_se = round((sqrt(sensitivity_estimate * (1 - sensitivity_estimate)/satirtop1)), 4)
    sensitivity_low = 1 - Rbeta.inv(((1 + CI/100)/2), (satirtop1+1-TP), TP)
    sensitivity_upp = Rbeta.inv(((1 + CI/100)/2), (TP+1), (satirtop1-TP))
    sensitivity_randomtest = sutuntop1 / toplam
    qualityindex_sensitivity = (sensitivity_estimate - sensitivity_randomtest) / (1 - sensitivity_randomtest)
    ## Secicilik
    specificity = TN / satirtop2
    specificity_se = sqrt(specificity * (1 - specificity) / satirtop2)
    specificity_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop2+1-TN),TN)
    specificity_upp = Rbeta.inv((1 + CI/100)/2, (TN+1), (satirtop2-TN))
    specificity_randomtest = (1 - sensitivity_randomtest)
    qualityindex_specificity = (specificity - 1 + sensitivity_randomtest) / sensitivity_randomtest
    ## Gain in Certainty
    gain_in_certainty = sensitivity_estimate + specificity
    ## Etkililik, yeterlilik ayni zamanda overall accuracy (toplam-genel dogruluk orani)
    efficiency = (TP + TN) / toplam
    efficiency_se = sqrt(efficiency * (1 - efficiency) / toplam)
    efficiency_low = 1-Rbeta.inv((1 + CI/100)/2, (toplam+1-TP-TN), (TP+TN))
    efficiency_upp = Rbeta.inv((1 + CI/100)/2, (TP+TN+1), (toplam-TP-TN))
    efficiency_randomtest = (satirtop1 / toplam) * sensitivity_randomtest + (1 - (satirtop1 / toplam)) * (1 - sensitivity_randomtest)
    mis_efficiency = 1 - efficiency
    ## Quality indeks
    quality_index = (efficiency - efficiency_randomtest) / (1 - efficiency_randomtest)
    quality_index_se = sqrt((efficiency*(1-efficiency)/(1-efficiency_randomtest)^2+2*(1-efficiency)*(2*efficiency*efficiency_randomtest-((TP*((satirtop1 / toplam)+sensitivity_randomtest)+TN*(2-(satirtop1 / toplam)-sensitivity_randomtest))/toplam))/(1-efficiency_randomtest)^3+((1-efficiency)^2)*((TP*((satirtop1 / toplam)+sensitivity_randomtest)^2+FN*(-(satirtop1 / toplam)+1+sensitivity_randomtest)^2+FP*(1+(satirtop1 / toplam)-sensitivity_randomtest)^2+TN*(2-(satirtop1 / toplam)-sensitivity_randomtest)^2)/toplam-4*efficiency_randomtest^2)/(1-efficiency_randomtest)^4)/toplam)
    quality_index_low = quality_index + qnorm((1 - CI/100)/2)*quality_index_se
    quality_index_upp = quality_index - qnorm((1 - CI/100)/2)*quality_index_se
    ## Youden indeksi
    youdens_index = sensitivity_estimate + specificity - 1
    youdens_index_se = sqrt(sensitivity_se^2+specificity_se^2)
    youdens_index_low = youdens_index + qnorm((1 - CI/100)/2)*youdens_index_se
    youdens_index_upp = youdens_index - qnorm((1 - CI/100)/2)*youdens_index_se
    
    number_needed_to_diagnose = 1 / youdens_index
    number_needed_to_misdiagnose = 1 / ( 1 - efficiency ) 
    ## Pozitif kestirim degeri
    predictivevalue_positivetest = TP / sutuntop1
    predictivevalue_positivetest_se = sqrt(predictivevalue_positivetest*(1-predictivevalue_positivetest)/sutuntop1)
    predictivevalue_positivetest_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop1+1-TP),TP)
    predictivevalue_positivetest_upp = Rbeta.inv((1 + CI/100)/2, (TP+1),(sutuntop1-TP))
    predvalue_positiverandomtest = (satirtop1 / toplam)
    predvalue_positiverandomtest_mar_ol = predvalue_positiverandomtest*sensitivity_estimate+(1-predvalue_positiverandomtest)*(1-specificity)
    ## Negatif kestirim degeri
    predictivevalue_negativetest = TN / sutuntop2
    predictivevalue_negativetest_se = sqrt(predictivevalue_negativetest*(1-predictivevalue_negativetest)/sutuntop2)
    predictivevalue_negativetest_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop2+1-TN),TN)
    predictivevalue_negativetest_upp = Rbeta.inv((1 + CI/100)/2, (TN+1), (sutuntop2-TN))
    predvalue_negativerandomtest = 1 - (satirtop1 / toplam)
    predvalue_negativerandomtest_mar_ol = 1 - predvalue_positiverandomtest_mar_ol
    ## Predictive Summary Index (PSI)
    predictive_summary_index = predictivevalue_positivetest + predictivevalue_negativetest - 1
    ## Yanlis pozitif orani
    false_positiverate = FP / satirtop2
    false_positiverate_se = sqrt(false_positiverate*(1-false_positiverate)/satirtop2)
    false_positiverate_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop2+1-FP),FP)
    false_positiverate_upp = Rbeta.inv((1 + CI/100)/2, (FP+1), (satirtop2-FP))
    ## Yansis negatif orani
    false_negativerate = FN / satirtop1
    false_negativerate_se = sqrt(false_negativerate*(1-false_negativerate)/satirtop1)
    false_negativerate_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop1+1-FN),FN)
    false_negativerate_upp = Rbeta.inv((1 + CI/100)/2, (FN+1), (satirtop1-FN))
    ## False Omission Rate (FOR) https://www.ncss.com/software/ncss/diagnostic-tests-in-ncss/
    false_omission_rate = FN/(sutuntop2)
    false_omission_rate_se = sqrt(false_omission_rate*(1-false_omission_rate)/sutuntop2)
    false_omission_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop2+1-FN),FN)
    false_omission_rate_upp = Rbeta.inv((1 + CI/100)/2, (FN+1), (sutuntop2-FN))
    ## False Discovery Rate (FDR) https://www.ncss.com/software/ncss/diagnostic-tests-in-ncss/
    false_discovery_rate = FP/(sutuntop1)
    false_discovery_rate_se = sqrt(false_discovery_rate*(1-false_discovery_rate)/sutuntop1)
    false_discovery_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop1+1-FP),FP)
    false_discovery_rate_upp = Rbeta.inv((1 + CI/100)/2, (FP+1), (sutuntop1-FP))
    ## Dogru pozitif orani
    true_positiverate = TP / (satirtop1)
    ## Yanlis siniflandirma orani
    misclassification_rate = (FN + FP) / toplam
    misclassification_rate_se = sqrt(misclassification_rate*(1-misclassification_rate)/toplam)
    misclassification_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-FP-FN), (FP+FN))
    misclassification_rate_upp = Rbeta.inv((1 + CI/100)/2, (FP+FN+1), (toplam-FP-FN))
    ## Prevelans
    prevalence = (satirtop1 / toplam)
    prevalence_se = sqrt((satirtop1 / toplam)*(1-(satirtop1 / toplam))/toplam)
    prevalence_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-satirtop1), satirtop1)
    prevalence_upp = Rbeta.inv((1 + CI/100)/2, (satirtop1+1), (toplam-satirtop1))
    
    ##caret paketinden olculer
    detection_rate = TP/toplam
    detection_prevalence = (TP+FP)/toplam
    balanced_accuracy = (sensitivity_estimate + specificity)/2
    lift = (TP/(TP+FN))/((TP+FP)/toplam)  ## https://gerardnico.com/wiki/data_mining/error_rate
    precision = TP/(TP+FP)
    recall = TP/(TP+FN)
    ## http://michael.hahsler.net/research/recomm_lnai2002/lnai2002.pdf
    conviction = (1-((TP+FP)/toplam))/(1-(TP/(TP+FN)))
    matthews_correlation_coefficient = (TP * TN + FN * FP) / ((TP+FP)*(FN+TN)*(FP+TN)*(TP+FN))^(1/2)
    ## Goreli Risk Orani (Relative risk)
    ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
    # https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20ratio.htm
    relative_risk = (TP/sutuntop1)/(FN/sutuntop2)
    relative_risk_se = sqrt((1/TP)+(1/FN)-(1/(TP+FP))-(1/(FN+TN)))
    relative_risk_low = exp(log(relative_risk)+qnorm((1-CI/100)/2)*relative_risk_se)
    relative_risk_upp = exp(log(relative_risk)-qnorm((1-CI/100)/2)*relative_risk_se)
    difference_in_proportion = (TP/sutuntop1) - (FN/sutuntop2)
    ## Leverage
    ## https://www3.nd.edu/~tweninge/pubs/AHBCW_ANNIE08.pdf
    leverage = (TP/sutuntop1)-((TP+FP)/toplam)
    ## http://www.ebm.med.ualberta.ca/TherapyCalc.html
    control_event_rate = FN/sutuntop2
    experimental_event_rate = TP/sutuntop1
    ## Absolute risk reduction
    absolute_risk_reduction = -difference_in_proportion
    absolute_risk_reduction_se = sqrt((TP/sutuntop1)*(1-(TP/sutuntop1))/sutuntop1+(FN/sutuntop2)*(1-(FN/sutuntop2))/sutuntop2)
    absolute_risk_reduction_low = absolute_risk_reduction - qnorm((1-CI/100)/2)*absolute_risk_reduction_se
    absolute_risk_reduction_upp = absolute_risk_reduction + qnorm((1-CI/100)/2)*absolute_risk_reduction_se
    ## Relative risk reduction
    relative_risk_reduction = absolute_risk_reduction / (FN/sutuntop2)
    relative_risk_reduction_low = 1 - exp(log(relative_risk)-qnorm((1-CI/100)/2)*relative_risk_se)
    relative_risk_reduction_upp = 1 - exp(log(relative_risk)+qnorm((1-CI/100)/2)*relative_risk_se)
    ##Number needed to treat
    number_needed_to_treat = 1 / abs(difference_in_proportion)
    number_needed_to_treat_low = 1 /absolute_risk_reduction_upp
    number_needed_to_treat_upp = 1 /absolute_risk_reduction_low
    ## Test duzeyi
    test_level = sensitivity_randomtest
    test_level_se = sqrt(test_level*(1-test_level)/toplam)
    test_level_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-sutuntop1),sutuntop1)
    test_level_upp = Rbeta.inv((1 + CI/100)/2, (sutuntop1+1), (toplam-sutuntop1))
    ## Pre-test odds
    pretest_odds = prevalence / (1 - prevalence)
    ## Pozitif Olabilirlik Orani
    likelihoodratio_positivetest = sensitivity_estimate/(1-specificity)
    likelihoodratio_positivetest_se = exp(sqrt((1-sensitivity_estimate)/(sensitivity_estimate*satirtop1) +specificity/((1-specificity)*satirtop2)))
    likelihoodratio_positivetest_low = exp(log(likelihoodratio_positivetest) + qnorm((1 - CI/100)/2)*log(likelihoodratio_positivetest_se))
    likelihoodratio_positivetest_upp = exp(log(likelihoodratio_positivetest) - qnorm((1 - CI/100)/2)*log(likelihoodratio_positivetest_se))
    ## post-test odds
    posttest_odds = pretest_odds * likelihoodratio_positivetest
    bayes_ppv = (sensitivity_estimate * prevalence)/((sensitivity_estimate * prevalence) + (1 - specificity) * (1 - prevalence))
    ## Post-test probability (positive test result)
    posttest_probability = (pretest_odds * likelihoodratio_positivetest) / (1 + (pretest_odds * likelihoodratio_positivetest))
    ## Negatif Olabilirlik Orani
    likelihoodratio_negativetest = (1-sensitivity_estimate)/specificity
    likelihoodratio_negativetest_se = exp(sqrt(sensitivity_estimate/((1-sensitivity_estimate)*satirtop1) +(1-specificity)/(specificity*satirtop2)))
    likelihoodratio_negativetest_low = exp(log(likelihoodratio_negativetest) + qnorm((1-CI/100)/2)*log(likelihoodratio_negativetest_se))
    likelihoodratio_negativetest_upp = exp(log(likelihoodratio_negativetest) - qnorm((1-CI/100)/2)*log(likelihoodratio_negativetest_se))
    bayes_npv = (sensitivity_estimate * (1 - prevalence))/((sensitivity_estimate * (1 - prevalence)) + (1 - specificity) * prevalence)
    ## Post-test probability (negative test result)
    posttest_probability_neg = (pretest_odds * likelihoodratio_negativetest) / (1 + (pretest_odds * likelihoodratio_negativetest))
    ## Ters negatif olabilirlik orani
    inverse_likelihoodratio_negativetest = specificity/(1-sensitivity_estimate)
    inverse_likelihoodratio_negativetest_se = exp(sqrt(sensitivity_estimate/((1-sensitivity_estimate)*satirtop1) +(1-specificity)/(specificity*satirtop2)))
    inverse_likelihoodratio_negativetest_low = exp(log(inverse_likelihoodratio_negativetest) + qnorm((1-CI/100)/2)*log(inverse_likelihoodratio_negativetest_se))
    inverse_likelihoodratio_negativetest_upp = exp(log(inverse_likelihoodratio_negativetest) - qnorm((1-CI/100)/2)*log(inverse_likelihoodratio_negativetest_se))
    ## Diagnostic Odds Ratio (Tani Odds Orani)
    odds_ratio = (TP/FN)/(FP/TN)
    odds_ratio_se = exp(sqrt(1/TP+1/FP+1/TN+1/FN))
    odds_ratio_low = exp(log(odds_ratio) + qnorm((1-CI/100)/2)*log(odds_ratio_se))
    odds_ratio_upp = exp(log(odds_ratio) - qnorm((1-CI/100)/2)*log(odds_ratio_se))
    odds_ratio_Haldane = ((TP+0.5)/(FN+0.5))/((FP+0.5)/(TN+0.5))
    odds_ratio_Haldane_se = exp(sqrt(1/(TP+0.5)+1/(FP+0.5)+1/(TN+0.5)+1/(FN+0.5)))
    odds_ratio_Haldane_low = exp(log(odds_ratio_Haldane) + qnorm((1-CI/100)/2)*log(odds_ratio_Haldane_se))
    odds_ratio_Haldane_upp = exp(log(odds_ratio_Haldane) - qnorm((1-CI/100)/2)*log(odds_ratio_Haldane_se))
    error_odds_ratio = (sensitivity_estimate/(1-sensitivity_estimate))/(specificity/(1-specificity))
    ## Rate ratio
    ## http://omarkasule-05.tripod.com/id52.html
    ## http://sphweb.bumc.bu.edu/otlt/mph-modules/ep/ep713_randomerror/ep713_randomerror4.html
    rate_ratio = (TP/sutuntop1)/(TP/sutuntop2)
    rate_ratio_low = exp(log(relative_risk)+qnorm((1-CI/100)/2)*sqrt(1/TP+1/FP))
    rate_ratio_upp = exp(log(relative_risk)-qnorm((1-CI/100)/2)*sqrt(1/TP+1/FP))
    ## Risk Difference
    ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
    risk_difference = (TP/sutuntop1) - (FN/sutuntop2)
    risk_difference_se = (((TP/sutuntop1)*(1-(TP/sutuntop1)))/sutuntop1)+(((FN/sutuntop2)*(1-(FN/sutuntop2)))/sutuntop2)
    risk_difference_low = risk_difference + qnorm((1-CI/100)/2)*sqrt(risk_difference_se)
    risk_difference_upp = risk_difference - qnorm((1-CI/100)/2)*sqrt(risk_difference_se)
    ## Nitelenebilen risk
    ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
    attributable_risk = (TP/sutuntop1)-(FN/sutuntop2)
    attributable_risk_se = sqrt((satirtop1/toplam)*(1-(satirtop1/toplam))*(1/sutuntop1+1/sutuntop2))
    attributable_risk_low = attributable_risk + qnorm((1-CI/100)/2)*attributable_risk_se
    attributable_risk_upp = attributable_risk - qnorm((1-CI/100)/2)*attributable_risk_se
    ## Attributable risk percent
    ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
    attributable_risk_percent = ((relative_risk-1)/relative_risk)*100
    attributable_risk_percent_low = attributable_risk_percent - (qnorm((1-CI/100)/2)*attributable_risk_se)/attributable_risk
    attributable_risk_percent_upp = attributable_risk_percent + (qnorm((1-CI/100)/2)*attributable_risk_se)/attributable_risk
    ## Population Attributable Risk
    ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
    population_attributable_risk = (satirtop1/toplam)-(FN/sutuntop2)
    ## Population Attributable Risk Percent
    population_attributable_risk_percent = (population_attributable_risk / (satirtop1/toplam))*100
    ## Kohen's Kappa
    cohens_kappa = (efficiency-efficiency_randomtest)/(1-efficiency_randomtest)
    cohens_kappa_se = quality_index_se
    cohens_kappa_low = cohens_kappa + qnorm((1-CI/100)/2)*cohens_kappa_se
    cohens_kappa_upp = cohens_kappa - qnorm((1-CI/100)/2)*cohens_kappa_se
    ## Gozlenen Karar
    observed_agreement = efficiency
    observed_agreement_se = efficiency_se
    observed_agreement_low = efficiency_low
    observed_agreement_upp = efficiency_upp
    ## Risk karari,expected agreement
    chance_agreement = efficiency_randomtest
    ## Pozitif karar
    positive_agreement = 2*TP /(satirtop1+sutuntop1)
    positive_agreement_se = sqrt(4*TP*(FP+FN)*(TP+FP+FN))/(sutuntop1+satirtop1)^2
    positive_agreement_low = positive_agreement + qnorm((1-CI/100)/2)*positive_agreement_se
    positive_agreement_upp = positive_agreement - qnorm((1-CI/100)/2)*positive_agreement_se
    ## Negatif karar
    negative_agreement = 2*TN/(satirtop2+sutuntop2)
    negative_agreement_se = sqrt(4*TN*(FP+FN)*(TN+FP+FN))/(sutuntop2+satirtop2)^2
    negative_agreement_low = negative_agreement + qnorm((1-CI/100)/2)*negative_agreement_se
    negative_agreement_upp = negative_agreement - qnorm((1-CI/100)/2)*negative_agreement_se
    ## Rand index
    ## http://www.cs.pomona.edu/classes/cs158/resources/158-10(EM).pdf
    rand_index = (TP+TN)/(TP+FN+FP+TN)
    ## e-measure
    ## http://michael.hahsler.net/research/recomm_lnai2002/lnai2002.pdf
    e_measure = 1/((((1-CI)/100)*(1-precision))+((1-((1-CI)/100))*(1/recall)))
    ## discriminant power 
    ## https://eva.fing.edu.uy/pluginfile.php/69453/mod_resource/content/1/7633-10048-1-PB.pdf
    discriminant_power = (sqrt(3)/pi)*(log (sensitivity_estimate / (1-sensitivity_estimate)) + log (specificity / (1-specificity)))
    ## F1 Score
    ## http://machinelearningmastery.com/classification-accuracy-is-not-enough-more-performance-measures-you-can-use/
    f1_score = 2*(((TP/(TP+FN))*(TP/(TP+FP)))/((TP/(TP+FN))+(TP/(TP+FP))))
    ## Byrt yanlilik indeksi
    byrt_bias_index = (FN-FP)/toplam
    ## Byrt  asimetrik indeks prevelansi
    byrt_prevalence_asymmetry_index = (TN-TP)/toplam
    ## Yanlilik duzeltmeli kappa
    bias_adjusted_kappa = (efficiency-0.25*((satirtop1+sutuntop1)^2+(satirtop2+sutuntop2)^2)/toplam^2)/(1-0.25*((satirtop1+sutuntop1)^2+(satirtop2+sutuntop2)^2)/toplam^2)
    ## Yanlilik duzeltmeli kappa prevelansi
    prevalence_bias_adjusted_kappa = cohens_kappa+(1-cohens_kappa)*(byrt_prevalence_asymmetry_index^2-byrt_bias_index^2)
    ## Dice'in indeksi (Czekanowski)
    dice_index = positive_agreement
    dice_index_se= positive_agreement_se
    dice_index_low = positive_agreement_low
    dice_index_upp = positive_agreement_upp
    ## Yule's Q
    yules_q = (odds_ratio - 1) / (odds_ratio + 1)
    yules_q_se = 0.5*(1-yules_q^2)*sqrt(1/TP+1/FP+1/TN+1/FN)
    yules_q_low = yules_q + qnorm((1-CI/100)/2)*yules_q_se
    yules_q_upp = yules_q - qnorm((1-CI/100)/2)*yules_q_se
    
    equitable_threatscore = (TP-(sutuntop1*satirtop1/toplam))/(TP+FP+FN-(sutuntop1*satirtop1/toplam))
    ## Phi
    phi = (TP*TN-FN*FP) / sqrt(satirtop1 * satirtop2 * sutuntop1 * sutuntop2)
    phi_se = sqrt((1-phi^2+(phi+0.5*phi^3)*((satirtop1-satirtop2)*(sutuntop1-sutuntop2)/sqrt(satirtop1*satirtop2*sutuntop1*sutuntop2))-0.75*phi^2*((satirtop1-satirtop2)^2/(satirtop1*satirtop2)+(sutuntop1-sutuntop2)^2/(sutuntop1*sutuntop2)))/toplam)
    phi_low = phi + qnorm((1-CI/100)/2)*phi_se
    phi_upp = phi - qnorm((1-CI/100)/2)*phi_se
    ## Cramer V katsayisi
    cramer_v = sqrt((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2))/toplam)
    ## Olaganlik katsayisi
    contingency_coefficient = sqrt((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2))/(((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)))+toplam))
    ## Goodman and Kruskal Gamma 
    goodman_and_kruskal_gamma = (TP*TN-FN*FP)/(TP*TN+FN*FP)
    goodman_and_kruskal_gamma_se = 0.5*(1-yules_q^2)*sqrt(1/TP+1/FP+1/TN+1/FN)
    goodman_and_kruskal_gamma_low = yules_q + qnorm((1-CI/100)/2)*yules_q_se
    goodman_and_kruskal_gamma_upp = yules_q - qnorm((1-CI/100)/2)*yules_q_se
    ## Kendall's tau a
    kendalls_tau_a = (TP*TN-FN*FP)/(toplam*(toplam-1)/2)
    ## Kendall's tau b
    ## https://v8doc.sas.com/sashtml/stat/chap28/sect20.htm
    ## http://slideplayer.com/slide/10766029/
    kendalls_tau_b = (TP*TN-FN*FP)/sqrt((TP*TN+FN*FP+(TP*FN+FP*TN))*(TP*TN+FN*FP+(TP*FP+FN*TN)))
    ## Kendall's tau c
    kendalls_tau_c = ((2*2)*(TP*TN-FN*FP)) / (toplam^2 * (2-1))
    ## Somers'd R|C
    somers_d = (TP*TN-FN*FP)/(TP*TN+FN*FP+(TP*FN+FP*TN))
    ## Somers'd C|R
    somers_d_2 = (TP*TN-FP*FN)/(TP*TN+FP*FN+(TP*FP+FN*TN))
    ## Scoot'un karar indeksi
    scotts_agreement_index = bias_adjusted_kappa
    ## Dort-duzeyli Korelasyon
    tetrachoric_correlation = cos(pi/(1+sqrt(TP*TN/FN/FP)))
    tetrachoric_correlation_se = 0.375*(1-tetrachoric_correlation^2)*sqrt((1/(TP))+(1/(FP))+(1/(TN))+(1/(FN)))
    tetrachoric_correlation_low = tetrachoric_correlation + qnorm((1-CI/100)/2)*tetrachoric_correlation_se
    tetrachoric_correlation_upp = tetrachoric_correlation - qnorm((1-CI/100)/2)*tetrachoric_correlation_se
    ## Goodman kruskal tau katsayisi
    goodman_kruskal_tau = (toplam*((TP^2+FP^2)/sutuntop1+(FN^2+TN^2)/sutuntop2)-satirtop1^2-satirtop2^2)/(toplam^2-satirtop1^2-satirtop2^2)
    goodman_kruskal_tau_se = toplam*sqrt(TP*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop1)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*TP/satirtop1-(TP^2+FN^2)/satirtop1^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+FN*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop2)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*FN/satirtop1-(TP^2+FN^2)/satirtop1^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+FP*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop1)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*FP/satirtop2-(FP^2+TN^2)/satirtop2^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+TN*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop2)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*TN/satirtop2-(FP^2+TN^2)/satirtop2^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2)/(toplam^2-sutuntop1^2-sutuntop2^2)^2
    goodman_kruskal_tau_low = goodman_kruskal_tau + qnorm((1-CI/100)/2)*goodman_kruskal_tau_se
    goodman_kruskal_tau_upp = goodman_kruskal_tau - qnorm((1-CI/100)/2)*goodman_kruskal_tau_se
    ## Simetrik Lambda 
    lambda_symmetric = ((max(TP,FN)+max(FP,TN)+max(TP,FP)+max(FN,TN)-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2))/(2*toplam-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2)))
    if (lambda_symmetric==0 || lambda_symmetric==1)
    {lambda_symmetric_se = 0}
    else
    {
      lambda_symmetric_se = sqrt((TP*((if(TP>FN) 1 else 0) + (if(TP>FP) 1 else 0) + ((if(sutuntop1>sutuntop2) 1 else 0)+ (if(satirtop1>satirtop2) 1 else 0))*(lambda_symmetric-1))^2+FN*( (if(TP>FN) 0 else 1)+ (if(FN>TN) 1 else 0) +( (if(sutuntop1>sutuntop2) 0 else 1) + (if(satirtop1>satirtop2) 1 else 0))*(lambda_symmetric-1))^2+FP*( (if(FP>TN) 1 else 0) + (if(TP>FP) 0 else 1) +( (if(sutuntop1>sutuntop2) 1 else 0) + (if(satirtop1<satirtop2) 1 else 0))*(lambda_symmetric-1))^2+TN*((if(FP>TN) 0 else 1)+ (if(FN>TN) 0 else 1) +((if(sutuntop1>sutuntop2) 0 else 1)+ (if(satirtop1<satirtop2) 1 else 0))*(lambda_symmetric-1))^2-4*toplam*lambda_symmetric^2)/(2*toplam-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2))^2)
    }
    
    lambda_symmetric_low = lambda_symmetric + qnorm((1-CI/100)/2)*lambda_symmetric_se
    lambda_symmetric_upp = lambda_symmetric - qnorm((1-CI/100)/2)*lambda_symmetric_se
    ## Lambda Asymmetric R|C
    lambda_criterion = (max(TP,FP)+max(FN,TN)-max(satirtop1,satirtop2))/(toplam-max(satirtop1,satirtop2))
    
    if (lambda_criterion==0 || lambda_criterion==1)
    {lambda_criterion_se = 0}
    else
    {
      lambda_criterion_se = sqrt((toplam-max(TP,FP)-max(FN,TN))*(max(TP,FP)+max(FN,TN)+max(satirtop1,satirtop2)-2*(if(satirtop1>satirtop2) max(TP,FN) else max(FP,TN)))/(toplam-max(satirtop1,satirtop2))^3)
    }
    
    lambda_criterion_low = lambda_criterion + qnorm((1-CI/100)/2)*lambda_criterion_se
    lambda_criterion_upp = lambda_criterion - qnorm((1-CI/100)/2)*lambda_criterion_se
    ## Lambda Asymmetric C|R
    lambda_criterion_2 = (max(TP,FN)+max(FP,TN)-max(sutuntop1,sutuntop2))/(toplam-max(sutuntop1,sutuntop2))
    
    if (lambda_criterion_2==0 || lambda_criterion_2==1)
    {lambda_criterion_se_2 = 0}
    else
    {
      lambda_criterion_se_2 = sqrt((toplam-max(TP,FN)-max(FP,TN))*(max(TP,FN)+max(FP,TN)+max(sutuntop1,sutuntop2)-2*(if(sutuntop1>sutuntop2) max(TP,FP) else max(FN,TN)))/(toplam-max(sutuntop1,sutuntop2))^3)
    }
    
    lambda_criterion_low_2 = lambda_criterion_2 + qnorm((1-CI/100)/2)*lambda_criterion_se_2
    lambda_criterion_upp_2 = lambda_criterion_2 - qnorm((1-CI/100)/2)*lambda_criterion_se_2
    ## belirsizlik katsayisi (coefficient of uncertainty) simetrik
    uncertainty_coefficient_symmetric = 2*(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam)))/(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))
    uncertainty_coefficient_symmetric_se = 2*sqrt(TP*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop1*sutuntop1/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(TP/toplam))^2+FN*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop1*sutuntop2/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(FN/toplam))^2+FP*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop2*sutuntop1/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(FP/toplam))^2+TN*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop2*sutuntop2/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(TN/toplam))^2)/toplam/(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam)^2
    uncertainty_coefficient_symmetric_low = uncertainty_coefficient_symmetric + qnorm((1-CI/100)/2)*uncertainty_coefficient_symmetric_se
    uncertainty_coefficient_symmetric_upp = uncertainty_coefficient_symmetric - qnorm((1-CI/100)/2)*uncertainty_coefficient_symmetric_se
    ## (coefficient of uncertainty) R|C
    uncertainty_coeff_crit = (satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam)))/(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))
    uncertainty_coeff_crit_se = sqrt(TP*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(TP/sutuntop1)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop1/toplam))^2+FN*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(FN/sutuntop2)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop1/toplam))^2+FP*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(FP/sutuntop1)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop2/toplam))^2+TN*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(TN/sutuntop2)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop2/toplam))^2)/toplam/((satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam)^2
    uncertainty_coeff_crit_low = uncertainty_coeff_crit + qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se
    uncertainty_coeff_crit_upp = uncertainty_coeff_crit - qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se
    ## (coefficient of uncertainty) C|R
    uncertainty_coeff_crit_2 = (sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)+satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)-(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam)))/(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))
    uncertainty_coeff_crit_se_2 = sqrt(TP*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(TP/satirtop1)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop1/toplam))^2+FP*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(FP/satirtop2)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop1/toplam))^2+FN*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(FN/satirtop1)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop2/toplam))^2+TN*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(TN/satirtop2)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop2/toplam))^2)/toplam/((sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)^2
    uncertainty_coeff_crit_low_2 = uncertainty_coeff_crit_2 + qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se_2
    uncertainty_coeff_crit_upp_2 = uncertainty_coeff_crit_2 - qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se_2
    ## Yule s Y (Coefficient of colligation)
    yule_y = (sqrt(TP*TN)-sqrt(FN*FP))/(sqrt(TP*TN)+sqrt(FN*FP))
    ## Pearson ki-kare
    pearson_chi_squ = toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)
    pearson_chi_squ_p = dchi(pearson_chi_squ, df=1)
    ## 
    with_yate_cor_for_pearson = toplam*(abs(TP*TN-FN*FP)-0.5*toplam)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)
    with_yate_cor_p_for_pearson = dchi(with_yate_cor_for_pearson, df=1)
    ## Mantel Haenszel chi-square
    ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
    mantel_haenszel = ((toplam-1)*(TP*TN-FN*FP)^2)/(satirtop1*satirtop2*sutuntop1*sutuntop2)
    mantel_haenszel_p = dchi(mantel_haenszel, df=1)
    ## Olasilik orani
    likelih_ratio = 2*(TP*log(TP/(sutuntop1*satirtop1/toplam))+FN*log(FN/(sutuntop2*satirtop1/toplam))+FP*log(FP/(sutuntop1*satirtop2/toplam))+TN*log(TN/(sutuntop2*satirtop2/toplam)))
    likelih_ratio_p =dchi(likelih_ratio, df=1)
    ## Fisher'in exact testi
    ##fisher_exact_test = (TP-(satirtop1*sutuntop1/toplam))^2/(satirtop1*sutuntop1/toplam) + (FN-(satirtop1*sutuntop2/toplam))^2/(satirtop1*sutuntop2/toplam) + (FP-(satirtop2*sutuntop1/toplam))^2/(satirtop2*sutuntop1/toplam) + (TN-(satirtop2*sutuntop2/toplam))^2/(satirtop2*sutuntop2/toplam)
    fisher <- c(TP, FN, FP, TN)
    tab <- t(matrix(fisher, nrow=2,ncol=2))
    fisher_exact_test = fisher.test(tab)$p.value
    minimum_exp_fre = min((sutuntop1*satirtop1/toplam):(sutuntop2*satirtop2/toplam))
    cells_exp_fre_5 = (if((sutuntop1*satirtop1/toplam)<5) 1 else 0)+ (if((sutuntop1*satirtop2/toplam)<5) 1 else 0)+ (if((sutuntop2*satirtop1/toplam) <5) 1 else 0)+ (if((sutuntop2*satirtop2/toplam) <5) 1 else 0)
    cells_exp_fre_1 = (if((sutuntop1*satirtop1/toplam) <1) 1 else 0)+ (if((sutuntop1*satirtop2/toplam) <1) 1 else 0)+ (if((sutuntop2*satirtop1/toplam) <1) 1 else 0)+ (if((sutuntop2*satirtop2/toplam) <1) 1 else 0)
    ## Mc Nemar testi
    mcNemar_test = (FP-FN)^2/(FP+FN)
    mcNemar_test_p = dchi(mcNemar_test, df=1)
    with_Yate_cor_for_mcnamer = (abs(FP-FN)-1)^2/(FP+FN)
    with_Yate_cor_p_for_mcnamer = dchi(with_Yate_cor_for_mcnamer, df=1)
    
    ## Belirsizlik (Entropi)
    forbes_NMI = sensitivity_estimate*predvalue_positiverandomtest*log((sensitivity_estimate/predvalue_positiverandomtest_mar_ol),base=2)+
      (1-specificity)*(1-predvalue_positiverandomtest)*log(((1-specificity)/predvalue_positiverandomtest_mar_ol),base=2)+
      (1-sensitivity_estimate)*predvalue_positiverandomtest*log(((1-sensitivity_estimate)/(1-predvalue_positiverandomtest_mar_ol)),base=2)+
      specificity*(1-predvalue_positiverandomtest)*log((specificity/(1-predvalue_positiverandomtest_mar_ol)),base=2)
    ## satir icin entropy (test icin)
    entropy_hr = - ((sutuntop1/toplam) * log(sutuntop1/toplam, base=2) + (sutuntop2/toplam) * log(sutuntop2/toplam, base=2))
    ## sutun icin entropy (hastalik icin)  
    entropy_hc = - ((satirtop1/toplam)*log(satirtop1/toplam, base=2) + (satirtop2/toplam)*log(satirtop2/toplam, base=2))
    ## birlesik entropi (joint entropy)  
    entropy_hrc = - ( (TP/toplam)*log(TP/toplam, base=2) + (FP/toplam)*log(FP/toplam, base=2) + (FN/toplam)*log(FN/toplam, base=2) + (TN/toplam)*log(TN/toplam, base=2))
    ## bilgi icerigi (mutual information)  
    information_r_c = entropy_hr + entropy_hc - entropy_hrc
    ## kosullu entropi (conditional entropy)  
    a = entropy_hrc - entropy_hr
    c = entropy_hrc - entropy_hc
    sim_r_c = information_r_c / (a + information_r_c + c)
    dis_r_c = (a + c) / (a + information_r_c + c)
    ## goreli entropi (relative entropy, kullback-leibler uzakligi)
    ## pozitif test sonucu icin goreli entropi, Relative Improvement Over Chance (RIOC)
    positive_relative_entropy = (TP/sutuntop1)*log(((TP/sutuntop1)/ prevalence), base=2)+(FP/sutuntop1)*log(((FP/sutuntop1)/(1- prevalence)), base=2)
    ## negatif test sonucu icin goreli entropi 
    negative_relative_entropy = (FN/sutuntop2)*log(((FN/sutuntop2)/ prevalence), base=2)+(TN/sutuntop2)*log(((TN/sutuntop2)/(1- prevalence)), base=2)
    
    verim <- data.frame(#Estimate=integer(),
      #LowerCI=integer(),
      #UpperCI=integer()
    )
    row <- vector(mode="character")
    if (cohens_kappa >= 0 && cohens_kappa <= 1) 
    {
      verim <- data.frame(Estimate = round(cohens_kappa, digit =3), LowerCI = round(cohens_kappa_low, digit =3), UpperCI = round(cohens_kappa_upp, digit =3))
      row <- c("Cohen\'s Kappa")
    } 
    if (observed_agreement >= 0 && observed_agreement <= 1)
    {
      newRow <- data.frame(Estimate = round(observed_agreement, digit =3),  LowerCI = round(observed_agreement_low, digit =3), UpperCI = round(observed_agreement_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Observed Agreement")
    }
    if (chance_agreement >= 0 && chance_agreement <= 1)
    {
      newRow <- data.frame(Estimate = round(chance_agreement, digit =3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Chance Agreement")
    }
    if (positive_agreement >= 0 && positive_agreement <= 1)
    {
      newRow <- data.frame(Estimate = round(positive_agreement, digit =3),  LowerCI = round(positive_agreement_low, digit =3), UpperCI = round(positive_agreement_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Positive Agreement")
    }
    if (negative_agreement >= 0 && negative_agreement <= 1)
    {
      newRow <- data.frame(Estimate = round(negative_agreement, digit =3),  LowerCI = round(negative_agreement_low, digit =3), UpperCI = round(negative_agreement_upp, digit =3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Negative Agreement")
    }
    if (byrt_bias_index >= 0 && byrt_bias_index <= 1)
    {
      newRow <- data.frame(Estimate = round(byrt_bias_index, digit =3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Byrt\'s Bias Index")
    }
    if (byrt_prevalence_asymmetry_index >= 0 && byrt_prevalence_asymmetry_index <= 1)
    {
      newRow <- data.frame(Estimate = round(byrt_prevalence_asymmetry_index, digit =3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Byrt\'s Prevalence Asymmetry Index")
    }
    if (bias_adjusted_kappa >= 0 && bias_adjusted_kappa <= 1)
    {
      newRow <- data.frame(Estimate = round(bias_adjusted_kappa, digit =3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Bias Adjusted Kappa")
    }
    if (prevalence_bias_adjusted_kappa >= 0 && prevalence_bias_adjusted_kappa <= 1)
    {
      newRow <- data.frame(Estimate = round(prevalence_bias_adjusted_kappa, digit =3),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Prevalence & Bias Adjusted Kappa")
    }
    
    rownames(verim) =  row
    
    highchart() %>% hc_exporting(enabled = TRUE, filename = "Plot2") %>% 
      hc_add_series(name = "Conf. Interval", type = "bar", data = verim$`Estimate`) %>%
      hc_add_series(name = "CI", data = as.matrix(cbind(verim$`LowerCI`, verim$`UpperCI`)),type = "errorbar", names = "Limits", showInLegend = FALSE, zIndex = 0, lineWidth = 1.5, linkedTo = "CI"
      ) %>%
      hc_xAxis(categories = rownames(verim),title = "Agreement:") %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, headerFormat = "<b>Agreement: </b>{point.x} <br>") %>%
      hc_plotOptions(line = list(tooltip = list(pointFormat = "<b>{series.name}: </b>{point.y:.3f} ")), 
                     errorbar = list(tooltip = list(pointFormat = "({point.low} - {point.high})"))) %>%
      hc_add_theme(hc_theme_google())
    
    
  })
  
  dataM3 <- reactive({   
    if (is.null(trimws(input$text3)) && is.null(trimws(input$text4)))  {return(NULL)}
    else {
      if (input$verigir_kontrol == TRUE){ 
        TP = 0
        FN = 0
        FP = 0
        TN = 0
        for (i in 1:nrow(tabloveri()))
        {
          if (tabloveri()[i,1]==1 & tabloveri()[i,2]==1)
          {TP = TP+1}
          else if (tabloveri()[i,1]==1 & tabloveri()[i,2]==2)
          {FN = FN+1}
          else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==1)
          {FP = FP+1}
          else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==2)
          {TN = TN+1}
          next
        }
        CI = as.numeric(trimws(input$text5_2))
      }
      else{
        TP = as.numeric(trimws(input$text1)) ### x
        FN = as.numeric(trimws(input$text2)) ### y
        FP = as.numeric(trimws(input$text3)) ### z
        TN = as.numeric(trimws(input$text4)) ### t
        CI = as.numeric(trimws(input$text5))
      }
      
      
      satirtop1 = TP + FN
      satirtop2 = FP + TN
      sutuntop1 = TP + FP
      sutuntop2 = FN + TN
      toplam = satirtop1 + satirtop2
      #table1 <- matrix(c(TP,FN,satirtop1,FP,TN,satirtop2,sutuntop1,sutuntop2,toplam),nrow=3,ncol=3,byrow=TRUE,dimnames=list(c("1.satir","2.satir","Sutun toplamlari"),c("1.kolon","2.kolon","Satir toplamlari")))
      #table1
      ## Duyarlilik 
      sensitivity_estimate = TP / satirtop1
      sensitivity_se = round((sqrt(sensitivity_estimate * (1 - sensitivity_estimate)/satirtop1)), 4)
      sensitivity_low = 1 - Rbeta.inv(((1 + CI/100)/2), (satirtop1+1-TP), TP)
      sensitivity_upp = Rbeta.inv(((1 + CI/100)/2), (TP+1), (satirtop1-TP))
      sensitivity_randomtest = sutuntop1 / toplam
      qualityindex_sensitivity = (sensitivity_estimate - sensitivity_randomtest) / (1 - sensitivity_randomtest)
      ## Secicilik
      specificity = TN / satirtop2
      specificity_se = sqrt(specificity * (1 - specificity) / satirtop2)
      specificity_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop2+1-TN),TN)
      specificity_upp = Rbeta.inv((1 + CI/100)/2, (TN+1), (satirtop2-TN))
      specificity_randomtest = (1 - sensitivity_randomtest)
      qualityindex_specificity = (specificity - 1 + sensitivity_randomtest) / sensitivity_randomtest
      ## Gain in Certainty
      gain_in_certainty = sensitivity_estimate + specificity
      ## Etkililik, yeterlilik ayni zamanda overall accuracy (toplam-genel dogruluk orani)
      efficiency = (TP + TN) / toplam
      efficiency_se = sqrt(efficiency * (1 - efficiency) / toplam)
      efficiency_low = 1-Rbeta.inv((1 + CI/100)/2, (toplam+1-TP-TN), (TP+TN))
      efficiency_upp = Rbeta.inv((1 + CI/100)/2, (TP+TN+1), (toplam-TP-TN))
      efficiency_randomtest = (satirtop1 / toplam) * sensitivity_randomtest + (1 - (satirtop1 / toplam)) * (1 - sensitivity_randomtest)
      mis_efficiency = 1 - efficiency
      ## Quality indeks
      quality_index = (efficiency - efficiency_randomtest) / (1 - efficiency_randomtest)
      quality_index_se = sqrt((efficiency*(1-efficiency)/(1-efficiency_randomtest)^2+2*(1-efficiency)*(2*efficiency*efficiency_randomtest-((TP*((satirtop1 / toplam)+sensitivity_randomtest)+TN*(2-(satirtop1 / toplam)-sensitivity_randomtest))/toplam))/(1-efficiency_randomtest)^3+((1-efficiency)^2)*((TP*((satirtop1 / toplam)+sensitivity_randomtest)^2+FN*(-(satirtop1 / toplam)+1+sensitivity_randomtest)^2+FP*(1+(satirtop1 / toplam)-sensitivity_randomtest)^2+TN*(2-(satirtop1 / toplam)-sensitivity_randomtest)^2)/toplam-4*efficiency_randomtest^2)/(1-efficiency_randomtest)^4)/toplam)
      quality_index_low = quality_index + qnorm((1 - CI/100)/2)*quality_index_se
      quality_index_upp = quality_index - qnorm((1 - CI/100)/2)*quality_index_se
      ## Youden indeksi
      youdens_index = sensitivity_estimate + specificity - 1
      youdens_index_se = sqrt(sensitivity_se^2+specificity_se^2)
      youdens_index_low = youdens_index + qnorm((1 - CI/100)/2)*youdens_index_se
      youdens_index_upp = youdens_index - qnorm((1 - CI/100)/2)*youdens_index_se
      
      number_needed_to_diagnose = 1 / youdens_index
      number_needed_to_misdiagnose = 1 / ( 1 - efficiency ) 
      ## Pozitif kestirim degeri
      predictivevalue_positivetest = TP / sutuntop1
      predictivevalue_positivetest_se = sqrt(predictivevalue_positivetest*(1-predictivevalue_positivetest)/sutuntop1)
      predictivevalue_positivetest_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop1+1-TP),TP)
      predictivevalue_positivetest_upp = Rbeta.inv((1 + CI/100)/2, (TP+1),(sutuntop1-TP))
      predvalue_positiverandomtest = (satirtop1 / toplam)
      predvalue_positiverandomtest_mar_ol = predvalue_positiverandomtest*sensitivity_estimate+(1-predvalue_positiverandomtest)*(1-specificity)
      ## Negatif kestirim degeri
      predictivevalue_negativetest = TN / sutuntop2
      predictivevalue_negativetest_se = sqrt(predictivevalue_negativetest*(1-predictivevalue_negativetest)/sutuntop2)
      predictivevalue_negativetest_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop2+1-TN),TN)
      predictivevalue_negativetest_upp = Rbeta.inv((1 + CI/100)/2, (TN+1), (sutuntop2-TN))
      predvalue_negativerandomtest = 1 - (satirtop1 / toplam)
      predvalue_negativerandomtest_mar_ol = 1 - predvalue_positiverandomtest_mar_ol
      ## Predictive Summary Index (PSI)
      predictive_summary_index = predictivevalue_positivetest + predictivevalue_negativetest - 1
      ## Yanlis pozitif orani
      false_positiverate = FP / satirtop2
      false_positiverate_se = sqrt(false_positiverate*(1-false_positiverate)/satirtop2)
      false_positiverate_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop2+1-FP),FP)
      false_positiverate_upp = Rbeta.inv((1 + CI/100)/2, (FP+1), (satirtop2-FP))
      ## Yansis negatif orani
      false_negativerate = FN / satirtop1
      false_negativerate_se = sqrt(false_negativerate*(1-false_negativerate)/satirtop1)
      false_negativerate_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop1+1-FN),FN)
      false_negativerate_upp = Rbeta.inv((1 + CI/100)/2, (FN+1), (satirtop1-FN))
      ## False Omission Rate (FOR) https://www.ncss.com/software/ncss/diagnostic-tests-in-ncss/
      false_omission_rate = FN/(sutuntop2)
      false_omission_rate_se = sqrt(false_omission_rate*(1-false_omission_rate)/sutuntop2)
      false_omission_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop2+1-FN),FN)
      false_omission_rate_upp = Rbeta.inv((1 + CI/100)/2, (FN+1), (sutuntop2-FN))
      ## False Discovery Rate (FDR) https://www.ncss.com/software/ncss/diagnostic-tests-in-ncss/
      false_discovery_rate = FP/(sutuntop1)
      false_discovery_rate_se = sqrt(false_discovery_rate*(1-false_discovery_rate)/sutuntop1)
      false_discovery_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop1+1-FP),FP)
      false_discovery_rate_upp = Rbeta.inv((1 + CI/100)/2, (FP+1), (sutuntop1-FP))
      ## Dogru pozitif orani
      true_positiverate = TP / (satirtop1)
      ## Yanlis siniflandirma orani
      misclassification_rate = (FN + FP) / toplam
      misclassification_rate_se = sqrt(misclassification_rate*(1-misclassification_rate)/toplam)
      misclassification_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-FP-FN), (FP+FN))
      misclassification_rate_upp = Rbeta.inv((1 + CI/100)/2, (FP+FN+1), (toplam-FP-FN))
      ## Prevelans
      prevalence = (satirtop1 / toplam)
      prevalence_se = sqrt((satirtop1 / toplam)*(1-(satirtop1 / toplam))/toplam)
      prevalence_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-satirtop1), satirtop1)
      prevalence_upp = Rbeta.inv((1 + CI/100)/2, (satirtop1+1), (toplam-satirtop1))
      
      ##caret paketinden olculer
      detection_rate = TP/toplam
      detection_prevalence = (TP+FP)/toplam
      balanced_accuracy = (sensitivity_estimate + specificity)/2
      lift = (TP/(TP+FN))/((TP+FP)/toplam)  ## https://gerardnico.com/wiki/data_mining/error_rate
      precision = TP/(TP+FP)
      recall = TP/(TP+FN)
      ## http://michael.hahsler.net/research/recomm_lnai2002/lnai2002.pdf
      conviction = (1-((TP+FP)/toplam))/(1-(TP/(TP+FN)))
      matthews_correlation_coefficient = (TP * TN + FN * FP) / ((TP+FP)*(FN+TN)*(FP+TN)*(TP+FN))^(1/2)
      ## Goreli Risk Orani (Relative risk)
      ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
      # https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20ratio.htm
      relative_risk = (TP/sutuntop1)/(FN/sutuntop2)
      relative_risk_se = sqrt((1/TP)+(1/FN)-(1/(TP+FP))-(1/(FN+TN)))
      relative_risk_low = exp(log(relative_risk)+qnorm((1-CI/100)/2)*relative_risk_se)
      relative_risk_upp = exp(log(relative_risk)-qnorm((1-CI/100)/2)*relative_risk_se)
      difference_in_proportion = (TP/sutuntop1) - (FN/sutuntop2)
      ## Leverage
      ## https://www3.nd.edu/~tweninge/pubs/AHBCW_ANNIE08.pdf
      leverage = (TP/sutuntop1)-((TP+FP)/toplam)
      ## http://www.ebm.med.ualberta.ca/TherapyCalc.html
      control_event_rate = FN/sutuntop2
      experimental_event_rate = TP/sutuntop1
      ## Absolute risk reduction
      absolute_risk_reduction = -difference_in_proportion
      absolute_risk_reduction_se = sqrt((TP/sutuntop1)*(1-(TP/sutuntop1))/sutuntop1+(FN/sutuntop2)*(1-(FN/sutuntop2))/sutuntop2)
      absolute_risk_reduction_low = absolute_risk_reduction - qnorm((1-CI/100)/2)*absolute_risk_reduction_se
      absolute_risk_reduction_upp = absolute_risk_reduction + qnorm((1-CI/100)/2)*absolute_risk_reduction_se
      ## Relative risk reduction
      relative_risk_reduction = absolute_risk_reduction / (FN/sutuntop2)
      relative_risk_reduction_low = 1 - exp(log(relative_risk)-qnorm((1-CI/100)/2)*relative_risk_se)
      relative_risk_reduction_upp = 1 - exp(log(relative_risk)+qnorm((1-CI/100)/2)*relative_risk_se)
      ##Number needed to treat
      number_needed_to_treat = 1 / abs(difference_in_proportion)
      number_needed_to_treat_low = 1 /absolute_risk_reduction_upp
      number_needed_to_treat_upp = 1 /absolute_risk_reduction_low
      ## Test duzeyi
      test_level = sensitivity_randomtest
      test_level_se = sqrt(test_level*(1-test_level)/toplam)
      test_level_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-sutuntop1),sutuntop1)
      test_level_upp = Rbeta.inv((1 + CI/100)/2, (sutuntop1+1), (toplam-sutuntop1))
      ## Pre-test odds
      pretest_odds = prevalence / (1 - prevalence)
      ## Pozitif Olabilirlik Orani
      likelihoodratio_positivetest = sensitivity_estimate/(1-specificity)
      likelihoodratio_positivetest_se = exp(sqrt((1-sensitivity_estimate)/(sensitivity_estimate*satirtop1) +specificity/((1-specificity)*satirtop2)))
      likelihoodratio_positivetest_low = exp(log(likelihoodratio_positivetest) + qnorm((1 - CI/100)/2)*log(likelihoodratio_positivetest_se))
      likelihoodratio_positivetest_upp = exp(log(likelihoodratio_positivetest) - qnorm((1 - CI/100)/2)*log(likelihoodratio_positivetest_se))
      ## post-test odds
      posttest_odds = pretest_odds * likelihoodratio_positivetest
      bayes_ppv = (sensitivity_estimate * prevalence)/((sensitivity_estimate * prevalence) + (1 - specificity) * (1 - prevalence))
      ## Post-test probability (positive test result)
      posttest_probability = (pretest_odds * likelihoodratio_positivetest) / (1 + (pretest_odds * likelihoodratio_positivetest))
      ## Negatif Olabilirlik Orani
      likelihoodratio_negativetest = (1-sensitivity_estimate)/specificity
      likelihoodratio_negativetest_se = exp(sqrt(sensitivity_estimate/((1-sensitivity_estimate)*satirtop1) +(1-specificity)/(specificity*satirtop2)))
      likelihoodratio_negativetest_low = exp(log(likelihoodratio_negativetest) + qnorm((1-CI/100)/2)*log(likelihoodratio_negativetest_se))
      likelihoodratio_negativetest_upp = exp(log(likelihoodratio_negativetest) - qnorm((1-CI/100)/2)*log(likelihoodratio_negativetest_se))
      bayes_npv = (sensitivity_estimate * (1 - prevalence))/((sensitivity_estimate * (1 - prevalence)) + (1 - specificity) * prevalence)
      ## Post-test probability (negative test result)
      posttest_probability_neg = (pretest_odds * likelihoodratio_negativetest) / (1 + (pretest_odds * likelihoodratio_negativetest))
      ## Ters negatif olabilirlik orani
      inverse_likelihoodratio_negativetest = specificity/(1-sensitivity_estimate)
      inverse_likelihoodratio_negativetest_se = exp(sqrt(sensitivity_estimate/((1-sensitivity_estimate)*satirtop1) +(1-specificity)/(specificity*satirtop2)))
      inverse_likelihoodratio_negativetest_low = exp(log(inverse_likelihoodratio_negativetest) + qnorm((1-CI/100)/2)*log(inverse_likelihoodratio_negativetest_se))
      inverse_likelihoodratio_negativetest_upp = exp(log(inverse_likelihoodratio_negativetest) - qnorm((1-CI/100)/2)*log(inverse_likelihoodratio_negativetest_se))
      ## Diagnostic Odds Ratio (Tani Odds Orani)
      odds_ratio = (TP/FN)/(FP/TN)
      odds_ratio_se = exp(sqrt(1/TP+1/FP+1/TN+1/FN))
      odds_ratio_low = exp(log(odds_ratio) + qnorm((1-CI/100)/2)*log(odds_ratio_se))
      odds_ratio_upp = exp(log(odds_ratio) - qnorm((1-CI/100)/2)*log(odds_ratio_se))
      odds_ratio_Haldane = ((TP+0.5)/(FN+0.5))/((FP+0.5)/(TN+0.5))
      odds_ratio_Haldane_se = exp(sqrt(1/(TP+0.5)+1/(FP+0.5)+1/(TN+0.5)+1/(FN+0.5)))
      odds_ratio_Haldane_low = exp(log(odds_ratio_Haldane) + qnorm((1-CI/100)/2)*log(odds_ratio_Haldane_se))
      odds_ratio_Haldane_upp = exp(log(odds_ratio_Haldane) - qnorm((1-CI/100)/2)*log(odds_ratio_Haldane_se))
      error_odds_ratio = (sensitivity_estimate/(1-sensitivity_estimate))/(specificity/(1-specificity))
      ## Rate ratio
      ## http://omarkasule-05.tripod.com/id52.html
      ## http://sphweb.bumc.bu.edu/otlt/mph-modules/ep/ep713_randomerror/ep713_randomerror4.html
      rate_ratio = (TP/sutuntop1)/(TP/sutuntop2)
      rate_ratio_low = exp(log(relative_risk)+qnorm((1-CI/100)/2)*sqrt(1/TP+1/FP))
      rate_ratio_upp = exp(log(relative_risk)-qnorm((1-CI/100)/2)*sqrt(1/TP+1/FP))
      ## Risk Difference
      ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
      risk_difference = (TP/sutuntop1) - (FN/sutuntop2)
      risk_difference_se = (((TP/sutuntop1)*(1-(TP/sutuntop1)))/sutuntop1)+(((FN/sutuntop2)*(1-(FN/sutuntop2)))/sutuntop2)
      risk_difference_low = risk_difference + qnorm((1-CI/100)/2)*sqrt(risk_difference_se)
      risk_difference_upp = risk_difference - qnorm((1-CI/100)/2)*sqrt(risk_difference_se)
      ## Nitelenebilen risk
      ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
      attributable_risk = (TP/sutuntop1)-(FN/sutuntop2)
      attributable_risk_se = sqrt((satirtop1/toplam)*(1-(satirtop1/toplam))*(1/sutuntop1+1/sutuntop2))
      attributable_risk_low = attributable_risk + qnorm((1-CI/100)/2)*attributable_risk_se
      attributable_risk_upp = attributable_risk - qnorm((1-CI/100)/2)*attributable_risk_se
      ## Attributable risk percent
      ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
      attributable_risk_percent = ((relative_risk-1)/relative_risk)*100
      attributable_risk_percent_low = attributable_risk_percent - (qnorm((1-CI/100)/2)*attributable_risk_se)/attributable_risk
      attributable_risk_percent_upp = attributable_risk_percent + (qnorm((1-CI/100)/2)*attributable_risk_se)/attributable_risk
      ## Population Attributable Risk
      ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
      population_attributable_risk = (satirtop1/toplam)-(FN/sutuntop2)
      ## Population Attributable Risk Percent
      population_attributable_risk_percent = (population_attributable_risk / (satirtop1/toplam))*100
      ## Kohen's Kappa
      cohens_kappa = (efficiency-efficiency_randomtest)/(1-efficiency_randomtest)
      cohens_kappa_se = quality_index_se
      cohens_kappa_low = cohens_kappa + qnorm((1-CI/100)/2)*cohens_kappa_se
      cohens_kappa_upp = cohens_kappa - qnorm((1-CI/100)/2)*cohens_kappa_se
      ## Gozlenen Karar
      observed_agreement = efficiency
      observed_agreement_se = efficiency_se
      observed_agreement_low = efficiency_low
      observed_agreement_upp = efficiency_upp
      ## Risk karari,expected agreement
      chance_agreement = efficiency_randomtest
      ## Pozitif karar
      positive_agreement = 2*TP /(satirtop1+sutuntop1)
      positive_agreement_se = sqrt(4*TP*(FP+FN)*(TP+FP+FN))/(sutuntop1+satirtop1)^2
      positive_agreement_low = positive_agreement + qnorm((1-CI/100)/2)*positive_agreement_se
      positive_agreement_upp = positive_agreement - qnorm((1-CI/100)/2)*positive_agreement_se
      ## Negatif karar
      negative_agreement = 2*TN/(satirtop2+sutuntop2)
      negative_agreement_se = sqrt(4*TN*(FP+FN)*(TN+FP+FN))/(sutuntop2+satirtop2)^2
      negative_agreement_low = negative_agreement + qnorm((1-CI/100)/2)*negative_agreement_se
      negative_agreement_upp = negative_agreement - qnorm((1-CI/100)/2)*negative_agreement_se
      ## Rand index
      ## http://www.cs.pomona.edu/classes/cs158/resources/158-10(EM).pdf
      rand_index = (TP+TN)/(TP+FN+FP+TN)
      ## e-measure
      ## http://michael.hahsler.net/research/recomm_lnai2002/lnai2002.pdf
      e_measure = 1/((((1-CI)/100)*(1-precision))+((1-((1-CI)/100))*(1/recall)))
      ## discriminant power 
      ## https://eva.fing.edu.uy/pluginfile.php/69453/mod_resource/content/1/7633-10048-1-PB.pdf
      discriminant_power = (sqrt(3)/pi)*(log (sensitivity_estimate / (1-sensitivity_estimate)) + log (specificity / (1-specificity)))
      ## F1 Score
      ## http://machinelearningmastery.com/classification-accuracy-is-not-enough-more-performance-measures-you-can-use/
      f1_score = 2*(((TP/(TP+FN))*(TP/(TP+FP)))/((TP/(TP+FN))+(TP/(TP+FP))))
      ## Byrt yanlilik indeksi
      byrt_bias_index = (FN-FP)/toplam
      ## Byrt  asimetrik indeks prevelansi
      byrt_prevalence_asymmetry_index = (TN-TP)/toplam
      ## Yanlilik duzeltmeli kappa
      bias_adjusted_kappa = (efficiency-0.25*((satirtop1+sutuntop1)^2+(satirtop2+sutuntop2)^2)/toplam^2)/(1-0.25*((satirtop1+sutuntop1)^2+(satirtop2+sutuntop2)^2)/toplam^2)
      ## Yanlilik duzeltmeli kappa prevelansi
      prevalence_bias_adjusted_kappa = cohens_kappa+(1-cohens_kappa)*(byrt_prevalence_asymmetry_index^2-byrt_bias_index^2)
      ## Dice'in indeksi (Czekanowski)
      dice_index = positive_agreement
      dice_index_se= positive_agreement_se
      dice_index_low = positive_agreement_low
      dice_index_upp = positive_agreement_upp
      ## Yule's Q
      yules_q = (odds_ratio - 1) / (odds_ratio + 1)
      yules_q_se = 0.5*(1-yules_q^2)*sqrt(1/TP+1/FP+1/TN+1/FN)
      yules_q_low = yules_q + qnorm((1-CI/100)/2)*yules_q_se
      yules_q_upp = yules_q - qnorm((1-CI/100)/2)*yules_q_se
      
      equitable_threatscore = (TP-(sutuntop1*satirtop1/toplam))/(TP+FP+FN-(sutuntop1*satirtop1/toplam))
      ## Phi
      phi = (TP*TN-FN*FP) / sqrt(satirtop1 * satirtop2 * sutuntop1 * sutuntop2)
      phi_se = sqrt((1-phi^2+(phi+0.5*phi^3)*((satirtop1-satirtop2)*(sutuntop1-sutuntop2)/sqrt(satirtop1*satirtop2*sutuntop1*sutuntop2))-0.75*phi^2*((satirtop1-satirtop2)^2/(satirtop1*satirtop2)+(sutuntop1-sutuntop2)^2/(sutuntop1*sutuntop2)))/toplam)
      phi_low = phi + qnorm((1-CI/100)/2)*phi_se
      phi_upp = phi - qnorm((1-CI/100)/2)*phi_se
      ## Cramer V katsayisi
      cramer_v = sqrt((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2))/toplam)
      ## Olaganlik katsayisi
      contingency_coefficient = sqrt((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2))/(((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)))+toplam))
      ## Goodman and Kruskal Gamma 
      goodman_and_kruskal_gamma = (TP*TN-FN*FP)/(TP*TN+FN*FP)
      goodman_and_kruskal_gamma_se = 0.5*(1-yules_q^2)*sqrt(1/TP+1/FP+1/TN+1/FN)
      goodman_and_kruskal_gamma_low = yules_q + qnorm((1-CI/100)/2)*yules_q_se
      goodman_and_kruskal_gamma_upp = yules_q - qnorm((1-CI/100)/2)*yules_q_se
      ## Kendall's tau a
      kendalls_tau_a = (TP*TN-FN*FP)/(toplam*(toplam-1)/2)
      ## Kendall's tau b
      ## https://v8doc.sas.com/sashtml/stat/chap28/sect20.htm
      ## http://slideplayer.com/slide/10766029/
      kendalls_tau_b = (TP*TN-FN*FP)/sqrt((TP*TN+FN*FP+(TP*FN+FP*TN))*(TP*TN+FN*FP+(TP*FP+FN*TN)))
      ## Kendall's tau c
      kendalls_tau_c = ((2*2)*(TP*TN-FN*FP)) / (toplam^2 * (2-1))
      ## Somers'd R|C
      somers_d = (TP*TN-FN*FP)/(TP*TN+FN*FP+(TP*FN+FP*TN))
      ## Somers'd C|R
      somers_d_2 = (TP*TN-FP*FN)/(TP*TN+FP*FN+(TP*FP+FN*TN))
      ## Scoot'un karar indeksi
      scotts_agreement_index = bias_adjusted_kappa
      ## Dort-duzeyli Korelasyon
      tetrachoric_correlation = cos(pi/(1+sqrt(TP*TN/FN/FP)))
      tetrachoric_correlation_se = 0.375*(1-tetrachoric_correlation^2)*sqrt((1/(TP))+(1/(FP))+(1/(TN))+(1/(FN)))
      tetrachoric_correlation_low = tetrachoric_correlation + qnorm((1-CI/100)/2)*tetrachoric_correlation_se
      tetrachoric_correlation_upp = tetrachoric_correlation - qnorm((1-CI/100)/2)*tetrachoric_correlation_se
      ## Goodman kruskal tau katsayisi
      goodman_kruskal_tau = (toplam*((TP^2+FP^2)/sutuntop1+(FN^2+TN^2)/sutuntop2)-satirtop1^2-satirtop2^2)/(toplam^2-satirtop1^2-satirtop2^2)
      goodman_kruskal_tau_se = toplam*sqrt(TP*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop1)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*TP/satirtop1-(TP^2+FN^2)/satirtop1^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+FN*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop2)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*FN/satirtop1-(TP^2+FN^2)/satirtop1^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+FP*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop1)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*FP/satirtop2-(FP^2+TN^2)/satirtop2^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+TN*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop2)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*TN/satirtop2-(FP^2+TN^2)/satirtop2^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2)/(toplam^2-sutuntop1^2-sutuntop2^2)^2
      goodman_kruskal_tau_low = goodman_kruskal_tau + qnorm((1-CI/100)/2)*goodman_kruskal_tau_se
      goodman_kruskal_tau_upp = goodman_kruskal_tau - qnorm((1-CI/100)/2)*goodman_kruskal_tau_se
      ## Simetrik Lambda 
      lambda_symmetric = ((max(TP,FN)+max(FP,TN)+max(TP,FP)+max(FN,TN)-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2))/(2*toplam-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2)))
      if (lambda_symmetric==0 || lambda_symmetric==1)
      {lambda_symmetric_se = 0}
      else
      {
        lambda_symmetric_se = sqrt((TP*((if(TP>FN) 1 else 0) + (if(TP>FP) 1 else 0) + ((if(sutuntop1>sutuntop2) 1 else 0)+ (if(satirtop1>satirtop2) 1 else 0))*(lambda_symmetric-1))^2+FN*( (if(TP>FN) 0 else 1)+ (if(FN>TN) 1 else 0) +( (if(sutuntop1>sutuntop2) 0 else 1) + (if(satirtop1>satirtop2) 1 else 0))*(lambda_symmetric-1))^2+FP*( (if(FP>TN) 1 else 0) + (if(TP>FP) 0 else 1) +( (if(sutuntop1>sutuntop2) 1 else 0) + (if(satirtop1<satirtop2) 1 else 0))*(lambda_symmetric-1))^2+TN*((if(FP>TN) 0 else 1)+ (if(FN>TN) 0 else 1) +((if(sutuntop1>sutuntop2) 0 else 1)+ (if(satirtop1<satirtop2) 1 else 0))*(lambda_symmetric-1))^2-4*toplam*lambda_symmetric^2)/(2*toplam-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2))^2)
      }
      
      lambda_symmetric_low = lambda_symmetric + qnorm((1-CI/100)/2)*lambda_symmetric_se
      lambda_symmetric_upp = lambda_symmetric - qnorm((1-CI/100)/2)*lambda_symmetric_se
      ## Lambda Asymmetric R|C
      lambda_criterion = (max(TP,FP)+max(FN,TN)-max(satirtop1,satirtop2))/(toplam-max(satirtop1,satirtop2))
      
      if (lambda_criterion==0 || lambda_criterion==1)
      {lambda_criterion_se = 0}
      else
      {
        lambda_criterion_se = sqrt((toplam-max(TP,FP)-max(FN,TN))*(max(TP,FP)+max(FN,TN)+max(satirtop1,satirtop2)-2*(if(satirtop1>satirtop2) max(TP,FN) else max(FP,TN)))/(toplam-max(satirtop1,satirtop2))^3)
      }
      
      lambda_criterion_low = lambda_criterion + qnorm((1-CI/100)/2)*lambda_criterion_se
      lambda_criterion_upp = lambda_criterion - qnorm((1-CI/100)/2)*lambda_criterion_se
      ## Lambda Asymmetric C|R
      lambda_criterion_2 = (max(TP,FN)+max(FP,TN)-max(sutuntop1,sutuntop2))/(toplam-max(sutuntop1,sutuntop2))
      
      if (lambda_criterion_2==0 || lambda_criterion_2==1)
      {lambda_criterion_se_2 = 0}
      else
      {
        lambda_criterion_se_2 = sqrt((toplam-max(TP,FN)-max(FP,TN))*(max(TP,FN)+max(FP,TN)+max(sutuntop1,sutuntop2)-2*(if(sutuntop1>sutuntop2) max(TP,FP) else max(FN,TN)))/(toplam-max(sutuntop1,sutuntop2))^3)
      }
      
      lambda_criterion_low_2 = lambda_criterion_2 + qnorm((1-CI/100)/2)*lambda_criterion_se_2
      lambda_criterion_upp_2 = lambda_criterion_2 - qnorm((1-CI/100)/2)*lambda_criterion_se_2
      ## belirsizlik katsayisi (coefficient of uncertainty) simetrik
      uncertainty_coefficient_symmetric = 2*(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam)))/(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))
      uncertainty_coefficient_symmetric_se = 2*sqrt(TP*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop1*sutuntop1/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(TP/toplam))^2+FN*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop1*sutuntop2/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(FN/toplam))^2+FP*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop2*sutuntop1/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(FP/toplam))^2+TN*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop2*sutuntop2/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(TN/toplam))^2)/toplam/(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam)^2
      uncertainty_coefficient_symmetric_low = uncertainty_coefficient_symmetric + qnorm((1-CI/100)/2)*uncertainty_coefficient_symmetric_se
      uncertainty_coefficient_symmetric_upp = uncertainty_coefficient_symmetric - qnorm((1-CI/100)/2)*uncertainty_coefficient_symmetric_se
      ## (coefficient of uncertainty) R|C
      uncertainty_coeff_crit = (satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam)))/(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))
      uncertainty_coeff_crit_se = sqrt(TP*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(TP/sutuntop1)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop1/toplam))^2+FN*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(FN/sutuntop2)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop1/toplam))^2+FP*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(FP/sutuntop1)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop2/toplam))^2+TN*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(TN/sutuntop2)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop2/toplam))^2)/toplam/((satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam)^2
      uncertainty_coeff_crit_low = uncertainty_coeff_crit + qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se
      uncertainty_coeff_crit_upp = uncertainty_coeff_crit - qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se
      ## (coefficient of uncertainty) C|R
      uncertainty_coeff_crit_2 = (sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)+satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)-(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam)))/(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))
      uncertainty_coeff_crit_se_2 = sqrt(TP*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(TP/satirtop1)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop1/toplam))^2+FP*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(FP/satirtop2)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop1/toplam))^2+FN*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(FN/satirtop1)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop2/toplam))^2+TN*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(TN/satirtop2)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop2/toplam))^2)/toplam/((sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)^2
      uncertainty_coeff_crit_low_2 = uncertainty_coeff_crit_2 + qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se_2
      uncertainty_coeff_crit_upp_2 = uncertainty_coeff_crit_2 - qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se_2
      ## Yule s Y (Coefficient of colligation)
      yule_y = (sqrt(TP*TN)-sqrt(FN*FP))/(sqrt(TP*TN)+sqrt(FN*FP))
      ## Pearson ki-kare
      pearson_chi_squ = toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)
      pearson_chi_squ_p = dchi(pearson_chi_squ, df=1)
      ## 
      with_yate_cor_for_pearson = toplam*(abs(TP*TN-FN*FP)-0.5*toplam)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)
      with_yate_cor_p_for_pearson = dchi(with_yate_cor_for_pearson, df=1)
      ## Mantel Haenszel chi-square
      ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
      mantel_haenszel = ((toplam-1)*(TP*TN-FN*FP)^2)/(satirtop1*satirtop2*sutuntop1*sutuntop2)
      mantel_haenszel_p = dchi(mantel_haenszel, df=1)
      ## Olasilik orani
      likelih_ratio = 2*(TP*log(TP/(sutuntop1*satirtop1/toplam))+FN*log(FN/(sutuntop2*satirtop1/toplam))+FP*log(FP/(sutuntop1*satirtop2/toplam))+TN*log(TN/(sutuntop2*satirtop2/toplam)))
      likelih_ratio_p =dchi(likelih_ratio, df=1)
      ## Fisher'in exact testi
      ##fisher_exact_test = (TP-(satirtop1*sutuntop1/toplam))^2/(satirtop1*sutuntop1/toplam) + (FN-(satirtop1*sutuntop2/toplam))^2/(satirtop1*sutuntop2/toplam) + (FP-(satirtop2*sutuntop1/toplam))^2/(satirtop2*sutuntop1/toplam) + (TN-(satirtop2*sutuntop2/toplam))^2/(satirtop2*sutuntop2/toplam)
      fisher_test = factorial(TP+FN)*factorial(FP+TN)*factorial(TP+FP)*factorial(FN*TN)/factorial(toplam)*factorial(TP)*factorial(TN)*factorial(FP)*factorial(FN)
      fisher <- c(TP, FN, FP, TN)
      tab <- t(matrix(fisher, nrow=2,ncol=2))
      fisher_exact_test = fisher.test(tab)$p.value
      minimum_exp_fre = min((sutuntop1*satirtop1/toplam):(sutuntop2*satirtop2/toplam))
      cells_exp_fre_5 = (if((sutuntop1*satirtop1/toplam)<5) 1 else 0)+ (if((sutuntop1*satirtop2/toplam)<5) 1 else 0)+ (if((sutuntop2*satirtop1/toplam) <5) 1 else 0)+ (if((sutuntop2*satirtop2/toplam) <5) 1 else 0)
      cells_exp_fre_1 = (if((sutuntop1*satirtop1/toplam) <1) 1 else 0)+ (if((sutuntop1*satirtop2/toplam) <1) 1 else 0)+ (if((sutuntop2*satirtop1/toplam) <1) 1 else 0)+ (if((sutuntop2*satirtop2/toplam) <1) 1 else 0)
      ## Mc Nemar testi
      mcNemar_test = (FP-FN)^2/(FP+FN)
      mcNemar_test_p = dchi(mcNemar_test, df=1)
      with_Yate_cor_for_mcnamer = (abs(FP-FN)-1)^2/(FP+FN)
      with_Yate_cor_p_for_mcnamer = dchi(with_Yate_cor_for_mcnamer, df=1)
      
      ## Belirsizlik (Entropi)
      forbes_NMI = sensitivity_estimate*predvalue_positiverandomtest*log((sensitivity_estimate/predvalue_positiverandomtest_mar_ol),base=2)+
        (1-specificity)*(1-predvalue_positiverandomtest)*log(((1-specificity)/predvalue_positiverandomtest_mar_ol),base=2)+
        (1-sensitivity_estimate)*predvalue_positiverandomtest*log(((1-sensitivity_estimate)/(1-predvalue_positiverandomtest_mar_ol)),base=2)+
        specificity*(1-predvalue_positiverandomtest)*log((specificity/(1-predvalue_positiverandomtest_mar_ol)),base=2)
      ## satir icin entropy (test icin)
      entropy_hr = - ((sutuntop1/toplam) * log(sutuntop1/toplam, base=2) + (sutuntop2/toplam) * log(sutuntop2/toplam, base=2))
      ## sutun icin entropy (hastalik icin)  
      entropy_hc = - ((satirtop1/toplam)*log(satirtop1/toplam, base=2) + (satirtop2/toplam)*log(satirtop2/toplam, base=2))
      ## birlesik entropi (joint entropy)  
      entropy_hrc = - ( (TP/toplam)*log(TP/toplam, base=2) + (FP/toplam)*log(FP/toplam, base=2) + (FN/toplam)*log(FN/toplam, base=2) + (TN/toplam)*log(TN/toplam, base=2))
      ## bilgi icerigi (mutual information)  
      information_r_c = entropy_hr + entropy_hc - entropy_hrc
      ## kosullu entropi (conditional entropy)  
      a = entropy_hrc - entropy_hr
      c = entropy_hrc - entropy_hc
      sim_r_c = information_r_c / (a + information_r_c + c)
      dis_r_c = (a + c) / (a + information_r_c + c)
      ## goreli entropi (relative entropy, kullback-leibler uzakligi)
      ## pozitif test sonucu icin goreli entropi, Relative Improvement Over Chance (RIOC)
      positive_relative_entropy = (TP/sutuntop1)*log(((TP/sutuntop1)/ prevalence), base=2)+(FP/sutuntop1)*log(((FP/sutuntop1)/(1- prevalence)), base=2)
      ## negatif test sonucu icin goreli entropi 
      negative_relative_entropy = (FN/sutuntop2)*log(((FN/sutuntop2)/ prevalence), base=2)+(TN/sutuntop2)*log(((TN/sutuntop2)/(1- prevalence)), base=2)
      
      
      verim <- data.frame(Chi_Square = format(round(pearson_chi_squ, digit=3),nsmall=3), P = format(round(pearson_chi_squ_p, digit=3),nsmall=3))
      newRow <- data.frame(Chi_Square = format(round(with_yate_cor_for_pearson, digit=3),nsmall=3), P = format(round(with_yate_cor_p_for_pearson, digit=3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Chi_Square = format(round(mantel_haenszel, digit=3),nsmall=3), P = format(round(mantel_haenszel_p, digit=3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Chi_Square = format(round(likelih_ratio, digit=3),nsmall=3), P = format(round(likelih_ratio_p, digit=3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Chi_Square = format(round(fisher_test, digit=3),nsmall=3), P = format(round(fisher_exact_test, digit=3),nsmall=3))
      verim <- rbind(verim,newRow)   
      newRow <- data.frame(Chi_Square = format(round(minimum_exp_fre, digit=3),nsmall=3), P = NA)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Chi_Square = NA, P = format(round(cells_exp_fre_5, digit=3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Chi_Square = NA, P = format(round(cells_exp_fre_1, digit=3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Chi_Square = format(round(mcNemar_test, digit=3),nsmall=3), P = format(round(mcNemar_test_p, digit=3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Chi_Square = format(round(with_Yate_cor_for_mcnamer, digit=3),nsmall=3), P = format(round(with_Yate_cor_p_for_mcnamer, digit=3),nsmall=3))
      verim <- rbind(verim,newRow)
      
      rownames(verim) = c("Pearson Chi-Square Analysis", "Pearson Chi-Square Analysis with Yate\'s correction (Continuity Correction)","Mantel Haenszel Chi-Square Test", "Likelihood Ratio Chi Square",
      "Fisher Exact Test", "Minimum Expected Frequency", "Cells with Expected Frequency less than 5", "Cells with Expected Frequency less than 1",
       "McNemar\'s Test", "McNemar\'s Test with Yate\'s correction")
      
      return(verim)
      
      
    }
  })
  
  my_data3 <- reactive({
    testdata <- dataM3()
    as.data.frame(cbind(Interpretation = shinyInput(actionButton, nrow(testdata),'button3_', label = "Interpretation", onclick = 'Shiny.onInputChange(\"select_button3\",  this.id)' ),testdata))
  })  
  output$dataUpload3 <- DT::renderDataTable(my_data3(),selection = 'single', extensions = c('Buttons','KeyTable', 'Responsive'),
                                            options = list(pageLength = 25,lengthMenu = c(25, 50, 75, 100),
                                                           dom = 'Bfrtip',
                                                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),keys = TRUE,
                                                           preDrawCallback = JS("function() { 
                                                                                Shiny.unbindAll(this.api().table().node()); }"), 
                                                           drawCallback = JS("function() { 
                                                                             Shiny.bindAll(this.api().table().node()); } ")),
                                            server = FALSE, escape = FALSE)
  
  
  # Here I created a reactive to save which row was clicked which can be stored for further analysis
  SelectedRow3 <- eventReactive(input$select_button3,{
    as.numeric(strsplit(input$select_button3, "_")[[1]][2])
  })
  
  # This is needed so that the button is clicked once for modal to show, a bug reported here
  # https://github.com/ebailey78/shinyBS/issues/57
  observeEvent(input$select_button3, {
    toggleModal(session, "modalExample3", "open")
  })
  
  DataRow3 <- eventReactive(input$select_button3,{
    my_data3()[SelectedRow3(),2:ncol(my_data3())]
  })
  
  output$popup3 <- renderUI({

    if (rownames(DataRow3()) == 'Pearson Chi-Square Analysis') 
    {
      if (as.numeric(DataRow3()$P) < 0.05)
      {
        sonuc = 'The hypothesis for p<0.05 is rejected. There is a link between the diagnostic test and the actual situation.'
      }
      else
      {
        sonuc = 'The hypothesis is accepted because p>=0.05. There is no link between the diagnostic test and the actual situation.'
      }
      bsModal("modalExample3", paste0("Pearson Chi-Square Analysis"), "", size = "large",
              h4("Description:"),
              print(paste0("Pearson ki-square statistic is used if the frequencies in the eyes are above 25 and 25. 
                           This formula is the basic formula of square-statistic. ", sonuc)),
              
              h4("Interpretation:"),
              h5(" "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow3()),
              column(12,                   
                     DT::renderDataTable(DataRow3())
                     
              )
      )}

    else if (rownames(DataRow3()) == 'Pearson Chi-Square Analysis with Yate\'s correction (Continuity Correction)') 
    {
      if (as.numeric(DataRow3()$P) < 0.05)
      {
        sonuc = 'The hypothesis for p<0.05 is rejected. There is a link between the diagnostic test and the actual situation.'
      }
      else
      {
        sonuc = 'The hypothesis is accepted because p>=0.05. There is no link between the diagnostic test and the actual situation.'
      }
      bsModal("modalExample3", paste0("Pearson Chi-Square Analysis with Yate\'s Correction: "), "", size = "large",
              h4("Description:"),
              print(paste0("If the number of observations in any eye in the 2x2 tables is below 25, the Yates modified Chi-square test is used. 
                           In statistical software, it is usually given as 'corrected chi-square or continuity correction'. ", sonuc)),
              h4("Interpretation:"),
              h5(" "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow3()),
              column(12,                   
                     DT::renderDataTable(DataRow3())
                     
              )
       
      )}

    else if (rownames(DataRow3()) == 'Fisher Exact Test:') 
    {
      if (as.numeric(DataRow3()$P) < 0.05)
      {
        sonuc = 'The hypothesis for p<0.05 is rejected. There is a link between the diagnostic test and the actual situation.'
      }
      else
      {
        sonuc = 'The hypothesis is accepted because p>=0.05. There is no link between the diagnostic test and the actual situation.'
      }
      bsModal("modalExample3", paste0("Fisher Exact Test: "), "", size = "large",
              h4("Description:"),
              print(paste0("Fisher exact test is computed when a table that does not result from missing 
rows or columns in a larger table has a cell with an expected frequency of less than 5. 
                           In this test a direct p value is obtained. ", sonuc)),
              h4("Interpretation:"),
              h5(" "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow3()),
              column(12,                   
                     DT::renderDataTable(DataRow3())
                     
              )
     
              
      )}

    else if (rownames(DataRow3()) == 'McNemar\'s Test:') 
    {
      if (as.numeric(DataRow3()$P) < 0.05)
      {
        sonuc = 'The hypothesis for p<0.05 is rejected. There is a link between the diagnostic test and the actual situation.'
      }
      else
      {
        sonuc = 'The hypothesis is accepted because p>=0.05. There is no link between the diagnostic test and the actual situation.'
      }
      bsModal("modalExample3", paste0("McNemar\'s Test: "), "", size = "large",
              h4("Description:"),
              print(paste0("A nonparametric test for two related dichotomous variables. Tests for changes in responses using 
the chi-square distribution. Useful for detecting changes in responses due to experimental intervention in before-and-after designs. 
For larger square tables, the McNemar-Bowker test of symmetry is reported.The McNemar Test, which is one of the tests that can be applied in 
cases where the difference between the two percentages in the dependent samples is tested using the significance test, 
                           is the chi-square test in the dependent samples. ", sonuc)),
              h4("Interpretation:"),
              h5(" "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow3()),
              column(12,                   
                     DT::renderDataTable(DataRow3())
                     
              )
              
              
      )

      }
    else
    {
    bsModal("modalExample3", paste0("Interpretation: "), "", size = "large",
              print(paste0(rownames(DataRow3())," chi-Square is ", DataRow3()$Estimate," and p-value is ", DataRow3()$P)),
            h5("**************************************************************************************************************************************************"),
            h5("Data for Row Number:",SelectedRow3()),
            column(12,                   
                   DT::renderDataTable(DataRow3())
                   
            )
            
            
    )
    }
  })
  
  
 
  ## Distance Measures
  dataM4 <- reactive({   
    if (is.null(trimws(input$text3)) && is.null(trimws(input$text4)))  {return(NULL)}
    else {
      if (input$verigir_kontrol == TRUE){ 
        a = 0
        b = 0
        c = 0
        d = 0
        for (i in 1:nrow(tabloveri()))
        {
          if (tabloveri()[i,1]==1 & tabloveri()[i,2]==1)
          {a = a+1}
          else if (tabloveri()[i,1]==1 & tabloveri()[i,2]==2)
          {b = b+1}
          else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==1)
          {c = c+1}
          else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==2)
          {d = d+1}
          next
        }
        CI = as.numeric(trimws(input$text5_2))
      }
      else{
        a = as.numeric(trimws(input$text1)) ### x
        b = as.numeric(trimws(input$text2)) ### y
        c = as.numeric(trimws(input$text3)) ### z
        d = as.numeric(trimws(input$text4)) ### t
        CI = as.numeric(trimws(input$text5))
      }

      
      satirtop1 = a + b
      satirtop2 = c + d
      sutuntop1 = a + c
      sutuntop2 = b + d
      toplam = satirtop1 + satirtop2
      
      ###http://www.iiisci.org/journal/CV$/sci/pdfs/GS315JG.pdf 
      jaccard = a / (a+b+c)
      dice = (2*a) / (2*a+b+c)
      czekanowski = (2*a) / (2*a+b+c)
      w3_jaccard = (3*a) / (3*a+b+c)
      nei_li = (2*a) / ((a+b)+(a+c))
      sokal_sneath_1 = a / (a+2*b+2*c)
      sokal_michener = (a+d) / (a+b+c+d)
      sokal_sneath_2 = (2*(a+d)) / (2*a+b+c+2*d)
      roger_tanimoto = (a+d) / (a+2*(b+c)+d)
      faith = (a + 0.5*d) / (a+b+c+d)
      gower_legendre = (a+d) / (a+0.5*(b+c)+d)
      intersection = a
      innerproduct = a+d
      russell_rao = a / (a+b+c+d)
      d_hamming = b+c
      d_euclid = sqrt(b+c)
      d_squared_euclid = sqrt((b+c)^2)
      d_canberra = (b+c)^(2/2)
      d_manhattan = b+c
      d_mean_manhattan = (b+c)/(a+b+c+d)
      d_cityblock = b+c
      d_minkowski =(b+c)^(1/1)
      d_vari = (b+c) / (4*(a+b+c+d))
      d_sizedifference = (b+c)^2 / (a+b+c+d)^2
      d_shapedifference =(toplam*(b+c)-(b-c)^2) / (a+b+c+d)^2
      d_patterndifference =(4*b*c) / (a+b+c+d)^2
      d_lance_williams = (b+c) / (2*a+b+c)
      d_bray_curtis = (b+c) / (2*a+b+c)
      d_hellinger = 2*sqrt((1-(a/sqrt((a+b)*(a+c)))))
      d_chord = sqrt(2*(1-a/ sqrt((a+b)*(a+c))))
      cosine = a/sqrt((a+b)*(a+c))^2
      gilbert_wells = log(a)-log(toplam)-log((a+b)/toplam)-log((a+c)/toplam)
      ochiai_1 = a / sqrt((a+b)*(a+c))
      forbesi = (toplam*a) / ((a+b)*(a+c))
      fossum =(toplam*(a-0.5)^2) / ((a+b)*(a+c))
      sorgenfrei = a^2 / ((a+b)*(a+c))
      mountford = a / (0.5*(a*b+a*c)+b*c)
      otsuka = a / ((a+b)*(a+c))^0.5
      mcconnaughey =(a^2-b*c) / ((a+b)*(a+c))
      tarwid =(toplam*a-(a+b)*(a+c)) / (toplam*a+(a+b)*(a+c))
      kulczynski_2 =((a/2)*(2*a+b+c)) / ((a+b)*(a+c))
      driver_kroeber = (a/2)*(1/(a+b)+1/(a+c))
      johnson = a/(a+b) + a/(a+c)
      dennis = (a*d-b*c) / sqrt(toplam*(a+b)*(a+c))
      simpson = a / min(a+b,a+c)
      braun_banquet = a / max(a+b,a+c)
      fager_mcgowan = a / sqrt((a+b)*(a+c)) - max(a+b,a+c)/2
      forbes_2 =(toplam*a-(a+b)*(a+c)) / (toplam*min(a+b,a+c)-(a+b)*(a+c))
      sokal_sneath_4 = (a/(a+b) + a/(a+c) + d/(b+d) + d/(b+d)) /4
      gower = (a+d) / sqrt((a+b)*(a+c)*(b+d)*(c+d))
      ## pearson_1
      ## pearson_2
      ## pearson_3
      pearson_heron_1 = (a*d-b*c) / sqrt((a+b)*(a+c)*(b+d)*(c+d))
      pearson_heron_2 = cos((pi*sqrt(b*c)) / (sqrt(a*d)+sqrt(b*c)))
      sokal_sneath_3 = (a+d)/(b+c)
      sokal_sneath_5 = (a*d) / ((a+b)*(a+c)*(b+d)*(c+d)^0.5)
      cole = (sqrt(2)*(a*d-b*c)) / sqrt((a*d-b*c)^2-(a+b)*(a+c)*(b+d)*(c+d))
      stiles = log((toplam*(abs(a*d-b*c)-toplam/2)^2) / ((a+b)*(a+c)*(b+d)*(c+d)))
      ochiai_2 = (a*d) / sqrt((a+b)*(a+c)*(b+d)*(c+d))
      yuleq = (a*d-b*c) / (a*d+b*c)
      d_yuleq = (2*b*c) / (a*d+b*c)
      yulew = (sqrt(a*d)-sqrt(b*c)) / (sqrt(a*d)+sqrt(b*c))
      kulczynski_1 = a / (b+c)
      tanimoto = a / ((a+b)+(a+c)-a)
      disperson = (a*d-b*c) / (a+b+c+d)^2
      hamann = ((a+b)-(b+c)) / (a+b+c+d)
      michael = (4*(a*d-b*c)) / ((a+d)^2+(b+c)^2)
      goodman_kruskal = ((max(a,b)+max(c,d)+max(a,c)+max(b,d)) - (max(a+c,b+d)+max(a+b,c+d))) / (2*toplam - (max(a+c,b+d)+max(a+b,c+d)))
      anderberg = ((max(a,b)+max(c,d)+max(a,c)+max(b,d)) - (max(a+c,b+d)+max(a+b,c+d))) / (2*toplam)
      baroni_urbani_buser_1 = (sqrt(a*d)+a) / (sqrt(a*d)+a+b+c)
      baroni_urbani_buser_2 = (sqrt(a*d)+a-(b+c)) / (sqrt(a*d)+a+b+c)
      peirce = (a*b+b*c) / (a*b+2*b*c+c*d)
      eyraud = (toplam^2*(toplam*a-(a+b)*(a+c))) / ((a+b)*(a+c)*(b+d)*(c+d))
      tarantula = (a*(c+d)) / (c*(a+b))
      ample = abs((a*(c+d)) / (c*(a+b)))
      
     ## EstimateMeasure = percent(round(jaccard, digit=3))
      verim <- data.frame(Measure = format(round(jaccard, digit=3),nsmall=3),MeasurePercent = percent(round(jaccard, digit=3)))
      newRow <- data.frame(Measure = format(round(czekanowski, digit=3),nsmall=3),MeasurePercent = percent(round(czekanowski, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(w3_jaccard, digit=3),nsmall=3),MeasurePercent = percent(round(w3_jaccard, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(nei_li, digit=3),nsmall=3),MeasurePercent = percent(round(nei_li, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(sokal_sneath_1, digit=3),nsmall=3),MeasurePercent = percent(round(sokal_sneath_1, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(sokal_michener, digit=3),nsmall=3),MeasurePercent = percent(round(sokal_michener, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(sokal_sneath_2, digit=3),nsmall=3),MeasurePercent = percent(round(sokal_sneath_2, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(roger_tanimoto, digit=3),nsmall=3),MeasurePercent = percent(round(roger_tanimoto, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(faith, digit=3),nsmall=3),MeasurePercent = percent(round(faith, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(gower_legendre, digit=3),nsmall=3),MeasurePercent = percent(round(gower_legendre, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(intersection, digit=3),nsmall=3),MeasurePercent = percent(round(intersection, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(innerproduct, digit=3),nsmall=3),MeasurePercent = percent(round(innerproduct, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(russell_rao, digit=3),nsmall=3),MeasurePercent = percent(round(russell_rao, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(d_hamming, digit=3),nsmall=3),MeasurePercent = percent(round(d_hamming, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(d_euclid, digit=3),nsmall=3),MeasurePercent = percent(round(d_euclid, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(d_squared_euclid, digit=3),nsmall=3),MeasurePercent = percent(round(d_squared_euclid, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(d_canberra, digit=3),nsmall=3),MeasurePercent = percent(round(d_canberra, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(d_manhattan, digit=3),nsmall=3),MeasurePercent = percent(round(d_manhattan, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(d_mean_manhattan, digit=3),nsmall=3),MeasurePercent = percent(round(d_mean_manhattan, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(d_cityblock, digit=3),nsmall=3),MeasurePercent = percent(round(d_cityblock, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(d_minkowski, digit=3),nsmall=3),MeasurePercent = percent(round(d_minkowski, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(d_vari, digit=3),nsmall=3),MeasurePercent = percent(round(d_vari, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(d_sizedifference, digit=3),nsmall=3),MeasurePercent = percent(round(d_sizedifference, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(d_shapedifference, digit=3),nsmall=3),MeasurePercent = percent(round(d_shapedifference, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(d_patterndifference, digit=3),nsmall=3),MeasurePercent = percent(round(d_patterndifference, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(d_lance_williams, digit=3),nsmall=3),MeasurePercent = percent(round(d_lance_williams, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(d_bray_curtis, digit=3),nsmall=3),MeasurePercent = percent(round(d_bray_curtis, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(d_hellinger, digit=3),nsmall=3),MeasurePercent = percent(round(d_hellinger, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(d_chord, digit=3),nsmall=3),MeasurePercent = percent(round(d_chord, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(cosine, digit=3),nsmall=3),MeasurePercent = percent(round(cosine, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(gilbert_wells, digit=3),nsmall=3),MeasurePercent = percent(round(gilbert_wells, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(ochiai_1, digit=3),nsmall=3),MeasurePercent = percent(round(ochiai_1, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(forbesi, digit=3),nsmall=3),MeasurePercent = percent(round(forbesi, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(fossum, digit=3),nsmall=3),MeasurePercent = percent(round(fossum, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(sorgenfrei, digit=3),nsmall=3),MeasurePercent = percent(round(sorgenfrei, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(mountford, digit=3),nsmall=3),MeasurePercent = percent(round(mountford, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(otsuka, digit=3),nsmall=3),MeasurePercent = percent(round(otsuka, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(mcconnaughey, digit=3),nsmall=3),MeasurePercent = percent(round(mcconnaughey, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(tarwid, digit=3),nsmall=3),MeasurePercent = percent(round(tarwid, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(kulczynski_2, digit=3),nsmall=3),MeasurePercent = percent(round(kulczynski_2, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(driver_kroeber, digit=3),nsmall=3),MeasurePercent = percent(round(driver_kroeber, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(johnson, digit=3),nsmall=3),MeasurePercent = percent(round(johnson, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(dennis, digit=3),nsmall=3),MeasurePercent = percent(round(dennis, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(simpson, digit=3),nsmall=3),MeasurePercent = percent(round(simpson, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(braun_banquet, digit=3),nsmall=3),MeasurePercent = percent(round(braun_banquet, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(fager_mcgowan, digit=3),nsmall=3),MeasurePercent = percent(round(fager_mcgowan, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(forbes_2, digit=3),nsmall=3),MeasurePercent = percent(round(forbes_2, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(sokal_sneath_4, digit=3),nsmall=3),MeasurePercent = percent(round(sokal_sneath_4, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(gower, digit=3),nsmall=3),MeasurePercent = percent(round(gower, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(pearson_heron_1, digit=3),nsmall=3),MeasurePercent = percent(round(pearson_heron_1, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(pearson_heron_2, digit=3),nsmall=3),MeasurePercent = percent(round(pearson_heron_2, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(sokal_sneath_3, digit=3),nsmall=3),MeasurePercent = percent(round(sokal_sneath_3, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(sokal_sneath_5, digit=3),nsmall=3),MeasurePercent = percent(round(sokal_sneath_5, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(cole, digit=3),nsmall=3),MeasurePercent = percent(round(cole, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(stiles, digit=3),nsmall=3),MeasurePercent = percent(round(stiles, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(ochiai_2, digit=3),nsmall=3),MeasurePercent = percent(round(ochiai_2, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(yuleq, digit=3),nsmall=3),MeasurePercent = percent(round(yuleq, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(d_yuleq, digit=3),nsmall=3),MeasurePercent = percent(round(d_yuleq, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(yulew, digit=3),nsmall=3),MeasurePercent = percent(round(yulew, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(kulczynski_1, digit=3),nsmall=3),MeasurePercent = percent(round(kulczynski_1, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(tanimoto, digit=3),nsmall=3),MeasurePercent = percent(round(tanimoto, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(disperson, digit=3),nsmall=3),MeasurePercent = percent(round(disperson, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(hamann, digit=3),nsmall=3),MeasurePercent = percent(round(hamann, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(michael, digit=3),nsmall=3),MeasurePercent = percent(round(michael, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(goodman_kruskal, digit=3),nsmall=3),MeasurePercent = percent(round(goodman_kruskal, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(anderberg, digit=3),nsmall=3),MeasurePercent = percent(round(anderberg, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(baroni_urbani_buser_1, digit=3),nsmall=3),MeasurePercent = percent(round(baroni_urbani_buser_1, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(baroni_urbani_buser_2, digit=3),nsmall=3),MeasurePercent = percent(round(baroni_urbani_buser_2, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(peirce, digit=3),nsmall=3),MeasurePercent = percent(round(peirce, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(eyraud, digit=3),nsmall=3),MeasurePercent = percent(round(eyraud, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(tarantula, digit=3),nsmall=3),MeasurePercent = percent(round(tarantula, digit=3)))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Measure=format(round(ample, digit=3),nsmall=3),MeasurePercent = percent(round(ample, digit=3)))
      verim <- rbind(verim,newRow)
      
      rownames(verim) <- c("Jaccard (Similarity)", "Czekanowski (Dice) (Similarity)", "3w Jaccard (Similarity)", "Nei&Li (Similarity)",
                           "Sokal&Sneath-1 (Similarity)", "Sokal&Michener (Similarity)", "Sokal&Sneath-2 (Similarity)", "Roger&Tanimoto (Similarity)",
                           "Faith (Similarity)", "Gower&Legendre (Similarity)", "Intersection (Similarity)", "Innerproduct (Similarity)", "Russell&Rao (Similarity)",
                           "Hamming (Distance)", "Euclid (Distance)", "Squared-Euclid (Distance)", "Canberra (Distance)", "Manhattan (Distance)",
                           "Mean-Manhattan (Distance)", "Cityblock (Distance)", "Minkowski (Distance)", "Vari (Distance)", "Sizedifference (Distance)",
                           "Shapedifference (Distance)", "Patterndifference (Distance)", "Lance&Williams (Distance)", "Bray&Curtis (Distance)",
                           "Hellinger (Distance)", "Chord (Distance)", "Cosine (Similarity)", "Gilbert&Wells (Similarity)", "Ochiai-1 (Similarity)",
                           "Forbesi (Similarity)", "Fossum (Similarity)", "Sorgenfrei (Similarity)", "Mountford (Similarity)", "Otsuka (Similarity)",
                           "Mcconnaughey (Similarity)", "Tarwid (Similarity)", "Kulczynski-2 (Similarity)", "Driver&Kroeber (Similarity)",
                           "Johnson (Similarity)", "Dennis (Similarity)", "Simpson (Similarity)", "Braun&Banquet (Similarity)", "Fager&Mcgowan (Similarity)",
                           "Forbes-2 (Similarity)", "Sokal&Sneath-4 (Similarity)", "Gower (Similarity)", "Pearson&Heron-1 (Similarity)", "Pearson&Heron-2 (Similarity)",
                           "Sokal&Sneath-3 (Similarity)", "Sokal&Sneath-5 (Similarity)", "Cole (Similarity)", "Stiles (Similarity)", "Ochiai-2 (Similarity)",
                           "YuleQ (Similarity)", "YuleQ (Distance)", "YuleW (Similarity)", "Kulczynski-1 (Similarity)", "Tanimoto (Similarity)", "Disperson (Similarity)",
                           "Hamann (Similarity)", "Michael (Similarity)", "Goodman&Kruskal (Similarity)", "Anderberg (Similarity)", "Baroni-Urbani&Buser-1 (Similarity)",
                           "Baroni-Urbani&Buser-2 (Similarity)", "Peirce (Similarity)", "Eyraud (Similarity)", "Tarantula (Similarity)", "Ample (Similarity)")
      
      return(verim)

      
    }
  })
  
  my_data4 <- reactive({
    testdata <- dataM4()
    as.data.frame(cbind(Interpretation = shinyInput(actionButton, nrow(testdata),'button4_', label = "Interpretation", onclick = 'Shiny.onInputChange(\"select_button4\",  this.id)' ),testdata))
  })  
  output$dataUpload4 <- DT::renderDataTable(my_data4(),selection = 'single', extensions = c('Buttons','KeyTable', 'Responsive'),
                                            options = list(pageLength = 25,lengthMenu = c(25, 50, 75, 100),
                                                           dom = 'Bfrtip',
                                                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),keys = TRUE,
                                                           preDrawCallback = JS("function() { 
                                                                                Shiny.unbindAll(this.api().table().node()); }"), 
                                                           drawCallback = JS("function() { 
                                                                             Shiny.bindAll(this.api().table().node()); } ")),
                                            server = FALSE, escape = FALSE)
  
  
  # Here I created a reactive to save which row was clicked which can be stored for further analysis
  SelectedRow4 <- eventReactive(input$select_button4,{
    as.numeric(strsplit(input$select_button4, "_")[[1]][2])
  })
  
  # This is needed so that the button is clicked once for modal to show, a bug reported here
  # https://github.com/ebailey78/shinyBS/issues/57
  observeEvent(input$select_button4, {
    toggleModal(session, "modalExample4", "open")
  })
  
  DataRow4 <- eventReactive(input$select_button4,{
    my_data4()[SelectedRow4(),2:ncol(my_data4())]
  })
  
  output$popup4 <- renderUI({
     if (rownames(DataRow4()) == 'Jaccard (Similarity)') 
    {
       ## http://www.statisticshowto.com/jaccard-index/
      bsModal("modalExample4", paste0("Jaccard (Similarity): "), "", size = "large",
              h4("Description:"),
              print(paste0("The Jaccard similarity index -sometimes called the Jaccard similarity coefficient- compares members 
                           for two sets to see which members are shared and which are distinct. It is a measure of similarity for 
                           the two sets of data, with a range from 0% to 100%. The higher the percentage, the more similar the two 
                           populations. Although it is easy to interpret, it is extremely sensitive to small samples sizes and may 
                           give erroneous results, especially with very small samples or data sets with missing observations.
                           This is an index in which joint absences are excluded from consideration. Equal weight is given to matches and nonmatches. Also known as the similarity ratio.")),
              h4("Interpretation:"),
              h5("The",rownames(DataRow4())," distance between actual situation and clinical test result is ",DataRow4()$Measure,"."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow4()),
              column(12,                   
                     DT::renderDataTable(DataRow4())
                     
              )
              
      )}
    else if (rownames(DataRow4()) == 'Czekanowski (Dice) (Similarity)') 
    {
      ## http://www.int-res.com/articles/meps/5/m005p125.pdf
      bsModal("modalExample4", paste0("Czekanowski (Dice) (Similarity): "), "", size = "large",
              h4("Description:"),
              print(paste0("Since the term Czekanowski s Index is often used in marine studies and should be distinguished 
                           from the qualitative presence,absence form of the expression, 
                           it will be referred to here as,Czekanowski s Quantitative Index. 
                           The Czekanowski index, also known by other names like Sorensen Dice index,F1 score or Dice similarity coefficient, 
                           is a statistic used for comparing the similarity of two samples.This is an index in which joint absences are excluded from consideration, and matches are weighted double.
This coefficient is not very different in form from the Jaccard index. 
                           In fact, both are equivalent in the sense that given a value for the Sorensen Dice coefficient, 
                           one can calculate the respective Jaccard index value J and vice versa.
                           Since the Sorensen Dice coefficient does not satisfy the triangle inequality, it can be 
                           considered a semimetric version of the Jaccard index. ")),
              h4("Interpretation:"),
              h5("The",rownames(DataRow4())," distance between actual situation and clinical test result is ",DataRow4()$Measure,"."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow4()),
              column(12,                   
                     DT::renderDataTable(DataRow4())
                     
              )
              
      )}
    else if (rownames(DataRow4()) == 'Nei&Li (Similarity)') 
    {
      ## http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.458.8335&rep=rep1&type=pdf
      bsModal("modalExample4", paste0("Nei&Li (Similarity): "), "", size = "large",
              h4("Description:"),
              print(paste0("Nei & Li (Similarity) has the same calculation and interpretation as the Czekanowski (Dice) measurement.")),
              h4("Interpretation:"),
              h5("The",rownames(DataRow4())," distance between actual situation and clinical test result is ",DataRow4()$Measure,"."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow4()),
              column(12,                   
                     DT::renderDataTable(DataRow4())
                     
              )
              
      )}
    else if (rownames(DataRow4()) == 'Sokal&Sneath-1 (Similarity)') 
    {
      ## https://www.ibm.com/support/knowledgecenter/en/SSLVMB_23.0.0/spss/base/cmd_proximities_sim_measure_binary.html
      bsModal("modalExample4", paste0("Sokal&Sneath-1 (Similarity): "), "", size = "large",
              h4("Description:"),
              print(paste0("This is an index in which double weight is given to matches.")),
              h4("Interpretation:"),
              h5("The",rownames(DataRow4())," distance between actual situation and clinical test result is ",DataRow4()$Measure,"."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow4()),
              column(12,                   
                     DT::renderDataTable(DataRow4())
                     
              )
              
      )}
    else if (rownames(DataRow4()) == 'Sokal&Sneath-2 (Similarity)') 
    {
      ## https://www.ibm.com/support/knowledgecenter/en/SSLVMB_23.0.0/spss/base/cmd_proximities_sim_measure_binary.html
      bsModal("modalExample4", paste0("Sokal&Sneath-2 (Similarity): "), "", size = "large",
              h4("Description:"),
              print(paste0("This is an index in which double weight is given to nonmatches, and joint absences are excluded from consideration.")),
              h4("Interpretation:"),
              h5("The",rownames(DataRow4())," distance between actual situation and clinical test result is ",DataRow4()$Measure,"."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow4()),
              column(12,                   
                     DT::renderDataTable(DataRow4())
                     
              )
              
      )}
    else if (rownames(DataRow4()) == 'Sokal&Sneath-3 (Similarity)') 
    {
      ## https://www.ibm.com/support/knowledgecenter/en/SSLVMB_23.0.0/spss/base/cmd_proximities_sim_measure_binary.html
      bsModal("modalExample4", paste0("Sokal&Sneath-3 (Similarity): "), "", size = "large",
              h4("Description:"),
              print(paste0("This is the ratio of matches to nonmatches. This index has a lower bound of 0 and is unbounded 
                           above. It is theoretically undefined when there are no nonmatches; however, Distances assigns 
                           an arbitrary value of 9999.999 when the value is undefined or is greater than this value.")),
              h4("Interpretation:"),
              h5("The",rownames(DataRow4())," distance between actual situation and clinical test result is ",DataRow4()$Measure,"."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow4()),
              column(12,                   
                     DT::renderDataTable(DataRow4())
                     
              )
              
      )}
    else if (rownames(DataRow4()) == 'Russell&Rao (Similarity)') 
    {
      ## https://www.ibm.com/support/knowledgecenter/en/SSLVMB_23.0.0/spss/base/cmd_proximities_sim_measure_binary.html
      bsModal("modalExample4", paste0("Russell&Rao (Similarity): "), "", size = "large",
              h4("Description:"),
              print(paste0("This is a binary version of the inner (dot) product. Equal weight is given to matches and 
                           nonmatches. This is the default for binary similarity data.")),
              h4("Interpretation:"),
              h5("The",rownames(DataRow4())," distance between actual situation and clinical test result is ",DataRow4()$Measure,"."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow4()),
              column(12,                   
                     DT::renderDataTable(DataRow4())
                     
              )
              
      )}
    else if (rownames(DataRow4()) == 'Roger&Tanimoto (Similarity)') 
    {
      ## https://www.ibm.com/support/knowledgecenter/en/SSLVMB_23.0.0/spss/base/cmd_proximities_sim_measure_binary.html
      bsModal("modalExample4", paste0("Roger&Tanimoto (Similarity): "), "", size = "large",
              h4("Description:"),
              print(paste0("This is an index in which double weight is given to nonmatches.")),
              h4("Interpretation:"),
              h5("The",rownames(DataRow4())," distance between actual situation and clinical test result is ",DataRow4()$Measure,"."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow4()),
              column(12,                   
                     DT::renderDataTable(DataRow4())
                     
              )
              
      )}
    else if (rownames(DataRow4()) == 'Kulczynski-1 (Similarity)') 
    {
      ## https://www.ibm.com/support/knowledgecenter/en/SSLVMB_23.0.0/spss/base/cmd_proximities_sim_measure_binary.html
      bsModal("modalExample4", paste0("Kulczynski-1 (Similarity): "), "", size = "large",
              h4("Description:"),
              print(paste0("his is the ratio of joint presences to all nonmatches. This index has a lower bound of 0 and is unbounded above. It is theoretically undefined when there are no nonmatches; however, Distances assigns an arbitrary value of 9999.999 when the value is undefined or is greater than this value.")),
              h4("Interpretation:"),
              h5("The",rownames(DataRow4())," distance between actual situation and clinical test result is ",DataRow4()$Measure,"."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow4()),
              column(12,                   
                     DT::renderDataTable(DataRow4())
                     
              )
              
      )}
    else if (rownames(DataRow4()) == 'Kulczynski-2 (Similarity)') 
    {
      ## https://www.ibm.com/support/knowledgecenter/en/SSLVMB_23.0.0/spss/base/cmd_proximities_sim_measure_binary.html
      bsModal("modalExample4", paste0("Kulczynski-2 (Similarity): "), "", size = "large",
              h4("Description:"),
              print(paste0(" This index is based on the conditional probability that the characteristic is present in one item, given that it is present in the other. The separate values for each item acting as predictor of the other are averaged to compute this value.")),
              h4("Interpretation:"),
              h5("The",rownames(DataRow4())," distance between actual situation and clinical test result is ",DataRow4()$Measure,"."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow4()),
              column(12,                   
                     DT::renderDataTable(DataRow4())
                     
              )
              
      )}
    else if (rownames(DataRow4()) == 'Sokal&Sneath-4 (Similarity)') 
    {
      ## https://www.ibm.com/support/knowledgecenter/en/SSLVMB_23.0.0/spss/base/cmd_proximities_sim_measure_binary.html
      bsModal("modalExample4", paste0("Sokal&Sneath-4 (Similarity): "), "", size = "large",
              h4("Description:"),
              print(paste0("This index is based on the conditional probability that the characteristic in one item matches the value in the other. The separate values for each item acting as predictor of the other are averaged to compute this value.")),
              h4("Interpretation:"),
              h5("The",rownames(DataRow4())," distance between actual situation and clinical test result is ",DataRow4()$Measure,"."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow4()),
              column(12,                   
                     DT::renderDataTable(DataRow4())
                     
              )
              
      )}
    else if (rownames(DataRow4()) == 'Hamann (Similarity)') 
    {
      ## https://www.ibm.com/support/knowledgecenter/en/SSLVMB_23.0.0/spss/base/cmd_proximities_sim_measure_binary.html
      bsModal("modalExample4", paste0("Hamann (Similarity): "), "", size = "large",
              h4("Description:"),
              print(paste0("This index is the number of matches minus the number of nonmatches, divided by the total number of items. It ranges from -1 to 1.")),
              h4("Interpretation:"),
              h5("The",rownames(DataRow4())," distance between actual situation and clinical test result is ",DataRow4()$Measure,"."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow4()),
              column(12,                   
                     DT::renderDataTable(DataRow4())
                     
              )
              
      )}
    else if (rownames(DataRow4()) == 'Anderberg (Similarity)') 
    {
      ## https://www.ibm.com/support/knowledgecenter/en/SSLVMB_23.0.0/spss/base/cmd_proximities_sim_measure_binary.html
      bsModal("modalExample4", paste0("Anderberg (Similarity): "), "", size = "large",
              h4("Description:"),
              print(paste0("Similar to lambda, this index corresponds to the actual reduction of error using one item to predict the other (predicting in both directions). Values range from 0 to 1.")),
              h4("Interpretation:"),
              h5("The",rownames(DataRow4())," distance between actual situation and clinical test result is ",DataRow4()$Measure,"."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow4()),
              column(12,                   
                     DT::renderDataTable(DataRow4())
                     
              )
              
      )}
    else if (rownames(DataRow4()) == 'Ochiai-1 (Similarity)') 
    {
      ## https://www.ibm.com/support/knowledgecenter/en/SSLVMB_23.0.0/spss/base/cmd_proximities_sim_measure_binary.html
      bsModal("modalExample4", paste0("Ochiai-1 (Similarity): "), "", size = "large",
              h4("Description:"),
              print(paste0("This index is the binary form of the cosine similarity measure. It has a range of 0 to 1.")),
              h4("Interpretation:"),
              h5("The",rownames(DataRow4())," distance between actual situation and clinical test result is ",DataRow4()$Measure,"."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow4()),
              column(12,                   
                     DT::renderDataTable(DataRow4())
                     
              )
              
      )}
    else if (rownames(DataRow4()) == 'Ochiai-2 (Similarity)') 
    {
      ## https://www.ibm.com/support/knowledgecenter/en/SSLVMB_23.0.0/spss/base/cmd_proximities_sim_measure_binary.html
      bsModal("modalExample4", paste0("Ochiai-2 (Similarity): "), "", size = "large",
              h4("Description:"),
              print(paste0("This index is the binary form of the cosine similarity measure. It has a range of 0 to 1.")),
              h4("Interpretation:"),
              h5("The",rownames(DataRow4())," distance between actual situation and clinical test result is ",DataRow4()$Measure,"."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow4()),
              column(12,                   
                     DT::renderDataTable(DataRow4())
                     
              )
              
      )}
    else if (rownames(DataRow4()) == 'Sokal&Sneath-5 (Similarity)') 
    {
      ## https://www.ibm.com/support/knowledgecenter/en/SSLVMB_23.0.0/spss/base/cmd_proximities_sim_measure_binary.html
      bsModal("modalExample4", paste0("Sokal&Sneath-5 (Similarity): "), "", size = "large",
              h4("Description:"),
              print(paste0("This index is the squared geometric mean of conditional probabilities of positive and negative matches. It is independent of item coding. It has a range of 0 to 1.")),
              h4("Interpretation:"),
              h5("The",rownames(DataRow4())," distance between actual situation and clinical test result is ",DataRow4()$Measure,"."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow4()),
              column(12,                   
                     DT::renderDataTable(DataRow4())
                     
              )
              
      )}
    else if (rownames(DataRow4()) == 'Disperson (Similarity)') 
    {
      ## https://www.ibm.com/support/knowledgecenter/en/SSLVMB_23.0.0/spss/base/cmd_proximities_sim_measure_binary.html
      bsModal("modalExample4", paste0("Disperson (Similarity): "), "", size = "large",
              h4("Description:"),
              print(paste0("This index has a range of -1 to 1.")),
              h4("Interpretation:"),
              h5("The",rownames(DataRow4())," distance between actual situation and clinical test result is ",DataRow4()$Measure,"."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow4()),
              column(12,                   
                     DT::renderDataTable(DataRow4())
                     
              )
              
      )}
    else if (rownames(DataRow4()) == 'Lance&Williams (Distance)') 
    {
      ## https://www.ibm.com/support/knowledgecenter/en/SSLVMB_23.0.0/spss/base/cmd_proximities_sim_measure_binary.html
      bsModal("modalExample4", paste0("Lance&Williams (Distance): "), "", size = "large",
              h4("Description:"),
              print(paste0("Computed from a fourfold table as (FN+FP)/(2*TP+FN+FP), where a represents the cell corresponding 
                           to cases present on both items, and FN and FP represent the diagonal cells corresponding to 
                           cases present on one item but absent on the other. This measure has a range of 0 to 1. 
                           Also known as the Bray-Curtis nonmetric coefficient.")),
              h4("Interpretation:"),
              h5("The",rownames(DataRow4())," distance between actual situation and clinical test result is ",DataRow4()$Measure,"."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow4()),
              column(12,                   
                     DT::renderDataTable(DataRow4())
                     
              )
              
      )}
    else if (rownames(DataRow4()) == 'Sizedifference (Distance)') 
    {
      ## https://www.ibm.com/support/knowledgecenter/en/SSLVMB_23.0.0/spss/base/cmd_proximities_sim_measure_binary.html
      bsModal("modalExample4", paste0("Sizedifference (Distance): "), "", size = "large",
              h4("Description:"),
              print(paste0("An index of asymmetry. It ranges from 0 to 1.")),
              h4("Interpretation:"),
              h5("The",rownames(DataRow4())," distance between actual situation and clinical test result is ",DataRow4()$Measure,"."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow4()),
              column(12,                   
                     DT::renderDataTable(DataRow4())
                     
              )
              
      )}
    else if (rownames(DataRow4()) == 'Shapedifference (Distance)') 
    {
      ## https://www.ibm.com/support/knowledgecenter/en/SSLVMB_23.0.0/spss/base/cmd_proximities_sim_measure_binary.html
      bsModal("modalExample4", paste0("Shapedifference (Distance): "), "", size = "large",
              h4("Description:"),
              print(paste0("This distance measure has a range of 0 to 1, and it penalizes asymmetry of mismatches.")),
              h4("Interpretation:"),
              h5("The",rownames(DataRow4())," distance between actual situation and clinical test result is ",DataRow4()$Measure,"."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow4()),
              column(12,                   
                     DT::renderDataTable(DataRow4())
                     
              )
              
      )}
    else if (rownames(DataRow4()) == 'Patterndifference (Distance)') 
    {
      ## https://www.ibm.com/support/knowledgecenter/en/SSLVMB_23.0.0/spss/base/cmd_proximities_sim_measure_binary.html
      bsModal("modalExample4", paste0("Patterndifference (Distance): "), "", size = "large",
              h4("Description:"),
              print(paste0("Dissimilarity measure for binary data that ranges from 0 to 1. Computed from a fourfold table as 
                           bc/(n**2), where b and c represent the diagonal cells corresponding to cases present on one item but absent on the other and n is the total number of observations.")),
              h4("Interpretation:"),
              h5("The",rownames(DataRow4())," distance between actual situation and clinical test result is ",DataRow4()$Measure,"."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow4()),
              column(12,                   
                     DT::renderDataTable(DataRow4())
                     
              )
              
      )}
    else if (rownames(DataRow4()) == 'Minkowski (Distance)') 
    {
      ## https://www.ibm.com/support/knowledgecenter/en/SSLVMB_23.0.0/spss/base/cmd_proximities_sim_measure_binary.html
      bsModal("modalExample4", paste0("Minkowski (Distance): "), "", size = "large",
              h4("Description:"),
              print(paste0("The pth root of the sum of the absolute differences to the pth power between the values for the items.")),
              h4("Interpretation:"),
              h5("The",rownames(DataRow4())," distance between actual situation and clinical test result is ",DataRow4()$Measure,"."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow4()),
              column(12,                   
                     DT::renderDataTable(DataRow4())
                     
              )
              
      )}
    else if (rownames(DataRow4()) == 'Cityblock (Distance)') 
    {
      ## https://www.ibm.com/support/knowledgecenter/en/SSLVMB_23.0.0/spss/base/cmd_proximities_sim_measure_binary.html
      bsModal("modalExample4", paste0("Cityblock (Distance): "), "", size = "large",
              h4("Description:"),
              print(paste0("The sum of the absolute differences between the values of the item. Also known as Manhattan distance.")),
              h4("Interpretation:"),
              h5("The",rownames(DataRow4())," distance between actual situation and clinical test result is ",DataRow4()$Measure,"."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow4()),
              column(12,                   
                     DT::renderDataTable(DataRow4())
                     
              )
              
      )}
    else if (rownames(DataRow4()) == 'Squared-Euclid (Distance)') 
    {
      ## https://www.ibm.com/support/knowledgecenter/en/SSLVMB_23.0.0/spss/base/cmd_proximities_sim_measure_binary.html
      bsModal("modalExample4", paste0("Squared-Euclid (Distance): "), "", size = "large",
              h4("Description:"),
              print(paste0("The sum of the squared differences between the values for the items.")),
              h4("Interpretation:"),
              h5("The",rownames(DataRow4())," distance between actual situation and clinical test result is ",DataRow4()$Measure,"."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow4()),
              column(12,                   
                     DT::renderDataTable(DataRow4())
                     
              )
              
      )}
    else if (rownames(DataRow4()) == 'Euclid (Distance)') 
    {
      ## https://www.ibm.com/support/knowledgecenter/en/SSLVMB_23.0.0/spss/base/cmd_proximities_sim_measure_binary.html
      bsModal("modalExample4", paste0("Euclid (Distance): "), "", size = "large",
              h4("Description:"),
              print(paste0("The square root of the sum of the squared differences between values for the items. This is the default for interval data.")),
              h4("Interpretation:"),
              h5("The",rownames(DataRow4())," distance between actual situation and clinical test result is ",DataRow4()$Measure,"."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow4()),
              column(12,                   
                     DT::renderDataTable(DataRow4())
                     
              )
              
      )}
    else if (rownames(DataRow4()) == 'Cosine (Similarity)') 
    {
      ## https://www.ibm.com/support/knowledgecenter/en/SSLVMB_23.0.0/spss/base/cmd_proximities_sim_measure_binary.html
      bsModal("modalExample4", paste0("Cosine (Similarity): "), "", size = "large",
              h4("Description:"),
              print(paste0("The cosine of the angle between two vectors of values.")),
              h4("Interpretation:"),
              h5("The",rownames(DataRow4())," distance between actual situation and clinical test result is ",DataRow4()$Measure,"."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow4()),
              column(12,                   
                     DT::renderDataTable(DataRow4())
                     
              )
              
      )}
    else if (rownames(DataRow4()) == 'Canberra (Distance)') 
    {
      ## http://people.revoledu.com/kardi/tutorial/Similarity/CanberraDistance.html
      bsModal("modalExample4", paste0("Canberra (Distance): "), "", size = "large",
              h4("Description:"),
              print(paste0("Canberra distance examines the sum of series of a fraction differences
                           between coordinates of a pair of objects. Each term of fraction difference 
                           has value between 0 and 1. The Canberra distance itself is not between zero and one.
                           If one of coordinate is zero, the term become unity regardless the other value,
                           thus the distance will not be affected. Note that if both coordinate are zeros,
                           we need to be defined as 0/0=0. This distance is very sensitive to a small change 
                           when both coordinate near to zero.")),
              h4("Interpretation:"),
              h5("The",rownames(DataRow4())," distance between actual situation and clinical test result is ",DataRow4()$Measure,"."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow4()),
              column(12,                   
                     DT::renderDataTable(DataRow4())
                     
              )
              
      )}
    else if (rownames(DataRow4()) == 'Cityblock (Distance)') 
    {
      ## https://docs.tibco.com/pub/spotfire/7.0.0/doc/html/hc/hc_city_block_distance.htm
      bsModal("modalExample4", paste0("Cityblock (Distance): "), "", size = "large",
              h4("Description:"),
              print(paste0("The City block distance is always greater than or equal to zero. The measurement would be zero for identical points and high for points that show little similarity.
                           In most cases, this distance measure yields results similar to the Euclidean distance. 
Note, however, that with City block distance, the effect of a large difference in a single dimension is dampened 
(since the distances are not squared).The name City block distance (also referred to as Manhattan distance) is explained 
                           if you consider two points in the xy-plane. The shortest distance between the two points is along 
                           the hypotenuse, which is the Euclidean distance. The City block distance is instead calculated as 
                           the distance in x plus the distance in y, which is similar to the way you move in a city (like Manhattan) 
                           where you have to move around the buildings instead of going straight through.")),
              h4("Interpretation:"),
              h5("The",rownames(DataRow4())," distance between actual situation and clinical test result is ",DataRow4()$Measure,"."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow4()),
              column(12,                   
                     DT::renderDataTable(DataRow4())
                     
              )
              
              )}
    else
      {
    bsModal("modalExample4", paste0("Interpretation: "), "", size = "large",
            print(paste0("The ",rownames(DataRow4())," distance between actual situation and clinical test result is ",DataRow4(),".")),
            h5("**************************************************************************************************************************************************"),
            h5("Data for Row Number:",SelectedRow4()),
            column(12,                   
                   DT::renderDataTable(DataRow4())
                   
           )
            
            
    )
    }
  })

  output$plot4 <- highcharter::renderHighchart({
    if (input$verigir_kontrol == TRUE){ 
      a = 0
      b = 0
      c = 0
      d = 0
      for (i in 1:nrow(tabloveri()))
      {
        if (tabloveri()[i,1]==1 & tabloveri()[i,2]==1)
        {a = a+1}
        else if (tabloveri()[i,1]==1 & tabloveri()[i,2]==2)
        {b = b+1}
        else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==1)
        {c = c+1}
        else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==2)
        {d = d+1}
        next
      }
      CI = as.numeric(trimws(input$text5_2))
    }
    else{
      a = as.numeric(trimws(input$text1)) ### x
      b = as.numeric(trimws(input$text2)) ### y
      c = as.numeric(trimws(input$text3)) ### z
      d = as.numeric(trimws(input$text4)) ### t
      CI = as.numeric(trimws(input$text5))
    }
    
    satirtop1 = a + b
    satirtop2 = c + d
    sutuntop1 = a + c
    sutuntop2 = b + d
    toplam = satirtop1 + satirtop2
    
    ###http://www.iiisci.org/journal/CV$/sci/pdfs/GS315JG.pdf 
    jaccard = a / (a+b+c)
    dice = (2*a) / (2*a+b+c)
    czekanowski = (2*a) / (2*a+b+c)
    w3_jaccard = (3*a) / (3*a+b+c)
    nei_li = (2*a) / ((a+b)+(a+c))
    sokal_sneath_1 = a / (a+2*b+2*c)
    sokal_michener = (a+d) / (a+b+c+d)
    sokal_sneath_2 = (2*(a+d)) / (2*a+b+c+2*d)
    roger_tanimoto = (a+d) / (a+2*(b+c)+d)
    faith = (a + 0.5*d) / (a+b+c+d)
    gower_legendre = (a+d) / (a+0.5*(b+c)+d)
    intersection = a
    innerproduct = a+d
    russell_rao = a / (a+b+c+d)
    d_hamming = b+c
    d_euclid = sqrt(b+c)
    d_squared_euclid = sqrt((b+c)^2)
    d_canberra = (b+c)^(2/2)
    d_manhattan = b+c
    d_mean_manhattan = (b+c)/(a+b+c+d)
    d_cityblock = b+c
    d_minkowski =(b+c)^(1/1)
    d_vari = (b+c) / (4*(a+b+c+d))
    d_sizedifference = (b+c)^2 / (a+b+c+d)^2
    d_shapedifference =(toplam*(b+c)-(b-c)^2) / (a+b+c+d)^2
    d_patterndifference =(4*b*c) / (a+b+c+d)^2
    d_lance_williams = (b+c) / (2*a+b+c)
    d_bray_curtis = (b+c) / (2*a+b+c)
    d_hellinger = 2*sqrt((1-(a/sqrt((a+b)*(a+c)))))
    d_chord = sqrt(2*(1-a/ sqrt((a+b)*(a+c))))
    cosine = a/sqrt((a+b)*(a+c))^2
    gilbert_wells = log(a)-log(toplam)-log((a+b)/toplam)-log((a+c)/toplam)
    ochiai_1 = a / sqrt((a+b)*(a+c))
    forbesi = (toplam*a) / ((a+b)*(a+c))
    fossum =(toplam*(a-0.5)^2) / ((a+b)*(a+c))
    sorgenfrei = a^2 / ((a+b)*(a+c))
    mountford = a / (0.5*(a*b+a*c)+b*c)
    otsuka = a / ((a+b)*(a+c))^0.5
    mcconnaughey =(a^2-b*c) / ((a+b)*(a+c))
    tarwid =(toplam*a-(a+b)*(a+c)) / (toplam*a+(a+b)*(a+c))
    kulczynski_2 =((a/2)*(2*a+b+c)) / ((a+b)*(a+c))
    driver_kroeber = (a/2)*(1/(a+b)+1/(a+c))
    johnson = a/(a+b) + a/(a+c)
    dennis = (a*d-b*c) / sqrt(toplam*(a+b)*(a+c))
    simpson = a / min(a+b,a+c)
    braun_banquet = a / max(a+b,a+c)
    fager_mcgowan = a / sqrt((a+b)*(a+c)) - max(a+b,a+c)/2
    forbes_2 =(toplam*a-(a+b)*(a+c)) / (toplam*min(a+b,a+c)-(a+b)*(a+c))
    sokal_sneath_4 = (a/(a+b) + a/(a+c) + d/(b+d) + d/(b+d)) /4
    gower = (a+d) / sqrt((a+b)*(a+c)*(b+d)*(c+d))
    ## pearson_1
    ## pearson_2
    ## pearson_3
    pearson_heron_1 = (a*d-b*c) / sqrt((a+b)*(a+c)*(b+d)*(c+d))
    pearson_heron_2 = cos((pi*sqrt(b*c)) / (sqrt(a*d)+sqrt(b*c)))
    sokal_sneath_3 = (a+d)/(b+c)
    sokal_sneath_5 = (a*d) / ((a+b)*(a+c)*(b+d)*(c+d)^0.5)
    cole = (sqrt(2)*(a*d-b*c)) / sqrt((a*d-b*c)^2-(a+b)*(a+c)*(b+d)*(c+d))
    stiles = log((toplam*(abs(a*d-b*c)-toplam/2)^2) / ((a+b)*(a+c)*(b+d)*(c+d)))
    ochiai_2 = (a*d) / sqrt((a+b)*(a+c)*(b+d)*(c+d))
    yuleq = (a*d-b*c) / (a*d+b*c)
    d_yuleq = (2*b*c) / (a*d+b*c)
    yulew = (sqrt(a*d)-sqrt(b*c)) / (sqrt(a*d)+sqrt(b*c))
    kulczynski_1 = a / (b+c)
    tanimoto = a / ((a+b)+(a+c)-a)
    disperson = (a*d-b*c) / (a+b+c+d)^2
    hamann = ((a+b)-(b+c)) / (a+b+c+d)
    michael = (4*(a*d-b*c)) / ((a+d)^2+(b+c)^2)
    goodman_kruskal = ((max(a,b)+max(c,d)+max(a,c)+max(b,d)) - (max(a+c,b+d)+max(a+b,c+d))) / (2*toplam - (max(a+c,b+d)+max(a+b,c+d)))
    anderberg = ((max(a,b)+max(c,d)+max(a,c)+max(b,d)) - (max(a+c,b+d)+max(a+b,c+d))) / (2*toplam)
    baroni_urbani_buser_1 = (sqrt(a*d)+a) / (sqrt(a*d)+a+b+c)
    baroni_urbani_buser_2 = (sqrt(a*d)+a-(b+c)) / (sqrt(a*d)+a+b+c)
    peirce = (a*b+b*c) / (a*b+2*b*c+c*d)
    eyraud = (toplam^2*(toplam*a-(a+b)*(a+c))) / ((a+b)*(a+c)*(b+d)*(c+d))
    tarantula = (a*(c+d)) / (c*(a+b))
    ample = abs((a*(c+d)) / (c*(a+b)))
    
   
    
    verim <- data.frame(Measure = round(jaccard, digit=3))
    newRow <- data.frame(Measure = round(dice, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure = round(czekanowski, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(w3_jaccard, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(nei_li, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(sokal_sneath_1, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(sokal_michener, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(sokal_sneath_2, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(roger_tanimoto, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(faith, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(gower_legendre, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(intersection, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(innerproduct, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(russell_rao, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_hamming, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_euclid, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_squared_euclid, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_canberra, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_manhattan, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_mean_manhattan, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_cityblock, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_minkowski, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_vari, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_sizedifference, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_shapedifference, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_patterndifference, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_lance_williams, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_bray_curtis, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_hellinger, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_chord, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(cosine, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(gilbert_wells, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(ochiai_1, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(forbesi, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(fossum, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(sorgenfrei, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(mountford, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(otsuka, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(mcconnaughey, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(tarwid, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(kulczynski_2, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(driver_kroeber, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(johnson, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(dennis, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(simpson, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(braun_banquet, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(fager_mcgowan, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(forbes_2, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(sokal_sneath_4, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(gower, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(pearson_heron_1, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(pearson_heron_2, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(sokal_sneath_3, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(sokal_sneath_5, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(cole, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(stiles, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(ochiai_2, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(yuleq, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_yuleq, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(yulew, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(kulczynski_1, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(tanimoto, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(disperson, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(hamann, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(michael, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(goodman_kruskal, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(anderberg, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(baroni_urbani_buser_1, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(baroni_urbani_buser_2, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(peirce, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(eyraud, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(tarantula, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(ample, digit=3))
    verim <- rbind(verim,newRow)
    
    rownames(verim) <- c("Jaccard (Similarity)", "Dice (Similarity)", "Czekanowski (Similarity)", "3w Jaccard (Similarity)", "Nei&Li (Similarity)",
                         "Sokal&Sneath-1 (Similarity)", "Sokal&Michener (Similarity)", "Sokal&Sneath-2 (Similarity)", "Roger&Tanimoto (Similarity)",
                         "Faith (Similarity)", "Gower&Legendre (Similarity)", "Intersection (Similarity)", "Innerproduct (Similarity)", "Russell&Rao (Similarity)",
                         "Hamming (Distance)", "Euclid (Distance)", "Squared-Euclid (Distance)", "Canberra (Distance)", "Manhattan (Distance)",
                         "Mean-Manhattan (Distance)", "Cityblock (Distance)", "Minkowski (Distance)", "Vari (Distance)", "Sizedifference (Distance)",
                         "Shapedifference (Distance)", "Patterndifference (Distance)", "Lance&Williams (Distance)", "Bray&Curtis (Distance)",
                         "Hellinger (Distance)", "Chord (Distance)", "Cosine (Similarity)", "Gilbert&Wells (Similarity)", "Ochiai-1 (Similarity)",
                         "Forbesi (Similarity)", "Fossum (Similarity)", "Sorgenfrei (Similarity)", "Mountford (Similarity)", "Otsuka (Similarity)",
                         "Mcconnaughey (Similarity)", "Tarwid (Similarity)", "Kulczynski-2 (Similarity)", "Driver&Kroeber (Similarity)",
                         "Johnson (Similarity)", "Dennis (Similarity)", "Simpson (Similarity)", "Braun&Banquet (Similarity)", "Fager&Mcgowan (Similarity)",
                         "Forbes-2 (Similarity)", "Sokal&Sneath-4 (Similarity)", "Gower (Similarity)", "Pearson&Heron-1 (Similarity)", "Pearson&Heron-2 (Similarity)",
                         "Sokal&Sneath-3 (Similarity)", "Sokal&Sneath-5 (Similarity)", "Cole (Similarity)", "Stiles (Similarity)", "Ochiai-2 (Similarity)",
                         "YuleQ (Similarity)", "YuleQ (Distance)", "YuleW (Similarity)", "Kulczynski-1 (Similarity)", "Tanimoto (Similarity)", "Disperson (Similarity)",
                         "Hamann (Similarity)", "Michael (Similarity)", "Goodman&Kruskal (Similarity)", "Anderberg (Similarity)", "Baroni-Urbani&Buser-1 (Similarity)",
                         "Baroni-Urbani&Buser-2 (Similarity)", "Peirce (Similarity)", "Eyraud (Similarity)", "Tarantula (Similarity)", "Ample (Similarity)")
    
    
    highchart() %>% hc_exporting(enabled = TRUE, filename = "Plot4") %>% 
      hc_add_series(name = "Measures", type = "line", data = verim$`Measure`) %>%
      hc_xAxis(categories = rownames(verim),title = "Similarity and Distance Measures") %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, headerFormat = "<b>Similarity and Distance Measures: </b>{point.x} <br>") %>%
      hc_plotOptions(line = list(tooltip = list(pointFormat = "<b>{series.name}: </b>{point.y:.3f} "))) %>%
      hc_add_theme(hc_theme_google())
    
    
    
    
  })
  
  output$plot41 <- highcharter::renderHighchart({
    if (input$verigir_kontrol == TRUE){ 
      a = 0
      b = 0
      c = 0
      d = 0
      for (i in 1:nrow(tabloveri()))
      {
        if (tabloveri()[i,1]==1 & tabloveri()[i,2]==1)
        {a = a+1}
        else if (tabloveri()[i,1]==1 & tabloveri()[i,2]==2)
        {b = b+1}
        else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==1)
        {c = c+1}
        else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==2)
        {d = d+1}
        next
      }
      CI = as.numeric(trimws(input$text5_2))
    }
    else{
      a = as.numeric(trimws(input$text1)) ### x
      b = as.numeric(trimws(input$text2)) ### y
      c = as.numeric(trimws(input$text3)) ### z
      d = as.numeric(trimws(input$text4)) ### t
      CI = as.numeric(trimws(input$text5))
    }
    
    satirtop1 = a + b
    satirtop2 = c + d
    sutuntop1 = a + c
    sutuntop2 = b + d
    toplam = satirtop1 + satirtop2
    
    ###http://www.iiisci.org/journal/CV$/sci/pdfs/GS315JG.pdf 
    jaccard = a / (a+b+c)
    dice = (2*a) / (2*a+b+c)
    czekanowski = (2*a) / (2*a+b+c)
    w3_jaccard = (3*a) / (3*a+b+c)
    nei_li = (2*a) / ((a+b)+(a+c))
    sokal_sneath_1 = a / (a+2*b+2*c)
    sokal_michener = (a+d) / (a+b+c+d)
    sokal_sneath_2 = (2*(a+d)) / (2*a+b+c+2*d)
    roger_tanimoto = (a+d) / (a+2*(b+c)+d)
    faith = (a + 0.5*d) / (a+b+c+d)
    gower_legendre = (a+d) / (a+0.5*(b+c)+d)
    intersection = a
    innerproduct = a+d
    russell_rao = a / (a+b+c+d)
    d_hamming = b+c
    d_euclid = sqrt(b+c)
    d_squared_euclid = sqrt((b+c)^2)
    d_canberra = (b+c)^(2/2)
    d_manhattan = b+c
    d_mean_manhattan = (b+c)/(a+b+c+d)
    d_cityblock = b+c
    d_minkowski =(b+c)^(1/1)
    d_vari = (b+c) / (4*(a+b+c+d))
    d_sizedifference = (b+c)^2 / (a+b+c+d)^2
    d_shapedifference =(toplam*(b+c)-(b-c)^2) / (a+b+c+d)^2
    d_patterndifference =(4*b*c) / (a+b+c+d)^2
    d_lance_williams = (b+c) / (2*a+b+c)
    d_bray_curtis = (b+c) / (2*a+b+c)
    d_hellinger = 2*sqrt((1-(a/sqrt((a+b)*(a+c)))))
    d_chord = sqrt(2*(1-a/ sqrt((a+b)*(a+c))))
    cosine = a/sqrt((a+b)*(a+c))^2
    gilbert_wells = log(a)-log(toplam)-log((a+b)/toplam)-log((a+c)/toplam)
    ochiai_1 = a / sqrt((a+b)*(a+c))
    forbesi = (toplam*a) / ((a+b)*(a+c))
    fossum =(toplam*(a-0.5)^2) / ((a+b)*(a+c))
    sorgenfrei = a^2 / ((a+b)*(a+c))
    mountford = a / (0.5*(a*b+a*c)+b*c)
    otsuka = a / ((a+b)*(a+c))^0.5
    mcconnaughey =(a^2-b*c) / ((a+b)*(a+c))
    tarwid =(toplam*a-(a+b)*(a+c)) / (toplam*a+(a+b)*(a+c))
    kulczynski_2 =((a/2)*(2*a+b+c)) / ((a+b)*(a+c))
    driver_kroeber = (a/2)*(1/(a+b)+1/(a+c))
    johnson = a/(a+b) + a/(a+c)
    dennis = (a*d-b*c) / sqrt(toplam*(a+b)*(a+c))
    simpson = a / min(a+b,a+c)
    braun_banquet = a / max(a+b,a+c)
    fager_mcgowan = a / sqrt((a+b)*(a+c)) - max(a+b,a+c)/2
    forbes_2 =(toplam*a-(a+b)*(a+c)) / (toplam*min(a+b,a+c)-(a+b)*(a+c))
    sokal_sneath_4 = (a/(a+b) + a/(a+c) + d/(b+d) + d/(b+d)) /4
    gower = (a+d) / sqrt((a+b)*(a+c)*(b+d)*(c+d))
    ## pearson_1
    ## pearson_2
    ## pearson_3
    pearson_heron_1 = (a*d-b*c) / sqrt((a+b)*(a+c)*(b+d)*(c+d))
    pearson_heron_2 = cos((pi*sqrt(b*c)) / (sqrt(a*d)+sqrt(b*c)))
    sokal_sneath_3 = (a+d)/(b+c)
    sokal_sneath_5 = (a*d) / ((a+b)*(a+c)*(b+d)*(c+d)^0.5)
    cole = (sqrt(2)*(a*d-b*c)) / sqrt((a*d-b*c)^2-(a+b)*(a+c)*(b+d)*(c+d))
    stiles = log((toplam*(abs(a*d-b*c)-toplam/2)^2) / ((a+b)*(a+c)*(b+d)*(c+d)))
    ochiai_2 = (a*d) / sqrt((a+b)*(a+c)*(b+d)*(c+d))
    yuleq = (a*d-b*c) / (a*d+b*c)
    d_yuleq = (2*b*c) / (a*d+b*c)
    yulew = (sqrt(a*d)-sqrt(b*c)) / (sqrt(a*d)+sqrt(b*c))
    kulczynski_1 = a / (b+c)
    tanimoto = a / ((a+b)+(a+c)-a)
    disperson = (a*d-b*c) / (a+b+c+d)^2
    hamann = ((a+b)-(b+c)) / (a+b+c+d)
    michael = (4*(a*d-b*c)) / ((a+d)^2+(b+c)^2)
    goodman_kruskal = ((max(a,b)+max(c,d)+max(a,c)+max(b,d)) - (max(a+c,b+d)+max(a+b,c+d))) / (2*toplam - (max(a+c,b+d)+max(a+b,c+d)))
    anderberg = ((max(a,b)+max(c,d)+max(a,c)+max(b,d)) - (max(a+c,b+d)+max(a+b,c+d))) / (2*toplam)
    baroni_urbani_buser_1 = (sqrt(a*d)+a) / (sqrt(a*d)+a+b+c)
    baroni_urbani_buser_2 = (sqrt(a*d)+a-(b+c)) / (sqrt(a*d)+a+b+c)
    peirce = (a*b+b*c) / (a*b+2*b*c+c*d)
    eyraud = (toplam^2*(toplam*a-(a+b)*(a+c))) / ((a+b)*(a+c)*(b+d)*(c+d))
    tarantula = (a*(c+d)) / (c*(a+b))
    ample = abs((a*(c+d)) / (c*(a+b)))
    
    
    
    verim <- data.frame(Measure = round(jaccard, digit=3))
    newRow <- data.frame(Measure = round(dice, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure = round(czekanowski, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(w3_jaccard, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(nei_li, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(sokal_sneath_1, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(sokal_michener, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(sokal_sneath_2, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(roger_tanimoto, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(faith, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(gower_legendre, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(intersection, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(innerproduct, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(russell_rao, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_hamming, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_euclid, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_squared_euclid, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_canberra, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_manhattan, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_mean_manhattan, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_cityblock, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_minkowski, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_vari, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_sizedifference, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_shapedifference, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_patterndifference, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_lance_williams, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_bray_curtis, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_hellinger, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_chord, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(cosine, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(gilbert_wells, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(ochiai_1, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(forbesi, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(fossum, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(sorgenfrei, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(mountford, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(otsuka, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(mcconnaughey, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(tarwid, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(kulczynski_2, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(driver_kroeber, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(johnson, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(dennis, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(simpson, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(braun_banquet, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(fager_mcgowan, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(forbes_2, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(sokal_sneath_4, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(gower, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(pearson_heron_1, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(pearson_heron_2, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(sokal_sneath_3, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(sokal_sneath_5, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(cole, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(stiles, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(ochiai_2, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(yuleq, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(d_yuleq, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(yulew, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(kulczynski_1, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(tanimoto, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(disperson, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(hamann, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(michael, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(goodman_kruskal, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(anderberg, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(baroni_urbani_buser_1, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(baroni_urbani_buser_2, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(peirce, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(eyraud, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(tarantula, digit=3))
    verim <- rbind(verim,newRow)
    newRow <- data.frame(Measure=round(ample, digit=3))
    verim <- rbind(verim,newRow)
    
    rownames(verim) <- c("Jaccard (Similarity)", "Dice (Similarity)", "Czekanowski (Similarity)", "3w Jaccard (Similarity)", "Nei&Li (Similarity)",
                         "Sokal&Sneath-1 (Similarity)", "Sokal&Michener (Similarity)", "Sokal&Sneath-2 (Similarity)", "Roger&Tanimoto (Similarity)",
                         "Faith (Similarity)", "Gower&Legendre (Similarity)", "Intersection (Similarity)", "Innerproduct (Similarity)", "Russell&Rao (Similarity)",
                         "Hamming (Distance)", "Euclid (Distance)", "Squared-Euclid (Distance)", "Canberra (Distance)", "Manhattan (Distance)",
                         "Mean-Manhattan (Distance)", "Cityblock (Distance)", "Minkowski (Distance)", "Vari (Distance)", "Sizedifference (Distance)",
                         "Shapedifference (Distance)", "Patterndifference (Distance)", "Lance&Williams (Distance)", "Bray&Curtis (Distance)",
                         "Hellinger (Distance)", "Chord (Distance)", "Cosine (Similarity)", "Gilbert&Wells (Similarity)", "Ochiai-1 (Similarity)",
                         "Forbesi (Similarity)", "Fossum (Similarity)", "Sorgenfrei (Similarity)", "Mountford (Similarity)", "Otsuka (Similarity)",
                         "Mcconnaughey (Similarity)", "Tarwid (Similarity)", "Kulczynski-2 (Similarity)", "Driver&Kroeber (Similarity)",
                         "Johnson (Similarity)", "Dennis (Similarity)", "Simpson (Similarity)", "Braun&Banquet (Similarity)", "Fager&Mcgowan (Similarity)",
                         "Forbes-2 (Similarity)", "Sokal&Sneath-4 (Similarity)", "Gower (Similarity)", "Pearson&Heron-1 (Similarity)", "Pearson&Heron-2 (Similarity)",
                         "Sokal&Sneath-3 (Similarity)", "Sokal&Sneath-5 (Similarity)", "Cole (Similarity)", "Stiles (Similarity)", "Ochiai-2 (Similarity)",
                         "YuleQ (Similarity)", "YuleQ (Distance)", "YuleW (Similarity)", "Kulczynski-1 (Similarity)", "Tanimoto (Similarity)", "Disperson (Similarity)",
                         "Hamann (Similarity)", "Michael (Similarity)", "Goodman&Kruskal (Similarity)", "Anderberg (Similarity)", "Baroni-Urbani&Buser-1 (Similarity)",
                         "Baroni-Urbani&Buser-2 (Similarity)", "Peirce (Similarity)", "Eyraud (Similarity)", "Tarantula (Similarity)", "Ample (Similarity)")
    
    
    highchart() %>% hc_exporting(enabled = TRUE, filename = "Plot4") %>% 
      hc_add_series(name = "Measures", type = "bar", data = verim$`Measure`) %>%
      hc_xAxis(categories = rownames(verim),title = "Similarity and Distance Measures") %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, headerFormat = "<b>Similarity and Distance Measures: </b>{point.x} <br>") %>%
      hc_plotOptions(line = list(tooltip = list(pointFormat = "<b>{series.name}: </b>{point.y:.3f} "))) %>%
      hc_add_theme(hc_theme_google())
    
    
    
    
  })
  
  ## Risk Measures
  dataM5 <- reactive({   
    if (is.null(trimws(input$text3)) && is.null(trimws(input$text4)))  {return(NULL)}
    else {
      if (input$verigir_kontrol == TRUE){ 
        TP = 0
        FN = 0
        FP = 0
        TN = 0
        for (i in 1:nrow(tabloveri()))
        {
          if (tabloveri()[i,1]==1 & tabloveri()[i,2]==1)
          {TP = TP+1}
          else if (tabloveri()[i,1]==1 & tabloveri()[i,2]==2)
          {FN = FN+1}
          else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==1)
          {FP = FP+1}
          else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==2)
          {TN = TN+1}
          next
        }
        CI = as.numeric(trimws(input$text5_2))
      }
      else{
        TP = as.numeric(trimws(input$text1)) ### x
        FN = as.numeric(trimws(input$text2)) ### y
        FP = as.numeric(trimws(input$text3)) ### z
        TN = as.numeric(trimws(input$text4)) ### t
        CI = as.numeric(trimws(input$text5))
      }
      
      
      satirtop1 = TP + FN
      satirtop2 = FP + TN
      sutuntop1 = TP + FP
      sutuntop2 = FN + TN
      toplam = satirtop1 + satirtop2
      #table1 <- matrix(c(TP,FN,satirtop1,FP,TN,satirtop2,sutuntop1,sutuntop2,toplam),nrow=3,ncol=3,byrow=TRUE,dimnames=list(c("1.satir","2.satir","Sutun toplamlari"),c("1.kolon","2.kolon","Satir toplamlari")))
      #table1
      ## Duyarlilik 
      sensitivity_estimate = TP / satirtop1
      sensitivity_se = round((sqrt(sensitivity_estimate * (1 - sensitivity_estimate)/satirtop1)), 4)
      sensitivity_low = 1 - Rbeta.inv(((1 + CI/100)/2), (satirtop1+1-TP), TP)
      sensitivity_upp = Rbeta.inv(((1 + CI/100)/2), (TP+1), (satirtop1-TP))
      sensitivity_randomtest = sutuntop1 / toplam
      qualityindex_sensitivity = (sensitivity_estimate - sensitivity_randomtest) / (1 - sensitivity_randomtest)
      ## Secicilik
      specificity = TN / satirtop2
      specificity_se = sqrt(specificity * (1 - specificity) / satirtop2)
      specificity_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop2+1-TN),TN)
      specificity_upp = Rbeta.inv((1 + CI/100)/2, (TN+1), (satirtop2-TN))
      specificity_randomtest = (1 - sensitivity_randomtest)
      qualityindex_specificity = (specificity - 1 + sensitivity_randomtest) / sensitivity_randomtest
      ## Gain in Certainty
      gain_in_certainty = sensitivity_estimate + specificity
      ## Etkililik, yeterlilik ayni zamanda overall accuracy (toplam-genel dogruluk orani)
      efficiency = (TP + TN) / toplam
      efficiency_se = sqrt(efficiency * (1 - efficiency) / toplam)
      efficiency_low = 1-Rbeta.inv((1 + CI/100)/2, (toplam+1-TP-TN), (TP+TN))
      efficiency_upp = Rbeta.inv((1 + CI/100)/2, (TP+TN+1), (toplam-TP-TN))
      efficiency_randomtest = (satirtop1 / toplam) * sensitivity_randomtest + (1 - (satirtop1 / toplam)) * (1 - sensitivity_randomtest)
      mis_efficiency = 1 - efficiency
      ## Quality indeks
      quality_index = (efficiency - efficiency_randomtest) / (1 - efficiency_randomtest)
      quality_index_se = sqrt((efficiency*(1-efficiency)/(1-efficiency_randomtest)^2+2*(1-efficiency)*(2*efficiency*efficiency_randomtest-((TP*((satirtop1 / toplam)+sensitivity_randomtest)+TN*(2-(satirtop1 / toplam)-sensitivity_randomtest))/toplam))/(1-efficiency_randomtest)^3+((1-efficiency)^2)*((TP*((satirtop1 / toplam)+sensitivity_randomtest)^2+FN*(-(satirtop1 / toplam)+1+sensitivity_randomtest)^2+FP*(1+(satirtop1 / toplam)-sensitivity_randomtest)^2+TN*(2-(satirtop1 / toplam)-sensitivity_randomtest)^2)/toplam-4*efficiency_randomtest^2)/(1-efficiency_randomtest)^4)/toplam)
      quality_index_low = quality_index + qnorm((1 - CI/100)/2)*quality_index_se
      quality_index_upp = quality_index - qnorm((1 - CI/100)/2)*quality_index_se
      ## Youden indeksi
      youdens_index = sensitivity_estimate + specificity - 1
      youdens_index_se = sqrt(sensitivity_se^2+specificity_se^2)
      youdens_index_low = youdens_index + qnorm((1 - CI/100)/2)*youdens_index_se
      youdens_index_upp = youdens_index - qnorm((1 - CI/100)/2)*youdens_index_se
      
      number_needed_to_diagnose = 1 / youdens_index
      number_needed_to_misdiagnose = 1 / ( 1 - efficiency ) 
      ## Pozitif kestirim degeri
      predictivevalue_positivetest = TP / sutuntop1
      predictivevalue_positivetest_se = sqrt(predictivevalue_positivetest*(1-predictivevalue_positivetest)/sutuntop1)
      predictivevalue_positivetest_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop1+1-TP),TP)
      predictivevalue_positivetest_upp = Rbeta.inv((1 + CI/100)/2, (TP+1),(sutuntop1-TP))
      predvalue_positiverandomtest = (satirtop1 / toplam)
      predvalue_positiverandomtest_mar_ol = predvalue_positiverandomtest*sensitivity_estimate+(1-predvalue_positiverandomtest)*(1-specificity)
      ## Negatif kestirim degeri
      predictivevalue_negativetest = TN / sutuntop2
      predictivevalue_negativetest_se = sqrt(predictivevalue_negativetest*(1-predictivevalue_negativetest)/sutuntop2)
      predictivevalue_negativetest_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop2+1-TN),TN)
      predictivevalue_negativetest_upp = Rbeta.inv((1 + CI/100)/2, (TN+1), (sutuntop2-TN))
      predvalue_negativerandomtest = 1 - (satirtop1 / toplam)
      predvalue_negativerandomtest_mar_ol = 1 - predvalue_positiverandomtest_mar_ol
      ## Predictive Summary Index (PSI)
      predictive_summary_index = predictivevalue_positivetest + predictivevalue_negativetest - 1
      ## Yanlis pozitif orani
      false_positiverate = FP / satirtop2
      false_positiverate_se = sqrt(false_positiverate*(1-false_positiverate)/satirtop2)
      false_positiverate_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop2+1-FP),FP)
      false_positiverate_upp = Rbeta.inv((1 + CI/100)/2, (FP+1), (satirtop2-FP))
      ## Yansis negatif orani
      false_negativerate = FN / satirtop1
      false_negativerate_se = sqrt(false_negativerate*(1-false_negativerate)/satirtop1)
      false_negativerate_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop1+1-FN),FN)
      false_negativerate_upp = Rbeta.inv((1 + CI/100)/2, (FN+1), (satirtop1-FN))
      ## False Omission Rate (FOR) https://www.ncss.com/software/ncss/diagnostic-tests-in-ncss/
      false_omission_rate = FN/(sutuntop2)
      false_omission_rate_se = sqrt(false_omission_rate*(1-false_omission_rate)/sutuntop2)
      false_omission_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop2+1-FN),FN)
      false_omission_rate_upp = Rbeta.inv((1 + CI/100)/2, (FN+1), (sutuntop2-FN))
      ## False Discovery Rate (FDR) https://www.ncss.com/software/ncss/diagnostic-tests-in-ncss/
      false_discovery_rate = FP/(sutuntop1)
      false_discovery_rate_se = sqrt(false_discovery_rate*(1-false_discovery_rate)/sutuntop1)
      false_discovery_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop1+1-FP),FP)
      false_discovery_rate_upp = Rbeta.inv((1 + CI/100)/2, (FP+1), (sutuntop1-FP))
      ## Dogru pozitif orani
      true_positiverate = TP / (satirtop1)
      ## Yanlis siniflandirma orani
      misclassification_rate = (FN + FP) / toplam
      misclassification_rate_se = sqrt(misclassification_rate*(1-misclassification_rate)/toplam)
      misclassification_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-FP-FN), (FP+FN))
      misclassification_rate_upp = Rbeta.inv((1 + CI/100)/2, (FP+FN+1), (toplam-FP-FN))
      ## Prevelans
      prevalence = (satirtop1 / toplam)
      prevalence_se = sqrt((satirtop1 / toplam)*(1-(satirtop1 / toplam))/toplam)
      prevalence_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-satirtop1), satirtop1)
      prevalence_upp = Rbeta.inv((1 + CI/100)/2, (satirtop1+1), (toplam-satirtop1))
      
      ##caret paketinden olculer
      detection_rate = TP/toplam
      detection_prevalence = (TP+FP)/toplam
      balanced_accuracy = (sensitivity_estimate + specificity)/2
      lift = (TP/(TP+FN))/((TP+FP)/toplam)  ## https://gerardnico.com/wiki/data_mining/error_rate
      precision = TP/(TP+FP)
      recall = TP/(TP+FN)
      ## http://michael.hahsler.net/research/recomm_lnai2002/lnai2002.pdf
      conviction = (1-((TP+FP)/toplam))/(1-(TP/(TP+FN)))
      matthews_correlation_coefficient = (TP * TN + FN * FP) / ((TP+FP)*(FN+TN)*(FP+TN)*(TP+FN))^(1/2)
      ## Risk
      ## https://sph.unc.edu/files/2015/07/nciph_ERIC7.pdf
      risk_in_exposed = TP/(TP+FP)
      risk_in_exposed_low = 1 - Rbeta.inv((1 + CI/100)/2, ((TP+FP)+1-TP), TP)
      risk_in_exposed_upp = Rbeta.inv((1 + CI/100)/2, (TP+1), ((TP+FP)-TP))
      risk_in_unexposed = FN/(FN+TN)
      risk_in_unexposed_low = 1 - Rbeta.inv((1 + CI/100)/2, ((FN+TN)+1-FN), FN)
      risk_in_unexposed_upp = Rbeta.inv((1 + CI/100)/2, (TP+1), ((FN+TN)-FN))
      overall_risk = (TP+FN)/toplam
      ## Goreli Risk Orani (Relative risk)
      ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
      # https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20ratio.htm
      relative_risk = (TP/sutuntop1)/(FN/sutuntop2)
      relative_risk_se = sqrt((1/TP)+(1/FN)-(1/(TP+FP))-(1/(FN+TN)))
      relative_risk_low = exp(log(relative_risk)+qnorm((1-CI/100)/2)*relative_risk_se)
      relative_risk_upp = exp(log(relative_risk)-qnorm((1-CI/100)/2)*relative_risk_se)
      difference_in_proportion = (TP/sutuntop1) - (FN/sutuntop2)
      ## Leverage
      ## https://www3.nd.edu/~tweninge/pubs/AHBCW_ANNIE08.pdf
      leverage = (TP/sutuntop1)-((TP+FP)/toplam)
      ## http://www.ebm.med.ualberta.ca/TherapyCalc.html
      control_event_rate = FN/sutuntop2
      experimental_event_rate = TP/sutuntop1
      ## Absolute risk reduction
      absolute_risk_reduction = -difference_in_proportion
      absolute_risk_reduction_se = sqrt((TP/sutuntop1)*(1-(TP/sutuntop1))/sutuntop1+(FN/sutuntop2)*(1-(FN/sutuntop2))/sutuntop2)
      absolute_risk_reduction_low = absolute_risk_reduction - qnorm((1-CI/100)/2)*absolute_risk_reduction_se
      absolute_risk_reduction_upp = absolute_risk_reduction + qnorm((1-CI/100)/2)*absolute_risk_reduction_se
      ## Relative risk reduction
      relative_risk_reduction = absolute_risk_reduction / (FN/sutuntop2)
      relative_risk_reduction_low = 1 - exp(log(relative_risk)-qnorm((1-CI/100)/2)*relative_risk_se)
      relative_risk_reduction_upp = 1 - exp(log(relative_risk)+qnorm((1-CI/100)/2)*relative_risk_se)
      ##Number needed to treat
      number_needed_to_treat = 1 / abs(difference_in_proportion)
      number_needed_to_treat_low = 1 /absolute_risk_reduction_upp
      number_needed_to_treat_upp = 1 /absolute_risk_reduction_low
      ## Test duzeyi
      test_level = sensitivity_randomtest
      test_level_se = sqrt(test_level*(1-test_level)/toplam)
      test_level_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-sutuntop1),sutuntop1)
      test_level_upp = Rbeta.inv((1 + CI/100)/2, (sutuntop1+1), (toplam-sutuntop1))
      ## Pre-test odds
      pretest_odds = prevalence / (1 - prevalence)
      ## Pozitif Olabilirlik Orani
      likelihoodratio_positivetest = sensitivity_estimate/(1-specificity)
      likelihoodratio_positivetest_se = exp(sqrt((1-sensitivity_estimate)/(sensitivity_estimate*satirtop1) +specificity/((1-specificity)*satirtop2)))
      likelihoodratio_positivetest_low = exp(log(likelihoodratio_positivetest) + qnorm((1 - CI/100)/2)*log(likelihoodratio_positivetest_se))
      likelihoodratio_positivetest_upp = exp(log(likelihoodratio_positivetest) - qnorm((1 - CI/100)/2)*log(likelihoodratio_positivetest_se))
      ## post-test odds
      ## http://www.fpnotebook.com/Prevent/Epi/PrTstOds.htm
      posttest_odds = pretest_odds * likelihoodratio_positivetest
      bayes_ppv = (sensitivity_estimate * prevalence)/((sensitivity_estimate * prevalence) + (1 - specificity) * (1 - prevalence))
      ## Post-test probability (positive test result)
      posttest_probability = (pretest_odds * likelihoodratio_positivetest) / (1 + (pretest_odds * likelihoodratio_positivetest))
      ## Negatif Olabilirlik Orani
      likelihoodratio_negativetest = (1-sensitivity_estimate)/specificity
      likelihoodratio_negativetest_se = exp(sqrt(sensitivity_estimate/((1-sensitivity_estimate)*satirtop1) +(1-specificity)/(specificity*satirtop2)))
      likelihoodratio_negativetest_low = exp(log(likelihoodratio_negativetest) + qnorm((1-CI/100)/2)*log(likelihoodratio_negativetest_se))
      likelihoodratio_negativetest_upp = exp(log(likelihoodratio_negativetest) - qnorm((1-CI/100)/2)*log(likelihoodratio_negativetest_se))
      bayes_npv = (sensitivity_estimate * (1 - prevalence))/((sensitivity_estimate * (1 - prevalence)) + (1 - specificity) * prevalence)
      ## Post-test probability (negative test result)
      posttest_probability_neg = (pretest_odds * likelihoodratio_negativetest) / (1 + (pretest_odds * likelihoodratio_negativetest))
      ## Ters negatif olabilirlik orani
      inverse_likelihoodratio_negativetest = specificity/(1-sensitivity_estimate)
      inverse_likelihoodratio_negativetest_se = exp(sqrt(sensitivity_estimate/((1-sensitivity_estimate)*satirtop1) +(1-specificity)/(specificity*satirtop2)))
      inverse_likelihoodratio_negativetest_low = exp(log(inverse_likelihoodratio_negativetest) + qnorm((1-CI/100)/2)*log(inverse_likelihoodratio_negativetest_se))
      inverse_likelihoodratio_negativetest_upp = exp(log(inverse_likelihoodratio_negativetest) - qnorm((1-CI/100)/2)*log(inverse_likelihoodratio_negativetest_se))
      ## Diagnostic Odds Ratio (Tani Odds Orani)
      odds_ratio = (TP/FN)/(FP/TN)
      odds_ratio_se = exp(sqrt(1/TP+1/FP+1/TN+1/FN))
      odds_ratio_low = exp(log(odds_ratio) + qnorm((1-CI/100)/2)*log(odds_ratio_se))
      odds_ratio_upp = exp(log(odds_ratio) - qnorm((1-CI/100)/2)*log(odds_ratio_se))
      odds_ratio_Haldane = ((TP+0.5)/(FN+0.5))/((FP+0.5)/(TN+0.5))
      odds_ratio_Haldane_se = exp(sqrt(1/(TP+0.5)+1/(FP+0.5)+1/(TN+0.5)+1/(FN+0.5)))
      odds_ratio_Haldane_low = exp(log(odds_ratio_Haldane) + qnorm((1-CI/100)/2)*log(odds_ratio_Haldane_se))
      odds_ratio_Haldane_upp = exp(log(odds_ratio_Haldane) - qnorm((1-CI/100)/2)*log(odds_ratio_Haldane_se))
      error_odds_ratio = (sensitivity_estimate/(1-sensitivity_estimate))/(specificity/(1-specificity))
      ## Rate ratio
      ## http://omarkasule-05.tripod.com/id52.html
      ## http://sphweb.bumc.bu.edu/otlt/mph-modules/ep/ep713_randomerror/ep713_randomerror4.html
      rate_ratio = (TP/sutuntop1)/(TP/sutuntop2)
      rate_ratio_low = exp(log(relative_risk)+qnorm((1-CI/100)/2)*sqrt(1/TP+1/FP))
      rate_ratio_upp = exp(log(relative_risk)-qnorm((1-CI/100)/2)*sqrt(1/TP+1/FP))
      ## Risk Difference
      ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
      risk_difference = (TP/sutuntop1) - (FN/sutuntop2)
      risk_difference_se = (((TP/sutuntop1)*(1-(TP/sutuntop1)))/sutuntop1)+(((FN/sutuntop2)*(1-(FN/sutuntop2)))/sutuntop2)
      risk_difference_low = risk_difference + qnorm((1-CI/100)/2)*sqrt(risk_difference_se)
      risk_difference_upp = risk_difference - qnorm((1-CI/100)/2)*sqrt(risk_difference_se)
      ## Nitelenebilen risk
      ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
      attributable_risk = (TP/sutuntop1)-(FN/sutuntop2)
      attributable_risk_se = sqrt((satirtop1/toplam)*(1-(satirtop1/toplam))*(1/sutuntop1+1/sutuntop2))
      attributable_risk_low = attributable_risk + qnorm((1-CI/100)/2)*attributable_risk_se
      attributable_risk_upp = attributable_risk - qnorm((1-CI/100)/2)*attributable_risk_se
      ## Attributable risk percent
      ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
      attributable_risk_percent = ((relative_risk-1)/relative_risk)*100
      attributable_risk_percent_low = attributable_risk_percent - (qnorm((1-CI/100)/2)*attributable_risk_se)/attributable_risk
      attributable_risk_percent_upp = attributable_risk_percent + (qnorm((1-CI/100)/2)*attributable_risk_se)/attributable_risk
      ## Population Attributable Risk
      ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
      population_attributable_risk = (satirtop1/toplam)-(FN/sutuntop2)
      ## Population Attributable Risk Percent
      population_attributable_risk_percent = (population_attributable_risk / (satirtop1/toplam))*100
      ## Kohen's Kappa
      cohens_kappa = (efficiency-efficiency_randomtest)/(1-efficiency_randomtest)
      cohens_kappa_se = quality_index_se
      cohens_kappa_low = cohens_kappa + qnorm((1-CI/100)/2)*cohens_kappa_se
      cohens_kappa_upp = cohens_kappa - qnorm((1-CI/100)/2)*cohens_kappa_se
      ## Gozlenen Karar
      observed_agreement = efficiency
      observed_agreement_se = efficiency_se
      observed_agreement_low = efficiency_low
      observed_agreement_upp = efficiency_upp
      ## Risk karari,expected agreement
      chance_agreement = efficiency_randomtest
      ## Pozitif karar
      positive_agreement = 2*TP /(satirtop1+sutuntop1)
      positive_agreement_se = sqrt(4*TP*(FP+FN)*(TP+FP+FN))/(sutuntop1+satirtop1)^2
      positive_agreement_low = positive_agreement + qnorm((1-CI/100)/2)*positive_agreement_se
      positive_agreement_upp = positive_agreement - qnorm((1-CI/100)/2)*positive_agreement_se
      ## Negatif karar
      negative_agreement = 2*TN/(satirtop2+sutuntop2)
      negative_agreement_se = sqrt(4*TN*(FP+FN)*(TN+FP+FN))/(sutuntop2+satirtop2)^2
      negative_agreement_low = negative_agreement + qnorm((1-CI/100)/2)*negative_agreement_se
      negative_agreement_upp = negative_agreement - qnorm((1-CI/100)/2)*negative_agreement_se
      ## Rand index
      ## http://www.cs.pomona.edu/classes/cs158/resources/158-10(EM).pdf
      rand_index = (TP+TN)/(TP+FN+FP+TN)
      ## e-measure
      ## http://michael.hahsler.net/research/recomm_lnai2002/lnai2002.pdf
      e_measure = 1/((((1-CI)/100)*(1-precision))+((1-((1-CI)/100))*(1/recall)))
      ## discriminant power 
      ## https://eva.fing.edu.uy/pluginfile.php/69453/mod_resource/content/1/7633-10048-1-PB.pdf
      discriminant_power = (sqrt(3)/pi)*(log (sensitivity_estimate / (1-sensitivity_estimate)) + log (specificity / (1-specificity)))
      ## F1 Score
      ## http://machinelearningmastery.com/classification-accuracy-is-not-enough-more-performance-measures-you-can-use/
      f1_score = 2*(((TP/(TP+FN))*(TP/(TP+FP)))/((TP/(TP+FN))+(TP/(TP+FP))))
      ## Byrt yanlilik indeksi
      byrt_bias_index = (FN-FP)/toplam
      ## Byrt  asimetrik indeks prevelansi
      byrt_prevalence_asymmetry_index = (TN-TP)/toplam
      ## Yanlilik duzeltmeli kappa
      bias_adjusted_kappa = (efficiency-0.25*((satirtop1+sutuntop1)^2+(satirtop2+sutuntop2)^2)/toplam^2)/(1-0.25*((satirtop1+sutuntop1)^2+(satirtop2+sutuntop2)^2)/toplam^2)
      ## Yanlilik duzeltmeli kappa prevelansi
      prevalence_bias_adjusted_kappa = cohens_kappa+(1-cohens_kappa)*(byrt_prevalence_asymmetry_index^2-byrt_bias_index^2)
      ## Dice'in indeksi (Czekanowski)
      dice_index = positive_agreement
      dice_index_se= positive_agreement_se
      dice_index_low = positive_agreement_low
      dice_index_upp = positive_agreement_upp
      ## Yule's Q
      yules_q = (odds_ratio - 1) / (odds_ratio + 1)
      yules_q_se = 0.5*(1-yules_q^2)*sqrt(1/TP+1/FP+1/TN+1/FN)
      yules_q_low = yules_q + qnorm((1-CI/100)/2)*yules_q_se
      yules_q_upp = yules_q - qnorm((1-CI/100)/2)*yules_q_se
      
      equitable_threatscore = (TP-(sutuntop1*satirtop1/toplam))/(TP+FP+FN-(sutuntop1*satirtop1/toplam))
      ## Phi
      phi = (TP*TN-FN*FP) / sqrt(satirtop1 * satirtop2 * sutuntop1 * sutuntop2)
      phi_se = sqrt((1-phi^2+(phi+0.5*phi^3)*((satirtop1-satirtop2)*(sutuntop1-sutuntop2)/sqrt(satirtop1*satirtop2*sutuntop1*sutuntop2))-0.75*phi^2*((satirtop1-satirtop2)^2/(satirtop1*satirtop2)+(sutuntop1-sutuntop2)^2/(sutuntop1*sutuntop2)))/toplam)
      phi_low = phi + qnorm((1-CI/100)/2)*phi_se
      phi_upp = phi - qnorm((1-CI/100)/2)*phi_se
      ## Cramer V katsayisi
      cramer_v = sqrt((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2))/toplam)
      ## Olaganlik katsayisi
      contingency_coefficient = sqrt((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2))/(((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)))+toplam))
      ## Goodman and Kruskal Gamma 
      goodman_and_kruskal_gamma = (TP*TN-FN*FP)/(TP*TN+FN*FP)
      goodman_and_kruskal_gamma_se = 0.5*(1-yules_q^2)*sqrt(1/TP+1/FP+1/TN+1/FN)
      goodman_and_kruskal_gamma_low = yules_q + qnorm((1-CI/100)/2)*yules_q_se
      goodman_and_kruskal_gamma_upp = yules_q - qnorm((1-CI/100)/2)*yules_q_se
      ## Kendall's tau a
      kendalls_tau_a = (TP*TN-FN*FP)/(toplam*(toplam-1)/2)
      ## Kendall's tau b
      ## https://v8doc.sas.com/sashtml/stat/chap28/sect20.htm
      ## http://slideplayer.com/slide/10766029/
      kendalls_tau_b = (TP*TN-FN*FP)/sqrt((TP*TN+FN*FP+(TP*FN+FP*TN))*(TP*TN+FN*FP+(TP*FP+FN*TN)))
      ## Kendall's tau c
      kendalls_tau_c = ((2*2)*(TP*TN-FN*FP)) / (toplam^2 * (2-1))
      ## Somers'd R|C
      somers_d = (TP*TN-FN*FP)/(TP*TN+FN*FP+(TP*FN+FP*TN))
      ## Somers'd C|R
      somers_d_2 = (TP*TN-FP*FN)/(TP*TN+FP*FN+(TP*FP+FN*TN))
      ## Scoot'un karar indeksi
      scotts_agreement_index = bias_adjusted_kappa
      ## Dort-duzeyli Korelasyon
      tetrachoric_correlation = cos(pi/(1+sqrt(TP*TN/FN/FP)))
      tetrachoric_correlation_se = 0.375*(1-tetrachoric_correlation^2)*sqrt((1/(TP))+(1/(FP))+(1/(TN))+(1/(FN)))
      tetrachoric_correlation_low = tetrachoric_correlation + qnorm((1-CI/100)/2)*tetrachoric_correlation_se
      tetrachoric_correlation_upp = tetrachoric_correlation - qnorm((1-CI/100)/2)*tetrachoric_correlation_se
      ## Goodman kruskal tau katsayisi
      goodman_kruskal_tau = (toplam*((TP^2+FP^2)/sutuntop1+(FN^2+TN^2)/sutuntop2)-satirtop1^2-satirtop2^2)/(toplam^2-satirtop1^2-satirtop2^2)
      goodman_kruskal_tau_se = toplam*sqrt(TP*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop1)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*TP/satirtop1-(TP^2+FN^2)/satirtop1^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+FN*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop2)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*FN/satirtop1-(TP^2+FN^2)/satirtop1^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+FP*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop1)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*FP/satirtop2-(FP^2+TN^2)/satirtop2^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+TN*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop2)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*TN/satirtop2-(FP^2+TN^2)/satirtop2^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2)/(toplam^2-sutuntop1^2-sutuntop2^2)^2
      goodman_kruskal_tau_low = goodman_kruskal_tau + qnorm((1-CI/100)/2)*goodman_kruskal_tau_se
      goodman_kruskal_tau_upp = goodman_kruskal_tau - qnorm((1-CI/100)/2)*goodman_kruskal_tau_se
      ## Simetrik Lambda 
      lambda_symmetric = ((max(TP,FN)+max(FP,TN)+max(TP,FP)+max(FN,TN)-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2))/(2*toplam-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2)))
      if (lambda_symmetric==0 || lambda_symmetric==1)
      {lambda_symmetric_se = 0}
      else
      {
        lambda_symmetric_se = sqrt((TP*((if(TP>FN) 1 else 0) + (if(TP>FP) 1 else 0) + ((if(sutuntop1>sutuntop2) 1 else 0)+ (if(satirtop1>satirtop2) 1 else 0))*(lambda_symmetric-1))^2+FN*( (if(TP>FN) 0 else 1)+ (if(FN>TN) 1 else 0) +( (if(sutuntop1>sutuntop2) 0 else 1) + (if(satirtop1>satirtop2) 1 else 0))*(lambda_symmetric-1))^2+FP*( (if(FP>TN) 1 else 0) + (if(TP>FP) 0 else 1) +( (if(sutuntop1>sutuntop2) 1 else 0) + (if(satirtop1<satirtop2) 1 else 0))*(lambda_symmetric-1))^2+TN*((if(FP>TN) 0 else 1)+ (if(FN>TN) 0 else 1) +((if(sutuntop1>sutuntop2) 0 else 1)+ (if(satirtop1<satirtop2) 1 else 0))*(lambda_symmetric-1))^2-4*toplam*lambda_symmetric^2)/(2*toplam-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2))^2)
      }
      
      lambda_symmetric_low = lambda_symmetric + qnorm((1-CI/100)/2)*lambda_symmetric_se
      lambda_symmetric_upp = lambda_symmetric - qnorm((1-CI/100)/2)*lambda_symmetric_se
      ## Lambda Asymmetric R|C
      lambda_criterion = (max(TP,FP)+max(FN,TN)-max(satirtop1,satirtop2))/(toplam-max(satirtop1,satirtop2))
      
      if (lambda_criterion==0 || lambda_criterion==1)
      {lambda_criterion_se = 0}
      else
      {
        lambda_criterion_se = sqrt((toplam-max(TP,FP)-max(FN,TN))*(max(TP,FP)+max(FN,TN)+max(satirtop1,satirtop2)-2*(if(satirtop1>satirtop2) max(TP,FN) else max(FP,TN)))/(toplam-max(satirtop1,satirtop2))^3)
      }
      
      lambda_criterion_low = lambda_criterion + qnorm((1-CI/100)/2)*lambda_criterion_se
      lambda_criterion_upp = lambda_criterion - qnorm((1-CI/100)/2)*lambda_criterion_se
      ## Lambda Asymmetric C|R
      lambda_criterion_2 = (max(TP,FN)+max(FP,TN)-max(sutuntop1,sutuntop2))/(toplam-max(sutuntop1,sutuntop2))
      
      if (lambda_criterion_2==0 || lambda_criterion_2==1)
      {lambda_criterion_se_2 = 0}
      else
      {
        lambda_criterion_se_2 = sqrt((toplam-max(TP,FN)-max(FP,TN))*(max(TP,FN)+max(FP,TN)+max(sutuntop1,sutuntop2)-2*(if(sutuntop1>sutuntop2) max(TP,FP) else max(FN,TN)))/(toplam-max(sutuntop1,sutuntop2))^3)
      }
      
      lambda_criterion_low_2 = lambda_criterion_2 + qnorm((1-CI/100)/2)*lambda_criterion_se_2
      lambda_criterion_upp_2 = lambda_criterion_2 - qnorm((1-CI/100)/2)*lambda_criterion_se_2
      ## belirsizlik katsayisi (coefficient of uncertainty) simetrik
      uncertainty_coefficient_symmetric = 2*(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam)))/(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))
      uncertainty_coefficient_symmetric_se = 2*sqrt(TP*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop1*sutuntop1/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(TP/toplam))^2+FN*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop1*sutuntop2/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(FN/toplam))^2+FP*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop2*sutuntop1/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(FP/toplam))^2+TN*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop2*sutuntop2/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(TN/toplam))^2)/toplam/(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam)^2
      uncertainty_coefficient_symmetric_low = uncertainty_coefficient_symmetric + qnorm((1-CI/100)/2)*uncertainty_coefficient_symmetric_se
      uncertainty_coefficient_symmetric_upp = uncertainty_coefficient_symmetric - qnorm((1-CI/100)/2)*uncertainty_coefficient_symmetric_se
      ## (coefficient of uncertainty) R|C
      uncertainty_coeff_crit = (satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam)))/(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))
      uncertainty_coeff_crit_se = sqrt(TP*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(TP/sutuntop1)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop1/toplam))^2+FN*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(FN/sutuntop2)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop1/toplam))^2+FP*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(FP/sutuntop1)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop2/toplam))^2+TN*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(TN/sutuntop2)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop2/toplam))^2)/toplam/((satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam)^2
      uncertainty_coeff_crit_low = uncertainty_coeff_crit + qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se
      uncertainty_coeff_crit_upp = uncertainty_coeff_crit - qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se
      ## (coefficient of uncertainty) C|R
      uncertainty_coeff_crit_2 = (sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)+satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)-(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam)))/(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))
      uncertainty_coeff_crit_se_2 = sqrt(TP*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(TP/satirtop1)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop1/toplam))^2+FP*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(FP/satirtop2)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop1/toplam))^2+FN*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(FN/satirtop1)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop2/toplam))^2+TN*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(TN/satirtop2)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop2/toplam))^2)/toplam/((sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)^2
      uncertainty_coeff_crit_low_2 = uncertainty_coeff_crit_2 + qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se_2
      uncertainty_coeff_crit_upp_2 = uncertainty_coeff_crit_2 - qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se_2
      ## Yule s Y (Coefficient of colligation)
      yule_y = (sqrt(TP*TN)-sqrt(FN*FP))/(sqrt(TP*TN)+sqrt(FN*FP))
      ## Pearson ki-kare
      pearson_chi_squ = toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)
      pearson_chi_squ_p = dchi(pearson_chi_squ, df=1)
      ## 
      with_yate_cor_for_pearson = toplam*(abs(TP*TN-FN*FP)-0.5*toplam)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)
      with_yate_cor_p_for_pearson = dchi(with_yate_cor_for_pearson, df=1)
      ## Mantel Haenszel chi-square
      ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
      mantel_haenszel = ((toplam-1)*(TP*TN-FN*FP)^2)/(satirtop1*satirtop2*sutuntop1*sutuntop2)
      mantel_haenszel_p = dchi(mantel_haenszel, df=1)
      ## Olasilik orani
      likelih_ratio = 2*(TP*log(TP/(sutuntop1*satirtop1/toplam))+FN*log(FN/(sutuntop2*satirtop1/toplam))+FP*log(FP/(sutuntop1*satirtop2/toplam))+TN*log(TN/(sutuntop2*satirtop2/toplam)))
      likelih_ratio_p =dchi(likelih_ratio, df=1)
      ## Fisher'in exact testi
      ##fisher_exact_test = (TP-(satirtop1*sutuntop1/toplam))^2/(satirtop1*sutuntop1/toplam) + (FN-(satirtop1*sutuntop2/toplam))^2/(satirtop1*sutuntop2/toplam) + (FP-(satirtop2*sutuntop1/toplam))^2/(satirtop2*sutuntop1/toplam) + (TN-(satirtop2*sutuntop2/toplam))^2/(satirtop2*sutuntop2/toplam)
      fisher_test = (factorial(TP+FN)*factorial(FP+TN)*factorial(TP+FP)*factorial(FN*TN))*(factorial(toplam)*factorial(TP)*factorial(TN)*factorial(FP)*factorial(FN))
      fisher <- c(TP, FN, FP, TN)
      tab <- t(matrix(fisher, nrow=2,ncol=2))
      fisher_exact_test = fisher.test(tab)$p.value
      minimum_exp_fre = min((sutuntop1*satirtop1/toplam):(sutuntop2*satirtop2/toplam))
      cells_exp_fre_5 = (if((sutuntop1*satirtop1/toplam)<5) 1 else 0)+ (if((sutuntop1*satirtop2/toplam)<5) 1 else 0)+ (if((sutuntop2*satirtop1/toplam) <5) 1 else 0)+ (if((sutuntop2*satirtop2/toplam) <5) 1 else 0)
      cells_exp_fre_1 = (if((sutuntop1*satirtop1/toplam) <1) 1 else 0)+ (if((sutuntop1*satirtop2/toplam) <1) 1 else 0)+ (if((sutuntop2*satirtop1/toplam) <1) 1 else 0)+ (if((sutuntop2*satirtop2/toplam) <1) 1 else 0)
      ## Mc Nemar testi
      mcNemar_test = (FP-FN)^2/(FP+FN)
      mcNemar_test_p = dchi(mcNemar_test, df=1)
      with_Yate_cor_for_mcnamer = (abs(FP-FN)-1)^2/(FP+FN)
      with_Yate_cor_p_for_mcnamer = dchi(with_Yate_cor_for_mcnamer, df=1)
      
      ## Belirsizlik (Entropi)
      forbes_NMI = sensitivity_estimate*predvalue_positiverandomtest*log((sensitivity_estimate/predvalue_positiverandomtest_mar_ol),base=2)+
        (1-specificity)*(1-predvalue_positiverandomtest)*log(((1-specificity)/predvalue_positiverandomtest_mar_ol),base=2)+
        (1-sensitivity_estimate)*predvalue_positiverandomtest*log(((1-sensitivity_estimate)/(1-predvalue_positiverandomtest_mar_ol)),base=2)+
        specificity*(1-predvalue_positiverandomtest)*log((specificity/(1-predvalue_positiverandomtest_mar_ol)),base=2)
      ## satir icin entropy (test icin)
      entropy_hr = - ((sutuntop1/toplam) * log(sutuntop1/toplam, base=2) + (sutuntop2/toplam) * log(sutuntop2/toplam, base=2))
      ## sutun icin entropy (hastalik icin)  
      entropy_hc = - ((satirtop1/toplam)*log(satirtop1/toplam, base=2) + (satirtop2/toplam)*log(satirtop2/toplam, base=2))
      ## birlesik entropi (joint entropy)  
      entropy_hrc = - ( (TP/toplam)*log(TP/toplam, base=2) + (FP/toplam)*log(FP/toplam, base=2) + (FN/toplam)*log(FN/toplam, base=2) + (TN/toplam)*log(TN/toplam, base=2))
      ## bilgi icerigi (mutual information)  
      information_r_c = entropy_hr + entropy_hc - entropy_hrc
      ## kosullu entropi (conditional entropy)  
      a = entropy_hrc - entropy_hr
      c = entropy_hrc - entropy_hc
      sim_r_c = information_r_c / (a + information_r_c + c)
      dis_r_c = (a + c) / (a + information_r_c + c)
      ## goreli entropi (relative entropy, kullback-leibler uzakligi)
      ## pozitif test sonucu icin goreli entropi, Relative Improvement Over Chance (RIOC)
      positive_relative_entropy = (TP/sutuntop1)*log(((TP/sutuntop1)/ prevalence), base=2)+(FP/sutuntop1)*log(((FP/sutuntop1)/(1- prevalence)), base=2)
      ## negatif test sonucu icin goreli entropi 
      negative_relative_entropy = (FN/sutuntop2)*log(((FN/sutuntop2)/ prevalence), base=2)+(TN/sutuntop2)*log(((TN/sutuntop2)/(1- prevalence)), base=2)
      
      verim <- data.frame(Estimate = format(round(risk_in_exposed, digit=3),nsmall=3), EstimatePercent = percent(round(risk_in_exposed, digit=3)), LowerCI = format(round(risk_in_exposed_low, digit=3),nsmall=3), UpperCI = format(round(risk_in_exposed_upp, digit=3),nsmall=3))
      newRow <- data.frame(Estimate = format(round(risk_in_unexposed, digit=3),nsmall=3), EstimatePercent = percent(round(risk_in_unexposed, digit=3)), LowerCI = format(round(risk_in_unexposed_low, digit=3),nsmall=3), UpperCI = format(round(risk_in_unexposed_upp, digit=3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(overall_risk, digit=3),nsmall=3), EstimatePercent = percent(round(overall_risk, digit=3)), LowerCI = format(round(prevalence_low, digit=3),nsmall=3), UpperCI = format(round(prevalence_upp, digit=3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(prevalence, digit=3),nsmall=3), EstimatePercent = percent(round(prevalence, digit=3)), LowerCI = format(round(prevalence_low, digit=3),nsmall=3), UpperCI = format(round(prevalence_upp, digit=3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(detection_rate, digit=3),nsmall=3), EstimatePercent = percent(round(detection_rate, digit=3)), LowerCI = 0, UpperCI = 0)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(detection_prevalence, digit=3),nsmall=3),EstimatePercent = percent(round(detection_prevalence, digit=3)), LowerCI = 0, UpperCI = 0)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(difference_in_proportion, digit=3),nsmall=3),EstimatePercent = percent(round(difference_in_proportion, digit=3)), LowerCI = 0, UpperCI = 0)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(control_event_rate, digit=3),nsmall=3),EstimatePercent = percent(round(difference_in_proportion, digit=3)), LowerCI = 0, UpperCI = 0)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(experimental_event_rate, digit=3),nsmall=3),EstimatePercent = percent(round(difference_in_proportion, digit=3)), LowerCI = 0, UpperCI = 0)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(number_needed_to_treat, digit=3),nsmall=3),EstimatePercent = percent(round(number_needed_to_treat, digit=3)),LowerCI = format(round(number_needed_to_treat_low, digit=3),nsmall=3), UpperCI = format(round(number_needed_to_treat_upp, digit=3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(absolute_risk_reduction, digit=3),nsmall=3),EstimatePercent = percent(round(absolute_risk_reduction, digit=3)), LowerCI = format(round(absolute_risk_reduction_low, digit=3),nsmall=3), UpperCI = format(round(absolute_risk_reduction_upp, digit=3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(relative_risk_reduction, digit=3),nsmall=3),EstimatePercent = percent(round(relative_risk_reduction, digit=3)),LowerCI = format(round(relative_risk_reduction_low, digit=3),nsmall=3), UpperCI = format(round(relative_risk_reduction_upp, digit=3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(test_level, digit=3),nsmall=3),EstimatePercent = percent(round(test_level, digit=3)), LowerCI = format(round(test_level_low, digit=3),nsmall=3), UpperCI = format(round(test_level_upp, digit=3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(pretest_odds, digit=3),nsmall=3),EstimatePercent = percent(round(pretest_odds, digit=3)), LowerCI = 0, UpperCI = 0)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(likelihoodratio_positivetest, digit=3),nsmall=3),EstimatePercent = percent(round(likelihoodratio_positivetest, digit=3)), LowerCI = format(round(likelihoodratio_positivetest_low, digit=3),nsmall=3), UpperCI = format(round(likelihoodratio_positivetest_upp, digit=3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(posttest_odds, digit=3),nsmall=3),EstimatePercent = percent(round(posttest_odds, digit=3)), LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(bayes_ppv, digit=3),nsmall=3),EstimatePercent = percent(round(bayes_ppv, digit=3)), LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(posttest_probability, digit=3),nsmall=3), EstimatePercent = percent(round(posttest_probability, digit=3)), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(likelihoodratio_negativetest, digit=3),nsmall=3), EstimatePercent = percent(round(likelihoodratio_negativetest, digit=3)), LowerCI = format(round(likelihoodratio_negativetest_low, digit=3),nsmall=3), UpperCI = format(round(likelihoodratio_negativetest_upp, digit=3),nsmall=3))
      verim <- rbind(verim,newRow) 
      newRow <- data.frame(Estimate = format(round(bayes_npv, digit=3),nsmall=3),EstimatePercent = percent(round(bayes_npv, digit=3)), LowerCI = 0, UpperCI = 1)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(posttest_probability_neg, digit=3),nsmall=3),EstimatePercent = percent(round(posttest_probability_neg, digit=3)),  LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(inverse_likelihoodratio_negativetest, digit=3),nsmall=3),EstimatePercent = percent(round(inverse_likelihoodratio_negativetest, digit=3)),  LowerCI = format(round(inverse_likelihoodratio_negativetest_low, digit=3),nsmall=3), UpperCI = format(round(inverse_likelihoodratio_negativetest_upp, digit=3),nsmall=3))
      verim <- rbind(verim,newRow)  
      newRow<- data.frame(Estimate = format(round(odds_ratio, digit=3),nsmall=3),EstimatePercent = percent(round(odds_ratio, digit=3)), LowerCI = format(round(odds_ratio_low, digit=3),nsmall=3), UpperCI = format(round(odds_ratio_upp, digit=3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(odds_ratio_Haldane, digit=3),nsmall=3),EstimatePercent = percent(round(odds_ratio_Haldane, digit=3)),  LowerCI = format(round(odds_ratio_Haldane_low, digit=3),nsmall=3), UpperCI = format(round(odds_ratio_Haldane_upp, digit=3),nsmall=3))
      verim <- rbind(verim,newRow) 
      newRow <- data.frame(Estimate = format(round(error_odds_ratio, digit=3),nsmall=3),EstimatePercent = percent(round(error_odds_ratio, digit=3)), LowerCI = 0, UpperCI = 0)
      verim <- rbind(verim,newRow)
      newRow <- data.frame(Estimate = format(round(rate_ratio, digit=3),nsmall=3),EstimatePercent = percent(round(rate_ratio, digit=3)), LowerCI = format(round(rate_ratio_low, digit=3),nsmall=3), UpperCI = format(round(rate_ratio_upp, digit=3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow<- data.frame(Estimate = format(round(relative_risk, digit=3),nsmall=3),EstimatePercent = percent(round(relative_risk, digit=3)), LowerCI = format(round(relative_risk_low, digit=3),nsmall=3), UpperCI = format(round(relative_risk_upp, digit=3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow<- data.frame(Estimate = format(round(risk_difference, digit=3),nsmall=3),EstimatePercent = percent(round(risk_difference, digit=3)), LowerCI = format(round(risk_difference_low, digit=3),nsmall=3), UpperCI = format(round(risk_difference_upp, digit=3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow<- data.frame(Estimate = format(round(attributable_risk, digit=3),nsmall=3),EstimatePercent = percent(round(attributable_risk, digit=3)), LowerCI = format(round(attributable_risk_low, digit=3),nsmall=3), UpperCI = format(round(attributable_risk_upp, digit=3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow<- data.frame(Estimate = NA,EstimatePercent = percent(round(attributable_risk_percent, digit=3)), LowerCI = format(round(attributable_risk_percent_low, digit=3),nsmall=3), UpperCI = format(round(attributable_risk_percent_upp, digit=3),nsmall=3))
      verim <- rbind(verim,newRow)
      newRow<- data.frame(Estimate = format(round(population_attributable_risk, digit=3),nsmall=3),EstimatePercent = percent(round(population_attributable_risk, digit=3)), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      newRow<- data.frame(Estimate = NA,EstimatePercent = percent(round(population_attributable_risk_percent, digit=3)), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      
      rownames(verim) = c("Risk in Exposed","Risk in Unexposed","Overall Risk", "Prevalence (Pre-test probability - Risk)", "Detection Rate", "Detection Prevalence", "Difference of Proportion", 
                          "Control Event Rate","Experimental Event Rate", "Number Needed to Treat (NNT)", "Absolute Risk Reduction (ARR)", 
      "Relative Risk Reduction (RRR)", "Test level","Pre-test Odds","Likelihood ratio of positive test (LR+)",
      "Post-test Odds", 
      "The Probabality of Disease Given a Positive Test (PPV with Bayes Formula)",
      "Post-test Probability (Positive Test Result)","Likelihood ratio of negative test (LR-)", "The Probabality of No Disease Given a Negative Test (NPV with Bayes Formula)",
      "Post-test Probability (Negative Test Result)","Inverse of the likelihood ratio of negative test", "Odds ratio (OR)", "Odds ratio (Haldanes_estimator)", "Error Odds Ratio",
      "Rate Ratio", "Relative Risk (Risk Ratio - RR)","Risk Difference", "Attributable Risk (Attributable Proportion, Attributable Fraction, AR)",
      "Attributable Risk Percent (AR%)", "Population Attributable Risk (PAR)", "Population Attributable Risk Percent (PAR%)")
      return(verim)
      
    }
  })
 
  my_data5 <- reactive({
    testdata <- dataM5()
    as.data.frame(cbind(Interpretation = shinyInput(actionButton, nrow(testdata),'button5_', label = "Interpretation", onclick = 'Shiny.onInputChange(\"select_button5\",  this.id)' ),testdata))
  })  
  output$dataUpload5 <- DT::renderDataTable(my_data5(),selection = 'single', extensions = c('Buttons','KeyTable', 'Responsive'),
                                            options = list(pageLength = 25,lengthMenu = c(25, 50, 75, 100),
                                                           dom = 'Bfrtip',
                                                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),keys = TRUE,
                                                           preDrawCallback = JS("function() { 
                                                                                Shiny.unbindAll(this.api().table().node()); }"), 
                                                           drawCallback = JS("function() { 
                                                                             Shiny.bindAll(this.api().table().node()); } ")),
                                            server = FALSE, escape = FALSE)
  
  
  # Here I created a reactive to save which row was clicked which can be stored for further analysis
  SelectedRow5 <- eventReactive(input$select_button5,{
    as.numeric(strsplit(input$select_button5, "_")[[1]][2])
  })
  
  # This is needed so that the button is clicked once for modal to show, a bug reported here
  # https://github.com/ebailey78/shinyBS/issues/57
  observeEvent(input$select_button5, {
    toggleModal(session, "modalExample5", "open")
  })
  
  DataRow5 <- eventReactive(input$select_button5,{
    my_data5()[SelectedRow5(),2:ncol(my_data5())]
  })

  output$popup5 <- renderUI({
    if (rownames(DataRow5()) == 'Prevalence (Pre-test probability - Risk)') 
    {
      bsModal("modalExample5", paste0("Prevalence: "), "", size = "large",
              h4("Description:"),
              print(paste0("Prevalence is the ratio of the number of people who are truly sick to the total number of people in the study." )),
              h4("Interpretation:"),
              h5(DataRow5()$EstimatePercent, " of the people who in the study are really sick."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow5()),
              column(12,                   
                     DT::renderDataTable(DataRow5())
                     
              )
                   
              
      )}

    else if (rownames(DataRow5()) == 'Detection Rate') 
    {
      bsModal("modalExample5", paste0("Detection Rate"), "", size = "large",
              h4("Description:"),
              print(paste0("The detection rate is the rate of true events also predicted to be events.")),
              h4("Interpretation:"),
              h5(DataRow5()$EstimatePercent, " of the people who in the study are really sick. "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow5()),
              column(12,                   
                     DT::renderDataTable(DataRow5())
                     
              )
              
              
      )}

    else if (rownames(DataRow5()) == 'Detection Prevalence') 
    {
      bsModal("modalExample5", paste0("Detection Prevalence"), "", size = "large",
              h4("Description:"),
              print(paste0("The detection prevalence is the prevalence of predicted events.")),
      h4("Interpretation:"),
      h5(" "),
      h5("**************************************************************************************************************************************************"),
      h5("Data for Row Number:",SelectedRow5()),
      column(12,                   
             DT::renderDataTable(DataRow5())
             
      )
      
      
      )}
    ## lr+
    ## https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4975285/
    

    else if (rownames(DataRow5()) == 'Likelihood ratio of positive test (LR+)') 
    {
      bsModal("modalExample5", paste0("Likelihood ratio of positive test (LR+)"), "", size = "large",
              h4("Description:"),
              print(paste0("The Likelihood ratio is the ratio of the test result expected to those who have the disease to those without the disease. That is, a test result indicates how many times the subjects with the disease have the disease compared to the subjects without the disease. When the likelihood ratio is equal to 1, both probabilities (test results in subjects with the disease and test results in subjects without the disease) are equal.
Likelihood ratios for positive test (LR +) show how much more positive the test result will be in patient subjects than in non-patient subjects. This rate is usually greater than 1. The higher the LR +, the greater the test-disease rate.
                           ")),
              h4("Interpretation:"),
              h5("The positive likelihood ratio of the test is ", DataRow5()$Estimate, " times more likely among people with real sick than 
                           among people without real sick.  "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow5()),
              column(12,                   
                     DT::renderDataTable(DataRow5())
                     
              )
              
              
      )}

    else if (rownames(DataRow5()) == 'The Probabality of Disease Given a Positive Test (PPV with Bayes Formula)') 
    {
      bsModal("modalExample5", paste0("The Probability of Disease Given a Positive Test (PPV with Bayes Formula)"), "", size = "large",
              h4("Description:"),
              print(paste0("Sensitivity and specificity are easy to evaluate by a case-control study but predictivity requires that the subjects be followed up 
till such time that their disease status is confirmed as present or absent. This could be very time consuming and expensive. 
Thus predictivities are difficult to evaluate. Luckily there is a statistical procedure, called Bayes 3 rule, which helps to get one 
from the other, provided some ancillary information is available. The significance of Bayes rule: High fever, rigors, spleenomegaly, 
and presence of parasite in the blood are the stages that progressively confirm malaria. As the information increases, the diagnosis becomes 
focused, and the probability of absence or presence of the disease firms up. The probability depends on what information is already available. 
The chance part is the uncovered information. The probability of any event without availability of any information is called prior probability 
and the probability after some information is available is called posterior probability. The latter obviously depends on the kind of information 
available to alter the probability. The function of Bayes Rule is to convert one posterior probability to its directional inverse. 
It converts probability of A given B to probability of B given A. This is useful to convert sensitivity of a test to its predictivity. 
The former is P(T+|D+) and the latter is P(D+|T+). Such conversion is a great convenience when one probability is already available or can be
easily obtained, and its inverse is otherwise difficult to obtain directly.")),
              h4("Interpretation:"),
              h5("Among those diagnosed with the test, we can say that those who confirmed the diagnosis were ", DataRow5()$Estimate, " times more than those who were not. 
                 The test yields 1 false positive for every ", DataRow5()$Estimate, " correct positives."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow5()),
              column(12,                   
                     DT::renderDataTable(DataRow5())
                     
              )
              
      )}

    else if (rownames(DataRow5()) == 'Post-test Probability (Positive Test Result)') 
    {
      bsModal("modalExample5", paste0("Post-test Probability (Positive Test Result)"), "", size = "large",
              h4("Description:"),
              print(paste0("If the test result is positive for any patient in the study, the pre-test prediction 
                           (prevalence) of the likelihood of being actually ill can be revised up to ", DataRow5()$Estimate, ".")),
              h4("Interpretation:"),
              h5(" "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow5()),
              column(12,                   
                     DT::renderDataTable(DataRow5())
                     
              )

      )}

    else if (rownames(DataRow5()) == 'Likelihood ratio of negative test (LR-)') 
    {
      bsModal("modalExample5", paste0("Likelihood ratio of negative test (LR-): "), "", size = "large",
              h4("Description:"),
              print(paste0("The Likelihood ratio is the ratio of the test result expected to those who have the disease to those without the disease. That is, a test result indicates how many times the subjects with the disease have the disease compared to the subjects without the disease. When the likelihood ratio is equal to 1, both probabilities (test results in subjects with the disease and test results in subjects without the disease) are equal.
Likelihood ratio for negative test (LR-) is the ratio of the likelihood of a negative outcome in a patient to the likelihood of a negative outcome in non-ill subjects. This rate is usually less than 1.
                           ")),
              h4("Interpretation:"),
              h5("The negative likelihood ratio of the test is ", DataRow5()$Estimate, " times likely among people with real sick than 
                 among people without real sick.  "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow5()),
              column(12,                   
                     DT::renderDataTable(DataRow5())
                     
              )
  
              
      )}

    else if (rownames(DataRow5()) == 'The Probabality of No Disease Given a Negative Test (NPV with Bayes Formula)') 
    {
      bsModal("modalExample5", paste0("The Probability of Disease Given a Negative Test (NPV with Bayes Formula) "), "", size = "large",
              h4("Description:"),
              print(paste0("The negative test-out likelihood ratio is the accuracy rate of a robust diagnosis. 
                           The smaller this rate is, the better the true robusFPess can be distinguished.")),
              h4("Interpretation:"),
              h5(" "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow5()),
              column(12,                   
                     DT::renderDataTable(DataRow5())
                     
              )
  
              
      )}

    ## http://www.sciencedirect.com/science/article/pii/S1836955316300583
    else if (rownames(DataRow5()) == 'Post-test Probability (Negative Test Result)') 
    {
      bsModal("modalExample5", paste0("Post-test Probability (Negative Test Result) "), "", size = "large",
              h4("Description:"),
              print(paste0("If the test result is negative for any patient in the study, the pre-test prediction 
                           (prevalence) of the likelihood of being actually ill can be revised up to ", DataRow5()$Estimate, ".")),
              h4("Interpretation:"),
              h5(" "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow5()),
              column(12,                   
                     DT::renderDataTable(DataRow5())
                     
              )
    
              
      )}

    ## http://www.openaccess.hacettepe.edu.tr:8080/xmlui/bitstream/handle/11655/995/debea6f0-303a-48e0-a740-bd9ed110ea7a.pdf?sequence=1&isAllowed=y
    else if (rownames(DataRow5()) == 'Odds Ratio (RT)') 
    {
      bsModal("modalExample5", paste0("Odds Ratio (RT)"), "", size = "large",
              h4("Description:"),
              print(paste0("OR is the ratio of the odds ratio of non-ill subjects to the odds ratio of non-ill subjects. DOR depends on the sensitivity and specificity of the test. 
OR is elevated when sensitivity and specificity are high, false positive and false negative rate is low.In real patients, the proportion of those who are positive for the diagnostic test is called the 
                           -odds of the diagnostic test in patients who are actually in the patient- to the negative ones.In real patients,
the proportion of those who are positive for the diagnosis test is called the 'odds of the diagnostic test in non-ill patients' 
in the ratio to the negative ones. The ratio of these two odds values is called the odds ratio and this rate can determine how much 
the diagnostic test is showing the disease. 1, it can be said that the diagnostic value of the test is absent. 
If the odds ratio is> 1, the positive test result
it is thought to be more likely to happen. If the odds ratio is> 1, the positive test result is considered more likely to 
be present in patients.")),
              h4("Interpretation:"),
              h5(" "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow5()),
              column(12,                   
                     DT::renderDataTable(DataRow5())
                     
              )
              
              
      )}
    ## relative risk
    ## http://sphweb.bumc.bu.edu/otlt/mph-modules/ep/ep713_association/ep713_association_print.html

    else if (rownames(DataRow5()) == 'Relative Risk (Risk Ratio - RR)') 
    {
      bsModal("modalExample5", paste0("Relative Risk (Risk Ratio - RR)"), "", size = "large",
              h4("Description:"),
              print(paste0("The relative risk (or risk ratio) is an intuitive way to compare the risks for the two groups. 
                           Simply divide the cumulative incidence in exposed group by the cumulative incidence in the unexposed group. 
If the risk ratio is 1 (or close to 1), it suggests no difference or little difference in risk (incidence in each group is the same).
A risk ratio > 1 suggests an increased risk of that outcome in the exposed group.
                           A risk ratio < 1 suggests a reduced risk in the exposed group.")),
              h4("Interpretation:"),
              h5("Subjects with positive test results are ", DataRow5()$Estimate ," times more risky than those who have negative test results.  "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow5()),
              column(12,                   
                     DT::renderDataTable(DataRow5())
                     
              )
              
              
      )}

    else if (rownames(DataRow5()) == 'Attributable Risk (Attributable Proportion, Attributable Fraction, AR)') 
    {
      bsModal("modalExample5", paste0("Attributable Risk (Attributable Proportion, Attributable Fraction, AR)"), "", size = "large",
              h4("Description:"),
              print(paste0("AR is the portion of the incidence of a disease in
the exposed that is due to the exposure. It is the incidence of a disease
                           in the exposed that would be eliminated if exposure were eliminated.Bak:http://www.collegeboard.com/prod_downloads/yes/4297_MODULE_17.pdf")),
              h4("Interpretation:"),
              h5("If the subjects with a positive test result result in a change in this situation and 
              the test results are negative, the true positivity will decrease by ", DataRow5()$Estimate ," per 100 people (AR)."),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow5()),
              column(12,                   
                     DT::renderDataTable(DataRow5())
                     
              )
              
              
      )}

    else if (rownames(DataRow5()) == 'Attributable Risk Percent (AR%)') 
    {
      bsModal("modalExample5", paste0("Attributable Risk Percent (AR%)"), "", size = "large",
              h4("Description:"),
              print(paste0("AR% is the percent of the incidence
of a disease in the exposed that is due to the exposure. It is the
                           percent of the incidence of a disease in the exposed that would be eliminated
                           if exposure were eliminated.Bak:http://www.collegeboard.com/prod_downloads/yes/4297_MODULE_17.pdf")),
              h4("Interpretation:"),
              h5("if the subjects with a positive test result are able to change this and make the test results negative, 
                 this will reduce the true positivity by ", DataRow5()$EstimatePercent ," (AR%). "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow5()),
              column(12,                   
                     DT::renderDataTable(DataRow5())
                     
              )
              
              
      )}

    else if (rownames(DataRow5()) == 'Population Attributable Risk (PAR)') 
    {
      bsModal("modalExample5", paste0("Population Attributable Risk (PAR)"), "", size = "large",
              h4("Description:"),
              print(paste0("PAR is the portion of the incidence
of a disease in the population (exposed and nonexposed) that is
                           due to exposure. It is the incidence of a disease in the population that
                           would be eliminated if exposure were eliminated.
                           Bak:http://www.collegeboard.com/prod_downloads/yes/4297_MODULE_17.pdf")),
              h4("Interpretation:"),
              h5("If everyone makes a negative test result in the diagnostic test, it is 
                 expected to decrease the positivity of ", DataRow5()$Estimate ," (PAR) new real situation in 100 people. "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow5()),
              column(12,                   
                     DT::renderDataTable(DataRow5())
                     
              )
              
              
      )}

    else if (rownames(DataRow5()) == 'Population Attributable Risk Percent (PAR%)') 
    {
      bsModal("modalExample5", paste0("Population Attributable Risk Percent (PAR%)"), "", size = "large",
              h4("Description:"),
              print(paste0("PAR% is the percent of
the incidence of a disease in the population (exposed and nonexposed)
                           that is due to exposure. It is the percent of the incidence of a disease
                           in the population that would be eliminated if exposure were eliminated.Bak:http://www.collegeboard.com/prod_downloads/yes/4297_MODULE_17.pdf")),
              h4("Interpretation:"),
              h5("If everyone makes a negative test result in the diagnostic test, this decrease represents 
                 a decrease of ", DataRow5()$EstimatePercent ," (PAR%) of population incidence "),
              h5("**************************************************************************************************************************************************"),
              h5("Data for Row Number:",SelectedRow5()),
              column(12,                   
                     DT::renderDataTable(DataRow5())
                     
              )
              
              
      )}
    else
    {
    bsModal("modalExample5", paste0("Interpretation: "), "", size = "large",
            print(paste0(rownames(DataRow5())," estimate is ", DataRow5()$Estimate," and its confidence interval is ", DataRow5()$LowerCI," - ",DataRow5()$UpperCI," . ")),
            h5("**************************************************************************************************************************************************"),
            h5("Data for Row Number:",SelectedRow5()),
            column(12,                   
                   DT::renderDataTable(DataRow5())
                   
            )
            
            
    )
    }
  })
  

  output$plot5 <- highcharter::renderHighchart({
    if (input$verigir_kontrol == TRUE){ 
      TP = 0
      FN = 0
      FP = 0
      TN = 0
      for (i in 1:nrow(tabloveri()))
      {
        if (tabloveri()[i,1]==1 & tabloveri()[i,2]==1)
        {TP = TP+1}
        else if (tabloveri()[i,1]==1 & tabloveri()[i,2]==2)
        {FN = FN+1}
        else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==1)
        {FP = FP+1}
        else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==2)
        {TN = TN+1}
        next
      }
      CI = as.numeric(trimws(input$text5_2))
    }
    else{
      TP = as.numeric(trimws(input$text1)) ### x
      FN = as.numeric(trimws(input$text2)) ### y
      FP = as.numeric(trimws(input$text3)) ### z
      TN = as.numeric(trimws(input$text4)) ### t
      CI = as.numeric(trimws(input$text5))
    }
    
    
    satirtop1 = TP + FN
    satirtop2 = FP + TN
    sutuntop1 = TP + FP
    sutuntop2 = FN + TN
    toplam = satirtop1 + satirtop2
    #table1 <- matrix(c(TP,FN,satirtop1,FP,TN,satirtop2,sutuntop1,sutuntop2,toplam),nrow=3,ncol=3,byrow=TRUE,dimnames=list(c("1.satir","2.satir","Sutun toplamlari"),c("1.kolon","2.kolon","Satir toplamlari")))
    #table1
    ## Duyarlilik 
    sensitivity_estimate = TP / satirtop1
    sensitivity_se = round((sqrt(sensitivity_estimate * (1 - sensitivity_estimate)/satirtop1)), 4)
    sensitivity_low = 1 - Rbeta.inv(((1 + CI/100)/2), (satirtop1+1-TP), TP)
    sensitivity_upp = Rbeta.inv(((1 + CI/100)/2), (TP+1), (satirtop1-TP))
    sensitivity_randomtest = sutuntop1 / toplam
    qualityindex_sensitivity = (sensitivity_estimate - sensitivity_randomtest) / (1 - sensitivity_randomtest)
    ## Secicilik
    specificity = TN / satirtop2
    specificity_se = sqrt(specificity * (1 - specificity) / satirtop2)
    specificity_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop2+1-TN),TN)
    specificity_upp = Rbeta.inv((1 + CI/100)/2, (TN+1), (satirtop2-TN))
    specificity_randomtest = (1 - sensitivity_randomtest)
    qualityindex_specificity = (specificity - 1 + sensitivity_randomtest) / sensitivity_randomtest
    ## Gain in Certainty
    gain_in_certainty = sensitivity_estimate + specificity
    ## Etkililik, yeterlilik ayni zamanda overall accuracy (toplam-genel dogruluk orani)
    efficiency = (TP + TN) / toplam
    efficiency_se = sqrt(efficiency * (1 - efficiency) / toplam)
    efficiency_low = 1-Rbeta.inv((1 + CI/100)/2, (toplam+1-TP-TN), (TP+TN))
    efficiency_upp = Rbeta.inv((1 + CI/100)/2, (TP+TN+1), (toplam-TP-TN))
    efficiency_randomtest = (satirtop1 / toplam) * sensitivity_randomtest + (1 - (satirtop1 / toplam)) * (1 - sensitivity_randomtest)
    mis_efficiency = 1 - efficiency
    ## Quality indeks
    quality_index = (efficiency - efficiency_randomtest) / (1 - efficiency_randomtest)
    quality_index_se = sqrt((efficiency*(1-efficiency)/(1-efficiency_randomtest)^2+2*(1-efficiency)*(2*efficiency*efficiency_randomtest-((TP*((satirtop1 / toplam)+sensitivity_randomtest)+TN*(2-(satirtop1 / toplam)-sensitivity_randomtest))/toplam))/(1-efficiency_randomtest)^3+((1-efficiency)^2)*((TP*((satirtop1 / toplam)+sensitivity_randomtest)^2+FN*(-(satirtop1 / toplam)+1+sensitivity_randomtest)^2+FP*(1+(satirtop1 / toplam)-sensitivity_randomtest)^2+TN*(2-(satirtop1 / toplam)-sensitivity_randomtest)^2)/toplam-4*efficiency_randomtest^2)/(1-efficiency_randomtest)^4)/toplam)
    quality_index_low = quality_index + qnorm((1 - CI/100)/2)*quality_index_se
    quality_index_upp = quality_index - qnorm((1 - CI/100)/2)*quality_index_se
    ## Youden indeksi
    youdens_index = sensitivity_estimate + specificity - 1
    youdens_index_se = sqrt(sensitivity_se^2+specificity_se^2)
    youdens_index_low = youdens_index + qnorm((1 - CI/100)/2)*youdens_index_se
    youdens_index_upp = youdens_index - qnorm((1 - CI/100)/2)*youdens_index_se
    
    number_needed_to_diagnose = 1 / youdens_index
    number_needed_to_misdiagnose = 1 / ( 1 - efficiency ) 
    ## Pozitif kestirim degeri
    predictivevalue_positivetest = TP / sutuntop1
    predictivevalue_positivetest_se = sqrt(predictivevalue_positivetest*(1-predictivevalue_positivetest)/sutuntop1)
    predictivevalue_positivetest_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop1+1-TP),TP)
    predictivevalue_positivetest_upp = Rbeta.inv((1 + CI/100)/2, (TP+1),(sutuntop1-TP))
    predvalue_positiverandomtest = (satirtop1 / toplam)
    predvalue_positiverandomtest_mar_ol = predvalue_positiverandomtest*sensitivity_estimate+(1-predvalue_positiverandomtest)*(1-specificity)
    ## Negatif kestirim degeri
    predictivevalue_negativetest = TN / sutuntop2
    predictivevalue_negativetest_se = sqrt(predictivevalue_negativetest*(1-predictivevalue_negativetest)/sutuntop2)
    predictivevalue_negativetest_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop2+1-TN),TN)
    predictivevalue_negativetest_upp = Rbeta.inv((1 + CI/100)/2, (TN+1), (sutuntop2-TN))
    predvalue_negativerandomtest = 1 - (satirtop1 / toplam)
    predvalue_negativerandomtest_mar_ol = 1 - predvalue_positiverandomtest_mar_ol
    ## Predictive Summary Index (PSI)
    predictive_summary_index = predictivevalue_positivetest + predictivevalue_negativetest - 1
    ## Yanlis pozitif orani
    false_positiverate = FP / satirtop2
    false_positiverate_se = sqrt(false_positiverate*(1-false_positiverate)/satirtop2)
    false_positiverate_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop2+1-FP),FP)
    false_positiverate_upp = Rbeta.inv((1 + CI/100)/2, (FP+1), (satirtop2-FP))
    ## Yansis negatif orani
    false_negativerate = FN / satirtop1
    false_negativerate_se = sqrt(false_negativerate*(1-false_negativerate)/satirtop1)
    false_negativerate_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop1+1-FN),FN)
    false_negativerate_upp = Rbeta.inv((1 + CI/100)/2, (FN+1), (satirtop1-FN))
    ## False Omission Rate (FOR) https://www.ncss.com/software/ncss/diagnostic-tests-in-ncss/
    false_omission_rate = FN/(sutuntop2)
    false_omission_rate_se = sqrt(false_omission_rate*(1-false_omission_rate)/sutuntop2)
    false_omission_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop2+1-FN),FN)
    false_omission_rate_upp = Rbeta.inv((1 + CI/100)/2, (FN+1), (sutuntop2-FN))
    ## False Discovery Rate (FDR) https://www.ncss.com/software/ncss/diagnostic-tests-in-ncss/
    false_discovery_rate = FP/(sutuntop1)
    false_discovery_rate_se = sqrt(false_discovery_rate*(1-false_discovery_rate)/sutuntop1)
    false_discovery_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop1+1-FP),FP)
    false_discovery_rate_upp = Rbeta.inv((1 + CI/100)/2, (FP+1), (sutuntop1-FP))
    ## Dogru pozitif orani
    true_positiverate = TP / (satirtop1)
    ## Yanlis siniflandirma orani
    misclassification_rate = (FN + FP) / toplam
    misclassification_rate_se = sqrt(misclassification_rate*(1-misclassification_rate)/toplam)
    misclassification_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-FP-FN), (FP+FN))
    misclassification_rate_upp = Rbeta.inv((1 + CI/100)/2, (FP+FN+1), (toplam-FP-FN))
    ## Prevelans
    prevalence = (satirtop1 / toplam)
    prevalence_se = sqrt((satirtop1 / toplam)*(1-(satirtop1 / toplam))/toplam)
    prevalence_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-satirtop1), satirtop1)
    prevalence_upp = Rbeta.inv((1 + CI/100)/2, (satirtop1+1), (toplam-satirtop1))
    
    ##caret paketinden olculer
    detection_rate = TP/toplam
    detection_prevalence = (TP+FP)/toplam
    balanced_accuracy = (sensitivity_estimate + specificity)/2
    lift = (TP/(TP+FN))/((TP+FP)/toplam)  ## https://gerardnico.com/wiki/data_mining/error_rate
    precision = TP/(TP+FP)
    recall = TP/(TP+FN)
    ## http://michael.hahsler.net/research/recomm_lnai2002/lnai2002.pdf
    conviction = (1-((TP+FP)/toplam))/(1-(TP/(TP+FN)))
    matthews_correlation_coefficient = (TP * TN + FN * FP) / ((TP+FP)*(FN+TN)*(FP+TN)*(TP+FN))^(1/2)
    ## Goreli Risk Orani (Relative risk)
    ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
    # https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20ratio.htm
    relative_risk = (TP/sutuntop1)/(FN/sutuntop2)
    relative_risk_se = sqrt((1/TP)+(1/FN)-(1/(TP+FP))-(1/(FN+TN)))
    relative_risk_low = exp(log(relative_risk)+qnorm((1-CI/100)/2)*relative_risk_se)
    relative_risk_upp = exp(log(relative_risk)-qnorm((1-CI/100)/2)*relative_risk_se)
    difference_in_proportion = (TP/sutuntop1) - (FN/sutuntop2)
    ## Leverage
    ## https://www3.nd.edu/~tweninge/pubs/AHBCW_ANNIE08.pdf
    leverage = (TP/sutuntop1)-((TP+FP)/toplam)
    ## http://www.ebm.med.ualberta.ca/TherapyCalc.html
    control_event_rate = FN/sutuntop2
    experimental_event_rate = TP/sutuntop1
    ## Absolute risk reduction
    absolute_risk_reduction = -difference_in_proportion
    absolute_risk_reduction_se = sqrt((TP/sutuntop1)*(1-(TP/sutuntop1))/sutuntop1+(FN/sutuntop2)*(1-(FN/sutuntop2))/sutuntop2)
    absolute_risk_reduction_low = absolute_risk_reduction - qnorm((1-CI/100)/2)*absolute_risk_reduction_se
    absolute_risk_reduction_upp = absolute_risk_reduction + qnorm((1-CI/100)/2)*absolute_risk_reduction_se
    ## Relative risk reduction
    relative_risk_reduction = absolute_risk_reduction / (FN/sutuntop2)
    relative_risk_reduction_low = 1 - exp(log(relative_risk)-qnorm((1-CI/100)/2)*relative_risk_se)
    relative_risk_reduction_upp = 1 - exp(log(relative_risk)+qnorm((1-CI/100)/2)*relative_risk_se)
    ##Number needed to treat
    number_needed_to_treat = 1 / abs(difference_in_proportion)
    number_needed_to_treat_low = 1 /absolute_risk_reduction_upp
    number_needed_to_treat_upp = 1 /absolute_risk_reduction_low
    ## Test duzeyi
    test_level = sensitivity_randomtest
    test_level_se = sqrt(test_level*(1-test_level)/toplam)
    test_level_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-sutuntop1),sutuntop1)
    test_level_upp = Rbeta.inv((1 + CI/100)/2, (sutuntop1+1), (toplam-sutuntop1))
    ## Pre-test odds
    pretest_odds = prevalence / (1 - prevalence)
    ## Pozitif Olabilirlik Orani
    likelihoodratio_positivetest = sensitivity_estimate/(1-specificity)
    likelihoodratio_positivetest_se = exp(sqrt((1-sensitivity_estimate)/(sensitivity_estimate*satirtop1) +specificity/((1-specificity)*satirtop2)))
    likelihoodratio_positivetest_low = exp(log(likelihoodratio_positivetest) + qnorm((1 - CI/100)/2)*log(likelihoodratio_positivetest_se))
    likelihoodratio_positivetest_upp = exp(log(likelihoodratio_positivetest) - qnorm((1 - CI/100)/2)*log(likelihoodratio_positivetest_se))
    ## post-test odds
    posttest_odds = pretest_odds * likelihoodratio_positivetest
    bayes_ppv = (sensitivity_estimate * prevalence)/((sensitivity_estimate * prevalence) + (1 - specificity) * (1 - prevalence))
    ## Post-test probability (positive test result)
    posttest_probability = (pretest_odds * likelihoodratio_positivetest) / (1 + (pretest_odds * likelihoodratio_positivetest))
    ## Negatif Olabilirlik Orani
    likelihoodratio_negativetest = (1-sensitivity_estimate)/specificity
    likelihoodratio_negativetest_se = exp(sqrt(sensitivity_estimate/((1-sensitivity_estimate)*satirtop1) +(1-specificity)/(specificity*satirtop2)))
    likelihoodratio_negativetest_low = exp(log(likelihoodratio_negativetest) + qnorm((1-CI/100)/2)*log(likelihoodratio_negativetest_se))
    likelihoodratio_negativetest_upp = exp(log(likelihoodratio_negativetest) - qnorm((1-CI/100)/2)*log(likelihoodratio_negativetest_se))
    bayes_npv = (sensitivity_estimate * (1 - prevalence))/((sensitivity_estimate * (1 - prevalence)) + (1 - specificity) * prevalence)
    ## Post-test probability (negative test result)
    posttest_probability_neg = (pretest_odds * likelihoodratio_negativetest) / (1 + (pretest_odds * likelihoodratio_negativetest))
    ## Ters negatif olabilirlik orani
    inverse_likelihoodratio_negativetest = specificity/(1-sensitivity_estimate)
    inverse_likelihoodratio_negativetest_se = exp(sqrt(sensitivity_estimate/((1-sensitivity_estimate)*satirtop1) +(1-specificity)/(specificity*satirtop2)))
    inverse_likelihoodratio_negativetest_low = exp(log(inverse_likelihoodratio_negativetest) + qnorm((1-CI/100)/2)*log(inverse_likelihoodratio_negativetest_se))
    inverse_likelihoodratio_negativetest_upp = exp(log(inverse_likelihoodratio_negativetest) - qnorm((1-CI/100)/2)*log(inverse_likelihoodratio_negativetest_se))
    ## Diagnostic Odds Ratio (Tani Odds Orani)
    odds_ratio = (TP/FN)/(FP/TN)
    odds_ratio_se = exp(sqrt(1/TP+1/FP+1/TN+1/FN))
    odds_ratio_low = exp(log(odds_ratio) + qnorm((1-CI/100)/2)*log(odds_ratio_se))
    odds_ratio_upp = exp(log(odds_ratio) - qnorm((1-CI/100)/2)*log(odds_ratio_se))
    odds_ratio_Haldane = ((TP+0.5)/(FN+0.5))/((FP+0.5)/(TN+0.5))
    odds_ratio_Haldane_se = exp(sqrt(1/(TP+0.5)+1/(FP+0.5)+1/(TN+0.5)+1/(FN+0.5)))
    odds_ratio_Haldane_low = exp(log(odds_ratio_Haldane) + qnorm((1-CI/100)/2)*log(odds_ratio_Haldane_se))
    odds_ratio_Haldane_upp = exp(log(odds_ratio_Haldane) - qnorm((1-CI/100)/2)*log(odds_ratio_Haldane_se))
    error_odds_ratio = (sensitivity_estimate/(1-sensitivity_estimate))/(specificity/(1-specificity))
    ## Rate ratio
    ## http://omarkasule-05.tripod.com/id52.html
    ## http://sphweb.bumc.bu.edu/otlt/mph-modules/ep/ep713_randomerror/ep713_randomerror4.html
    rate_ratio = (TP/sutuntop1)/(TP/sutuntop2)
    rate_ratio_low = exp(log(relative_risk)+qnorm((1-CI/100)/2)*sqrt(1/TP+1/FP))
    rate_ratio_upp = exp(log(relative_risk)-qnorm((1-CI/100)/2)*sqrt(1/TP+1/FP))
    ## Risk Difference
    ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
    risk_difference = (TP/sutuntop1) - (FN/sutuntop2)
    risk_difference_se = (((TP/sutuntop1)*(1-(TP/sutuntop1)))/sutuntop1)+(((FN/sutuntop2)*(1-(FN/sutuntop2)))/sutuntop2)
    risk_difference_low = risk_difference + qnorm((1-CI/100)/2)*sqrt(risk_difference_se)
    risk_difference_upp = risk_difference - qnorm((1-CI/100)/2)*sqrt(risk_difference_se)
    ## Nitelenebilen risk
    ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
    attributable_risk = (TP/sutuntop1)-(FN/sutuntop2)
    attributable_risk_se = sqrt((satirtop1/toplam)*(1-(satirtop1/toplam))*(1/sutuntop1+1/sutuntop2))
    attributable_risk_low = attributable_risk + qnorm((1-CI/100)/2)*attributable_risk_se
    attributable_risk_upp = attributable_risk - qnorm((1-CI/100)/2)*attributable_risk_se
    ## Attributable risk percent
    ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
    attributable_risk_percent = ((relative_risk-1)/relative_risk)*100
    attributable_risk_percent_low = attributable_risk_percent - (qnorm((1-CI/100)/2)*attributable_risk_se)/attributable_risk
    attributable_risk_percent_upp = attributable_risk_percent + (qnorm((1-CI/100)/2)*attributable_risk_se)/attributable_risk
    ## Population Attributable Risk
    ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
    population_attributable_risk = (satirtop1/toplam)-(FN/sutuntop2)
    ## Population Attributable Risk Percent
    population_attributable_risk_percent = (population_attributable_risk / (satirtop1/toplam))*100
    ## Kohen's Kappa
    cohens_kappa = (efficiency-efficiency_randomtest)/(1-efficiency_randomtest)
    cohens_kappa_se = quality_index_se
    cohens_kappa_low = cohens_kappa + qnorm((1-CI/100)/2)*cohens_kappa_se
    cohens_kappa_upp = cohens_kappa - qnorm((1-CI/100)/2)*cohens_kappa_se
    ## Gozlenen Karar
    observed_agreement = efficiency
    observed_agreement_se = efficiency_se
    observed_agreement_low = efficiency_low
    observed_agreement_upp = efficiency_upp
    ## Risk karari,expected agreement
    chance_agreement = efficiency_randomtest
    ## Pozitif karar
    positive_agreement = 2*TP /(satirtop1+sutuntop1)
    positive_agreement_se = sqrt(4*TP*(FP+FN)*(TP+FP+FN))/(sutuntop1+satirtop1)^2
    positive_agreement_low = positive_agreement + qnorm((1-CI/100)/2)*positive_agreement_se
    positive_agreement_upp = positive_agreement - qnorm((1-CI/100)/2)*positive_agreement_se
    ## Negatif karar
    negative_agreement = 2*TN/(satirtop2+sutuntop2)
    negative_agreement_se = sqrt(4*TN*(FP+FN)*(TN+FP+FN))/(sutuntop2+satirtop2)^2
    negative_agreement_low = negative_agreement + qnorm((1-CI/100)/2)*negative_agreement_se
    negative_agreement_upp = negative_agreement - qnorm((1-CI/100)/2)*negative_agreement_se
    ## Rand index
    ## http://www.cs.pomona.edu/classes/cs158/resources/158-10(EM).pdf
    rand_index = (TP+TN)/(TP+FN+FP+TN)
    ## e-measure
    ## http://michael.hahsler.net/research/recomm_lnai2002/lnai2002.pdf
    e_measure = 1/((((1-CI)/100)*(1-precision))+((1-((1-CI)/100))*(1/recall)))
    ## discriminant power 
    ## https://eva.fing.edu.uy/pluginfile.php/69453/mod_resource/content/1/7633-10048-1-PB.pdf
    discriminant_power = (sqrt(3)/pi)*(log (sensitivity_estimate / (1-sensitivity_estimate)) + log (specificity / (1-specificity)))
    ## F1 Score
    ## http://machinelearningmastery.com/classification-accuracy-is-not-enough-more-performance-measures-you-can-use/
    f1_score = 2*(((TP/(TP+FN))*(TP/(TP+FP)))/((TP/(TP+FN))+(TP/(TP+FP))))
    ## Byrt yanlilik indeksi
    byrt_bias_index = (FN-FP)/toplam
    ## Byrt  asimetrik indeks prevelansi
    byrt_prevalence_asymmetry_index = (TN-TP)/toplam
    ## Yanlilik duzeltmeli kappa
    bias_adjusted_kappa = (efficiency-0.25*((satirtop1+sutuntop1)^2+(satirtop2+sutuntop2)^2)/toplam^2)/(1-0.25*((satirtop1+sutuntop1)^2+(satirtop2+sutuntop2)^2)/toplam^2)
    ## Yanlilik duzeltmeli kappa prevelansi
    prevalence_bias_adjusted_kappa = cohens_kappa+(1-cohens_kappa)*(byrt_prevalence_asymmetry_index^2-byrt_bias_index^2)
    ## Dice'in indeksi (Czekanowski)
    dice_index = positive_agreement
    dice_index_se= positive_agreement_se
    dice_index_low = positive_agreement_low
    dice_index_upp = positive_agreement_upp
    ## Yule's Q
    yules_q = (odds_ratio - 1) / (odds_ratio + 1)
    yules_q_se = 0.5*(1-yules_q^2)*sqrt(1/TP+1/FP+1/TN+1/FN)
    yules_q_low = yules_q + qnorm((1-CI/100)/2)*yules_q_se
    yules_q_upp = yules_q - qnorm((1-CI/100)/2)*yules_q_se
    
    equitable_threatscore = (TP-(sutuntop1*satirtop1/toplam))/(TP+FP+FN-(sutuntop1*satirtop1/toplam))
    ## Phi
    phi = (TP*TN-FN*FP) / sqrt(satirtop1 * satirtop2 * sutuntop1 * sutuntop2)
    phi_se = sqrt((1-phi^2+(phi+0.5*phi^3)*((satirtop1-satirtop2)*(sutuntop1-sutuntop2)/sqrt(satirtop1*satirtop2*sutuntop1*sutuntop2))-0.75*phi^2*((satirtop1-satirtop2)^2/(satirtop1*satirtop2)+(sutuntop1-sutuntop2)^2/(sutuntop1*sutuntop2)))/toplam)
    phi_low = phi + qnorm((1-CI/100)/2)*phi_se
    phi_upp = phi - qnorm((1-CI/100)/2)*phi_se
    ## Cramer V katsayisi
    cramer_v = sqrt((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2))/toplam)
    ## Olaganlik katsayisi
    contingency_coefficient = sqrt((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2))/(((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)))+toplam))
    ## Goodman and Kruskal Gamma 
    goodman_and_kruskal_gamma = (TP*TN-FN*FP)/(TP*TN+FN*FP)
    goodman_and_kruskal_gamma_se = 0.5*(1-yules_q^2)*sqrt(1/TP+1/FP+1/TN+1/FN)
    goodman_and_kruskal_gamma_low = yules_q + qnorm((1-CI/100)/2)*yules_q_se
    goodman_and_kruskal_gamma_upp = yules_q - qnorm((1-CI/100)/2)*yules_q_se
    ## Kendall's tau a
    kendalls_tau_a = (TP*TN-FN*FP)/(toplam*(toplam-1)/2)
    ## Kendall's tau b
    ## https://v8doc.sas.com/sashtml/stat/chap28/sect20.htm
    ## http://slideplayer.com/slide/10766029/
    kendalls_tau_b = (TP*TN-FN*FP)/sqrt((TP*TN+FN*FP+(TP*FN+FP*TN))*(TP*TN+FN*FP+(TP*FP+FN*TN)))
    ## Kendall's tau c
    kendalls_tau_c = ((2*2)*(TP*TN-FN*FP)) / (toplam^2 * (2-1))
    ## Somers'd R|C
    somers_d = (TP*TN-FN*FP)/(TP*TN+FN*FP+(TP*FN+FP*TN))
    ## Somers'd C|R
    somers_d_2 = (TP*TN-FP*FN)/(TP*TN+FP*FN+(TP*FP+FN*TN))
    ## Scoot'un karar indeksi
    scotts_agreement_index = bias_adjusted_kappa
    ## Dort-duzeyli Korelasyon
    tetrachoric_correlation = cos(pi/(1+sqrt(TP*TN/FN/FP)))
    tetrachoric_correlation_se = 0.375*(1-tetrachoric_correlation^2)*sqrt((1/(TP))+(1/(FP))+(1/(TN))+(1/(FN)))
    tetrachoric_correlation_low = tetrachoric_correlation + qnorm((1-CI/100)/2)*tetrachoric_correlation_se
    tetrachoric_correlation_upp = tetrachoric_correlation - qnorm((1-CI/100)/2)*tetrachoric_correlation_se
    ## Goodman kruskal tau katsayisi
    goodman_kruskal_tau = (toplam*((TP^2+FP^2)/sutuntop1+(FN^2+TN^2)/sutuntop2)-satirtop1^2-satirtop2^2)/(toplam^2-satirtop1^2-satirtop2^2)
    goodman_kruskal_tau_se = toplam*sqrt(TP*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop1)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*TP/satirtop1-(TP^2+FN^2)/satirtop1^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+FN*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop2)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*FN/satirtop1-(TP^2+FN^2)/satirtop1^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+FP*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop1)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*FP/satirtop2-(FP^2+TN^2)/satirtop2^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+TN*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop2)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*TN/satirtop2-(FP^2+TN^2)/satirtop2^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2)/(toplam^2-sutuntop1^2-sutuntop2^2)^2
    goodman_kruskal_tau_low = goodman_kruskal_tau + qnorm((1-CI/100)/2)*goodman_kruskal_tau_se
    goodman_kruskal_tau_upp = goodman_kruskal_tau - qnorm((1-CI/100)/2)*goodman_kruskal_tau_se
    ## Simetrik Lambda 
    lambda_symmetric = ((max(TP,FN)+max(FP,TN)+max(TP,FP)+max(FN,TN)-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2))/(2*toplam-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2)))
    if (lambda_symmetric==0 || lambda_symmetric==1)
    {lambda_symmetric_se = 0}
    else
    {
      lambda_symmetric_se = sqrt((TP*((if(TP>FN) 1 else 0) + (if(TP>FP) 1 else 0) + ((if(sutuntop1>sutuntop2) 1 else 0)+ (if(satirtop1>satirtop2) 1 else 0))*(lambda_symmetric-1))^2+FN*( (if(TP>FN) 0 else 1)+ (if(FN>TN) 1 else 0) +( (if(sutuntop1>sutuntop2) 0 else 1) + (if(satirtop1>satirtop2) 1 else 0))*(lambda_symmetric-1))^2+FP*( (if(FP>TN) 1 else 0) + (if(TP>FP) 0 else 1) +( (if(sutuntop1>sutuntop2) 1 else 0) + (if(satirtop1<satirtop2) 1 else 0))*(lambda_symmetric-1))^2+TN*((if(FP>TN) 0 else 1)+ (if(FN>TN) 0 else 1) +((if(sutuntop1>sutuntop2) 0 else 1)+ (if(satirtop1<satirtop2) 1 else 0))*(lambda_symmetric-1))^2-4*toplam*lambda_symmetric^2)/(2*toplam-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2))^2)
    }
    
    lambda_symmetric_low = lambda_symmetric + qnorm((1-CI/100)/2)*lambda_symmetric_se
    lambda_symmetric_upp = lambda_symmetric - qnorm((1-CI/100)/2)*lambda_symmetric_se
    ## Lambda Asymmetric R|C
    lambda_criterion = (max(TP,FP)+max(FN,TN)-max(satirtop1,satirtop2))/(toplam-max(satirtop1,satirtop2))
    
    if (lambda_criterion==0 || lambda_criterion==1)
    {lambda_criterion_se = 0}
    else
    {
      lambda_criterion_se = sqrt((toplam-max(TP,FP)-max(FN,TN))*(max(TP,FP)+max(FN,TN)+max(satirtop1,satirtop2)-2*(if(satirtop1>satirtop2) max(TP,FN) else max(FP,TN)))/(toplam-max(satirtop1,satirtop2))^3)
    }
    
    lambda_criterion_low = lambda_criterion + qnorm((1-CI/100)/2)*lambda_criterion_se
    lambda_criterion_upp = lambda_criterion - qnorm((1-CI/100)/2)*lambda_criterion_se
    ## Lambda Asymmetric C|R
    lambda_criterion_2 = (max(TP,FN)+max(FP,TN)-max(sutuntop1,sutuntop2))/(toplam-max(sutuntop1,sutuntop2))
    
    if (lambda_criterion_2==0 || lambda_criterion_2==1)
    {lambda_criterion_se_2 = 0}
    else
    {
      lambda_criterion_se_2 = sqrt((toplam-max(TP,FN)-max(FP,TN))*(max(TP,FN)+max(FP,TN)+max(sutuntop1,sutuntop2)-2*(if(sutuntop1>sutuntop2) max(TP,FP) else max(FN,TN)))/(toplam-max(sutuntop1,sutuntop2))^3)
    }
    
    lambda_criterion_low_2 = lambda_criterion_2 + qnorm((1-CI/100)/2)*lambda_criterion_se_2
    lambda_criterion_upp_2 = lambda_criterion_2 - qnorm((1-CI/100)/2)*lambda_criterion_se_2
    ## belirsizlik katsayisi (coefficient of uncertainty) simetrik
    uncertainty_coefficient_symmetric = 2*(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam)))/(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))
    uncertainty_coefficient_symmetric_se = 2*sqrt(TP*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop1*sutuntop1/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(TP/toplam))^2+FN*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop1*sutuntop2/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(FN/toplam))^2+FP*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop2*sutuntop1/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(FP/toplam))^2+TN*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop2*sutuntop2/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(TN/toplam))^2)/toplam/(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam)^2
    uncertainty_coefficient_symmetric_low = uncertainty_coefficient_symmetric + qnorm((1-CI/100)/2)*uncertainty_coefficient_symmetric_se
    uncertainty_coefficient_symmetric_upp = uncertainty_coefficient_symmetric - qnorm((1-CI/100)/2)*uncertainty_coefficient_symmetric_se
    ## (coefficient of uncertainty) R|C
    uncertainty_coeff_crit = (satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam)))/(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))
    uncertainty_coeff_crit_se = sqrt(TP*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(TP/sutuntop1)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop1/toplam))^2+FN*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(FN/sutuntop2)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop1/toplam))^2+FP*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(FP/sutuntop1)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop2/toplam))^2+TN*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(TN/sutuntop2)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop2/toplam))^2)/toplam/((satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam)^2
    uncertainty_coeff_crit_low = uncertainty_coeff_crit + qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se
    uncertainty_coeff_crit_upp = uncertainty_coeff_crit - qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se
    ## (coefficient of uncertainty) C|R
    uncertainty_coeff_crit_2 = (sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)+satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)-(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam)))/(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))
    uncertainty_coeff_crit_se_2 = sqrt(TP*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(TP/satirtop1)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop1/toplam))^2+FP*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(FP/satirtop2)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop1/toplam))^2+FN*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(FN/satirtop1)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop2/toplam))^2+TN*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(TN/satirtop2)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop2/toplam))^2)/toplam/((sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)^2
    uncertainty_coeff_crit_low_2 = uncertainty_coeff_crit_2 + qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se_2
    uncertainty_coeff_crit_upp_2 = uncertainty_coeff_crit_2 - qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se_2
    ## Yule s Y (Coefficient of colligation)
    yule_y = (sqrt(TP*TN)-sqrt(FN*FP))/(sqrt(TP*TN)+sqrt(FN*FP))
    ## Pearson ki-kare
    pearson_chi_squ = toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)
    pearson_chi_squ_p = dchi(pearson_chi_squ, df=1)
    ## 
    with_yate_cor_for_pearson = toplam*(abs(TP*TN-FN*FP)-0.5*toplam)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)
    with_yate_cor_p_for_pearson = dchi(with_yate_cor_for_pearson, df=1)
    ## Mantel Haenszel chi-square
    ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
    mantel_haenszel = ((toplam-1)*(TP*TN-FN*FP)^2)/(satirtop1*satirtop2*sutuntop1*sutuntop2)
    mantel_haenszel_p = dchi(mantel_haenszel, df=1)
    ## Olasilik orani
    likelih_ratio = 2*(TP*log(TP/(sutuntop1*satirtop1/toplam))+FN*log(FN/(sutuntop2*satirtop1/toplam))+FP*log(FP/(sutuntop1*satirtop2/toplam))+TN*log(TN/(sutuntop2*satirtop2/toplam)))
    likelih_ratio_p =dchi(likelih_ratio, df=1)
    ## Fisher'in exact testi
    ##fisher_exact_test = (TP-(satirtop1*sutuntop1/toplam))^2/(satirtop1*sutuntop1/toplam) + (FN-(satirtop1*sutuntop2/toplam))^2/(satirtop1*sutuntop2/toplam) + (FP-(satirtop2*sutuntop1/toplam))^2/(satirtop2*sutuntop1/toplam) + (TN-(satirtop2*sutuntop2/toplam))^2/(satirtop2*sutuntop2/toplam)
    fisher_test = (factorial(TP+FN)*factorial(FP+TN)*factorial(TP+FP)*factorial(FN*TN))*(factorial(toplam)*factorial(TP)*factorial(TN)*factorial(FP)*factorial(FN))
    fisher <- c(TP, FN, FP, TN)
    tab <- t(matrix(fisher, nrow=2,ncol=2))
    fisher_exact_test = fisher.test(tab)$p.value
    minimum_exp_fre = min((sutuntop1*satirtop1/toplam):(sutuntop2*satirtop2/toplam))
    cells_exp_fre_5 = (if((sutuntop1*satirtop1/toplam)<5) 1 else 0)+ (if((sutuntop1*satirtop2/toplam)<5) 1 else 0)+ (if((sutuntop2*satirtop1/toplam) <5) 1 else 0)+ (if((sutuntop2*satirtop2/toplam) <5) 1 else 0)
    cells_exp_fre_1 = (if((sutuntop1*satirtop1/toplam) <1) 1 else 0)+ (if((sutuntop1*satirtop2/toplam) <1) 1 else 0)+ (if((sutuntop2*satirtop1/toplam) <1) 1 else 0)+ (if((sutuntop2*satirtop2/toplam) <1) 1 else 0)
    ## Mc Nemar testi
    mcNemar_test = (FP-FN)^2/(FP+FN)
    mcNemar_test_p = dchi(mcNemar_test, df=1)
    with_Yate_cor_for_mcnamer = (abs(FP-FN)-1)^2/(FP+FN)
    with_Yate_cor_p_for_mcnamer = dchi(with_Yate_cor_for_mcnamer, df=1)
    
    ## Belirsizlik (Entropi)
    forbes_NMI = sensitivity_estimate*predvalue_positiverandomtest*log((sensitivity_estimate/predvalue_positiverandomtest_mar_ol),base=2)+
      (1-specificity)*(1-predvalue_positiverandomtest)*log(((1-specificity)/predvalue_positiverandomtest_mar_ol),base=2)+
      (1-sensitivity_estimate)*predvalue_positiverandomtest*log(((1-sensitivity_estimate)/(1-predvalue_positiverandomtest_mar_ol)),base=2)+
      specificity*(1-predvalue_positiverandomtest)*log((specificity/(1-predvalue_positiverandomtest_mar_ol)),base=2)
    ## satir icin entropy (test icin)
    entropy_hr = - ((sutuntop1/toplam) * log(sutuntop1/toplam, base=2) + (sutuntop2/toplam) * log(sutuntop2/toplam, base=2))
    ## sutun icin entropy (hastalik icin)  
    entropy_hc = - ((satirtop1/toplam)*log(satirtop1/toplam, base=2) + (satirtop2/toplam)*log(satirtop2/toplam, base=2))
    ## birlesik entropi (joint entropy)  
    entropy_hrc = - ( (TP/toplam)*log(TP/toplam, base=2) + (FP/toplam)*log(FP/toplam, base=2) + (FN/toplam)*log(FN/toplam, base=2) + (TN/toplam)*log(TN/toplam, base=2))
    ## bilgi icerigi (mutual information)  
    information_r_c = entropy_hr + entropy_hc - entropy_hrc
    ## kosullu entropi (conditional entropy)  
    a = entropy_hrc - entropy_hr
    c = entropy_hrc - entropy_hc
    sim_r_c = information_r_c / (a + information_r_c + c)
    dis_r_c = (a + c) / (a + information_r_c + c)
    ## goreli entropi (relative entropy, kullback-leibler uzakligi)
    ## pozitif test sonucu icin goreli entropi, Relative Improvement Over Chance (RIOC)
    positive_relative_entropy = (TP/sutuntop1)*log(((TP/sutuntop1)/ prevalence), base=2)+(FP/sutuntop1)*log(((FP/sutuntop1)/(1- prevalence)), base=2)
    ## negatif test sonucu icin goreli entropi 
    negative_relative_entropy = (FN/sutuntop2)*log(((FN/sutuntop2)/ prevalence), base=2)+(TN/sutuntop2)*log(((TN/sutuntop2)/(1- prevalence)), base=2)
    
    verim <- data.frame(#Estimate=integer(),
      #LowerCI=integer(),
      #UpperCI=integer()
    )
    row <- vector(mode="character")
    if (prevalence >= 0 && prevalence <= 1) 
    {
      verim <- data.frame(Estimate = round(prevalence, digit=3), LowerCI = round(prevalence_low, digit=3), UpperCI = round(prevalence_upp, digit=3))
      row <- c("Prevalence")
    } 
    if (detection_rate >= 0 && detection_rate <= 1)
    {
      newRow <- data.frame(Estimate = round(detection_rate, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Detection Rate")
    }
    if (detection_prevalence >= 0 && detection_prevalence <= 1)
    {
      newRow <- data.frame(Estimate = round(detection_prevalence, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Detection Prevalence")
    }
    
    if (difference_in_proportion >= 0 && difference_in_proportion <= 1)
    {
      newRow <- data.frame(Estimate = round(difference_in_proportion, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Difference of Proportion")
    }
    if (number_needed_to_treat >= 0 && number_needed_to_treat <= 1)
    {
      newRow <- data.frame(Estimate = round(number_needed_to_treat, digit=3),LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Number Needed to Treat")
    }
    if (absolute_risk_reduction >= 0 && absolute_risk_reduction <= 1)
    {
      newRow <- data.frame(Estimate = round(absolute_risk_reduction, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Absolute Risk Reduction")
    }
    if (relative_risk_reduction >= 0 && relative_risk_reduction <= 1)
    {
      newRow <- data.frame(Estimate = round(relative_risk_reduction, digit=3),LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Relative Risk Reduction")
    }
    if (test_level >= 0 && test_level <= 1)
    {
      newRow <- data.frame(Estimate = round(test_level, digit=3), LowerCI = round(test_level_low, digit=3), UpperCI = round(test_level_upp, digit=3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Test level")
    }
    if (pretest_odds >= 0 && pretest_odds <= 1)
    {
      newRow <- data.frame(Estimate = round(pretest_odds, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Pre-test Odds")
    }
    if (likelihoodratio_positivetest >= 0 && likelihoodratio_positivetest <= 1)
    {
      newRow <- data.frame(Estimate = round(likelihoodratio_positivetest, digit=3), LowerCI = round(likelihoodratio_positivetest_low, digit=3), UpperCI = round(likelihoodratio_positivetest_upp, digit=3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Likelihood ratio of positive test (LR+)")
    }
    if (posttest_odds >= 0 && posttest_odds <= 1)
    {
      newRow <- data.frame(Estimate = round(posttest_odds, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Post-test Odds")
    }
    if (bayes_ppv >= 0 && bayes_ppv <= 1)
    {
      newRow <- data.frame(Estimate = round(bayes_ppv, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "The Probabality of Disease Given a Positive Test (PPV with Bayes Formula)")
    }
    if (posttest_probability >= 0 && posttest_probability <= 1)
    {
      newRow <- data.frame(Estimate = round(posttest_probability, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Post-test Probability (Positive Test Result)")
    }
    if (likelihoodratio_negativetest >= 0 && likelihoodratio_negativetest <= 1)
    {
      newRow <- data.frame(Estimate = round(likelihoodratio_negativetest, digit=3),  LowerCI = round(likelihoodratio_negativetest_low, digit=3), UpperCI = round(likelihoodratio_negativetest_upp, digit=3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Likelihood ratio of negative test (LR-)")
    }
    if (bayes_npv >= 0 && bayes_npv <= 1)
    {
      newRow <- data.frame(Estimate = round(bayes_npv, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "The Probabality of No Disease Given a Negative Test (NPV with Bayes Formula)")
    }
    if (posttest_probability_neg >= 0 && posttest_probability_neg <= 1)
    {
      newRow <- data.frame(Estimate = round(posttest_probability_neg, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Post-test Probability (Negative Test Result)")
    }
    if (inverse_likelihoodratio_negativetest >= 0 && inverse_likelihoodratio_negativetest <= 1)
    {
      newRow <- data.frame(Estimate = round(inverse_likelihoodratio_negativetest, digit=3),  LowerCI = round(inverse_likelihoodratio_negativetest_low, digit=3), UpperCI = round(inverse_likelihoodratio_negativetest_upp, digit=3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Inverse of the likelihood ratio of negative test")
    }
    if (odds_ratio >= 0 && odds_ratio <= 1)
    {
      newRow<- data.frame(Estimate = round(odds_ratio, digit=3), LowerCI = round(odds_ratio_low, digit=3), UpperCI = round(odds_ratio_upp, digit=3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Odds ratio (OR)")
    } 
    if (odds_ratio_Haldane >= 0 && odds_ratio_Haldane <= 1)
    {
      newRow <- data.frame(Estimate = round(odds_ratio_Haldane, digit=3),  LowerCI = round(odds_ratio_Haldane_low, digit=3), UpperCI = round(odds_ratio_Haldane_upp, digit=3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Odds ratio (Haldanes_estimator)")
    } 
    if (error_odds_ratio >= 0 && error_odds_ratio <= 1)
    {
      newRow <- data.frame(Estimate = round(error_odds_ratio, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Error Odds Ratio")
    }   
    if (relative_risk >= 0 && relative_risk <= 1)
    {
      newRow<- data.frame(Estimate = round(relative_risk, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Relative Risk (Risk Ratio - RR)")
    }  
    if (attributable_risk >= 0 && attributable_risk <= 1)
    {
      newRow<- data.frame(Estimate = round(attributable_risk, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Attributable Risk (AR)")
    }  

    rownames(verim) = row
  
    highchart() %>% hc_exporting(enabled = TRUE, filename = "Plot5") %>% 
      hc_add_series(name = "Conf. Interval", type = "line", data = verim$`Estimate`) %>%
      hc_add_series(name = "CI", data = as.matrix(cbind(verim$`LowerCI`, verim$`UpperCI`)),type = "errorbar", names = "Limits", showInLegend = FALSE, zIndex = 0, lineWidth = 1.5, linkedTo = "CI"
      ) %>%
      hc_xAxis(categories = rownames(verim),title = "Risk Measures:") %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, headerFormat = "<b>Risk Measures: </b>{point.x} <br>") %>%
      hc_plotOptions(line = list(tooltip = list(pointFormat = "<b>{series.name}: </b>{point.y:.3f} ")), 
                     errorbar = list(tooltip = list(pointFormat = "({point.low} - {point.high})"))) %>%
      hc_add_theme(hc_theme_google())
    

    })
  output$plot51 <- highcharter::renderHighchart({
    if (input$verigir_kontrol == TRUE){ 
      TP = 0
      FN = 0
      FP = 0
      TN = 0
      for (i in 1:nrow(tabloveri()))
      {
        if (tabloveri()[i,1]==1 & tabloveri()[i,2]==1)
        {TP = TP+1}
        else if (tabloveri()[i,1]==1 & tabloveri()[i,2]==2)
        {FN = FN+1}
        else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==1)
        {FP = FP+1}
        else if (tabloveri()[i,1]==2 & tabloveri()[i,2]==2)
        {TN = TN+1}
        next
      }
      CI = as.numeric(trimws(input$text5_2))
    }
    else{
      TP = as.numeric(trimws(input$text1)) ### x
      FN = as.numeric(trimws(input$text2)) ### y
      FP = as.numeric(trimws(input$text3)) ### z
      TN = as.numeric(trimws(input$text4)) ### t
      CI = as.numeric(trimws(input$text5))
    }
    
    
    satirtop1 = TP + FN
    satirtop2 = FP + TN
    sutuntop1 = TP + FP
    sutuntop2 = FN + TN
    toplam = satirtop1 + satirtop2
    #table1 <- matrix(c(TP,FN,satirtop1,FP,TN,satirtop2,sutuntop1,sutuntop2,toplam),nrow=3,ncol=3,byrow=TRUE,dimnames=list(c("1.satir","2.satir","Sutun toplamlari"),c("1.kolon","2.kolon","Satir toplamlari")))
    #table1
    ## Duyarlilik 
    sensitivity_estimate = TP / satirtop1
    sensitivity_se = round((sqrt(sensitivity_estimate * (1 - sensitivity_estimate)/satirtop1)), 4)
    sensitivity_low = 1 - Rbeta.inv(((1 + CI/100)/2), (satirtop1+1-TP), TP)
    sensitivity_upp = Rbeta.inv(((1 + CI/100)/2), (TP+1), (satirtop1-TP))
    sensitivity_randomtest = sutuntop1 / toplam
    qualityindex_sensitivity = (sensitivity_estimate - sensitivity_randomtest) / (1 - sensitivity_randomtest)
    ## Secicilik
    specificity = TN / satirtop2
    specificity_se = sqrt(specificity * (1 - specificity) / satirtop2)
    specificity_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop2+1-TN),TN)
    specificity_upp = Rbeta.inv((1 + CI/100)/2, (TN+1), (satirtop2-TN))
    specificity_randomtest = (1 - sensitivity_randomtest)
    qualityindex_specificity = (specificity - 1 + sensitivity_randomtest) / sensitivity_randomtest
    ## Gain in Certainty
    gain_in_certainty = sensitivity_estimate + specificity
    ## Etkililik, yeterlilik ayni zamanda overall accuracy (toplam-genel dogruluk orani)
    efficiency = (TP + TN) / toplam
    efficiency_se = sqrt(efficiency * (1 - efficiency) / toplam)
    efficiency_low = 1-Rbeta.inv((1 + CI/100)/2, (toplam+1-TP-TN), (TP+TN))
    efficiency_upp = Rbeta.inv((1 + CI/100)/2, (TP+TN+1), (toplam-TP-TN))
    efficiency_randomtest = (satirtop1 / toplam) * sensitivity_randomtest + (1 - (satirtop1 / toplam)) * (1 - sensitivity_randomtest)
    mis_efficiency = 1 - efficiency
    ## Quality indeks
    quality_index = (efficiency - efficiency_randomtest) / (1 - efficiency_randomtest)
    quality_index_se = sqrt((efficiency*(1-efficiency)/(1-efficiency_randomtest)^2+2*(1-efficiency)*(2*efficiency*efficiency_randomtest-((TP*((satirtop1 / toplam)+sensitivity_randomtest)+TN*(2-(satirtop1 / toplam)-sensitivity_randomtest))/toplam))/(1-efficiency_randomtest)^3+((1-efficiency)^2)*((TP*((satirtop1 / toplam)+sensitivity_randomtest)^2+FN*(-(satirtop1 / toplam)+1+sensitivity_randomtest)^2+FP*(1+(satirtop1 / toplam)-sensitivity_randomtest)^2+TN*(2-(satirtop1 / toplam)-sensitivity_randomtest)^2)/toplam-4*efficiency_randomtest^2)/(1-efficiency_randomtest)^4)/toplam)
    quality_index_low = quality_index + qnorm((1 - CI/100)/2)*quality_index_se
    quality_index_upp = quality_index - qnorm((1 - CI/100)/2)*quality_index_se
    ## Youden indeksi
    youdens_index = sensitivity_estimate + specificity - 1
    youdens_index_se = sqrt(sensitivity_se^2+specificity_se^2)
    youdens_index_low = youdens_index + qnorm((1 - CI/100)/2)*youdens_index_se
    youdens_index_upp = youdens_index - qnorm((1 - CI/100)/2)*youdens_index_se
    
    number_needed_to_diagnose = 1 / youdens_index
    number_needed_to_misdiagnose = 1 / ( 1 - efficiency ) 
    ## Pozitif kestirim degeri
    predictivevalue_positivetest = TP / sutuntop1
    predictivevalue_positivetest_se = sqrt(predictivevalue_positivetest*(1-predictivevalue_positivetest)/sutuntop1)
    predictivevalue_positivetest_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop1+1-TP),TP)
    predictivevalue_positivetest_upp = Rbeta.inv((1 + CI/100)/2, (TP+1),(sutuntop1-TP))
    predvalue_positiverandomtest = (satirtop1 / toplam)
    predvalue_positiverandomtest_mar_ol = predvalue_positiverandomtest*sensitivity_estimate+(1-predvalue_positiverandomtest)*(1-specificity)
    ## Negatif kestirim degeri
    predictivevalue_negativetest = TN / sutuntop2
    predictivevalue_negativetest_se = sqrt(predictivevalue_negativetest*(1-predictivevalue_negativetest)/sutuntop2)
    predictivevalue_negativetest_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop2+1-TN),TN)
    predictivevalue_negativetest_upp = Rbeta.inv((1 + CI/100)/2, (TN+1), (sutuntop2-TN))
    predvalue_negativerandomtest = 1 - (satirtop1 / toplam)
    predvalue_negativerandomtest_mar_ol = 1 - predvalue_positiverandomtest_mar_ol
    ## Predictive Summary Index (PSI)
    predictive_summary_index = predictivevalue_positivetest + predictivevalue_negativetest - 1
    ## Yanlis pozitif orani
    false_positiverate = FP / satirtop2
    false_positiverate_se = sqrt(false_positiverate*(1-false_positiverate)/satirtop2)
    false_positiverate_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop2+1-FP),FP)
    false_positiverate_upp = Rbeta.inv((1 + CI/100)/2, (FP+1), (satirtop2-FP))
    ## Yansis negatif orani
    false_negativerate = FN / satirtop1
    false_negativerate_se = sqrt(false_negativerate*(1-false_negativerate)/satirtop1)
    false_negativerate_low = 1 - Rbeta.inv((1 + CI/100)/2, (satirtop1+1-FN),FN)
    false_negativerate_upp = Rbeta.inv((1 + CI/100)/2, (FN+1), (satirtop1-FN))
    ## False Omission Rate (FOR) https://www.ncss.com/software/ncss/diagnostic-tests-in-ncss/
    false_omission_rate = FN/(sutuntop2)
    false_omission_rate_se = sqrt(false_omission_rate*(1-false_omission_rate)/sutuntop2)
    false_omission_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop2+1-FN),FN)
    false_omission_rate_upp = Rbeta.inv((1 + CI/100)/2, (FN+1), (sutuntop2-FN))
    ## False Discovery Rate (FDR) https://www.ncss.com/software/ncss/diagnostic-tests-in-ncss/
    false_discovery_rate = FP/(sutuntop1)
    false_discovery_rate_se = sqrt(false_discovery_rate*(1-false_discovery_rate)/sutuntop1)
    false_discovery_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (sutuntop1+1-FP),FP)
    false_discovery_rate_upp = Rbeta.inv((1 + CI/100)/2, (FP+1), (sutuntop1-FP))
    ## Dogru pozitif orani
    true_positiverate = TP / (satirtop1)
    ## Yanlis siniflandirma orani
    misclassification_rate = (FN + FP) / toplam
    misclassification_rate_se = sqrt(misclassification_rate*(1-misclassification_rate)/toplam)
    misclassification_rate_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-FP-FN), (FP+FN))
    misclassification_rate_upp = Rbeta.inv((1 + CI/100)/2, (FP+FN+1), (toplam-FP-FN))
    ## Prevelans
    prevalence = (satirtop1 / toplam)
    prevalence_se = sqrt((satirtop1 / toplam)*(1-(satirtop1 / toplam))/toplam)
    prevalence_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-satirtop1), satirtop1)
    prevalence_upp = Rbeta.inv((1 + CI/100)/2, (satirtop1+1), (toplam-satirtop1))
    
    ##caret paketinden olculer
    detection_rate = TP/toplam
    detection_prevalence = (TP+FP)/toplam
    balanced_accuracy = (sensitivity_estimate + specificity)/2
    lift = (TP/(TP+FN))/((TP+FP)/toplam)  ## https://gerardnico.com/wiki/data_mining/error_rate
    precision = TP/(TP+FP)
    recall = TP/(TP+FN)
    ## http://michael.hahsler.net/research/recomm_lnai2002/lnai2002.pdf
    conviction = (1-((TP+FP)/toplam))/(1-(TP/(TP+FN)))
    matthews_correlation_coefficient = (TP * TN + FN * FP) / ((TP+FP)*(FN+TN)*(FP+TN)*(TP+FN))^(1/2)
    ## Goreli Risk Orani (Relative risk)
    ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
    # https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20ratio.htm
    relative_risk = (TP/sutuntop1)/(FN/sutuntop2)
    relative_risk_se = sqrt((1/TP)+(1/FN)-(1/(TP+FP))-(1/(FN+TN)))
    relative_risk_low = exp(log(relative_risk)+qnorm((1-CI/100)/2)*relative_risk_se)
    relative_risk_upp = exp(log(relative_risk)-qnorm((1-CI/100)/2)*relative_risk_se)
    difference_in_proportion = (TP/sutuntop1) - (FN/sutuntop2)
    ## Leverage
    ## https://www3.nd.edu/~tweninge/pubs/AHBCW_ANNIE08.pdf
    leverage = (TP/sutuntop1)-((TP+FP)/toplam)
    ## http://www.ebm.med.ualberta.ca/TherapyCalc.html
    control_event_rate = FN/sutuntop2
    experimental_event_rate = TP/sutuntop1
    ## Absolute risk reduction
    absolute_risk_reduction = -difference_in_proportion
    absolute_risk_reduction_se = sqrt((TP/sutuntop1)*(1-(TP/sutuntop1))/sutuntop1+(FN/sutuntop2)*(1-(FN/sutuntop2))/sutuntop2)
    absolute_risk_reduction_low = absolute_risk_reduction - qnorm((1-CI/100)/2)*absolute_risk_reduction_se
    absolute_risk_reduction_upp = absolute_risk_reduction + qnorm((1-CI/100)/2)*absolute_risk_reduction_se
    ## Relative risk reduction
    relative_risk_reduction = absolute_risk_reduction / (FN/sutuntop2)
    relative_risk_reduction_low = 1 - exp(log(relative_risk)-qnorm((1-CI/100)/2)*relative_risk_se)
    relative_risk_reduction_upp = 1 - exp(log(relative_risk)+qnorm((1-CI/100)/2)*relative_risk_se)
    ##Number needed to treat
    number_needed_to_treat = 1 / abs(difference_in_proportion)
    number_needed_to_treat_low = 1 /absolute_risk_reduction_upp
    number_needed_to_treat_upp = 1 /absolute_risk_reduction_low
    ## Test duzeyi
    test_level = sensitivity_randomtest
    test_level_se = sqrt(test_level*(1-test_level)/toplam)
    test_level_low = 1 - Rbeta.inv((1 + CI/100)/2, (toplam+1-sutuntop1),sutuntop1)
    test_level_upp = Rbeta.inv((1 + CI/100)/2, (sutuntop1+1), (toplam-sutuntop1))
    ## Pre-test odds
    pretest_odds = prevalence / (1 - prevalence)
    ## Pozitif Olabilirlik Orani
    likelihoodratio_positivetest = sensitivity_estimate/(1-specificity)
    likelihoodratio_positivetest_se = exp(sqrt((1-sensitivity_estimate)/(sensitivity_estimate*satirtop1) +specificity/((1-specificity)*satirtop2)))
    likelihoodratio_positivetest_low = exp(log(likelihoodratio_positivetest) + qnorm((1 - CI/100)/2)*log(likelihoodratio_positivetest_se))
    likelihoodratio_positivetest_upp = exp(log(likelihoodratio_positivetest) - qnorm((1 - CI/100)/2)*log(likelihoodratio_positivetest_se))
    ## post-test odds
    posttest_odds = pretest_odds * likelihoodratio_positivetest
    bayes_ppv = (sensitivity_estimate * prevalence)/((sensitivity_estimate * prevalence) + (1 - specificity) * (1 - prevalence))
    ## Post-test probability (positive test result)
    posttest_probability = (pretest_odds * likelihoodratio_positivetest) / (1 + (pretest_odds * likelihoodratio_positivetest))
    ## Negatif Olabilirlik Orani
    likelihoodratio_negativetest = (1-sensitivity_estimate)/specificity
    likelihoodratio_negativetest_se = exp(sqrt(sensitivity_estimate/((1-sensitivity_estimate)*satirtop1) +(1-specificity)/(specificity*satirtop2)))
    likelihoodratio_negativetest_low = exp(log(likelihoodratio_negativetest) + qnorm((1-CI/100)/2)*log(likelihoodratio_negativetest_se))
    likelihoodratio_negativetest_upp = exp(log(likelihoodratio_negativetest) - qnorm((1-CI/100)/2)*log(likelihoodratio_negativetest_se))
    bayes_npv = (sensitivity_estimate * (1 - prevalence))/((sensitivity_estimate * (1 - prevalence)) + (1 - specificity) * prevalence)
    ## Post-test probability (negative test result)
    posttest_probability_neg = (pretest_odds * likelihoodratio_negativetest) / (1 + (pretest_odds * likelihoodratio_negativetest))
    ## Ters negatif olabilirlik orani
    inverse_likelihoodratio_negativetest = specificity/(1-sensitivity_estimate)
    inverse_likelihoodratio_negativetest_se = exp(sqrt(sensitivity_estimate/((1-sensitivity_estimate)*satirtop1) +(1-specificity)/(specificity*satirtop2)))
    inverse_likelihoodratio_negativetest_low = exp(log(inverse_likelihoodratio_negativetest) + qnorm((1-CI/100)/2)*log(inverse_likelihoodratio_negativetest_se))
    inverse_likelihoodratio_negativetest_upp = exp(log(inverse_likelihoodratio_negativetest) - qnorm((1-CI/100)/2)*log(inverse_likelihoodratio_negativetest_se))
    ## Diagnostic Odds Ratio (Tani Odds Orani)
    odds_ratio = (TP/FN)/(FP/TN)
    odds_ratio_se = exp(sqrt(1/TP+1/FP+1/TN+1/FN))
    odds_ratio_low = exp(log(odds_ratio) + qnorm((1-CI/100)/2)*log(odds_ratio_se))
    odds_ratio_upp = exp(log(odds_ratio) - qnorm((1-CI/100)/2)*log(odds_ratio_se))
    odds_ratio_Haldane = ((TP+0.5)/(FN+0.5))/((FP+0.5)/(TN+0.5))
    odds_ratio_Haldane_se = exp(sqrt(1/(TP+0.5)+1/(FP+0.5)+1/(TN+0.5)+1/(FN+0.5)))
    odds_ratio_Haldane_low = exp(log(odds_ratio_Haldane) + qnorm((1-CI/100)/2)*log(odds_ratio_Haldane_se))
    odds_ratio_Haldane_upp = exp(log(odds_ratio_Haldane) - qnorm((1-CI/100)/2)*log(odds_ratio_Haldane_se))
    error_odds_ratio = (sensitivity_estimate/(1-sensitivity_estimate))/(specificity/(1-specificity))
    ## Rate ratio
    ## http://omarkasule-05.tripod.com/id52.html
    ## http://sphweb.bumc.bu.edu/otlt/mph-modules/ep/ep713_randomerror/ep713_randomerror4.html
    rate_ratio = (TP/sutuntop1)/(TP/sutuntop2)
    rate_ratio_low = exp(log(relative_risk)+qnorm((1-CI/100)/2)*sqrt(1/TP+1/FP))
    rate_ratio_upp = exp(log(relative_risk)-qnorm((1-CI/100)/2)*sqrt(1/TP+1/FP))
    ## Risk Difference
    ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
    risk_difference = (TP/sutuntop1) - (FN/sutuntop2)
    risk_difference_se = (((TP/sutuntop1)*(1-(TP/sutuntop1)))/sutuntop1)+(((FN/sutuntop2)*(1-(FN/sutuntop2)))/sutuntop2)
    risk_difference_low = risk_difference - qnorm((1-CI/100)/2)*sqrt(risk_difference_se)
    risk_difference_upp = risk_difference + qnorm((1-CI/100)/2)*sqrt(risk_difference_se)
    ## Nitelenebilen risk
    ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
    attributable_risk = (TP/sutuntop1)-(FN/sutuntop2)
    attributable_risk_se = sqrt((satirtop1/toplam)*(1-(satirtop1/toplam))*(1/sutuntop1+1/sutuntop2))
    attributable_risk_low = attributable_risk + qnorm((1-CI/100)/2)*attributable_risk_se
    attributable_risk_upp = attributable_risk - qnorm((1-CI/100)/2)*attributable_risk_se
    ## Attributable risk percent
    ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
    attributable_risk_percent = ((relative_risk-1)/relative_risk)*100
    attributable_risk_percent_low = attributable_risk_percent - (qnorm((1-CI/100)/2)*attributable_risk_se)/attributable_risk
    attributable_risk_percent_upp = attributable_risk_percent + (qnorm((1-CI/100)/2)*attributable_risk_se)/attributable_risk
    ## Population Attributable Risk
    ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
    population_attributable_risk = (satirtop1/toplam)-(FN/sutuntop2)
    ## Population Attributable Risk Percent
    population_attributable_risk_percent = (population_attributable_risk / (satirtop1/toplam))*100
    ## Kohen's Kappa
    cohens_kappa = (efficiency-efficiency_randomtest)/(1-efficiency_randomtest)
    cohens_kappa_se = quality_index_se
    cohens_kappa_low = cohens_kappa + qnorm((1-CI/100)/2)*cohens_kappa_se
    cohens_kappa_upp = cohens_kappa - qnorm((1-CI/100)/2)*cohens_kappa_se
    ## Gozlenen Karar
    observed_agreement = efficiency
    observed_agreement_se = efficiency_se
    observed_agreement_low = efficiency_low
    observed_agreement_upp = efficiency_upp
    ## Risk karari,expected agreement
    chance_agreement = efficiency_randomtest
    ## Pozitif karar
    positive_agreement = 2*TP /(satirtop1+sutuntop1)
    positive_agreement_se = sqrt(4*TP*(FP+FN)*(TP+FP+FN))/(sutuntop1+satirtop1)^2
    positive_agreement_low = positive_agreement + qnorm((1-CI/100)/2)*positive_agreement_se
    positive_agreement_upp = positive_agreement - qnorm((1-CI/100)/2)*positive_agreement_se
    ## Negatif karar
    negative_agreement = 2*TN/(satirtop2+sutuntop2)
    negative_agreement_se = sqrt(4*TN*(FP+FN)*(TN+FP+FN))/(sutuntop2+satirtop2)^2
    negative_agreement_low = negative_agreement + qnorm((1-CI/100)/2)*negative_agreement_se
    negative_agreement_upp = negative_agreement - qnorm((1-CI/100)/2)*negative_agreement_se
    ## Rand index
    ## http://www.cs.pomona.edu/classes/cs158/resources/158-10(EM).pdf
    rand_index = (TP+TN)/(TP+FN+FP+TN)
    ## e-measure
    ## http://michael.hahsler.net/research/recomm_lnai2002/lnai2002.pdf
    e_measure = 1/((((1-CI)/100)*(1-precision))+((1-((1-CI)/100))*(1/recall)))
    ## discriminant power 
    ## https://eva.fing.edu.uy/pluginfile.php/69453/mod_resource/content/1/7633-10048-1-PB.pdf
    discriminant_power = (sqrt(3)/pi)*(log (sensitivity_estimate / (1-sensitivity_estimate)) + log (specificity / (1-specificity)))
    ## F1 Score
    ## http://machinelearningmastery.com/classification-accuracy-is-not-enough-more-performance-measures-you-can-use/
    f1_score = 2*(((TP/(TP+FN))*(TP/(TP+FP)))/((TP/(TP+FN))+(TP/(TP+FP))))
    ## Byrt yanlilik indeksi
    byrt_bias_index = (FN-FP)/toplam
    ## Byrt  asimetrik indeks prevelansi
    byrt_prevalence_asymmetry_index = (TN-TP)/toplam
    ## Yanlilik duzeltmeli kappa
    bias_adjusted_kappa = (efficiency-0.25*((satirtop1+sutuntop1)^2+(satirtop2+sutuntop2)^2)/toplam^2)/(1-0.25*((satirtop1+sutuntop1)^2+(satirtop2+sutuntop2)^2)/toplam^2)
    ## Yanlilik duzeltmeli kappa prevelansi
    prevalence_bias_adjusted_kappa = cohens_kappa+(1-cohens_kappa)*(byrt_prevalence_asymmetry_index^2-byrt_bias_index^2)
    ## Dice'in indeksi (Czekanowski)
    dice_index = positive_agreement
    dice_index_se= positive_agreement_se
    dice_index_low = positive_agreement_low
    dice_index_upp = positive_agreement_upp
    ## Yule's Q
    yules_q = (odds_ratio - 1) / (odds_ratio + 1)
    yules_q_se = 0.5*(1-yules_q^2)*sqrt(1/TP+1/FP+1/TN+1/FN)
    yules_q_low = yules_q + qnorm((1-CI/100)/2)*yules_q_se
    yules_q_upp = yules_q - qnorm((1-CI/100)/2)*yules_q_se
    
    equitable_threatscore = (TP-(sutuntop1*satirtop1/toplam))/(TP+FP+FN-(sutuntop1*satirtop1/toplam))
    ## Phi
    phi = (TP*TN-FN*FP) / sqrt(satirtop1 * satirtop2 * sutuntop1 * sutuntop2)
    phi_se = sqrt((1-phi^2+(phi+0.5*phi^3)*((satirtop1-satirtop2)*(sutuntop1-sutuntop2)/sqrt(satirtop1*satirtop2*sutuntop1*sutuntop2))-0.75*phi^2*((satirtop1-satirtop2)^2/(satirtop1*satirtop2)+(sutuntop1-sutuntop2)^2/(sutuntop1*sutuntop2)))/toplam)
    phi_low = phi + qnorm((1-CI/100)/2)*phi_se
    phi_upp = phi - qnorm((1-CI/100)/2)*phi_se
    ## Cramer V katsayisi
    cramer_v = sqrt((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2))/toplam)
    ## Olaganlik katsayisi
    contingency_coefficient = sqrt((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2))/(((toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)))+toplam))
    ## Goodman and Kruskal Gamma 
    goodman_and_kruskal_gamma = (TP*TN-FN*FP)/(TP*TN+FN*FP)
    goodman_and_kruskal_gamma_se = 0.5*(1-yules_q^2)*sqrt(1/TP+1/FP+1/TN+1/FN)
    goodman_and_kruskal_gamma_low = yules_q + qnorm((1-CI/100)/2)*yules_q_se
    goodman_and_kruskal_gamma_upp = yules_q - qnorm((1-CI/100)/2)*yules_q_se
    ## Kendall's tau a
    kendalls_tau_a = (TP*TN-FN*FP)/(toplam*(toplam-1)/2)
    ## Kendall's tau b
    ## https://v8doc.sas.com/sashtml/stat/chap28/sect20.htm
    ## http://slideplayer.com/slide/10766029/
    kendalls_tau_b = (TP*TN-FN*FP)/sqrt((TP*TN+FN*FP+(TP*FN+FP*TN))*(TP*TN+FN*FP+(TP*FP+FN*TN)))
    ## Kendall's tau c
    kendalls_tau_c = ((2*2)*(TP*TN-FN*FP)) / (toplam^2 * (2-1))
    ## Somers'd R|C
    somers_d = (TP*TN-FN*FP)/(TP*TN+FN*FP+(TP*FN+FP*TN))
    ## Somers'd C|R
    somers_d_2 = (TP*TN-FP*FN)/(TP*TN+FP*FN+(TP*FP+FN*TN))
    ## Scoot'un karar indeksi
    scotts_agreement_index = bias_adjusted_kappa
    ## Dort-duzeyli Korelasyon
    tetrachoric_correlation = cos(pi/(1+sqrt(TP*TN/FN/FP)))
    tetrachoric_correlation_se = 0.375*(1-tetrachoric_correlation^2)*sqrt((1/(TP))+(1/(FP))+(1/(TN))+(1/(FN)))
    tetrachoric_correlation_low = tetrachoric_correlation + qnorm((1-CI/100)/2)*tetrachoric_correlation_se
    tetrachoric_correlation_upp = tetrachoric_correlation - qnorm((1-CI/100)/2)*tetrachoric_correlation_se
    ## Goodman kruskal tau katsayisi
    goodman_kruskal_tau = (toplam*((TP^2+FP^2)/sutuntop1+(FN^2+TN^2)/sutuntop2)-satirtop1^2-satirtop2^2)/(toplam^2-satirtop1^2-satirtop2^2)
    goodman_kruskal_tau_se = toplam*sqrt(TP*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop1)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*TP/satirtop1-(TP^2+FN^2)/satirtop1^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+FN*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop2)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*FN/satirtop1-(TP^2+FN^2)/satirtop1^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+FP*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop1)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*FP/satirtop2-(FP^2+TN^2)/satirtop2^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2+TN*(2*(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))*(toplam-sutuntop2)+(toplam^2-sutuntop1^2-sutuntop2^2)*(2*TN/satirtop2-(FP^2+TN^2)/satirtop2^2-(toplam-((TP^2+FN^2)/satirtop1+(FP^2+TN^2)/satirtop2))/toplam-1))^2)/(toplam^2-sutuntop1^2-sutuntop2^2)^2
    goodman_kruskal_tau_low = goodman_kruskal_tau + qnorm((1-CI/100)/2)*goodman_kruskal_tau_se
    goodman_kruskal_tau_upp = goodman_kruskal_tau - qnorm((1-CI/100)/2)*goodman_kruskal_tau_se
    ## Simetrik Lambda 
    lambda_symmetric = ((max(TP,FN)+max(FP,TN)+max(TP,FP)+max(FN,TN)-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2))/(2*toplam-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2)))
    if (lambda_symmetric==0 || lambda_symmetric==1)
    {lambda_symmetric_se = 0}
    else
    {
      lambda_symmetric_se = sqrt((TP*((if(TP>FN) 1 else 0) + (if(TP>FP) 1 else 0) + ((if(sutuntop1>sutuntop2) 1 else 0)+ (if(satirtop1>satirtop2) 1 else 0))*(lambda_symmetric-1))^2+FN*( (if(TP>FN) 0 else 1)+ (if(FN>TN) 1 else 0) +( (if(sutuntop1>sutuntop2) 0 else 1) + (if(satirtop1>satirtop2) 1 else 0))*(lambda_symmetric-1))^2+FP*( (if(FP>TN) 1 else 0) + (if(TP>FP) 0 else 1) +( (if(sutuntop1>sutuntop2) 1 else 0) + (if(satirtop1<satirtop2) 1 else 0))*(lambda_symmetric-1))^2+TN*((if(FP>TN) 0 else 1)+ (if(FN>TN) 0 else 1) +((if(sutuntop1>sutuntop2) 0 else 1)+ (if(satirtop1<satirtop2) 1 else 0))*(lambda_symmetric-1))^2-4*toplam*lambda_symmetric^2)/(2*toplam-max(satirtop1,satirtop2)-max(sutuntop1,sutuntop2))^2)
    }
    
    lambda_symmetric_low = lambda_symmetric + qnorm((1-CI/100)/2)*lambda_symmetric_se
    lambda_symmetric_upp = lambda_symmetric - qnorm((1-CI/100)/2)*lambda_symmetric_se
    ## Lambda Asymmetric R|C
    lambda_criterion = (max(TP,FP)+max(FN,TN)-max(satirtop1,satirtop2))/(toplam-max(satirtop1,satirtop2))
    
    if (lambda_criterion==0 || lambda_criterion==1)
    {lambda_criterion_se = 0}
    else
    {
      lambda_criterion_se = sqrt((toplam-max(TP,FP)-max(FN,TN))*(max(TP,FP)+max(FN,TN)+max(satirtop1,satirtop2)-2*(if(satirtop1>satirtop2) max(TP,FN) else max(FP,TN)))/(toplam-max(satirtop1,satirtop2))^3)
    }
    
    lambda_criterion_low = lambda_criterion + qnorm((1-CI/100)/2)*lambda_criterion_se
    lambda_criterion_upp = lambda_criterion - qnorm((1-CI/100)/2)*lambda_criterion_se
    ## Lambda Asymmetric C|R
    lambda_criterion_2 = (max(TP,FN)+max(FP,TN)-max(sutuntop1,sutuntop2))/(toplam-max(sutuntop1,sutuntop2))
    
    if (lambda_criterion_2==0 || lambda_criterion_2==1)
    {lambda_criterion_se_2 = 0}
    else
    {
      lambda_criterion_se_2 = sqrt((toplam-max(TP,FN)-max(FP,TN))*(max(TP,FN)+max(FP,TN)+max(sutuntop1,sutuntop2)-2*(if(sutuntop1>sutuntop2) max(TP,FP) else max(FN,TN)))/(toplam-max(sutuntop1,sutuntop2))^3)
    }
    
    lambda_criterion_low_2 = lambda_criterion_2 + qnorm((1-CI/100)/2)*lambda_criterion_se_2
    lambda_criterion_upp_2 = lambda_criterion_2 - qnorm((1-CI/100)/2)*lambda_criterion_se_2
    ## belirsizlik katsayisi (coefficient of uncertainty) simetrik
    uncertainty_coefficient_symmetric = 2*(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam)))/(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))
    uncertainty_coefficient_symmetric_se = 2*sqrt(TP*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop1*sutuntop1/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(TP/toplam))^2+FN*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop1*sutuntop2/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(FN/toplam))^2+FP*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop2*sutuntop1/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(FP/toplam))^2+TN*(-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam*log(satirtop2*sutuntop2/toplam^2)-(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)*log(TN/toplam))^2)/toplam/(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam)^2
    uncertainty_coefficient_symmetric_low = uncertainty_coefficient_symmetric + qnorm((1-CI/100)/2)*uncertainty_coefficient_symmetric_se
    uncertainty_coefficient_symmetric_upp = uncertainty_coefficient_symmetric - qnorm((1-CI/100)/2)*uncertainty_coefficient_symmetric_se
    ## (coefficient of uncertainty) R|C
    uncertainty_coeff_crit = (satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)+sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)-(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam)))/(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))
    uncertainty_coeff_crit_se = sqrt(TP*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(TP/sutuntop1)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop1/toplam))^2+FN*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(FN/sutuntop2)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop1/toplam))^2+FP*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(FP/sutuntop1)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop2/toplam))^2+TN*(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam*log(TN/sutuntop2)+(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam+(TP*log(TP/toplam)+FN*log(FN/toplam)+FP*log(FP/toplam)+TN*log(TN/toplam))/toplam)*log(satirtop2/toplam))^2)/toplam/((satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam)^2
    uncertainty_coeff_crit_low = uncertainty_coeff_crit + qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se
    uncertainty_coeff_crit_upp = uncertainty_coeff_crit - qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se
    ## (coefficient of uncertainty) C|R
    uncertainty_coeff_crit_2 = (sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam)+satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam)-(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam)))/(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))
    uncertainty_coeff_crit_se_2 = sqrt(TP*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(TP/satirtop1)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop1/toplam))^2+FP*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(FP/satirtop2)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop1/toplam))^2+FN*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(FN/satirtop1)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop2/toplam))^2+TN*(-(sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam*log(TN/satirtop2)+(-(satirtop1*log(satirtop1/toplam)+satirtop2*log(satirtop2/toplam))/toplam+(TP*log(TP/toplam)+FP*log(FP/toplam)+FN*log(FN/toplam)+TN*log(TN/toplam))/toplam)*log(sutuntop2/toplam))^2)/toplam/((sutuntop1*log(sutuntop1/toplam)+sutuntop2*log(sutuntop2/toplam))/toplam)^2
    uncertainty_coeff_crit_low_2 = uncertainty_coeff_crit_2 + qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se_2
    uncertainty_coeff_crit_upp_2 = uncertainty_coeff_crit_2 - qnorm((1-CI/100)/2)*uncertainty_coeff_crit_se_2
    ## Yule s Y (Coefficient of colligation)
    yule_y = (sqrt(TP*TN)-sqrt(FN*FP))/(sqrt(TP*TN)+sqrt(FN*FP))
    ## Pearson ki-kare
    pearson_chi_squ = toplam*(TP*TN-FN*FP)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)
    pearson_chi_squ_p = dchi(pearson_chi_squ, df=1)
    ## 
    with_yate_cor_for_pearson = toplam*(abs(TP*TN-FN*FP)-0.5*toplam)^2/(satirtop1*satirtop2*sutuntop1*sutuntop2)
    with_yate_cor_p_for_pearson = dchi(with_yate_cor_for_pearson, df=1)
    ## Mantel Haenszel chi-square
    ## http://www.openepi.com/PDFDocs/TwobyTwoDoc.pdf
    mantel_haenszel = ((toplam-1)*(TP*TN-FN*FP)^2)/(satirtop1*satirtop2*sutuntop1*sutuntop2)
    mantel_haenszel_p = dchi(mantel_haenszel, df=1)
    ## Olasilik orani
    likelih_ratio = 2*(TP*log(TP/(sutuntop1*satirtop1/toplam))+FN*log(FN/(sutuntop2*satirtop1/toplam))+FP*log(FP/(sutuntop1*satirtop2/toplam))+TN*log(TN/(sutuntop2*satirtop2/toplam)))
    likelih_ratio_p =dchi(likelih_ratio, df=1)
    ## Fisher'in exact testi
    ##fisher_exact_test = (TP-(satirtop1*sutuntop1/toplam))^2/(satirtop1*sutuntop1/toplam) + (FN-(satirtop1*sutuntop2/toplam))^2/(satirtop1*sutuntop2/toplam) + (FP-(satirtop2*sutuntop1/toplam))^2/(satirtop2*sutuntop1/toplam) + (TN-(satirtop2*sutuntop2/toplam))^2/(satirtop2*sutuntop2/toplam)
    fisher <- c(TP, FN, FP, TN)
    tab <- t(matrix(fisher, nrow=2,ncol=2))
    fisher_exact_test = fisher.test(tab)$p.value
    minimum_exp_fre = min((sutuntop1*satirtop1/toplam):(sutuntop2*satirtop2/toplam))
    cells_exp_fre_5 = (if((sutuntop1*satirtop1/toplam)<5) 1 else 0)+ (if((sutuntop1*satirtop2/toplam)<5) 1 else 0)+ (if((sutuntop2*satirtop1/toplam) <5) 1 else 0)+ (if((sutuntop2*satirtop2/toplam) <5) 1 else 0)
    cells_exp_fre_1 = (if((sutuntop1*satirtop1/toplam) <1) 1 else 0)+ (if((sutuntop1*satirtop2/toplam) <1) 1 else 0)+ (if((sutuntop2*satirtop1/toplam) <1) 1 else 0)+ (if((sutuntop2*satirtop2/toplam) <1) 1 else 0)
    ## Mc Nemar testi
    mcNemar_test = (FP-FN)^2/(FP+FN)
    mcNemar_test_p = dchi(mcNemar_test, df=1)
    with_Yate_cor_for_mcnamer = (abs(FP-FN)-1)^2/(FP+FN)
    with_Yate_cor_p_for_mcnamer = dchi(with_Yate_cor_for_mcnamer, df=1)
    
    ## Belirsizlik (Entropi)
    forbes_NMI = sensitivity_estimate*predvalue_positiverandomtest*log((sensitivity_estimate/predvalue_positiverandomtest_mar_ol),base=2)+
      (1-specificity)*(1-predvalue_positiverandomtest)*log(((1-specificity)/predvalue_positiverandomtest_mar_ol),base=2)+
      (1-sensitivity_estimate)*predvalue_positiverandomtest*log(((1-sensitivity_estimate)/(1-predvalue_positiverandomtest_mar_ol)),base=2)+
      specificity*(1-predvalue_positiverandomtest)*log((specificity/(1-predvalue_positiverandomtest_mar_ol)),base=2)
    ## satir icin entropy (test icin)
    entropy_hr = - ((sutuntop1/toplam) * log(sutuntop1/toplam, base=2) + (sutuntop2/toplam) * log(sutuntop2/toplam, base=2))
    ## sutun icin entropy (hastalik icin)  
    entropy_hc = - ((satirtop1/toplam)*log(satirtop1/toplam, base=2) + (satirtop2/toplam)*log(satirtop2/toplam, base=2))
    ## birlesik entropi (joint entropy)  
    entropy_hrc = - ( (TP/toplam)*log(TP/toplam, base=2) + (FP/toplam)*log(FP/toplam, base=2) + (FN/toplam)*log(FN/toplam, base=2) + (TN/toplam)*log(TN/toplam, base=2))
    ## bilgi icerigi (mutual information)  
    information_r_c = entropy_hr + entropy_hc - entropy_hrc
    ## kosullu entropi (conditional entropy)  
    a = entropy_hrc - entropy_hr
    c = entropy_hrc - entropy_hc
    sim_r_c = information_r_c / (a + information_r_c + c)
    dis_r_c = (a + c) / (a + information_r_c + c)
    ## goreli entropi (relative entropy, kullback-leibler uzakligi)
    ## pozitif test sonucu icin goreli entropi, Relative Improvement Over Chance (RIOC)
    positive_relative_entropy = (TP/sutuntop1)*log(((TP/sutuntop1)/ prevalence), base=2)+(FP/sutuntop1)*log(((FP/sutuntop1)/(1- prevalence)), base=2)
    ## negatif test sonucu icin goreli entropi 
    negative_relative_entropy = (FN/sutuntop2)*log(((FN/sutuntop2)/ prevalence), base=2)+(TN/sutuntop2)*log(((TN/sutuntop2)/(1- prevalence)), base=2)
    
    verim <- data.frame(#Estimate=integer(),
      #LowerCI=integer(),
      #UpperCI=integer()
    )
    row <- vector(mode="character")
    if (prevalence >= 0 && prevalence <= 1) 
    {
      verim <- data.frame(Estimate = round(prevalence, digit=3), LowerCI = round(prevalence_low, digit=3), UpperCI = round(prevalence_upp, digit=3))
      row <- c("Prevalence")
    } 
    if (detection_rate >= 0 && detection_rate <= 1)
    {
      newRow <- data.frame(Estimate = round(detection_rate, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Detection Rate")
    }
    if (detection_prevalence >= 0 && detection_prevalence <= 1)
    {
      newRow <- data.frame(Estimate = round(detection_prevalence, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Detection Prevalence")
    }
    
    if (difference_in_proportion >= 0 && difference_in_proportion <= 1)
    {
      newRow <- data.frame(Estimate = round(difference_in_proportion, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Difference of Proportion")
    }
    if (number_needed_to_treat >= 0 && number_needed_to_treat <= 1)
    {
      newRow <- data.frame(Estimate = round(number_needed_to_treat, digit=3),LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Number Needed to Treat")
    }
    if (absolute_risk_reduction >= 0 && absolute_risk_reduction <= 1)
    {
      newRow <- data.frame(Estimate = round(absolute_risk_reduction, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Absolute Risk Reduction")
    }
    if (relative_risk_reduction >= 0 && relative_risk_reduction <= 1)
    {
      newRow <- data.frame(Estimate = round(relative_risk_reduction, digit=3),LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Relative Risk Reduction")
    }
    if (test_level >= 0 && test_level <= 1)
    {
      newRow <- data.frame(Estimate = round(test_level, digit=3), LowerCI = round(test_level_low, digit=3), UpperCI = round(test_level_upp, digit=3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Test level")
    }
    if (pretest_odds >= 0 && pretest_odds <= 1)
    {
      newRow <- data.frame(Estimate = round(pretest_odds, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Pre-test Odds")
    }
    if (likelihoodratio_positivetest >= 0 && likelihoodratio_positivetest <= 1)
    {
      newRow <- data.frame(Estimate = round(likelihoodratio_positivetest, digit=3), LowerCI = round(likelihoodratio_positivetest_low, digit=3), UpperCI = round(likelihoodratio_positivetest_upp, digit=3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Likelihood ratio of positive test (LR+)")
    }
    if (posttest_odds >= 0 && posttest_odds <= 1)
    {
      newRow <- data.frame(Estimate = round(posttest_odds, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Post-test Odds")
    }
    if (bayes_ppv >= 0 && bayes_ppv <= 1)
    {
      newRow <- data.frame(Estimate = round(bayes_ppv, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "The Probabality of Disease Given a Positive Test (PPV with Bayes Formula)")
    }
    if (posttest_probability >= 0 && posttest_probability <= 1)
    {
      newRow <- data.frame(Estimate = round(posttest_probability, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Post-test Probability (Positive Test Result)")
    }
    if (likelihoodratio_negativetest >= 0 && likelihoodratio_negativetest <= 1)
    {
      newRow <- data.frame(Estimate = round(likelihoodratio_negativetest, digit=3),  LowerCI = round(likelihoodratio_negativetest_low, digit=3), UpperCI = round(likelihoodratio_negativetest_upp, digit=3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Likelihood ratio of negative test (LR-)")
    }
    if (bayes_npv >= 0 && bayes_npv <= 1)
    {
      newRow <- data.frame(Estimate = round(bayes_npv, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "The Probabality of No Disease Given a Negative Test (NPV with Bayes Formula)")
    }
    if (posttest_probability_neg >= 0 && posttest_probability_neg <= 1)
    {
      newRow <- data.frame(Estimate = round(posttest_probability_neg, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Post-test Probability (Negative Test Result)")
    }
    if (inverse_likelihoodratio_negativetest >= 0 && inverse_likelihoodratio_negativetest <= 1)
    {
      newRow <- data.frame(Estimate = round(inverse_likelihoodratio_negativetest, digit=3),  LowerCI = round(inverse_likelihoodratio_negativetest_low, digit=3), UpperCI = round(inverse_likelihoodratio_negativetest_upp, digit=3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Inverse of the likelihood ratio of negative test")
    }
    if (odds_ratio >= 0 && odds_ratio <= 1)
    {
      newRow<- data.frame(Estimate = round(odds_ratio, digit=3), LowerCI = round(odds_ratio_low, digit=3), UpperCI = round(odds_ratio_upp, digit=3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Odds ratio (OR)")
    } 
    if (odds_ratio_Haldane >= 0 && odds_ratio_Haldane <= 1)
    {
      newRow <- data.frame(Estimate = round(odds_ratio_Haldane, digit=3),  LowerCI = round(odds_ratio_Haldane_low, digit=3), UpperCI = round(odds_ratio_Haldane_upp, digit=3))
      verim <- rbind(verim,newRow)
      row <- append(row, "Odds ratio (Haldanes_estimator)")
    } 
    if (error_odds_ratio >= 0 && error_odds_ratio <= 1)
    {
      newRow <- data.frame(Estimate = round(error_odds_ratio, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Error Odds Ratio")
    }   
    if (relative_risk >= 0 && relative_risk <= 1)
    {
      newRow<- data.frame(Estimate = round(relative_risk, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Relative Risk (Risk Ratio - RR)")
    }  
    if (attributable_risk >= 0 && attributable_risk <= 1)
    {
      newRow<- data.frame(Estimate = round(attributable_risk, digit=3), LowerCI = NA, UpperCI = NA)
      verim <- rbind(verim,newRow)
      row <- append(row, "Attributable Risk (AR)")
    }  
    
    rownames(verim) = row
    
    highchart() %>% hc_exporting(enabled = TRUE, filename = "Plot5") %>% 
      hc_add_series(name = "Conf. Interval", type = "bar", data = verim$`Estimate`) %>%
      hc_add_series(name = "CI", data = as.matrix(cbind(verim$`LowerCI`, verim$`UpperCI`)),type = "errorbar", names = "Limits", showInLegend = FALSE, zIndex = 0, lineWidth = 1.5, linkedTo = "CI"
      ) %>%
      hc_xAxis(categories = rownames(verim),title = "Risk Measures:") %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, headerFormat = "<b>Risk Measures: </b>{point.x} <br>") %>%
      hc_plotOptions(line = list(tooltip = list(pointFormat = "<b>{series.name}: </b>{point.y:.3f} ")), 
                     errorbar = list(tooltip = list(pointFormat = "({point.low} - {point.high})"))) %>%
      hc_add_theme(hc_theme_google())
    
    
  })
}
  

  


shinyApp(ui, server)