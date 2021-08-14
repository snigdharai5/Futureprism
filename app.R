library(shiny)
library(shinythemes)
library(ggplot2)
library(DT)
library(readxl)
library(dplyr)
library(mice)
library(caret)
library(shinydashboard)
library(gridExtra)
library(tidyr)
library(rpart.plot)
library(wordcloud2)
library(tidytext)
library(stringr)
library(tm)



filePath = getwd()
setwd(filePath)
source("DataQualityReport.R")
options(warn = -1)

### === Constants and Definitions === 
## == UI Constants ==
job_title_dropdown = c(
  "Business Analyst",
  "Business Systems Analyst ",
  "Data Analyst",
  "Data Engineer",
  "Data Scientist",
  "Financial Analyst",
  "Management Analyst",
  "Market Research Analyst",
  "Operations Manager",
  "Product Manager",
  "Program Manager"
)

## == Server Constants ==
yearAxisRange <- c(2016, 2017, 2018, 2019, 2020)

### === Application Settings === 
## == Server Setting ==

# PDF Report object
pdfReport <- ggplot() + theme_void() + geom_text(aes(0,0,label=''))

### === Source data for the application === 

myFile <- readRDS("data_employment")
myFile[, 'CASE_STATUS'] <- (ifelse(myFile[, 'CASE_STATUS'] == 'Denied', 1, 0))

# = Preprocess data =
myFile1 <- myFile %>% drop_na()
#myFile1[, 'CASE_STATUS'] <- (ifelse(myFile1[, 'CASE_STATUS'] == 'Denied', 1, 0))
myFile1$CLASS_OF_ADMISSION <- as.factor(myFile1$CLASS_OF_ADMISSION)
myFile1$COMPANY_SIZE <- as.factor(myFile1$COMPANY_SIZE)
myFile1$PW_SKILL_LEVEL <- as.factor(myFile1$PW_SKILL_LEVEL)
myFile1$Region <- as.factor(myFile1$Region)
myFile1$EMPLOYER_STATE <- NULL
myFile1$EMPLOYER_STATE_CODE <- as.character(myFile1$EMPLOYER_STATE_CODE)
#data$CASE_STATUS <- as.factor(data$CASE_STATUS)
#data$EMPLOYER_NUM_EMPLOYEES <- NULL
#making NA skill levels as "Any", then making it a factor vector
#data$PW_SKILL_LEVEL <- ifelse(is.na(data$PW_SKILL_LEVEL) == TRUE, "ANY", data$PW_SKILL_LEVEL)

### preprocess skill dataset

#project_data<-read_excel("Project Data_team11.xlsx")
project_data <- readRDS("data_skills")

#data cleaning for job matching
lex = filter(project_data, project_data$`Job Title` %in% c("Financial Analyst", "Business Systems Analyst", "Data Engineer","Program Manager",
                                                           "Management Analyst","Operations Manager","Data Analyst","Software Engineer", 
                                                           "Business Analyst", "Data Scientist", "Project Manager", "Market Research Analyst", "Product Manager","Software Engineer"))
names <- tolower(colnames(lex))[7:length(colnames(lex))]
lex1 = c()
for (i in names) {
  for(j in strsplit(i, " ")){
    lex1 = c(lex1, j)
  }
}
df = as.data.frame(matrix(nrow = 10, ncol = 0))
for (name in colnames(lex)){
  for (j in strsplit(name, " ")){
    for (k in j){
      df[k] <- lex[name]
    }
  }
}
df = df %>%
  replace(is.na(.), 0)
df = df[11:ncol(df)]
colnames(df) <- tolower(colnames(df))
rn = lex$`Job Title`
rownames(df) = rn
tot = rowSums(df)


#data cleaning for skills data 
colnames(project_data) <- tolower(gsub(" ", "_", colnames(project_data)))
data_clean <- project_data %>%
  filter(job_title %in% c("Financial Analyst", "Business Systems Analyst", "Data Engineer","Program Manager",
                          "Management Analyst","Operations Manager","Data Analyst","Software Engineer", 
                          "Business Analyst", "Data Scientist", "Project Manager", "Market Research Analyst", "Product Manager")) %>%
  select(-`current_visa_status_(class)`, -citizenship, -companies, -location, -salary) %>%
  replace(is.na(.), 0)
# myFile$year <- as.integer(myFile$year)
skills <- data.frame(tolower(gsub("_", " ", colnames(data_clean))))
colnames(skills) <- "skill"

dummy1<-data.frame(data_clean$job_title)
colnames(dummy1) <- "skill"

skills <-rbind(skills , dummy1)
skills <- skills[-c(1:6), ]
skills <- data.frame(skills)
skills_processed <- data.frame(tolower(gsub(" ", "_", skills$skill)))


# Predictions
predictionDF <- myFile1
DataQualityReport(predictionDF) %>% select(Attributes, Type, NumberMissing, PercentComplete) %>%
  filter(NumberMissing > 0) %>%
  arrange(desc(NumberMissing))

#Imputing values to fill in for the missing values
imputedValues <-
  mice(
    data = predictionDF,
    seed = 123,
    method = "cart",
    m = 1,
    maxit = 1
  )
predictionDF <- mice::complete(imputedValues, 1)
DataQualityReport(predictionDF)

#traintestsplit
set.seed(22)
train.index <-
  sample(x = 1:nrow(predictionDF),
         size = ceiling(0.8 * nrow(predictionDF)))
train <- predictionDF[train.index,]
test <- predictionDF[-train.index,]


fit <-
  rpart(
    CASE_STATUS ~ COMPANY_SIZE + PW_WAGE + PW_SKILL_LEVEL + Region,
    method = "class",
    data = train
  )

fit1 <- glm(CASE_STATUS ~ COMPANY_SIZE + PW_WAGE + PW_SKILL_LEVEL + Region, family = "binomial", data = train)
# png(file = "decTreePICGFG.png",
#     width = 1000,
#     height = 1000)
# 
# 
# plot(fit, uniform = TRUE, main = "MPG Decision Tree using Regression")
# text(fit, use.n = TRUE, cex = .6)
# dev.off()
# mytest  <-
#   data.frame (
#     COMPANY_SIZE = "Large",
#     PW_WAGE = 70000,
#     PW_SKILL_LEVEL = "Level I",
#     Region = "Midwest"
#   )
# predictedResult <- predict(fit1,mytest, type = "response")

### === User Interface === 
ui <- fluidPage(
  navbarPage(
    h3("FUTUREPRISM",style = "padding: 5px 14px 5px 14px;margin: 5px 5px 5px 0px;"),
    theme = shinytheme("darkly"),
    tabPanel(
      (h4("Dashboard")),
      dashboardPage(
        dashboardHeader(titleWidth = -10),
        dashboardSidebar(
          selectInput(
            inputId = "jobtitle",
            
            label = h4("Job Title"),
            choices = job_title_dropdown
          ),
          titlePanel(h4("Download PDF Report",style = "padding: 5px 14px 5px 14px;margin: 5px 5px 5px 0px;")),
          downloadButton('report',style = "color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 50px;")
        ),
        dashboardBody(
          br(),
          fluidRow(
            valueBoxOutput("averagesalary", width = 4),
            valueBoxOutput("RejectionRate", width = 4),
            valueBoxOutput("ApplicationNumber", width = 4)
          ),
          fluidRow(
            uiOutput("Top10company"),
            uiOutput("Top10city")
          ),
          fluidRow(
            uiOutput("Yearmean"),
            uiOutput("Yearrejection")
          )
        ) # end of dashboardBody
      ) # end of dashboardPage
    ),
    # end of Dashboard tabPanel
    # end of tabPanel("Dashboard")
    tabPanel(h4("Skill Matching"),
             dashboardPage(
               dashboardHeader(titleWidth = 0),
               dashboardSidebar(
                 titlePanel(h4("Resume Analysis",style = "padding: 5px 14px 5px 14px;margin: 5px 5px 5px 0px;")),
                 fileInput("doc",
                           "Upload Your Resume As .docx"),
                 selectInput(inputId = "jobDesired", label = "Select A Desired Job Profile",
                             choices=c("Financial Analyst", "Business Systems Analyst", "Data Engineer","Program Manager",
                                       "Management Analyst","Operations Manager","Data Analyst","Software Engineer", 
                                       "Business Analyst", "Data Scientist", "Project Manager", "Market Research Analyst", "Product Manager"),
                             multiple = FALSE,
                             selected = "Data Analyst")
               ),
               dashboardBody(
                 br(),
                 fluidRow(
                   uiOutput("cloud_resume")
                 ),
                 fluidRow(
                   uiOutput("cloud_recommendations")
                 ),
                 fluidRow(
                   column(dataTableOutput("resmatch"), width = 12)
                 )
               ) 
             )
    ),
    tabPanel(h4("Prediction"),
             dashboardPage(
               dashboardHeader(titleWidth = 0),
               dashboardSidebar(titlePanel(""),
                                radioButtons(inputId = "companySize", h4("Company Size"),
                                             c("Small" = "Small",
                                               "Medium" = "Medium",
                                               "Large" = "Large")),
                                numericInput("pwWAGE", h4("Wage"), 70000, min = 5000, max = 500000),
                                selectInput( inputId = "pwSkillLevel", label = h4("Skill Level"),
                                             choices = c("Level I","Level II","Level III","Level IV","Not Specified")),
                                selectInput( inputId = "region", label = h4("Region"),
                                             choices = c("Northeast","Midwest","West","South"))
                                # actionButton("predict_1", "Predict", icon("paper-plane"),
                                #              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
               ),
               dashboardBody(
                 fluidRow(
                   # The above can be deleted when all codes is finalized
                   valueBoxOutput("predictedRejectionRate", width = 12)
                   
                 )
               )
             ) # end of dashboardPage of Prediction
    ) # end of Prediction
  ) # end of navbarPage
) # end of fluidPage

### === All Server side Functons === 
getSalaryAverage <- function(jobTitle) {
  SarayAverage <- myFile %>%
    filter(grepl(jobTitle, JOB_TITLE, ignore.case = TRUE)) %>%
    summarize(DataAnalystmean = mean(PW_WAGE, na.rm = TRUE))
  averagesalaryvalue <- round(SarayAverage[1, 1], 2)
  return(c(as.character(averagesalaryvalue)))
}

getrejectionrate <- function(jobTitle) {
  # Denied case: 1
  # Certified case: 0
  RejectionRate <- myFile %>%
    filter(grepl(jobTitle, JOB_TITLE, ignore.case = TRUE))
  RejectionRate <-
    paste(as.character(round(
      count(RejectionRate, CASE_STATUS == 1)[2, 2] /
        (
          count(RejectionRate, CASE_STATUS == 1)[2, 2] + count(RejectionRate, CASE_STATUS ==
                                                                 0)[2, 2]
        ) * 100,2)), "%")
  return(as.character(RejectionRate))
}

getapplicationnumber <- function(jobTitle) {
  applicationnumber <- myFile %>%
    filter(grepl(jobTitle, JOB_TITLE, ignore.case = TRUE))%>%
    nrow
  return(as.character(applicationnumber[1]))
}

gettop10company <- function(jobTitle) {
  Top10company <- myFile %>%
    filter(grepl(jobTitle, JOB_TITLE, ignore.case = TRUE)) %>%
    count(EMPLOYER_NAME, sort = T) %>%
    arrange(desc(n))%>%
    head(10) 
  Top10company <- transform(Top10company, 
                            EMPLOYER_NAME = reorder(EMPLOYER_NAME, n))
  
  
}

gettop10city <- function(jobTitle) {
  Top10city <- myFile %>%
    filter(grepl(jobTitle, JOB_TITLE, ignore.case = TRUE))%>%
    count(EMPLOYER_CITY,EMPLOYER_STATE_CODE)
  
  Top10city <- Top10city %>% dplyr::mutate(CITY_STATE = paste(Top10city$EMPLOYER_CITY,Top10city$EMPLOYER_STATE_CODE))%>%
    arrange(desc(n))%>%
    head(10)
  Top10city <- transform(Top10city, 
                         CITY_STATE = reorder(CITY_STATE, n))
  Top10city<- Top10city %>%
    dplyr::rename(State=EMPLOYER_STATE_CODE)
}

getyearmean <- function(jobTitle) {
  Yearmean <- myFile %>%
    filter(grepl(jobTitle , JOB_TITLE, ignore.case = TRUE)) %>%
    group_by(year) %>%
    summarise(yearmean = mean(PW_WAGE, na.rm = TRUE))
}

getyearrejection <- function(jobTitle) {
  YearrejectionD <- myFile %>%
    filter(grepl(jobTitle, JOB_TITLE, ignore.case = TRUE),
           CASE_STATUS == 1) %>%
    group_by(year) %>%
    count(year)
  # YearrejectionD
  YearrejectionA <- myFile %>%
    filter(grepl(jobTitle, JOB_TITLE, ignore.case = TRUE),
           CASE_STATUS == 0) %>%
    group_by(year) %>%
    count(year)
  #YearrejectionA
  #Deal the case when a year has not AD or RJ case
  year <- yearAxisRange
  yearBaseDf <- data.frame(year)
  YearrejectionA <-left_join(yearBaseDf, YearrejectionA, by = "year")
  YearrejectionA[c("n")][is.na(YearrejectionA[c("n")])] <- 0
  YearrejectionD <-left_join(yearBaseDf, YearrejectionD, by = "year")
  YearrejectionD[c("n")][is.na(YearrejectionD[c("n")])] <- 0
  
  Yearrejection = cbind(YearrejectionD, YearrejectionA)
  rm(YearrejectionA, YearrejectionD)
  Yearrejection[3] <- NULL
  colnames(Yearrejection) <- c('year', 'Denied', 'Certified')
  Yearrejection <- Yearrejection %>%
    dplyr::mutate(rejectionrate = round((Denied / Certified)*100,2))
}

plotYearRejection <- function(jobTitle) {
  ggplot(getyearrejection(jobTitle), aes(x = year, y = rejectionrate)) + geom_point() + geom_line() + scale_x_continuous(breaks = yearAxisRange) + ylab("Average Rejection Rate(%)") + xlab("Year") + theme(text = element_text(face = "bold"))
}

## skill matching code

createLexicon <-function(data_clean, jobTitle ="Data Analyst"){
  skills_lexicon <- data_clean %>% filter(job_title == jobTitle)
  skills_lexicon <- skills_lexicon[, colSums(skills_lexicon != 0) > 0]
  skills_lexicon <- data.frame(colnames(skills_lexicon))
  skills_lexicon <- data.frame(skills_lexicon[2:nrow(skills_lexicon), ])
  colnames(skills_lexicon) <- "skill"
  return(skills_lexicon)
}

getResumeDetails <- function(resume, jobTitle){
  
  #data_clean=read_preprocess_data()
  #skills=preprocess(data_clean)
  for(i in 1:nrow(skills)){
    resume <- gsub(skills[i,1], skills_processed[i,1], tolower(resume))
  }
  
  resume_df<-data.frame(resume)
  colnames(resume_df) <- "word"
  resume_df <- resume_df %>%
    unnest_tokens(output=word, input=word) %>% mutate(word = str_extract(word, "^[a-zA-Z_']+"))
  resume_df <- resume_df %>% anti_join(stop_words) %>% drop_na()
  
  count_c<-resume_df %>%
    count(word, sort = TRUE) 
  skills_lexicon=createLexicon(data_clean, jobTitle)
  
  for(i in 1:nrow(count_c)){
    for(j in 1:nrow(skills_lexicon)){
      if(count_c[i, 1] == skills_lexicon[j, 1]){
        count_c[i, 2] = 2.5
      }
    }
  }
  count_c<-count_c %>% filter(n>1, sort=TRUE)
  return(count_c)
  
}

getMatch <- function(resume){
  for(i in 1:nrow(skills)){
    resume <- gsub(skills[i,1], skills_processed[i,1], tolower(resume))
  }
  
  resume_df<-data.frame(resume)
  colnames(resume_df) <- "word"
  resume_df <- resume_df %>%
    unnest_tokens(output=word, input=word) %>% mutate(word = str_extract(word, "^[a-zA-Z_']+"))
  resume_df <- resume_df %>% anti_join(stop_words) %>% drop_na()
  
  
  res <- resume_df %>%
    filter(word %in% colnames(df))
  
  df = df[,(res$word)]
  sums = rowSums(df)
  result = as.data.frame(cbind(sums, round(sums/tot, 6)))
  row.names(result) <- rn
  colnames(result)<- c("Number of Keywords", "Ratio of Keywords")
  return(result)
}

getRecommendations <- function(resume, jobTitle){
  # data_clean <- read_preprocess_data()
  # skills <- preprocess(data_clean)
  #skills_processed <- data.frame(tolower(gsub(" ", "_", skills$skill)))
  
  for(i in 1:nrow(skills)){
    resume <- gsub(skills[i,1], skills_processed[i,1], tolower(resume))
  }
  
  resume_df<-data.frame(resume)
  colnames(resume_df) <- "word"
  resume_df <- resume_df %>%
    unnest_tokens(output=word, input=word) %>% mutate(word = str_extract(word, "^[a-zA-Z_']+"))
  resume_df <- resume_df %>% anti_join(stop_words) %>% drop_na()
  
  count_c<-resume_df %>%
    count(word, sort = TRUE) 
  skills_lexicon=createLexicon(data_clean, jobTitle)
  
  for(i in 1:nrow(count_c)){
    for(j in 1:nrow(skills_lexicon)){
      if(count_c[i, 1] == skills_lexicon[j, 1]){
        count_c[i, 2] = 2.5
      }
      
    }
  }
  #recommendations
  skills_exist <- count_c %>% inner_join(skills_lexicon, by=c("word"="skill")) 
  recommendations <- skills_lexicon%>% anti_join(skills_exist, by=c("skill"="word"))
  
  recommendations <- data.frame(tolower(gsub("_", " ", recommendations$skill)))
  colnames(recommendations) <- "skill"
  recommendations <- recommendations %>% count(skill, sort=TRUE)
  return(recommendations)
}



getprediction <- function(companySize, pwWAGE, pwSkillLevel, region) {
  mytest  <-
    data.frame (
      COMPANY_SIZE = companySize,
      PW_WAGE = pwWAGE,
      PW_SKILL_LEVEL = pwSkillLevel,
      Region = region
    )
  predictedResult <- predict(fit1, mytest, type = "response")
  percentage_num <- round(predictedResult * 100, 2)
  return (paste(as.character(percentage_num), "%"))
} 

### === Application Server === 
server <- function(input, output) {
  
  data_source <- reactive({
    data <- input_file()
    return(data)
  })
  
  input_file <- reactive({
    if (is.null(input$doc)) {
      return("")
    }
    textreadr::read_docx(input$doc$datapath)
    #readLines(input$doc$datapath)
  })
  
  reportExport <- reactiveValues(Top10company = pdfReport, Top10Cities = pdfReport, Yearmean = pdfReport, Yearrejection = pdfReport)
  
  ## == Dashboard ==
  # Stats
  output$averagesalary <- renderValueBox({
    getSalaryAverage(input$jobtitle) %>%
      valueBox("Average Salary",
               #icon = icon("money-check-alt"),
               icon = tags$i(class = "fas fa-money-check-alt", style="font-size: 54px; color: white"),
               color = "purple")
    
  })
  
  output$RejectionRate <- renderValueBox({
    getrejectionrate(input$jobtitle) %>%
      valueBox("Average Rejection Rate", icon = tags$i(class = "fas fa-times-circle", style="font-size: 54px; color: white"),  color =
                 "red")
  })
  
  output$ApplicationNumber <- renderValueBox({
    getapplicationnumber(input$jobtitle) %>%
      valueBox("Number of Visa Applications",
               icon = tags$i(class = "fas fa-poll", style="font-size: 54px; color: white"),
               color = "yellow")
  })
  
  output$ApplicationNumber <- renderValueBox({
    getapplicationnumber(input$jobtitle) %>%
      valueBox("Number of Visa Applications",
               icon = tags$i(class = "fas fa-poll", style="font-size: 54px; color: white"),
               color = "yellow")
  })
  
  # Plots
  output$Top10company <- renderUI({
    box(
      title = "Top 10 Companies",
      status = "primary",
      width = 6,
      solidHeader = TRUE
      ,
      collapsible = TRUE,
      renderPlot({
        reportExport$Top10company <- ggplot(
          gettop10company(input$jobtitle),
          aes(x = EMPLOYER_NAME, y = n , fill = EMPLOYER_NAME)
        ) +
          coord_flip() + geom_bar(stat = "identity") + theme(
            legend.position = "none",
            axis.text.y = element_blank(),
            text = element_text(face = "bold")
          ) +
          geom_text(
            aes(y = 0, label = paste(EMPLOYER_NAME, "(", n, ")")),
            hjust = 0,
            fontface = "bold"
          ) +
          ylab("Number of Visa Applications") +
          xlab("Employer")
        reportExport$Top10company
      }
      )
    )
  })
  
  output$Top10city <- renderUI({
    box(
      title = "Top 10 Cities",
      status = "primary",
      width = 6,
      solidHeader = TRUE
      ,
      collapsible = TRUE,
      renderPlot({ reportExport$Top10Cities <- ggplot(gettop10city(input$jobtitle), aes(x = n, y = CITY_STATE,fill=State)) + geom_bar(stat ="identity") + theme(text=element_text(face="bold"),legend.position=c(0.9, 0.4)) + ylab("Employer City")+ xlab("Number of Foreign Workers")
      reportExport$Top10Cities})
    )
  })
  
  output$Yearmean <- renderUI({
    box(
      title = "Average Wage by Year",
      status = "primary",
      width = 6,
      solidHeader = TRUE
      ,
      collapsible = TRUE,
      renderPlot({
        reportExport$Yearmean <- ggplot(getyearmean(input$jobtitle), aes(x = year, y = yearmean)) + geom_point() + geom_line() + scale_x_continuous(breaks = yearAxisRange) + ylab("Average Wage") + xlab("Year") + theme(text = element_text(face = "bold"))
        reportExport$Yearmean
      })
    )
  })
  
  output$Yearrejection <- renderUI({
    box(
      title = "Average Rejection Rate by Year",
      status = "primary",
      width = 6,
      solidHeader = TRUE,
      collapsible = TRUE,
      renderPlot({reportExport$Yearrejection <- plotYearRejection(input$jobtitle)
      reportExport$Yearrejection})
    )
  })
  
  # PDF Report
  output$report = downloadHandler(
    filename = function() {
      "DashboardReport.pdf"
    },
    content = function(file) {
      pdf(file, onefile = TRUE)
      grid.arrange(
        reportExport$Top10company + ggtitle('Top 10 Companies'),
        reportExport$Top10Cities + ggtitle('Top 10 Cities'),
        reportExport$Yearmean + ggtitle('Average Wage by Year'),
        reportExport$Yearrejection + ggtitle('Average Rejection Rate by Year'),
        ncol = 2,
        nrow = 2,
        top = "Skills and Employment Matching - Dashboard Report"
      )
      dev.off()
    }
  )
  ## == End of Dashboard ==
  
  ## == Start of Skill Matching Dashboard ==
  output$cloud_resume <- renderUI(
    box(
      title = "What Stands Out In Your Resume...",
      status="primary",
      width = 12,
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,
      renderWordcloud2(
        wordcloud2(data=getResumeDetails(data_source(), input$jobDesired), size=0.7, shape="rectangle",color="random-dark",
                   fontFamily = "serif", fontWeight= "600")
      )
    )
  )
  
  output$resmatch <- renderDataTable({
    getMatch(data_source())
  })
  
  output$cloud_recommendations <- renderUI(
    box(
      title = "Recommendations Of Skills To Include, Based On The Desired Job Profile..",
      status="primary",
      width = 12,
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,
      renderWordcloud2(
        wordcloud2(data=getRecommendations(data_source(), input$jobDesired), size=0.3, shape="rectangle",color="random-dark",
                   fontFamily = "serif", fontWeight= "600", minRotation = 0, maxRotation = 0, rotateRatio = 1)
      )
    )
  )
  ## == Prediction ==
  output$predictedRejectionRate <- renderValueBox({getprediction(input$companySize ,
                                                                 input$pwWAGE,
                                                                 input$pwSkillLevel,
                                                                 input$region) %>%
      valueBox(
        "Predicted Rejection Rate",
        icon = tags$i(class = "fas fa-times-circle", style = "font-size: 54px; color: white"),
        color = "red"
      )
  })
  
  ## == End of Prediction ==
  
}

shinyApp(ui, server)
