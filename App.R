## Load Packages
library(tidyverse)
library(ggpubr)
library(shiny)
library(plotly)
library(DT)
library(shinythemes)
library(shinyBS)

## define global variables and clean the data
url <- "https://docs.google.com/spreadsheets/d/1fxnSOjqDUzl47mKgiQy4kNZ6gKNrVqXb4-51kpFtD6k/export?format=csv&id=1fxnSOjqDUzl47mKgiQy4kNZ6gKNrVqXb4-51kpFtD6k&gid=1046519671"
## "https://docs.google.com/spreadsheets/d/e/2PACX-1vSPMVnMVJKsheRuoN-uFL0rz0KeQVzw2wvzCLCT0tlGubsRr3Jy221XSJWRf64cgfvQH5t-D_O8_Xow/pub?gid=69416159&single=true&output=csv"
data <- read.csv(url,stringsAsFactors=FALSE,na.strings=c("","NA"))[1:6]

data$Your.JWA.name <- str_trim(data$Your.JWA.name, side ="right")
data$Timestamp <- as.POSIXct(data$Timestamp, format = "%m/%d/%Y %H:%M:%S")

Alliances <- c(data$Alliance %>% unique())[-1]

excl <- c('Titanoboa (epic)', 'Blue', 'Diplod', 'Woolly Mammoth', 'Allo g2 (epic)', 'Dsunga','Megaloceros (Deer)','Nasuto')

## define global functions
dataFreq <- function(x){
  data <- data %>% 
    filter(Timestamp > x) %>%
    group_by(Alliance,Your.JWA.name) %>%
    arrange(Timestamp) %>%
    filter(row_number()==n()) %>% 
    ungroup() %>% 
    arrange(Timestamp) %>% as.data.frame()
  
  
  origData <- data[,-1]
  names(origData) <- c("IGN","Alliance","st","ed","rd")
  origData[which(origData$st == origData$ed & is.na(origData$rd)),5] <- origData[which(origData$st == origData$ed & is.na(origData$rd)),3]
  origData[which(is.na(origData$ed) & is.na(origData$rd)),c(4,5)] <- origData[which(is.na(origData$ed) & is.na(origData$rd)),c(3,3)]
  ##
  freqTotal <- as.data.frame(table(as.matrix(origData[,3:5],ncol = 1)))
  freqRank1 <- as.data.frame(table(as.matrix(origData[,3],ncol = 1)))
  freqRank2 <- as.data.frame(table(as.matrix(origData[,4],ncol = 1)))
  freqRank3 <- as.data.frame(table(as.matrix(origData[,5],ncol = 1)))
  ####Freq Plot
  rank1 <- origData %>%
    select(starts_with('s')) %>%
    mutate(rank = 1) %>%
    rename(Dino = st)
  rank2 <- origData %>%
    select(starts_with('e')) %>%
    mutate(rank = 2) %>%
    rename(Dino = ed)
  rank3 <- origData %>%
    select(starts_with('r')) %>%
    mutate(rank = 3) %>%
    rename(Dino = rd)
  
  Freq <- rbind(rank1,rank2,rank3) %>%
    filter(!is.na(Dino)) %>%
    mutate(freq = 1) %>%
    left_join(freqTotal,by= c('Dino' = 'Var1')) %>%
    left_join(freqRank1,by= c('Dino' = 'Var1')) %>%
    left_join(freqRank2,by= c('Dino' = 'Var1')) %>%
    left_join(freqRank3,by= c('Dino' = 'Var1'))
  
  Freq[is.na(Freq)] <- 0
  
  return(Freq)
}

#List

listCreate <- function(Freq,q,w,rmv) {
  Freq <-Freq %>%
      mutate(weight = Freq.y*(q+0.1)+Freq.x.x*2.01+Freq.y.y) %>% 
      mutate(weight = ifelse(Dino %in% excl, weight*1.1 , weight))  
  

  resultWeight <- Freq[c(1,8)] %>% distinct() %>% arrange(desc(weight))
  names(resultWeight) <- c('Var1','n')
  
  # resultWeight <- if (rmv == TRUE) {resultWeight %>% filter(n > 4.1)} else {resultWeight}
  
  resultWeight2 <- resultWeight %>% add_column(ind = 'x')
  top1 <- if (rmv == FALSE) {unique(c((Freq %>% filter(rank == 1))[,1]))} else
    {unique(c((Freq %>% filter(rank == 1) %>% filter(weight>4.1) )[,1]))}
  
  
  x <- length(top1)
  indexDouble <- which((resultWeight[,1] %>%
                          unlist() %>%
                          as.character() %in% top1))[1:(69-x-w)]
  indexSingleTop1 <-which((resultWeight[,1] %>%
                             unlist() %>%
                             as.character() %in% top1))[(61-x-w):x]
  if(w>0){
    indexSingleNoTop1 <- which((!resultWeight[,1] %>%
                                  unlist() %>%
                                  as.character() %in% top1))[1:w]
    resultWeight2[indexDouble,3] <- 'double'
    resultWeight2[indexSingleTop1,3] <- 'single1'
    resultWeight2[indexSingleNoTop1,3] <- 'single0'
  }else{
    resultWeight2[indexDouble,3] <- 'double'
    resultWeight2[indexSingleTop1,3] <- 'single1'
  }
  double <- resultWeight2[indexDouble,]
  single <- resultWeight2 %>%
    filter(ind %in% c('single1','single0'))
  listA_single <- single[seq(1,nrow(single)-1,by = 2),]
  listB_single <- single[seq(2,nrow(single),by = 2),]
  ListA <- union(double,listA_single) %>%  as.data.frame()
  ListB <- union(double,listB_single) %>%  as.data.frame()
  names(ListA) <- c('List1','Weight','Status')
  names(ListB) <- c('List2','Weight','Status')
  ListA <<-ListA
  ListB <<-ListB
}


## Define UI for dataset viewer app ----
ui <- 
  
  fluidPage(theme = shinytheme("cyborg"),
            fluidRow(column(3,
                            wellPanel(h2("Control Panel"),
                                      # actionButton("reload","Reload Data"),
                                      img(src='Dinosaur.jpg',  height = 100, width = 160),
                                      dateInput('date',
                                                label = 'Starting Date',
                                                value = Sys.Date()-60),
                                      
                                      hr(),
                                      h3("Data Viewer:"),
                                      selectInput("Alliance","Alliance",c(Alliances,"All"),selected = "ApexPredators"),
                                      checkboxInput("dup", "Remove Duplicate", FALSE),
                                      radioButtons("radio", label = "1st Choices Plots",
                                                   choices = list("ON" = 1, "OFF" = 2), 
                                                   selected = 1),
                                      hr(),
                                      h3("List Creator:"),
                                      sliderInput("y","Number of Dinos with Zero 1st.Choices",
                                                  min=0,max=6,value=0,step=2),
                                      numericInput('rank1', 'Weight of 1st Choice', value = 4, min = 0, max = 100, step = 1,
                                                   width = 135),
                                      checkboxInput("rmv", "Remove Single Vote", TRUE),
                                      actionButton("List","Generate Lists"),
                                      downloadButton("downloadData", "Download")
                            ),
                            sidebarPanel(h3("Notes:"),br(),HTML("<ul><li><b>[Starting Date]:</b> Choose the first day of current round of poll to exclude old votes</li>
                                                                       <li><b>[Choose Alliance]:</b> Filter by alliance; can choose 'All'</li>
                                                                       <li><b>[Remove Duplicate]:</b> Only keep the latest vote for each name</li>
                                                                       <li><b>[Number of Dinos with 0 1st.Choices]:</b> Choose the number of dinos that has no 1st choice vote you want to include in the to be created lists</li>
                                                                       <li><b>[Weight]:</b> 1st.Choices*(4+0.1) + 2ed.Choices*(2+0.01) + 3rd.Choices*1, exclusive dinos have an extra multiplier of 1.1  </li>
                                                                       <li><b>[Lists Creating]:</b> 1. rank dinos by weight 2. decide number of dinos not in the column 1st.Choices from [Number of Dinos with 0 1st.Choices]
                                                                        3. every dino in column 1st.Choice be at least singled 4. double to fill the rest of slots</li>
                                                                       <li><b>[Remove Single Vote]:</b> Remove those 'single1' with lower than 4.1 weight  </li></ul>"),width=15)
            ),              
            column(9,
                   tabsetPanel(tabPanel("Data Viewer",
                                        mainPanel(
                                          
                                          ### add your style inline css values here
                                          
                                          ### added a line of code here too `.dataTables_wrapper .dataTables_paginate .paginate_button.current:hover `###
                                          tags$style(HTML("
                    .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate, .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
                    color: #ffffff;
                    }

                    .dataTables_wrapper .dataTables_paginate .paginate_button{box-sizing:border-box;display:inline-block;min-width:1.5em;padding:0.5em 1em;margin-left:2px;text-align:center;text-decoration:none !important;cursor:pointer;*cursor:hand;color:#ffffff !important;border:1px solid transparent;border-radius:2px}


                    .dataTables_length select {
                           color: black;
                           background-color: white
                           }

              ###To change text and background color of the `Search` box ###
                    .dataTables_filter input {
                            color: #0E334A;
                            background-color: #0E334A
                           }

                    thead {
                    color: #ffffff;
                    }

                     tbody {
                    color: #000000;
                    }

                   "
                                                          
                                                          
                                          ))
                                        ),
                                        dataTableOutput('Votes'),
                                        #list(tags$head(tags$style("shipment.table span {color: #333333 ; background: #999999;}")))   
                                        #tags$head(tags$style("#shipment.table table {color: red;}"))  ,
                                        hr(),
                                        hr(),
                                        conditionalPanel('input.radio == "1"',plotOutput("Each",width = "100%", height = "800px"))
                   ),
                   
                   
                   tabPanel("List Creator",
                            plotlyOutput("Freq",width = "100%", height = "800px"),
                            hr(),
                            fluidRow(column(1,
                                            ""
                            ),
                            hr(),
                            column(4,
                                   dataTableOutput("listA")
                            ),
                            column(2,
                                   ""
                            ),
                            column(4,
                                   dataTableOutput("listB")
                            )
                            ),column(1,
                                     ""
                            )
                   )
                   )
            )
            )
  )




# Define server logic to input four locators, database and retrieve data.
server <- function(input, output) {
  observeEvent(input$reload,{url <- "https://docs.google.com/spreadsheets/d/1fxnSOjqDUzl47mKgiQy4kNZ6gKNrVqXb4-51kpFtD6k/export?format=csv&id=1fxnSOjqDUzl47mKgiQy4kNZ6gKNrVqXb4-51kpFtD6k&gid=1046519671"
  data <- read.csv(url,stringsAsFactors=FALSE,na.strings=c("","NA")) 
  
  data$Your.JWA.name <- str_trim(data$Your.JWA.name, side ="right")
  data$Timestamp <- as.POSIXct(data$Timestamp, format = "%m/%d/%Y %H:%M:%S")
  data <<- data})
  ##weighted plot##

  observe({
    Freq <- dataFreq(input$date)
    q <- as.numeric(input$rank1)
    Freq <- Freq%>%
      mutate(weight = Freq.y*(q+0.1)+Freq.x.x*2.01+Freq.y.y)
      FreqP <- ggplot(data=Freq, aes(x= reorder(Dino,weight),freq, fill=rank)) +
      geom_bar(stat="identity")+
      # geom_text(aes(label=Freq), hjust=-0.5) +
      geom_bar(stat ="identity") +
      # scale_y_sqrt() +
      coord_flip() +
      xlab("") + theme(legend.position="none") +
      ylab("Number of Votes")
    output$Freq <- renderPlotly(FreqP)
  })
  
  
  ##raw votes##
  
  observe({
    opt <- if(input$radio == 1) {list(
      pageLength = 10)} else {list(
        pageLength = 25)}
    
    output$Votes <- renderDT(
      if(input$Alliance == "All") {
        if(input$dup == TRUE) { 
          return(data %>%
                   filter(Timestamp > input$date) %>%
                   group_by(Alliance,Your.JWA.name) %>%
                   arrange(Timestamp) %>%
                   filter(row_number()==n()) %>% 
                   ungroup() %>% arrange(desc(Timestamp))) }  else {
                     return (data %>%
                                filter(Timestamp > input$date)%>% 
                                group_by(Alliance,Your.JWA.name) %>%
                                  # mutate(dupFlag = ifelse(n()>1,1,0)) %>% 
                                arrange(desc(Timestamp))
                             # %>% formatStyle(
                             #                  'dupFlag',
                             #                   backgroundColor = styleEqual(1, 'red'))
                                             )
                   }
      }else{
        if(input$dup == TRUE) {
          return(data %>%
                   filter(Timestamp > input$date) %>% 
                   filter(Alliance == input$Alliance) %>% 
                   group_by(Alliance,Your.JWA.name) %>%
                   arrange(Timestamp) %>%
                   filter(row_number()==n()) %>% 
                   ungroup() %>% arrange(desc(Timestamp))) } else {
                     return(data %>%
                              filter(Timestamp > input$date) %>% 
                              filter(Alliance == input$Alliance) %>% 
                                group_by(Alliance,Your.JWA.name) %>% 
                                # mutate(dupFlag = ifelse(n()>1,1,0)) %>% 
                                arrange(desc(Timestamp))
                            # %>% formatStyle(
                            #       'dupFlag',
                            #       backgroundColor = styleEqual(1, 'red'))
                     )
                     }
        
      },filter = "bottom",
      options = opt
    )
  })
  
  ###plot each alliance #1
  
  observe({
    output$Each  <- renderPlot({
      ##
      if(input$radio == 1) {
        data <- data %>%
          filter(Timestamp > input$date) %>%
          group_by(Alliance,Your.JWA.name) %>%
          arrange(Timestamp) %>%
          filter(row_number()==n()) %>% 
          ungroup() %>% 
          arrange(Timestamp) 
        origData <- data[,-1]
        names(origData) <- c("IGN","Alliance","st","ed","rd")
        origData[which(origData$st == origData$ed & is.na(origData$rd)),5] <- origData[which(origData$st == origData$ed & is.na(origData$rd)),3]
        origData[which(is.na(origData$ed) & is.na(origData$rd)),c(4,5)] <- origData[which(is.na(origData$ed) & is.na(origData$rd)),c(3,3)]
        list_alliance <- group_split(origData,Alliance)
        
        each <- function(x) {
          ggplot(as.data.frame(table(c(x[,3]))),aes(x= reorder(Var1,Freq),Freq)) +
            geom_bar(stat ="identity",alpha = if(input$Alliance == "All") {1} 
                     else if(x %>% select(starts_with("All")) %>% unique() == input$Alliance) {1} 
                     else {0.4}) +
            coord_flip() +
            geom_text(aes(label=Freq), hjust=1,color = 'white')+
            xlab("") +
            ylab(x$Alliance[1])
        }
        rank1Each <- lapply(list_alliance,each)
        return(ggarrange(rank1Each[[1]],rank1Each[[2]],rank1Each[[3]],rank1Each[[4]],rank1Each[[5]],rank1Each[[6]],rank1Each[[7]],rank1Each[[8]],
                         nrow = 1))
      } else
      {return(NULL)}
    })
  })
  
  ##Create lists##
  
  observeEvent(input$List,{
    w <- input$y
    Freq <- dataFreq(input$date)
    q <- as.numeric(input$rank1)
    rmv <- input$rmv
    
    listCreate(Freq,q,w,rmv)
    
    output$listA <- renderDT(ListA,
                             options = list(
                               pageLength = 25))
    output$listB <- renderDT(ListB,
                             options = list(
                               pageLength = 25))
  })
  
  ##download lists##
  
  observe({
    output$downloadData <- downloadHandler(
      # This function returns a string which tells the client
      # browser what name to use when saving the file.
      filename = function() {
        paste("Lists", Sys.Date(), ".csv", sep="")
      },
      # This function should write data to a file given to it by
      # the argument 'file'.
      content = function(file) {
        w <- input$y
        Freq <- dataFreq(input$date)
        q <- as.numeric(input$rank1)
        rmv <- input$rmv
        
        listCreate(Freq,q,w,rmv)
        
        Lists <- cbind(ListA,'',ListB)
        # Write to a file specified by the 'file' argument
        write.csv(Lists, file)
      }
    )})
}
# Run the application
shinyApp(ui = ui, server = server)
