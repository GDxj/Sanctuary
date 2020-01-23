library(tidyverse)
library(ggpubr)
library(shiny)
library(plotly)
library(DT)
##Global variables
url <- "https://docs.google.com/spreadsheets/d/1fxnSOjqDUzl47mKgiQy4kNZ6gKNrVqXb4-51kpFtD6k/export?format=csv&id=1fxnSOjqDUzl47mKgiQy4kNZ6gKNrVqXb4-51kpFtD6k&gid=1046519671"
data <- read.csv(url,stringsAsFactors=FALSE,na.strings=c("","NA")) 

data$Your.JWA.name <- str_trim(data$Your.JWA.name, side ="right")
data$Timestamp <- as.POSIXct(data$Timestamp, format = "%m/%d/%Y %H:%M:%S")

Alliances <- c(data$Alliance %>% unique())

##function
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
    left_join(freqTotal,by= c('Dino' = 'Var1'))
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
# Define UI for dataset viewer app ----
ui <- fluidPage(
  fluidRow(
    column(3,
           h3("Lists Generator"),
           dateInput('date',
                     label = 'Start Date of Poll',
                     value = Sys.Date()-30),
           numericInput('rank1', 'Weight#1', value = 4, min = 0, max = 100, step = 1,
                        width = 100),
           sliderInput("y","Number of dinos without #1's",
                       min=0,max=6,value=0,step=2),
           actionButton("Plot","Plot"),
           actionButton("List","List"),
           downloadLink("downloadData", "Download")
           
    ),
    column(9,
           selectInput("Alliance","Choose Alliance",c(Alliances,"All"),selected = "All"),
           dataTableOutput("Votes")
    )
  ),
  hr(),
  plotlyOutput("Freq",width = "100%", height = "800px"),
  hr(),
  fluidRow(
    column(5,
           dataTableOutput("listA")
    ),
    column(2,
           ""
    ),
    column(5,
           dataTableOutput("listB")
    )
  ),
  hr(),
  plotOutput("Each",width = "100%", height = "800px")
)
# Define server logic to input four locators, database and retrieve data.
server <- function(input, output) {
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
      xlab("") +
      ylab("Frequency")
    output$Freq <- renderPlotly(FreqP)
  })
  ###raw votes
  observe({
    output$Votes <- renderDT(if(input$Alliance == "All") {
      return(data %>%
               filter(Timestamp > input$date))
    }else{
      return(data %>%
               filter(Timestamp > input$date)%>% 
               filter(Alliance == input$Alliance))
    },filter = "bottom",
    options = list(
      pageLength = 10
    ))
  })
  ###plot for each alliance #1
  observe({
    output$Each  <- renderPlot({
      ##
      if(input$Plot %% 2 == 1) {
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
                     else {0.5}) +
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
  observeEvent(input$List,{
    ##
    Freq <- dataFreq(input$date)
    q <- as.numeric(input$rank1)
    Freq <- Freq %>%
      mutate(weight = Freq.y*(q+0.1)+Freq.x.x*2.01+Freq.y.y)
    resultWeight <- Freq[c(1,8)] %>% distinct() %>% arrange(desc(weight))
    names(resultWeight) <- c('Var1','n')
    y <- input$y
    resultWeight2 <- resultWeight %>% add_column(ind = 'x')
    top1 <- unique(c((Freq %>% filter(rank == 1))[,1]))
    x <- length(top1)
    indexDouble <- which((resultWeight[,1] %>%
                            unlist() %>%
                            as.character() %in% top1))[1:(48-x-y)]
    indexSingleTop1 <-which((resultWeight[,1] %>%
                               unlist() %>%
                               as.character() %in% top1))[(49-x-y):x]
    if(y>0){
      indexSingleNoTop1 <- which((!resultWeight[,1] %>%
                                    unlist() %>%
                                    as.character() %in% top1))[1:y]
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
    ListA <- union(double,listA_single)
    ListB <- union(double,listB_single)
    names(ListA) <- c('List1','Weight','Status')
    names(ListB) <- c('List2','Weight','Status')
    
    output$listA <- renderDT(ListA,
                             options = list(
                               pageLength = 25))
    output$listB <- renderDT(ListB,
                             options = list(
                               pageLength = 25))
  })
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
        Freq <- dataFreq(input$date)
        Freq[is.na(Freq)] <- 0
        q <- as.numeric(input$rank1)
        Freq <- Freq%>%
          mutate(weight = Freq.y*(q+0.1)+Freq.x.x*2.01+Freq.y.y)
        resultWeight <- Freq[c(1,8)] %>% distinct() %>% arrange(desc(weight))
        names(resultWeight) <- c('Var1','n')
        y <- input$y
        resultWeight2 <- resultWeight %>% add_column(ind = 'x')
        
        top1 <- unique(c((Freq %>% filter(rank == 1))[,1]))
        
        x <- length(top1)
        indexDouble <- which((resultWeight[,1] %>%
                                unlist() %>%
                                as.character() %in% top1))[1:(48-x-y)]
        indexSingleTop1 <-which((resultWeight[,1] %>%
                                   unlist() %>%
                                   as.character() %in% top1))[(49-x-y):x]
        if(y>0){
          indexSingleNoTop1 <- which((!resultWeight[,1] %>%
                                        unlist() %>%
                                        as.character() %in% top1))[1:y]
          
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
        ListA <- union(double,listA_single)
        ListB <- union(double,listB_single)
        names(ListA) <- c('List1','Weight','Status')
        names(ListB) <- c('List2','Weight','Status')
        Lists <- cbind(ListA,'',ListB)
        # Write to a file specified by the 'file' argument
        write.csv(Lists, file)
      }
    )})
}
# Run the application
shinyApp(ui = ui, server = server)