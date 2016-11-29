

# load the shinydashboard package
require(shiny)
require(shinydashboard)
require(ggplot2)
require(dplyr)
require(jsonlite)
require(xml2)
require(lubridate)
require(scales)




header <- dashboardHeader(title = "iTunes App Dashboard") 

sidebar <- dashboardSidebar(
  sidebarMenu(
    
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Github", icon = icon("question-sign",lib='glyphicon'), 
             href = "https://github.com/amrrs/")
  )
)

frow1 <- fluidRow(
  tags$img(src='http://www.nateharasim.com/wp-content/uploads/2015/07/itunes-logo-png-transparent.png',width='200px')
  ,valueBoxOutput("LatestVersion")
  ,valueBoxOutput("avgRating")
  
  
)

frow2 <- fluidRow(
  
  
  box(
    title = "Daily Ratings % Split from Reviews"
    ,status = "warning"
    ,solidHeader = TRUE 
    #,collapsible = TRUE 
    ,plotOutput("dailyRating", height = "300px")
    ,width = 12
    
  )
  
)


frow3 <- fluidRow(
  box(
    title = "Daily Average Ratings from Reviews"
    ,status = "primary"
    ,solidHeader = TRUE 
    #,collapsible = TRUE 
    ,plotOutput("dailyRatingLine", height = "300px")
    ,width = 12
    
  )
)

# data table to view the raw data
frow4 <- fluidRow(
  tabBox(
    title = "Data Table"
    ,width = 10
    ,id = "dataTabBox"
    ,tabPanel(
      title = "Ratings"
      ,dataTableOutput("dailyAvgRatings")
    )
    ,tabPanel(
      title = "Reviews"
      ,dataTableOutput("Reviews")
    ) 
  )
)




# combine the three fluid rows to make the body
body <- dashboardBody(frow1, frow2, frow3, frow4)

ui <- dashboardPage(header, sidebar, body, skin='green')

# create the server functions for the dashboard  
server <- function(input, output) { 
  
  
  #reviews extraction code
  ##################
  
  
  app_id = '297606951' #amazon
  extract_reviews <- function(app_id,page_num){
    
    
    #building_url
    
    
    json_url <- paste0('http://itunes.apple.com/us/rss/customerreviews/page=',page_num,'/id=',app_id,'/sortby=mostrecent/','json')
    
    xml_url <- paste0('http://itunes.apple.com/us/rss/customerreviews/page=',page_num,'/id=',app_id,'/sortby=mostrecent/','xml')
    
    
    #json_url <- 'http://itunes.apple.com/gb/rss/customerreviews/id=370901726/sortBy=mostRecent/json'
    
    js <- fromJSON(json_url)
    
    reviews <- cbind(Title = js$feed$entry$title$label,Author_URL = js$feed$entry$author$uri,Author_Name = js$feed$entry$author$name,App_Version = js$feed$entry$`im:version`$label,Rating = js$feed$entry$`im:rating`$label,Review = js$feed$entry$content$label)
    
    logo <- js$feed$entry$`im:image`[[1]][[1]][3]
    
    reviews <- reviews[-1,]
    
    #names(reviews) <- c('Title','Author_URL','Author_Name','App_Version','Rating','Review')
    
    #reading xml for date
   
    xml_n <- read_xml(xml_url)
    
    
    entries <- xml_children(xml_n)[xml_name(xml_children(xml_n))=='entry']
    
    entries <- entries[-1]
    
    #extrcting date from entries 
    
    date <- xml_text(xml_children(entries))[xml_name(xml_children(entries))=='updated']
    
    reviews$Date <- with_tz(strptime(date,format='%Y-%m-%dT%H:%M:%S',tz='America/Los_Angeles'),tzone='Europe/London') 
    
    return(reviews)
    
  }
  
  
  reviews1 <- extract_reviews(app_id,1)
  reviews2 <- extract_reviews(app_id,2)
  reviews3 <- extract_reviews(app_id,3)
  #reviews4 <- extract_reviews(app_id,4)
  
  reviews <- rbind(reviews1,reviews2,reviews3)#,reviews4)
  
  #re-arraning column order
  
  reviews <- reviews[,c(7,4,5,1,6,3,2)]
  
  
  myvfreviews <- reviews
  
  myvfreviews$Date <- as.POSIXct(myvfreviews$Date)
  
  
  total.ratings <- myvfreviews %>% group_by(App_Version) %>% summarise(value=mean(as.numeric(Rating)))
  
  
  
  output$LatestVersion <- renderValueBox({
    
    valueBox(
      as.character(myvfreviews$App_Version[1])
      ,paste('Latest App Version:')
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "purple")  
    
  })
  
  
  
  output$avgRating <- renderValueBox({
    
    valueBox(
      formatC(total.ratings$value, format="d", big.mark=',')
      ,'Average Rating'
      ,icon = icon("gbp",lib='glyphicon')
      ,color = "green")
    
  })
  
   
  
  output$dailyRating <- renderPlot({
    
    myvfreviews %>% group_by(Date=as.Date(as.POSIXct(Date)),Rating=as.factor(Rating)) %>% filter(Date>Sys.Date()-30) %>% summarise(count=n()) %>% mutate(freq = round((count / sum(count)),2)) %>%
      ggplot(data=.,aes(x=as.Date(Date),y=freq,fill=Rating)) + geom_bar(position='fill', stat="identity")  + scale_y_continuous(labels = percent) + geom_text(aes(label=paste0(freq*100,'%'), y=freq),position='stack',vjust=2.2) +
      scale_fill_manual(values = c(rgb(211,98,89,maxColorValue = 255),rgb(239, 126, 20,maxColorValue = 255),rgb(255, 193, 5,maxColorValue = 255),rgb(191, 208, 71,maxColorValue = 255),rgb(14, 157, 88,maxColorValue = 255))) +
      xlab('Date') + ylab('Ratings Percentage %')
    
  })
  
   
  
  
  output$dailyRatingLine <- renderPlot({
    
    myvfreviews %>% group_by(Date = as.Date(as.POSIXct(Date))) %>% filter(Date>Sys.Date()-30) %>% summarise(Avg_Rating=mean(as.numeric(Rating))) %>% 
      ggplot(data=.,aes(x=Date,y=Avg_Rating)) + geom_line(color='blue') + geom_point(color='black')  + geom_text(aes(label=paste0(round(Avg_Rating,2))),vjust=1)+
      xlab('Date') + ylab('Average Daily Rating')
    
  })
  
  
  output$dailyAvgRatings <- renderDataTable(myvfreviews %>% group_by(as.Date(Date)) %>% summarise(value=mean(as.numeric(Rating))))
  output$Reviews <- renderDataTable(myvfreviews[,c(1,2,3,4,5)])
  

}


shinyApp(ui, server)