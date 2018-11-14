
options(shiny.maxRequestSize=30*1024^2)
library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(lattice)
library(shinythemes)

df<-read.csv('directory.csv')

shinyApp(
  
  ################  UI PART #########################
  ui = tagList(
    #    shinythemes::themeSelector(),
    titlePanel("Starbucks Worldwide"),
    navbarPage(
      # Set the theme
      theme = shinythemes::shinytheme("sandstone"),
      "Shiny Project",
      # Tab 1 Problem Description
      tabPanel("Problem Description",
               mainPanel(
                 p("For every American, coffee is an indispensable item for everyday life and work. Starbucks has become the choice of most people, so how many stores does Starbucks open in the United States? Where else in the world?"),
                 p("In this project, I use the dataset 'Starbucks Locations Worldwide' to explore the worldwide locations about the Starbucks. If you want to know more about this dataset, Please visit:",a("Starbucks Locations Worldwide_kaggle_dataset", href = "https://www.kaggle.com/starbucks/store-locations")),
                 p("This dataset includes a record for every Starbucks or subsidiary store location currently in operation as of February 2017."),
                 p("So do you have the answers: How many Starbucks stores are there in the world by 2017? Do you know Top 10 countries that have highest number of Starbucks stores? And what about Top 10 cities?"),
                 strong("Now, let's click the 'Interactive Map' and 'Data Explorer' to find out the answers."),
                 p(strong("Enjoy exploring!"))
               )),
      
      # Tab 2 Interactive Map
      tabPanel("Interactive Map", 
               sidebarLayout(
                 sidebarPanel(
                   # Create a file input
                   fileInput("file","Choose a CSV File Please:",
                             multiple = TRUE,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")),
                   helpText('Data from Starbucks Locations Worldwide_kaggle_dataset'),
                   
                   # Create a multiple checkbox input for continent
                   checkboxGroupInput("Continent",
                                      "Continent:",
                                      choices = list("Asia"= "Asia","Europe"= "Europe",
                                                     "America"="America","Australia"="Australia",
                                                     "Africa"="Africa","Pacific"="Pacific")),
                                      
                   # Choices for drop-downs
                   vars <- c("Ownership.Type"= "ownership","Brand" = "brand"),
                   
                   # Create a select bar
                   helpText('You may change the color here!'),
                   selectInput("color", "Color", vars),
                   
                   hr(),
                   helpText('You can find Top 10 Courntires with the numbers of stores Here!'),
                   # Create the table
                   tableOutput('top_country'),
                   hr()
                   
                 ),
                 # Make the sidebar on the right of the webpage
                 position = "right",
                 fluid = TRUE,
                 
                 mainPanel(
                   leafletOutput("map", height=630),
                   hr(),
                   helpText(strong("Not surprisingly, the most number of locations are in U.S and then in Western Europe. Also, there are many location in Eastern Asia. They have one location in Africa, which is their capital. Also, they have some locations in Australia and Russia and couple of ones in Southern America."))
                 )
                 
               )),
      
      # Tab 3 Data Explorer 
      tabPanel("Data Explorer", 
               
               helpText(strong("Before exploring the data, you need to upload the data first! Please Go back to the previous Tab! ")),
               checkboxGroupInput("Ownership.Type",
                                  "Please Select Ownership Type:",
                                  c('Licensed','Joint Venture','Franchise','Company Owned')
               ),
               hr(),
               
               fluidPage(
                 helpText(strong('You Can Select Country and City that you interested in Here!')),
                 
                 fluidRow(column(2,selectInput("country","Country:",
                                               c("All",unique(as.character(df$Country))))),
                          column(2,selectInput("city","City:",
                                               c("All",unique(as.character(df$City)))))
                 ),
                 

                 numericInput(inputId = "obs",
                              label = "Number of observations to view:",
                              value = 10)
               ),
               mainPanel(
                 tabsetPanel(type="tabs",
                             tabPanel("Data",
                                      tableOutput("view"),
                                      helpText(strong('If there is no data shown here, you can change a filter!')
                                               )),
                             #Add a tab for decriptive table
                             tabPanel("Descriptive Analysis",
                                      
                                      # Create a plot output
                                      plotOutput('brand',height = 200),
                                      helpText('You can see that Starbucks is definitely leading and main brand!'),
                                      hr(),
                                      h3("Let's Find the Top 10 !"),
                                      helpText('Here you can find the top 10 countries that have the most Starbucks stores!'),
                                      plotOutput('top_countries',height = 300),
                                      hr(),
                                      helpText(" You can find that USA has got most starbuck stores, followed by China, Cannada, Japan... Let's find out which cities has got the most."),
                                      # plotOutput('top_10_USA'),
                                      hr(),
                                      helpText('Here you can find the top 10 cities that have the most Starbucks stores!'),
                                      plotOutput("top_cities",height = 300)
                                      
                             )
                 )
               )
      )
      
    )),
  
  ################  SERVER PART ######################### 
  server = function(input, output) {

  ###############Interactive Map######################
    # Create the map
    output$map <- renderLeaflet({
      
      # Connect to the sidebar of file input
      inFile <- input$file
      if(is.null(inFile))
        return(NULL)
      
      # Read input file
      mydata <- read.csv(inFile$datapath)
      attach(mydata)
      
      # Filter the data for different Continents
      target <- c(input$Continent)
      mydata <- filter(mydata, Continent %in% target)
      
      # This loop is responsible for maintaining the circles and legend,
      # according to the variables the user has chosen to map to color.
      if(input$color == 'brand'){
        map_type<-Brand
      } else if(input$color =='ownership'){
        map_type<-Ownership.Type
      }
      
      # Create colors with a categorical color function
      color <- colorFactor(rainbow(9), map_type)
      
      # Create the leaflet function for data
      m<-leaflet(mydata) %>%
        
        # Set the default view
        setView(lng = -30.0369, lat = 35.9072, zoom = 1) %>%
        
        # Provide tiles
        addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
        
        # Add circles according to the category that people select to explore
        addCircles(
          radius = 4,
          lat = ~ Latitude, 
          lng = ~ Longitude,
          stroke= FALSE,
          fillOpacity=0.3,
          color = color(map_type))%>%
        
        # Add legends according to the category that people select to explore
        addLegend(
          "bottomleft",
          pal=color,
          values=map_type,
          opacity=0.5,
          title="Color Legend"
        )
      
    })
    
    # Create the Descriptive table
    output$view <- renderTable({
      # Connect to the sidebar of file input
      inFile <- input$file
      if(is.null(inFile))
        return(NULL)
      
      # Read input file
      mydata <- read.csv(inFile$datapath)
      attach(mydata)
      
      # Filter the data for different Country
      if(input$country !="All"){
        target2 <- c(input$country)
        mydata <- filter(mydata,Country %in% target2)
      }
      
      # Filter the data for different City
      
      if(input$city !="All"){
        target3 <- c(input$city)
        mydata <- filter(mydata,City %in% target3)
      }
      
      # Filter the data for different Ownership_Type
      target1 <- c(input$Ownership.Type)
      my_df <- filter(mydata,Ownership.Type %in% target1)
      # Show the table and lines can be customized by input. 
      head(my_df[,1:9], n = input$obs)
    })
    
    # Create the 'top_countries' Table
    output$top_country<- renderTable({
      
      # Connect to the sidebar of file input
      inFile <- input$file
      if(is.null(inFile))
        return(NULL)
      # Read input file
      mydata <- read.csv(inFile$datapath)
      attach(mydata)
      
      top_countires<- mydata %>% 
        filter(Brand=="Starbucks")%>%
        group_by(Country) %>%
        summarise(Count= n()) %>% 
        arrange(desc(Count)) %>% top_n(n=10)
    })
    
    # # Create top 10 cities in USA
    # output$top_10_USA<- renderPlot({
    #   
    #   # Connect to the sidebar of file input
    #   inFile <- input$file
    #   if(is.null(inFile))
    #     return(NULL)
    #   # Read input file
    #   mydata <- read.csv(inFile$datapath)
    #   
    #   cityrank<-mydata %>% 
    #     filter(Brand=="Starbucks" & Country=="US") %>% 
    #     group_by(City)%>%
    #     summarize(Count= n())%>% 
    #     arrange(desc(Count))%>% top_n(n=10)
    #   
    #   ggplot(cityrank, aes(x=factor(City,levels=City), y=Count))+ 
    #     geom_point(size=5, color="green4")+
    #     geom_segment(aes(x=City, 
    #                      xend=City, 
    #                      y=0, 
    #                      yend=Count),size=2,col="darkgoldenrod3")+
    #     labs(title="Top10 Cities in USA - Starbucks",x="City" )+
    #     theme(axis.text.x = element_text(angle=90, vjust=0.6))
    #   options(repr.plot.width=7, repr.plot.height=5)
    # })
    
    # Create the 'Brand Distribution' Plot
    output$brand<-renderPlot({
      
      # Connect to the sidebar of file input
      inFile <- input$file
      if(is.null(inFile))
        return(NULL)
      # Read input file
      mydata <- read.csv(inFile$datapath)
      attach(mydata)
      
      ggplot(mydata,aes(x=Brand,fill=Brand))+
        geom_bar()+
        labs(title='Brand Distribution')+
        theme(plot.title = element_text(hjust = 0.5))
    })
    
    # Create the 'Top 10 Countires' Plot
    output$top_countries<-renderPlot({
      
      # Connect to the sidebar of file input
      inFile <- input$file
      if(is.null(inFile))
        return(NULL)
      
      # Read input file
      mydata <- read.csv(inFile$datapath)
      
      # Filter the Top 10 Countries
      top_countires<- mydata %>% 
        filter(Brand=="Starbucks")%>%
        group_by(Country) %>%
        summarise(Count= n()) %>% 
        arrange(desc(Count)) %>% top_n(n=10)
      # PLot 
      ggplot(top_countires, aes(x=Country, y=Count))+
        geom_bar(stat="identity",fill='green4')+
        ylab("Starbucks Store Number")+
        labs(title='Top 10 Countries')+
        theme(plot.title = element_text(hjust = 0.5))
    })
    
    # Create the 'Top 10 Cities' Plot
    output$top_cities<-renderPlot({
      
      # Connect to the sidebar of file input
      inFile <- input$file
      if(is.null(inFile))
        return(NULL)
      
      # Read input file
      mydata <- read.csv(inFile$datapath)
      
      # Filter the Top 10 Countries
      top_cities<- mydata %>% 
        filter(Brand=="Starbucks")%>%
        group_by(City) %>%
        summarise(Count= n()) %>% 
        arrange(desc(Count)) %>% top_n(n=10)
      # PLot 
      ggplot(top_cities, aes(x=City, y=Count))+
        geom_bar(stat="identity",fill='green4')+
        ylab("Starbucks Store Number")+
        labs(title='Top 10 Cities')+
        theme(plot.title = element_text(hjust = 0.5),text = element_text(family = "STHeiti"))
    })
  }
)


