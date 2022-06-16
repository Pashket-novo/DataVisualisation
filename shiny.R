# loading required libraries
library(shiny)
library(leaflet)
library(plyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# loading dataset
trees <- read.csv("PE2_R_Tree_Data.csv", header = TRUE)
# find top 5 genera by count, sort and slice
top5 <- trees %>% count(Genus, sort = TRUE) %>% slice_max(n,n = 5)
# assigning top5 trees to variable
Trees_top5 <- filter(trees, Genus %in% top5$Genus)

# user interface for shiny application
ui <- fixedPage(
  # creating page layout according to the template
  titlePanel("Common Trees around Fitzroy Gardens"),
  fixedRow(
    column(4,
           h3("The Top 5 Trees"),
           p(strong("Bar chart"),"illustrates the top 5 genera by count around Fitzroy Gardens.", style = "font-family: 'times'; font-si16pt; font-size:15px;"),
           p("Distribution of these genera shown on",strong("map."), "Diameter of the circles represents tree diameter.",style = "font-family: 'times'; font-si16pt, font-size:15px;"),
           fixedRow(
             column(12,
                    plotOutput("plot1"))
           )
    ),
    column(8,
           fixedRow(
             column(1),
             column(11,
                    # ui for genus selection
                    selectInput("genus1", "Genus:",
                                unique(Trees_top5$Genus),
                                multiple = TRUE,
                                selected = NULL
                    ))),
           leafletOutput("mymap", height = "470px")
    )
  ),
  fixedRow(
    column(4,
           h3("Life expectancy"),
           p("The distribution of life expectancy for each of the top 5 genus shown on",
             strong("histogram."),style = "font-family: 'times'; font-si16pt; font-size:15px;"),
           p("Dashed line represent mean life expectancy in years.", style = "font-family: 'times'; font-si16pt; font-size:15px;")
    ),
    column(8,
           plotOutput("plot2")
    )
  )
)

# server implementation of the shiny application
server <- function(input, output, session) {
  
  # calculating mean life expectancy for each genus and assigning to variable
  ml <- ddply(Trees_top5, "Genus", summarise, le.mean=mean(Useful.Life.Expectancy.Value))
  # assigning color brewer palette set1 for Genus Qualitative dataset
  pal2 <- colorFactor(brewer.pal(5, 'Set1'), domain = Trees_top5$Genus)

  # rendering barchart
  output$plot1 <- renderPlot({
    ggplot(Trees_top5, aes(Genus, fill = Genus)) + 
      geom_bar() + scale_fill_brewer(palette = "Set1") +
      geom_text(aes(label=..count..),stat = "count", vjust=1.6, color="black", size=3.5)+
      theme_minimal() +
      theme(legend.position="none")
  })
  # rendering histogram
  output$plot2 <- renderPlot({
    ggplot(Trees_top5, aes(x = Useful.Life.Expectancy.Value, fill = Genus,
                           color = Genus)) +
      geom_histogram(binwidth = 10, alpha=0.8) +
      facet_grid(. ~ Genus) + labs(x="Life expectancy(years)", y = "Count")+
      theme_bw() + theme(legend.position="none") + scale_color_brewer(palette="Set1")+
      scale_fill_brewer(palette="Set1") +
      geom_vline(data=ml, aes(xintercept=le.mean),
                 linetype="dashed")
  })
  
  # rendering leaflet map
  output$mymap <- renderLeaflet({
    
    # checking user selection for genera, returning genera set. If no genus
    # selected, returns all top5 genus
    if(is.null(input$genus1)== TRUE) {
      genInp1 <- unique(Trees_top5$Genus)
    }
    else {
      genInp1 <- input$genus1
    }
    # subseting of original data set to include only selected genera
    genera <- subset(trees, trees$Genus%in%genInp1)
    
    # plotting the leaflet
    leaflet(genera) %>% addTiles() %>%
      addCircles(lng = ~ Longitude, lat = ~Latitude, weight =1,
                 color = ~pal2(Genus),
                 radius = ~sqrt(Diameter.Breast.Height)*1.5, 
                 popup = ~paste("Genus:",Genus,"<br>","Diameter(cm):",Diameter.Breast.Height),
                 stroke = FALSE, fillOpacity = 0.5,
                 group = ~Genus) %>%
      addLegend("bottomright", pal = pal2, values = ~Genus,
                title = "Genus",
                opacity = 1)
  })
  
}
# executing application
shinyApp(ui, server)
