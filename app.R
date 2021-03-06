require(shiny)
require(tidyverse)
require(shinyjs)
require(Stat2Data)
require(bslib)
require(ggplot2)
require(plotly)

data(CO2SouthPole)
data = CO2SouthPole[1:3]
data
months=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
data$Month = factor(data$Month, levels = 1:12, labels = months)

plotdata = data.frame('Month'= months)
labelling = c('Month')
for (yr in unique(data$Year)){
    plotdata = cbind(plotdata, data[data$Year==yr,'CO2'])
    labelling = c(labelling, as.character(yr))
}
colnames(plotdata) = labelling
plotdata

YearSelector = div(
    selectInput(
        inputId = "yearSel",
        label = "Select the Year",
        multiple = TRUE,
        choices = Years,
        selected = Years[1]
    )#selectInput
)#div

MultPlot = tabPanel(
    "Barplot of Concentration by Month and Year",
    plotOutput("Barplot")
)

# Define UI
ui = navbarPage(
    theme = bs_theme(
        bg = "#005ce6", fg = "white", primary = "#ffcc00",
        base_font = font_google("Space Mono"),
        code_font = font_google("Space Mono")
    ),#theme
    
    # Application title
    titlePanel("Assignment 7 - Shiny App"
    ),
    
    # Sidebar with a slider input for number of bins 
    tabPanel('Introduction',
             fluidPage(
                 h1('The Stat2Data Package'),
                 br(),
                 p('Stat2Data is an R package containing a huge amount of datasets extracted from the textbook Stat2: Modeling with Regression and ANOVA.'),
                 br(),
                 p('More information about this package can be found in:'),
                 a(href="https://cran.r-project.org/web/packages/Stat2Data/Stat2Data.pdf", "Official Stat2Data Documentation"),
                 br(),
                 br(),
                 h2(HTML(paste0("CO",tags$sub("2"),"SouthPole Dataset"))),
                 br(),
                 p('This object contains monthly average measurements of carbon dioxide concentrations in a US scientific settlement. Data is given by ERSL (Earth System Research Laboratory) of the U.S. NOAA (National Oceanic and Atmospheric Administration).'),
                 br(),
                 br(),
                 img(src="uc3mLogo.jpg")
             )#fluidPage
    
       
    ),#tabPanel
    
    tabPanel("Comparison of Concentrations",
             fluidPage(
                 sidebarLayout(
                     sidebarPanel(
                         p(HTML(paste0('Check yourself the evolution of CO',tags$sub("2")," concentration throughout the registered years. See how the values are alarmingly increasing!")))
                     ),#sidebarPanel
                     mainPanel(
                         YearSelector,
                         MultPlot
                     )#mainPanel
                 )#sidebarLayout
            )#fluidPage    
    )#tabPanel
)#navbarPage

# Define server logic required to draw a histogram
server = function(input, output) {
    sel_year = reactive({plotdata %>%
            select(c(Month,input$yearSel))})
    output$Barplot = renderPlot(
        sel_year() %>% gather(key, value, -c(Month)) %>% 
            ggplot(aes(x=Month, y=value, fill = key)) +
            geom_col(position = "dodge")+
            coord_cartesian(ylim = c(340, 405))+
            scale_fill_discrete(name = "Year")+
            theme_minimal()+
            labs(title = "CO2 in The South Pole by Month - 1988, 1989", x = 'Month', y = 'Concentration (ppm)')
    )

}

# Run the application 
shinyApp(ui = ui, server = server)
