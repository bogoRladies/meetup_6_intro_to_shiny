## ===========  block 1: simple hello world   ===================

library(shiny)

ui <- fluidPage(
  'Hello, World'
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)


## ===========  block 2: simple hello world + name   ===================

library(shiny)

ui <- fluidPage(
  textInput('name', 'type your name'), 
  textOutput('greeting')
)

server <- function(input, output, session) {
  output$greeting <- renderText({paste('Hello ', input$name)})
}

shinyApp(ui, server)

## ===========  block 3: text + plot   ===================

library(shiny)
library(ggplot2)

ui <- fluidPage(
  textInput('name', 'type your name'), 
  textOutput('greeting'), 
  plotOutput('trend')
)

server <- function(input, output, session) {
  output$greeting <- renderText({paste('Hello ', input$name)})
  output$trend <- renderPlot({ggplot()})
}

shinyApp(ui, server)


## ===========  block 4: Layouting  ===================

library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("My First Shiny App"),
  sidebarLayout(
    sidebarPanel(textInput('name', 'type your name')), 
    mainPanel(
      textOutput('greeting'), 
      plotOutput('trend')
    )
  ))

server <- function(input, output, session) {
  output$greeting <- renderText({paste('Hello ', input$name)})
  output$trend <- renderPlot({ggplot()})
}

shinyApp(ui, server)


## ===========  block 5: Babynames Plot  ===================

library(shiny)
library(ggplot2)
library(babynames)

ui <- fluidPage(
  titlePanel("My First Shiny App"), 
  sidebarLayout(
    sidebarPanel(textInput('name', 'type your name')), 
    mainPanel(textOutput('greeting'), 
              plotOutput('trend'))
  )
)

server <- function(input, output, session) {
  output$greeting <- renderText({paste('Hello ', input$name)})
  output$trend <- renderPlot({
    ggplot(subset(babynames, name == input$name)) +
      geom_line(aes(x = year, y = prop, color = sex))
  })
}

shinyApp(ui, server)

## ===========  block 6: SelectInput  ===================

library(shiny)
library(ggplot2)
library(babynames)
library(dplyr)

ui <- fluidPage(
  titlePanel("My First Shiny App"), 
  sidebarLayout(
    sidebarPanel(
      # input 1: text
      textInput('name', 'type your name'), 
      
      # input 2: selection (dropdown)
      selectInput('sex', 'select sex', selected = 'F', choices = c('M', 'F'))
    ), 
    mainPanel(
      textOutput('greeting'), 
      plotOutput('trend'), 
      plotOutput('plot_top_10_names')
    )))

server <- function(input, output, session) {
  output$greeting <- renderText({paste('Hello ', input$name)})
  
  output$trend <- renderPlot({
    ggplot(subset(babynames, name == input$name)) +
      geom_line(aes(x = year, y = prop, color = sex))
  })
  
  output$plot_top_10_names <- renderPlot({
    # Get top 10 names by sex and year
    top_10_names <- babynames %>% 
      # MODIFY CODE BELOW: Filter for the selected sex
      filter(sex == input$sex) %>% 
      filter(year == 1900) %>% 
      top_n(10, prop)
    # Plot top 10 names by sex and year
    ggplot(top_10_names, aes(x = name, y = prop)) +
      geom_col(fill = "#263e63")
  })
}

shinyApp(ui, server)


## ===========  block 7: Slider  ===================

library(shiny)
library(ggplot2)
library(babynames)
library(dplyr)

ui <- fluidPage(
  titlePanel("My First Shiny App"), 
  sidebarLayout(
    sidebarPanel(
      # input 1: text
      textInput('name', 'type your name'), 
      
      # input 2: selection (dropdown)
      selectInput('sex', 'select sex', selected = 'F', choices = c('M', 'F')),
      
      # input 3: slider
      sliderInput('year', 'Years', min = 1900, max = 2010, value = 1900)
    ), 
    mainPanel(
      textOutput('greeting'), 
      plotOutput('trend'), 
      plotOutput('plot_top_10_names')
    )
  )
)

server <- function(input, output, session) {
  output$greeting <- renderText({paste('Hello ', input$name)})
  
  output$trend <- renderPlot({
    ggplot(subset(babynames, name == input$name)) +
      geom_line(aes(x = year, y = prop, color = sex))
  })
  
  output$plot_top_10_names <- renderPlot({
    # Get top 10 names by sex and year
    top_10_names <- babynames %>% 
      # MODIFY CODE BELOW: Filter for the selected sex
      filter(sex == input$sex) %>% 
      filter(year == input$year) %>% 
      top_n(10, prop)
    # Plot top 10 names by sex and year
    ggplot(top_10_names, aes(x = name, y = prop)) +
      geom_col(fill = "#263e63")
  })
}

shinyApp(ui, server)
## ===========  block 8: tableOutput + reactive variables  ===================

library(shiny)
library(ggplot2)
library(babynames)
library(dplyr)

ui <- fluidPage(
  titlePanel("My First Shiny App"), 
  sidebarLayout(
    sidebarPanel(
      # input 1: text
      textInput('name', 'type your name'), 
      
      # input 2: selection (dropdown)
      selectInput('sex', 'select sex', selected = 'F', choices = c('M', 'F')),
      
      # input 3: slider
      sliderInput('year', 'Years', min = 1900, max = 2010, value = 1900)
    ), 
    mainPanel(
      # output 3: plot
      plotOutput('plot_top_10_names'), 
      # output 4: table
      tableOutput('table_top_10_names'),
      # output 1: text
      textOutput('greeting'), 
      # output 2: plot
      plotOutput('trend')
    )
  )
)

server <- function(input, output, session) {
  # Function to create a data frame of top 10 names by sex and year 
  top_10_names <- function(){
    top_10_names <- babynames %>% 
      filter(sex == input$sex) %>% 
      filter(year == input$year) %>% 
      top_n(10, prop)
  }
  
  output$greeting <- renderText({paste('Hello ', input$name)})
  
  output$trend <- renderPlot({
    ggplot(subset(babynames, name == input$name)) +
      geom_line(aes(x = year, y = prop, color = sex))
  })
  
  output$plot_top_10_names <- renderPlot({
    # Plot top 10 names by sex and year
    ggplot(top_10_names(), aes(x = name, y = prop)) +
      geom_col(fill = "#263e63")
  })
  
  output$table_top_10_names <- renderTable({
    top_10_names()
  })
}

shinyApp(ui, server)

## ===========  block 9: Interactive Table with DT Table ===================

library(shiny)
library(ggplot2)
library(babynames)
library(dplyr)
library(DT)

ui <- fluidPage(
  titlePanel("My First Shiny App"), 
  sidebarLayout(
    sidebarPanel(
      # input 1: text
      textInput('name', 'type your name'), 
      
      # input 2: selection (dropdown)
      selectInput('sex', 'select sex', selected = 'F', choices = c('M', 'F')),
      
      # input 3: slider
      sliderInput('year', 'Years', min = 1900, max = 2010, value = 1900)
    ), 
    mainPanel(
      # output 3: plot
      plotOutput('plot_top_10_names'), 
      # output 4: table
      # tableOutput('table_top_10_names'),
      # output 5: DT table
      DT::DTOutput('table_top_10_names'), 
      # output 1: text
      textOutput('greeting'), 
      # output 2: plot
      plotOutput('trend')
    )
  )
)

server <- function(input, output, session) {
  # Function to create a data frame of top 10 names by sex and year 
  top_10_names <- function(){
    top_10_names <- babynames %>% 
      filter(sex == input$sex) %>% 
      filter(year == input$year) %>% 
      top_n(10, prop)
  }
  
  output$greeting <- renderText({paste('Hello ', input$name)})
  
  output$trend <- renderPlot({
    ggplot(subset(babynames, name == input$name)) +
      geom_line(aes(x = year, y = prop, color = sex))
  })
  
  output$plot_top_10_names <- renderPlot({
    # Plot top 10 names by sex and year
    ggplot(top_10_names(), aes(x = name, y = prop)) +
      geom_col(fill = "#263e63")
  })
  
  output$table_top_10_names <- DT::renderDT({
    top_10_names()
  })
}

shinyApp(ui, server)
## ===========  block 10: Interactive Plot with Plotly ===================

library(shiny)
library(ggplot2)
library(babynames)
library(dplyr)
library(DT)
library(plotly)

ui <- fluidPage(
  titlePanel("My First Shiny App"), 
  sidebarLayout(
    sidebarPanel(
      # input 1: text
      textInput('name', 'type your name'), 
      
      # input 2: selection (dropdown)
      selectInput('sex', 'select sex', selected = 'F', choices = c('M', 'F')),
      
      # input 3: slider
      sliderInput('year', 'Years', min = 1900, max = 2010, value = 1900)
    ), 
    mainPanel(
      # output 6: plotly
      plotly::plotlyOutput('plotly_top_10_names'),
      # output 3: plot
      # plotOutput('plot_top_10_names'), 
      # output 4: table
      # tableOutput('table_top_10_names'),
      # output 5: DT table
      DT::DTOutput('table_top_10_names'), 
      # output 1: text
      textOutput('greeting'), 
      # output 2: plot
      plotOutput('trend')
    )
  )
)

server <- function(input, output, session) {
  # Function to create a data frame of top 10 names by sex and year 
  top_10_names <- function(){
    top_10_names <- babynames %>% 
      filter(sex == input$sex) %>% 
      filter(year == input$year) %>% 
      top_n(10, prop)
  }
  
  output$greeting <- renderText({paste('Hello ', input$name)})
  
  output$trend <- renderPlot({
    ggplot(subset(babynames, name == input$name)) +
      geom_line(aes(x = year, y = prop, color = sex))
  })
  
  # output$plot_top_10_names <- renderPlot({
  #   # Plot top 10 names by sex and year
  #   ggplot(top_10_names(), aes(x = name, y = prop)) +
  #     geom_col(fill = "#263e63")
  # })
  
  output$plotly_top_10_names <- plotly::renderPlotly({
    ggplot(top_10_names(), aes(x = name, y = prop)) +
      geom_col(fill = "#263e63")
  })
  
  output$table_top_10_names <- DT::renderDT({
    top_10_names()
  })
}

shinyApp(ui, server)
## ===========  block 11: Working with Tabs ===================

library(shiny)
library(ggplot2)
library(babynames)
library(dplyr)
library(DT)
library(plotly)

ui <- fluidPage(
  titlePanel("My First Shiny App"), 
  sidebarLayout(
    sidebarPanel(
      # input 1: text
      textInput('name', 'type your name'), 
      
      # input 2: selection (dropdown)
      selectInput('sex', 'select sex', selected = 'F', choices = c('M', 'F')),
      
      # input 3: slider
      sliderInput('year', 'Years', min = 1900, max = 2010, value = 1900)
    ), 
    mainPanel(
      tabsetPanel(
        tabPanel('plotly', plotly::plotlyOutput('plotly_top_10_names')), 
        tabPanel('plot', plotOutput('plot_top_10_names')), 
        tabPanel('dt', DT::DTOutput('table_top_10_names')), 
        tabPanel('text & output', textOutput('greeting'), plotOutput('trend'))
      )
    )
  )
)

server <- function(input, output, session) {
  # Function to create a data frame of top 10 names by sex and year 
  top_10_names <- function(){
    top_10_names <- babynames %>% 
      filter(sex == input$sex) %>% 
      filter(year == input$year) %>% 
      top_n(10, prop)
  }
  
  output$greeting <- renderText({paste('Hello ', input$name)})
  
  output$trend <- renderPlot({
    ggplot(subset(babynames, name == input$name)) +
      geom_line(aes(x = year, y = prop, color = sex))
  })
  
  output$plot_top_10_names <- renderPlot({
    # Plot top 10 names by sex and year
    ggplot(top_10_names(), aes(x = name, y = prop)) +
      geom_col(fill = "#263e63")
  })
  
  output$plotly_top_10_names <- plotly::renderPlotly({
    ggplot(top_10_names(), aes(x = name, y = prop)) +
      geom_col(fill = "#263e63")
  })
  
  output$table_top_10_names <- DT::renderDT({
    top_10_names()
  })
}

shinyApp(ui, server)
## ===========  block 12: Themes ===================

library(shiny)
library(ggplot2)
library(babynames)
library(dplyr)
library(DT)
library(plotly)
library(shinythemes)

ui <- fluidPage(
  titlePanel("My First Shiny App"), 
  # shinythemes::themeSelector(), 
  theme = shinythemes::shinytheme('superhero'), 
  sidebarLayout(
    sidebarPanel(
      # input 1: text
      textInput('name', 'type your name'), 
      
      # input 2: selection (dropdown)
      selectInput('sex', 'select sex', selected = 'F', choices = c('M', 'F')),
      
      # input 3: slider
      sliderInput('year', 'Years', min = 1900, max = 2010, value = 1900)
    ), 
    mainPanel(
      tabsetPanel(
        # output 6: plotly
        tabPanel('plotly', plotly::plotlyOutput('plotly_top_10_names')), 
        # output 3: plot
        tabPanel("plot", plotOutput('plot_top_10_names')), 
        # output 5: DT table
        tabPanel("dt", DT::DTOutput('table_top_10_names')), 
        # output 1 & 2: text and plot
        tabPanel("text and outplut", textOutput('greeting'), plotOutput('trend'))
      )
    )
  )
)

server <- function(input, output, session) {
  # Function to create a data frame of top 10 names by sex and year 
  top_10_names <- function(){
    top_10_names <- babynames %>% 
      filter(sex == input$sex) %>% 
      filter(year == input$year) %>% 
      top_n(10, prop)
  }
  
  output$greeting <- renderText({paste('Hello ', input$name)})
  
  output$trend <- renderPlot({
    ggplot(subset(babynames, name == input$name)) +
      geom_line(aes(x = year, y = prop, color = sex))
  })
  
  output$plot_top_10_names <- renderPlot({
    # Plot top 10 names by sex and year
    ggplot(top_10_names(), aes(x = name, y = prop)) +
      geom_col(fill = "#263e63")
  })
  
  output$plotly_top_10_names <- plotly::renderPlotly({
    ggplot(top_10_names(), aes(x = name, y = prop)) +
      geom_col(fill = "#263e63")
  })
  
  output$table_top_10_names <- DT::renderDT({
    top_10_names()
  })
}

shinyApp(ui, server)


## ===========  block 13: bonus - greeting card ====================

library(shiny)

ui <- fluidPage(
  selectInput('greeting', 'Select greeting', choices = c('Hello', 'Bonjour', 'Hola', 'Konnichiwa', 'Assalamualaykum')), 
  textInput('name', 'Enter your name'), 
  textOutput('name')
)

server <- function(input, output, session) {
  output$name <- renderText({
    paste(input$greeting, ', ', input$name)
  })
}

shinyApp(ui = ui, server = server)