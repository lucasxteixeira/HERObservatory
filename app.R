library(shiny)
library(dplyr)
library(googleCharts)


variables <- c("Country", "Year", "IDPT", "IM", "AFR", "BR", 
    "DR", "GDP", "HEP", "MR", "NoOfPatents", "hindex", "NoOfDocs", 
    "Region", "ID")
data <- read.csv("data.csv")
data <- data[, variables]
data$Region <- as.factor(data$Region)
data$Year <- as.numeric(data$Year)

ylim <- list(
  min = min(data$IDPT,na.rm=TRUE) - 50,
  max = max(data$IDPT,na.rm=TRUE) + 50
)

server = function(input, output, session) {

  # Provide explicit colors for regions, so they don't get recoded when the
  # different series happen to be ordered differently from year to year.
  # http://andrewgelman.com/2014/09/11/mysterious-shiny-things/
  defaultColors <- c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477")
  series <- structure(
      lapply(defaultColors, function(color) { list(color=color) }),
      names = levels(data$Region)
  )

  updateData <- reactive({
    # Filter to the desired year, and put the columns
    # in the order that Google's Bubble Chart expects
    # them (name, x, y, color, size). Also sort by region
    # so that Google Charts orders and colors the regions
    # consistently.
    df <- data %>%
      filter(Year == input$year) %>%
      select(Country, eval(parse(text = input$xvar)), eval(parse(text = input$yvar)),
        Region, hindex) %>%
      arrange(Region)
  })

     output$chart <- reactive({
        # Return the data and options
        list(
            data = googleDataTable(updateData()),
            options = list(
                title = sprintf(
                    "%s vs. %s, %s", input$xvar, input$yvar, input$year),
                series = series,
                hAxis = list(
                    viewWindow = list(
                        min = min(data[[input$xvar]],na.rm=TRUE) - mean(data[[input$xvar]],na.rm=TRUE),
                        max = max(data[[input$xvar]],na.rm=TRUE) + mean(data[[input$xvar]],na.rm=TRUE)
                    )
                ),
                vAxis = list(
                    viewWindow = list(
                        min = min(data[[input$yvar]],na.rm=TRUE) - mean(data[[input$yvar]],na.rm=TRUE),
                        max = max(data[[input$yvar]],na.rm=TRUE) + mean(data[[input$yvar]],na.rm=TRUE)
                    )
                )

            )
        )
    })

}

ui = fluidPage(

  # This line loads the Google Charts JS library
  googleChartsInit(),

  # Use the Google webfont "Source Sans Pro"
  tags$link(
    href=paste0("http://fonts.googleapis.com/css?",
                "family=Source+Sans+Pro:300,600,300italic"),
    rel="stylesheet", type="text/css"),
  tags$style(type="text/css",
    "body {font-family: 'Source Sans Pro'}"
  ),

  title = 'Impact of Academic Production in the Development of Countries',
    titlePanel("Impact of Academic Production in the Development of Countries"),
    sidebarLayout(
        sidebarPanel(
            radioButtons("xvar", label = "X Variable",
                choices = list("NoOfPatents" = "NoOfPatents", "NoOfDocs" = "NoOfDocs"), selected = "NoOfPatents"
            ),
            radioButtons("yvar", label = "Y Variable",
                choices = list("IDPT" = "IDPT", "IM" = "IM",
                    "AFR" = "AFR", "BR" = "BR", "DR" = "DR",
                    "GDP" = "GDP", "HEP" = "HEP"," MR" = "MR"), selected = "IDPT"
            ),
            sliderInput("year", "Year",
                min = min(data$Year), max = max(data$Year),
                value = min(data$Year), animate = TRUE
            ),
            h5("Created by ", a(href="https://github.com/lucasxteixeira", target="_blank", "Lucas de Oliveira Teixeira"), align = "right")
        ),
        mainPanel(
            googleBubbleChart("chart",
                width="100%", height = "475px",
                # Set the default options for this chart; they can be
                # overridden in server.R on a per-update basis. See
                # https://developers.google.com/chart/interactive/docs/gallery/bubblechart
                # for option documentation.
                options = list(
                    fontName = "Source Sans Pro",
                    fontSize = 13,
                    # The default padding is a little too spaced out
                    chartArea = list(
                        top = 50, left = 75,
                        height = "75%", width = "75%"
                    ),
                    # Allow pan/zoom
                    explorer = list(),
                    # Set bubble visual props
                    bubble = list(
                        opacity = 0.4, stroke = "none",
                        # Hide bubble label
                        textStyle = list(
                          color = "none"
                        )
                    ),
                    # Set fonts
                    titleTextStyle = list(
                        fontSize = 16
                    ),
                    tooltip = list(
                        textStyle = list(
                            fontSize = 12
                        )
                    )
                )
            )
        )
    )
)

shinyApp( ui = ui, server = server )