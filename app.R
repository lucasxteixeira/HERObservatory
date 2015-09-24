library(shiny)
library(dplyr)
library(googleCharts)


variables <- c("Country", "Year", "IDPT", "IM", "AFR", "BR", 
    "DR", "GDP", "HEP", "HET", "PSE", "MR", "NoOfPatents", "hindex", "NoOfDocs", 
    "Region")
data <- read.csv("data.csv", stringsAsFactors = TRUE, na.strings=c("NA","NaN", "", " ", NULL))
data <- data[, variables]
data$Year <- as.numeric(data$Year)



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
        Region, NoOfDocs) %>%
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

    tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),

    # This line loads the Google Charts JS library
    googleChartsInit(),

    title = 'HER Observatory',
    fluidRow(
        shiny::column(6, offset = 2,
            a(href="https://herobservatory.wordpress.com/", target="_blank",
                img(src="logo.jpg", width="100%")
            )
        )
    ),

    sidebarLayout(
        sidebarPanel(
            radioButtons("xvar", label = "Latent Variables",
                choices = list("Country's number of articles (h) that have received at least h citations" = "hindex"), selected = "hindex"
            ),
            radioButtons("yvar", label = "Observed Variables",
                choices = list(
                    "Adolescent fertility rate (births per 1,000 women ages 15-19)" = "AFR", 
                    "Birth rate, crude (per 1,000 people)" = "BR", 
                    "Death rate, crude (per 1,000 people)" = "DR",
                    "GDP per capita (current US$)" = "GDP", 
                    "Health expenditure public (% of total health expenditure)" = "HEP",
                    "Public spending on education, total (% of govt expenditure)" = "PSE",
                    "Mortality rate, infant (per 1,000 live births)" = "MR",
                    "Immunization DPT (% of children ages 12 - 23 months)" = "IDPT", 
                    "Immunization measles (% of children ages 12 - 24 months)" = "IM"),
                selected = "AFR"
            ),
            sliderInput("year", "Year",
                min = min(data$Year), max = max(data$Year),
                value = min(data$Year), animate = TRUE
            ),
            h5("Developed by ", a(href="https://github.com/lucasxteixeira", target="_blank", "Lucas de Oliveira Teixeira"), align = "right")
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