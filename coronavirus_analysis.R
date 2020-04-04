library('shiny')
library('shinydashboard')
library('data.table')
library('ggplot2')
library('DT')
library('plotly')
library('leaflet')

###### Tratamento dos dados
fonteCasos <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
fonteObitos <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'
fonteRecuperados <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv'

dadosCasos       <- fread(fonteCasos)
dadosObitos      <- fread(fonteObitos)
dadosRecuperados <- fread(fonteRecuperados)

paleta <- colorRampPalette(c('#5685ff', '#ec7cda', '#fcd67a', '#76e5ff', '#526ae4', '#67deb4', '#fc9b59', '#c173f9'))

TratarDados <- function(dados) {
  setnames(dados, c('Province/State', 'Country/Region', 'Lat', 'Long'), c('provinceOrState', 'countryOrRegion', 'lat', 'long'))
  dados <- melt(dados, id.vars = c('provinceOrState', 'countryOrRegion', 'lat', 'long'))
  setnames(dados, c('variable', 'value'), c('date', 'confirmedCases'))
  dados[, date := as.POSIXct(date, format = '%m/%d/%y')]
  dados[, totalConfirmedCases := sum(confirmedCases), by = .(countryOrRegion, date)]
  dados[totalConfirmedCases > 0, daysSinceDayOne := 1:.N, by = .(countryOrRegion, provinceOrState)]
  dados
}

dadosCasos       <- TratarDados(dadosCasos)
dadosRecuperados <- TratarDados(dadosRecuperados)
dadosObitos      <- TratarDados(dadosObitos)

dadosCasos <- merge(dadosCasos,
                    dadosRecuperados[, .(countryOrRegion, provinceOrState, date, confirmedRecovered = confirmedCases, totalConfirmedRecovered = totalConfirmedCases)],
                    by = c('countryOrRegion', 'provinceOrState', 'date'),
                    all.x = TRUE)

dadosCasos <- merge(dadosCasos,
                    dadosObitos[, .(countryOrRegion, provinceOrState, date, confirmedDeaths = confirmedCases, totalConfirmedDeaths = totalConfirmedCases)],
                    by = c('countryOrRegion', 'provinceOrState', 'date'),
                    all.x = TRUE)

# params
startDate <- dadosCasos[, min(as.Date(date, format = '%Y-%m-%d'))]
endDate   <- dadosCasos[, max(as.Date(date, format = '%Y-%m-%d'))]
maxDays   <- dadosCasos[, max(daysSinceDayOne, na.rm = TRUE)]

###### UI
ui <- dashboardPage(
  dashboardHeader(title = 'COVID-19'),
  dashboardSidebar(sidebarMenu(
    menuItem('Histórico', tabName = 'historico', icon = icon('signal')),
    menuItem('Mapa',     tabName = 'mapa',      icon = icon('map'))
  ),
  checkboxInput('discount', 'Casos ativos', value = FALSE)),
  dashboardBody(
  tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'custom.css'),
            tags$link(rel = 'stylesheet', type = 'text/css', href = 'https://fonts.googleapis.com/css2?family=Sen:wght@400;700;800&display=swap')),
  tags$style(type = 'text/css', '#map {height: calc(100vh - 80px) !important;}'),
    tabItems(
	  tabItem(tabName = 'historico',
	    fluidRow(
	      valueBoxOutput('totalCases', width = 3),
	      valueBoxOutput('deaths',     width = 3),
	      valueBoxOutput('recovered',  width = 3),
	      valueBoxOutput('mortality',  width = 3)
	    ),
	    fluidRow(
	      selectizeInput('paises', 'Selecione os países que deseja consultar:',
	      dadosCasos[, sort(unique(countryOrRegion))],
	      selected = 'Brazil', multiple = TRUE,
	      width = '100%'),style = "margin-left:0px;margin-right:0px"
	    ),
	    fluidRow(
	      tabBox(
  	      tabPanel('Com base no primeiro caso', '',
  	        fluidRow(plotlyOutput('graficoDias', width = '100%')),
  	        fluidRow(sliderInput('daySlider', '', 
  	                 min = 1, max = maxDays, value = maxDays, step = 1,
  	                 animate = animationOptions(interval = 300, loop = FALSE), width = '100%')), 
  	        width = 12, fluidRow('Dados públicos disponibilizados pelo Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE). Saiba mais em: https://github.com/CSSEGISandData/COVID-19')
  	      ),
  	      tabPanel('Com base no tempo', '',
  	        plotlyOutput('graficoDatas', width = '100%'),
  	        sliderInput('dateChoose', '',
  	                    min = startDate, max = endDate, value = endDate,
  	                    timeFormat = '%d/%m/%Y',
  	                    animate = animationOptions(interval = 300, loop = FALSE), width = '100%'),
  	        width = 12, fluidRow('Dados públicos disponibilizados pelo Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE). Saiba mais em: https://github.com/CSSEGISandData/COVID-19')
  	      ),
	    width = 12)
	    )
		
		),
	    # fluidRow(
	    #   box(tableOutput('tabela'), width = 12, height = 400)
	    # )),
	  tabItem(tabName = 'mapa',
	    fluidRow(
	      box(
	        leafletOutput('worldmap', height = 500),
	        sliderInput('mapDateReference', '',
	                    min = startDate, max = endDate, value = Sys.Date(),
	                    timeFormat = '%d/%m/%Y',
	                    animate = animationOptions(interval = 150, loop = TRUE)),
	        fluidRow('Dados públicos disponibilizados pelo Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE). Saiba mais em: https://github.com/CSSEGISandData/COVID-19'),
	        width = 12),
			
        ),
	  )
	)
  )
)

####### SERVER
server <- function(input, output) {
  output$graficoDias <- renderPlotly({
    plotar <- dadosCasos[countryOrRegion %chin% input$paises, .SD[.N], by = .(countryOrRegion, date)]
    maxX <- plotar[, max(daysSinceDayOne, na.rm = TRUE)]
    maxY <- plotar[, max(totalConfirmedCases, na.rm = TRUE)]
	  plotar <- plotar[daysSinceDayOne > 0 & daysSinceDayOne <= input$daySlider]
	  if (input$discount) {
	    plotar[, totalConfirmedCases := totalConfirmedCases - totalConfirmedRecovered - totalConfirmedDeaths]
	  }
	plt <- ggplot(plotar, aes(x = daysSinceDayOne, y = totalConfirmedCases, fill = countryOrRegion, color = countryOrRegion)) +
      geom_area(position='identity') +
	    geom_path() +
	  xlab("Dias desde o primeiro caso confirmado") + ylab("Casos confirmados") +
	  theme(
	    panel.grid.major = element_blank(),
	    panel.background = element_rect(fill = "transparent", colour = NA),
	    plot.background = element_rect(fill = "transparent", colour = NA)
	  ) + 
	  scale_color_manual(values = paleta(plotar[, uniqueN(countryOrRegion)])) +
	  scale_fill_manual(values = alpha(paleta(plotar[, uniqueN(countryOrRegion)]), 0.5)) +
	  scale_x_continuous(breaks = seq(0, maxX, 10),
	                     limits = c(0, maxX)) +
	  scale_y_continuous(limits = c(0, maxY)) +
	  theme(legend.position = 'none')
	ggplotly(plt) %>% layout(font = list(family = 'Sen', color = '#5a6a9b')) %>% config(displayModeBar = F)
  })
  
  output$graficoDatas <- renderPlotly({
    plotar <- dadosCasos[countryOrRegion %chin% input$paises, .SD[.N], by = .(countryOrRegion, date)]
    maxY   <- plotar[, max(totalConfirmedCases, na.rm = TRUE)]
    plotar <- plotar[format(date, '%Y-%m-%d') <= input$dateChoose]
    if (input$discount) {
      plotar[, totalConfirmedCases := totalConfirmedCases - totalConfirmedRecovered - totalConfirmedDeaths]
    }
    plt <- ggplot(plotar, aes(x = as.Date(date), y = totalConfirmedCases, fill = countryOrRegion, color = countryOrRegion)) +
      geom_path() +
      geom_area(position = 'identity') +
      xlab("Data") + ylab("Casos confirmados") +
      theme(
        panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA)
      ) + scale_x_date(date_breaks = '1 week', limits = c(startDate, endDate)) +
      scale_color_manual(values = paleta(plotar[, uniqueN(countryOrRegion)])) +
      scale_fill_manual(values = alpha(paleta(plotar[, uniqueN(countryOrRegion)]), 0.5)) +
      scale_y_continuous(limits = c(0, maxY)) +
      theme(legend.position = 'none', axis.text.x = element_text(angle = 45, hjust = 1)) 
    ggplotly(plt) %>% layout(font = list(family = 'Sen', color = '#5a6a9b')) %>% config(displayModeBar = F)
  })
	
#   output$tabela <- renderTable({
#     plotar <- dadosCasos[countryOrRegion %chin% input$paises, .SD[.N], by = .(countryOrRegion, date)]
# 	  plotar <- plotar[, .(countryOrRegion, date, totalConfirmedCases, totalConfirmedRecovered, totalConfirmedDeaths)]
# 	  if (input$discount) {
# 	    plotar[, totalConfirmedCases := totalConfirmedCases - totalConfirmedRecovered - totalConfirmedDeaths]
# 	  }
# 	  plotar <- dcast.data.table(plotar[1:5], countryOrRegion~date, value.var = 'totalConfirmedCases')
# 	  # setorder(plotar, -date)
# 	  # plotar[, date := format(date, '%d/%m/%Y')]
# 	  # setnames(plotar, 'date', 'Data')
#   },options = list(scrollX = TRUE))
#   
  output$worldmap <- renderLeaflet({
    plotar <- dadosCasos[as.Date(date) == endDate & confirmedCases > 0]
    if (input$discount) {
      plotar[, confirmedCases := confirmedCases - confirmedRecovered - confirmedDeaths]
    }
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
      options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(lat = plotar[, lat],
                       lng = plotar[, long],
                       radius = plotar[, log(confirmedCases)],
                       stroke = FALSE,
                       fillOpacity = 0.6,
                       color = 'red'
      )
  })
  
  observe({
    dateRef <- as.character(input$mapDateReference)
    plotar <- dadosCasos[date == dateRef & confirmedCases > 0]
    if (input$discount) {
      plotar[, confirmedCases := confirmedCases - confirmedRecovered - confirmedDeaths]
    }
    isolate({
      leafletProxy('worldmap') %>%
        clearMarkers() %>%
        addCircleMarkers(lat = plotar[, lat],
                         lng = plotar[, long],
                         radius = plotar[, log(confirmedCases)],
                         stroke = FALSE,
                         fillOpacity = 0.6,
                         color = 'red'
        )
    })
  })
  
  output$totalCases <- renderValueBox({
    dadosCasos <- dadosCasos[countryOrRegion %chin% input$paises, .SD[.N], by = countryOrRegion]
    valor <- dadosCasos[, sum(totalConfirmedCases)]
    valueBox(format(valor, big.mark = '.', decimal.mark = ','), 'Casos confirmados', icon = icon("stats", lib = "glyphicon"))
  })
  output$deaths <- renderValueBox({
    dadosCasos <- dadosCasos[countryOrRegion %chin% input$paises, .SD[.N], by = countryOrRegion]
    valor <- dadosCasos[, sum(totalConfirmedDeaths)]
    valueBox(format(valor, big.mark = '.', decimal.mark = ','), 'Óbitos', icon = icon("exclamation-sign", lib = "glyphicon"))
  })
  output$recovered <- renderValueBox({
    dadosCasos <- dadosCasos[countryOrRegion %chin% input$paises, .SD[.N], by = countryOrRegion]
    valor <- dadosCasos[, sum(totalConfirmedRecovered)]
    valueBox(format(valor, big.mark = '.', decimal.mark = ','), 'Recuperados', icon = icon("heart"))
  })
  output$mortality <- renderValueBox({
    dadosCasos <- dadosCasos[countryOrRegion %chin% input$paises, .SD[.N], by = countryOrRegion]
    valor <- dadosCasos[, sum(totalConfirmedDeaths) / sum(totalConfirmedCases)]
    valueBox(paste0(format(round(valor * 100, 2), big.mark = '.', decimal.mark = ','), '%'), 'Fatalidade', icon = icon("plus-sign", lib = "glyphicon"))
  })
  
}

shinyApp(ui, server)
