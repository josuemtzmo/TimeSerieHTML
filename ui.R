
shinyUI(pageWithSidebar(
  headerPanel("Time Series by Fast Fourier Transform"),
  
  sidebarPanel(
    wellPanel(
    helpText("This app is shows how a user can update a csv file from their own hard drive for instant analysis."),
    tags$hr(),
    fileInput('file1', 'Choose CSV File from local drive, adjusting parameters if necessary',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    
    checkboxInput('header', 'Header', FALSE),
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t',
                   Space=' '),
                 'Tab'),
    tags$head(tags$style(type="text/css",
                         "label.radio { display: inline-block; margin:0 10 0 0;  }",
                         ".radio input[type=\"radio\"] { float: none; }")),
    helpText("The cleaning data factor of standard deviation should be maximum 2"),
    numericInput(
      inputId="Peak",
      "Factor of Standar Desviation for clear Peaks",
      value =0,
      min = NA,
      max = NA),
    numericInput(
      inputId="Noise",
      "Factor of Standar Desviation for clear Noise",
      value = 1,
      min = NA,
      max = NA)
    ),
             
    wellPanel(
      HTML('<p><center><img src= "Ciencias.jpg" width = "45%"/>
             <br/><br/><b><a href="http://www.fciencias.unam.mx/" target = "_blank">Science Faculty </a></b><br/>
             <a href="https://www.facebook.com/josue.martinezmoreno"
             target = "_blank">Facebook</a><b>&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;</b>
             <a href="https://plus.google.com/u/0/114109867789337706145/posts"
             target = "_blank">Google+</a><br/><br/>Josué Martínez Moreno<br/><a 
             href="https://twitter.com/JosueMartinezMo"
             target = "_blank">@JosueMartinezMo</a></center></p>'))
  ),
  mainPanel(p("This application is on ALPHA fase and actually it will give you the period time"),
            tabsetPanel(
              id="dataset",
              
              tabPanel("Table",
                       align = "center",
                       tableOutput('contents'),
                       value = 1),
              
              tabPanel(
                title="Data Plot",
                align = "center",
                plotOutput('plot'),
                value = 2),
              
              tabPanel(
                title="Peridiogram",
                plotOutput('plot1'),
                plotOutput('plot2'),
                align = "center",
                div(
                  class = "fluid-row",
                  div(
                    class ="span5",
                    numericInput(
                      inputId="inxi",
                      "Initial number for plot(x)",
                      value = 0,
                      min = NA,
                      max = NA)),
                  div(
                    class ="span5",
                    numericInput(
                      inputId="inxf",
                      "Final number for plot (x)",
                      value = 0.03,
                      min = NA,
                      max = NA)),
                  class = "fluid-row",
                  tags$hr(),
                  div(
                    class ="span5",
                    numericInput(
                      inputId="inyi",
                      "Initial number for plot(y)",
                      value = 0,
                      min = NA,
                      max = NA)),
                  div(
                    class ="span5",
                    numericInput(
                      inputId="inyf",
                      "Final number for plot (y)",
                      value = 110,
                      min = NA,
                      max = NA)),
                  align = "center",
                  tableOutput('tableperiod')
                ),
                value=3),
              
              tabPanel(
                title="Noise",
                align = "center",
                plotOutput('noise'),
                textOutput('help1'),   
                textOutput('noisedata'),
                value=4),
              
              tabPanel(
                title="Data Plot Noise Remove",
                align = "center",
                plotOutput('plotnoise'),
                value = 5),
              tabPanel(
                title="Peridiogram Noise Remove",
                plotOutput('plotnoise1'),
                plotOutput('plotnoise2'),
                align = "center",
                div(
                  class = "fluid-row",
                  div(
                    class ="span5",
                    numericInput(
                      inputId="inxin",
                      "Initial number for plot(x)",
                      value = 0,
                      min = NA,
                      max = NA)),
                  div(
                    class ="span5",
                    numericInput(
                      inputId="inxfn",
                      "Final number for plot (x)",
                      value = 0.03,
                      min = NA,
                      max = NA)),
                  class = "fluid-row",
                  tags$hr(),
                  div(
                    class ="span5",
                    numericInput(
                      inputId="inyin",
                      "Initial number for plot(y)",
                      value = 0,
                      min = NA,
                      max = NA)),
                  div(
                    class ="span5",
                    numericInput(
                      inputId="inyfn",
                      "Final number for plot (y)",
                      value = 50,
                      min = NA,
                      max = NA)),
                  align = "center",
                  tableOutput('tableperiodnoise')
                  ),
                value=6),
              tabPanel(
                title="Noise of Noise",
                align = "center",
                plotOutput('noisenoise'),
                textOutput('help1noise'),   
                textOutput('noisenoisedata'),
                value=7)
            )
  )
)
)
