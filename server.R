#Functions

shinyServer(function(input, output, session) {
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects and uploads a 
    # file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
    # columns. The 'datapath' column will contain the local filenames where the 
    # data can be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  })

  output$plot <- renderPlot({
    inFile <- input $file1
    if (is.null(inFile))
      return(NULL)
    
    data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    colnames(data) <-c("V1", "V2")
    max <- max(data$V2)
    min <- min(data$V2)
    desv <-sd(data$V2, na.rm = FALSE)
    data <- data[(data$V2 >= min+input$Peak*desv) & (data$V2<= max-input$Peak*desv), ]
    qplot(x=data$V1, y=data$V2, geom=c("point", "line")) + xlab("JD(day)") + ylab("V(mag)")
})

  output$plotnoise <- renderPlot({
    inFile <- input $file1
    if (is.null(inFile))
      return(NULL)
  
    data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    colnames(data) <-c("V1", "V2")
    mean <- mean(data$V2)
    desv <-sd(data$V2, na.rm = FALSE)
    data <- data[(data$V2 <= mean+input$Noise*desv) & (data$V2>= mean-input$Noise*desv), ]
    qplot(x=data$V1, y=data$V2, geom=c("point", "line")) + xlab("JD(day)") + ylab("V(mag)")
})
  
  output$plot1 <- renderPlot({
    inFile <- input $file1
    if (is.null(inFile))
      return(NULL)
    
    data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    colnames(data) <-c("V1", "V2")
    max <- max(data$V2)
    min <- min(data$V2)
    desv <-sd(data$V2, na.rm = FALSE)
    data <- data[(data$V2 >= min+input$Peak*desv) & (data$V2<= max-input$Peak*desv), ]
    ### calculando la transformada rapida de Fourier
    fourier2 <- fft(data$V2)
    ### calculando su espectro de potencia ##
    magnitude2 <- Mod(fourier2)
    phase2 <- Arg(fourier2)
    magnitude_firsthalf2 <- magnitude2[1:length((magnitude2)/2)]
    phase_firsthalf2 <- phase2[1:length(magnitude2/2)]
    ### generando ejes para graficar ###
    x.axis2 <- 1:length(magnitude_firsthalf2)/length(magnitude2)
    qplot(x=x.axis2,y=magnitude_firsthalf2,geom="line") + xlim(0,0.5) + ylim(0,100 ) + xlab("Frecuencia (s)") + ylab("Magnitud")
    })

  output$plotnoise1 <- renderPlot({
    inFile <- input $file1
    if (is.null(inFile))
      return(NULL)
    
    data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    colnames(data) <-c("V1", "V2")
    mean <- mean(data$V2)
    desv <-sd(data$V2, na.rm = FALSE)
    data <- data[(data$V2 <= mean+input$Noise*desv) & (data$V2>= mean-input$Noise*desv), ]
    ### calculando la transformada rapida de Fourier
    fourier2 <- fft(data$V2)
    ### calculando su espectro de potencia ##
    magnitude2 <- Mod(fourier2)
    phase2 <- Arg(fourier2)
    magnitude_firsthalf2 <- magnitude2[1:length((magnitude2)/2)]
    phase_firsthalf2 <- phase2[1:length(magnitude2/2)]
    ### generando ejes para graficar ###
    x.axis2 <- 1:length(magnitude_firsthalf2)/length(magnitude2)
    qplot(x=x.axis2,y=magnitude_firsthalf2,geom="line") + xlim(0,0.5) + ylim(0,50 ) + xlab("Frecuencia (s)") + ylab("Magnitud")
  })
  
  output$plot2 <- renderPlot({
    inFile <- input $file1
    if (is.null(inFile))
      return(NULL)
    
    data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    colnames(data) <-c("V1", "V2")
    max <- max(data$V2)
    min <- min(data$V2)
    desv <-sd(data$V2, na.rm = FALSE)
    data <- data[(data$V2 >= min+input$Peak*desv) & (data$V2<= max-input$Peak*desv), ]
    ### calculando la transformada rapida de Fourier
    fourier2 <- fft(data$V2)
    ### calculando su espectro de potencia ##
    magnitude2 <- Mod(fourier2)
    phase2 <- Arg(fourier2)
    magnitude_firsthalf2 <- magnitude2[1:length((magnitude2)/2)]
    phase_firsthalf2 <- phase2[1:length(magnitude2/2)]
    ### generando ejes para graficar ###
    x.axis2 <- 1:length(magnitude_firsthalf2)/length(magnitude2)
    
    qplot(x=x.axis2,y=magnitude_firsthalf2,geom=c("point", "line")) + xlim(input$inxi,input$inxf) + ylim(input$inyi,input$inyf) + xlab("Frecuencia (s)") + ylab("Magnitud") + geom_text(aes(label = round(x.axis2, 5), y = magnitude_firsthalf2+2), size = 3.5)
  })
  
  output$plotnoise2 <- renderPlot({
    inFile <- input $file1
    if (is.null(inFile))
      return(NULL)
    
    data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    colnames(data) <-c("V1", "V2")
    mean <- mean(data$V2)
    desv <-sd(data$V2, na.rm = FALSE)
    data <- data[(data$V2 <= mean+input$Noise*desv) & (data$V2>= mean-input$Noise*desv), ]
    ### calculando la transformada rapida de Fourier
    fourier2 <- fft(data$V2)
    ### calculando su espectro de potencia ##
    magnitude2 <- Mod(fourier2)
    phase2 <- Arg(fourier2)
    magnitude_firsthalf2 <- magnitude2[1:length((magnitude2)/2)]
    phase_firsthalf2 <- phase2[1:length(magnitude2/2)]
    ### generando ejes para graficar ###
    x.axis2 <- 1:length(magnitude_firsthalf2)/length(magnitude2)
    
    qplot(x=x.axis2,y=magnitude_firsthalf2,geom=c("point", "line")) + xlim(input$inxin,input$inxfn) + ylim(input$inyin,input$inyfn) + xlab("Frecuencia (s)") + ylab("Magnitud") + geom_text(aes(label = round(x.axis2, 5), y = magnitude_firsthalf2+2), size = 3.5)
  })
  
output$tableperiod <- renderTable({
  inFile <- input $file1
  if (is.null(inFile))
    return(NULL)
  
  data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  colnames(data) <-c("V1", "V2")
  max <- max(data$V2)
  min <- min(data$V2)
  desv <-sd(data$V2, na.rm = FALSE)
  data <- data[(data$V2 >= min+input$Peak*desv) & (data$V2<= max-input$Peak*desv), ]
  ### calculando la transformada rapida de Fourier
  fourier2 <- fft(data$V2)
  ### calculando su espectro de potencia ##
  magnitude2 <- Mod(fourier2)
  phase2 <- Arg(fourier2)
  magnitude_firsthalf2 <- magnitude2[1:length((magnitude2)/2)]
  phase_firsthalf2 <- phase2[1:length(magnitude2/2)]
  ### generando ejes para graficar ###
  x.axis2 <- 1:length(magnitude_firsthalf2)/length(magnitude2)
  Frequency <- x.axis2[2:20]
  Magnitude <- magnitude_firsthalf2 [2:20]
  periodJ <- 1/Frequency
  PeriodGregorianDays <- (periodJ+10)*0.9999794661191
  Years <- PeriodGregorianDays/365.25
  b <- data.frame(Frequency,PeriodGregorianDays,Magnitude,Years)
  format(b, digits=3) 
})
output$tableperiodnoise <- renderTable({
  inFile <- input $file1
  if (is.null(inFile))
    return(NULL)
    
  data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  colnames(data) <-c("V1", "V2")
  mean <- mean(data$V2)
  desv <-sd(data$V2, na.rm = FALSE)
  data <- data[(data$V2 <= mean+input$Noise*desv) & (data$V2>= mean-input$Noise*desv), ]
  ### calculando la transformada rapida de Fourier
  fourier2 <- fft(data$V2)
  ### calculando su espectro de potencia ##
  magnitude2 <- Mod(fourier2)
  phase2 <- Arg(fourier2)
  magnitude_firsthalf2 <- magnitude2[1:length((magnitude2)/2)]
  phase_firsthalf2 <- phase2[1:length(magnitude2/2)]
  ### generando ejes para graficar ###
  x.axis2 <- 1:length(magnitude_firsthalf2)/length(magnitude2)
  Frequency <- x.axis2[2:20]
  Magnitude <- magnitude_firsthalf2 [2:20]
  periodJ <- 1/Frequency
  PeriodGregorianDays <- (periodJ+10)*0.9999794661191
  Years <- PeriodGregorianDays/365.25
  b <- data.frame(Frequency,PeriodGregorianDays,Magnitude,Years)
  format(b, digits=3) 
})

  output$noise <- renderPlot({
    inFile <- input $file1
    if (is.null(inFile))
      return(NULL)
    
    data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    colnames(data) <-c("V1", "V2")
    max <- max(data$V2)
    min <- min(data$V2)
    desv <-sd(data$V2, na.rm = FALSE)
    data <- data[(data$V2 >= min+input$Peak*desv) & (data$V2<= max-input$Peak*desv), ]
    ### calculando la transformada rapida de Fourier
    fourier2 <- fft(data$V2)
    ### calculando su espectro de potencia ##
    magnitude2 <- Mod(fourier2)
    phase2 <- Arg(fourier2)
    magnitude_firsthalf2 <- magnitude2[1:length((magnitude2)/2)]
    phase_firsthalf2 <- phase2[1:length(magnitude2/2)]
    ### generando ejes para graficar ###
    x.axis2 <- 1:length(magnitude_firsthalf2)/length(magnitude2)
    logx2 = log(x.axis2)
    logy2 = log(magnitude_firsthalf2) 
    qplot(x=logx2,y=logy2,geom="line") + stat_smooth(method="lm", se=FALSE) + xlab("Log(Frecuencia (s))") + ylab("Log(Magnitud)")
  })
  
  output$noisenoise <- renderPlot({
    inFile <- input $file1
    if (is.null(inFile))
      return(NULL)
    
    data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    colnames(data) <-c("V1", "V2")
    mean <- mean(data$V2)
    desv <-sd(data$V2, na.rm = FALSE)
    data <- data[(data$V2 <= mean+input$Noise*desv) & (data$V2>= mean-input$Noise*desv), ]
    ### calculando la transformada rapida de Fourier
    fourier2 <- fft(data$V2)
    ### calculando su espectro de potencia ##
    magnitude2 <- Mod(fourier2)
    phase2 <- Arg(fourier2)
    magnitude_firsthalf2 <- magnitude2[1:length((magnitude2)/2)]
    phase_firsthalf2 <- phase2[1:length(magnitude2/2)]
    ### generando ejes para graficar ###
    x.axis2 <- 1:length(magnitude_firsthalf2)/length(magnitude2)
    logx2 = log(x.axis2)
    logy2 = log(magnitude_firsthalf2) 
    qplot(x=logx2,y=logy2,geom="line") + stat_smooth(method="lm", se=FALSE) + xlab("Log(Frecuencia (s))") + ylab("Log(Magnitud)")
  })
  
  output$noisedata <- renderText({
    inFile <- input $file1
    if (is.null(inFile))
      return(NULL)
    data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    colnames(data) <-c("V1", "V2")
    max <- max(data$V2)
    min <- min(data$V2)
    desv <-sd(data$V2, na.rm = FALSE)
    data <- data[(data$V2 >= min+input$Peak*desv) & (data$V2<= max-input$Peak*desv), ]
    ### calculando la transformada rapida de Fourier
    fourier2 <- fft(data$V2)
    ### calculando su espectro de potencia ##
    magnitude2 <- Mod(fourier2)
    phase2 <- Arg(fourier2)
    magnitude_firsthalf2 <- magnitude2[1:length((magnitude2)/2)]
    phase_firsthalf2 <- phase2[1:length(magnitude2/2)]
    ### generando ejes para graficar ###
    x.axis2 <- 1:length(magnitude_firsthalf2)/length(magnitude2)
    logx2 = log(x.axis2)
    logy2 = log(magnitude_firsthalf2) 
    qplot(x=logx2,y=logy2,geom="line") + stat_smooth(method="lm", se=FALSE)
    coef(lm(logx2 ~ logy2, data = data))
  })
  
  output$noisenoisedata <- renderText({
    inFile <- input $file1
    if (is.null(inFile))
      return(NULL)
    data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    colnames(data) <-c("V1", "V2")
    mean <- mean(data$V2)
    desv <-sd(data$V2, na.rm = FALSE)
    data <- data[(data$V2 <= mean+input$Noise*desv) & (data$V2>= mean-input$Noise*desv), ]
    ### calculando la transformada rapida de Fourier
    fourier2 <- fft(data$V2)
    ### calculando su espectro de potencia ##
    magnitude2 <- Mod(fourier2)
    phase2 <- Arg(fourier2)
    magnitude_firsthalf2 <- magnitude2[1:length((magnitude2)/2)]
    phase_firsthalf2 <- phase2[1:length(magnitude2/2)]
    ### generando ejes para graficar ###
    x.axis2 <- 1:length(magnitude_firsthalf2)/length(magnitude2)
    logx2 = log(x.axis2)
    logy2 = log(magnitude_firsthalf2) 
    qplot(x=logx2,y=logy2,geom="line") + stat_smooth(method="lm", se=FALSE)
    coef(lm(logx2 ~ logy2, data = data))
  })
  
  output$help1 <- renderText( {
    if (is.null(input$file1)) { return() }
    
    "This results are the slope and intercept"
    })

output$help1noise <- renderText( {
  if (is.null(input$file1)) { return() }
  
  "This results are the slope and intercept"
})

})
