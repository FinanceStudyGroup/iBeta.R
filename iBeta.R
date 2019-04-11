#### iBeta ---------------------------------------------------------------------
Beta<-function(Y, X='^GSPC', Since=NULL, Until=NULL, Interval=2, Frequency='Weekly', Theme='BB'){
  
  # ----------------------------------------------------------------------------
  #### iBeta 5.3 (An Online Regression Beta Calculator) ------------------------
  require(quantmod)
  require(tidyquant)
  require(ggplot2)
  require(grid)
  require(gridExtra)
  require(lattice)
  
  #### Fix non-standard names --------------------------------------------------
  Fix<-function(x){
    if(class(x)=='character'){
      
      #Collect the name of the variable
      name<-deparse(substitute(x))
      
      #Rename indicies to standard names
      if(x=='^DJI'){x<-'DJIA'}
      if(x=='^GSPC'){x<-'SPX'}
      if(x=='^IXIC'){x<-'IXIC'}
      if(x=='^RUT'){x<-'RUT'}
      if(x=='^VIX'){x<-'VIX'}
      if(x=='^GSPTSE'){x<-'GSPTSE'}
      if(x=='^BVSP'){x<-'BVSP'}
      if(x=='^MXX'){x<-'MXX'}
      if(x=='^GDAXI'){x<-'GDAXI'}
      if(x=='^FTSE'){x<-'FTSE'}
      if(x=='^FCHI'){x<-'FCHI'}
      if(x=='^STOXX50E'){x<-'STOXX50E'}
      if(x=='^AEX'){x<-'AEX'}
      if(x=='^IBEX'){x<-'IBEX'}
      if(x=='FTSEMIB.MI'){x<-'FTSEMIB'}
      if(x=='^SSMI'){x<-'SSMI'}
      if(x=='PSI20.LS'){x<-'PSI20'}
      if(x=='^BFX'){x<-'BFX'}
      if(x=='^ATX'){x<-'ATX'}
      if(x=='^OMX'){x<-'OMX'}
      if(x=='^OMXC25'){x<-'OMXC25'}
      if(x=='IMOEX.ME'){x<-'MOEX'}
      if(x=='RTSI.ME'){x<-'RTSI'}
      if(x=='XU100.IS'){x<-'XU100'}
      if(x=='TA35.TA'){x<-'TA35'}
      if(x=='^TASI.SR'){x<-'SASEIDX'}
      if(x=='^N225'){x<-'NKY'}
      if(x=='^AXJO'){x<-'AS51'}
      if(x=='^SSEC'){x<-'SHCOMP'}
      if(x=='399001.SZ'){x<-'SICOM'}
      if(x=='XINA50N.FGI'){x<-'TXIN9'}
      if(x=='^DJSH'){x<-'DJSH'}
      if(x=='^HSI'){x<-'HSI'}
      if(x=='^TWII'){x<-'TWSE'}
      if(x=='^SET.BK'){x<-'SET'}
      if(x=='^KS11'){x<-'KOSPI'}
      if(x=='^JKSE'){x<-'JCI'}
      if(x=='^NSEI'){x<-'NIFTY'}
      if(x=='^BSESN'){x<-'SENSEX'}
      if(x=='PSEI.PS'){x<-'PCOMP'}
      if(x=='^STI'){x<-'FSSTI'}
      if(x=='^KSE'){x<-'KSE100'}
      
    }
    return(
      assign(name, x, env=parent.frame())
    )
  }
  # ----------------------------------------------------------------------------
  
  Xname<-NULL
  Yname<-NULL
  
  if(is.xts(X)=='FALSE'){
    Xname<-as.character(X)
  }else{
    x.name<-sub('\\..*', '', (names(X[,1])))
  }
  if(is.xts(Y)=='FALSE'){
    Yname<-as.character(Y)
  }else{
    y.name<-sub('\\..*', '', (names(Y[,1])))
  }
  
  price<-function(x, get = 'stock.prices', rename = NULL, from = NULL){
    
    if(is.xts(x)=='FALSE'){
      
      #Name the data
      name<-x
      output<-deparse(substitute(x))
      
      #Collect the data
      if(is.null(from)==FALSE){
        x<-tq_get(x, get = get, from = from)
      }else{
        x<-tq_get(x, get = get)
      }
      
      #Rename if rename is required
      if(class(rename)=='character'){
        name<-rename
      }
      
      #Extract date series
      dates<-x$date
      
      #Extract price data
      Open<-x$open
      High<-x$high
      Low<-x$low
      Close<-x$close
      Volume<-x$volume
      Adjusted<-x$adjusted
      
      # Define the data frame.
      data<-data.frame(cbind(Open,High,Low,Close,Volume,Adjusted))
      colnames(data)[1]<-paste(name,'Open', sep='.')
      colnames(data)[2]<-paste(name,'High', sep='.')
      colnames(data)[3]<-paste(name,'Low', sep='.')
      colnames(data)[4]<-paste(name,'Close', sep='.')
      colnames(data)[5]<-paste(name,'Volume', sep='.')
      colnames(data)[6]<-paste(name,'Adjusted', sep='.')
      row.names(data)<-dates
      
      # Assigning the data to the environment.
      return(
        assign(output, (data<-as.xts(data)), env=parent.frame())
      )
    }
  }
  price(Y, get = 'stock.prices', rename = NULL, from = '1900-01-01')
  price(X, get = 'stock.prices', rename = NULL, from = '1900-01-01')
  Y<-to.daily(Y)
  X<-to.daily(X)
  # ----------------------------------------------------------------------------
  
  #### Converting X- and Y-Series to a Common Length ---------------------------
  #### If the length of the Y-time series is shorter than that of the X- -------
  if((as.numeric((as.numeric(start(X)))-(as.numeric(start(Y)))))==0){
    Earliest.Date.Available<-(as.Date((max(start(X),start(Y)))))
  }
  
  if(length(Y)<length(X)){
    Earliest.Date.Available<-start(Y)
    minlength<-as.double(length(Y [,1]))
    #### Reacquiring price data from a common date (Start of the Y-series) -----
    Y<-as.xts(tail(Y, n=minlength))
    X<-as.xts(tail(X, n=minlength))
  }
  
  #### If the length of the X-time series is shorter than that of the Y- -------
  if(length(Y)>length(X)){
    Earliest.Date.Available<-start(X)
    minlength<-as.double(length(X [,1]))
    #### Reacquiring price data from a common date (Start of the X-series) -----
    Y<-as.xts(tail(Y, n=minlength))
    X<-as.xts(tail(X, n=minlength))
  }
  
  #### Removable Control Module (3.0) ------------------------------------------
  # ----------------------------------------------------------------------------
  if(class(Since)=='character'){
    Since<-as.Date(Since)
  }
  if(class(Until)=='character'){
    Until<-as.Date(Until)
  }
  if(class(Interval)=='numeric'){
    Interval<-as.double(Interval)
  }
  ### Scenario  1: No Parameters are specified (Implied) -----------------------
  ### Scenario 2B: 'Since and 'Until' Parameters are specified -----------------
  if(class(Since)=='Date'){
    if(class(Until)=='Date'){
      Since=as.Date(max(Since,Earliest.Date.Available))
      X<-X[paste(Since,Until,sep='::')]
      Y<-Y[paste(Since,Until,sep='::')]
    }
  }
  ### Scenario 2C: Only the 'Until' Parameter is specified ---------------------
  if(class(Until)=='Date'){
    X<-X[paste('',Until,sep='::')]
    Y<-Y[paste('',Until,sep='::')]
  }else{
    ### Scenario 2A: The 'Since' Parameter is specified ------------------------
    if(class(Since)=='Date'){
      Since=as.Date(max(Since,Earliest.Date.Available))
      X<-X[paste(Since,'',sep='::')]
      Y<-Y[paste(Since,'',sep='::')]
    }else{
      ### Scenario  3: Interval Mode (Takes precedence) ------------------------
      if(class(Interval)=='numeric'){
        if((Interval)>0){
          Since=as.Date((max(end(X),end(Y))))-(Interval*365)
          Until=as.Date((min(end(X),end(Y))))
          Since=as.Date(max((max(start(X),start(Y))),Since))
          X<-X[paste(Since,Until,sep='::')] # This might work
          Y<-Y[paste(Since,Until,sep='::')] # This might work
        }
      }
    }
  }
  
  #### Reconvert to Common Length (Failsafe) -----------------------------------
  #### Converting X- and Y-Series to a Common Length. ####
  #### If the length of the Y-time series is shorter than that of the X- -------
  if(length(Y)<length(X)){
    Earliest.Date.Available<-start(Y)
    minlength<-as.double(length(Y [,1]))
    #### Reacquiring price data from a common date (Start of the Y-series) -----
    Y<-as.xts(tail(Y, n=minlength))
    X<-as.xts(tail(X, n=minlength))
  }
  
  #### If the length of the X-time series is shorter than that of the Y- -------
  if(length(Y)>length(X)){
    Earliest.Date.Available<-start(X)
    minlength<-as.double(length(X [,1]))
    #### Reacquiring price data from a common date (Start of the X-series) -----
    Y<-as.xts(tail(Y, n=minlength))
    X<-as.xts(tail(X, n=minlength))
  }
  #### End of Removable Control Module -----------------------------------------
  # ----------------------------------------------------------------------------
  
  #### Determining Frequency: If Daily, Convert to Daily -----------------------
  if(Frequency=='Daily'){
    #### Converting data to Daily, if Frequency = Daily ------------------------
    adj.Y<-to.daily(Y)
    adj.X<-to.daily(X)
    pardiff<-5
  }
  
  #### Determining Frequency: If Weekly, Convert to Weekly ---------------------
  if(Frequency=='Weekly'){
    #### Converting data to Weekly, if Frequency = Weekly ----------------------
    adj.Y<-to.weekly(Y)
    adj.X<-to.weekly(X)
    pardiff<-5
  }
  
  #### Determining Frequency: If Monthly, Convert to Monthly -------------------
  if(Frequency=='Monthly'){
    #### Converting data to Monthly, if Frequency = Monthly --------------------
    adj.Y<-to.monthly(Y)
    adj.X<-to.monthly(X)
    pardiff<-25
  }
  
  #### Closing Price Extraction ------------------------------------------------
  #### Extracting the columns containing closing price data --------------------
  YClose<-as.xts(adj.Y[,4])                  
  XClose<-as.xts(adj.X[,4])
  Latest.Date.Used<-as.Date(max(end(XClose),end(YClose)))
  Latest.to.Today<-as.double(difftime(as.Date(Sys.Date()),Latest.Date.Used))
  
  #### Returns Calculation -----------------------------------------------------
  #### Converting Price data to Returns data -----------------------------------
  YReturns<-(diff(YClose)/YClose[-length(YClose)])
  XReturns<-(diff(XClose)/XClose[-length(XClose)])
  
  #### Aggregation into Data Frame ---------------------------------------------
  #### Creating a data frame to make the plot ----------------------------------
  GGFRAME<-data.frame(as.xts(merge(XReturns, YReturns)))
  GGFRAME<-data.frame(na.omit(GGFRAME))
  
  if(Frequency=='Daily'){
    Since<-as.Date(rownames(GGFRAME)[1]) #Error here if Frequency = 'Monthly'. The 'if' statements obviate the error.
    Until<-as.Date(rownames(GGFRAME)[(length(rownames(GGFRAME)))]) #Error here if Frequency = 'Monthly'
    Observations<-(length(rownames(GGFRAME)))
  }
  if(Frequency=='Weekly'){
    Since<-as.Date(rownames(GGFRAME)[1]) #Error here if Frequency = 'Monthly'. The 'if' statements obviate the error.
    Until<-as.Date(rownames(GGFRAME)[(length(rownames(GGFRAME)))]) #Error here if Frequency = 'Monthly'
    Observations<-(length(rownames(GGFRAME)))
  }
  Observations<-(length(rownames(GGFRAME)))
  
  #### Three-step process for making the x- and y- labels ----------------------
  #### Setting up dates --------------------------------------------------------
  xdate<-max((start(XClose)),(start(YClose)))
  ydate<-max((start(XClose)),(start(YClose)))
  
  #### Here are the beginning month and year -----------------------------------
  xmonth<-paste(format(xdate, '%b'), format(xdate, '%Y'))
  ymonth<-paste(format(ydate, '%b'), format(ydate, '%Y'))
  
  #### Names of the variables --------------------------------------------------
  if(is.null(Xname)=='FALSE'){
    x.name<-Xname
  }
  if(is.null(Yname)=='FALSE'){
    y.name<-Yname
  }
  
  x.name<-Fix(x.name)
  y.name<-Fix(y.name)
  
  #### If Beta is taken over an interval, here are the ending month and year ---
  if((Latest.to.Today)>=pardiff){
    xend<-end(XClose)
    yend<-end(YClose)
    
    xmonth.end<-paste(format(xend, '%b'), format(xend, '%Y'))
    ymonth.end<-paste(format(yend, '%b'), format(xend, '%Y'))
    
    ### If Beta is taken over an interval, here are the x- and y- labels -------
    xlabel<-paste0((Frequency),' Returns on ', x.name, ' (From ', (xmonth),',',' To ',(xmonth.end),')  [Interval-Bound]')
    ylabel<-paste0((Frequency),' Returns on ', y.name, ' (From ', (ymonth),',',' To ',(ymonth.end),')')
  }
  
  #### Setting up regular x-, and y-labels -------------------------------------
  if((Latest.to.Today)<pardiff){
    xlabel<-paste0((Frequency),' Returns on ', x.name, ' (Since ', (xmonth),')')
    ylabel<-paste0((Frequency),' Returns on ', y.name, ' (Since ', (ymonth),')')
  }
  
  #### Generating a linear model to find Beta ----------------------------------
  Results<-lm(Y.Close ~ X.Close, GGFRAME)
  
  #### Extracting coefficients -------------------------------------------------
  cf <- round(coef(Results),6)
  
  #### Restate the 'Interval' variable -----------------------------------------
  Interval<-signif(as.double((difftime(max(end(adj.Y),end(adj.X)),min(start(adj.Y),start(adj.X)), units='days')/360)), digits=4)
  
  #### Calculating Time Interval and Unit for Reporting ------------------------
  if(difftime(max(end(adj.Y),end(adj.X)),min(start(adj.Y),start(adj.X)), units='days')>300) {
    ReportingInterval<-round(as.double((difftime(max(end(adj.Y),end(adj.X)),min(start(adj.Y),start(adj.X)), units='days')/360)), digits=0)
    Unit<-'-Year, '
    t.name<-paste(ReportingInterval, 'Y', sep='')
  }
  
  #### Calculating Time Interval and Unit for Reporting ------------------------
  if(difftime(max(end(adj.Y),end(adj.X)),min(start(adj.Y),start(adj.X)), units='days')<300) {
    ReportingInterval<-round(as.double((difftime(max(end(adj.Y),end(adj.X)),min(start(adj.Y),start(adj.X)), units='days')/30)), digits=0)
    Unit<-'-Month, '
    t.name<-paste(ReportingInterval, 'M', sep='')
  }
  
  #### Reporting ---------------------------------------------------------------
  GraphTitle1<-paste0('Beta Calculation: ', y.name,' (', ReportingInterval, Unit, Frequency,'): ')
  ### Beta ---------------------------------------------------------------------
  GraphTitle2<-paste0('Beta = ')
  BetaValue<-(as.double(sprintf('%.4f', (cf[2]))))
  GraphTitle3<-paste0(', ')
  ### Alpha --------------------------------------------------------------------
  GraphTitle4<-paste0('Alpha = ')
  Alpha.percent<-(as.double(cf[1])*100)
  GraphTitle5<-paste0(', ')
  ### R-Squared ----------------------------------------------------------------
  GraphTitle6<-paste0(' = ')
  R.Squared<-as.double((format(summary(Results)$r.squared, digits=4)))
  # ----------------------------------------------------------------------------
  
  #### Setting up base plot, color, linear regression, and theme ---------------
  #### Setting up common limits ------------------------------------------------
  if(Theme=='GV'){
    plotcolor=rgb(0.4,0,1) # --------------------------------------------------- 1
    linecolor='blue' # --------------------------------------------------------- 2
    pointsize=3 # -------------------------------------------------------------- 3
    textcolor=rgb(0.15,0.15,0.15) # -------------------------------------------- 4
    background=rgb(1,1,1) # ---------------------------------------------------- 5
    axislines=rgb(0.75,0.75,0.75) # -------------------------------------------- 6
    gridlines=rgb(0.9,0.9,0.9) # ----------------------------------------------- 7
    shade=rgb(0.8,0.8,0.8) # --------------------------------------------------- 8
  }
  
  if(Theme=='BB'){
    plotcolor='orange' # ------------------------------------------------------- 1
    linecolor='red' # ---------------------------------------------------------- 2
    pointsize=2 # -------------------------------------------------------------- 3
    textcolor=rgb(1,1,1) # ----------------------------------------------------- 4
    background=rgb(0,0,0) # ---------------------------------------------------- 5
    axislines=rgb(0.25,0.25,0.25) # -------------------------------------------- 6
    gridlines=rgb(0.15,0.15,0.15) # -------------------------------------------- 7
    shade=rgb(0.5,0.5,0.5) # --------------------------------------------------- 8
  }
  
  xrange <- range(pretty((GGFRAME[,1])))
  yrange <- range(pretty((GGFRAME[,2])))
  
  #### Setting up y-margin histogram -------------------------------------------
  p.left <- ggplot(GGFRAME, aes(Y.Close)) +
    geom_vline(aes(xintercept=0), colour=axislines, linetype='solid')+
    geom_density(color=linecolor, size=0.65, fill=shade, alpha=0.45)+
    geom_histogram(fill=plotcolor,
                   color='black', alpha=0.7) +
    #geom_vline(data=GGFRAME, aes(xintercept=mean(as.numeric(Y.Close)), colour='red', linetype='solid'))+
    #geom_vline(data=GGFRAME, aes(xintercept=quantile(as.numeric(Y.Close), 0.25), colour='red', linetype='solid'))+
    #geom_vline(data=GGFRAME, aes(xintercept=quantile(as.numeric(Y.Close), 0.75), colour='red', linetype='solid'))+
    lims(x = yrange) +
    coord_flip() +
    theme_light() +
    xlab(ylabel)+
    ylab('')+
    theme(
      text = element_text(colour=textcolor),
      panel.background = element_rect(fill=background),
      rect = element_rect(fill =background, color=background),
      axis.text.y = element_text(colour=textcolor),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major = element_line(colour =gridlines),
      panel.grid.minor = element_line(colour =gridlines),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.margin = unit(c(1, 0.05, 0.05, 1), 'lines'),
      legend.position='none'
    )
  
  #### Setting up empty plot ---------------------------------------------------
  p.blank <- ggplot() +
    theme_void() +
    theme(plot.margin = unit(rep(0, 4), 'lines'))
  
  #### Setting up main plot ----------------------------------------------------
  p.main <- ggplot(GGFRAME, aes(X.Close, Y.Close))+
    geom_vline(aes(xintercept=0), colour=axislines, linetype='solid')+
    geom_hline(aes(yintercept=0), colour=axislines, linetype='solid')+
    geom_smooth(method='lm', se=TRUE, color=linecolor, fill=shade, size=0.70)+
    geom_point(color=plotcolor, size=pointsize, alpha=0.7)+
    geom_abline(aes(slope=cf[2],intercept=cf[1]), color=linecolor)+
    annotate('text', x=-Inf, y=Inf, label='hat(y) == beta(x) + alpha', hjust=-.2, vjust=2, size=(5), parse=TRUE, col=textcolor) +
    lims(x=xrange, y=yrange)+
    theme_light() +
    theme(
      panel.background = element_rect(fill=background),
      rect = element_rect(fill =background, color=background),
      panel.grid.major = element_line(colour =gridlines),
      panel.grid.minor = element_line(colour =gridlines),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.margin = unit(c(1, 1, 0.05, 0.05), 'lines'),
      legend.position='none'
    )
  
  #### Setting up x-margin histogram -------------------------------------------
  p.bottom <- ggplot(GGFRAME, aes(X.Close)) +
    geom_vline(aes(xintercept=0), colour=axislines, linetype='solid')+
    geom_density(color=linecolor, size=0.65, fill=shade, alpha=0.45)+
    geom_histogram(fill=plotcolor,
                   color='black', alpha=0.7) +
    #geom_vline(data=GGFRAME, aes(xintercept=mean(as.numeric(X.Close)), colour='red', linetype='solid'))+
    #geom_vline(data=GGFRAME, aes(xintercept=quantile(as.numeric(X.Close), 0.25), colour='red', linetype='solid'))+
    #geom_vline(data=GGFRAME, aes(xintercept=quantile(as.numeric(X.Close), 0.75), colour='red', linetype='solid'))+
    lims(x = xrange) +
    theme_light() +
    xlab(xlabel)+
    ylab('')+
    theme(
      text = element_text(colour=textcolor),
      panel.background = element_rect(fill=background),
      rect = element_rect(fill =background, color=background),
      axis.text.x = element_text(colour=textcolor), 
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major = element_line(colour =gridlines),
      panel.grid.minor = element_line(colour =gridlines),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      plot.margin = unit(c(0.05, 1, 1, 0.05), 'lines'),
      legend.position='none'
    )
  
  lm <- matrix(1:4, nrow = 2)
  
  GGGRID<-grid.arrange(
    p.left, p.blank, p.main, p.bottom,
    top = textGrob((paste0('\n',(GraphTitle1), (GraphTitle2), (BetaValue), (GraphTitle3), (GraphTitle4), (Alpha.percent), '%', (GraphTitle5), 'R-Squared', (GraphTitle6), (R.Squared), '\n   ')), gp=gpar(col=textcolor, cex=1.1, fontface='bold')),
    #(paste0('\n',(GraphTitle1), (GraphTitle2), (BetaValue), (GraphTitle3), (GraphTitle4), (Alpha.percent), '%', (GraphTitle5), 'R-Squared', (GraphTitle6), (R.Squared))),
    layout_matrix = lm,
    widths = c(1, 5),
    heights = c(5, 1),
    padding = unit(0.05, 'line')
  )
  
  grid.draw(grobTree(rectGrob(gp=gpar(fill=background, col=background, lwd=0.05)), GGGRID))
  
  # ----------------------------------------------------------------------------
  #### Generating the Beta, and labelling it by its parameters -----------------
  if(Frequency=='Daily'){
    freq.name<-'d'
  }
  if(Frequency=='Weekly'){
    freq.name<-'w'
  }
  if(Frequency=='Monthly'){
    freq.name<-'m'
  }
  if((Latest.to.Today)>=pardiff){
    freq.name<-paste(freq.name, 'p', sep='.')
  }
  
  BetaName <- paste('Beta', x.name, t.name, freq.name, y.name, sep='.')
  Beta     <- as.double(sprintf('%.4f', (cf[2])))
  Adj.Beta <- as.double(sprintf('%.4f', ((cf[2]))*(2/3)+(1/3)))
  # ----------------------------------------------------------------------------
  
  return(
    #### Beta ------------------------------------------------------------------
    assign(BetaName, data.frame(Beta,Adj.Beta,Alpha.percent,R.Squared,Interval,Frequency,Observations,Since,Until,row.names = BetaName), env=parent.frame())
  )
}