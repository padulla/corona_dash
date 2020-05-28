

difference <- function(x) {
  this <- x[,-1] - x[,-ncol(x)]
  return(this)
}

percentage <- function(x) {
    this <- (100 * difference(x) / x[,-ncol(x)]) %>% round(2)
    return(this)
}

printDT <- function(x, selectedRows) {
    toPrint <- x[selectedRows,] %>% select(last_col(0:4)) 
    this <- DT::datatable(toPrint, 
                          options = list(pageLength = 20, 
                                         paging     = FALSE,
                                         searching  = FALSE,
                                         autoWidth  = TRUE,
                                         columnDefs = list(
                                           list(width = '100px', targets = 0:5, tablelayout = 'fixed')
                                         ),
                                          initComplete = DT::JS(
                                              "function(settings, json) {",
                                              "$(this.api().table().header()).css({'background-color': '#4c1112', 'color': '#fff'});",
                                              "}")
                                         ),
                          class = 'compact cell-border'
                          ) %>%  DT::formatStyle(columns = 0:5, fontSize = '14pt', color = "#4c1112")
    return(this)
}

printDTFull <- function(x, selectedRows) {
  toPrint <- x %>% select(last_col(0:4))
  table <- 
  DT::datatable(toPrint, 
                options = list(order = list(list(1, 'desc')),
                               initComplete = DT::JS(
                                   "function(settings, json) {", 
                                   "$(this.api().table().header()).css({'background-color': '#4c1112', 'color': '#fff'});",
                                   "}")
                              
                              ),
                class = 'compact cell-border',
  ) %>%  DT::formatStyle(columns = 0:ncol(toPrint), fontSize = '14pt', color = "#4c1121")
  return(table)
}

##################################################################
plotThis <- function(x, region, abbreviations = NULL, ytitle, xtitle) {
  
  if (is.null(abbreviations)) { abbreviations <- region }
  
  listRegions <- 
  x %>% 
    rownames_to_column(var = "COUNTRY") %>% 
    filter(COUNTRY %in% abbreviations) %>% 
    arrange(COUNTRY) %>% 
    select(-1) %>% 
    t()
  
  lv <- lapply(
          lapply(1:ncol(listRegions), function(x) listRegions[,x] %>% as.vector),
          function(x) {
            y <- x[x >= 100]
            if (identical(y, numeric(0))) {
              return(rep(NA, length(x)))
            } else {
              return(y)
            }
          }
        )
  
  N <- vapply(lv, function(x) max(length(x)), numeric(1)) %>% max
  
  lvtibble <- 
  lapply(lv,
         function(x) {
           y <- replace(vector(mode = 'numeric', length = N), 1:length(x), x)
           y[y == 0] <- NA
           y
         })
  
  dh <- as_tibble(do.call(cbind, lvtibble)) %>% 
          add_column(Days = 0:(N - 1)) %>% 
          rename_at(vars(-Days), ~ sort(region))
  
  p <- plot_ly(dh, x = ~Days, height = 550)
  for (place in 1:length(region)) {
      expr <- paste0("~","`", region[place], "`")
      p <- p %>% add_trace(data = dh, y = as.formula(expr), name = region[place], type = 'scatter', mode = 'lines+markers', line=list(color=plotlyColors[place]), marker=list(color=plotlyColors[place]) )
     
  }
  
  
  f1 <- list(family = 'Old Standard TT, serif', size = 14, color = 'black')
  
  xax <- list(
    title = xtitle,
    titlefont = f1,
    showticklabels = TRUE,
   # range = c(0.5, 45),
    size = 13
  )
  
  yax <- list(
    title = ytitle, 
    titlefont = f1,
    showticklabels = TRUE,
    type = "log",
    size = 50
  )
  
  leg <- list(
    orientation = 'h',
    y = -0.15,
    x = -0.05
    )
  
  p <- p %>% layout(xaxis = xax, yaxis = yax, legend = leg)
   
  names(region) <- plotlyColors[1:length(region)]
  plotColors    <- sort(region) %>% names
 
  ggfig <- dh %>% ggplot(aes(x = Days))
  for (place in region) {
      ggfig <- ggfig + 
                 geom_line( aes_(y = as.name(place), colour = place), na.rm = TRUE) +
                 geom_point(aes_(y = as.name(place), colour = place), na.rm = TRUE)
  }
  ggfig <- ggfig + 
             scale_y_log10()  +
             scale_color_manual("", values = plotColors, breaks = sort(region)) +
             theme(legend.position = "bottom") +
             labs(y = ytitle
                 ,x = xtitle
                 )
  
  return(list("plotly" = p, "ggplot" = ggfig))
}

#################################################
plotThis10 <- function(x, region, abbreviations = NULL, ytitle, xtitle) {
  
  if (is.null(abbreviations)) { abbreviations <- region }
  
  listRegions <- 
    x %>% 
    rownames_to_column(var = "COUNTRY") %>% 
    filter(COUNTRY %in% abbreviations) %>% 
    arrange(COUNTRY) %>% 
    select(-1) %>% 
    t()
  
  lv <- lapply(
    lapply(1:ncol(listRegions), function(x) listRegions[,x] %>% as.vector),
    function(x) {
      y <- x[x >= 10]
      if (identical(y, numeric(0))) {
        return(rep(NA, length(x)))
      } else {
        return(y)
      }
    }
  )
  
  N <- vapply(lv, function(x) max(length(x)), numeric(1)) %>% max
  
  lvtibble <- 
    lapply(lv,
           function(x) {
             y <- replace(vector(mode = 'numeric', length = N), 1:length(x), x)
             y[y == 0] <- NA
             y
           })
  
  dh <- as_tibble(do.call(cbind, lvtibble)) %>% 
    add_column(Days = 0:(N - 1)) %>% 
    rename_at(vars(-Days), ~ sort(region))
  
  p <- plot_ly(dh, x = ~Days, height = 550)
  for (place in 1:length(region)) {
    expr <- paste0("~","`", region[place], "`")
    p <- p %>% add_trace(data = dh, y = as.formula(expr), name = region[place], type = 'scatter', mode = 'lines+markers', line=list(color=plotlyColors[place]), marker=list(color=plotlyColors[place]) )
    
  }
  
  f1 <- list(family = 'Old Standard TT, serif', size = 14, color = 'black')
  
  xax <- list(
    title = xtitle,
    titlefont = f1,
    showticklabels = TRUE,
    # range = c(0.5, 45),
    size = 13
  )
  
  yax <- list(
    title = ytitle, 
    titlefont = f1,
    showticklabels = TRUE,
    type = "log",
    size = 50
  )
  
  leg <- list(
    orientation = 'h',
    y = -0.15,
    x = -0.05
  )
  
  p <- p %>% layout(xaxis = xax, yaxis = yax, legend = leg)
  
  names(region) <- plotlyColors[1:length(region)]
  plotColors    <- sort(region) %>% names
  
  ggfig <- dh %>% ggplot(aes(x = Days))
  for (place in region) {
    ggfig <- ggfig + 
      geom_line( aes_(y = as.name(place), colour = place), na.rm = TRUE) +
      geom_point(aes_(y = as.name(place), colour = place), na.rm = TRUE)
  }
  ggfig <- ggfig + 
    scale_y_log10()  +
    scale_color_manual("", values = plotColors, breaks = sort(region)) +
    theme(legend.position = "bottom") +
    labs(y = ytitle
         ,x = xtitle
    )
  
  return(list("plotly" = p, "ggplot" = ggfig))
}





###################################################
plotMap <- function(dataFrame, place, plotTitle, proj = "Equirectangular"){

  if (place == 'usa') {
    mode = "USA-states"
  } else {
    mode = "ISO-3"
  }
  
  # light grey boundaries
  l <- list(color = toRGB("grey"), width = 0.5)
  
  # specify map projection/options
  g <- list(
    scope = place,
    showframe = FALSE,
    showcoastlines = TRUE,
    projection = list(type = proj)
  )
  
  fig <- plot_geo(dataFrame, locationmode = mode) #, width = 800, height = 650)
  fig <- fig %>% add_trace(
      customdata = ~CASES,
      z = ~log10(CASES + 1),
      type="choropleth",
      colorscale    = "Reds",
      reversescale  = FALSE,
      showscale     = FALSE,
      hovertemplate = paste("Cases: %{customdata} <extra>%{text}</extra>"),
      text = ~COUNTRY, locations = ~CODE, marker = list(line = l)
    )
  fig <- fig %>% layout(
      title = paste0('<b>', plotTitle, '</b>'),
      geo = g,
      autosize = TRUE
  )
  
  
 
  
  fig
}


plotMortality <- function(dataFrame, region, plotTitle) {
  
  xax <-  list(
              title = "Date",
              rangeselector = list(
                buttons = list(
                  list(
                    count = 7,
                    label = "1 week",
                    step  = "day",
                    stepmode = "backward"),
                  list(
                    count = 14,
                    label = "2 weeks",
                    step  = "day",
                    stepmode = "backward"),
                  list(
                    count = 21,
                    label = "3 weeks",
                    step  = "day",
                    stepmode = "backward"),
                  list(
                    count = 30,
                    label = "4 weeks",
                    step  = "day",
                    stepmode = "backward"),
                  list(
                    step = "all") )),
        
              rangeslider = list(type = "date",
                                 thickness = 0.03)
             ) 
  
  fig <- plot_ly(dataFrame, x = ~Dates)
  for (place in region) {
    expr <- paste0("~", "`", place, "`")
    fig <- fig %>% add_trace(y = as.formula(expr), name = place, type = 'scatter', mode = 'lines+markers')
  }
  fig <- fig %>% layout(
    title = plotTitle,
    xaxis = xax,
    yaxis = list(title = "Mortality", tickformat = "%")
  )
  
  fig
}


lockdownPlot <- function(dtibble, countries, lockdown, dates) {
   
   if (is.null(names(lockdown))) { names(lockdown) <- countries }
  
   f1 <- list(family = 'Old Standard TT, serif', size = 14, color = 'black')
  
   da <- 
   dtibble %>% 
     t %>% 
     as_tibble %>% 
     rename_all(~ rownames(dtibble)) %>% 
     select(countries) %>% 
     add_column(Dates = dates[-1])

  lda <-  
  lapply(countries, function(x) da %>% 
                                  select(Dates, x) %>% 
                                  filter(Dates >= as.Date(unname(lockdown[x]))) %>% 
                                  pull(x)
        ) 
  
  N = max(vapply(lda, length, numeric(1)))
  
  db <- 
  lapply(lda, function(x) replace(rep(NA, N), 1:length(x), x)) %>% 
    do.call(cbind, .) %>% as_tibble %>% 
    rename_all(~ countries) %>% 
    add_column(Days = 0:(N-1))
  
  p <- plot_ly(db, x = ~Days)
  for (country in countries) {
       expr <- paste0("~", "`", country, "`")
       p <- p %>% add_trace(y = as.formula(expr), name = country, type = 'scatter', mode = 'lines+markers')
  }
  
  xax <- list(
    title = 'Days from Lockdown',
    titlefont = f1,
    showticklabels = TRUE,
    size = 13
  )
  
  yax <- list(
    title = 'New cases',
    titlefont = f1,
    showticklabels = TRUE,
    size = 50
  )
  
  leg <- list(
    orientation = 'h',
    y = -0.15,
    x =  0.25
    )
  p <- p %>% layout(xaxis = xax, yaxis = yax, legend = leg)
  
  p 
}
