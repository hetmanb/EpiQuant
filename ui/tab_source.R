tabItem(tabName = "source",
        shiny_alert_container('source_alert'),
        fluidRow(
          tabBox(
          title = "Source Analysis",
          id = "sourcetab1", height = "800px", width = 12,
          tabPanel("Raw Data",
                   box(width = 12,
                       status = "info",
                       shinysky::hotable("scoretable")
                   )
          ),
          tabPanel("Results",
                   box(width = 8, 
                       height = "800px",
                       status = "info",
                       d3heatmapOutput("source_heatmap", 
                                       height = "600px", 
                                       width = "700px")
                       ),
                   box(width = 4, 
                       height = "800px",
                       status = "warning",
                       # busyIndicator("Processing...", wait = 500),
                       networkD3::chordNetworkOutput('sourceChord',
                                                     height = "400px",
                                                     width = "400px")
                       # div(id = 'jschord', class = 'jschord')
                       )
          )
          )
        )
)