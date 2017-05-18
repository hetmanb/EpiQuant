tabItem(
        tabName = "dashboard",
        shiny_alert_container('welcome_alert'),
        h2("Welcome to epiQuant"),
        p("epiQuant is a webapp for quantifying epidemiological features of bacterial isolates for use with genomic data in public health."),
        p("See the publication", a(href="http://jcm.asm.org/content/early/2017/02/09/JCM.01945-16", "here!")), 
        fluidRow(
          column(width = 8,
                 box(# Abstract from EQ Paper
                   width = NULL, 
                   title = "Abstract from the manuscript",
                   status = 'info',
                   includeMarkdown("abstract.md")
                 ),
                 box(
                   width = NULL, 
                   title = "Projects by my colleagues:",
                   status = 'danger',
                   infoBox(title = "SISTR:", href = "http://lfz.corefacility.ca/sistr-app", icon = icon("spinner"), color = "orange", fill = T, width = 6, 
                           subtitle = "The Salmonella in-silico Typing Resource"),
                   infoBox(title = "SuperPhy:", href = "http://lfz.corefacility.ca/superphy/", icon = icon("rocket"), color = "purple", fill = T, width = 6, 
                           subtitle = "Phylogenetic and epidemiological analysis of pathogens")
                 )
          ),
          
          column(width = 4,
                 box(# twitter feed:
                   width = NULL, 
                   title = "Top Rstats Tweets:",
                   status = "info",
                   a("#RStats:",
                     class="twitter-timeline",
                     href="https://twitter.com/hashtag/Rstats",
                     "data-widget-id" ="864864988397260800")
                 )
          )
        )
)