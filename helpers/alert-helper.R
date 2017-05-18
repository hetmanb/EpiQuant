prepend_shiny_alert <- function(session, id, message, alert_level) 
{
  session$sendCustomMessage("shiny_alert_handler", 
                            list(id=id, 
                                 message=message, 
                                 alert_level=alert_level)
  )}



alert_shiny_start_msg <- renderMarkdown(text=
'Welcome to epiQuant!
To get started, click a link on the sidebar.')


alert_shiny_source_msg <- renderMarkdown(text=
'To start using **SourceMatrix** either:
 1. Download and populate the template file with your own data, then upload it as a .csv in the sidebar
 1. Update the table below with the source names and scorings you want to use ')

alert_sourcematrix_heat_msg <- renderMarkdown(text=
                                                'To generate the heatmap, either
  1. Upload your scoring table and hit **Submit**
  1. If you are using the built-in table, just hit **Submit** ')

alert_sourcematrix_chord_msg <- renderMarkdown(text='  * Hit the **Submit** button to generate the chord diagram if nothing shows up.
  * Hover your mouse over the subjects to see the relationships between individual sources ')

alert_epimatrix_chord_msg <- renderMarkdown(text='  * Hit the **Submit** button to generate the chord diagram if nothing shows up.
  * Hover your mouse over the subjects to see the relationships between individual strains in your dataset.
  * Caution : if you select a threshold containing no data, the image will attempt to process indefinitely. If this happens, just reselect a new threshold and hit **Submit**. ')