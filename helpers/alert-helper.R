prepend_shiny_alert <- function(session, id, message, alert_level="success") 
{
  session$sendCustomMessage("shiny_alert_handler", 
                            list(id=id, 
                                 message=message, 
                                 alert_level=alert_level)
  )}




alert_sourcematrix_start_msg <- renderMarkdown(text=
                                                 'To start using **SourceMatrix** either:
 1. Download and populate the template file with your own data
 1. Update the table below with the scorings you want to use
 1. Just hit **Submit!** ')

alert_sourcematrix_heat_msg <- renderMarkdown(text=
                                                'To generate the heatmap, either
  1. Upload your scoring table and hit **Submit**
  1. If you are using the built-in table, just hit **Submit** ')

alert_sourcematrix_chord_msg <- renderMarkdown(text='  * Hit the **Submit** button to generate the chord diagram if nothing shows up.
  * Hover your mouse over the subjects to see the relationships between individual sources ')
