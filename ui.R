library(shiny)
shinyUI(pageWithSidebar(
        headerPanel("A starting portfolio"),
        sidebarPanel(
                conditionalPanel(condition="input.conditionedPanels==1",
                                 helpText("Inputs"), 
                ## The inputs
                numericInput('ratio_s', 'Fraction in stocks', 0.6, min = 0, max = 1, step = 0.05),
                numericInput('t', 'Rebalancing threshold', 0.05, min = 0, max = 0.2, step = 0.01)
                                 
                ),
                conditionalPanel(condition="input.conditionedPanels==2",
                                 helpText("Documentation")
                ) 
        ),
        mainPanel(
                tabsetPanel(
                        tabPanel("Outputs", value=1, 
                                 h4('Fraction Stocks (VTI)'),
                                 verbatimTextOutput("oratio_s"),
                                 h4('Fraction Bonds (BND)'),
                                 verbatimTextOutput("oratio_b"),
                                 h4('Threshold'),
                                 verbatimTextOutput("ot"),
                                 h4('Average annual return (%)'),
                                 verbatimTextOutput("oreturn"),
                                 h4('Beta value relative to S&P 500'),
                                 verbatimTextOutput("obeta")
                                 ), 
                        tabPanel("Documentation", value=2,
                                "This app computes the average yearly return and beta value relative 
                                to the S&P 500 for a portfolio of Vanguard's total Stock Market ETF, VTI, and 
                                Vanguard's total Bond Market ETF, BND.  The time period is from April 10, 2007 (the inception of
                                BND) to the current date.  The inputs are the fraction of your portfolio
                                consisting of VTI (this automatically determines the fraction consisting of BND), and a threshold
                                parameter.  

                                The threshold parameter determines when the portfolio should be rebalanced. 
                                This means that if your fraction of VTI veers away from the original 
                                fraction by at least the threshold parameter, the portfolio will be rebalanced 
                                to the original ratio.  
                                
                                The data is taken from yahoo finance.  The app uses the adjusted closing price as the value of the assest
                                each day. "
                                 )
                        , id = "conditionedPanels"
                )
        )
))