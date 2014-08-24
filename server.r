library(shiny)

## Read the data from yahoo.  
vtiURL<-"http://real-chart.finance.yahoo.com/table.csv?s=VTI&d=7&e=24&f=2014&g=d&a=5&b=15&c=2001&ignore=.csv"
bndURL<-"http://real-chart.finance.yahoo.com/table.csv?s=BND&d=7&e=24&f=2014&g=d&a=3&b=10&c=2007&ignore=.csv"
spURL<-"http://real-chart.finance.yahoo.com/table.csv?s=%5EGSPC&d=7&e=23&f=2014&g=d&a=0&b=3&c=1950&ignore=.csv"
vti<-read.csv(vtiURL)
bnd<-read.csv(bndURL)
sp<-read.csv(spURL)

## Process BND Date data.  
bnd$Date<-as.POSIXct(strptime(bnd$Date, "%Y-%m-%d"))
bnd<-bnd[order(bnd$Date),]

## Process VTI data
vti$Date<-as.POSIXct(strptime(vti$Date, "%Y-%m-%d"))
## Only keep overlapping dates. 
vti<-vti[vti$Date>as.POSIXct(strptime("2007-04-09", "%Y-%m-%d")),]
vti<-vti[order(vti$Date),]

## Process S&P data
sp$Date<-as.POSIXct(strptime(sp$Date, "%Y-%m-%d"))

## Only keep overlapping dates. 
sp<-sp[sp$Date>as.POSIXct(strptime("2007-04-09", "%Y-%m-%d")),]
sp<-sp[order(sp$Date),]

shinyServer(
        function(input, output) {
         
                
                ## Function that computes the beta value.  
                ## ratio_s is original fraction stocks, t is threshold. 
                beta<-function(ratio_s=0.6, t=0.05){
                        ratio_b<-1-ratio_s #Fraction of bonds. 
                        
                        # Assume 10,000 initial investment. 
                        s<-ratio_s*10000  # Initial amount in stocks
                        b<-ratio_b*10000  # Initial amount in bonds. 
                        
                        num_s<-s/vti$Adj.Close[1] # Number of shares of stocks
                        num_b<-b/bnd$Adj.Close[1] # Number of shares of bonds
                        
                        value<-c(s+b) # Total value of portfolio. 
                        
                        n<-nrow(bnd) # Number of days.  
                        
                        for (i in 2:nrow(bnd)){
                                ## Total change in value of stocks and bonds. 
                                change_s<-(vti$Adj.Close[i]-vti$Adj.Close[i-1])*num_s
                                change_b<-(bnd$Adj.Close[i]-bnd$Adj.Close[i-1])*num_b
                                
                                ## New value of stocks and bonds. 
                                new_s<- s + change_s
                                new_b<- b + change_b
                                
                                ## Rebalancing if necessary.  
                                if(new_s/(new_s+new_b) > ratio_s + t | new_s/(new_s+new_b) < ratio_s - t ){
                                        total<-new_s+new_b 
                                        s<-ratio_s*total 
                                        b<-ratio_b*total 
                                        num_s<-s/vti$Adj.Close[i] # New number of shares of stocks
                                        num_b<-b/bnd$Adj.Close[i] # New number of shares of bonds
                                }
                                
                                ## If no re-balancing necessary
                                else{
                                        s<-new_s 
                                        b<-new_b 
                                }
                                
                                ## Keeping track of daily value of portfolio. 
                                value<-c(value, s+b)
                        }
                        
                        ## Data.frame with value of portfolio. 
                        portfolio<-data.frame(bnd$Date, value)
                        colnames(portfolio)<-c("Date", "Value")

                        ## Daily logarithmic return of portfolio.  
                        rb<-sapply(2:n, function(x) log(portfolio$Value[x]/portfolio$Value[x-1]))
                        ## Daily logarithmic return of S&P 500. 
                        ra<-sapply(2:n, function(x) log(sp$Adj.Close[x]/sp$Adj.Close[x-1]))
                        
                        ## Beta value. 
                        beta_value<-cov(rb, ra)/var(ra)
                        beta_value
                }
                
                ## Function computing the yearly rate of return.    
                ## ratio_s is original fraction stocks, t is threshold. 
                ror<-function(ratio_s=0.6, t=0.05){
                        ratio_b<-1-ratio_s #Fraction of bonds. 
                        
                        # Assume 10,000 initial investment. 
                        s<-ratio_s*10000  # Initial amount in stocks
                        b<-ratio_b*10000  # Initial amount in bonds. 
                        
                        num_s<-s/vti$Adj.Close[1] # Number of shares of stocks
                        num_b<-b/bnd$Adj.Close[1] # Number of shares of bonds
                        
                        value<-c(s+b) # Total value of portfolio. 
                        
                        n<-nrow(bnd) # Number of days.  
                        
                        for (i in 2:nrow(bnd)){
                                ## Total change in value of stocks and bonds. 
                                change_s<-(vti$Adj.Close[i]-vti$Adj.Close[i-1])*num_s
                                change_b<-(bnd$Adj.Close[i]-bnd$Adj.Close[i-1])*num_b
                                
                                ## New value of stocks and bonds. 
                                new_s<- s + change_s
                                new_b<- b + change_b
                                
                                ## Rebalancing if necessary.  
                                if(new_s/(new_s+new_b) > ratio_s + t | new_s/(new_s+new_b) < ratio_s - t ){
                                        total<-new_s+new_b 
                                        s<-ratio_s*total 
                                        b<-ratio_b*total 
                                        num_s<-s/vti$Adj.Close[i] # New number of shares of stocks
                                        num_b<-b/bnd$Adj.Close[i] # New number of shares of bonds
                                }
                                
                                ## If no re-balancing necessary
                                else{
                                        s<-new_s 
                                        b<-new_b 
                                }
                                
                                ## Keeping track of daily value of portfolio. 
                                value<-c(value, s+b)
                        }
                        
                        ## Data.frame with value of portfolio. 
                        portfolio<-data.frame(bnd$Date, value)
                        colnames(portfolio)<-c("Date", "Value")
                        

                        
                        # Logarithmic rate of return. Assume 252 trading days per year.  
                        log_ror<-log(portfolio$Value[n]/portfolio$Value[1])/(n/252)*100
                        
                        log_ror    
                }
                
                
                ## Outputs
                output$oratio_s <- renderPrint({input$ratio_s}) # input ratio stocks
                output$oratio_b <- renderPrint({1-input$ratio_s}) # ratio bonds
                output$ot <- renderPrint({input$t}) # threshold value
                output$oreturn<-renderPrint({ror(input$ratio_s,input$t)}) # Rate of return
                output$obeta<-renderPrint({beta(input$ratio_s,input$t)}) # Beta value
        }
)