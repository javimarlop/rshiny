library(shiny)

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  # Reactive expression to generate the requested distribution. This is 
  # called whenever the inputs change. The renderers defined 
  # below then all use the value computed from this expression
  data <- reactive({  
    dynasys <- switch(
                   input$dynasys,
                   
                   lorenz3d = out.lor.3d,
                   lorenz2d = out.lor.2d,
                   
                   rossler3d =  out.ros.3d,
                   rossler2d =  out.ros.2d,
                   
                   pleiade =  out.plei,
                   pleiadeall =  out.plei.all,
                   pleiade4 =  out.plei.4df,
                   pleiade4all =  out.plei.4all,
                   
                   mandel = mandel.new.df,
                   
                   julia = julia.new.df,
                   
                   catalitic3d = catal.df,
                   catalitic2d = catal.df.2d
                   
                   
                   ) #dynasys <- swi
    
  }) #data <- react  
  
  
#-----------------------------------------
  # Generate a plot of the data. Also uses the inputs to build the 
  # plot label. Note that the dependencies on both the inputs and
  # the 'data' reactive expression are both tracked, and all expressions 
  # are called in the sequence implied by the dependency graph
  output$plot <- renderPlot({
    
   # Lorenz - 3D
   if(dim(data())[2] == 5 & names(data())[5]=="lor3d" ) {  
    scatterplot3d(
                    data()[,2], data()[,3], data()[,4]
                   ,xlab="Y", ylab="X", zlab="Z", main="Lorenz Attractor"
                   ,color=round(data()[,1],1)
                   ,col.axis="blue", angle=55, col.grid="lightblue", cex.symbols=0.8
                  )
   } 
   
   # Lorenz - 2D
   if( dim(data())[2] == 5 & names(data())[5]=="lor2d" ) {  
     par(mfrow=c(2,2))
     plot(data()[,1], data()[,2], xlab="time", ylab="X", type="l")
     plot(data()[,1], data()[,3], xlab="time", ylab="Y", type="l")
     plot(data()[,1], data()[,4], xlab="time", ylab="Z", type="l")
     plot(data()[,2], data()[,3], xlab="X", ylab="Y", type="l")
   }  
   
   
   # Rossler - 3D
   if(dim(data())[2] == 5 & names(data())[5]=="ros3d" ) {  
     scatterplot3d(
       data()[,2], data()[,3], data()[,4]
       ,xlab="X", ylab="Y", zlab="Z", main="Rossler Attractor"
       ,color=round(data()[,1],1)
       ,col.axis="blue", angle=55, col.grid="lightblue", cex.symbols=0.8
     )
   } 
   
   # Rossler - 2D
   if( dim(data())[2] == 5 & names(data())[5]=="ros2d" ) {  
     par(mfrow=c(2,2))
     plot(data()[,1], data()[,2], xlab="time", ylab="X", type="l")
     plot(data()[,1], data()[,3], xlab="time", ylab="Y", type="l")
     plot(data()[,1], data()[,4], xlab="time", ylab="Z", type="l")
     plot(data()[,2], data()[,3], xlab="X", ylab="Y", type="l")
   } 
   
   
   #-------------------------
   # Pleiade 7 stars - All charts
   if(dim(data())[2] == 29) {
     par(mfrow = c(3, 3))
     # Individual values
     for (i in 1:7) {
       plot(data()[,i+1], data()[,i+8], type = "l",
            main = paste("star ",i), xlab = "x", ylab = "y")
       points (yini.plei[i], yini.plei[i+7])
     }
     
     # All stars together
     col.val <- c('red','black','green','blue','yellow', 'navy', 'orange')
     for(i in 1:7) {
       if(i==1) { 
         plot(
           data()[,i], data()[,i+7], col=col.val[i], xlab="X", ylab="Y"
           ,xlim=range(data()[,1:7]), ylim=range(data()[,8:14]) 
           ,main="7 Stars"
         )
       } else {
         points(data()[,i], data()[,i+7], col=col.val[i])
       }
     } 
     
     # Velocities 
     matplot(data()[,"time"], data()[, c("u1", "u7")], type = "l",
             lwd = 2, col = c("black", "grey"), lty = 1,
             xlab = "time", ylab = "velocity", main = "stars 1, 7")
     abline(v = c(1.23, 1.68), lty = 2)
     legend("bottomright", col = c("black", "grey"), lwd = 2,
            legend = c("u1", "u7")) 
     
   } 
      
    # Pleiade - All - 7 stars - just chart with all the stars together
    if(dim(data())[2]==14) {
      col.val <- c('red','black','green','blue','yellow', 'navy', 'orange')
      
      for(i in 1:7) {
        if(i==1) { 
                   plot(
                          data()[,i], data()[,i+7], col=col.val[i], xlab="X", ylab="Y"
                         ,xlim=range(data()[,1:7]), ylim=range(data()[,8:14]) 
                         ,main="7 Stars"
                       )
        } else {
                   points(data()[,i], data()[,i+7], col=col.val[i])
        }
      }
    }  
   
   
   #-------------------------
   # Pleiade 4 stars
   if(dim(data())[2] == 17 ) {
     par(mfrow = c(2, 3))
     # Individual values
     for (i in 1:4) {
       plot(data()[,i+1], data()[,i+5], type = "l",
            main = paste("star ",i), xlab = "x", ylab = "y")
       points (yini.plei[i], yini.plei[i+4])
     }
     
     # All stars together
     col.val <- c('red','black','green','blue')
     for(i in 1:4) {
       if(i==1) { 
         plot(
           data()[,i], data()[,i+4], col=col.val[i], xlab="X", ylab="Y"
           ,xlim=range(data()[,1:4]), ylim=range(data()[,5:8]) 
           ,main="4 Stars"
         )
       } else {
         points(data()[,i], data()[,i+4], col=col.val[i])
       }
     } 
     
     # Velocities 
     matplot(data()[,"time"], data()[, c("u1", "u4")], type = "l",
             lwd = 2, col = c("black", "grey"), lty = 1,
             xlab = "time", ylab = "velocity", main = "stars 1, 4")
     abline(v = c(1.23, 1.68), lty = 2)
     legend("bottomright", col = c("black", "grey"), lwd = 2,
            legend = c("u1", "u4")) 
     
   } #if(dim(data())[2] == 17 )
   
   # Pleiade - All - 4 stars - just chart with all the stars together
   if(dim(data())[2]== 8) {
     col.val <- c('red','black','green','blue')
     
     for(i in 1:4) {
       if(i==1) { 
         plot(
           data()[,i], data()[,i+4], col=col.val[i], xlab="X", ylab="Y"
           ,xlim=range(data()[,1:4]), ylim=range(data()[,5:8]) 
           ,main="4 Stars"
         )
       } else {
         points(data()[,i], data()[,i+4], col=col.val[i])
       }
     }
   }  
   
   
   
   # Mandel 
   if( dim(data())[1] > 300000  ) {  
     
     plot(
          data()[,"X"], data()[,"Y"]
         ,col=data()[,"color"]
         ,xlab="", ylab="", main="Mandelbrot Set"
          )
     
   } 
   
   
   # Julia
   if( dim(data())[1] > 150000 & dim(data())[1] < 170000  ) {  
     
     plot(
       data()[,"X"], data()[,"Y"]
       ,col=data()[,"color"]
       ,xlab="", ylab="", main="Julia Set"
     )
     
   }  
   
   # Catalytic - 3D
   if( dim(data())[1] == 4 & dim(data())[2] == 10001  ) {  
     
     red_den <- max(data()[1,])
     green_den <- max(data()[2,])
     blue_den <- max(data()[3,])
     scatterplot3d(
                    data()[1,], data()[2,], data()[3,]
                   ,xlab="X", ylab="Y", zlab="Z", main="Catalytic Network Attractor"
                   #,color=rgb( data()[1,]/red_den, data()[2,]/green_den, data()[3,]/blue_den )
                   ,color=round(data()[3],1)
                   ,col.axis="blue", angle=40, col.grid="lightblue", cex.symbols=0.8
     )
     
   }
   
   # Catalytic - 2D
   if( dim(data())[1] == 4 & dim(data())[2] == 10002  ) {  
     
     out <- data()
     out <- out[,c(1:10001)]
     red_den <- max(out[1,])
     green_den <- max(out[2,])
     blue_den <- max(out[3,])
     col.rgb <- rgb( data()[1,]/red_den, data()[2,]/green_den, data()[3,]/blue_den )
     
     par(mfrow=c(3,2)) 
     plot(
           t(out[1,1:10001]), t(out[2,])
          ,lty=1, type="l" ,col=col.rgb
          ,xlab="X1", ylab="X2"
     )
     plot(
           t(out[1,1:10001]), t(out[3,])
          ,lty=1, type="l" ,col=col.rgb
          ,xlab="X1", ylab="X3"
     )
     plot(
           t(out[1,1:10001]), t(out[4,])
          ,lty=1, type="l" ,col=col.rgb
          ,xlab="X1", ylab="X4"
     )
     plot(
           t(out[2,1:10001]), t(out[3,])
          ,lty=1, type="l" ,col=col.rgb
          ,xlab="X2", ylab="X3"
     )
     plot(
           t(out[2,1:10001]), t(out[4,])
          ,lty=1, type="l" ,col=col.rgb
          ,xlab="X2", ylab="X4"
     )
     plot(
           t(out[3,1:10001]), t(out[4,])
          ,lty=1, type="l" ,col=col.rgb
          ,xlab="X3", ylab="X4"
     )
     
   }  
   
   
}) #output$plot <- renderPlot({
  

#-----------------------------------------
# Description of the Dynamic System
  output$descrip <- renderPrint({
    
   # Lorenz - 3D
   if(dim(data())[2] == 5 & names(data())[5]=="lor3d" ) {  
     lor.des <- c(' dX = A * X + Y * Z\n'
                  ,'dY = B * (Y - Z)\n'
                  ,'dZ = -X * Y + C * Y - Z') 
     cat(lor.des) 
   }
   
   
   # Lorenz - 2D
   if( dim(data())[2] == 5 & names(data())[5]=="lor2d" ) {  
     lor.des <- c(' dX = A * X + Y * Z\n'
                  ,'dY = B * (Y - Z)\n'
                  ,'dZ = -X * Y + C * Y - Z') 
     cat(lor.des) 
   }
   
   # Rossler - 3D
   if(dim(data())[2] == 5 & names(data())[5]=="ros3d" ) {  
     ros.des <- c(' dy1 = a * y[2] - y[3]\n'
                  ,'dy2 = y[1] + b * y[2]\n'
                  ,'dy3 = b + y[3] * (y[1] - c)')
     cat(ros.des)
   }
     
   # Rossler - 2D
   if( dim(data())[2] == 5 & names(data())[5]=="ros2d" ) {  
     ros.des <- c(' dy1 = a * y[2] - y[3]\n'
                  ,'dy2 = y[1] + b * y[2]\n'
                  ,'dy3 = b + y[3] * (y[1] - c)')
     cat(ros.des)
   }
   
   
   # Pleiade
   if(dim(data())[2] == 29) {
     plei.des <- c(
                   ' dxi = ui\n'
                  ,'dyi = vi\n'
                  ,'dui = G * Sum [ mj * (xj - xi) / rij^(3/2) ]\n'
                  ,'dvi = G * Sum [ mj * (xj - xi) / rij^(3/2) ]'
                 )
     cat(plei.des)
   }
   
    # Pleiade - All
    if(dim(data())[2]==14) {
     plei.des <- c(
                   ' dxi = ui\n'
                  ,'dyi = vi\n'
                  ,'dui = G * Sum [ mj * (xj - xi) / rij^(3/2) ]\n'
                  ,'dvi = G * Sum [ mj * (xj - xi) / rij^(3/2) ]'
                 )
     cat(plei.des)
    }
   
   
   #-------------------------
   # Pleiade 4 stars
   if(dim(data())[2] == 17 ) {
     plei4.des <- c(
       ' dxi = ui\n'
       ,'dyi = vi\n'
       ,'dui = G * Sum [ mj * (xj - xi) / rij^(3/2) ]\n'
       ,'dvi = G * Sum [ mj * (xj - xi) / rij^(3/2) ]\n'
       ,'\n'
       ,'https://vimeo.com/74538256 - (Same Velocity)\n'
       ,'https://vimeo.com/74242572 - (Normal)\n'
       ,'https://vimeo.com/74537332 - (Long)\n'
       ,'https://vimeo.com/74540411 - (One mass different)\n'
     )
     cat(plei4.des) 
   }
   
   #-------------------------
   # Pleiade 4 stars
   if(dim(data())[2] == 8 ) {
     plei4.des <- c(
       ' dxi = ui\n'
       ,'dyi = vi\n'
       ,'dui = G * Sum [ mj * (xj - xi) / rij^(3/2) ]\n'
       ,'dvi = G * Sum [ mj * (xj - xi) / rij^(3/2) ]\n'
       ,'\n'
       ,'https://vimeo.com/74538256 - (Same Velocity)\n'
       ,'https://vimeo.com/74242572 - (Normal)\n'
       ,'https://vimeo.com/74537332 - (Long)\n'
       ,'https://vimeo.com/74540411 - (One mass different)\n'
     )
     cat(plei4.des) 
   } 
   
   
   # Mandelbrot
   if(dim(data())[1] == 1 ) {  
    mandel.des <- c('Z = Z^2 + C') 
    cat(mandel.des)
   }
   
   # Mandelbrot 2
   if(dim(data())[1] > 300000 ) {  
    mandel.des <- c('Z = Z^2 + C') 
    cat(mandel.des)
   }
   
   
   # Julia 
   if(dim(data())[1] > 150000 & dim(data())[1] < 170000 ) {  
    julia.des <- c('Z = Z^2 + C') 
    cat(julia.des)
   }
   
   # Catalytic - 3D
   if(dim(data())[1] == 4 & dim(data())[2] == 10001 ) {  
     #catal.des <- c('Catalytic Network Dynamic') 
     catal.des <- c('x1 [0.8 - 0.00125 x1^2 - 0.0025 x2^2 + x2 (1.4995 - 0.005 x3) + 0.4995 x3 - 0.0015 x3^2 + x1 (0.4995 - 0.00775 x2 - 0.00125 x3 - 0.008 x4) + 0.9995 x4 - 0.001 x3 x4]')
     cat(catal.des)
   } 
   
   # Catalytic - 2D
   if(dim(data())[1] == 4 & dim(data())[2] == 10002 ) {  
     #catal.des <- c('Catalytic Network Dynamic') 
     catal.des <- c('x1 [0.8 - 0.00125 x1^2 - 0.0025 x2^2 + x2 (1.4995 - 0.005 x3) + 0.4995 x3 - 0.0015 x3^2 + x1 (0.4995 - 0.00775 x2 - 0.00125 x3 - 0.008 x4) + 0.9995 x4 - 0.001 x3 x4]')
     cat(catal.des)
   } 
   
}) #output$descrip <- renderPrint({
  
  
  
#  output$view <- renderTable({
#    kk <- data()[,2]
#    head(kk, n = 10)
#  })  
  
  
})   # shinyServer(f