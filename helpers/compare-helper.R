
CompareMatrix <- function(cgf_data, epi_data) {
# Import data from shiny 
  cgf<- cgf_data
  epi <- epi_data
  row.names(cgf) <- colnames(cgf)
  row.names(epi) <- colnames(epi)
  
  melt_cgf <- melt(as.matrix(cgf))
  melt_epi <- melt(as.matrix(epi))
  
# Rank the similarities from min - max, to allow for 'fair' comparisons
  melt_cgf$rank <- rank(melt_cgf$value, ties.method = 'random')
  melt_epi$rank <- rank(melt_epi$value, ties.method = 'random')
  
  
  merged_data <- merge(x=melt_cgf, y=melt_epi, by = c("Var1", "Var2"), all = T)
  merged_data$rank.x <- merged_data$rank.x / max(merged_data$rank.x)
  merged_data$rank.y <- merged_data$rank.y / max(merged_data$rank.y)
  
# Subtract the Epi from the CGF data. If result is >0 the similarity was stronger via CGF. If the result
# is <0, then the similarity was stronger via Epi relationships. 
  merged_data$minus <- merged_data$rank.x - merged_data$rank.y
  merged_data <- merged_data[,c(1:2,7)]
  
  
  caster <- acast(merged_data, formula = Var1 ~ Var2)
  return(data.matrix(caster))
}

######## Copy/Paste of CompareMatrix, but uses raw comparisons for same subtyping comparisons

CompareMatrixRaw <- function(cgf_data, epi_data) {
  # Import data from shiny 
  cgf<- cgf_data
  epi <- epi_data
  
  row.names(cgf) <- colnames(cgf)
  row.names(epi) <- colnames(epi)
  
  melt_cgf <- melt(as.matrix(cgf))
  melt_epi <- melt(as.matrix(epi))
  
  melt_cgf$value <- (melt_cgf$value / max(melt_cgf$value))
  melt_epi$value <- (melt_epi$value / max(melt_epi$value))
  merged_data <- merge(x=melt_cgf, y=melt_epi, by = c("Var1", "Var2"), all = T)
    
  # Subtract the Epi from the CGF data. If result is >0 the similarity was stronger via CGF. If the result
  # is <0, then the similarity was stronger via Epi relationships. 
  merged_data$minus <- merged_data$value.x - merged_data$value.y
  merged_data <- merged_data[,c(1:2,5)]
  
  
  caster <- acast(merged_data, formula = Var1 ~ Var2)
  return(data.matrix(caster))
}


CompareDisplay <- function(m, cgf_data, epi_data, type, sigma_in){
  
    clus_type <- switch(type, 
                          A = as.dendrogram(hclust(dist(data.matrix(cgf_data))), method = 'single'),
                          B = as.dendrogram(hclust(dist(data.matrix(epi_data))), method = 'single'), 
                          C = as.dendrogram(hclust(dist(m))), method = 'single')
    
    sigma <- sigma_in 
    pn_0 <- pnorm(sigma ,mean=0, sd=1) - pnorm(-sigma, mean=0, sd=1)
    
    pn_1 <- (1 - pn_0)/2 
    p_value <- pn_1
    
    pn_0 <- round(pn_0*1000, 0)
    pn_1 <- round(pn_1*1000, 0)
    
    
    dg <- colorRampPalette(c("darkgreen","honeydew"))(pn_1)
    wh <- colorRampPalette("white")(pn_0)
    db <- colorRampPalette(c("lightcyan", "darkblue"))(pn_1)
    
    sig1 <- c(dg, wh, db)

    if(any(m) != 0) {
      heatcolor <- sig1
      print('some differences present')
      d3heatmap(m, colors = heatcolor, Rowv= clus_type, symm=TRUE, Colv ="Rowv", revC=TRUE, 
                main = paste("P-Value for Outliers: ", round(p_value, 3)))
#       heatmap.2(m, col= heatcolor, Rowv= clus_type, symm=TRUE, Colv ="Rowv", trace='none',keysize=0.6, revC=TRUE, 
#                 main = paste("P-Value for Outliers: ", round(p_value, 3)))
          } else { 
            plot(1,1,col="white")
            text(1,1,"Error: No comparisons available, data is identical")
            print("no differences present")
#             heatcolor <- colorRampPalette(c("darkgreen","white","darkblue"))
#             heatmap.2(m, col= heatcolor, symm=TRUE, Colv ="Rowv", trace='none',keysize=0.6, revC=TRUE, breaks = c(-1, -0.01, 0.01, 1))
    }

    
}


CompareDisplay_pdf <- function(m, cgf_data, epi_data, type, sigma_in){
  
  clus_type <- switch(type, 
                      A = as.dendrogram(hclust(dist(data.matrix(cgf_data))), method = 'single'),
                      B = as.dendrogram(hclust(dist(data.matrix(epi_data))), method = 'single'), 
                      C = as.dendrogram(hclust(dist(m))), method = 'single')
  
  sigma <- sigma_in 
  pn_0 <- pnorm(sigma ,mean=0, sd=1) - pnorm(-sigma, mean=0, sd=1)
  
  pn_1 <- (1 - pn_0)/2 
  p_value <- pn_1
  
  pn_0 <- round(pn_0*1000, 0)
  pn_1 <- round(pn_1*1000, 0)
  
  
  dg <- colorRampPalette(c("darkgreen","honeydew"))(pn_1)
  wh <- colorRampPalette("white")(pn_0)
  db <- colorRampPalette(c("lightcyan", "darkblue"))(pn_1)
  
  sig1 <- c(dg, wh, db)
  
  if(any(m) != 0) {
    heatcolor <- sig1
    print('some differences present')
          heatmap.2(m, col= heatcolor, Rowv= clus_type, symm=TRUE, Colv ="Rowv", trace='none',keysize=0.6, revC=TRUE, 
                    main = paste("P-Value for Outliers: ", round(p_value, 3)))
  } else { 
    plot(1,1,col="white")
    text(1,1,"Error: No comparisons available, data is identical")
    print("no differences present")
    #             heatcolor <- colorRampPalette(c("darkgreen","white","darkblue"))
    #             heatmap.2(m, col= heatcolor, symm=TRUE, Colv ="Rowv", trace='none',keysize=0.6, revC=TRUE, breaks = c(-1, -0.01, 0.01, 1))
  }
  
  
}
