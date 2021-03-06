tangle_helper <- function(gene_data, epi_data, cut_epi, cut_cgf){
require(dendextend)
epi <- epi_data
cgf <- gene_data

cut_e <- cut_epi
cut_g <- cut_cgf


rownames(epi)<-colnames(epi)
rownames(cgf)<-colnames(cgf)

#### Compute trees for calculating wallace coefficients
w_cgf <- (hclust(dist(cgf)))
w_epi <- (hclust(dist(epi)))
m_cgf <- w_cgf
m_epi <- w_epi


wmax_cgf <- max(w_cgf$height)
wmax_epi <- max(w_epi$height)

print(max(w_cgf$height))
print(max(w_epi$height))


w_cgf$height <- (w_cgf$height / wmax_cgf)
w_epi$height <- (w_epi$height / wmax_epi)


print(max(w_cgf$height))
print(max(w_epi$height))

# cut_cgf <- cutree(w_cgf, k = 100)
# cut_epi <- cutree(w_epi, k = 100)

cut_cgf <- cutree(m_cgf, h = cut_g*wmax_cgf)
cut_epi <- cutree(m_epi, h = cut_e*wmax_epi)

wallace_list <- list(adj_wallace(cut_cgf, cut_epi))

#### 
d_cgf<- as.dendrogram(w_cgf)
d_epi<- as.dendrogram(w_epi)


# d_cgf<- cut(d_cgf, h = (1-cut_g)*attr(d_cgf,"height"))
# d_epi<- cut(d_epi, h = (1-cut_g)*attr(d_epi,"height"))

labels(d_cgf) <- as.character(labels(d_cgf))#[[1]]
labels(d_epi) <- as.character(labels(d_epi))#[[1]]



dendo_random <- untangle_random_search(d_cgf, d_epi, R = 100)
dend_heights <- heights_per_k.dendrogram(dendo_random[[1]])
dendo <- untangle_step_rotate_1side(dend1 = dendo_random[[1]], dend2_fixed = dendo_random[[2]], dendextend_heights_per_k.dendrogram= (dend_heights)[[1]], k_seq = 2:4 )
return(list(dendo, wallace_list))
}

tangle_plot<- function(d, k){
  
num_k <- k  
dendo <- d[[1]] 
wallace_list <- d[[2]]
wallace_list <- wallace_list[[1]]



library(RColorBrewer)
dendo[[1]] <-  color_branches(dendo[[1]], num_k, col = brewer.pal(num_k, "Dark2"))
col_lines_left2 <- brewer.pal(num_k, "Dark2")[cutree(dendo[[1]], num_k, order_clusters_as_data = F)] #, sort_cluster_numbers = T

tanglegram(dendo[[1]], dendo[[2]], margin_inner = 5,
           color_lines = col_lines_left2, 
           lab.cex = 0.75, 
           main_left = "Epidemiological Tree", 
           main_right = "Genetic Tree", 
           sub = paste("Adj Wallace A vs. B:", round(wallace_list$Adjusted_Wallace_A_vs_B, 3), "\n", 
                       "Adj Wallace B vs. A:", round(wallace_list$Adjusted_Wallace_B_vs_A, 3), "\n", 
                       "Entanglement:", round(entanglement(dendo[[1]], dendo[[2]]), 2), "\n"),
           cex_sub = 0.9)

}




tangle_plot2<- function(d, k){
  
  num_k <- k  
  dendo <- d[[1]] 
  wallace_list <- d[[2]]
  wallace_list <- wallace_list[[1]]
  
  
  
  library(RColorBrewer)
  dendo[[1]] <-  color_branches(dendo[[1]], num_k, col = brewer.pal(num_k, "Set2"))
  col_lines_left2 <- brewer.pal(num_k, "Set2")[cutree(dendo[[1]], num_k, order_clusters_as_data = F)] #,sort_cluster_numbers = T
  
  tanglegram(dendo[[1]], dendo[[2]], margin_inner = 5,
             color_lines = col_lines_left2, 
             lab.cex = 0.75, 
             main_left = "Source-Clustering", 
             main_right = "Temporal-Clustering", 
             sub = paste("Adj Wallace A vs. B:", round(wallace_list$Adjusted_Wallace_A_vs_B, 3), "\n", 
                         "Adj Wallace B vs. A:", round(wallace_list$Adjusted_Wallace_B_vs_A, 3), "\n", 
                         "Entanglement:", round(entanglement(dendo[[1]], dendo[[2]]), 2), "\n"),
                          cex_sub = 0.9)
  
}








