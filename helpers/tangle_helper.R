tangle_helper <- function(gene_data, epi_data){
require(dendextend)

# epi <- read.table("../../../0 - Publications_bh/Amy_Paul_presentation/basedata/Epi_Table (CI_only_60_25_15).txt", sep = '\t', header = T)
# cgf <- read.table("../../../0 - Publications_bh/Amy_Paul_presentation/basedata/hexMLST-SimTable.txt", sep='\t', header = T)

epi <- epi_data
cgf <- gene_data

rownames(epi)<-colnames(epi)
rownames(cgf)<-colnames(cgf)

#### Compute trees for calculating wallace coefficients
w_cgf <- (hclust(dist(cgf)))
w_epi <- (hclust(dist(epi)))

wmax_cgf <- max(w_cgf$height)
wmax_epi <- max(w_epi$height)

# cut_cgf <- cutree(w_cgf, k = 100)
# cut_epi <- cutree(w_epi, k = 100)

cut_cgf <- cutree(w_cgf, h = .9*wmax_cgf)
cut_epi <- cutree(w_epi, h = .9*wmax_epi)

wallace_list <- list(adj_wallace(cut_cgf, cut_epi))

#### 
d_cgf<- as.dendrogram(hclust(dist(cgf)))
d_epi<- as.dendrogram(hclust(dist(epi)))

labels(d_cgf) <- as.character(labels(d_cgf))
labels(d_epi) <- as.character(labels(d_epi))



dendo_random <- untangle_random_search(d_cgf, d_epi, R = 25)
dend_heights <- heights_per_k.dendrogram(dendo_random[[2]])
dendo <- untangle_step_rotate_1side(dend1 = dendo_random[[2]], dend2_fixed = dendo_random[[1]], dendextend_heights_per_k.dendrogram= (dend_heights)[[1]], k_seq = 2:4 )
return(list(dendo, wallace_list))
}

tangle_plot<- function(d, k){
  
num_k <- k  
dendo <- d[[1]] 
wallace_list <- d[[2]]
wallace_list <- wallace_list[[1]]



library(RColorBrewer)
dendo[[1]] <-  color_branches(dendo[[1]], num_k, col = brewer.pal(num_k, "Dark2"))
col_lines_left2 <- brewer.pal(num_k, "Dark2")[cutree(dendo[[1]], num_k, order_clusters_as_data = F, sort_cluster_numbers = T)]

tanglegram(dendo[[1]], dendo[[2]], margin_inner = 5,
           color_lines = col_lines_left2, 
           lab.cex = 0.75, 
           main_right = "Epidemiological Similarity", 
           main_left = "Genetic Similarity", 
#            sub = paste("Entanglement: ", round(entanglement(dendo[[1]], dendo[[2]]), 2)),
           sub = paste("Adj Wallace A vs. B:", round(wallace_list$Adjusted_Wallace_A_vs_B, 3), "\n", 
                       "Adj Wallace B vs. A:", round(wallace_list$Adjusted_Wallace_B_vs_A, 3), "\n", 
                       "Entanglement:", round(entanglement(dendo[[1]], dendo[[2]]), 2)),
           cex_sub = 0.9)

}

