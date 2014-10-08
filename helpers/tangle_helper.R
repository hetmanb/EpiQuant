tangle_helper <- function(gene_data, epi_data){
require(dendextend)

# epi <- read.table("../../../0 - Publications_bh/Amy_Paul_presentation/basedata/Epi_Table (CI_only_60_25_15).txt", sep = '\t', header = T)
# cgf <- read.table("../../../0 - Publications_bh/Amy_Paul_presentation/basedata/hexMLST-SimTable.txt", sep='\t', header = T)

epi <- epi_data
cgf <- gene_data

rownames(epi)<-colnames(epi)
rownames(cgf)<-colnames(cgf)

d_cgf<- as.dendrogram(hclust(dist(cgf)))
d_epi<- as.dendrogram(hclust(dist(epi)))

labels(d_cgf) <- as.character(labels(d_cgf))
labels(d_epi) <- as.character(labels(d_epi))



dendo_random <- untangle_random_search(d_cgf, d_epi, R = 200)
dend_heights <- heights_per_k.dendrogram(dendo_random[[2]])
dendo <- untangle_step_rotate_1side(dend1 = dendo_random[[2]], dend2_fixed = dendo_random[[1]], dendextend_heights_per_k.dendrogram= (dend_heights)[[1]], k_seq = 2:3 )
return(dendo)
}

tangle_plot<- function(d, k){
  
num_k <- k  
dendo <- d  
  
library(RColorBrewer)
dendo[[1]] <-  color_branches(dendo[[1]], num_k, col = brewer.pal(num_k, "Dark2"))
col_lines_left2 <- brewer.pal(num_k, "Dark2")[cutree(dendo[[1]], num_k, order_clusters_as_data = F, sort_cluster_numbers = T)]

tanglegram(dendo[[1]], dendo[[2]], 
           color_lines = col_lines_left2, 
           lab.cex = 0.75, 
           main_left = "Genetic Similarity", 
           main_right = "Epidemiological Similarity", 
           sub = paste("Entanglement: ", round(entanglement(dendo[[1]], dendo[[2]]), 2)),
           cex_sub = 0.9) 


}

