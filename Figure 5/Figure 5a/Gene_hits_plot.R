library(ggplot2)
library(ggrepel)

gene_hits1 = read.table("gene_hits_kmer.txt", stringsAsFactors=FALSE, header=TRUE)

ggplot(gene_hits1, aes(x=avg_beta, y=maxp, colour=avg_maf, size=hits, label=gene)) +
  geom_point(alpha=0.5) +
  geom_text_repel(aes(size=18), show.legend = FALSE, colour='black', box.padding = 0.05,segment.size=.02, point.padding = .05, max.overlaps = 100) +
  scale_size("Number of k-mers", range=c(1,10)) +
  scale_colour_gradient('Average MAF') +
  theme_bw(base_size=14) +
  ggtitle("") +
  xlab("Average effect size") +
  ylab("Maximum -log10(p-value)")+
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),    # Reduce legend text size
        legend.title = element_text(size = 8),  # Reduce legend title size
        legend.key.size = unit(0.2, "cm"))+
  guides(   # Reduce circle size in the color legend
    shape = guide_legend(override.aes = list(size = .5), keyheight = unit(0.2, "cm"), keywidth = unit(0.05, "cm")))


ggsave("Gene_hits.pdf", width = 6, height = 4, units = "in", dpi = 700)