# Coordinated evolution between N2 neuraminidase and H1 and H3 hemagglutinin genes increased influenza A virus genetic diversity in swine
Graphics and analysis files for the analysis of the N2.2002 neuraminidase from 2009-2018 (swine) in the manuscript:

Zeller, M.A., Chang, J., Vincent, A.L., Gauger, P.C., and Anderson, T.K. Coordinated evolution between N2 neuraminidase and H1 and H3 hemagglutinin genes increased influenza A virus genetic diversity in swine. [*bioRxiv Preprint* doi:10.1101/2020.05.29.123828](https://www.biorxiv.org/content/10.1101/2020.05.29.123828v1).

Abstract: The neuraminidase (NA) and hemagglutinin (HA) of influenza A virus (IAV) are essential surface glycoproteins. In this study, the evolution of subtype N2 NA paired with H1 and H3 subtype HA in swine was evaluated to understand if genetic diversity of HA and NA were linked. Using time-scaled Bayesian phylodynamic analyses, the relationships of paired swine N2 with H1 or H3 from 2009 to 2018 were evaluated. These data demonstrated increased relative genetic diversity within the major N2 clades circulating in swine (N2.1998 between 2014-2017 and N2.2002 between 2010-2016). Relative genetic diversity of NA-HA pairs (e.g., N2.1998B/ H1.Delta1B) were correlated, suggesting inter-gene epistasis. Preferential pairing was observed among specific NA and HA genetic clades and this was associated with gene reassortment between cocirculating influenza A strains. Using the phylogenetic topology of inferred N2 trees, the expansion of genetic diversity in the NA gene was quantified and increases in diversity were observed subsequent to NA-HA reassortment events. The rate of evolution among NA-N2 clades and HA-H1 and HA-H3 clades were similar. The frequent regional movement of pigs and their influenza viruses is a possible explanation driving this pattern of drift, reassortment, and rapid evolution. Bayesian phylodynamic analyses demonstrated strong spatial patterns in N2 genetic diversity, and that frequent interstate movement of N2 clades homogenized diversity. The reassortment and evolution of NA and its influence on HA evolution may affect antigenic drift, impacting vaccine control programs and animal health.


## Measuring effective population size
Effective population size studies were run using BEAST 1.8.4 in duplicate. The seeds for the runs as such

 | Phylogenetic clade | BEAST XML location | Run1 | Run2 | 
 | ------------------ | ------------------ | ---- | ---- |
 | N2.1998 (full) | N2.1998_eps/n2_98_refined.aln.xml | 1540330733520 | 1540330926474 | 
 | N2.2002 (full) | N2.2002_eps/n2_02_refined2.aln.xml | 1542054165048 | 1542054280700 | 
 | N2.1998A | figures1_figures5_split_eps_plots/98A.aln.xml | 1546965014683 | 1546965036074 | 
 | N2.1998B | figures1_figures5_split_eps_plots/98B.aln.xml | 1546965075418 | 1547148808634 | 
 | N2.2002A | figures1_figures5_split_eps_plots/02A.aln.xml | 1547148566418 | 1546975888745 | 
 | N2.2002B | figures1_figures5_split_eps_plots/02B.aln.xml | 1546975916323 | 1546975934694 | 
 | H1.Delta2 | figures1_figures5_split_eps_plots/delta2.xml | 1542906315381 | 1542906092756 | 
 | N2.2002A (paired with H1.Delta1b) | figures1_figures5_split_eps_plots/2002A_with_delta1b.aln.xml | 1551826266183 | 1552491185349 | 
 | H1.Delta1b (paired with N2.2002A) | figures1_figures5_split_eps_plots/Delta1b.aln.xml | 1551826286917 | 1552491078775 | 





