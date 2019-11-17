
## `jotaR`: Visual aesthetic themes for ‘ggplot2’ used at <https://jota.info>

### Installation

``` r
library(devtools)
devtools::install_github("JOTAJornalismo/jotaR")
```

### Usage

``` r
library(jotaR)
library(tidyverse)
# current verison
packageVersion("jotaR")
## [1] '0.0.2'

jotaR::import_roboto()
```

#### Scatterplot

``` r
ggplot(Governismo, aes(x=D1, y=D2)) +
geom_point() +
geom_abline(aes(slope = -Beta1/Beta2, intercept = -Intercept/Beta2)) +
labs(x="IDEOLOGIA (D1)", y="POSICIONAMENTO ECONÔMICO (D2)",
title="Exemplo de Scatterplot",
subtitle="Apoio ao texto da Reforma: Threshold 80% a 20%",
caption="Fonte: https://jota.info") +
theme_jota()
```

<img src="inst/figs/README-unnamed-chunk-4-1.png" width="720" style="display: block; margin: auto;" />

#### With background colors

``` r
ggplot(Governismo, aes(x=D1, y=D2)) +
geom_point() +
geom_abline(aes(slope = -Beta1/Beta2, intercept = -Intercept/Beta2)) +
labs(x="IDEOLOGIA (D1)", y="POSICIONAMENTO ECONÔMICO (D2)",
title="Exemplo de Scatterplot",
subtitle="Apoio ao texto da Reforma: Threshold 80% a 20%",
caption="Fonte: https://jota.info") +
theme_jota(panel_col = "#F0F0F0")
```

<img src="inst/figs/README-unnamed-chunk-5-1.png" width="720" style="display: block; margin: auto;" />

#### Scatterplot (map of ideal points)

``` r
# Cut points 
mutate(Governismo, 
       Indice = ifelse(Prob_Votacao > 0.8, "Favorável",
                ifelse(Prob_Votacao < 0.2, "Desfavorável", "Neutro"))) %>%
ggplot() +
  geom_point(aes(x = D1, y = D2, color = Indice)) +
  geom_abline(aes(slope = -Beta1/Beta2, intercept = -Intercept/Beta2)) +
labs(x="IDEOLOGIA (D1)", y="POSICIONAMENTO ECONÔMICO (D2)",
     title="Exemplo de Scatterplot",
     subtitle="Apoio ao texto da Reforma: Threshold 80% a 20%",
     caption="Fonte: https://jota.info") +
   theme_jota() + 
  scale_color_idealpoints() +
   theme(legend.position = "top")
```

<img src="inst/figs/README-unnamed-chunk-6-1.png" width="720" style="display: block; margin: auto;" />

``` r
# Cut points 
mutate(Governismo, 
       Indice = ifelse(Prob_Votacao > 0.8, "Favorável",
                ifelse(Prob_Votacao < 0.2, "Desfavorável", "Neutro"))) %>%
ggplot() +
  geom_point(aes(x = D1, y = D2, color = Indice)) +
  geom_hline(yintercept = 0, size = .75, colour="#cbcbcb") +
  geom_vline(xintercept = 0, size = .75, colour="#cbcbcb") +
  geom_abline(aes(slope = -Beta1/Beta2, intercept = -Intercept/Beta2)) +
labs(x="IDEOLOGIA (D1)", y="POSICIONAMENTO ECONÔMICO (D2)",
     title="Exemplo de Scatterplot",
     subtitle="Apoio ao texto da Reforma: Threshold 80% a 20%",
     caption="Fonte: https://jota.info") +
  theme_jota(grid = FALSE) + 
  scale_color_idealpoints() +
   theme(legend.position = "top")
```

<img src="inst/figs/README-unnamed-chunk-7-1.png" width="720" style="display: block; margin: auto;" />

#### Bar plot

``` r
group_by(Governismo, Reeleito) %>%
  summarize(Indice = mean(Indice, na.rm=TRUE)) %>%
  mutate(Reeleito = ifelse(Reeleito==1, "Reeleito", "Novato")) %>%
  ggplot(aes(x=Reeleito, y=Indice)) +
  geom_col() +
  geom_hline(yintercept = 0, size = 1, colour="#666000") +
  geom_text(aes(label=round(Indice,3)), nudge_y=.04) +
  labs(x="", y="ÍNDICE DE GOVERNISMO",
       title="Exemplo de Gráfico de Barras",
       subtitle="Novatos compõem a principal força de apoio ao governo",
       caption="Fonte: https://jota.info") +
  theme_jota(grid="Y") +
  theme(axis.text.y=element_blank(), legend.position = "none")
```

<img src="inst/figs/README-unnamed-chunk-8-1.png" width="720" style="display: block; margin: auto;" />

``` r
group_by(Governismo, Reeleito) %>%
  summarize(Indice = mean(Indice, na.rm=TRUE)) %>%
  mutate(Reeleito = ifelse(Reeleito==1, "Reeleito", "Novato")) %>%
  ggplot(aes(x=Reeleito, y=Indice, fill=Reeleito)) +
  geom_col() +
  geom_hline(yintercept = 0, size = 1, colour="#666000") +
  geom_text(aes(label=round(Indice,3)), nudge_y=.04) +
  labs(x="", y="ÍNDICE DE GOVERNISMO",
       title="Exemplo de Gráfico de Barras",
       subtitle="Novatos compõem a principal força de apoio ao governo",
       caption="Fonte: https://jota.info") +
  theme_jota(grid="Y") +
  theme(axis.text.y=element_blank(), legend.position = "none") +
   scale_fill_jota()
```

<img src="inst/figs/README-unnamed-chunk-9-1.png" width="720" style="display: block; margin: auto;" />

#### Using with logos

``` r


finalize_ggplot(plot_name = pl,
 source = "Pesquisa Parlamentar realizada com 200 deputados entre os dias 27 e 30 de outubro.",
 width_pixels = 640,
 height_pixels = 450)
```

<img src="inst/figs/README-unnamed-chunk-11-1.png" width="912" style="display: block; margin: auto;" />

### jotaR Metrics
