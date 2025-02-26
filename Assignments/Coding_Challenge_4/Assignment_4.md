[Link to Published
Manuscript](https://doi.org/10.1094/PDIS-06-21-1253-RE)

[Link to my Github](https://github.com/Logz1n/Reproducibility_Class)

# Question 1

``` r
  #a) YAML Headers are the output files at the top that has the "title, author, date and output".
  #b) Literate programming is making it both readable for humans and readable for the computer. By including the YAML header export it makes it easier for people to understand exactly what your code does!
```

# Loading libraries and color pallettes

``` r
#Loading Libraries
library(ggplot2) 
library(knitr)
library(readr)
library(ggpubr)

#Colorblind pallette
cbbPalette <- c("#56B4E9", "#009E73", "#F0E442",
"#000000", "#D55E00", "#CC79A7", "#E69F00","#0072B2" ) #loading a color pallette
```

# Reading in Data

``` r
MycotoxinData <- read.csv("MycotoxinData.csv", na = "na" ) #loading in the data so that R understands na is na so the column is numeric

MycotoxinData$Cultivar <- as.factor(MycotoxinData$Cultivar)#since we are going to facet wrap it makes life easier to make the column a facter instead of a character.

MycotoxinData$Treatment <- as.factor(MycotoxinData$Treatment) #same as above
```

# Creating Figures

## DON vs Treatment Plot

``` r
DON5a_Plot <- ggplot(MycotoxinData, aes(x = Treatment, y = DON, group = Treatment, fill = Cultivar)) + 
  # Create separate panels for each level of the 'Cultivar' variable
  facet_wrap(~Cultivar) +
  # Add a boxplot layer without outlier points
  geom_boxplot(outlier.shape = NA) +
  # Add jittered points to the plot for better visibility of individual data points
  geom_point(position = position_jitterdodge(dodge.width = 0.9), shape = 21, alpha = 0.6) +
  # Add pairwise comparison results using t-test
  geom_pwc(method = "t.test", label = "{p.adj.format}.{p.adj.signif}") +
  # Label the y-axis
  ylab("DON (ppm)") +
  # Label the x-axis (left blank in this case)
  xlab(" ") +
  # Manually set the fill colors using a predefined palette
  scale_fill_manual(values = cbbPalette, aesthetics = "fill") +
  # Apply a classic theme to the plot
  theme_classic()

DON5a_Plot
```

![](Assignment_4_files/figure-gfm/DON%20vs%20Treatment%20Plot-1.png)<!-- -->

## x15ADON vs Treatment Plot

``` r
DON5b_Plot <- ggplot(MycotoxinData, aes(x = Treatment, y = X15ADON, group = Treatment, fill = Cultivar)) + 
  # Add a boxplot layer without outlier points
  geom_boxplot(outlier.shape = NA) +
  # Add jittered points to the plot for better visibility of individual data points
  geom_point(position = position_jitterdodge(dodge.width = 0.9), shape = 21, alpha = 0.6) +
  # Add pairwise comparison results using t-test
  geom_pwc(method = "t.test", label = "{p.adj.format}.{p.adj.signif}") +
  # Label the y-axis
  ylab("15ADON") +
  # Label the x-axis (left blank in this case)
  xlab(" ") +
  # Manually set the fill colors using a predefined palette
  scale_fill_manual(values = cbbPalette, aesthetics = "fill") +
  # Apply a classic theme to the plot
  theme_classic() +
  # Create separate panels for each level of the 'Cultivar' variable
  facet_wrap(~Cultivar)

DON5b_Plot
```

![](Assignment_4_files/figure-gfm/x15ADON%20vs%20Treatment%20Plot-1.png)<!-- -->

## Mass per Seed vs Treatment

``` r
DON5c_Plot <- ggplot(MycotoxinData, aes(x = Treatment, y = MassperSeed_mg, group = Treatment, fill = Cultivar)) + 
  # Add a boxplot layer without outlier points
  geom_boxplot(outlier.shape = NA) +
  # Add jittered points to the plot for better visibility of individual data points
  geom_point(position = position_jitterdodge(dodge.width = 0.9), shape = 21, alpha = 0.6) +
  # Add pairwise comparison results using t-test
  geom_pwc(method = "t.test", label = "{p.adj.format}.{p.adj.signif}") +
  # Label the y-axis
  ylab("Seed Mass (mg)") +
  # Label the x-axis (left blank in this case)
  xlab(" ") +
  # Manually set the fill colors using a predefined palette
  scale_fill_manual(values = cbbPalette, aesthetics = "fill") +
  # Apply a classic theme to the plot
  theme_classic() +
  # Create separate panels for each level of the 'Cultivar' variable
  facet_wrap(~Cultivar)

DON5c_Plot
```

![](Assignment_4_files/figure-gfm/Mass%20per%20Seed%20vs%20Treatment%20Plot-1.png)<!-- -->

## Combining the Above Plots

``` r
CombinedPlot1 <- ggpubr::ggarrange(DON5a_Plot,
                                       DON5b_Plot,
                                       DON5c_Plot,
                                       labels = "AUTO",
                                       nrow = 3, ncol = 1, common.legend = T)

CombinedPlot1
```

![](Assignment_4_files/figure-gfm/Combined%20Plot-1.png)<!-- -->

``` r
#Common legend makes it so theres only 1 legend for all of the figures; if it is set to F, each figure has their own respective legend. Arranging the plots so DON1 is A, DON3a is B. AUTO in capital yields ABC, where auto yields abc (lowercase) 3 rows, 1 column.
```

\`\`\`
