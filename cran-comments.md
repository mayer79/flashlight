# Emergency release

ggplot2 3.3.0 seems to have changed the way how geom_bar is using ymin and ymax aesthetic. This has broken plots for light_importance and light_interaction.

Example that shows how ggplot reacts now with 3.3.0:

```
library(ggplot2)
library(dplyr)

counts <- iris %>% 
  count(Species)

# Unexpected
ggplot(counts, aes(x = Species, y = n, ymin = 0, ymax = 100)) +
  geom_bar(stat="identity")

# Okay
ggplot(counts, aes(x = Species, y = n)) +
  geom_bar(stat="identity")
```

In earlier releases of ggplot2, the problem did not appear as geom_bar has not used ymin and ymax aesthetics.
