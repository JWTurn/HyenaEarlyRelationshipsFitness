
## Early life relationships matter: Social position during early life predicts fitness among female spotted hyenas

[![DOI](https://zenodo.org/badge/268907300.svg)](https://zenodo.org/badge/latestdoi/268907300)

  - Authors:
      - [Julie W. Turner](https://www.julwturner.com)
      - [Alec L. Robitaille](http://robitalec.ca)
      - Patrick S. Bills
      - [Kay E. Holekamp](https://www.holekamplab.org)

This repository contains the data and code accompanying the paper “Early
life relationships matter: Social position during early life predicts
fitness among female spotted hyenas”.

## Abstract

1.  How social development in early life affects fitness remains poorly
    understood.

2.  Though there is growing evidence that early-life relationships can
    affect fitness, little research has investigated how social
    positions develop or whether there are particularly important
    periods for social position development in an animal’s life history.
    In long-lived species in particular, understanding the lasting
    consequences of early-life social environments requires detailed,
    long-term datasets.

3.  Here we used a 25-year dataset to test whether social positions held
    during early development predicted adult fitness. Specifically, we
    quantified social position using three social network metrics:
    degree, strength, and betweenness. We determined the social position
    of each individual in three types of networks during each of three
    stages of ontogeny to test whether they predict annual reproductive
    success (ARS) or longevity among adult female spotted hyenas
    (*Crocuta crocuta*).

4.  The social positions occupied by juvenile hyenas did predict their
    fitness, but the effects of social position on fitness measures
    differed between stages of early development. Network metrics when
    individuals were young adults better predicted ARS, but network
    metrics for younger animals, particularly when youngsters were
    confined to the communal den, better predicted longevity than did
    metrics assessed during other stages of development.

5.  Our study shows how multiple types of social bonds formed during
    multiple stages of social development predict lifetime fitness
    outcomes. We suggest that social bonds formed during specific phases
    of development may be more important than others when considering
    fitness outcomes.

## Data

Data included are the calculated social network metrics, randomizations,
and other variables used in the models in scripts 7-8. Descriptions of
the variables in the data for analysis are
below.

| variable           | description                                                                                           |
| ------------------ | ----------------------------------------------------------------------------------------------------- |
| ego                | focal female for analyses                                                                             |
| period             | cd (communal den)                                                                                     |
| ego\_period\_rank  | ego’s rank during that period                                                                         |
| mom                | ego’s mother                                                                                          |
| clan\_size         | number of individuals in the clan during stated period for that ego                                   |
| nSession           | number of sessions the ego was observed in during that period                                         |
| alone              | alone rate                                                                                            |
| sri\_degree        | degree centrality of the ego during that period based on the association network (simple ratio index) |
| sri\_strength      | stength of the ego during that period based on the association network (simple ratio index )          |
| sri\_betweenness   | betweenness of the ego during that period based on the association network (simple ratio index)       |
| aggr\_outdegree    | out-degree centrality of the ego during that period based on the aggression network                   |
| aggr\_indegree     | in-degree centrality of the ego during that period based on the aggression network                    |
| aggr\_outstrength  | out-stength of the ego during that period based on the aggression network                             |
| aggr\_instrength   | in-stength of the ego during that period based on the aggression network                              |
| aggr\_betweenness  | betweenness of the ego during that period based on the aggression network                             |
| affil\_outdegree   | out-degree centrality of the ego during that period based on the affiliation network                  |
| affil\_indegree    | in-degree centrality of the ego during that period based on the affiliation network                   |
| affil\_outstrength | out-stength of the ego during that period based on the affiliation network                            |
| affil\_instrength  | in-stength of the ego during that period based on the affiliation network                             |
| affil\_betweenness | betweenness of the ego during that period based on the affiliation network                            |
| longevity\_years   | age in years when the ego died                                                                        |
| annual\_rs         | annual reproductive success (ARS) for the ego                                                         |
| iteration          | iteration of the randomizations                                                                       |
| observed           | TRUE - the data comes from observations; FALSE - the data comes from randomizations                   |
