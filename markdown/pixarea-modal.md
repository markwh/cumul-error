This plot shows the total area for the selected nodes, as it is composed of the underlying pixels. 

The solid portion of the plot is comprised of thin vertical bars. Each vertical bar is a pixel belonging to one of the selected nodes. The width of the bar is the pixel area. The height of the bar is its effective water fraction--that is, the water fraction used by the pixel->node aggregation method. For the "simple" method, the effective water fraction is always 1. For "water fraction" it is always the estimated water fraction. For "composite" it is the estimated water fraction except for interior water pixels, for which it is 1.

The dashed-line box shows the GDEM truth area for selected nodes. 

If width estimates agree with GDEM truth, then the total area of the vertical bars will equal the area of the dashed-line box. (Note that width is directly proportional to area, so the comparison to truth is identical for area and width.)

