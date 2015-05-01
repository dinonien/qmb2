align.plots2 <- function (..., vertical = TRUE, pos = NULL) 
{
        dots <- list(...)
        if (is.null(pos)) pos <- lapply(seq(dots), I)
        dots <- lapply(dots, ggplotGrob)
        ytitles <- lapply(dots, function(.g) editGrob(getGrob(.g, 
                                                              "axis.title.y.text", grep = TRUE), vp = NULL))
        ylabels <- lapply(dots, function(.g) editGrob(getGrob(.g, 
                                                              "axis.text.y.text", grep = TRUE), vp = NULL))
        legends <- lapply(dots, function(.g) if (!is.null(.g$children$legends)) 
                editGrob(.g$children$legends, vp = NULL)
                else ggplot2:::.zeroGrob)
        gl <- grid.layout(nrow = do.call(max,pos))
        vp <- viewport(layout = gl)
        pushViewport(vp)
        widths.left <- mapply(`+`, e1 = lapply(ytitles, grobWidth), 
                              e2 = lapply(ylabels, grobWidth), SIMPLIFY = F)
        widths.right <- lapply(legends, function(g) grobWidth(g) + 
                                       if (is.zero(g)) 
                                               unit(0, "lines")
                               else unit(0.5, "lines"))
        widths.left.max <- max(do.call(unit.c, widths.left))
        widths.right.max <- max(do.call(unit.c, widths.right))
        for (ii in seq_along(dots)) {
                pushViewport(viewport(layout.pos.row = pos[[ii]]))
                pushViewport(viewport(x = unit(0, "npc") + widths.left.max - 
                                              widths.left[[ii]], width = unit(1, "npc") - widths.left.max + 
                                              widths.left[[ii]] - widths.right.max + widths.right[[ii]], 
                                      just = "left"))
                grid.draw(dots[[ii]])
                upViewport(2)
        }
}