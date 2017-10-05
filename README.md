#Small Cell Suppression

When reporting on survey data, it is important to avoid inadvertantly identifying individuals. The motivation behind this effort is to create a series of functions that allow the user to look for and mask small cell size.

`replace_loop` takes a dataframe, grouping variables, and minimum cell size, it returns a data frame that, when collapsed on the grouping variables, every cell will have an n > than the specified minimum cell size.

Currently, `replace_loop` only replaces the values within a single grouping variable.