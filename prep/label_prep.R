library(data.table)

data_folder <- 'C:\\Users\\cates_a\\Desktop\\2018-19 My Daily Travel Survey, Data and Documentation\\data'

value_labels <- fread(file.path(data_folder, 'value.label.csv'), encoding = 'UTF-8')
variable_labels <- fread(file.path(data_folder, 'variable.label.csv'), encoding = 'UTF-8')

value_labels[, NAME := tolower(NAME)]
value_labels[, TABLE := tolower(TABLE)]

variable_labels[, NAME := tolower(NAME)]
variable_labels[, TABLE := tolower(TABLE)]
variable_labels[TYPE == 'TEXT', TYPE := 'character']
variable_labels[, TYPE := tolower(TYPE)]

variable_labels <- unique(rbind(variable_labels,
  variable_labels[NAME == 'tpurp' & TABLE == 'place', list(NAME = 'tpurp_origin', TABLE, TYPE, LABEL = paste(LABEL, 'at origin'))],
  variable_labels[NAME == 'tpurp' & TABLE == 'place', list(NAME = 'tpurp_o_origin', TABLE, TYPE, LABEL = paste(LABEL, 'at origin'))],
  variable_labels[NAME == 'tpurp' & TABLE == 'place', list(NAME = 'tpurp2_origin', TABLE, TYPE, LABEL = paste(LABEL, 'at destination'))],
  variable_labels[NAME == 'tpurp' & TABLE == 'place', list(NAME = 'tpurp2_o_origin', TABLE, TYPE, LABEL = paste(LABEL, 'at destination'))],
  variable_labels[NAME == 'actdur' & TABLE == 'place', list(NAME = 'actdur_origin', TABLE, TYPE, LABEL)],
  data.table(NAME = 'starttime', TABLE = 'place', TYPE = 'numeric', LABEL = 'Start time (ISO 8601 Date and Time)')
))

value_labels <- unique(rbind(value_labels,
  value_labels[NAME == 'tpurp' & TABLE == 'place', list(NAME = 'tpurp_origin', TABLE, VALUE, LABEL)],
  value_labels[NAME == 'tpurp' & TABLE == 'place', list(NAME = 'tpurp_o_origin', TABLE, VALUE, LABEL)],
  value_labels[NAME == 'tpurp' & TABLE == 'place', list(NAME = 'tpurp2_origin', TABLE, VALUE, LABEL)],
  value_labels[NAME == 'tpurp' & TABLE == 'place', list(NAME = 'tpurp2_o_origin', TABLE, VALUE, LABEL)]
))

variable_labels[TABLE == 'place', TABLE := 'trip']
value_labels[TABLE == 'place', TABLE := 'trip']

variable_labels <- variable_labels[TABLE %in% c('household','person','trip','trip','tour')]
value_labels <- value_labels[TABLE %in% c('household','person','trip','trip','tour')]

# setorder(variable_labels, NAME)
# setorder(value_labels, NAME, VALUE)

fwrite(variable_labels, normalizePath(file.path(data_folder, 'variable_labels.csv'), mustWork = FALSE))
fwrite(value_labels, normalizePath(file.path(data_folder, 'value_labels.csv'), mustWork = FALSE))
