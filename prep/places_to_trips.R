library(data.table)

place_file <- file.choose()
csv_path <- dirname(place_file)

place <- fread(place_file, colClasses = 'character')
place[, perno := as.numeric(perno)]
place[, placeno := as.numeric(placeno)]
# get place 1 information before collapsing places to trips
setDT(place, key = c('sampno','perno','placeno'))
place[, starttime := shift(deptime), by = list(sampno, perno)]
place[, actdur_origin := shift(actdur), by = list(sampno, perno)]
place[, locno_origin := shift(locno), by = list(sampno, perno)]
place[, tpurp_origin := shift(tpurp), by = list(sampno, perno)]
place[, tpurp_o_origin := shift(tpurp_o), by = list(sampno, perno)]
place[, tpurp2_origin := shift(tpurp2), by = list(sampno, perno)]
place[, tpurp2_o_origin := shift(tpurp2_o), by = list(sampno, perno)]

# collapse places into trips
trip <- place[placeno > 1, ]
setnames(trip, 'placeno', 'tripno')
trip$tripno <- as.character(as.integer(trip$tripno) - 1)

trip[, perno := as.character(perno)]
trip[, tripno := as.character(tripno)]

fwrite(trip, file.path(csv_path, 'trip.csv'))
