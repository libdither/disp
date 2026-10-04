# A DEF pin is metal: draw metal (datatype 0) under every pin shape (datatype 2) of the top cell, so
# pin labels attach to their nets in LVS and pins meet IHP's enclosure rule (Pin.f).
# klayout -b -r pins_to_metal.py -rd src=in.gds -rd dst=out.gds
import klayout.db as db
ly = db.Layout(); ly.read(src)
top = ly.top_cell()
for li in list(ly.layer_indexes()):
    info = ly.get_info(li)
    if info.datatype != 2: continue
    region = db.Region(top.shapes(li))
    if not region.is_empty(): top.shapes(ly.layer(info.layer, 0)).insert(region)
ly.write(dst)
