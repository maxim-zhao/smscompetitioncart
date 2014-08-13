setlocal
set bmp2tile="C:\Users\Maxim\Documents\Code\Delphi\BMP to tile\bmp2tile.exe"

%bmp2tile% graphics.png -removedupes -mirror -savetiles Tiles.psgcompr -savetiles Tiles.aPLib -savetiles Tiles.bin -savetiles Tiles.soniccompr -savetiles Tiles.PuCrunch -savetilemap Tilemaps.bin -palsms -savepalette "palette.inc" -exit
