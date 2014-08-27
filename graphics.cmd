setlocal
set bmp2tile="C:\Users\Maxim\Documents\Code\Delphi\BMP to tile\bmp2tile.exe"

%bmp2tile% graphics.png -removedupes -mirror -savetiles Tiles.pscompr  -exit
%bmp2tile% graphics.png -removedupes -mirror -savetiles Tiles.psgcompr  -exit
%bmp2tile% graphics.png -removedupes -mirror -savetiles Tiles.aPLib  -exit
%bmp2tile% graphics.png -removedupes -mirror -savetiles Tiles.bin  -exit
%bmp2tile% graphics.png -removedupes -mirror -savetiles Tiles.soniccompr  -exit
%bmp2tile% graphics.png -removedupes -mirror -savetiles Tiles.PuCrunch  -exit
%bmp2tile% graphics.png -removedupes -mirror -savetilemap Tilemaps.bin -palsms -savepalette "palette.inc" -exit
