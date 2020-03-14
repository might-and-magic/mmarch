copy test_original.txt test.txt

%~dp0..\mmarch create test.lod mmiconslod . test.txt test.bin

copy /Y test_new.txt test.txt

%~dp0..\mmarch add test.lod test.txt test_new.txt

%~dp0..\mmarch list test.lod

%~dp0..\mmarch list test.lod "|"

%~dp0..\mmarch extract test.lod test_resource

%~dp0..\mmarch rename test.lod test_new.txt test_new2.txt

%~dp0..\mmarch create test2.mm7 mm78save . test.bat

%~dp0..\mmarch merge test.lod test2.mm7

%~dp0..\mmarch delete test.lod test.bin test_new2.txt test.bat

%~dp0..\mmarch optimize test.lod

rmdir /s/q test_resource

del test.lod test2.mm7 test.txt

REM mmarch add sprites.lod leafs03.bmp

mmarch create test.sprites.lod mmspriteslod . leafs03.bmp -p 995

mmarch create test.sprites.lod mmspriteslod .

mmarch create test.bitmaps.lod mmbitmapslod . pal995.act


REM all format test

mmarch c h3lod.lod h3lod .
mmarch c h3snd.snd h3snd .
mmarch c mmsnd.snd mmsnd .
mmarch c h3mm78vid.vid h3mm78vid .
mmarch c mm6vid.vid mm6vid .
mmarch c mmbitmapslod.bitmaps.lod mmbitmapslod .
mmarch c mmiconslod.icons.lod mmiconslod .
mmarch c mmspriteslod.sprites.lod mmspriteslod .
mmarch c mm8loclod.T.lod mm8loclod .
mmarch c mm78gameslod.games.lod mm78gameslod .
mmarch c mm6gameslod.games.lod mm6gameslod .
mmarch c mm78save.lod mm78save .
mmarch c mm6save.mm6 mm6save .


mmarch add test.sprites.lod a.bmp -p 994
