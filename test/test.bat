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
