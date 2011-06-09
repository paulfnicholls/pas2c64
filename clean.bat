pushd "%~dp0"
del *.~* *.ddp *.dcu *.map *.o *.ppu /s
del executable\*.exe
popd
