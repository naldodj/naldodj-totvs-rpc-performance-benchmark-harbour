@setlocal
    call "%ProgramFiles%\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvarsall.bat" amd64
    F:\harbour_msvc\bin\win\msvc64\hbmk2 hb_inetserver.hbp -comp=msvc64
    if EXIST .\hb_inetserver.exe (
        rem cmd /c f:\upx\upx.exe .\hb_inetserver.exe
        certutil -hashfile .\hb_inetserver.exe SHA256 > hb_inetserver.sh256
    )
    F:\harbour_msvc\bin\win\msvc64\hbmk2 hb_socketserver.hbp -comp=msvc64
    if EXIST .\hb_socketserver.exe (
        rem cmd /c f:\upx\upx.exe .\hb_socketserver.exe
        certutil -hashfile .\hb_socketserver.exe SHA256 > hb_socketserver.sh256
    )
    F:\harbour_msvc\bin\win\msvc64\hbmk2 u_rpctest.hbp -comp=msvc64
    if EXIST .\u_rpctest.exe (
        rem cmd /c f:\upx\upx.exe .\hb_inetserver.exe
        certutil -hashfile .\u_rpctest.exe SHA256 > u_rpctest.sh256
    )
@endlocal
