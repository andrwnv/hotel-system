# Hotel system

# Run
`cabal run`


# Build GTK3 on Windows
## Gcup install
`Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072;Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true`

`SET PATH=C:\ghcup\msys64\mingw64\bin;C:\ghcup\msys64\usr\bin;%PATH%`
`SET PKG_CONFIG_PATH=C:\ghcup\msys64\mingw64\lib\pkgconfig`
`SET XDG_DATA_DIRS=C:\ghcup\msys64\mingw64\share`

## Msys2
`pacman -S -q --noconfirm mingw64/mingw-w64-x86_64-pkg-config mingw64/mingw-w64-x86_64-gobject-introspection mingw64/mingw-w64-x86_64-gtksourceview3`
`cabal user-config init`
`cabal update`
`cabal install -j4 alex happy`
`cabal install --lib -j4 gi-gtk`
