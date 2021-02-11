$beebasm_dir = "build\beebasm"
$beebasm = "$beebasm_dir\beebasm.exe"
$ssd_out = "butil.ssd"
$title = "Backup util"
$srcs = ("butil.bas", "bu_lib.asm", "!boot")
$asm_src = "bu_lib.asm"

[Security.Principal.WindowsBuiltInRole] $adminrole = "Administrator"
$isadmin = ([Security.Principal.WindowsPrincipal]`
     [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole($adminrole)

if (!(Test-Path $beebasm)) {
    echo "Getting beebasm"
    git submodule update --init
}

[DateTime]$ssd_out_stamp = if (!(Test-Path $ssd_out)) {0} else {(Get-Item $ssd_out).LastWriteTime}

$newer = $srcs.Where({Test-Path $_ -NewerThan $ssd_out_stamp}).Count

if ($newer -gt 0) {
    echo "Checking for Microsoft Visual C++ 2010 redistibutable"
    $vccount=(Get-WmiObject -Class Win32_Product -Filter "Name LIKE '%Visual C++ 2010%'").Count
    if ($vccount -eq 0) {
        echo "Downloading Microsoft Visual C++ 2010 redistibutable"
        $tempdir=[IO.Path]::GetTempPath()
        $vcredist_src="http://download.microsoft.com/download/1/6/5/165255E7-1014-4D0A-B094-B6A430A6BFFC/vcredist_x86.exe"
        $vcredist_dst=Join-Path $tempdir "vcredist_x86_2010_sp1.exe"
        Invoke-WebRequest $vcredist_src -Out $vcredist_dst
        echo "Invoking Microsoft Visual C++ 2010 redistributable installer"
        Start-Process $vcredist_dst -Wait
        if (-Not $?) { break }
        Remove-Item -Recurse $vcredist_dst
    }

    echo "Building '$ssd_out'"
    & "$beebasm" -i $asm_src -do $ssd_out -opt 3 -title "$title"
} else {
    echo "File '$ssd_out' is up to date."
}
