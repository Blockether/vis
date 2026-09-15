# Run in a fresh PowerShell process: mocks never enter the packaging/signing process.
$ErrorActionPreference = 'Stop'
. "$PSScriptRoot/windows-sign.ps1"

$script:checks = 0
$script:toolExitCode = 0
$script:signature = $null

function Get-AuthenticodeSignature {
    param($LiteralPath)
    return $script:signature
}

function Get-ChildItem {
    return [pscustomobject]@{ FullName = 'Invoke-TestSignTool' }
}

function Invoke-TestSignTool {
    if (($args -join '|') -cne 'verify|/pa|/all|/tw|test app.exe') {
        throw "Unexpected signtool arguments: $args"
    }
    $global:LASTEXITCODE = $script:toolExitCode
}

function Assert-Rejected {
    param([scriptblock]$Action, [string]$Message)
    try { & $Action } catch {
        if ($_.Exception.Message -notlike "*$Message*") { throw }
        $script:checks++
        return
    }
    throw "Expected rejection: $Message"
}

function New-TestSignature {
    return [pscustomobject]@{
        Status = 'Valid'
        SignerCertificate = [pscustomobject]@{ Subject = 'CN=Example publisher' }
        TimeStamperCertificate = [pscustomobject]@{ Subject = 'CN=Example timestamp' }
    }
}

$script:signature = New-TestSignature
Assert-VisSignature 'test app.exe' 'CN=Example publisher'
$script:checks++
Assert-Rejected { Assert-VisSignature 'test app.exe' '' } 'WINDOWS_SIGNING_PUBLISHER'
foreach ($status in 'NotSigned', 'HashMismatch', 'NotTrusted', 'UnknownError') {
    $script:signature = New-TestSignature
    $script:signature.Status = $status
    Assert-Rejected { Assert-VisSignature 'test app.exe' 'CN=Example publisher' } 'Invalid Authenticode'
}
$script:signature = New-TestSignature
Assert-Rejected { Assert-VisSignature 'test app.exe' 'CN=Another publisher' } 'Unexpected signing publisher'
$script:signature.SignerCertificate = $null
Assert-Rejected { Assert-VisSignature 'test app.exe' 'CN=Example publisher' } 'Unexpected signing publisher'
$script:signature = New-TestSignature
$script:signature.TimeStamperCertificate = $null
Assert-Rejected { Assert-VisSignature 'test app.exe' 'CN=Example publisher' } 'Missing trusted timestamp'
$script:signature = New-TestSignature
$script:toolExitCode = 1
Assert-Rejected { Assert-VisSignature 'test app.exe' 'CN=Example publisher' } 'trust verification failed'
$script:toolExitCode = 2
Assert-Rejected { Assert-VisSignature 'test app.exe' 'CN=Example publisher' } 'trust verification failed'
Write-Output "Windows signature verification: $script:checks checks passed."
