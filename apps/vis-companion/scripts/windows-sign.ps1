param(
    [string]$FilePath,
    [switch]$VerifyOnly
)

$ErrorActionPreference = 'Stop'

function Assert-VisSignature {
    param([string]$Path, [string]$Publisher)

    if ([string]::IsNullOrWhiteSpace($Publisher)) {
        throw 'Missing required signing configuration: WINDOWS_SIGNING_PUBLISHER'
    }
    $signature = Get-AuthenticodeSignature -LiteralPath $Path
    if ($signature.Status -ne 'Valid') {
        throw "Invalid Authenticode signature on ${Path}: $($signature.Status)"
    }
    if ($null -eq $signature.SignerCertificate -or $signature.SignerCertificate.Subject -cne $Publisher) {
        throw "Unexpected signing publisher on $Path"
    }
    if ($null -eq $signature.TimeStamperCertificate) {
        throw "Missing trusted timestamp on $Path"
    }
    # WinVerifyTrust validates timestamp/certificate chains, not merely embedded certificates.
    $sdk = Join-Path ${env:ProgramFiles(x86)} 'Windows Kits/10/bin'
    $signtools = @(Get-ChildItem "$sdk/*/x64/signtool.exe" | Sort-Object FullName -Descending)
    if ($signtools.Count -eq 0) { throw 'Windows SDK signtool.exe is required' }
    $signtool = $signtools[0].FullName
    & $signtool verify /pa /all /tw $Path
    if ($LASTEXITCODE -ne 0) { throw "Authenticode trust verification failed on $Path" }
}

function Invoke-VisSigning {
    param([string]$Path)

    foreach ($name in 'ENDPOINT', 'ACCOUNT', 'PROFILE', 'PUBLISHER') {
        if ([string]::IsNullOrWhiteSpace([Environment]::GetEnvironmentVariable("WINDOWS_SIGNING_$name"))) {
            throw "Missing required signing configuration: WINDOWS_SIGNING_$name"
        }
    }
    Import-Module ArtifactSigning -RequiredVersion 0.1.8
    # azure/login obtains its Azure CLI session with GitHub OIDC. Disable all fallback credentials.
    $parameters = @{
        Endpoint = $env:WINDOWS_SIGNING_ENDPOINT
        CodeSigningAccountName = $env:WINDOWS_SIGNING_ACCOUNT
        CertificateProfileName = $env:WINDOWS_SIGNING_PROFILE
        Files = $Path
        FileDigest = 'SHA256'
        TimestampRfc3161 = 'http://timestamp.acs.microsoft.com'
        TimestampDigest = 'SHA256'
        ExcludeEnvironmentCredential = $true
        ExcludeWorkloadIdentityCredential = $true
        ExcludeManagedIdentityCredential = $true
        ExcludeSharedTokenCacheCredential = $true
        ExcludeVisualStudioCredential = $true
        ExcludeVisualStudioCodeCredential = $true
        ExcludeAzureCliCredential = $false
        ExcludeAzurePowerShellCredential = $true
        ExcludeAzureDeveloperCliCredential = $true
        ExcludeInteractiveBrowserCredential = $true
    }
    Invoke-ArtifactSigning @parameters
    Assert-VisSignature -Path $Path -Publisher $env:WINDOWS_SIGNING_PUBLISHER
}

if ($FilePath) {
    if ($VerifyOnly) {
        Assert-VisSignature -Path $FilePath -Publisher $env:WINDOWS_SIGNING_PUBLISHER
    } else {
        Invoke-VisSigning -Path $FilePath
    }
}
