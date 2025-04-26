param
(
    [Parameter(Mandatory)]
    $Url,
    
    [Parameter(Mandatory)]
    $Destination
)

mkdir $Destination -Force
$fullDestination = Resolve-Path $Destination
Write-Host "Downloading to '$fullDestination' from '$Url'"


# enable TLS 1.2 and TLS 1.1 protocols
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12, [Net.SecurityProtocolType]::Tls11

$WebResponse = Invoke-WebRequest -Uri $Url
# get the list of links, skip the first one ("../") and download the files
$jobs = $WebResponse.Links | Select-Object -ExpandProperty href -Skip 1 | ForEach-Object {
    Start-Job -ScriptBlock {
        param($inputObject)
        Write-Host "Downloading file '$inputObject'"
        $filePath = Join-Path -Path $($using:fullDestination) -ChildPath $inputObject
        $fileUrl  = '{0}/{1}' -f $($using:Url).TrimEnd('/'), $inputObject
        Invoke-WebRequest -Uri $fileUrl -OutFile $filePath
    } -ArgumentList $_
}

$results = foreach ($job in $jobs) {
    Wait-Job $job
    Receive-Job $job
}

# Clean up jobs
Remove-Job $jobs

# Process the results
$results
