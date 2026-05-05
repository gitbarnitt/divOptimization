# Extract unique sites from plant_data.rds
# This helps identify which sites are available for processing

$dataPath = "C:/Users/dbarnett/OneDrive - Battelle Ecology/optimization/outputs/plant_data.rds"

# Check if file exists
if (-not (Test-Path $dataPath)) {
    Write-Host "Error: Data file not found at: $dataPath" -ForegroundColor Red
    Write-Host "Update the path in this script to point to your plant_data.rds file" -ForegroundColor Yellow
    exit
}

Write-Host "Reading plant_data.rds to extract site list..." -ForegroundColor Cyan

# Read RDS and get unique sites
$script = @"
data <- readRDS('$dataPath')
sites <- unique(data`$siteID)
sites <- sort(sites)
cat(paste(sites, collapse=','))
"@

$sitesString = Rscript -e $script

if ($LASTEXITCODE -eq 0 -and $sitesString) {
    $sites = $sitesString -split ','
    
    Write-Host "`nFound $($sites.Count) unique sites:" -ForegroundColor Green
    Write-Host ""
    
    # Display all sites
    for ($i = 0; $i -lt $sites.Count; $i++) {
        Write-Host "  $($i+1). $($sites[$i])"
    }
    
    Write-Host ""
    Write-Host "First 3 sites for testing:" -ForegroundColor Yellow
    $firstThree = $sites[0..2]
    Write-Host "  $($firstThree -join ', ')" -ForegroundColor Cyan
    
    Write-Host ""
    Write-Host "To update execute_all_sites.ps1, use this list:" -ForegroundColor Yellow
    Write-Host "`$sites = @(" -ForegroundColor Gray
    foreach ($site in $sites) {
        Write-Host "    `"$site`"," -ForegroundColor Gray
    }
    Write-Host ")" -ForegroundColor Gray
    
} else {
    Write-Host "Error reading data file" -ForegroundColor Red
    Write-Host $sitesString
}
