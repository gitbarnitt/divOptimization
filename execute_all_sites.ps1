# Execute plant diversity optimization for all sites in parallel
# Each site runs as a separate Cloud Run job execution

# List of sites to process (3 sites for testing)
$sites = @(
    "HARV", "JERC", "SJER"
)

$jobName = "plant-div-optimization"
$region = "us-central1"
$project = "neon-nonprod-common-services"

# Track execution IDs
$executions = @{}

Write-Host "Starting parallel execution for $($sites.Count) sites..." -ForegroundColor Cyan
Write-Host "Job: $jobName" -ForegroundColor Cyan
Write-Host ""

foreach ($site in $sites) {
    Write-Host "Triggering: $site" -ForegroundColor Yellow
    
    # Execute job with SITE_ID environment variable
    $result = gcloud run jobs execute $jobName `
        --region=$region `
        --project=$project `
        --set-env-vars="SITE_ID=$site" `
        --format="value(metadata.name)" `
        2>&1
    
    if ($LASTEXITCODE -eq 0) {
        $executions[$site] = $result
        Write-Host "  ✓ Started: $result" -ForegroundColor Green
    } else {
        Write-Host "  ✗ Failed to start: $site" -ForegroundColor Red
        Write-Host "    Error: $result" -ForegroundColor Red
    }
    
    # Brief pause to avoid overwhelming the API
    Start-Sleep -Seconds 2
}

Write-Host ""
Write-Host "=================================" -ForegroundColor Cyan
Write-Host "All sites triggered!" -ForegroundColor Green
Write-Host "Started $($executions.Count) out of $($sites.Count) sites" -ForegroundColor Green
Write-Host "=================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Monitor progress in Cloud Console or with:" -ForegroundColor Yellow
Write-Host "  gcloud run jobs executions list --job=$jobName --region=$region --limit=50" -ForegroundColor Gray
