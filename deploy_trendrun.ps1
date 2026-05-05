# Deploy TrendRun analysis to GCP Cloud Run for overnight runs
# Usage: .\deploy_trendrun.ps1

# Configuration
$PROJECT_ID = "neon-dev-is"
$IMAGE = "gcr.io/neon-dev-is/plantdiv-opt:latest"
$REGION = "us-central1"
$SERVICE_ACCOUNT = "plantdiv-opt-runner@neon-dev-is.iam.gserviceaccount.com"

# TrendRun sites to process (modify as needed)
$SITES = @("HARV", "JERC", "ORNL")

# TrendRun parameters
$ENV_VARS = @{
    "TIER_MODE" = "trendrun"
    "POST_DRAWS" = "1000"
    "TREND_ADD_NET" = "0.20"
    "TREND_REPS" = "100"
}

Write-Host "===== TrendRun GCP Deployment =====" -ForegroundColor Cyan
Write-Host "Sites: $($SITES -join ', ')" -ForegroundColor Yellow
Write-Host "Expected runtime: 30-60 min per site with ng=5000" -ForegroundColor Yellow
Write-Host ""

foreach ($SITE in $SITES) {
    $JOB_NAME = "plantdiv-opt-trendrun-$($SITE.ToLower())"
    
    Write-Host "Deploying: $JOB_NAME" -ForegroundColor Green
    
    # Build environment variable string for gcloud
    $env_string = ($ENV_VARS.GetEnumerator() | ForEach-Object { 
        "$($_.Key)=$($_.Value)" 
    }) -join ","
    $env_string += ",SITE_ID=$SITE"
    
    # Create or update Cloud Run job
    gcloud run jobs deploy $JOB_NAME `
        --project=$PROJECT_ID `
        --region=$REGION `
        --image=$IMAGE `
        --service-account=$SERVICE_ACCOUNT `
        --set-env-vars=$env_string `
        --memory=32Gi `
        --cpu=4 `
        --max-retries=0 `
        --task-timeout=3h `
        --parallelism=1 `
        --tasks=1
    
    if ($LASTEXITCODE -eq 0) {
        Write-Host "  ✓ Job deployed successfully" -ForegroundColor Green
        
        # Execute the job
        Write-Host "  ▶ Starting execution..." -ForegroundColor Cyan
        gcloud run jobs execute $JOB_NAME `
            --project=$PROJECT_ID `
            --region=$REGION
        
        if ($LASTEXITCODE -eq 0) {
            Write-Host "  ✓ Job started - logs: https://console.cloud.google.com/run/jobs/details/$REGION/$JOB_NAME" -ForegroundColor Green
        } else {
            Write-Host "  ✗ Failed to start job" -ForegroundColor Red
        }
    } else {
        Write-Host "  ✗ Failed to deploy job" -ForegroundColor Red
    }
    
    Write-Host ""
}

Write-Host "===== Deployment Complete =====" -ForegroundColor Cyan
Write-Host "Monitor jobs at: https://console.cloud.google.com/run/jobs?project=$PROJECT_ID" -ForegroundColor Yellow
Write-Host ""
Write-Host "Output location: gs://neon-dev-os-data-availability/div_optimization/outputs_trendrun/" -ForegroundColor Yellow
Write-Host "  Structure: outputs_trendrun/SITE_ID/trendrun/*.parquet" -ForegroundColor Gray
