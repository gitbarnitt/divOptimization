# Deploy TierP0 analysis (ygibbs version) to GCP Cloud Run
# Usage: .\deploy_tierp0_ygibbs.ps1

# Configuration
$PROJECT_ID = "neon-dev-is"
$IMAGE = "gcr.io/neon-dev-is/plantdiv-opt:latest"
$REGION = "us-central1"
$SERVICE_ACCOUNT = "plantdiv-opt-runner@neon-dev-is.iam.gserviceaccount.com"

# TierP0 sites to process (modify as needed)
$SITES = @("HARV", "JERC", "ORNL", "BARR")

# TierP0 parameters
$ENV_VARS = @{
    "TIER_MODE" = "tierp0"
    "PIPELINE_VERSION" = "ygibbs"  # Separates new runs from old manual-posterior results
    "EFFECT" = "0.20"               # 20% perturbation
    "POWER_REPS" = "100"            # Monte Carlo replicates
}

Write-Host "===== TierP0 (ygibbs) GCP Deployment =====" -ForegroundColor Cyan
Write-Host "Sites: $($SITES -join ', ')" -ForegroundColor Yellow
Write-Host "Pipeline Version: ygibbs (NEW - uses extract_ygibbs_predictions)" -ForegroundColor Yellow
Write-Host "Expected runtime: 20-40 min per site with ng=5000" -ForegroundColor Yellow
Write-Host ""
Write-Host "Output location: gs://neon-dev-os-data-availability/div_optimization/outputs_ygibbs/" -ForegroundColor Yellow
Write-Host "  (Old results preserved at: outputs/)" -ForegroundColor Gray
Write-Host ""

foreach ($SITE in $SITES) {
    $JOB_NAME = "plantdiv-opt-tierp0-ygibbs-$($SITE.ToLower())"
    
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
