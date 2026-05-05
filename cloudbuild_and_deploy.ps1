##############################################################################################
# Rebuild using Cloud Build and Deploy Plant Diversity Optimization
# Usage: .\cloudbuild_and_deploy.ps1 [-ProjectId "neon-nonprod-common-services"] [-Region "us-central1"] [-Sites @("JERC")]
#
# This script:
#   1. Builds Docker image in Google Cloud Build (no local Docker needed)
#   2. Deploys to specified sites (or all sites if none specified)
##############################################################################################

param(
    [string]$ProjectId = "neon-nonprod-common-services",
    [string]$Region = "us-central1",
    [string[]]$Sites = @()
)

$ServiceName = "plant-div-optimization"
$ImageName = "gcr.io/$ProjectId/$ServiceName"

$AllSites = @(
    "ABBY", "BARR", "BART", "BLAN", "BONA", "CLBJ", "CPER", "DCFS", "DEJU",
    "DELA", "DSNY", "GRSM", "GUAN", "HARV", "HEAL", "JERC", "JORN", "KONA",
    "KONZ", "LAJA", "LENO", "MLBS", "MOAB", "NIWO", "NOGP", "OAES", "ONAQ",
    "ORNL", "OSBS", "PUUM", "RMNP", "SCBI", "SERC", "SJER", "SOAP", "SRER",
    "STEI", "STER", "TALL", "TEAK", "TOOL", "TREE", "UKFS", "UNDE", "WOOD",
    "WREF", "YELL"
)

Write-Host "========================================" -ForegroundColor Cyan
Write-Host "Cloud Build and Deploy: $ServiceName" -ForegroundColor Cyan
Write-Host "Project: $ProjectId" -ForegroundColor Cyan
Write-Host "Region: $Region" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan

# Step 1: Build using Cloud Build
Write-Host ""
Write-Host "Step 1/2: Building Docker image with Cloud Build..." -ForegroundColor Yellow
Write-Host "----------------------------------------"
Write-Host "This will build in GCP (no local Docker required)" -ForegroundColor Gray

gcloud builds submit --tag $ImageName --project=$ProjectId .

if ($LASTEXITCODE -ne 0) {
    Write-Host "ERROR: Cloud Build failed" -ForegroundColor Red
    exit 1
}
Write-Host "Image built and pushed to $ImageName" -ForegroundColor Green

# Step 2: Deploy
Write-Host ""
Write-Host "Step 2/2: Deploying..." -ForegroundColor Yellow
Write-Host "----------------------------------------"

$DeploySites = if ($Sites.Count -eq 0) { 
    Write-Host "No sites specified - deploying to ALL $($AllSites.Count) sites" -ForegroundColor Yellow
    $AllSites 
} else { 
    Write-Host "Deploying to $($Sites.Count) specified sites" -ForegroundColor Yellow
    $Sites 
}

$SuccessCount = 0
$FailedSites = @()

foreach ($Site in $DeploySites) {
    Write-Host ""
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host "Deploying to: $Site" -ForegroundColor Cyan
    Write-Host "========================================" -ForegroundColor Cyan
    
    $JobName = "$ServiceName-$($Site.ToLower())-$(Get-Date -Format 'yyyyMMdd-HHmmss')"
    
    gcloud run jobs create $JobName `
        --image=$ImageName `
        --region=$Region `
        --project=$ProjectId `
        --max-retries=0 `
        --task-timeout=3h `
        --memory=32Gi `
        --cpu=4 `
        --set-env-vars="SITE_ID=$Site,GJAM_QUICK=false,PRUNE_MODE=aggressive" `
        --service-account="neon-dev-os-service@$ProjectId.iam.gserviceaccount.com"
    
    if ($LASTEXITCODE -eq 0) {
        Write-Host "Job created: $JobName" -ForegroundColor Green
        
        Write-Host "Executing job..." -ForegroundColor Yellow
        gcloud run jobs execute $JobName `
            --region=$Region `
            --project=$ProjectId `
            --wait
        
        if ($LASTEXITCODE -eq 0) {
            Write-Host "SUCCESS: $Site completed" -ForegroundColor Green
            $SuccessCount++
        } else {
            Write-Host "FAILED: $Site execution failed" -ForegroundColor Red
            $FailedSites += $Site
        }
    } else {
        Write-Host "FAILED: Could not create job for $Site" -ForegroundColor Red
        $FailedSites += $Site
    }
}

Write-Host ""
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "Deployment Summary" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "Successful: $SuccessCount / $($DeploySites.Count)" -ForegroundColor Green

if ($FailedSites.Count -gt 0) {
    Write-Host "Failed sites:" -ForegroundColor Red
    foreach ($Site in $FailedSites) {
        Write-Host "  - $Site" -ForegroundColor Red
    }
}

Write-Host ""
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "Cloud Build and deployment complete" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
