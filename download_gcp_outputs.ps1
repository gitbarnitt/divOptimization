#
# Download Plant Diversity Optimization Outputs from GCP
# Downloads TierP0 and TrendRun results from GCS buckets to local OneDrive
#

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

$BucketName = "neon-dev-os-data-availability"

# GCS paths (note: trendrun has "outputss_trendrun" with double 's')
$TierP0_GCS = "gs://$BucketName/div_optimization/outputs_ygibbs/*/*"
$TrendRun_GCS = "gs://$BucketName/div_optimization/outputss_trendrun/*/*"

# Local destination paths
$TierP0_Local = "C:\Users\dbarnett\OneDrive - Battelle Ecology\optimization\outputs\tierp0"
$TrendRun_Local = "C:\Users\dbarnett\OneDrive - Battelle Ecology\optimization\outputs\trendrun"

# Temporary download location (in project folder for easier access)
$TempDownload = "$PSScriptRoot\temp_gcp_downloads"

# ------------------------------------------------------------------------------
# Helper Functions
# ------------------------------------------------------------------------------

function Test-GCloudAuth {
    Write-Host "`n[CHECK] Verifying gcloud authentication..." -ForegroundColor Cyan
    $account = gcloud config get-value account 2>$null
    if (-not $account) {
        Write-Host "[ERROR] Not authenticated to gcloud. Please run:" -ForegroundColor Red
        Write-Host "  gcloud auth login" -ForegroundColor Yellow
        Write-Host "  gcloud config set project neon-nonprod-common-services" -ForegroundColor Yellow
        return $false
    }
    Write-Host "[OK] Authenticated as: $account" -ForegroundColor Green
    
    # Verify correct project
    $project = gcloud config get-value project 2>$null
    if ($project -ne "neon-nonprod-common-services") {
        Write-Host "[WARNING] Current project: $project" -ForegroundColor Yellow
        Write-Host "[WARNING] Expected: neon-nonprod-common-services" -ForegroundColor Yellow
        Write-Host "[WARNING] Run: gcloud config set project neon-nonprod-common-services" -ForegroundColor Yellow
    } else {
        Write-Host "[OK] Using project: $project" -ForegroundColor Green
    }
    
    return $true
}

function Download-AndFlatten {
    param(
        [string]$GCSPath,
        [string]$TempDir,
        [string]$FinalDir,
        [string]$Label
    )
    
    Write-Host "`n========================================" -ForegroundColor Cyan
    Write-Host "  $Label" -ForegroundColor Cyan
    Write-Host "========================================" -ForegroundColor Cyan
    
    # Create temp directory
    $tempTarget = Join-Path $TempDir $Label
    New-Item -ItemType Directory -Path $tempTarget -Force | Out-Null
    
    Write-Host "[DOWNLOAD] Fetching from GCS: $GCSPath" -ForegroundColor Yellow
    Write-Host "[DOWNLOAD] Temporary location: $tempTarget" -ForegroundColor Yellow
    
    # Download with gsutil (recursive, multi-threaded)
    gsutil -m cp -r $GCSPath $tempTarget
    
    if ($LASTEXITCODE -ne 0) {
        Write-Host "[ERROR] Download failed for $Label" -ForegroundColor Red
        return
    }
    
    # Count files
    $downloadedFiles = Get-ChildItem -Path $tempTarget -Recurse -File
    Write-Host "[INFO] Downloaded $($downloadedFiles.Count) files" -ForegroundColor Green
    
    # Flatten: Move all files to final directory
    Write-Host "[FLATTEN] Moving files to: $FinalDir" -ForegroundColor Yellow
    New-Item -ItemType Directory -Path $FinalDir -Force | Out-Null
    
    $fileCount = 0
    $parquetCount = 0
    
    foreach ($file in $downloadedFiles) {
        $destPath = Join-Path $FinalDir $file.Name
        
        # Handle duplicates by adding site prefix if needed
        if (Test-Path $destPath) {
            # Extract site ID from path (assuming structure like .../SITE/tierp0/file.parquet)
            $siteMatch = $file.FullName -match '[\\/]([A-Z]{4})[\\/]'
            if ($siteMatch) {
                $siteID = $matches[1]
                $newName = "${siteID}_$($file.Name)"
                $destPath = Join-Path $FinalDir $newName
                Write-Host "  [RENAME] $($file.Name) -> $newName (duplicate)" -ForegroundColor DarkYellow
            }
        }
        
        Copy-Item -Path $file.FullName -Destination $destPath -Force
        $fileCount++
        
        if ($file.Extension -eq ".parquet") {
            $parquetCount++
        }
    }
    
    Write-Host "[SUCCESS] Copied $fileCount files ($parquetCount parquet) to $FinalDir" -ForegroundColor Green
    
    # Show sample files
    $sampleFiles = Get-ChildItem -Path $FinalDir -File | Select-Object -First 5
    Write-Host "`n[SAMPLE] First 5 files in destination:" -ForegroundColor Cyan
    $sampleFiles | ForEach-Object { Write-Host "  - $($_.Name)" }
}

# ------------------------------------------------------------------------------
# Main Script
# ------------------------------------------------------------------------------

Write-Host @"

================================================================================
  GCP Output Downloader - Plant Diversity Optimization
================================================================================
  TierP0 Source:   $TierP0_GCS
  TierP0 Dest:     $TierP0_Local
  
  TrendRun Source: $TrendRun_GCS
  TrendRun Dest:   $TrendRun_Local
  
  Temp Location:   $TempDownload
================================================================================

"@ -ForegroundColor White

# Check authentication
if (-not (Test-GCloudAuth)) {
    exit 1
}

# Create temp directory
New-Item -ItemType Directory -Path $TempDownload -Force | Out-Null

# Prompt user for what to download
Write-Host "`nWhat would you like to download?" -ForegroundColor Yellow
Write-Host "  1) TierP0 only"
Write-Host "  2) TrendRun only"
Write-Host "  3) Both TierP0 and TrendRun"
Write-Host "  Q) Quit"
$choice = Read-Host "`nEnter choice (1-3, Q)"

switch ($choice) {
    "1" {
        Download-AndFlatten -GCSPath $TierP0_GCS -TempDir $TempDownload -FinalDir $TierP0_Local -Label "TierP0"
    }
    "2" {
        Download-AndFlatten -GCSPath $TrendRun_GCS -TempDir $TempDownload -FinalDir $TrendRun_Local -Label "TrendRun"
    }
    "3" {
        Download-AndFlatten -GCSPath $TierP0_GCS -TempDir $TempDownload -FinalDir $TierP0_Local -Label "TierP0"
        Download-AndFlatten -GCSPath $TrendRun_GCS -TempDir $TempDownload -FinalDir $TrendRun_Local -Label "TrendRun"
    }
    "Q" {
        Write-Host "`n[CANCELLED] Exiting..." -ForegroundColor Yellow
        exit 0
    }
    default {
        Write-Host "`n[ERROR] Invalid choice: $choice" -ForegroundColor Red
        exit 1
    }
}

# Cleanup temp directory
Write-Host "`n[CLEANUP] Removing temporary files..." -ForegroundColor Yellow
Remove-Item -Path $TempDownload -Recurse -Force -ErrorAction SilentlyContinue

Write-Host "`n[COMPLETE] Download finished!" -ForegroundColor Green
Write-Host "`nFiles saved to:" -ForegroundColor Cyan
if ($choice -eq "1" -or $choice -eq "3") {
    Write-Host "  TierP0:   $TierP0_Local" -ForegroundColor White
}
if ($choice -eq "2" -or $choice -eq "3") {
    Write-Host "  TrendRun: $TrendRun_Local" -ForegroundColor White
}

Write-Host "`nTo explore files, run:" -ForegroundColor Cyan
Write-Host "  Get-ChildItem '$TierP0_Local' -File" -ForegroundColor Yellow
Write-Host "  Get-ChildItem '$TrendRun_Local' -File" -ForegroundColor Yellow
