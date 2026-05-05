#!/bin/bash
# Deploy TrendRun analysis to GCP Cloud Run for overnight runs
# Usage: ./deploy_trendrun.sh

set -e

# Configuration
PROJECT_ID="neon-dev-is"
IMAGE="gcr.io/neon-dev-is/plantdiv-opt:latest"
REGION="us-central1"
SERVICE_ACCOUNT="plantdiv-opt-runner@neon-dev-is.iam.gserviceaccount.com"

# TrendRun sites to process (modify as needed)
SITES=("HARV" "JERC" "ORNL")

# TrendRun parameters
TIER_MODE="trendrun"
POST_DRAWS="1000"
TREND_ADD_NET="0.20"
TREND_REPS="100"

echo "===== TrendRun GCP Deployment ====="
echo "Sites: ${SITES[*]}"
echo "Expected runtime: 30-60 min per site with ng=5000"
echo ""

for SITE in "${SITES[@]}"; do
    JOB_NAME="plantdiv-opt-trendrun-$(echo $SITE | tr '[:upper:]' '[:lower:]')"
    
    echo "Deploying: $JOB_NAME"
    
    # Create or update Cloud Run job
    gcloud run jobs deploy $JOB_NAME \
        --project=$PROJECT_ID \
        --region=$REGION \
        --image=$IMAGE \
        --service-account=$SERVICE_ACCOUNT \
        --set-env-vars="TIER_MODE=$TIER_MODE,SITE_ID=$SITE,POST_DRAWS=$POST_DRAWS,TREND_ADD_NET=$TREND_ADD_NET,TREND_REPS=$TREND_REPS" \
        --memory=32Gi \
        --cpu=4 \
        --max-retries=0 \
        --task-timeout=3h \
        --parallelism=1 \
        --tasks=1
    
    echo "  ✓ Job deployed successfully"
    
    # Execute the job
    echo "  ▶ Starting execution..."
    gcloud run jobs execute $JOB_NAME \
        --project=$PROJECT_ID \
        --region=$REGION
    
    echo "  ✓ Job started - logs: https://console.cloud.google.com/run/jobs/details/$REGION/$JOB_NAME"
    echo ""
done

echo "===== Deployment Complete ====="
echo "Monitor jobs at: https://console.cloud.google.com/run/jobs?project=$PROJECT_ID"
echo ""
echo "Output location: gs://neon-dev-os-data-availability/div_optimization/outputs_trendrun/"
echo "  Structure: outputs_trendrun/SITE_ID/trendrun/*.parquet"
