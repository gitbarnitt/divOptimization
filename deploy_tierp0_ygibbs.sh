#!/bin/bash
# Deploy TierP0 analysis (ygibbs version) to GCP Cloud Run
# Usage: ./deploy_tierp0_ygibbs.sh

set -e

# Configuration
PROJECT_ID="neon-dev-is"
IMAGE="gcr.io/neon-dev-is/plantdiv-opt:latest"
REGION="us-central1"
SERVICE_ACCOUNT="plantdiv-opt-runner@neon-dev-is.iam.gserviceaccount.com"

# TierP0 sites to process (modify as needed)
SITES=("HARV" "JERC" "ORNL" "BARR")

# TierP0 parameters
TIER_MODE="tierp0"
PIPELINE_VERSION="ygibbs"  # Separates new runs from old manual-posterior results
EFFECT="0.20"              # 20% perturbation
POWER_REPS="100"           # Monte Carlo replicates

echo "===== TierP0 (ygibbs) GCP Deployment ====="
echo "Sites: ${SITES[*]}"
echo "Pipeline Version: ygibbs (NEW - uses extract_ygibbs_predictions)"
echo "Expected runtime: 20-40 min per site with ng=5000"
echo ""
echo "Output location: gs://neon-dev-os-data-availability/div_optimization/outputs_ygibbs/"
echo "  (Old results preserved at: outputs/)"
echo ""

for SITE in "${SITES[@]}"; do
    JOB_NAME="plantdiv-opt-tierp0-ygibbs-$(echo $SITE | tr '[:upper:]' '[:lower:]')"
    
    echo "Deploying: $JOB_NAME"
    
    # Create or update Cloud Run job
    gcloud run jobs deploy $JOB_NAME \
        --project=$PROJECT_ID \
        --region=$REGION \
        --image=$IMAGE \
        --service-account=$SERVICE_ACCOUNT \
        --set-env-vars="TIER_MODE=$TIER_MODE,SITE_ID=$SITE,PIPELINE_VERSION=$PIPELINE_VERSION,EFFECT=$EFFECT,POWER_REPS=$POWER_REPS" \
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
