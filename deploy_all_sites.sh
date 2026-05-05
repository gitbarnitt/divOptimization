#!/bin/bash
##############################################################################################
# Deploy Plant Diversity Optimization for All 47 Sites
# Usage: bash deploy_all_sites.sh [PROJECT_ID] [REGION]
##############################################################################################

PROJECT_ID=${1:-"neon-dev-project"}
REGION=${2:-"us-central1"}
SERVICE_NAME="plant-div-optimization"
IMAGE_NAME="gcr.io/${PROJECT_ID}/${SERVICE_NAME}"

# List of all 47 NEON sites
SITES=(
  "ABBY" "BARR" "BART" "BLAN" "BONA" "CLBJ" "CPER" "DCFS" "DEJU" 
  "DELA" "DSNY" "GRSM" "GUAN" "HARV" "HEAL" "JERC" "JORN" "KONA" 
  "KONZ" "LAJA" "LENO" "MLBS" "MOAB" "NIWO" "NOGP" "OAES" "ONAQ" 
  "ORNL" "OSBS" "PUUM" "RMNP" "SCBI" "SERC" "SJER" "SOAP" "SRER" 
  "STEI" "STER" "TALL" "TEAK" "TOOL" "TREE" "UKFS" "UNDE" "WOOD" 
  "WREF" "YELL"
)

echo "========================================"
echo "Deploying ${SERVICE_NAME} for ${#SITES[@]} sites"
echo "Project: ${PROJECT_ID}"
echo "Region: ${REGION}"
echo "========================================"

# Check if image exists
echo "Checking for Docker image..."
if ! gcloud container images describe ${IMAGE_NAME} --project=${PROJECT_ID} &>/dev/null; then
  echo "ERROR: Docker image not found: ${IMAGE_NAME}"
  echo "Build and push the image first:"
  echo "  docker build -t ${IMAGE_NAME} ."
  echo "  docker push ${IMAGE_NAME}"
  exit 1
fi
echo "✓ Image found: ${IMAGE_NAME}"

# Submit jobs for all sites
SUCCESS_COUNT=0
FAILED_SITES=()

for SITE in "${SITES[@]}"; do
  echo ""
  echo "========================================" 
  echo "Submitting job for site: ${SITE}"
  echo "========================================"
  
  JOB_NAME="${SERVICE_NAME}-${SITE,,}-$(date +%Y%m%d-%H%M%S)"
  
  gcloud run jobs create ${JOB_NAME} \
    --image=${IMAGE_NAME} \
    --region=${REGION} \
    --project=${PROJECT_ID} \
    --max-retries=0 \
    --task-timeout=3h \
    --memory=32Gi \
    --cpu=4 \
    --set-env-vars="SITE_ID=${SITE},GJAM_QUICK=false,PRUNE_MODE=aggressive" \
    --service-account=neon-dev-os-service@${PROJECT_ID}.iam.gserviceaccount.com
  
  if [ $? -eq 0 ]; then
    echo "✓ Job created: ${JOB_NAME}"
    
    # Execute the job
    echo "Executing job..."
    gcloud run jobs execute ${JOB_NAME} \
      --region=${REGION} \
      --project=${PROJECT_ID} \
      --wait
    
    if [ $? -eq 0 ]; then
      echo "✓ SUCCESS: ${SITE} completed"
      ((SUCCESS_COUNT++))
    else
      echo "✗ FAILED: ${SITE} execution failed"
      FAILED_SITES+=("${SITE}")
    fi
  else
    echo "✗ FAILED: Could not create job for ${SITE}"
    FAILED_SITES+=("${SITE}")
  fi
done

echo ""
echo "========================================"
echo "DEPLOYMENT SUMMARY"
echo "========================================"
echo "Total sites: ${#SITES[@]}"
echo "Successful: ${SUCCESS_COUNT}"
echo "Failed: ${#FAILED_SITES[@]}"

if [ ${#FAILED_SITES[@]} -gt 0 ]; then
  echo ""
  echo "Failed sites:"
  for SITE in "${FAILED_SITES[@]}"; do
    echo "  - ${SITE}"
  done
fi

echo ""
echo "Check outputs at:"
echo "  gs://neon-dev-os-data-availability/div_optimization/outputs/"
echo "========================================"
