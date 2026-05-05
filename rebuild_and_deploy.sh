#!/bin/bash
##############################################################################################
# Rebuild Docker Image and Deploy Plant Diversity Optimization
# Usage: bash rebuild_and_deploy.sh [PROJECT_ID] [REGION] [SITE1] [SITE2] ...
#
# This script:
#   1. Builds a fresh Docker image from current code
#   2. Pushes it to GCP Container Registry
#   3. Deploys to specified sites (or all sites if none specified)
##############################################################################################

PROJECT_ID=${1:-"neon-nonprod-common-services"}
REGION=${2:-"us-central1"}
SERVICE_NAME="plant-div-optimization"
IMAGE_NAME="gcr.io/${PROJECT_ID}/${SERVICE_NAME}"

# Shift to get remaining args as sites
shift 2 2>/dev/null || shift $# 2>/dev/null

echo "========================================"
echo "Rebuild and Deploy: ${SERVICE_NAME}"
echo "Project: ${PROJECT_ID}"
echo "Region: ${REGION}"
echo "========================================"

# Step 1: Build Docker image
echo ""
echo "Step 1/3: Building Docker image..."
echo "----------------------------------------"
docker build -t ${IMAGE_NAME} .

if [ $? -ne 0 ]; then
  echo "❌ ERROR: Docker build failed"
  exit 1
fi
echo "✓ Docker image built successfully"

# Step 2: Push to GCP Container Registry
echo ""
echo "Step 2/3: Pushing to GCP Container Registry..."
echo "----------------------------------------"
docker push ${IMAGE_NAME}

if [ $? -ne 0 ]; then
  echo "❌ ERROR: Docker push failed"
  echo "Make sure you're authenticated: gcloud auth configure-docker"
  exit 1
fi
echo "✓ Image pushed to ${IMAGE_NAME}"

# Step 3: Deploy
echo ""
echo "Step 3/3: Deploying..."
echo "----------------------------------------"

if [ $# -eq 0 ]; then
  # No specific sites provided, deploy all
  echo "No sites specified - deploying to ALL sites"
  bash deploy_all_sites.sh ${PROJECT_ID} ${REGION}
else
  # Deploy to specified sites only
  SITES=("$@")
  echo "Deploying to ${#SITES[@]} sites: ${SITES[*]}"
  
  SUCCESS_COUNT=0
  FAILED_SITES=()
  
  for SITE in "${SITES[@]}"; do
    echo ""
    echo "========================================"
    echo "Deploying to: ${SITE}"
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
  echo "Deployment Summary"
  echo "========================================"
  echo "Successful: ${SUCCESS_COUNT}/${#SITES[@]}"
  
  if [ ${#FAILED_SITES[@]} -gt 0 ]; then
    echo "Failed sites: ${FAILED_SITES[*]}"
  fi
fi

echo ""
echo "========================================"
echo "✓ Rebuild and deployment complete!"
echo "========================================"
