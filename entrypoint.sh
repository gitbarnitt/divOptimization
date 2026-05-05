#!/bin/sh

# $TOKEN_NAME represents an environment variable passed to the container at runtime
# This script makes the contents of that environment variable available to R via the
# .Renviron file.

echo "NEON_PAT=$NEON_PAT" >> /srv/.Renviron
echo "DB_TOKEN=$DB_TOKEN" >> /srv/.Renviron
echo "GITHUB_PAT=$GITHUB_PAT" >> /srv/.Renviron
echo "PROJ=$PROJ" >> /srv/.Renviron
echo "START_DATE=$START_DATE" >> /srv/.Renviron
echo "REPORT_TYPE=$REPORT_TYPE" >> /srv/.Renviron
echo "FIELD_FULL=$FIELD_FULL" >> /srv/.Renviron
echo "SITE_ID=$SITE_ID" >> /srv/.Renviron
# Tier mode selection
echo "TIER_MODE=$TIER_MODE" >> /srv/.Renviron

# Tier1/Tier2A parameters
echo "TIER2_REPS=$TIER2_REPS" >> /srv/.Renviron
echo "GJAM_QUICK=$GJAM_QUICK" >> /srv/.Renviron

# TierP0 parameters (optional overrides - most hard-coded in tierp0_analysis.R)
if [ -n "$EFFECT" ]; then echo "EFFECT=$EFFECT" >> /srv/.Renviron; fi
if [ -n "$POWER_REPS" ]; then echo "POWER_REPS=$POWER_REPS" >> /srv/.Renviron; fi
if [ -n "$N_GRID" ]; then echo "N_GRID=$N_GRID" >> /srv/.Renviron; fi
if [ -n "$NOISE_MODE" ]; then echo "NOISE_MODE=$NOISE_MODE" >> /srv/.Renviron; fi
if [ -n "$DECISION_RULE" ]; then echo "DECISION_RULE=$DECISION_RULE" >> /srv/.Renviron; fi
if [ -n "$YEAR_BASELINE" ]; then echo "YEAR_BASELINE=$YEAR_BASELINE" >> /srv/.Renviron; fi
if [ -n "$YEAR_PERTURBED" ]; then echo "YEAR_PERTURBED=$YEAR_PERTURBED" >> /srv/.Renviron; fi

# Ensure R reads the .Renviron file and execute script
export R_ENVIRON_USER=/srv/.Renviron
exec Rscript /srv/plantDivOptimization_job_MULTISITE.R