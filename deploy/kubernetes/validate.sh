#!/bin/bash
# Validation script for copybook-rs Kubernetes manifests
# This script checks manifest syntax and validates configuration

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "========================================"
echo "copybook-rs Kubernetes Manifest Validation"
echo "========================================"
echo ""

# Check if kubectl is available
if ! command -v kubectl &> /dev/null; then
    echo -e "${RED}ERROR: kubectl not found${NC}"
    echo "Please install kubectl to validate manifests"
    exit 1
fi

echo -e "${GREEN}✓${NC} kubectl found: $(kubectl version --client --short 2>/dev/null || kubectl version --client)"

# Check if kustomize is available (built into kubectl)
if ! kubectl kustomize --help &> /dev/null; then
    echo -e "${YELLOW}WARNING: kustomize not available in kubectl${NC}"
    echo "Skipping kustomize validation"
    SKIP_KUSTOMIZE=1
else
    echo -e "${GREEN}✓${NC} kustomize available"
    SKIP_KUSTOMIZE=0
fi

echo ""
echo "========================================"
echo "Validating Base Manifests"
echo "========================================"

# Validate deployment.yaml syntax
echo -n "Checking deployment.yaml syntax... "
if kubectl apply --dry-run=client -f deployment.yaml &> /dev/null; then
    echo -e "${GREEN}✓${NC}"
else
    echo -e "${RED}✗${NC}"
    echo "Error validating deployment.yaml:"
    kubectl apply --dry-run=client -f deployment.yaml
    exit 1
fi

# Validate kustomization
if [ "$SKIP_KUSTOMIZE" -eq 0 ]; then
    echo -n "Checking base kustomization... "
    if kubectl kustomize . &> /dev/null; then
        echo -e "${GREEN}✓${NC}"
    else
        echo -e "${RED}✗${NC}"
        echo "Error in kustomization.yaml:"
        kubectl kustomize .
        exit 1
    fi
fi

echo ""
echo "========================================"
echo "Validating Environment Overlays"
echo "========================================"

# Validate dev overlay
if [ "$SKIP_KUSTOMIZE" -eq 0 ]; then
    echo -n "Checking dev overlay... "
    if kubectl kustomize overlays/dev/ &> /dev/null; then
        echo -e "${GREEN}✓${NC}"
    else
        echo -e "${RED}✗${NC}"
        echo "Error in dev overlay:"
        kubectl kustomize overlays/dev/
        exit 1
    fi

    # Validate staging overlay
    echo -n "Checking staging overlay... "
    if kubectl kustomize overlays/staging/ &> /dev/null; then
        echo -e "${GREEN}✓${NC}"
    else
        echo -e "${RED}✗${NC}"
        echo "Error in staging overlay:"
        kubectl kustomize overlays/staging/
        exit 1
    fi

    # Validate production overlay
    echo -n "Checking production overlay... "
    if kubectl kustomize overlays/production/ &> /dev/null; then
        echo -e "${GREEN}✓${NC}"
    else
        echo -e "${RED}✗${NC}"
        echo "Error in production overlay:"
        kubectl kustomize overlays/production/
        exit 1
    fi
fi

echo ""
echo "========================================"
echo "Validating Job Manifests"
echo "========================================"

# Validate decode job
echo -n "Checking decode-job.yaml... "
if kubectl apply --dry-run=client -f jobs/decode-job.yaml &> /dev/null; then
    echo -e "${GREEN}✓${NC}"
else
    echo -e "${RED}✗${NC}"
    echo "Error validating decode-job.yaml:"
    kubectl apply --dry-run=client -f jobs/decode-job.yaml
    exit 1
fi

# Validate encode job
echo -n "Checking encode-job.yaml... "
if kubectl apply --dry-run=client -f jobs/encode-job.yaml &> /dev/null; then
    echo -e "${GREEN}✓${NC}"
else
    echo -e "${RED}✗${NC}"
    echo "Error validating encode-job.yaml:"
    kubectl apply --dry-run=client -f jobs/encode-job.yaml
    exit 1
fi

# Validate verify job
echo -n "Checking verify-job.yaml... "
if kubectl apply --dry-run=client -f jobs/verify-job.yaml &> /dev/null; then
    echo -e "${GREEN}✓${NC}"
else
    echo -e "${RED}✗${NC}"
    echo "Error validating verify-job.yaml:"
    kubectl apply --dry-run=client -f jobs/verify-job.yaml
    exit 1
fi

# Validate determinism job
echo -n "Checking determinism-job.yaml... "
if kubectl apply --dry-run=client -f jobs/determinism-job.yaml &> /dev/null; then
    echo -e "${GREEN}✓${NC}"
else
    echo -e "${RED}✗${NC}"
    echo "Error validating determinism-job.yaml:"
    kubectl apply --dry-run=client -f jobs/determinism-job.yaml
    exit 1
fi

echo ""
echo "========================================"
echo "Resource Summary"
echo "========================================"

if [ "$SKIP_KUSTOMIZE" -eq 0 ]; then
    echo ""
    echo "Base Resources:"
    kubectl kustomize . | grep -E "^kind:|  name:" | sed 'N;s/\n/ /'

    echo ""
    echo "Production Resources:"
    kubectl kustomize overlays/production/ | grep -E "^kind:|  name:" | sed 'N;s/\n/ /'
fi

echo ""
echo "========================================"
echo -e "${GREEN}All validations passed!${NC}"
echo "========================================"
echo ""
echo "Next steps:"
echo "  - Apply base: kubectl apply -k ."
echo "  - Apply dev: kubectl apply -k overlays/dev/"
echo "  - Apply staging: kubectl apply -k overlays/staging/"
echo "  - Apply production: kubectl apply -k overlays/production/"
echo ""
