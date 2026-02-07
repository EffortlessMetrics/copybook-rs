#!/bin/bash
# Local Fuzzing Script
# Runs fuzzers locally with configurable options

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
FUZZ_DIR="${FUZZ_DIR:-fuzz}"
FUZZ_SECONDS="${FUZZ_SECONDS:-60}"
FUZZ_RUNS="${FUZZ_RUNS:-0}"
JOBS="${JOBS:-1}"
WORKERS="${WORKERS:-1}"

# Available targets
TARGETS=(
    "copybook_parse"
    "binary_decode"
    "json_encode"
    "pic_clause"
    "occurs_odo"
    "redefines"
)

# Function to print usage
print_usage() {
    echo "Usage: $0 [OPTIONS] [TARGET]"
    echo ""
    echo "Options:"
    echo "  -t, --time SECONDS    Fuzzing duration in seconds (default: 60)"
    echo "  -r, --runs N          Number of fuzzing iterations (default: 0 = time-based)"
    echo "  -j, --jobs N          Number of parallel jobs (default: 1)"
    echo "  -w, --workers N       Number of worker threads (default: 1)"
    echo "  -c, --cmin            Minimize corpus"
    echo "  -m, --tmin FILE       Minimize specific input file"
    echo "  -l, --list            List available targets"
    echo "  -a, --all             Run all targets"
    echo "  -h, --help            Show this help message"
    echo ""
    echo "Targets:"
    for target in "${TARGETS[@]}"; do
        echo "  $target"
    done
    echo ""
    echo "Examples:"
    echo "  $0 copybook_parse                    # Run copybook_parse fuzzer for 60 seconds"
    echo "  $0 -t 300 copybook_parse            # Run for 5 minutes"
    echo "  $0 -r 10000 binary_decode          # Run 10000 iterations"
    echo "  $0 -c copybook_parse                # Minimize corpus"
    echo "  $0 -a                               # Run all targets"
}

# Function to check if cargo-fuzz is installed
check_cargo_fuzz() {
    if ! command -v cargo-fuzz &> /dev/null; then
        echo -e "${RED}Error: cargo-fuzz is not installed${NC}"
        echo ""
        echo "Install cargo-fuzz:"
        echo "  cargo install cargo-fuzz --version 0.13.4"
        exit 1
    fi
}

# Function to run a fuzzer
run_fuzzer() {
    local target="$1"
    local extra_args=("${@:2}")

    echo -e "${BLUE}Running fuzzer: ${target}${NC}"
    echo "Duration: ${FUZZ_SECONDS}s, Runs: ${FUZZ_RUNS}, Jobs: ${JOBS}, Workers: ${WORKERS}"
    echo ""

    cd "${FUZZ_DIR}"

    # Build fuzzer options
    local fuzz_opts=(
        "-max_total_time=${FUZZ_SECONDS}"
        "-runs=${FUZZ_RUNS}"
        "-jobs=${JOBS}"
        "-workers=${WORKERS}"
    )

    # Add extra args if provided
    if [ ${#extra_args[@]} -gt 0 ]; then
        fuzz_opts+=("${extra_args[@]}")
    fi

    # Run the fuzzer
    cargo fuzz run "${target}" -- "${fuzz_opts[@]}"
}

# Function to minimize corpus
minimize_corpus() {
    local target="$1"

    echo -e "${BLUE}Minimizing corpus for: ${target}${NC}"
    echo ""

    cd "${FUZZ_DIR}"
    cargo fuzz cmin "${target}"
}

# Function to minimize specific input
minimize_input() {
    local target="$1"
    local input_file="$2"

    echo -e "${BLUE}Minimizing input: ${input_file} for target: ${target}${NC}"
    echo ""

    cd "${FUZZ_DIR}"
    cargo fuzz tmin "${target}" "${input_file}"
}

# Function to list targets
list_targets() {
    echo "Available fuzz targets:"
    echo ""
    for target in "${TARGETS[@]}"; do
        echo "  ${target}"
    done
}

# Function to run all targets
run_all_targets() {
    echo -e "${BLUE}Running all fuzz targets${NC}"
    echo ""

    for target in "${TARGETS[@]}"; do
        echo ""
        echo -e "${GREEN}========================================${NC}"
        echo -e "${GREEN}Target: ${target}${NC}"
        echo -e "${GREEN}========================================${NC}"
        echo ""

        run_fuzzer "${target}"
    done
}

# Parse command line arguments
TARGET=""
CMIN=false
TMIN_FILE=""

while [[ $# -gt 0 ]]; do
    case $1 in
        -t|--time)
            FUZZ_SECONDS="$2"
            shift 2
            ;;
        -r|--runs)
            FUZZ_RUNS="$2"
            shift 2
            ;;
        -j|--jobs)
            JOBS="$2"
            shift 2
            ;;
        -w|--workers)
            WORKERS="$2"
            shift 2
            ;;
        -c|--cmin)
            CMIN=true
            shift
            ;;
        -m|--tmin)
            TMIN_FILE="$2"
            shift 2
            ;;
        -l|--list)
            list_targets
            exit 0
            ;;
        -a|--all)
            check_cargo_fuzz
            run_all_targets
            exit 0
            ;;
        -h|--help)
            print_usage
            exit 0
            ;;
        -*)
            echo -e "${RED}Error: Unknown option: $1${NC}"
            print_usage
            exit 1
            ;;
        *)
            TARGET="$1"
            shift
            ;;
    esac
done

# Check if cargo-fuzz is installed
check_cargo_fuzz

# Check if target is specified
if [ -z "${TARGET}" ]; then
    echo -e "${RED}Error: No target specified${NC}"
    echo ""
    print_usage
    exit 1
fi

# Validate target
valid_target=false
for t in "${TARGETS[@]}"; do
    if [ "${t}" = "${TARGET}" ]; then
        valid_target=true
        break
    fi
done

if [ "${valid_target}" = false ]; then
    echo -e "${RED}Error: Invalid target: ${TARGET}${NC}"
    echo ""
    echo "Available targets:"
    for t in "${TARGETS[@]}"; do
        echo "  ${t}"
    done
    exit 1
fi

# Run the requested operation
if [ "${CMIN}" = true ]; then
    minimize_corpus "${TARGET}"
elif [ -n "${TMIN_FILE}" ]; then
    if [ ! -f "${TMIN_FILE}" ]; then
        echo -e "${RED}Error: Input file not found: ${TMIN_FILE}${NC}"
        exit 1
    fi
    minimize_input "${TARGET}" "${TMIN_FILE}"
else
    run_fuzzer "${TARGET}"
fi

echo ""
echo -e "${GREEN}Fuzzing completed${NC}"
