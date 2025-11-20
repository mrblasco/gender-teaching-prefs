#!/usr/bin/env bash
set -euo pipefail

# --- Default values ---
LOG_DIR="logs"
LOG_FILE="$LOG_DIR/$(date +'%Y%m%d_%H%M%S').log"
PARALLEL_JOBS=2

# --- Functions ---
usage() {
    cat <<EOF
Usage: $(basename "$0") [OPTIONS]

Options:
  -f, --filter FILE       Path to JQ filter file
  -s, --source DIR        Path to source directory with raw .json.gz files (default: $RAW_DIR)
  -o, --output FILE       Output file path (default: $OUTPUT_FILE)
  -p, --parallel N        Number of parallel jobs (default: $PARALLEL_JOBS)
  -h, --help              Show this help message

Example:
  $(basename "$0") --filter ./filters/assessment.jq --source ./data/raw --output ./processed/output.json --parallel 8
EOF
}

# --- Parse CLI args ---
while [[ $# -gt 0 ]]; do
    case "$1" in
        -f|--filter)
            FILTER="$2"
            shift 2
            ;;
        -s|--source)
            RAW_DIR="$2"
            shift 2
            ;;
        -o|--output)
            OUTPUT_FILE="$2"
            shift 2
            ;;
        -p|--parallel)
            PARALLEL_JOBS="$2"
            shift 2
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            echo "❌ Unknown option: $1"
            usage
            exit 1
            ;;
    esac
done

# --- Validation ---
if [[ ! -f "$FILTER" ]]; then
    echo "❌ Error: Filter file not found: $FILTER" >&2
    exit 1
fi

if [[ ! -d "$RAW_DIR" ]]; then
    echo "❌ Error: Source directory not found: $RAW_DIR" >&2
    exit 1
fi

mkdir -p "$(dirname $OUTPUT_FILE)" "$LOG_DIR"

# --- Main ---
{
    echo "[$(date +'%F %T')] 🚀 Starting assessment_strategy extraction"
    echo "Filter file:   $FILTER"
    echo "Source dir:    $RAW_DIR"
    echo "Output file:   $OUTPUT_FILE"
    echo "Parallel jobs: $PARALLEL_JOBS"
    echo "Log file:      $LOG_FILE"
    echo "----------------------------------------"

    # Expand $FILTER before passing to parallel (no export needed)
    find "$RAW_DIR" -type f -name "*.json.gz" | \
        parallel -j "$PARALLEL_JOBS" "gunzip -c {} | jq -rc -f $FILTER" > "$OUTPUT_FILE"

    echo "----------------------------------------"
    echo "✅ Saved processed data to $OUTPUT_FILE"
    echo "🏋️ File size: $(du -h "$OUTPUT_FILE" | cut -f1)"

    duration=$SECONDS
    printf "⏱️  Execution time: %02d:%02d:%02d\n" \
        $((duration/3600)) $((duration%3600/60)) $((duration%60))

    echo "----------------------------------------"
    echo "[$(date +'%F %T')] ✅ Completed successfully"
} 2>&1 | tee -a "$LOG_FILE"
