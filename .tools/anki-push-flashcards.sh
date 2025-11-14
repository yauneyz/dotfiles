#!/bin/bash

# anki-push-flashcards.sh
# Pushes all flashcard org files with #+PROPERTY tags to Anki

set -euo pipefail

FLASHCARD_DIR="/home/zac/development/org/flashcards"
ANKI_CONNECT_URL="http://localhost:8765"
ERRORS=()

# Check if Anki is running and AnkiConnect is accessible
check_anki_connection() {
  if ! curl -s --max-time 2 "$ANKI_CONNECT_URL" -X POST -d '{"action":"version","version":6}' > /dev/null 2>&1; then
    echo "Error: Cannot connect to Anki. Please ensure:"
    echo "  1. Anki is running"
    echo "  2. AnkiConnect plugin is installed"
    echo "  3. AnkiConnect is listening on $ANKI_CONNECT_URL"
    exit 1
  fi
}

# Push notes from a single file
push_file() {
  local file="$1"
  local emacs_cmd='(progn
    (require '"'"'anki-editor)
    (condition-case err
        (progn
          (anki-editor-push-notes)
          (kill-emacs 0))
      (error
        (message "ERROR: %s" (error-message-string err))
        (kill-emacs 1))))'

  if ! emacs --batch \
       -l ~/.emacs.d/init.el \
       "$file" \
       --eval "$emacs_cmd" 2>&1 | grep -q "ERROR:"; then
    return 0
  else
    return 1
  fi
}

# Main execution
main() {
  echo "Checking Anki connection..."
  check_anki_connection

  echo "Finding flashcard files..."

  # Find all .org files with #+PROPERTY or #+PROPERTIES
  mapfile -t files < <(find "$FLASHCARD_DIR" -type f -name "*.org" -exec grep -l "^#\+PROPERT" {} \;)

  if [ ${#files[@]} -eq 0 ]; then
    echo "No flashcard files found with #+PROPERTY tags."
    exit 0
  fi

  echo "Processing ${#files[@]} file(s)..."

  # Process each file
  for file in "${files[@]}"; do
    if ! push_file "$file"; then
      ERRORS+=("$file")
    fi
  done

  # Report results
  echo ""
  if [ ${#ERRORS[@]} -eq 0 ]; then
    echo "✓ Success! All ${#files[@]} flashcard file(s) pushed to Anki."
  else
    echo "⚠ Completed with errors:"
    echo "  Successfully pushed: $((${#files[@]} - ${#ERRORS[@]})) file(s)"
    echo "  Failed: ${#ERRORS[@]} file(s)"
    echo ""
    echo "Failed files:"
    for error_file in "${ERRORS[@]}"; do
      echo "  - $error_file"
    done
    exit 1
  fi
}

main
