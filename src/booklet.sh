#/bin/bash

set -e

if [ $# -ne 1 ] || [ "x$(file -b $1)" != "xdirectory" ]; then
  echo "Usage: $0 DIR"
  exit 1
fi

WORK=$(mktemp -d booklet.XXXXXXXX)

NAME="booklet"
DIR=$1
for FILE in $DIR/*.txt; do
  SONG="$(basename "$FILE" .txt)"
  ROOT="$WORK/$SONG"
  a2ps --margin=0 \
       --borders=0 \
       -R -1 \
       -B "$FILE" \
       -o "$ROOT.ps"
  ps2pdf "$ROOT.ps" "$ROOT.pdf"
  rm "$ROOT.ps"
done

pdfunite $WORK/*.pdf "$DIR/$NAME.pdf"
pdfbook2 "$DIR/$NAME.pdf"
mv "$DIR/$NAME-book.pdf" "$DIR/$NAME.pdf"

rm -rf "$WORK"


