#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import difflib
import io
import re
import struct
import subprocess
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
CATALOG_PATH = REPO_ROOT / 'outputs' / 'chart_catalog' / 'chart_catalog.csv'
FIGURES_DIR = REPO_ROOT / 'outputs' / 'figures'
ALLOWED_STATUS = {'published', 'draft', 'supplementary', 'archived', 'hold'}
PNG_SIGNATURE = b'\x89PNG\r\n\x1a\n'
STANDARD_IMAGE_SIZE = (1200, 1500)
REQUIRED_COLUMNS = [
    'Chart Name',
    'Subtitle',
    'Date',
    'LinkedIn Link',
    'Image Filename',
    'Image Path',
    'Author',
    'Description',
    'Script Link',
    'Chart ID',
    'Status',
    'Series',
    'Notes',
]


CHART_ID_PATTERN = re.compile(r'^(?P<number>\d{2})(?:_(?P<letter>[a-z]))?$')
DRAFT_ID_PATTERN = re.compile(r'^draft-[a-z0-9]+(?:-[a-z0-9]+)*$')
NUMBERED_IMAGE_PATTERN = re.compile(r'^(?P<number>\d{2})(?:_(?P<letter>[a-z]))?_[^/\\]+\.png$')
DATE_PATTERN = re.compile(r'^\d{4}-\d{2}-\d{2}$')
TRIMMED_COLUMNS = [
    'Chart Name',
    'Subtitle',
    'Date',
    'LinkedIn Link',
    'Image Filename',
    'Image Path',
    'Author',
    'Script Link',
    'Chart ID',
    'Status',
    'Series',
]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description='Validate the chart catalog.')
    parser.add_argument(
        '--base-ref',
        default='',
        help=(
            'Git ref to compare against (for example origin/main). When given, '
            'the check fails if a row changes its raw text without changing any value.'
        ),
    )
    return parser.parse_args()


def check_file_bytes(raw: bytes, errors: list[str]) -> None:
    if raw.startswith(b'\xef\xbb\xbf'):
        errors.append('Catalog must be UTF-8 without BOM')
    if b'\r' in raw:
        errors.append('Catalog must use LF line endings (found CR characters)')
    if raw and not raw.endswith(b'\n'):
        errors.append('Catalog must end with a newline')


def check_chart_id_and_image(
    row_number: int, chart_id: str, status: str, image_filename: str, errors: list[str]
) -> None:
    id_match = CHART_ID_PATTERN.match(chart_id)
    if DRAFT_ID_PATTERN.match(chart_id):
        if status == 'published':
            errors.append(f'Row {row_number}: published row cannot use a draft Chart ID {chart_id}')
        if image_filename and not image_filename.startswith('draft_'):
            errors.append(
                f'Row {row_number}: draft Chart ID {chart_id} needs an Image Filename starting with draft_'
            )
        return
    if not id_match:
        errors.append(
            f'Row {row_number}: Chart ID {chart_id!r} must look like NN, NN_x or draft-slug'
        )
        return
    image_match = NUMBERED_IMAGE_PATTERN.match(image_filename)
    if not image_match:
        errors.append(
            f'Row {row_number}: Image Filename {image_filename} must look like NN_slug.png or NN_x_slug.png'
        )
        return
    if image_match['number'] != id_match['number']:
        errors.append(
            f'Row {row_number}: Chart ID {chart_id} and Image Filename {image_filename} have different numbers'
        )
    if image_match['letter'] and image_match['letter'] != id_match['letter']:
        errors.append(
            f'Row {row_number}: Chart ID {chart_id} and Image Filename {image_filename} have different letters'
        )


def split_records(text: str) -> tuple[list[str], list[dict[str, str]], list[list[int]]]:
    """Return header, parsed rows and the source line numbers of each row."""
    reader = csv.reader(io.StringIO(text))
    header = next(reader)
    rows: list[dict[str, str]] = []
    spans: list[list[int]] = []
    start = reader.line_num
    for values in reader:
        end = reader.line_num
        rows.append(dict(zip(header, values)))
        spans.append(list(range(start, end)))
        start = end
    return header, rows, spans


def check_formatting_only_changes(base_text: str, head_text: str, errors: list[str]) -> None:
    _, base_rows, _ = split_records(base_text)
    _, head_rows, head_spans = split_records(head_text)
    base_values = {tuple(row.items()) for row in base_rows}
    base_lines = base_text.split('\n')
    head_lines = head_text.split('\n')
    changed_head_lines: set[int] = set()
    matcher = difflib.SequenceMatcher(a=base_lines, b=head_lines, autojunk=False)
    for tag, _, _, j1, j2 in matcher.get_opcodes():
        if tag in ('replace', 'insert'):
            changed_head_lines.update(range(j1, j2))
    for row_index, (row, span) in enumerate(zip(head_rows, head_spans), start=2):
        if changed_head_lines.intersection(span) and tuple(row.items()) in base_values:
            chart_id = row.get('Chart ID', '')
            errors.append(
                f'Row {row_index} ({chart_id}): raw text changed but no value changed; '
                'keep the existing quoting and formatting'
            )


def read_base_catalog(base_ref: str) -> str:
    relative = CATALOG_PATH.relative_to(REPO_ROOT).as_posix()
    result = subprocess.run(
        ['git', 'show', f'{base_ref}:{relative}'],
        cwd=REPO_ROOT,
        capture_output=True,
        text=True,
        encoding='utf-8',
    )
    if result.returncode != 0:
        raise RuntimeError(f'Could not read {relative} at {base_ref}: {result.stderr.strip()}')
    return result.stdout


def read_png_dimensions(image_path: Path) -> tuple[int, int]:
    with image_path.open('rb') as handle:
        if handle.read(8) != PNG_SIGNATURE:
            raise ValueError('file does not have a valid PNG signature')
        chunk_length = handle.read(4)
        chunk_type = handle.read(4)
        if len(chunk_length) != 4 or chunk_type != b'IHDR':
            raise ValueError('file does not begin with a PNG IHDR chunk')
        dimensions = handle.read(8)
        if len(dimensions) != 8:
            raise ValueError('PNG IHDR chunk is incomplete')
        return struct.unpack('>II', dimensions)


def script_link_to_repo_path(script_link: str) -> Path | None:
    marker = '/blob/main/'
    if marker not in script_link:
        return None
    relative = script_link.split(marker, 1)[1]
    return REPO_ROOT / relative.replace('/', str(Path('/'))).lstrip(str(Path('/')))


def main() -> int:
    args = parse_args()
    raw = CATALOG_PATH.read_bytes()
    errors: list[str] = []
    warnings: list[str] = []
    check_file_bytes(raw, errors)
    text = raw.decode('utf-8-sig').replace('\r\n', '\n')

    reader = csv.DictReader(io.StringIO(text))
    fieldnames = reader.fieldnames or []
    rows = list(reader)

    for column in REQUIRED_COLUMNS:
        if column not in fieldnames:
            errors.append(f'Missing required column: {column}')
    if fieldnames != REQUIRED_COLUMNS:
        errors.append(f'Columns must be exactly, in order: {", ".join(REQUIRED_COLUMNS)}')

    if args.base_ref:
        check_formatting_only_changes(read_base_catalog(args.base_ref), text, errors)

    seen_image_filenames: dict[str, int] = {}
    seen_chart_ids: dict[str, int] = {}

    for row_number, row in enumerate(rows, start=2):
        image_filename = (row.get('Image Filename') or '').strip()
        image_path = (row.get('Image Path') or '').strip()
        chart_id = (row.get('Chart ID') or '').strip()
        status = (row.get('Status') or '').strip()
        author = (row.get('Author') or '').strip()
        description = (row.get('Description') or '')
        script_link = (row.get('Script Link') or '').strip()
        linkedin_link = (row.get('LinkedIn Link') or '').strip()
        date_value = (row.get('Date') or '').strip()

        for column in TRIMMED_COLUMNS:
            value = row.get(column) or ''
            if value != value.strip():
                errors.append(f'Row {row_number}: {column} has leading or trailing spaces')
        if date_value and not DATE_PATTERN.fullmatch(date_value):
            errors.append(f'Row {row_number}: Date must use YYYY-MM-DD: {date_value}')

        if not image_filename:
            errors.append(f'Row {row_number}: missing Image Filename')
        if not chart_id:
            errors.append(f'Row {row_number}: missing Chart ID')
        if status not in ALLOWED_STATUS:
            errors.append(f'Row {row_number}: invalid Status {status!r}')
        if author.startswith('http') or '/' in author:
            errors.append(f'Row {row_number}: Author should be plain display name only')
        if '![' in description or '](' in description:
            errors.append(f'Row {row_number}: Description contains markdown image or link syntax')
        previous_image_row = seen_image_filenames.get(image_filename)
        if previous_image_row is not None:
            errors.append(
                f'Rows {previous_image_row} and {row_number}: duplicate Image Filename {image_filename}'
            )
        elif image_filename:
            seen_image_filenames[image_filename] = row_number

        previous_chart_row = seen_chart_ids.get(chart_id)
        if previous_chart_row is not None:
            errors.append(
                f'Rows {previous_chart_row} and {row_number}: duplicate Chart ID {chart_id}'
            )
        elif chart_id:
            seen_chart_ids[chart_id] = row_number

        if chart_id and image_filename:
            check_chart_id_and_image(row_number, chart_id, status, image_filename, errors)

        if image_filename and image_path != f'outputs/figures/{image_filename}':
            errors.append(
                f'Row {row_number}: Image Path should be outputs/figures/{image_filename}'
            )

        if image_path and not (REPO_ROOT / image_path).exists():
            errors.append(f'Row {row_number}: missing published image at {image_path}')
        elif image_path:
            try:
                image_size = read_png_dimensions(REPO_ROOT / image_path)
            except ValueError as exc:
                errors.append(f'Row {row_number}: invalid PNG at {image_path}: {exc}')
            else:
                if image_size != STANDARD_IMAGE_SIZE:
                    warnings.append(
                        f'Row {row_number}: non-standard image size {image_size} at {image_path}'
                    )

        repo_script_path = script_link_to_repo_path(script_link)
        if not script_link:
            errors.append(f'Row {row_number}: missing Script Link')
        elif repo_script_path is None or not repo_script_path.exists():
            errors.append(f'Row {row_number}: Script Link does not resolve to a local file')

        if status == 'published':
            if not date_value:
                errors.append(f'Row {row_number}: published row missing Date')
            if not linkedin_link:
                errors.append(f'Row {row_number}: published row missing LinkedIn Link')
            elif not re.match(r'https?://', linkedin_link):
                errors.append(
                    f'Row {row_number}: published LinkedIn Link must be an HTTP URL'
                )
            elif not re.match(r'https?://([a-z]{2,3}\.|www\.)?linkedin\.com/', linkedin_link):
                errors.append(
                    f'Row {row_number}: published LinkedIn Link must point to linkedin.com: {linkedin_link}'
                )
            if not image_path:
                errors.append(f'Row {row_number}: published row missing Image Path')

    if errors:
        print('Catalog validation failed:', file=sys.stderr)
        for error in errors:
            print(f'- {error}', file=sys.stderr)
        return 1

    for warning in warnings:
        print(f'Warning: {warning}', file=sys.stderr)
    print(f'Catalog OK: {len(rows)} rows validated.')
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
