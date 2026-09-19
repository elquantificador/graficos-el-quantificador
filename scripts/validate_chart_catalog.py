#!/usr/bin/env python3
from __future__ import annotations

import csv
import re
import struct
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
    with CATALOG_PATH.open('r', encoding='utf-8', newline='') as handle:
        reader = csv.DictReader(handle)
        fieldnames = reader.fieldnames or []
        rows = list(reader)

    errors: list[str] = []
    warnings: list[str] = []
    for column in REQUIRED_COLUMNS:
        if column not in fieldnames:
            errors.append(f'Missing required column: {column}')

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

        if image_filename:
            expected_prefix = image_filename.split('_', 1)[0]
            normalized_chart_id = chart_id.replace('_', '').lower()
            normalized_prefix = expected_prefix.replace('_', '').lower()
            if image_filename[:1].isdigit() and not normalized_prefix.startswith(normalized_chart_id[:2]):
                errors.append(
                    f'Row {row_number}: Chart ID {chart_id} does not align with Image Filename {image_filename}'
                )

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
            elif not re.fullmatch(r'\d{4}-\d{2}-\d{2}', date_value):
                errors.append(
                    f'Row {row_number}: published Date must use YYYY-MM-DD: {date_value}'
                )
            if not linkedin_link:
                errors.append(f'Row {row_number}: published row missing LinkedIn Link')
            elif not re.match(r'https?://', linkedin_link):
                errors.append(
                    f'Row {row_number}: published LinkedIn Link must be an HTTP URL'
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
