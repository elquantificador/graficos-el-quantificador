param(
    [Parameter(Mandatory = $true)]
    [string]$Path
)

$ErrorActionPreference = "Stop"

function Add-Finding {
    param(
        [string]$Severity,
        [string]$Rule,
        [int]$Line,
        [string]$Message
    )

    $script:Findings.Add([pscustomobject]@{
        Severity = $Severity
        Rule = $Rule
        Line = $Line
        Message = $Message
    }) | Out-Null
}

function Find-RepoRoot {
    param([string]$StartPath)

    $item = Get-Item -LiteralPath $StartPath
    $directory = if ($item.PSIsContainer) { $item } else { $item.Directory }

    while ($null -ne $directory) {
        if ((Test-Path -LiteralPath (Join-Path $directory.FullName "content")) -and
            (Test-Path -LiteralPath (Join-Path $directory.FullName ".git"))) {
            return $directory.FullName
        }
        $directory = $directory.Parent
    }

    return $null
}

function Get-LineNumber {
    param(
        [string[]]$Lines,
        [string]$Pattern
    )

    for ($index = 0; $index -lt $Lines.Count; $index++) {
        if ($Lines[$index] -match $Pattern) {
            return $index + 1
        }
    }

    return 0
}

$resolvedPath = (Resolve-Path -LiteralPath $Path).Path
$lines = @(Get-Content -LiteralPath $resolvedPath)
$Findings = [System.Collections.Generic.List[object]]::new()

if ($lines.Count -eq 0) {
    Add-Finding -Severity "ERROR" -Rule "empty-file" -Line 0 -Message "El archivo está vacío."
}

$frontmatter = ""
$bodyStartLine = 1

if ($lines.Count -gt 0 -and $lines[0] -eq "---") {
    $closing = 0
    for ($index = 1; $index -lt $lines.Count; $index++) {
        if ($lines[$index] -eq "---") {
            $closing = $index
            break
        }
    }

    if ($closing -eq 0) {
        Add-Finding -Severity "ERROR" -Rule "frontmatter" -Line 1 -Message "El frontmatter no tiene cierre."
    }
    else {
        $frontmatter = ($lines[1..($closing - 1)] -join "`n")
        $bodyStartLine = $closing + 2
    }
}
else {
    Add-Finding -Severity "ERROR" -Rule "frontmatter" -Line 1 -Message "Falta el frontmatter YAML inicial."
}

$requiredFields = @("title", "date", "authors", "categories", "description", "thumbnail")
foreach ($field in $requiredFields) {
    if ($frontmatter -notmatch "(?m)^$([regex]::Escape($field))\s*:") {
        Add-Finding -Severity "ERROR" -Rule "metadata-$field" -Line 1 -Message "Falta el campo esencial '$field'."
    }
}

$recommendedFields = @("slug", "tags", "images", "bibliography", "csl", "edition")
foreach ($field in $recommendedFields) {
    if ($frontmatter -notmatch "(?m)^$([regex]::Escape($field))\s*:") {
        Add-Finding -Severity "INFO" -Rule "metadata-$field" -Line 1 -Message "Revisar si el artículo necesita el campo '$field'."
    }
}

if ($frontmatter -match '(?m)^thumbnail\s*:\s*["'']?([^"''\r\n]+)') {
    $thumbnail = $Matches[1].Trim()
    if ($thumbnail -notmatch '^https?://') {
        $repoRoot = Find-RepoRoot -StartPath $resolvedPath
        if ($null -ne $repoRoot) {
            $relativeThumbnail = $thumbnail.TrimStart("/", "\") -replace '/', '\'
            $candidates = @(
                (Join-Path $repoRoot (Join-Path "content" $relativeThumbnail)),
                (Join-Path $repoRoot (Join-Path "static" $relativeThumbnail)),
                (Join-Path (Split-Path -Parent $resolvedPath) $relativeThumbnail)
            )
            if (-not ($candidates | Where-Object { Test-Path -LiteralPath $_ })) {
                $lineNumber = Get-LineNumber -Lines $lines -Pattern '^thumbnail\s*:'
                Add-Finding -Severity "ERROR" -Rule "thumbnail-path" -Line $lineNumber -Message "La miniatura local declarada no se encontró: $thumbnail"
            }
        }
    }
}

$patterns = @(
    @{ Severity = "ERROR"; Rule = "em-dash"; Pattern = '—'; Message = "Usar coma, punto, dos puntos o paréntesis en lugar de raya larga." },
    @{ Severity = "ERROR"; Rule = "formal-reader"; Pattern = '(?i)\busted(?:es)?\b'; Message = "Dirigirse al lector con tuteo." },
    @{ Severity = "WARN"; Rule = "ai-contrast"; Pattern = '(?i)\bno\s+solo\b.{0,160}\bsino\b'; Message = "Reescribir la fórmula 'no solo..., sino...' con una construcción directa." },
    @{ Severity = "WARN"; Rule = "ai-contrast"; Pattern = '(?i)\bno\s+se\s+trata\s+de\b.{0,160}\bsino\b'; Message = "Reescribir la fórmula 'no se trata de..., sino...' con una construcción directa." },
    @{ Severity = "WARN"; Rule = "stock-opening"; Pattern = '(?i)\b(?:en el mundo actual|hoy más que nunca|en un contexto cada vez más complejo)\b'; Message = "Sustituir la frase genérica por el problema, dato o contexto concreto." },
    @{ Severity = "WARN"; Rule = "stock-emphasis"; Pattern = '(?i)\b(?:cabe destacar|es importante destacar|como podemos observar)\b'; Message = "Nombrar directamente el hallazgo o su importancia." },
    @{ Severity = "WARN"; Rule = "generic-conclusion"; Pattern = '(?i)^\s*en conclusión[,.:]'; Message = "Comprobar si el cierre puede responder la pregunta con una síntesis más específica." },
    @{ Severity = "ERROR"; Rule = "local-path"; Pattern = '(?i)\b[A-Z]:\\Users\\'; Message = "Eliminar rutas locales visibles o sustituirlas por rutas reproducibles." }
)

for ($index = 0; $index -lt $lines.Count; $index++) {
    $line = $lines[$index]
    foreach ($check in $patterns) {
        if ($line -match $check.Pattern) {
            Add-Finding -Severity $check.Severity -Rule $check.Rule -Line ($index + 1) -Message $check.Message
        }
    }

    if ($line -match '^#{1,6}\s+(.+)$') {
        $heading = $Matches[1]
        $letters = $heading -replace '[^A-Za-zÁÉÍÓÚÜÑáéíóúüñ]', ''
        if ($letters.Length -ge 8 -and $heading -ceq $heading.ToUpperInvariant()) {
            Add-Finding -Severity "WARN" -Rule "heading-case" -Line ($index + 1) -Message "Usar sentence case en el encabezado."
        }
        if ($heading -match '^(?i:resultados|análisis|contexto|conclusión)$') {
            Add-Finding -Severity "INFO" -Rule "generic-heading" -Line ($index + 1) -Message "Considerar un encabezado que comunique el hallazgo de la sección."
        }
    }
}

$visibleBody = if ($bodyStartLine -le $lines.Count) { $lines[($bodyStartLine - 1)..($lines.Count - 1)] -join "`n" } else { "" }
if ($visibleBody -notmatch '(?i)\b(?:fuente|datos|microdatos|encuesta|registro|repositorio|bibliograf)\b') {
    Add-Finding -Severity "WARN" -Rule "data-provenance" -Line $bodyStartLine -Message "No se encontró una referencia visible a fuentes, datos o bibliografía."
}

if ($Findings.Count -eq 0) {
    Write-Output "OK: no se detectaron problemas mecánicos. La revisión editorial y cuantitativa sigue siendo necesaria."
    exit 0
}

$rank = @{ ERROR = 0; WARN = 1; INFO = 2 }
$Findings |
    Sort-Object @{ Expression = { $rank[$_.Severity] } }, Line, Rule |
    ForEach-Object {
        $location = if ($_.Line -gt 0) { "línea $($_.Line)" } else { "archivo" }
        Write-Output "[$($_.Severity)] $location [$($_.Rule)]: $($_.Message)"
    }

$errors = @($Findings | Where-Object Severity -eq "ERROR").Count
$warnings = @($Findings | Where-Object Severity -eq "WARN").Count
$info = @($Findings | Where-Object Severity -eq "INFO").Count
Write-Output "Resumen: $errors error(es), $warnings advertencia(s), $info nota(s)."

if ($errors -gt 0) {
    exit 1
}

exit 0
