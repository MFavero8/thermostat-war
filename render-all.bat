@echo off
cd /d "%~dp0"

echo.
echo ==========================================
echo       RENDERING QUARTO PROJECT
echo ==========================================

echo.
echo [1/5] Rendering main article...
quarto render index.qmd --no-clean
if errorlevel 1 goto error

echo.
echo [2/5] Rendering model development...
quarto render supporting/model-development-and-specification.qmd --profile supp
if errorlevel 1 goto error

echo.
echo [4/5] Rendering posterior predictive checks...
quarto render supporting/posterior-predictive-checks.qmd --profile supp
if errorlevel 1 goto error

echo.
echo ==========================================
echo       ALL DOCUMENTS RENDERED!
echo ==========================================
goto end

:error
echo.
echo ERROR: Rendering failed. See messages above.

:end
pause