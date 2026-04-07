# ── Base image ────────────────────────────────────────────────────────────────
# rocker/shiny:4.4.2 = Ubuntu 22.04 (Jammy) + R 4.4.2 + Shiny Server
FROM rocker/shiny:4.4.2

# ── System libraries ──────────────────────────────────────────────────────────
# Required by arrow, curl, xml2, and text-rendering packages
RUN apt-get update -qq && \
    apt-get install -y --no-install-recommends \
      libcurl4-openssl-dev \
      libssl-dev \
      libxml2-dev \
      libfontconfig1-dev \
      libharfbuzz-dev \
      libfribidi-dev \
    && rm -rf /var/lib/apt/lists/*

# ── R package repository ──────────────────────────────────────────────────────
# Use Posit Package Manager pre-built binaries for Ubuntu 22.04 (Jammy).
# This avoids compiling arrow from C++ source, which takes 20+ minutes.
RUN echo 'options(repos = c(\
  RSPM = "https://packagemanager.posit.co/cran/__linux__/jammy/latest",\
  CRAN = "https://cloud.r-project.org"\
))' >> /usr/local/lib/R/etc/Rprofile.site

# ── Install remotes (needed for GitHub packages and install_deps) ─────────────
RUN R -e "install.packages('remotes')"

# ── Dependency layer caching trick ───────────────────────────────────────────
# Copy only DESCRIPTION first. Docker caches this layer separately, so
# re-building after code-only changes skips the slow dep install step.
COPY DESCRIPTION /app/DESCRIPTION

RUN R -e "remotes::install_deps('/app', dependencies = TRUE, upgrade = 'never')"

# ── RTL (GitHub-only package, installed after CRAN deps) ─────────────────────
# RTL is a public repo — no auth token needed for installation.
# The token is only required when GitHub API rate limits are hit.
RUN R -e "remotes::install_github('risktoollib/RTL', upgrade = 'never')"

# ── Copy the full package and install it ─────────────────────────────────────
COPY . /app
RUN R -e "remotes::install_local('/app', upgrade = 'never')"

# ── Shiny server configuration ────────────────────────────────────────────────
# Run on port 3838, accessible from outside the container
RUN echo 'local(options(shiny.port = 3838, shiny.host = "0.0.0.0"))' \
    >> /usr/local/lib/R/etc/Rprofile.site

EXPOSE 3838

CMD ["R", "-e", "GolemAppProject::run_app()"]

# Should run smoothly. Adding this to check if the github/actions works
