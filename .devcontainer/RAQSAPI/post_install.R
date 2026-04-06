# This script installs the R package dependencies neeeded for RAQSAPI

# use the R package pak instead of install.packages because pak allows
# parallel download and installation of R packages which may speed up
# devcontainer setup
options(repos = c(CRAN =
                    sprintf("https://packagemanager.posit.co/cran/latest/bin/linux/noble-%s/%s",
                            R.version["arch"],
                            substr(getRversion(), 1, 3))))
installed_pkgs <- rownames(installed.packages())
if (!"pak" %in% installed_pkgs) install.packages("pak", dependencies = TRUE)
if (!"desc" %in% installed_pkgs) pak::pkg_install("desc", dependencies = TRUE)
if (!"languageserver" %in% installed_pkgs) pak::pkg_install("languageserver")
require(desc)
require(pak)
pak::repo_add(
  CRAN = "https://cloud.r-project.org",
  ropenscireviewtools = "https://ropensci-review-tools.r-universe.dev"
)

if (file.exists("./DESCRIPTION"))
{
  desc_path <- "./DESCRIPTION"
} else if (file.exists("../DESCRIPTION"))
{
  desc_path <- "../DESCRIPTION"
}

RAQSAPI_deps <- desc::desc_get_deps(file = desc_path)[-1, "package"]
RAQSAPI_suggests <- desc::desc_get_field(key="Suggests", file=desc_path) |> strsplit(split=", ")
RAQSAPI_suggests <- RAQSAPI_suggests[[1]] |> as.list() |>
  gsub(pattern="\\s*\\([^)]*\\)", replacement="") #remove version infor which will not work with pak
installpkgs <- setdiff(c(RAQSAPI_deps, unlist(RAQSAPI_suggests)), installed_pkgs)
if (length(installpkgs) > 0) pak::pkg_install(pkg = installpkgs, dependencies = TRUE)
