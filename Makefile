default: all


all: update_deps package docs man/figures/logo.png README.md build_report git_commit


## Update dependencies based on those installed
.PHONY: update_deps
update_deps:
		 Rscript -e "usethis::use_tidy_versions(overwrite = TRUE)"

#build update documents and build package
.PHONY: package
package:
		 R CMD INSTALL --no-multiarch --with-keep.source .

.PHONY: docs
docs:
     Rscript -e 'devtools::document(roclets=c('rd', 'collate', 'namespace'))'

#update logo
man/figures/logo.png: inst/scripts/generate_hex_sticker.R
		Rscript inst/scripts/generate_hex_sticker.R

#update readme
README.md: README.Rmd
		Rscript -e 'rmarkdown::render("README.Rmd")' && \
		rm README.html

#generate pknet report
.PHONY: build_report
build_report:
		cd pkgnet && make

#Commit updates
.PHONY: git_commit
git_commit:
		git add --all
		git commit -m "$(message)"
		git push
