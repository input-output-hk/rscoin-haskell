if [ -z $1 ]; then
  rscoin-user --bank-mode update
  rscoin-user --bank-mode list
else
  stack exec rscoin-user -- --bank-mode update
  stack exec rscoin-user -- --bank-mode list
fi
