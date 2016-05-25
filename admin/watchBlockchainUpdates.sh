while true; do
  stack $NIX_STACK exec rscoin-user -- --bank-mode update
  stack $NIX_STACK exec rscoin-user -- --wallet-path wallet-db-user update
  sleep 10
done
