while true; do
  rscoin-user --bank-mode update
  rscoin-user --wallet-path wallet-db-user update
  sleep 10
done
