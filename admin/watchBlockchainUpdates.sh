while true; do
  rscoin-user --log-severity Debug --bank-mode update
  rscoin-user --log-severity Debug --wallet-path wallet-db-user update
  sleep 10
done
