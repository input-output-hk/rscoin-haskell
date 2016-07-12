paths=$@

if [[ "$paths" == "" ]]; then
  paths=wallet-db-user
fi

for i in $paths; do
  mkdir "$i"
  rscoin-user --wallet-path "$i" update
  rscoin-user --wallet-path "$i" list
done
