mkdir wallet-db-user
stack $NIX_STACK exec rscoin-user -- --wallet-path wallet-db-user update
stack $NIX_STACK exec rscoin-user -- --wallet-path wallet-db-user list
