if hash nix-env 2>/dev/null; then
  echo "Nix package is already installed, skipping installation step"
else
  echo "Installing Nix package manager"
  curl https://nixos.org/nix/install | sh
  echo "Sourcing environment variables"
  . ${HOME}/.nix-profile/etc/profile.d/nix.sh
fi

echo "Making /etc/nix directory if needed"
sudo sh -c "mkdir /etc/nix"
echo "Setting trusted caches to /etc/nic/nix.conf"
sudo sh -c "echo 'trusted-binary-caches = https://cache.nixos.org http://hydra.cryp.to https://hydra.serokell.io' > /etc/nix/nix.conf"
echo "Registering public keys of caches in /etc/nix/nix.conf"
sudo sh -c "echo 'binary-cache-public-keys = hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.serokell.io-1:he7AKwJKKiOiy8Sau9sPcso9T/PmlVNxcnNpRgcFsps= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=' >> /etc/nix/nix.conf"
echo "Installing RSCoin"
nix-install-package --non-interactive --url https://hydra.serokell.io/build/35/nix/pkg/rscoin-0.1.0.0-x86_64-linux.nixpkg
