#!/bin/bash

# Initialize Let's Encrypt SSL certificates for primaltechinvest.com
# Based on: https://github.com/wmnnd/nginx-certbot

set -e

domains=(www.primaltechinvest.com primaltechinvest.com)
rsa_key_size=4096
data_path="./certbot"
email="contact@primaltechinvest.com" # Change this to your email
staging=0 # Set to 1 for testing

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${GREEN}=== Let's Encrypt SSL Certificate Initialization ===${NC}"
echo -e "${YELLOW}Domains: ${domains[*]}${NC}"
echo ""

if [ -d "$data_path" ]; then
  read -p "Existing data found for ${domains[0]}. Continue and replace existing certificate? (y/N) " decision
  if [ "$decision" != "Y" ] && [ "$decision" != "y" ]; then
    exit
  fi
fi

# Download recommended TLS parameters
if [ ! -e "$data_path/conf/options-ssl-nginx.conf" ] || [ ! -e "$data_path/conf/ssl-dhparams.pem" ]; then
  echo -e "${YELLOW}### Downloading recommended TLS parameters ...${NC}"
  mkdir -p "$data_path/conf"
  curl -s https://raw.githubusercontent.com/certbot/certbot/master/certbot-nginx/certbot_nginx/_internal/tls_configs/options-ssl-nginx.conf > "$data_path/conf/options-ssl-nginx.conf"
  curl -s https://raw.githubusercontent.com/certbot/certbot/master/certbot/certbot/ssl-dhparams.pem > "$data_path/conf/ssl-dhparams.pem"
  echo -e "${GREEN}Done${NC}"
fi

# Create dummy certificate for nginx to start
echo -e "${YELLOW}### Creating dummy certificate for ${domains[0]} ...${NC}"
path="/etc/letsencrypt/live/${domains[0]}"
mkdir -p "$data_path/conf/live/${domains[0]}"
docker-compose -f docker-compose.production.yml -f docker-compose.primaltechinvest.yml run --rm --entrypoint "\
  openssl req -x509 -nodes -newkey rsa:$rsa_key_size -days 1\
    -keyout '$path/privkey.pem' \
    -out '$path/fullchain.pem' \
    -subj '/CN=localhost'" certbot
echo -e "${GREEN}Done${NC}"

# Start nginx
echo -e "${YELLOW}### Starting nginx ...${NC}"
docker-compose -f docker-compose.production.yml -f docker-compose.primaltechinvest.yml up --force-recreate -d dashboard
echo -e "${GREEN}Done${NC}"

# Delete dummy certificate
echo -e "${YELLOW}### Deleting dummy certificate for ${domains[0]} ...${NC}"
docker-compose -f docker-compose.production.yml -f docker-compose.primaltechinvest.yml run --rm --entrypoint "\
  rm -Rf /etc/letsencrypt/live/${domains[0]} && \
  rm -Rf /etc/letsencrypt/archive/${domains[0]} && \
  rm -Rf /etc/letsencrypt/renewal/${domains[0]}.conf" certbot
echo -e "${GREEN}Done${NC}"

# Request Let's Encrypt certificate
echo -e "${YELLOW}### Requesting Let's Encrypt certificate for ${domains[0]} ...${NC}"
domain_args=""
for domain in "${domains[@]}"; do
  domain_args="$domain_args -d $domain"
done

# Select appropriate email arg
case "$email" in
  "") email_arg="--register-unsafely-without-email" ;;
  *) email_arg="--email $email" ;;
esac

# Enable staging mode if needed
if [ $staging != "0" ]; then staging_arg="--staging"; fi

docker-compose -f docker-compose.production.yml -f docker-compose.primaltechinvest.yml run --rm --entrypoint "\
  certbot certonly --webroot -w /var/www/certbot \
    $staging_arg \
    $email_arg \
    $domain_args \
    --rsa-key-size $rsa_key_size \
    --agree-tos \
    --force-renewal" certbot

echo -e "${GREEN}Done${NC}"

# Reload nginx
echo -e "${YELLOW}### Reloading nginx ...${NC}"
docker-compose -f docker-compose.production.yml -f docker-compose.primaltechinvest.yml exec dashboard nginx -s reload
echo -e "${GREEN}Done${NC}"

echo ""
echo -e "${GREEN}=== SSL Certificate Installation Complete ===${NC}"
echo -e "${GREEN}Your site should now be accessible at: https://www.primaltechinvest.com${NC}"
echo ""
echo -e "${YELLOW}Note: Certificates will auto-renew every 12 hours via the certbot container${NC}"
