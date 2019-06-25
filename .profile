export PATH="/Users/dmitry.dzhus/.local/bin:/Users/dmitry.dzhus/Library/Python/3.7/bin:/Library/TeX/texbin:${PATH}"
export WORKON_HOME=$HOME/.local/share/virtualenvs
gpg-agent --daemon
alias prp="pipenv run python"
alias sha256="shasum -a256"

# TODO Remove after Terraform 0.12 migration
alias terraform11="/usr/local/opt/terraform@0.11/bin/terraform"
