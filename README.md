# dotfiles

`brew install opensc git`, configure `~/.ssh/config`, clone dotfiles
```
brew install git clojure
brew cask install emacs docker karabiner-elements yubico-authenticator keepassxc
```

```
ln -s ~/projects/dotfiles/.emacs.el ~/.emacs.el
ln -s ~/projects/dotfiles/.emacs-custom.el ~/.emacs-custom.el
ln -s ~/projects/dotfiles/.gitconfig ~/.gitconfig
ln -s ~/projects/dotfiles/.globalgitignore ~/.globalgitignore
ln -s ~/projects/dotfiles/gpg-agent.conf ~/.gnupg/gpg-agent.conf
ln -s ~/projects/dotfiles/.profile ~/.profile
ln -s ~/projects/dotfiles/ssh-config ~/.ssh/config
```
