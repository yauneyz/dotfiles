#!/bin/bash

# Dotfiles symlink script
# This script creates symlinks for dotfiles in the home directory

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Function to create symlink with backup
create_symlink() {
    local source="$1"
    local target="$2"

    # Create parent directory if it doesn't exist
    mkdir -p "$(dirname "$target")"

    # If target exists and is not a symlink, back it up
    if [[ -e "$target" && ! -L "$target" ]]; then
        echo "Backing up existing $target to $target.backup"
        mv "$target" "$target.backup"
    fi

    # Remove existing symlink if it exists
    if [[ -L "$target" ]]; then
        rm "$target"
    fi

    # Create the symlink
    ln -s "$source" "$target"
    echo "Created symlink: $target -> $source"
}

echo "Setting up dotfiles symlinks..."

# Files to symlink
create_symlink "$DOTFILES_DIR/kitty.conf" "$HOME/.config/kitty/kitty.conf"
create_symlink "$DOTFILES_DIR/picom.conf" "$HOME/.config/picom/picom.conf"
create_symlink "$DOTFILES_DIR/.zshrc" "$HOME/.zshrc"
create_symlink "$DOTFILES_DIR/.zshrc_zac" "$HOME/.zshrc_zac"
create_symlink "$DOTFILES_DIR/.xinitrc" "$HOME/.xinitrc"
create_symlink "$DOTFILES_DIR/.vimrc" "$HOME/.vimrc"
create_symlink "$DOTFILES_DIR/.bash_aliases" "$HOME/.bash_aliases"
create_symlink "$DOTFILES_DIR/.bashrc" "$HOME/.bashrc"
create_symlink "$DOTFILES_DIR/.gitignore" "$HOME/.gitignore"
create_symlink "$DOTFILES_DIR/.p10k.zsh" "$HOME/.p10k.zsh"
create_symlink "$DOTFILES_DIR/.tmux.conf" "$HOME/.tmux.conf"
create_symlink "$DOTFILES_DIR/.Xresources" "$HOME/.Xresources"

# Directories to symlink
create_symlink "$DOTFILES_DIR/nvim" "$HOME/.config/nvim"
create_symlink "$DOTFILES_DIR/.i3" "$HOME/.config/i3"
create_symlink "$DOTFILES_DIR/.oh-my-zsh" "$HOME/.oh-my-zsh"
create_symlink "$DOTFILES_DIR/.tools" "$HOME/.tools"

echo "Dotfiles symlinks created successfully!"
