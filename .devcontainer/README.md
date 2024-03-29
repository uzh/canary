# Development with DevContainer

[Back to Main README](../README.md)

## VSCode setup

### Requirements

This project is setup to run in a DevContainer. Ensure requirements to run in a DevContainer:

1. [Git](https://gitlab.uzh.ch/econ/it/engineering-public/-/wikis/git) installed
1. Source Tree for GitLab setup (see [Install user interface](https://gitlab.uzh.ch/econ/it/engineering-public/-/wikis/git#install-user-interface) and [Configure GitLab for Source Tree](https://gitlab.uzh.ch/econ/it/engineering-public/-/wikis/git#configure-gitlab-for-source-tree))
1. [Docker](/Technologies/Docker) installed
1. [Visual Studio Code](https://code.visualstudio.com/) (VS Code) installed
1. VS Code Extension [Remote Container](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers) installed

Your SSH folder and Git config gets mapped to the container. You should be able to use SSH and Git inside the
container. Please ensure `~/.gitconfig` doesn't contain absolute paths (you may use the `~` profile prefix, i.e.
`excludesfile = ~/.gitignore_global`). **Please note:** You probably have to update Sourcetree settings. In its
settings "General" tab uncheck "Allow Sourcetree to modify your global Mercurial and Git configuration files".

### Start DevContainer

Click on the icon similar to "><" in the bottom left corner and select `Remote-Containers: Reopen in Container`.
If any changes were made to files in `.devcontainer` folder the Container should be rebuilt (`Remote-Containers: Rebuild Container`)

> **NOTE**: When the setup is fully installed, select `View` -> `Command Palette...` and run the command `OCaml: Restart Language Server`

## Build and run or start project

[Back to Main README](../README.md)
