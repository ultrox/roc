# Roc installation guide for x86 Linux systems

## How to install Roc
> **Note**:
> In order to develop in Roc, you need to install the Roc CLI, which includes the Roc compiler and various helpful utilities.

1. Download Latest nightly Roc (or mranually [here](https://github.com/roc-lang/roc/releases))
```sh
curl -s https://api.github.com/repos/roc-lang/roc/releases|jq -r '.[0].assets [0].browser_download_url'|xargs -n1 -I % curl -L % --output roc_nightly.tar.gz
```
2. Unpack

```sh
tar xf roc_nightly.tar.gz --one-top-level
```

## How to install Roc platform dependencies

> **Note**:
> In order to compile Roc apps (either in examples/ or in your own projects), 
> you need to install one or more of these platform language compilers, too.


<details>
<summary>
Install the Rust compiler, for apps with Rust-based platforms:
</summary>

```sh
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```
</details>


<details>
<summary>
Install the Zig compiler, for apps with Zig-based platforms:
</summary>
<br>

1. Download zig 
```sh
curl -Ls  https://ziglang.org/download/0.9.1/zig-linux-x86_64-0.9.1.tar.xz -o zig-0.9.1.tar.xz
```

2. Unpack
```sh
tar xf zig-0.9.1.tar.xz
```
3. Move To Path

```sh
cp zig-linux-x86_64-0.9.1/zig /usr/local/bin/zig
```
</details>

<details>
<summary>
Install a C compiler, for apps with C-based platforms:
</summary>

### On a Debian-based distro like Ubuntu
```sh
sudo apt update && sudo apt install build-essential clang
```

### On an RPM-based distro like Fedora
```sh
sudo dnf check-update && sudo dnf install clang
```

</details>


