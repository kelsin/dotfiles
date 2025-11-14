# vcpkg
# git clone https://github.com/microsoft/vcpkg.git ~/src/vcpkg
if [ -d ~/src/vcpkg ]; then
    export VCPKG_ROOT="${HOME}/src/vcpkg"
    export PATH="${VCPKG_ROOT}:${PATH}"
fi
