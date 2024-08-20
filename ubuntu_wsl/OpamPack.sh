#!/bin/bash
# Initialization

USER_PACKAGES="ocaml.4.14.2 ocamlbuild dune ocamlfind mrmime ocamlnet camlp-streams ounit2 cmdliner utop ocamlformat-rpc ocaml-lsp-server prelude"

git clone https://github.com/ocaml/opam-repository.git --depth=1



# Create an empty directory that will contain the opam files

mkdir opamroot



# Set it as the default OPAM root directory

export OPAMROOT=$PWD/opamroot



# Initialize it as an Opam repository, without creating a switch to
# prevent the installation of an OCaml compiler.

echo n \
    | opam init \
             --bare \
             --disable-sandboxing \
             --disable-shell-hook \
             --root=$PWD/opamroot \
             $PWD/opam-repository
eval $(opam env)



# Create an empty switch named =opampack=

opam switch create --empty opampack
eval $(opam env --switch=opampack)



# Find the complete list of necessary packages

PACKAGES="0install-solver angstrom astring base-bigarray base-bytes base-threads base-unix base64 bigarray-overlap bigstringaf cache camlp-streams chrome-trace cmdliner coin conf-c++ conf-pkg-config cppo csexp cudf domain-name dose3 dot-merlin-reader dune dune-build-info dune-configurator dune-rpc dyn emile extlib fiber fmt fpath host-arch-arm64 host-system-other hxd ipaddr jsonm ke lambda-term logs lwt lwt_react macaddr mccs merlin-lib mew mew_vi mrmime ocaml ocaml-base-compiler ocaml-config ocaml-lsp-server ocaml-makefile ocaml-options-vanilla ocaml-syntax-shims ocamlbuild ocamlc-loc ocamlfind ocamlformat-rpc ocamlformat-rpc-lib ocamlgraph ocamlnet ocplib-endian opam-0install-cudf opam-client opam-core opam-file-format opam-format opam-repository opam-solver opam-state ordering ounit2 pecu pp ppx_yojson_conv_lib prelude prettym ptime re react result rosetta seq sha spawn spdx_licenses stdlib-shims stdune swhid_core topkg trie uchar unstrctrd utop uucp uuseg uutf uuuu xdg yojson yuscii zed"
#ocaml.4.14.2 ocamlbuild dune.3.16.0 ocamlfind mrmime ocamlnet camlp-streams ounit2 cmdliner utop ocamlformat-rpc ocaml-lsp-server prelude
#0install-solver angstrom astring base-bigarray base-bytes base-threads base-unix base64 bigarray-overlap bigstringaf cache camlp-streams chrome-trace cmdliner coin conf-c++ conf-pkg-config cppo csexp cudf domain-name dose3 dot-merlin-reader dune dune-build-info dune-configurator dune-rpc dyn emile extlib fiber fmt fpath host-arch-arm64 host-system-other hxd ipaddr jsonm ke lambda-term logs lwt lwt_react macaddr mccs merlin-lib mew mew_vi mrmime ocaml ocaml-base-compiler ocaml-config ocaml-lsp-server ocaml-makefile ocaml-options-vanilla ocaml-syntax-shims ocamlbuild ocamlc-loc ocamlfind ocamlformat-rpc ocamlformat-rpc-lib ocamlgraph ocamlnet ocplib-endian opam-0install-cudf opam-client opam-core opam-file-format opam-format opam-repository opam-solver opam-state ordering ounit2 pecu pp ppx_yojson_conv_lib prelude prettym ptime re react result rosetta seq sha spawn spdx_licenses stdlib-shims stdune swhid_core topkg trie uchar unstrctrd utop uucp uuseg uutf uuuu xdg yojson yuscii zed

# In the repository, keep only the required packages:

cd opam-repository
mv packages packages_old
mkdir packages
for p in $PACKAGES ; do
  mv packages_old/$p packages
done

mv ../prelude.100.0 packages/prelude/

# Remove unnecessary packages and git files

rm -rf packages_old
rm -rf .git
cd ..


# Download all the required packages for the installation

opam install -y --download-only $USER_PACKAGES



# Create a script to extract the =tar.gz= file and to install the packages.

INSTALL_SCRIPT=install.sh

cat << EOF > $INSTALL_SCRIPT
#!/bin/bash
export OPAMROOT=\$PWD/opamroot
eval \$(./opam env --root=\$PWD/opamroot)
./opam install -y --assume-depexts $USER_PACKAGES
EOF
chmod +x $INSTALL_SCRIPT



# Make a =tar.gz= of all the needed files for exporting OPAM

cd ..  # back in _build
tar -zcvf opampack.tar.gz opampack
rm -rf opampack
