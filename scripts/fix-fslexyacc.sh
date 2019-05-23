NUGET_PACKAGES=$(nuget locals global-packages -list | cut -d ' ' -f 2)
FSLEXYACC_BUILD=${NUGET_PACKAGES}/fslexyacc/8.0.1/build

if [ ! -e "${FSLEXYACC_BUILD}/fslex.exe" ]
then
  cp "${FSLEXYACC_BUILD}/FsLex.exe" "${FSLEXYACC_BUILD}/fslex.exe"
  echo "Copied FsLex.exe to fslex.exe"
fi

if [ ! -e "${FSLEXYACC_BUILD}/fsyacc.exe" ]
then
  cp "${FSLEXYACC_BUILD}/FsYacc.exe" "${FSLEXYACC_BUILD}/fsyacc.exe"
  echo "Copied FsYacc.exe to fsyacc.exe"
fi
