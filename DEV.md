App Signing
===========

First, we need to sign the app.

```
codesign --force --deep --verbose \
   --sign 'Developer ID Application: Aleksandr Artemenko (JC3CSXCTUP)' \
   --entitlements entitlements.plist \
   --options runtime \
   "Multitrainer.app"
```

Make a package installer:

```
pkgbuild --install-location /Applications --component Multitrainer.app Multitrainer.pkg

productsign --sign "Developer ID Installer: Aleksandr Artemenko" ./Multitrainer.pkg ./Multitrainer_signed.pkg
```

Then to send it to apple for notarization:

```
export USER_ID=svetlyak.40wt@some-mail-provider.com

xcrun altool --notarize-app \
   -f 'Multitrainer_signed.pkg' \
   --primary-bundle-id com.40ants.multitrainer.pkg \
   -u $USER_ID \
   --asc-provider JC3CSXCTUP \
   -p "@keychain:AC_PASSWORD"
```

It will return a code of request and it's status can be checked like that:

```
xcrun altool \
   -u $USER_ID \
   -p "@keychain:AC_PASSWORD" \
   --notarization-info 8ebcf8ea-c047-4d92-86a6-24f57c8da567
```

When app is notarized, it's sign should be stapled to archive:

```
xcrun stapler staple "Multitrainer_signed.pkg"

xcrun stapler validate  "Multitrainer_signed.pkg"
```

Links
-----

PKG signing procedure was taken from [this article](https://www.davidebarranca.com/2019/04/notarizing-installers-for-macos-catalina/).
