App Building
============

Use LispWorks to process `build.lisp` with `Application Builder`.

Edit `Multitrainer.app/Contents/Info.plist` and add such lines:

```
<key>CFBundleSupportedPlatforms</key>
<array>
   <string>MacOSX</string>
</array>

<key>LSApplicationCategoryType</key>
<string>public.app-category.educational-games</string>

<key>LSMinimumSystemVersion</key>
<string>10.15.0</string>
```

App Signing
===========

For AppStore
------------

First, we need to sign the app.

```
codesign --force --deep --verbose \
   --sign '3rd Party Mac Developer Application: Aleksandr Artemenko' \
   --entitlements entitlements.plist \
   --options runtime \
   "Multitrainer.app"
```

Make a package installer:

```
# pkgbuild --install-location /Applications --component Multitrainer.app Multitrainer.pkg

productbuild --component Multitrainer.app /Applications/ Multitrainer.unsigned.pkg

productsign --sign "3rd Party Mac Developer Installer: Aleksandr Artemenko" ./Multitrainer.unsigned.pkg ./Multitrainer.pkg
```

Upload it to AppStore:

```
xcrun altool --upload-app \
             -f "Multitrainer.pkg" --type osx \
             -u "$USER_ID" \
             -p "@keychain:AC_PASSWORD"
```

For distributing outside AppStore
---------------------------------

We need to apply other signatures to app and pkg.

First, we need to sign the app.

```
codesign --force --deep --verbose \
   --sign 'Developer ID Application: Aleksandr Artemenko' \
   --entitlements entitlements.plist \
   --options runtime \
   "Multitrainer.app"
```

```
productbuild --component Multitrainer.app /Applications/ Multitrainer.unsigned.pkg

productsign --sign "Developer ID Installer: Aleksandr Artemenko" ./Multitrainer.unsigned.pkg ./Multitrainer.pkg
```


Then to send it to apple for notarization:

```
export USER_ID=svetlyak.40wt@gmail.com

xcrun altool --notarize-app \
   -f 'Multitrainer.pkg' \
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
xcrun stapler staple "Multitrainer.pkg"

xcrun stapler validate  "Multitrainer.pkg"
```


Links
-----

PKG signing procedure was taken from [this article](https://www.davidebarranca.com/2019/04/notarizing-installers-for-macos-catalina/).

