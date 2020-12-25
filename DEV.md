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

Then to zip it and to send to apple for notarization:

```
xcrun altool --notarize-app \
   -f 'Multitrainer.zip' \
   --primary-bundle-id com.40ants.multitrainer.zip 
   -u $EMAIL
   --asc-provider JC3CSXCTUP 
   -p "@keychain:AC_PASSWORD"
```

It will return a code of request and it's status can be checked like that:

```
xcrun altool \
   -u $EMAIL \
   -p "@keychain:AC_PASSWORD" \
   --notarization-info 8ebcf8ea-c047-4d92-86a6-24f57c8da567
```
