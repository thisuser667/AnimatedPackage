# AnimatedPackage
Delphi and Lazarus not windowed control for displaying all kind of images and utilities for Gif and Webp animated images. Some knowledge of any of these apps is necessary. Licenses and copyrights are those that Embarcadero, Lazarus, Microsoft or Google dispose for what I use in any case. This one superseedes the old https://github.com/thisuser667/TWebpImage

The control AnimatedNwBox (Nw = no window) descends from TGraphicControl. It displays, in Windows, wicimage supported static formats, and in Lazarus, jpeg, png and bmp files, and animates gif and webp images, both in Windows and Lazarus. 

Lazarus is checked only in Windows, but both defining and undefining Windows. It's the only platform I have, so I just don't know how it will perform in other platforms. 

It uses libwebp.dll 1.2.4 version, I don't know if I can include this file, so you'll have to seek for it how to compile or if you find it already compiled. Maybe it works in another late versions, in this case you have to change the version constant numbers in both decode124nodelay and encode124nodelay.

The classes for displaying the graphics don't descend from TGraphic, just for the sake of doing the things the more simple possible, so I don't have to override the abstract classes. But it's implemented in all of them LoadFromStream, SaveToStream, CopyToClipboard and PasteFromClipboard. In case you want, you can implement an assign and assignto procedures. I implemented the conversion I see more interesting, Gif to Webp, and also saving the frames of both formats to a directory and make a webp from images in a folder.

You can make packages from these files in both Lazarus and Delphi, but in Lazarus it uses a runtime package (bithelpers), so if you use it as a package, it won't be able to install. You'll have to use the unit bithelpers and not the package to be able to install the package.

Decoding and encoding gif files use http://www.tolderlund.eu/delphi/gifimaged2010b.zip embedded in the supported unit gifencdec.pas.
For decoding and encoding webp files it's necessary the dll libwebp.dll (see above).

Lazwiccodec and Lazwicimage are delphi's conversion to lazarus, for use it in lazarus in windows. Source code for these are not included.
