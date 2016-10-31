# aleph-one-sound-unpacker

This script exports all the sounds from an Aleph One sound file to WAV
format.

It's been tested against the M1A1 and Marathon 2 sound files; it works
perfectly on the latter, but there's a couple of garbled-sounding M1A1
sounds. Not sure if that's a problem with this script or with the data.
Probably the former! (I'll investigate later.)

You can get yourself a copy of the Marathon sound files by downloading a
copy of Aleph One: https://alephone.lhowon.org/

(For the confused: The Marathon trilogy was a series of old Macintosh video
games by Bungie Software from the mid-1990s. The sounds are in old
Macintosh sound resources, in a variety of formats, packed in
Marathon-specific archive files. As such, special efforts are required if
you want to use any of the sounds from it as, say, a phone ringtone, which
is why I went to all this trouble in the first place.)

I used SBCL for this, but I expect it should work fine on other Common Lisp
implementations. (If it doesn't, patches welcome!)

References for the curious:
* Aleph One's sound loading code: https://sourceforge.net/p/marathon/code/HEAD/tree/trunk/Source_Files/Sound/SoundFile.cpp
* Inside Macintosh: Sound, chapter 2: http://mirror.informatimago.com/next/developer.apple.com/documentation/mac/pdf/Sound/Sound_Manager.pdf
* soundfile++'s WAV format documentation: http://soundfile.sapp.org/doc/WaveFormat/
