buccupeer
=========

[![Build Status](https://secure.travis-ci.org/aleksandr-vin/buccupeer.png)](http://travis-ci.org/aleksandr-vin/buccupeer)

*Backup buccaneer*: local service who will backup your files to your usb
drives periodically and automatically when you plug it.


Use case
--------

Create `.buccupeer` file on your *USB drive* with content like the one
below:

```erlang
buccupeer_disk_info.
[{jobs,[
        [{copies, 5},
         {dest,   "h:/backups/"},
         {src,    "c:/buccupeer/apps/buccupeer"}]
        ]}
].
```

Its meaning is that you want to:
* perform one backup job *(to the drive with this `.buccupeer` file)*
* copy directory *(or file)* `c:/buccupeer/apps/buccupeer` to `h:/backups/`
* keep 5 rotated copies of it

Don't mind the drive letter `h:` in *dest* -- it will be replaced on
the fly.

Now unplug and plug in this USB drive! Supposing you have a
*buccupeer* running, your directory `c:/buccupeer/apps/buccupeer` will
be copied to the `backups/buccupeer/copy` directory on your USB drive.

You can also check the result of your last backup by visiting
http://localhost/last-result, but it is limited in functionality for
now. Watch the aleksandr-vin/buccupeer repository for improvements and
contribute too.

*PS.* Tried replagging your USB drive again and frustrated of not
 seeing a new fresh backup copy on it? -- Maybe you have no changes in
 you backup job's source?