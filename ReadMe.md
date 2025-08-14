# Simple Log

## Description

Simple Log is a cross-platform logging framework for Delphi, that aims to be as simple as possible to use.

## Features

### Simple log calls

Simple Log provides a simple interface for logging messages, via the `Log` interface. Each method sends a log message with a specific log level, and a message.

```delphi
    Log.d // Debug level
    Log.e // Error level
    Log.f // Fatal level
    Log.i // Info level
    Log.m // Method level
    Log.w // Warning level
```

Each method takes a format string and an array of parameters, similar to the `Format` method in Delphi.

The `m` method is a special case, and is useful for tracing method calls. Use `m` at the beginning of a method, and it logs a message that the method has started. When the method exits, it will automatically log a message that the method has ended, e.g:

```delphi
procedure TMyClass.MyMethod;
begin
  // Call m at the beginning of the method to log a message that the method has started
  Log.m('MyMethod');
  // Do something in this method, here
  // When the method exists, a message will be logged that the method has ended
end;
```

### Tag support (Android only)

The `Tag` property of the `Log` interface can be used to set a tag for log messages that are sent to the system log. This is used only on Android, and is ignored on other platforms.

### Multiple providers

Classes that implement `ILogProvider` can be added to the `Log` interface using the `AddProvider` method, and messages will be sent to all providers. Simple Log includes built-in providers - see the Getting Started section for more information.

Similar to `DW.OSLog` from [Kastri](https://github.com/DelphiWorlds/Kastri) (in the Core folder), Simple Log provides logging of messages to the system log, via a built-in provider.

## Getting Started

### System logging

Simply add `SimpleLog.Log` to the uses clause of the unit(s) where you need logging, and add calls to the logging methods (described above). NOTE: Since the `FMX.Types` unit also has a reference called `Log`,if you are using that unit you will need to ensure that `SimpleLog.Log` is added to the uses clause **AFTER** `FMX.Types`.

See the [Viewing operating system logs](#viewing-operating-system-logs) section for more information on how to view the logs on your platform.

### File logging

Add a file log provider by calling `Log.AddLogProvider` with an instance of `TFileLogProvider`, e.g.:

```delphi
Log.AddLogProvider(TFileLogProvider.Create(LFileName));
```

Where `LFileName` is a string containing the path to the file where the log will be written. Calling any of the logging methods will be output via the file log provider.

TFileLogProvider also has a constructor that allows log files to be created depending on what is specified in the `TFileLogSettings` parameter, which is declared as:

```delphi
  TFileLogCreationOption = (UseSame, NewAtStart, NewOnDayChange);

  TFileLogSettings = record
  public
    class function Defaults: TFileLogSettings; static;
  public
    CreationOption: TFileLogCreationOption;
    FileNameExt: string;
    FileNameDateTimeFormat: string; // e.g. 'yyyy-mm-dd'
    Path: string;
  end;
```
The `CreationOption` property of the `TFileLogSettings` record can be set to one of the following values:

* `UseSame` - use the same file for all log messages
* `NewAtStart` - create a new file at the start of the application
* `NewOnDayChange` - create a new file when the day changes

`FileNameExt` is a string that is appended to the file name, e.g. '.log'

`FileNameDateTimeFormat` is a string that is used to format the date and time in the file name, e.g. 'yyyy-mm-dd'

`Path` is a string that is used to specify the path to the file where the log will be written.

Calling the constuctor that takes `APath` and `ASettings` parameters will populate the `Path` member of the settings with the `APath` parameter. This allows a `TFileLogProvider` to be created with a path and the default settings, like this:

```delphi
Log.AddLogProvider(TFileLogProvider.Create(LPath, TFileLogSettings.Defaults));
```
Where `LPath` is a string containing the path to the directory where the log files will be written.

The Defaults method of `TFileLogSettings` sets these values:

* `CreationOption` to `NewOnDayChange`
* `FileNameExt` to `'.log'`
* `FileNameDateTimeFormat` to `'yyyy-mm-dd'`

### SysLog logging

Simple Log was created mostly to provide sending of SysLog messages, so support for this is included.

The most basic form is to create a provider similar to how one is created for file logging. In the case of the included `TSysLogProvider`, for example:

```delphi
Log.AddLogProvider(TSysLogProvider.Create(LAddress));
```

Where `LAddress` is a string containing the address of a SysLog server. The constructor also accepts a port number, which defaults to 514 (the default for SysLog). 

#### SysLog encryption

`TSysLogProvider` also includes support for encrypting the messages being sent, however this of course means that the server side will need to handle decryption, which is included in `SysLogServer`, in the `SimpleLog.Server.SysLog` unit.

Encryption/Decryption is achieved by implementing the `ICipher` interface. The following implementations are included in Simple Log:

`TSimpleCipher` (in `SimpleLog.Cipher.Simple`) - ridiculously basic "encryption". Please just use this for testing purposes
`TOpenSSLCipher` (in `SimpleLog.Cipher.OpenSSL`) - much stronger encryption, that is dependent on OpenSSL 3.x

Alternatively you can implement your own encryption using your preferred encryption library.

Encryption/Decryption is enabled by calling the `SetCipher` method on the `ISysLogProvider` reference on the client side, and also on `SysLogServer` if this is chosen for the server side.

See also the [SysLogViewer](#syslogviewer) demo.

## SimpleLogDemo

A very basic demo application called `SimpleLogDemo` is included in the `Demos` folder, that demonstrates the use of Simple Log. It demonstrates basic logging, as well as how to create and add a `TSysLogProvider` and set the cipher for encryption.

## SysLogViewer

An application called `SysLogViewer` is included, that demonstrates the use of Simple Log to receive and display SysLog messages, including setting a cipher for decryption of messages received from clients that send encrypted messages.

The demo makes use of the `SysLogServer` reference in the `SimpleLog.Server.SysLog` unit, to act as the server. When the unit is used, the `SysLogServer` reference is created at startup. Listener classes that implement `ISysLogListener` can be added by using the `AddListener` method. The main form in the demo implements `ISysLogListener`, and displays the received messages in a grid.

## SysLogRelay

An application called `SysLogRelay` is included, that demonstrates the use of Simple Log to receive and forward SysLog messages to another SysLog server.

If your client application is sending log messages across the internet, you may wish to use encryption to secure the messages in transit. SysLogRelay could be used to receive those messages, decrypt them, and forward them on a local network to the preferred SysLog server that does not support encryption, e.g. the popular [Visual SysLog Server](https://maxbelkov.github.io/visualsyslog/).

## Viewing operating system logs

### macOS

The Console application (in the `/Applications/Utilities` folder) can be used to view logs from iOS and macOS devices. There is some more detail available about using Console [here](https://github.com/DelphiWorlds/HowTo/tree/main/Solutions/LogViewers#ios-macos).

### Windows

The recommended way to view system logs on Windows is to use DebugView, which can be downloaded from [Sysinternals](https://docs.microsoft.com/en-us/sysinternals/downloads/debugview).

### Android

Use your favorite logcat viewer. I highly recommend using [Device Lens](https://github.com/DelphiWorlds/DeviceLens) (disclaimer: I am the author)


