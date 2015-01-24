# finagle-smtp

This is an SMTP client implementation for finagle based on 
[`RFC5321`][rfc]. The simplest guide to SMTP can be found, for example, [here][smtp2go].

[rfc]: http://tools.ietf.org/search/rfc5321
[smtp2go]: http://www.smtp2go.com/articles/smtp-protocol

## Usage

### Sending an email

The object for instantiating a client capable of sending a simple email can be obtained by calling
`Smtp.client.simple`. For services created with it the request type is `EmailMessage`, described in
[`EmailMessage.scala`][EmailMessage].

You can create an email using `DefaultEmail` class described in [`DefaultEmail.scala`][DefaultEmail]:

```scala
    val email = DefaultEmail()
      .from_("from@from.com")
      .to_("first@to.com", "second@to.com")
      .subject_("test")
      .text("text")
```

You can either create a plain text message with `DefaultEmail.text()` or a MIME message using 
`DefaultEmail.addBodyPart()` and `DefaultEmail.setBody()`. The `Mime` trait is described in 
[`Mime.scala`][Mime].

Applying the service on the email returns `Future.Done` in case of a successful operation.
In case of failure it returns the first encountered error wrapped in a `Future`.

[EmailMessage]: src/main/scala/com/twitter/finagle/smtp/EmailMessage.scala
[DefaultEmail]: src/main/scala/com/twitter/finagle/smtp/DefaultEmail.scala
[Mime]: src/main/scala/com/twitter/finagle/smtp/Mime.scala

#### Greeting and session

Upon the connection the client receives server greeting.
In the beginning of the session an greeting request is sent automatically to identify the client.
The session state is reset before every subsequent try.

#### Extensions

If the server supports `8BITMIME`, `BINARYMIME` or `SIZE` extensions, they will be used. Other 
extensions are ignored. Note that it means that for now this client can send emails only to servers that 
don't require authentication.

#### Logging

To make a simple client log the session using the `Logger` passed to the function, you should
call `Smtp.client.withLogging().simple`. Only the dialog is logged, with
no additional information.

### Example

The example of sending email to a local SMTP server with SmtpSimple and handling errors can be seen 
in [`Example.scala`](src/main/scala/com/twitter/finagle/example/smtp/Example.scala).

### Sending independent SMTP commands

The object for instantiating an SMTP client capable of sending any command defined in *RFC5321*
can be obtained by calling `Smtp.client`.

For services created with it the request type is `Request`. Command classes are described in 
[`Request.scala`][Request]. 

Replies are differentiated by groups, which are described in [`ReplyGroups.scala`][ReplyGroups].
The concrete reply types are case classes described in [`SmtpReplies.scala`][SmtpReplies].

This allows flexible error handling:

```scala
val res = service(command) onFailure {
  // An error group
  case ex: SyntaxErrorReply => log.error("Syntax error: %s", ex.info)
  ...
  // A concrete reply
  case ProcessingError(info) => log,error("Error processing request: %s", info)
  ...
  // Default
  case _ => log.error("Error!")
}
```
```scala
    // Or, another way:
    res handle {
    ...
    }
```

[Request]: src/main/scala/com/twitter/finagle/smtp/Request.scala
[ReplyGroups]: src/main/scala/com/twitter/finagle/smtp/ReplyGroups.scala
[SmtpReplies]: src/main/scala/com/twitter/finagle/smtp/SmtpReplies.scala

#### Greeting and session

Default SMTP client connects to the server, receives its greeting and replies to it. 
In case of malformed greeting the service is closed when trying to send a request.
Upon service.close() a quit command is sent automatically, if not sent earlier.

#### Extensions

You can make the client support SMTP extensions by passing their names to `Smtp.client.withSupportFor()` and
instantiating the service from the derived `Client`. Default client does not support any extensions. 
If the extension is supported by both client and server (and also implemented - see *Using extensions*), 
it will be able to be used.

#### Logging

You can log the session using `SmtpLoggingFilter` described in [`SmtpLoggingFilter.scala`][logging]

[logging]: src/main/scala/com/twitter/finagle/smtp/filter/SmtpLoggingFilter.scala

### Using extensions

Each implemented extension has its own package in [extension package][extension]. Currently supported
extensions are: `AUTH`, `BINARYMIME`, `CHUNKING`, `8BITMIME`, `EXPN` (which is actually an extension 
according to *RFC5321*), `PIPELINING` and `SIZE`. Some of these extensions require adding the parameters
to `MAIL FROM` command, which you can achieve by sending `ExtendedMailingSession` described in 
[`ExtendedMailingSession.scala`][ExtMailingSession] instead of `Request.NewMailingSession`

[extension]: src/main/scala/com/twitter/finagle/smtp/extension
[ExtMailingSession]: src/main/scala/com/twitter/finagle/smtp/extension/ExtendedMailingSession.scala

#### AUTH 

This extension is described in the [auth][extauth] package. It lets you send `AuthRequest` to 
perform authentication with some `AuthMechanism` and receive `AuthReply`s, and also to authorize the 
email message using `ExtendedMailingSession.authorize()`. For example:

```scala
    // Sending authentication request
    val mechanism = AuthMechanism.plain("login", "password")
    service(AuthRequest(mechanism)) flatMap {
      case ch@ServerChallenge(_) => service(mechanism.reply(ch))
      ...
    } handle {
      case AuthRejected(reason) => log.info("You cannot pass!")
    }
```
```scala
    // Authorizing email message
    val sender = MailingAddress("sender@example.org")
    val authorizedSender = MailingAddress("authorized@example.org")
    service(ExtendedMailingSession(sender).authorize(authorizedSender))
```

Note that for now there is no API for using more sophisticated SASL mechanisms.

[extauth]: src/main/scala/com/twitter/finagle/smtp/extension/auth

#### CHUNKING 

This extension is described in the [chunking][extchunk] package. It lets you send `BeginDataChunk` 
and `BeginLastDataChunk` commands instead of `Request.BeginData`. For example: 

```scala
    service(BeginDataChunk(120))
    // Sending 120 bytes...
    ...
    service(BeginLastDataChunk(50))
    // Sending last 50 bytes...
```

[extchunk]: src/main/scala/com/twitter/finagle/smtp/extension/chunking

#### BINARYMIME 

This extension is described in the [binarymime][extbmime] package. It lets you specify binary body 
encoding using `ExtendedMailingSession.bodyEncoding()` with `BodyEncoding.Binary`. Note that this 
extension only works with `CHUNKING` extension support. For example:

```scala
    val sender = MailingAddress("sender@example.org")
    service(ExtendedMailingSession(sender).bodyEncoding(BodyEncoding.Binary))
    ...
    service(BeginDataChunk(120))
    // Sending binary data...
```

Without this extension any binary data will be rejected.

[extbmime]: src/main/scala/com/twitter/finagle/smtp/extension/binarymime

#### 8BITMIME 

This extension is described in the [eightbitmime][ext8mime] package. It lets you specify 8bit body 
encoding using `ExtendedMailingSession.bodyEncoding()` with `BodyEncoding.EightBit`. For example:

```scala
    val sender = MailingAddress("sender@example.org")
    service(ExtendedMailingSession(sender).bodyEncoding(BodyEncoding.EightBit))
    ...
    service(Request.BeginData)
    // Sending 8bit data...
```

Without this extension all 8-bit data will be either rejected (if it is MIME data) or converted to 
7-bit (if it is plain text).

[ext8mime]: src/main/scala/com/twitter/finagle/smtp/extension/eightbitmime

#### EXPN 

This extension allows you to send request to expand a 
mailing list. For example:

```scala
    val list = MailingAddress("mailing-list@example.org")
    service(Request.ExpandMailingList(list)) onSuccess {
      case ok@OK(_) => ok.lines foreach log.info
      ...
    }
```

#### PIPELINING 

This extension is described in the [pipelining][extpipe] package. It lets you send groups of 
requests using `RequestGroup` without waiting individually for each of them, but receiving replies 
batched in `GroupedReply`. For example:

```scala
    val sender = MailingAddress("sender@example.org")
    val group = RequestGroup(Request.NewMailingSession(sender), Request.Quit)
    service(group) onSuccess {
      case GroupedReply(reps) => reps foreach log.info(_.info) 
      ...
    }
```

Note that, according to [RFC4954][rfcpipe], the following commands can appear 
only in the end of the group:

* Greeting commands
* Commands indicating that data is going to be sent
* Address debugging commands (verifying mailboxes and expanding mailing lists)
* Quit request
* Noop request
* Authentication request 
* the commands coming from other extensions, for which it is specified.

[extpipe]: src/main/scala/com/twitter/finagle/smtp/extension/pipelining
[rfcpipe]: http://tools.ietf.org/html/rfc4954

#### SIZE 

This extension is described in the [size][extsize] package. It lets you specify message size 
using `ExtendedMailingSession.messageSize()` to make sure that the server can or can not accept the 
message of such size. For example:

```scala
    val sender = MailingAddress("sender@example.org")
    service(ExtendedMailingSession(sender).messageSize(25000)) handle {
      case InsufficientStorageError(_) => log.info("Can't place these damn bytes anywhere!")
      ...
    }
```

[extsize]: src/main/scala/com/twitter/finagle/smtp/extension/size



