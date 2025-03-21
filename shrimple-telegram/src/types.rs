use {
    crate::{Multipart, Result, PAYLOAD_LINK},
    bytes::{Bytes, BytesMut},
    reqwest::{multipart::Part, Body},
    serde::{
        de::{DeserializeOwned, Error, Unexpected},
        Deserialize, Deserializer, Serialize, Serializer,
    },
    shrimple_telegram_proc_macro::telegram_type,
    std::{
        borrow::Cow,
        fmt::{Debug, Formatter},
        io,
        mem::{replace, take},
        num::NonZero,
        sync::Mutex,
    },
    tokio::io::{AsyncRead, AsyncReadExt},
    tokio_util::io::ReaderStream,
};

/// a boolean that is always `true`, useful for correct (de)serialization
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct True;

impl<'de> Deserialize<'de> for True {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        if !bool::deserialize(d)? {
            return Err(D::Error::invalid_value(Unexpected::Bool(false), &"`true`"));
        }
        Ok(Self)
    }
}

impl Serialize for True {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        true.serialize(s)
    }
}

#[telegram_type]
pub struct BotCommand<'src> {
    pub command: Cow<'src, str>,
    pub description: Cow<'src, str>,
}

pub type UpdateId = u64;

#[telegram_type(copy, no_doc)]
pub enum AllowedUpdate {
    CallbackQuery,
    ChannelPost,
    ChatBoost,
    ChatJoinRequest,
    ChatMember,
    ChosenInlineResult,
    EditedChannelPost,
    EditedMessage,
    InlineQuery,
    Message,
    MessageReaction,
    MessageReactionCount,
    MyChatMember,
    Poll,
    PollAnswer,
    PreCheckoutQuery,
    RemovedChatBoost,
    ShippingQuery,
}

#[telegram_type(no_eq)]
pub struct Update {
    #[serde(rename = "update_id")]
    pub id: u64,
    #[serde(flatten)]
    pub kind: UpdateKind,
}

impl Update {
    pub fn from(&self) -> Option<&User> {
        self.kind.from()
    }

    pub fn chat(&self) -> Option<&Chat> {
        self.kind.chat()
    }
}

#[telegram_type(no_doc, no_eq, common_fields {
    #[optional] from: User,
    #[optional] chat: Chat,
})]
pub enum UpdateKind {
    #[telegram_type(nested_fields(from = &field0.from))]
    CallbackQuery(CallbackQuery),
    #[telegram_type(nested_fields(from = field0.from.as_ref()?, chat = &field0.chat))]
    ChannelPost(Message),
    #[telegram_type(nested_fields(chat = &field0.chat))]
    ChatBoost(ChatBoostUpdated),
    #[telegram_type(nested_fields(from = field0.from.as_ref()?, chat = &field0.chat))]
    Message(Message),
    #[serde(other)]
    Other,
}

pub type MessageId = NonZero<i32>;

#[telegram_type(no_eq)]
pub struct Message {
    #[serde(rename = "message_id")]
    pub id: MessageId,
    pub from: Option<User>,
    pub chat: Chat,
    pub date: Date,
    #[serde(flatten)]
    pub kind: MessageKind,
}

#[telegram_type(untagged, no_eq, no_doc)]
pub enum MessageKind {
    Common(MessageCommon),
}

#[telegram_type(no_eq, no_doc)]
pub struct MessageCommon {
    pub from: Option<User>,
    pub sender_chat: Option<Chat>,
    pub reply_to_message: Option<Box<Message>>,
    #[serde(flatten)]
    pub media: Media,
}

#[telegram_type(untagged, no_eq, no_doc)]
pub enum Media {
    Text {
        text: Box<str>,
        #[serde(default)]
        entities: Box<[MessageEntity]>,
    },
    #[telegram_type(name_all(audio))]
    Audio(File),
    #[telegram_type(name_all(video))]
    Video(File),
    #[telegram_type(name_all(photo))]
    Photo(Vec<File>),
    #[telegram_type(name_all(document))]
    Document(File),
    #[telegram_type(name_all(location))]
    Location(Location),
}

#[telegram_type]
pub struct MessageEntity {
    pub length: usize,
    pub offset: usize,
    #[serde(flatten)]
    pub kind: MessageEntityKind,
}

#[telegram_type(no_doc)]
#[serde(tag = "type")]
pub enum MessageEntityKind {
    BotCommand,
    #[serde(other)]
    Other,
}

#[telegram_type]
pub struct File {
    #[serde(rename = "file_id")]
    pub id: Box<str>,
}

pub type UserId = i64;

#[telegram_type]
pub struct User {
    pub id: UserId,
    pub is_bot: bool,
    pub first_name: Box<str>,
    pub last_name: Option<Box<str>>,
    pub username: Option<Box<str>>,
    pub language_code: Option<Box<str>>,
}

impl User {
    pub fn full_name(&self) -> String {
        format!("{} {}", self.first_name, self.last_name.as_deref().unwrap_or_default())
    }

    pub fn username_or_full_name(&self) -> String {
        self.username.as_deref().map_or_else(|| self.full_name(), |x| format!("@{x}"))
    }
}

pub type ChatId = i64;

#[telegram_type]
pub struct Chat {
    pub id: ChatId,
    pub username: Option<Box<str>>,
    #[serde(flatten)]
    pub kind: ChatKind,
}

#[telegram_type(no_doc)]
#[serde(tag = "type")]
pub enum ChatKind {
    Private {
        first_name: Box<str>,
        last_name: Option<Box<str>>,
    },
    Group {
        title: Box<str>,
    },
    Supergroup {
        title: Box<str>,
        #[serde(default)]
        is_forum: bool,
    },
    Channel {
        title: Box<str>,
    },
}

#[telegram_type(untagged, no_doc)]
pub enum ReplyMarkup<'src> {
    Keyboard(KeyboardMarkup<'src>),
    InlineKeyboard(InlineKeyboardMarkup<'src>),
    #[telegram_type(phantom_fields { remove_keyboard: True })]
    Remove,
    ForceReply(ForceReply<'src>),
    #[serde(serialize_with = "Serializer::serialize_none")]
    None,
}

#[telegram_type(name_all(keyboard))]
pub struct KeyboardMarkup<'src>(pub Vec<Vec<KeyboardButton<'src>>>);

impl<'src, A: IntoIterator<Item = KeyboardButton<'src>>> FromIterator<A> for KeyboardMarkup<'src> {
    fn from_iter<T: IntoIterator<Item = A>>(iter: T) -> Self {
        Self(Vec::from_iter(iter.into_iter().map(Vec::from_iter)))
    }
}

impl<'src> KeyboardMarkup<'src> {
    /// Collects buttons into a keyboard where every row has exactly 1 button 
    pub fn from_rows(iter: impl IntoIterator<Item = KeyboardButton<'src>>) -> Self {
        Self(iter.into_iter().map(|x| vec![x]).collect())
    }
}

#[telegram_type(name_all(inline_keyboard))]
#[derive(Default)]
pub struct InlineKeyboardMarkup<'src>(pub Vec<Vec<InlineKeyboardButton<'src>>>);

impl<'src, A: IntoIterator<Item = InlineKeyboardButton<'src>>> FromIterator<A> for InlineKeyboardMarkup<'src> {
    fn from_iter<T: IntoIterator<Item = A>>(iter: T) -> Self {
        Self(Vec::from_iter(iter.into_iter().map(Vec::from_iter)))
    }
}

impl<'src> InlineKeyboardMarkup<'src> {
    /// Collects buttons into a keyboard where every row has exactly 1 button 
    pub fn from_rows(iter: impl IntoIterator<Item = InlineKeyboardButton<'src>>) -> Self {
        Self(iter.into_iter().map(|x| vec![x]).collect())
    }
}

#[telegram_type(phantom_fields { force_reply: True })]
#[derive(Default)]
pub struct ForceReply<'src> {
    pub input_field_placeholder: Option<Cow<'src, str>>,
}

#[telegram_type]
pub struct KeyboardButton<'src> {
    pub text: Cow<'src, str>,
}

impl<'src, T: Into<Cow<'src, str>>> From<T> for KeyboardButton<'src> {
    fn from(value: T) -> Self {
        Self { text: value.into() }
    }
}

#[telegram_type]
pub struct InlineKeyboardButton<'src> {
    pub text: Cow<'src, str>,
    #[serde(skip_serializing_if = "str::is_empty")]
    pub callback_data: Cow<'src, str>,
    #[serde(skip_serializing_if = "str::is_empty")]
    pub url: Cow<'src, str>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)]
pub enum ParseMode {
    #[serde(rename = "MarkdownV2")]
    Markdown,
    #[serde(rename = "HTML")]
    Html,
}

#[telegram_type(no_eq)]
pub struct CallbackQuery {
    pub id: Box<str>,
    pub from: User,
    pub data: Box<str>,
    #[serde(flatten)]
    pub message: Option<CallbackQueryMessage>,
}

// TODO: replace with MaybeInaccessibleMessage
#[telegram_type(no_eq, untagged, no_doc)]
pub enum CallbackQueryMessage {
    Message {
        message: Message,
    },
    InlineMessage {
        #[serde(rename = "inline_message_id")]
        id: Box<str>,
    },
}

#[telegram_type]
pub struct ChatBoostUpdated {
    pub chat: Chat,
    pub boost: ChatBoost,
}

#[telegram_type]
pub struct ChatBoost {
    #[serde(rename = "boost_id")]
    pub id: Box<str>,
    pub add_date: Date,
    pub expiration_date: Date,
    pub source: ChatBoostSource,
}

#[telegram_type]
#[serde(tag = "source")]
pub enum ChatBoostSource {
    GiftCode {
        user: User,
    },
    Giveaway {
        user: User,
        #[serde(deserialize_with = "deserialize_via_try_into::<i32, _, _>")]
        giveaway_message_id: Option<MessageId>,
        #[serde(default)]
        prize_star_count: u32,
        #[serde(default)]
        is_unclaimed: bool,
    },
    Premium {
        user: User,
    },
}

#[telegram_type(copy, no_doc)]
#[serde(transparent)]
// TODO: feature-gated parsing as a date
/// A date represented as a Unix timestamp.
pub struct Date(pub i64);

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(transparent)]
pub struct InputFile<'src> {
    kind: InputFileKind<'src>,
    #[serde(skip_serializing)]
    file_name: Option<Cow<'src, str>>,
}

impl Multipart for InputFile<'_> {
    fn get_payload(&self) -> Option<Part> {
        match &self.kind {
            InputFileKind::UrlOrFileId(_) => None,
            InputFileKind::Payload(payload) => {
                let bytes = payload.get_shared();
                let bytes_len = bytes.len();
                let mut res = Part::stream_with_length(bytes, bytes_len as u64);
                if let Some(filename) = self.file_name.as_deref() {
                    res = res.file_name(filename.to_owned());
                }
                Some(res)
            }
        }
    }

    fn get_payload_mut(&mut self) -> Option<Part> {
        match &mut self.kind {
            InputFileKind::UrlOrFileId(_) => None,
            InputFileKind::Payload(payload) => {
                let mut res =
                    match replace(payload, Payload(Mutex::new(PayloadRepr::Owned(vec![]))))
                        .into_body()
                    {
                        (Some(len), body) => Part::stream_with_length(body, len as u64),
                        (None, body) => Part::stream(body),
                    };
                if let Some(filename) = self.file_name.take() {
                    res = res.file_name(filename.into_owned());
                }
                Some(res)
            }
        }
    }
}

impl<'src> InputFile<'src> {
    /// Creates an input file that points to a resource on the internet that Telegram should fetch.
    pub fn url(url: impl Into<Cow<'src, str>>) -> Self {
        Self { kind: InputFileKind::UrlOrFileId(url.into()), file_name: None }
    }

    /// Creates an input file that points to a file stored by Telegram by its ID
    pub fn file_id(file_id: impl Into<Cow<'src, str>>) -> Self {
        Self { kind: InputFileKind::UrlOrFileId(file_id.into()), file_name: None }
    }

    /// Creates an input file that's represented by bytes in memory. The ownership of the bytes is
    /// transferred to the file.
    pub fn memory(vec: Vec<u8>) -> Self {
        Self { kind: InputFileKind::Payload(PayloadRepr::Owned(vec).into()), file_name: None }
    }

    /// Creates an input file that's represented by a shared span of bytes in memory.
    pub fn shared_memory(bytes: Bytes) -> Self {
        Self { kind: InputFileKind::Payload(PayloadRepr::Shared(bytes).into()), file_name: None }
    }

    /// Creates an input file that's represented by an async reader
    pub fn reader(reader: impl AsyncRead + Send + Sync + Unpin + 'static) -> Self {
        Self {
            kind: InputFileKind::Payload(PayloadRepr::Reader(Box::new(reader)).into()),
            file_name: None,
        }
    }

    /// Creates an input file from a span of text, either owned or static
    pub fn text(text: impl Into<Cow<'static, str>>) -> Self {
        match text.into() {
            Cow::Owned(text) => Self::memory(text.into_bytes()),
            Cow::Borrowed(text) => Self::shared_memory(Bytes::from_static(text.as_bytes())),
        }
    }

    /// Sets the name of the input file with which it should be labelled in Telegram
    pub fn file_name(self, file_name: impl Into<Cow<'src, str>>) -> Self {
        Self { file_name: Some(file_name.into()), ..self }
    }

    /// Makes the input file reusable by loading a reader into memory, if this input file is backed
    /// by one. After calling this function, the input file can be cloned and
    /// [`to_future`](crate::Request::to_future) can be called on the containing request.
    ///
    /// Should be called if the file could be backed by a reader and the containing request could be
    /// sent more than once.
    pub async fn reusable(self) -> Result<Self> {
        Ok(match self.kind {
            InputFileKind::UrlOrFileId(_) => self,
            InputFileKind::Payload(p) => Self {
                kind: InputFileKind::Payload(p.0.into_inner().unwrap().reusable().await?.into()),
                ..self
            },
        })
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(untagged)]
enum InputFileKind<'src> {
    UrlOrFileId(Cow<'src, str>),
    #[serde(serialize_with = "serialize_payload")]
    Payload(Payload),
}

fn serialize_payload<S: Serializer>(_: &'_ Payload, s: S) -> Result<S::Ok, S::Error> {
    s.serialize_str(PAYLOAD_LINK)
}

struct Payload(Mutex<PayloadRepr>);

impl From<PayloadRepr> for Payload {
    fn from(repr: PayloadRepr) -> Self {
        Self(Mutex::new(repr))
    }
}

impl PartialEq for Payload {
    fn eq(&self, other: &Self) -> bool {
        let (Ok(lhs), Ok(rhs)) = (self.0.try_lock(), other.0.try_lock()) else {
            return false;
        };
        match (&*lhs, &*rhs) {
            (PayloadRepr::Shared(b1), PayloadRepr::Shared(b2)) => b1 == b2,
            (PayloadRepr::Owned(v1), PayloadRepr::Owned(v2)) => v1 == v2,
            _ => false,
        }
    }
}

impl Debug for Payload {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Payload").finish_non_exhaustive()
    }
}

impl Clone for Payload {
    fn clone(&self) -> Self {
        Self(Mutex::new(PayloadRepr::Shared(self.0.lock().unwrap().make_shared())))
    }
}

impl From<Payload> for Body {
    fn from(payload: Payload) -> Self {
        match payload.0.into_inner().unwrap() {
            PayloadRepr::Shared(bytes) => bytes.into(),
            PayloadRepr::Owned(vec) => vec.into(),
            PayloadRepr::Reader(reader) => Body::wrap_stream(ReaderStream::new(reader)),
        }
    }
}

impl Payload {
    fn get_shared(&self) -> Bytes {
        self.0.lock().unwrap().make_shared()
    }

    /// Returns the body and, optionally, its length
    fn into_body(self) -> (Option<usize>, Body) {
        match self.0.into_inner().unwrap() {
            PayloadRepr::Shared(bytes) => (bytes.len().into(), bytes.into()),
            PayloadRepr::Owned(vec) => (vec.len().into(), vec.into()),
            PayloadRepr::Reader(reader) => (None, Body::wrap_stream(ReaderStream::new(reader))),
        }
    }
}

enum PayloadRepr {
    Shared(Bytes),
    Owned(Vec<u8>),
    Reader(Box<dyn AsyncRead + Send + Sync + Unpin>),
}

impl PayloadRepr {
    /// # Panics
    /// Panics if `self` is `Reader`.
    fn make_shared(&mut self) -> Bytes {
        match self {
            PayloadRepr::Shared(b) => b.clone(),
            PayloadRepr::Owned(vec) => {
                let bytes = Bytes::from(take(vec));
                *self = PayloadRepr::Shared(bytes.clone());
                bytes
            }
            PayloadRepr::Reader(_) => {
                panic!("PayloadRepr::make_shared: can't extract bytes from an async reader")
            }
        }
    }

    async fn reusable(self) -> io::Result<Self> {
        Ok(Self::Shared(match self {
            PayloadRepr::Shared(bytes) => bytes,
            PayloadRepr::Owned(vec) => vec.into(),
            PayloadRepr::Reader(mut reader) => {
                let mut buf = BytesMut::new();
                reader.read_buf(&mut buf).await?;
                buf.freeze()
            }
        }))
    }
}

#[telegram_type(no_eq, copy)]
pub struct Location {
    pub latitude: f64,
    pub longitude: f64,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub horizontal_accuracy: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub live_period: Option<u32>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub heading: Option<u16>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub proximity_alert_radius: Option<u32>,
}

fn deserialize_via_try_into<'de, Medium, Out, D>(d: D) -> Result<Option<Out>, D::Error>
where
    D: Deserializer<'de>,
    Medium: DeserializeOwned + TryInto<Out>,
{
    Medium::deserialize(d).map(|x| x.try_into().ok())
}
