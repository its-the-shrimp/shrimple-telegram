use {
    serde::{
        de::{self, DeserializeOwned, Error, Unexpected, Visitor},
        Deserialize, Deserializer, Serialize, Serializer,
    },
    shrimple_telegram_proc_macro::telegram_type,
    std::{borrow::Cow, fmt::Formatter, marker::PhantomData, num::NonZero},
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

#[telegram_type(copy)]
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

#[telegram_type]
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

#[telegram_type(common_fields {
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
}

pub type MessageId = NonZero<i32>;

#[telegram_type]
pub struct Message {
    #[serde(rename = "message_id")]
    pub id: MessageId,
    pub from: Option<User>,
    pub chat: Chat,
    #[serde(flatten)]
    pub kind: MessageKind,
}

#[telegram_type(untagged)]
pub enum MessageKind {
    Common(MessageCommon),
}

#[telegram_type]
pub struct MessageCommon {
    pub from: Option<User>,
    pub sender_chat: Option<Chat>,
    pub reply_to_message: Option<Box<Message>>,
    #[serde(flatten)]
    pub media_kind: MediaKind,
}

#[telegram_type(untagged)]
pub enum MediaKind {
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
}

#[telegram_type]
pub struct MessageEntity {
    pub length: usize,
    pub offset: usize,
    #[serde(flatten)]
    pub kind: MessageEntityKind,
}

#[telegram_type]
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
    #[serde(flatten)]
    pub kind: ChatKind,
}

#[telegram_type(untagged)]
pub enum ChatKind {
    Public {
        title: Option<Box<str>>,
        #[serde(flatten)]
        kind: PublicChatKind,
    },
    Private {
        username: Option<Box<str>>,
    },
}

#[telegram_type]
#[serde(tag = "type")]
pub enum PublicChatKind {
    Channel,
    Group,
    Supergroup,
}

#[telegram_type(untagged)]
pub enum ReplyMarkup<'src> {
    Keyboard(KeyboardMarkup<'src>),
    InlineKeyboard(InlineKeyboardMarkup<'src>),
    #[telegram_type(phantom_fields(remove_keyboard: True))]
    Remove,
    ForceReply(ForceReply<'src>),
    #[serde(serialize_with = "Serializer::serialize_none")]
    None,
}

#[telegram_type(name_all(keyboard))]
pub struct KeyboardMarkup<'src>(pub Vec<Vec<KeyboardButton<'src>>>);

#[telegram_type(name_all(inline_keyboard))]
#[derive(Default)]
pub struct InlineKeyboardMarkup<'src>(pub Vec<Vec<InlineKeyboardButton<'src>>>);

#[telegram_type(phantom_fields(force_reply: True))]
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

#[telegram_type]
pub struct CallbackQuery {
    pub id: Box<str>,
    pub from: User,
    pub data: Box<str>,
    #[serde(flatten)]
    pub message: Option<CallbackQueryMessage>,
}

#[telegram_type(untagged)]
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

#[telegram_type(copy)]
#[serde(transparent)]
// TODO: feature-gated parsing as a date
/// A date represented as a Unix timestamp.
pub struct Date(pub i64);

fn deserialize_first<'de, T: Deserialize<'de>, D: Deserializer<'de>>(
    deserializer: D,
) -> Result<T, D::Error> {
    struct First<T>(PhantomData<T>);

    impl<'de, T: Deserialize<'de>> Visitor<'de> for First<T> {
        type Value = T;
        fn expecting(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            f.write_str("a nonempty array")
        }

        fn visit_seq<A: de::SeqAccess<'de>>(self, mut seq: A) -> Result<Self::Value, A::Error> {
            seq.next_element()?.ok_or_else(|| serde::de::Error::invalid_length(0, &self))
        }
    }

    deserializer.deserialize_seq(First(PhantomData))
}

fn deserialize_via_try_into<'de, Medium, Out, D>(d: D) -> Result<Option<Out>, D::Error>
where
    D: Deserializer<'de>,
    Medium: DeserializeOwned + TryInto<Out>,
{
    Medium::deserialize(d).map(|x| x.try_into().ok())
}
