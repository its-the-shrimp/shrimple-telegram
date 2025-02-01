use {
    serde::{
        de::{self, Visitor},
        ser::{SerializeStruct, SerializeStructVariant},
        Deserialize, Deserializer, Serialize, Serializer,
    },
    std::{borrow::Cow, fmt::Formatter, marker::PhantomData},
};

#[derive(Debug, Clone, Serialize)]
pub struct BotCommand<'src> {
    pub command: Cow<'src, str>,
    pub description: Cow<'src, str>,
}

pub type UpdateId = u64;

#[derive(Debug, Deserialize)]
pub struct Update {
    #[serde(rename = "update_id")]
    pub id: u64,
    #[serde(flatten)]
    pub kind: UpdateKind,
}

impl Update {
    pub const fn from(&self) -> Option<&User> {
        match &self.kind {
            UpdateKind::Message(m) => m.from.as_ref(),
            UpdateKind::CallbackQuery(q) => Some(&q.from),
        }
    }

    pub const fn chat(&self) -> Option<&Chat> {
        match &self.kind {
            UpdateKind::Message(m) => Some(&m.chat),
            UpdateKind::CallbackQuery(_) => None,
        }
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum UpdateKind {
    Message(Message),
    CallbackQuery(CallbackQuery),
}

pub type MessageId = i32;

#[derive(Debug, Deserialize)]
pub struct Message {
    #[serde(rename = "message_id")]
    pub id: MessageId,
    pub from: Option<User>,
    pub chat: Chat,
    #[serde(flatten)]
    pub kind: MessageKind,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum MessageKind {
    Common(MessageCommon),
}

#[derive(Debug, Deserialize)]
pub struct MessageCommon {
    pub from: Option<User>,
    pub sender_chat: Option<Chat>,
    pub reply_to_message: Option<Box<Message>>,
    #[serde(flatten)]
    pub media_kind: MediaKind,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum MediaKind {
    Text {
        text: Box<str>,
        #[serde(default)]
        entities: Box<[MessageEntity]>,
    },
    Audio {
        audio: File,
    },
    Video {
        video: File,
    },
    Photo {
        #[serde(deserialize_with = "deserialize_first")]
        photo: File,
    },
    Document {
        document: File,
    },
}

#[derive(Debug, Deserialize)]
pub struct MessageEntity {
    pub length: usize,
    pub offset: usize,
    #[serde(flatten)]
    pub kind: MessageEntityKind,
}

#[derive(Debug, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum MessageEntityKind {
    BotCommand,
    #[serde(other)]
    Other,
}

#[derive(Debug, Deserialize)]
pub struct File {
    #[serde(rename = "file_id")]
    pub id: Box<str>,
}

pub type UserId = i64;

#[derive(Debug, Deserialize)]
pub struct User {
    pub id: i64,
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

#[derive(Debug, Deserialize)]
pub struct Chat {
    pub id: ChatId,
    #[serde(flatten)]
    pub kind: ChatKind,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
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

#[derive(Debug, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum PublicChatKind {
    Channel,
    Group,
    SuperGroup,
}

fn serialize_remove_keyboard<S: Serializer>(s: S) -> Result<S::Ok, S::Error> {
    let mut s = s.serialize_struct_variant("ReplyMarkup", 2, "Remove", 1)?;
    s.serialize_field("remove_keyboard", &true)?;
    s.end()
}

#[derive(Debug, Serialize, Default)]
#[serde(untagged)]
pub enum ReplyMarkup<'src> {
    Keyboard(KeyboardMarkup<'src>),
    InlineKeyboard(InlineKeyboardMarkup<'src>),
    #[serde(serialize_with = "serialize_remove_keyboard")]
    Remove,
    ForceReply(ForceReply<'src>),
    #[default]
    #[serde(serialize_with = "Serializer::serialize_none")]
    None,
}

impl<'src> From<InlineKeyboardMarkup<'src>> for ReplyMarkup<'src> {
    fn from(value: InlineKeyboardMarkup<'src>) -> Self {
        Self::InlineKeyboard(value)
    }
}

impl<'src> From<KeyboardMarkup<'src>> for ReplyMarkup<'src> {
    fn from(value: KeyboardMarkup<'src>) -> Self {
        Self::Keyboard(value)
    }
}

impl<'src> From<ForceReply<'src>> for ReplyMarkup<'src> {
    fn from(value: ForceReply<'src>) -> Self {
        Self::ForceReply(value)
    }
}

impl ReplyMarkup<'_> {
    /// Returns `true` if the reply markup doesn't represent any kind of markup.
    pub fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }
}

#[derive(Debug, Default)]
pub struct KeyboardMarkup<'src>(pub Vec<Vec<KeyboardButton<'src>>>);

impl Serialize for KeyboardMarkup<'_> {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        let mut s = s.serialize_struct("KeyboardMarkup", 1)?;
        s.serialize_field("keyboard", &self.0)?;
        s.end()
    }
}

#[derive(Debug, Default)]
pub struct InlineKeyboardMarkup<'src>(pub Vec<Vec<InlineKeyboardButton<'src>>>);

impl Serialize for InlineKeyboardMarkup<'_> {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        let mut s = s.serialize_struct("InlineKeyboardMarkup", 1)?;
        s.serialize_field("inline_keyboard", &self.0)?;
        s.end()
    }
}

#[derive(Debug, Default)]
pub struct ForceReply<'src> {
    pub input_field_placeholder: Option<&'src str>,
}

impl Serialize for ForceReply<'_> {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        let mut s =
            s.serialize_struct("ForceReply", 1 + self.input_field_placeholder.is_some() as usize)?;
        s.serialize_field("force_reply", &true)?;
        if let Some(input_field_placeholder) = self.input_field_placeholder {
            s.serialize_field("input_field_placeholder", input_field_placeholder)?;
        }
        s.end()
    }
}

#[derive(Debug, Serialize)]
pub struct KeyboardButton<'src> {
    pub text: Cow<'src, str>,
}

impl<'src, T: Into<Cow<'src, str>>> From<T> for KeyboardButton<'src> {
    fn from(value: T) -> Self {
        Self { text: value.into() }
    }
}

#[derive(Debug, Default, Serialize)]
pub struct InlineKeyboardButton<'src> {
    pub text: Cow<'src, str>,
    #[serde(skip_serializing_if = "str::is_empty")]
    pub callback_data: Cow<'src, str>,
    #[serde(skip_serializing_if = "str::is_empty")]
    pub url: Cow<'src, str>,
}

#[derive(Debug, Serialize, Clone, Copy)]
pub enum ParseMode {
    #[serde(rename = "MarkdownV2")]
    Markdown,
    #[serde(rename = "HTML")]
    Html,
}

#[derive(Debug, Deserialize)]
pub struct CallbackQuery {
    pub id: Box<str>,
    pub from: User,
    pub data: Box<str>,
    #[serde(flatten)]
    pub message: Option<CallbackQueryMessage>,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum CallbackQueryMessage {
    Message {
        message: Message,
    },
    InlineMessage {
        #[serde(rename = "inline_message_id")]
        id: Box<str>,
    },
}

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
