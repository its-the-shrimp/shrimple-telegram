use {crate::types::*, shrimple_telegram_proc_macro::telegram_request, std::borrow::Cow};

pub trait IsDefault {
    fn is_default(&self) -> bool;
}

impl<T> IsDefault for Option<T> {
    fn is_default(&self) -> bool {
        self.is_none()
    }
}

impl IsDefault for bool {
    fn is_default(&self) -> bool {
        !*self
    }
}

impl IsDefault for Cow<'_, str> {
    fn is_default(&self) -> bool {
        self.is_empty()
    }
}

impl<T> IsDefault for Cow<'_, [T]>
where
    [T]: ToOwned,
{
    fn is_default(&self) -> bool {
        self.is_empty()
    }
}

impl IsDefault for ReplyMarkup<'_> {
    fn is_default(&self) -> bool {
        matches!(self, Self::None)
    }
}

#[telegram_request(response_type = bool)]
pub struct AnswerCallbackQuery<'src> {
    pub callback_query_id: Cow<'src, str>,
    #[telegram_request(optional, via_into)]
    pub text: Cow<'src, str>,
    #[telegram_request(optional, via_into)]
    pub show_alert: bool,
}

#[telegram_request(response_type = bool)]
pub struct DeleteMessage {
    pub chat_id: ChatId,
    pub message_id: MessageId,
}

#[telegram_request(response_type = bool)]
pub struct DeleteWebhook;

#[telegram_request(response_type = Message)]
pub struct EditMessageReplyMarkup<'src> {
    pub chat_id: ChatId,
    pub message_id: MessageId,
    #[telegram_request(optional, via_into)]
    pub reply_markup: Option<InlineKeyboardMarkup<'src>>,
}

#[telegram_request(response_type = Message)]
pub struct EditMessageText<'src> {
    pub chat_id: ChatId,
    pub message_id: MessageId,
    #[telegram_request(via_into)]
    pub text: Cow<'src, str>,
    #[telegram_request(optional, via_into)]
    pub reply_markup: Option<InlineKeyboardMarkup<'src>>,
}

#[telegram_request(response_type = Message)]
pub struct ForwardMessage {
    pub chat_id: ChatId,
    pub from_chat_id: ChatId,
    pub message_id: MessageId,
}

#[telegram_request(response_type = User)]
pub struct GetMe;

#[telegram_request(response_type = Vec<Update>)]
pub struct GetUpdates<'src> {
    #[telegram_request(optional, via_into)]
    pub offset: Option<u32>,
    #[telegram_request(optional, via_into)]
    pub limit: Option<u8>,
    #[telegram_request(optional, via_into)]
    pub timeout: Option<u32>,
    #[telegram_request(optional, via_into)]
    pub allowed_updates: Cow<'src, [AllowedUpdate]>,
}

#[telegram_request(response_type = Message)]
pub struct SendAudio<'src> {
    pub chat_id: ChatId,
    pub audio: Cow<'src, str>,
    #[telegram_request(optional, via_into)]
    pub caption: Cow<'src, str>,
    #[telegram_request(optional, via_into)]
    pub reply_to_message_id: Option<MessageId>,
}

#[telegram_request(response_type = Message)]
pub struct SendDocument<'src> {
    pub chat_id: ChatId,
    pub document: Cow<'src, str>,
    #[telegram_request(optional, via_into)]
    pub caption: Cow<'src, str>,
    #[telegram_request(optional, via_into)]
    pub parse_mode: Option<ParseMode>,
    #[telegram_request(optional, via_into)]
    pub reply_to_message_id: Option<MessageId>,
    #[telegram_request(optional, via_into)]
    pub reply_markup: ReplyMarkup<'src>,
}

#[telegram_request(response_type = Message)]
pub struct SendMessage<'src> {
    pub chat_id: ChatId,
    #[telegram_request(via_into)]
    pub text: Cow<'src, str>,
    #[telegram_request(optional)]
    pub business_connection_id: Cow<'src, str>,
    #[telegram_request(optional, via_into)]
    pub parse_mode: Option<ParseMode>,
    #[telegram_request(optional)]
    pub disable_web_page_preview: bool,
    #[telegram_request(optional, via_into)]
    pub reply_to_message_id: Option<MessageId>,
    #[telegram_request(optional, via_into)]
    pub reply_markup: ReplyMarkup<'src>,
}

#[telegram_request(response_type = Message)]
pub struct SendVideo<'src> {
    pub chat_id: ChatId,
    #[telegram_request(via_into)]
    pub video: Cow<'src, str>,
    #[telegram_request(optional, via_into)]
    pub caption: Cow<'src, str>,
    #[telegram_request(optional, via_into)]
    pub reply_to_message_id: Option<MessageId>,
}

#[telegram_request(response_type = bool)]
pub struct SetMyCommands<'src> {
    pub commands: Cow<'src, [BotCommand<'src>]>,
    #[telegram_request(optional, via_into)]
    pub language_code: Cow<'src, str>,
}

#[telegram_request(response_type = bool)]
pub struct SetWebhook<'src> {
    pub url: Cow<'src, str>,
    pub drop_pending_updates: bool,
    #[telegram_request(optional, via_into)]
    pub secret_token: Cow<'src, str>,
}
