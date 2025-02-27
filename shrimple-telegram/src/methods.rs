use {
    crate::{types::*, IsDefault},
    shrimple_telegram_proc_macro::telegram_request,
    std::borrow::Cow,
};

#[telegram_request(response_type = True)]
pub struct AnswerCallbackQuery<'src> {
    #[telegram_request(via_into)]
    pub callback_query_id: Cow<'src, str>,
    #[telegram_request(optional, via_into)]
    pub text: Cow<'src, str>,
    #[telegram_request(optional, via_into)]
    pub show_alert: bool,
}

#[telegram_request(response_type = True)]
pub struct DeleteMessage {
    pub chat_id: ChatId,
    pub message_id: MessageId,
}

#[telegram_request(response_type = True)]
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
    pub parse_mode: Option<ParseMode>,
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

/// # Warning
///
/// If the input `document` is a stream, attempting to clone this object or turn it into a future
/// via [`crate::Request::to_future`] will result in a panic
#[telegram_request(response_type = Message)]
pub struct SendDocument<'src> {
    pub chat_id: ChatId,
    #[telegram_request(payload)]
    pub document: InputFile<'src>,
    #[telegram_request(optional, via_into)]
    pub caption: Cow<'src, str>,
    #[telegram_request(optional, via_into)]
    pub parse_mode: Option<ParseMode>,
    #[telegram_request(optional, via_into)]
    pub reply_to_message_id: Option<MessageId>,
    #[telegram_request(optional, via_into)]
    pub reply_markup: ReplyMarkup<'src>,
}

// TODO: ReplyParameters

#[telegram_request(response_type = Message)]
pub struct SendLocation<'src> {
    pub chat_id: ChatId,
    #[serde(flatten)]
    pub location: Location,
    #[telegram_request(optional, via_into)]
    pub reply_to_message_id: Option<MessageId>,
    #[telegram_request(optional, via_into)]
    pub disable_notification: Option<bool>,
    #[telegram_request(optional, via_into)]
    pub protect_content: Option<bool>,
    #[telegram_request(optional, via_into)]
    pub allow_paid_broadcast: Option<bool>,
    #[telegram_request(optional, via_into)]
    pub message_effect_id: Cow<'src, str>,
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

#[telegram_request(response_type = True)]
pub struct SetMyCommands<'src> {
    #[telegram_request(via_into)]
    pub commands: Cow<'src, [BotCommand<'src>]>,
    #[telegram_request(optional, via_into)]
    pub language_code: Cow<'src, str>,
}

#[telegram_request(response_type = True)]
pub struct SetWebhook<'src> {
    #[telegram_request(via_into)]
    pub url: Cow<'src, str>,
    #[telegram_request(optional)]
    pub drop_pending_updates: bool,
    #[telegram_request(optional, via_into)]
    pub secret_token: Cow<'src, str>,
}
