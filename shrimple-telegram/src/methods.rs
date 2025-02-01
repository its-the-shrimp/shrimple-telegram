use {crate::{types::*, Bot}, serde::Serialize, shrimple_telegram_proc_macro::Request, std::borrow::Cow};

#[derive(Serialize, Request)]
#[shrimple_telegram(response_type = bool)]
pub struct AnswerCallbackQuery<'src> {
    #[serde(skip_serializing)]
    bot: Bot,
    pub callback_query_id: Cow<'src, str>,
    #[shrimple_telegram(optional, via_into)]
    #[serde(skip_serializing_if = "str::is_empty")]
    pub text: Cow<'src, str>,
    #[shrimple_telegram(optional, via_into)]
    #[serde(skip_serializing_if = "std::ops::Not::not")]
    pub show_alert: bool,
}

#[derive(Serialize, Request)]
#[shrimple_telegram(response_type = bool)]
pub struct SetWebhook<'src> {
    #[serde(skip_serializing)]
    bot: Bot,
    pub url: Cow<'src, str>,
    pub drop_pending_updates: bool,
    #[shrimple_telegram(optional, via_into)]
    #[serde(skip_serializing_if = "str::is_empty")]
    pub secret_token: Cow<'src, str>,
}

#[derive(Serialize, Request)]
#[shrimple_telegram(response_type = bool)]
pub struct SetMyCommands<'src> {
    #[serde(skip_serializing)]
    bot: Bot,
    pub commands: Cow<'src, [BotCommand<'src>]>,
    #[shrimple_telegram(optional, via_into)]
    #[serde(skip_serializing_if = "str::is_empty")]
    pub language_code: Cow<'src, str>,
}

#[derive(Serialize, Request)]
#[shrimple_telegram(response_type = Message)]
pub struct SendMessage<'src> {
    #[serde(skip_serializing)]
    bot: Bot,

    pub chat_id: ChatId,
    #[shrimple_telegram(via_into)]
    pub text: Cow<'src, str>,
    #[shrimple_telegram(optional)]
    pub business_connection_id: Cow<'src, str>,
    #[shrimple_telegram(optional, via_into)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parse_mode: Option<ParseMode>,
    #[shrimple_telegram(optional)]
    #[serde(skip_serializing_if = "std::ops::Not::not")]
    pub disable_web_page_preview: bool,
    #[shrimple_telegram(optional, via_into)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reply_to_message_id: Option<MessageId>,
    #[shrimple_telegram(optional, via_into)]
    #[serde(skip_serializing_if = "ReplyMarkup::is_none")]
    pub reply_markup: ReplyMarkup<'src>,
}

#[derive(Serialize, Request)]
#[shrimple_telegram(response_type = Message)]
pub struct SendAudio<'src> {
    #[serde(skip_serializing)]
    bot: Bot,
    pub chat_id: ChatId,
    pub audio: Cow<'src, str>,
    #[shrimple_telegram(optional, via_into)]
    #[serde(skip_serializing_if = "str::is_empty")]
    pub caption: Cow<'src, str>,
    #[shrimple_telegram(optional, via_into)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reply_to_message_id: Option<MessageId>,
}

#[derive(Serialize, Request)]
#[shrimple_telegram(response_type = Message)]
pub struct SendVideo<'src> {
    #[serde(skip_serializing)]
    bot: Bot,
    pub chat_id: ChatId,
    #[shrimple_telegram(via_into)]
    pub video: Cow<'src, str>,
    #[shrimple_telegram(optional, via_into)]
    #[serde(skip_serializing_if = "str::is_empty")]
    pub caption: Cow<'src, str>,
    #[shrimple_telegram(optional, via_into)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reply_to_message_id: Option<MessageId>,
}

#[derive(Serialize, Request)]
#[shrimple_telegram(response_type = Message)]
pub struct SendDocument<'src> {
    #[serde(skip_serializing)]
    bot: Bot,
    pub chat_id: ChatId,
    pub document: Cow<'src, str>,
    #[shrimple_telegram(optional, via_into)]
    #[serde(skip_serializing_if = "str::is_empty")]
    pub caption: Cow<'src, str>,
    #[shrimple_telegram(optional, via_into)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parse_mode: Option<ParseMode>,
    #[shrimple_telegram(optional, via_into)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reply_to_message_id: Option<MessageId>,
    #[shrimple_telegram(optional, via_into)]
    #[serde(skip_serializing_if = "ReplyMarkup::is_none")]
    pub reply_markup: ReplyMarkup<'src>,
}

#[derive(Serialize, Request)]
#[shrimple_telegram(response_type = Message)]
pub struct EditMessageText<'src> {
    #[serde(skip_serializing)]
    bot: Bot,
    pub chat_id: ChatId,
    pub message_id: MessageId,
    pub text: Cow<'src, str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reply_markup: Option<InlineKeyboardMarkup<'src>>,
}

#[derive(Serialize, Request)]
#[shrimple_telegram(response_type = Message)]
pub struct EditMessageReplyMarkup<'src> {
    #[serde(skip_serializing)]
    bot: Bot,
    pub chat_id: ChatId,
    pub message_id: MessageId,
    #[shrimple_telegram(optional, via_into)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reply_markup: Option<InlineKeyboardMarkup<'src>>,
}

#[derive(Serialize, Request)]
#[shrimple_telegram(response_type = Message)]
pub struct ForwardMessage {
    #[serde(skip_serializing)]
    bot: Bot,
    pub chat_id: ChatId,
    pub from_chat_id: ChatId,
    pub message_id: MessageId,
}

#[derive(Serialize, Request)]
#[shrimple_telegram(response_type = bool)]
pub struct DeleteMessage {
    #[serde(skip_serializing)]
    bot: Bot,
    pub chat_id: ChatId,
    pub message_id: MessageId,
}

#[derive(Serialize, Request)]
#[shrimple_telegram(response_type = User)]
pub struct GetMe {
    #[serde(skip_serializing)]
    bot: Bot,
}

#[derive(Serialize, Request)]
#[shrimple_telegram(response_type = bool)]
pub struct DeleteWebhook {
    #[serde(skip_serializing)]
    bot: Bot,
}

#[test]
fn f() {
    let bot = Bot::new("abc");
    bot.send_message(420, "hey").parse_mode(ParseMode::Html);
}
