//! A library for running a Telegram bot.
//!
//! Work in progress.

pub mod methods;
pub mod types;

use {
    reqwest::{
        header::{HeaderValue, CONTENT_TYPE},
        multipart::{Form, Part},
        RequestBuilder, Url,
    },
    serde::{
        de::{DeserializeOwned, Error as _},
        ser::{Impossible, SerializeStruct},
        Deserialize, Serialize, Serializer,
    },
    std::{
        borrow::Cow,
        fmt::{Display, Formatter},
        future::{Future, IntoFuture},
        pin::Pin,
        sync::Arc,
    },
    types::ReplyMarkup,
};

pub const MAX_MSG_LEN: usize = 4096;
pub type Result<T = (), E = Error> = std::result::Result<T, E>;

trait IsDefault {
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

/// An error that can occur while sending a request to Telegram Bot API
#[derive(Debug)]
pub enum Error {
    Io(std::io::Error),
    Request { method_name: &'static str, inner: reqwest::Error },
    Response { method_name: &'static str, description: Box<str> },
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Self::Io(e)
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Io(e) => write!(f, "IO error: {e}"),
            Self::Request { method_name, inner } => {
                write!(f, "Error while sending `{method_name}`: {inner}")
            }
            Self::Response { method_name, description } => {
                writeln!(f, "Telegram API error from method {method_name}: {description}")
            }
        }
    }
}

impl std::error::Error for Error {}

pub trait Request: Serialize + IntoFuture<Output = Result<Self::Response>> {
    const NAME: &str;
    type Response: DeserializeOwned;

    /// Analogous to [`IntoFuture::into_future`], but creates a future without consuming the
    /// request.
    ///
    /// # Warning
    ///
    /// Implementors may panic upon calling this method if the request body may not be shared
    /// without additional async processing; for an example,
    /// see [`InputFile`](types::InputFile) and its [`reusable`](types::InputFile::reusable) method.
    fn to_future(&self) -> impl Future<Output = Result<Self::Response>>;
}

#[derive(Deserialize)]
struct TelegramResponse<T> {
    ok: bool,
    #[serde(default)]
    description: String,
    result: Option<T>,
}

fn form_filler_err(s: impl Display) -> serde::de::value::Error {
    serde::de::value::Error::custom(format_args!("FormFiller::{s}"))
}

struct StringExtractor;

impl Serializer for StringExtractor {
    type Ok = String;
    type Error = std::fmt::Error;
    type SerializeSeq = Impossible<Self::Ok, std::fmt::Error>;
    type SerializeTuple = Impossible<Self::Ok, std::fmt::Error>;
    type SerializeTupleStruct = Impossible<Self::Ok, std::fmt::Error>;
    type SerializeTupleVariant = Impossible<Self::Ok, std::fmt::Error>;
    type SerializeMap = Impossible<Self::Ok, std::fmt::Error>;
    type SerializeStruct = Impossible<Self::Ok, std::fmt::Error>;
    type SerializeStructVariant = Impossible<Self::Ok, std::fmt::Error>;

    fn serialize_bool(self, _: bool) -> Result<Self::Ok, Self::Error> {
        Err(std::fmt::Error)
    }

    fn serialize_i8(self, _: i8) -> Result<Self::Ok, Self::Error> {
        Err(std::fmt::Error)
    }

    fn serialize_i16(self, _: i16) -> Result<Self::Ok, Self::Error> {
        Err(std::fmt::Error)
    }

    fn serialize_i32(self, _: i32) -> Result<Self::Ok, Self::Error> {
        Err(std::fmt::Error)
    }

    fn serialize_i64(self, _: i64) -> Result<Self::Ok, Self::Error> {
        Err(std::fmt::Error)
    }

    fn serialize_u8(self, _: u8) -> Result<Self::Ok, Self::Error> {
        Err(std::fmt::Error)
    }

    fn serialize_u16(self, _: u16) -> Result<Self::Ok, Self::Error> {
        Err(std::fmt::Error)
    }

    fn serialize_u32(self, _: u32) -> Result<Self::Ok, Self::Error> {
        Err(std::fmt::Error)
    }

    fn serialize_u64(self, _: u64) -> Result<Self::Ok, Self::Error> {
        Err(std::fmt::Error)
    }

    fn serialize_f32(self, _: f32) -> Result<Self::Ok, Self::Error> {
        Err(std::fmt::Error)
    }

    fn serialize_f64(self, _: f64) -> Result<Self::Ok, Self::Error> {
        Err(std::fmt::Error)
    }

    fn serialize_char(self, c: char) -> Result<Self::Ok, Self::Error> {
        Ok(c.to_string())
    }

    fn serialize_str(self, s: &str) -> Result<Self::Ok, Self::Error> {
        Ok(s.to_owned())
    }

    fn serialize_bytes(self, _: &[u8]) -> Result<Self::Ok, Self::Error> {
        Err(std::fmt::Error)
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        Err(std::fmt::Error)
    }

    fn serialize_some<T: ?Sized + Serialize>(self, x: &T) -> Result<Self::Ok, Self::Error> {
        x.serialize(self)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        Err(std::fmt::Error)
    }

    fn serialize_unit_struct(self, name: &'static str) -> Result<Self::Ok, Self::Error> {
        Ok(name.to_owned())
    }

    fn serialize_unit_variant(
        self,
        _: &'static str,
        _: u32,
        name: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        Ok(name.to_owned())
    }

    fn serialize_newtype_struct<T: ?Sized + Serialize>(
        self,
        _: &'static str,
        x: &T,
    ) -> Result<Self::Ok, Self::Error> {
        x.serialize(self)
    }

    fn serialize_newtype_variant<T: ?Sized + Serialize>(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
        x: &T,
    ) -> Result<Self::Ok, Self::Error> {
        x.serialize(self)
    }

    fn serialize_seq(self, _: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Err(std::fmt::Error)
    }

    fn serialize_tuple(self, _: usize) -> Result<Self::SerializeTuple, Self::Error> {
        Err(std::fmt::Error)
    }

    fn serialize_tuple_struct(
        self,
        _: &'static str,
        _: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Err(std::fmt::Error)
    }

    fn serialize_tuple_variant(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
        _: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Err(std::fmt::Error)
    }

    fn serialize_map(self, _: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Err(std::fmt::Error)
    }

    fn serialize_struct(
        self,
        _: &'static str,
        _: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        Err(std::fmt::Error)
    }

    fn serialize_struct_variant(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
        _: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Err(std::fmt::Error)
    }
}

fn get_string<T: ?Sized + Serialize>(value: &T) -> Result<String, &T> {
    value.serialize(StringExtractor).map_err(|_| value)
}

struct FormFiller(Option<Form>);

impl SerializeStruct for FormFiller {
    type Ok = Form;
    type Error = serde::de::value::Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + Serialize,
    {
        let json = get_string(value) // strings must be passed verbatim, not formatted as JSON
            .map_or_else(serde_json::to_string, Ok)
            .map_err(|e| {
                form_filler_err(format_args!("serialize_field: JSON serializer failed: {e}"))
            })?;
        self.0 = self
            .0
            .take()
            .ok_or(form_filler_err("serialize_field: form lost"))?
            .text(key, json)
            .into();
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.0.ok_or(form_filler_err("end: form lost"))
    }
}

fn e<T>() -> Result<T, serde::de::value::Error> {
    Err(serde::de::value::Error::custom("FormFiller failed: Request must be a struct"))
}

impl Serializer for FormFiller {
    type Ok = Form;
    type Error = serde::de::value::Error;
    type SerializeSeq = Impossible<Self::Ok, Self::Error>;
    type SerializeTuple = Impossible<Self::Ok, Self::Error>;
    type SerializeTupleStruct = Impossible<Self::Ok, Self::Error>;
    type SerializeTupleVariant = Impossible<Self::Ok, Self::Error>;
    type SerializeMap = Impossible<Self::Ok, Self::Error>;
    type SerializeStruct = Self;
    type SerializeStructVariant = Impossible<Self::Ok, Self::Error>;

    fn serialize_struct(self, _: &str, _: usize) -> Result<Self::SerializeStruct, Self::Error> {
        Ok(self)
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        e()
    }
    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        e()
    }
    fn serialize_u8(self, _: u8) -> Result<Self::Ok, Self::Error> {
        e()
    }
    fn serialize_i8(self, _: i8) -> Result<Self::Ok, Self::Error> {
        e()
    }
    fn serialize_i16(self, _: i16) -> Result<Self::Ok, Self::Error> {
        e()
    }
    fn serialize_i32(self, _: i32) -> Result<Self::Ok, Self::Error> {
        e()
    }
    fn serialize_i64(self, _: i64) -> Result<Self::Ok, Self::Error> {
        e()
    }
    fn serialize_u16(self, _: u16) -> Result<Self::Ok, Self::Error> {
        e()
    }
    fn serialize_u32(self, _: u32) -> Result<Self::Ok, Self::Error> {
        e()
    }
    fn serialize_u64(self, _: u64) -> Result<Self::Ok, Self::Error> {
        e()
    }
    fn serialize_f32(self, _: f32) -> Result<Self::Ok, Self::Error> {
        e()
    }
    fn serialize_f64(self, _: f64) -> Result<Self::Ok, Self::Error> {
        e()
    }
    fn serialize_str(self, _: &str) -> Result<Self::Ok, Self::Error> {
        e()
    }
    fn serialize_bool(self, _: bool) -> Result<Self::Ok, Self::Error> {
        e()
    }
    fn serialize_char(self, _: char) -> Result<Self::Ok, Self::Error> {
        e()
    }
    fn serialize_bytes(self, _: &[u8]) -> Result<Self::Ok, Self::Error> {
        e()
    }
    fn serialize_unit_struct(self, _: &str) -> Result<Self::Ok, Self::Error> {
        e()
    }
    fn serialize_tuple(self, _: usize) -> Result<Self::SerializeTuple, Self::Error> {
        e()
    }
    fn serialize_map(self, _: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        e()
    }
    fn serialize_seq(self, _: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        e()
    }
    fn serialize_some<T: ?Sized + Serialize>(self, _: &T) -> Result<Self::Ok, Self::Error> {
        e()
    }
    fn serialize_unit_variant(self, _: &str, _: u32, _: &str) -> Result<Self::Ok, Self::Error> {
        e()
    }
    fn serialize_tuple_struct(
        self,
        _: &str,
        _: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        e()
    }
    fn serialize_newtype_struct<T: ?Sized + Serialize>(
        self,
        _: &str,
        _: &T,
    ) -> Result<Self::Ok, Self::Error> {
        e()
    }
    fn serialize_tuple_variant(
        self,
        _: &str,
        _: u32,
        _: &str,
        _: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        e()
    }
    fn serialize_struct_variant(
        self,
        _: &str,
        _: u32,
        _: &str,
        _: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        e()
    }
    fn serialize_newtype_variant<T: ?Sized + Serialize>(
        self,
        _: &str,
        _: u32,
        _: &str,
        _: &T,
    ) -> Result<Self::Ok, Self::Error> {
        e()
    }
}

pub(crate) const PAYLOAD_NAME: &str = "__PAYLOAD";
pub(crate) const PAYLOAD_LINK: &str = "attach://__PAYLOAD";

pub(crate) fn serialise_into_form<R: Serialize>(req: &R, payload: Part) -> Form {
    req.serialize(FormFiller(Some(Form::new())))
        .expect("request to be a struct")
        .part(PAYLOAD_NAME, payload)
}

pub(crate) trait Multipart {
    fn get_payload(&self) -> Option<Part>;
    /// Exclusive access allows for more performant access.
    ///
    /// Implementation may assume that the functions of this trait will never be called again on
    /// this same value.
    fn get_payload_mut(&mut self) -> Option<Part>;
}

/// A builder for the [`Bot`] that allows for customising the API URL & the HTTPS client config used
/// for connecting to the API endpoint.
pub struct BotBuilder<'src> {
    token: &'src str,
    api_url: Option<Url>,
    client: Option<reqwest::Client>,
}

impl<'src> BotBuilder<'src> {
    /// Creates a new builder for the [`Bot`].
    pub fn new(token: &'src str) -> Self {
        Self { token, api_url: None, client: None }
    }

    /// Set a different URL as the endpoint for requests to the Telegram Bot API.
    ///
    /// The default one is `https://api.telegram.org`.
    pub fn api_url(mut self, base: impl reqwest::IntoUrl) -> reqwest::Result<Self> {
        self.api_url = Some(base.into_url()?);
        Ok(self)
    }

    /// Set a different URL as the endpoint for requests to the Telegram Bot API.
    ///
    /// The default one is `https://api.telegram.org`.
    pub fn set_api_url(&mut self, base: impl reqwest::IntoUrl) -> reqwest::Result<()> {
        self.api_url = Some(base.into_url()?);
        Ok(())
    }

    /// Set a different HTTPS client for sending requests to the Telegram Bot API.
    ///
    /// Use this if the default config of [`reqwest::Client`] doesn't suffice for your case.
    pub fn client(mut self, client: reqwest::Client) -> Self {
        self.client = Some(client);
        self
    }

    /// Set a different HTTPS client for sending requests to the Telegram Bot API.
    ///
    /// Use this if the default config of [`reqwest::Client`] doesn't suffice for your case.
    pub fn set_client(&mut self, client: reqwest::Client) {
        self.client = Some(client);
    }

    /// Set a different Telegram Bot API token.
    pub fn token(mut self, token: &'src str) -> Self {
        self.token = token;
        self
    }

    /// Set a different Telegram Bot API token.
    pub fn set_token(&mut self, token: &'src str) {
        self.token = token;
    }

    /// Build the [`Bot`] instance.
    pub fn build(self) -> Bot {
        let mut base =
            self.api_url.unwrap_or_else(|| Url::parse("https://api.telegram.org").unwrap());
        base.set_path(&format!("bot{}", self.token));
        Bot(Arc::new((self.client.unwrap_or_default(), base)))
    }
}

/// A high-level representation of the connection to Telegram Bot API.
#[derive(Clone)]
pub struct Bot(Arc<(reqwest::Client, Url)>);

impl Bot {
    /// Create a [`Bot`] instance.
    ///
    /// if you need more customisation for the API connection, use [`BotBuilder`] instead.
    pub fn new(token: &str) -> Self {
        BotBuilder::new(token).build()
    }

    fn client(&self) -> &reqwest::Client {
        &self.0 .0
    }

    fn base(&self) -> &Url {
        &self.0 .1
    }

    /// Make a manual request to Telegram Bot API using `multipart/form-data`.
    ///
    /// The `payload` will be referred to in the request as "payload"
    ///
    /// # Warning
    /// This is a low-level method, only use this if the Telegram method is not yet adapted to
    /// `shrimple-telegram`.
    /// Consider [opening an issue](https://github.com/its-the-shrimp/shrimple-telegram/issues/new)
    /// if that's the case.
    pub fn multipart_request<R: DeserializeOwned + 'static>(
        &self,
        method_name: &'static str,
        form: Form,
    ) -> Pin<Box<dyn Future<Output = Result<R>> + Send + Sync + 'static>> {
        let mut url = self.base().clone();
        if let Ok(mut segments) = url.path_segments_mut() {
            segments.push(method_name);
        }
        let req = self.client().get(url).multipart(form);

        Box::pin(Self::prepared_request(method_name, req))
    }

    /// Make a manual request to Telegram Bot API.
    ///
    /// # Warning
    /// This is a low-level method, only use this if the Telegram method is not yet adapted to
    /// `shrimple-telegram`.
    /// Consider [opening an issue](https://github.com/its-the-shrimp/shrimple-telegram/issues/new)
    /// if that's the case.
    pub fn request<R: DeserializeOwned + 'static>(
        &self,
        method_name: &'static str,
        json: String,
    ) -> Pin<Box<dyn Future<Output = Result<R>> + Send + Sync + 'static>> {
        let mut url = self.base().clone();
        if let Ok(mut segments) = url.path_segments_mut() {
            segments.push(method_name);
        }
        let req = self
            .client()
            .get(url)
            .body(json)
            .header(CONTENT_TYPE, const { HeaderValue::from_static("application/json") });

        Box::pin(Self::prepared_request(method_name, req))
    }

    async fn prepared_request<R: DeserializeOwned>(
        method_name: &'static str,
        req: RequestBuilder,
    ) -> Result<R> {
        let (client, req) = req.build_split();
        let req = req.map_err(|inner| Error::Request { method_name, inner })?;

        match client
            .execute(req)
            .await
            .map_err(|inner| Error::Request { method_name, inner })?
            .json()
            .await
            .map_err(|inner| Error::Request { method_name, inner })?
        {
            TelegramResponse { ok: true, result: Some(result), .. } => Ok(result),
            TelegramResponse { mut description, .. } => Err({
                description.insert_str(0, ": Telegram API error: ");
                description.insert_str(0, method_name);
                crate::Error::Response { method_name, description: description.into() }
            }),
        }
    }
}
