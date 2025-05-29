use std::any::Any;
use std::path::{Path, PathBuf};
use std::time::Duration;

use crate::communication::{perform_register_operation, IlaRegisters, ReadWrite};
use crate::config::{self, ConfigMethod};
use crate::predicates::PredicateOperation;
use crate::predicates_tui::NumericState;
use clap::builder::{TypedValueParser, ValueParserFactory};
use clap::error::ErrorKind;
use clap::{arg, Arg, Args, Command, Error, FromArgMatches, Subcommand};
use serialport::SerialPort;

/// A macro for generating 'Parser' variants for 'Container' types used by Clap
///
/// I cannot directly implement the Clap `ValueParserFactory` for Rust standard types, so instead I
/// have to generate 'Container' structs. These 'Container' structs will then also get an associated
/// 'Parser' struct.
///
/// The parser structs should implement `TypedValueParser`, that is the only thing this macro will
/// not automatically generate.
///
/// Once `TypedValueParser` is implemented, you can savely use the 'Container' struct as a CLI
/// argument
macro_rules! make_container_arg {
    ($parser:ident, $container:ident, $type:ty, $description:literal) => {
        make_container_arg!($parser, $container, $type, $description, $description);
    };

    ($parser:ident, $container:ident, $type:ty, $read_desc:literal, $write_desc:literal) => {
        #[derive(Debug, Clone)]
        struct $container($type);

        impl ValueParserFactory for $container {
            type Parser = $parser;

            fn value_parser() -> Self::Parser {
                $parser
            }
        }

        impl From<$container> for $type {
            fn from(value: $container) -> Self {
                value.0
            }
        }

        impl ArgumentContainer for $container {
            type Contains = $type;
        }

        #[derive(Debug, Clone, Copy)]
        struct $parser;

        impl ArgumentType for $parser {
            fn description(is_rd: IsRW) -> &'static str {
                match is_rd {
                    IsRW::Read => $read_desc,
                    IsRW::Write => $write_desc,
                }
            }
        }
    };
}

/// A wrapper trait, only meant for 'Container' structs to be capable of pointing to their
/// 'contained' type
///
/// See `make_container_arg!()`
pub trait ArgumentContainer {
    type Contains;
}

/// A trait for generating the help description, output may vary depending on wether of not the
/// command being issued is `IsRW::Read` or `IsRW::Write`
trait ArgumentType {
    fn description(is_rd: IsRW) -> &'static str;
}

/// A simple enum indicating if something is either a `IsRW::Read` or a `IsRW::Write`
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum IsRW {
    Read,
    Write,
}

make_container_arg!(UnsupportedParser, Unsupported, (), "Unsupported argument");
impl TypedValueParser for UnsupportedParser {
    type Value = Unsupported;

    fn parse_ref(
        &self,
        _cmd: &Command,
        _arg: Option<&Arg>,
        _value: &std::ffi::OsStr,
    ) -> Result<Self::Value, Error> {
        Err(Error::new(ErrorKind::InvalidValue))
    }
}

make_container_arg!(FlagParser, Flag, (), "Performs a read", "Performs a write");
impl TypedValueParser for FlagParser {
    type Value = Flag;

    fn parse_ref(
        &self,
        _cmd: &Command,
        _arg: Option<&Arg>,
        value: &std::ffi::OsStr,
    ) -> Result<Self::Value, Error> {
        match value.to_str() {
            Some("true") => Ok(Flag(())),
            _ => Err(Error::new(ErrorKind::InvalidValue)),
        }
    }
}

make_container_arg!(
    ByteStreamParser,
    ByteStream,
    Vec<u8>,
    "Reads an unsigned value with the same bitwidth as of the combined signal",
    "Writes an unsigned value with the same bitwidth as of the combined signal"
);
impl TypedValueParser for ByteStreamParser {
    type Value = ByteStream;

    fn parse_ref(
        &self,
        _cmd: &Command,
        _arg: Option<&Arg>,
        value: &std::ffi::OsStr,
    ) -> Result<Self::Value, Error> {
        match NumericState::immediate_parse(&value.to_string_lossy()) {
            Ok(num) => Ok(ByteStream(num.to_bytes_be())),
            Err(_) => Err(Error::new(ErrorKind::InvalidValue)),
        }
    }
}

make_container_arg!(
    IndicesParser,
    Indices,
    Vec<u32>,
    "Reads an list of comma seperated indices (unsinged 32 bit numbers) (example: 1,2,3)",
    "Writes an list of comma seperated indices (unsinged 32 bit numbers) (example: 1,2,3)"
);
impl TypedValueParser for IndicesParser {
    type Value = Indices;

    fn parse_ref(
        &self,
        _cmd: &Command,
        _arg: Option<&Arg>,
        value: &std::ffi::OsStr,
    ) -> Result<Self::Value, Error> {
        let lossy = value.to_string_lossy();
        let mut words = vec![];
        for chunk in lossy.split(",") {
            let word: u32 = NumericState::immediate_parse(chunk)
                .map_err(|_| Error::new(ErrorKind::InvalidValue))?
                .try_into()
                .map_err(|_| Error::new(ErrorKind::InvalidValue))?;
            words.push(word);
        }
        Ok(Indices(words))
    }
}

make_container_arg!(
    WordParser,
    Word,
    u32,
    "Reads an unsigned 32 bit value",
    "Writes an unsigned 32 bit value"
);
impl TypedValueParser for WordParser {
    type Value = Word;

    fn parse_ref(
        &self,
        _cmd: &Command,
        _arg: Option<&Arg>,
        value: &std::ffi::OsStr,
    ) -> Result<Self::Value, Error> {
        match u32::try_from(
            NumericState::immediate_parse(&value.to_string_lossy())
                .map_err(|_| Error::new(ErrorKind::InvalidValue))?,
        ) {
            Ok(n) => Ok(Word(n)),
            Err(_) => Err(Error::new(ErrorKind::InvalidValue)),
        }
    }
}

make_container_arg!(
    OperationParser,
    Operation,
    PredicateOperation,
    "Either the literal string 'and' or 'or' (without the quotes)"
);
impl TypedValueParser for OperationParser {
    type Value = Operation;

    fn parse_ref(
        &self,
        _cmd: &Command,
        _arg: Option<&Arg>,
        value: &std::ffi::OsStr,
    ) -> Result<Self::Value, Error> {
        match value.to_str() {
            Some("and") => Ok(Operation(PredicateOperation::And)),
            Some("or") => Ok(Operation(PredicateOperation::Or)),
            _ => Err(Error::new(ErrorKind::InvalidValue)),
        }
    }
}

/// An CLI option to either issue an `ReadWriteArgument::Read` or a `ReadWriteArgument::Write`
/// command to a register
#[derive(Debug)]
pub enum ReadWriteArgument<R, W> {
    Read(R),
    Write(W),
}

impl<R, W> FromArgMatches for ReadWriteArgument<R, W>
where
    R: ValueParserFactory + std::fmt::Debug,
    W: ValueParserFactory + std::fmt::Debug,
    <R as ValueParserFactory>::Parser: TypedValueParser<Value = R>,
    <W as ValueParserFactory>::Parser: TypedValueParser<Value = W>,
    R: Sync + Send + Clone + 'static,
    W: Sync + Send + Clone + 'static,
{
    fn from_arg_matches(matches: &clap::ArgMatches) -> Result<Self, clap::Error> {
        // Ensure we only ever have one option
        let is_rw = match (
            matches.try_contains_id("read"),
            matches.try_contains_id("write"),
        ) {
            (Ok(true), Ok(true)) => return Err(Error::new(ErrorKind::ArgumentConflict)),
            (Ok(true), _) => IsRW::Read,
            (_, Ok(true)) => IsRW::Write,
            _ => return Err(Error::new(ErrorKind::MissingRequiredArgument)),
        };

        // Protect against unsupported arguments
        if is_rw == IsRW::Read && R::value_parser().type_id() == UnsupportedParser.type_id() {
            return Err(Error::new(ErrorKind::InvalidValue));
        }
        if is_rw == IsRW::Write && W::value_parser().type_id() == UnsupportedParser.type_id() {
            return Err(Error::new(ErrorKind::InvalidValue));
        }

        match is_rw {
            IsRW::Read => matches
                .get_one::<R>("read")
                .map(|read| ReadWriteArgument::Read(read.to_owned()))
                .ok_or(Error::new(ErrorKind::InvalidValue)),
            IsRW::Write => matches
                .get_one::<W>("write")
                .map(|read| ReadWriteArgument::Write(read.to_owned()))
                .ok_or(Error::new(ErrorKind::InvalidValue)),
        }
    }

    fn update_from_arg_matches(&mut self, _matches: &clap::ArgMatches) -> Result<(), clap::Error> {
        Err(clap::Error::new(ErrorKind::InvalidSubcommand))
    }
}

impl<R, W> Args for ReadWriteArgument<R, W>
where
    R: ValueParserFactory + std::fmt::Debug,
    W: ValueParserFactory + std::fmt::Debug,
    <R as ValueParserFactory>::Parser: TypedValueParser<Value = R>,
    <W as ValueParserFactory>::Parser: TypedValueParser<Value = W>,
    <R as ValueParserFactory>::Parser: ArgumentType + std::fmt::Debug,
    <W as ValueParserFactory>::Parser: ArgumentType + std::fmt::Debug,
    R: Sync + Send + Clone + 'static,
    W: Sync + Send + Clone + 'static,
{
    fn augment_args(cmd: clap::Command) -> clap::Command {
        let read_parser = R::value_parser();
        let write_parser = W::value_parser();

        // Append our args to the command
        // If either the read of write is `Unsupported`, then it will not render that argument
        // If it is instead a `Flag`, it will now allow any aditional input, only the flag needs to
        // be given

        let cmd = match read_parser.type_id() {
            id if id == UnsupportedParser.type_id() => cmd,
            id if id == FlagParser.type_id() => cmd.arg(
                arg!(read: -r --read [TRUE])
                    .help(R::Parser::description(IsRW::Read))
                    .default_missing_value("true")
                    .value_parser(read_parser),
            ),
            _ => cmd.arg(
                arg!(read: -r --read <VALUE>)
                    .help(R::Parser::description(IsRW::Read))
                    .value_parser(read_parser),
            ),
        };
        match write_parser.type_id() {
            id if id == UnsupportedParser.type_id() => cmd,
            id if id == FlagParser.type_id() => cmd.arg(
                arg!(write: -w --write [TRUE])
                    .help(W::Parser::description(IsRW::Write))
                    .default_missing_value("true")
                    .value_parser(write_parser),
            ),
            _ => cmd.arg(
                arg!(write: -w --write <VALUE>)
                    .help(W::Parser::description(IsRW::Write))
                    .value_parser(write_parser),
            ),
        }
    }

    fn augment_args_for_update(cmd: clap::Command) -> clap::Command {
        cmd
    }
}

/// All the possible registers the CLI may write too
///
/// This list SHOULD be kept up to date and 1:1 reflect the `IlaRegisters` enum
/// The only reason this is a different enum is because the `IlaRegisters` enum is not compatible
/// with Clap's `Subcommand`
#[derive(Subcommand, Debug)]
enum RegisterSubcommand {
    /// Register containing the capture state of the ILA
    Capture,
    /// Register re-arming the trigger (and clear the buffer)
    TriggerReset,
    /// Checks the ILA for its triggered status
    TriggerState,
    /// Register controlling how many samples it should store after triggering
    TriggerPoint(ReadWriteArgument<Unsupported, Word>),
    /// The value to mask the samples against before triggering
    TriggerMask(ReadWriteArgument<Word, ByteStream>),
    /// The value used by the trigger to compare samples against, if it is set to do so
    TriggerCompare(ReadWriteArgument<Word, ByteStream>),
    /// How the trigger should handle mutliple predicates
    TriggerOp(ReadWriteArgument<Flag, Operation>),
    /// Which predicates are active for the trigger
    TriggerSelect(ReadWriteArgument<Flag, Word>),
    /// Read out samples from the ILA buffer, each vector item is an index within the buffer
    Buffer(ReadWriteArgument<Indices, Unsupported>),
    /// Compare the hash value within the ILA with the value provided
    Hash(ReadWriteArgument<Word, Unsupported>),
    /// The value to mask samples before being fed to the capture predicates
    CaptureMask(ReadWriteArgument<Word, ByteStream>),
    /// The value used by the capture predicates to compare samples against, if it is set to do so
    CaptureCompare(ReadWriteArgument<Word, ByteStream>),
    /// How the capture should handle mutliple predicates
    CaptureOp(ReadWriteArgument<Flag, Operation>),
    /// Which predicates are active for the capture
    CaptureSelect(ReadWriteArgument<Flag, Word>),
}

impl<R> ReadWriteArgument<R, Unsupported>
where
    R: ArgumentContainer,
    <R as ArgumentContainer>::Contains: From<R>,
{
    fn coerce_read(self) -> R::Contains {
        match self {
            ReadWriteArgument::Read(read) => read.into(),
            ReadWriteArgument::Write(_) => panic!("Attempt to use unsupported argument"),
        }
    }
}

impl<W> ReadWriteArgument<Unsupported, W>
where
    W: ArgumentContainer,
    <W as ArgumentContainer>::Contains: From<W>,
{
    fn coerce_write(self) -> W::Contains {
        match self {
            ReadWriteArgument::Read(_) => panic!("Attempt to use unsupported argument"),
            ReadWriteArgument::Write(write) => write.into(),
        }
    }
}

impl<R, W> From<ReadWriteArgument<R, W>> for ReadWrite<R::Contains, W::Contains>
where
    R: ArgumentContainer,
    <R as ArgumentContainer>::Contains: From<R>,
    W: ArgumentContainer,
    <W as ArgumentContainer>::Contains: From<W>,
{
    fn from(value: ReadWriteArgument<R, W>) -> Self {
        match value {
            ReadWriteArgument::Read(read) => ReadWrite::Read(read.into()),
            ReadWriteArgument::Write(write) => ReadWrite::Write(write.into()),
        }
    }
}

impl From<RegisterSubcommand> for IlaRegisters {
    fn from(value: RegisterSubcommand) -> Self {
        // I am sure that there is a way to automagically generate this implementation, or the
        // entire `RegisterSubcommand` type for that matter of fact. But I am not well versed
        // enough in macros do to so
        match value {
            RegisterSubcommand::Capture => Self::Capture,
            RegisterSubcommand::TriggerReset => Self::TriggerReset,
            RegisterSubcommand::TriggerState => Self::TriggerState,
            RegisterSubcommand::TriggerPoint(read_write_argument) => {
                IlaRegisters::TriggerPoint(read_write_argument.coerce_write())
            }
            RegisterSubcommand::TriggerMask(read_write_argument) => {
                Self::TriggerMask(read_write_argument.into())
            }
            RegisterSubcommand::TriggerCompare(read_write_argument) => {
                Self::TriggerCompare(read_write_argument.into())
            }
            RegisterSubcommand::TriggerOp(read_write_argument) => {
                Self::TriggerOp(read_write_argument.into())
            }
            RegisterSubcommand::TriggerSelect(read_write_argument) => {
                Self::TriggerSelect(read_write_argument.into())
            }
            RegisterSubcommand::Buffer(read_write_argument) => {
                Self::Buffer(read_write_argument.coerce_read())
            }
            RegisterSubcommand::Hash(read_write_argument) => {
                Self::Hash(read_write_argument.coerce_read())
            }
            RegisterSubcommand::CaptureMask(read_write_argument) => {
                Self::CaptureMask(read_write_argument.into())
            }
            RegisterSubcommand::CaptureCompare(read_write_argument) => {
                Self::CaptureCompare(read_write_argument.into())
            }
            RegisterSubcommand::CaptureOp(read_write_argument) => {
                Self::CaptureOp(read_write_argument.into())
            }
            RegisterSubcommand::CaptureSelect(read_write_argument) => {
                Self::CaptureSelect(read_write_argument.into())
            }
        }
    }
}

/// Check if an user given port path is valid and readable, intended to be used within a CLI-like
/// context
///
/// Returns a readable serial port on success
///
/// # Panics
///
/// Panics if the user provided an incorrect path or other IO errors accure
pub fn find_specified_port(check: &Path, baud: u32) -> Box<dyn SerialPort> {
    let ports = match serialport::available_ports() {
        Ok(ports) => ports,
        Err(err) => {
            println!("Unable to qeury serial port information;");
            println!("Kind: {:?}", err.kind);
            println!("Reason: {}", err.description);
            panic!("Unable to query serial port information.")
        }
    };

    let check_path = check.display().to_string();

    let valid_port = ports
        .into_iter()
        .find(|port| port.port_name == check_path)
        .expect("Provided path is not a valid serial port.");

    serialport::new(valid_port.port_name, baud)
        .timeout(Duration::from_secs(1))
        .open()
        .expect("Unable to open serial port (maybe busy?)")
}

/// Implementing this trait implies the data type can be converted to a command line output
pub trait CommandOutput {
    fn command_output(&self) -> String;
}

/// Simple trait to indicate this is a valid subcommand with arguments
pub trait ParseSubcommand {
    fn parse(self);
}

#[derive(Args, Debug)]
pub struct RegisterArgs {
    #[arg(short, long, help = "Path to serial port to use")]
    port: PathBuf,

    #[arg(short, long, default_value_t = 9600, help = "Sets baud rate")]
    baud: u32,

    #[command(flatten)]
    config: ConfigMethod,

    #[command(subcommand)]
    register: RegisterSubcommand,
}

impl ParseSubcommand for RegisterArgs {
    fn parse(self) {
        let mut tx_port = find_specified_port(&self.port, self.baud);
        let config = self.config.get_config()
            .unwrap_or_else(|_| panic!("File at {:?} contained errors", &self.config));

        let output = perform_register_operation(&mut tx_port, &config, &self.register.into());
        match output {
            Ok(output) => println!("{}", output.command_output()),
            Err(err) => panic!("{err}"),
        }
    }
}
