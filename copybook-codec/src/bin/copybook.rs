use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, RecordFormat, decode_file_to_jsonl,
    encode_jsonl_to_file,
};
use copybook_core::parse_copybook;
use std::env;
use std::fs::{self, File};
use std::io::{self, BufReader, BufWriter, Write};
use std::path::PathBuf;

fn parse_record_format(value: &str) -> Result<RecordFormat, String> {
    match value.to_ascii_lowercase().as_str() {
        "fixed" => Ok(RecordFormat::Fixed),
        "rdw" => Ok(RecordFormat::RDW),
        other => Err(format!("Unsupported format: {other}")),
    }
}

fn parse_codepage(value: &str) -> Result<Codepage, String> {
    match value.to_ascii_lowercase().as_str() {
        "ascii" => Ok(Codepage::ASCII),
        "cp037" => Ok(Codepage::CP037),
        "cp273" => Ok(Codepage::CP273),
        "cp500" => Ok(Codepage::CP500),
        "cp1047" => Ok(Codepage::CP1047),
        "cp1140" => Ok(Codepage::CP1140),
        other => Err(format!("Unsupported codepage: {other}")),
    }
}

fn parse_common_args(args: &[String]) -> Result<(RecordFormat, Codepage, Vec<String>), String> {
    let mut format = RecordFormat::Fixed;
    let mut codepage = Codepage::ASCII;
    let mut positional = Vec::new();

    let mut iter = args.iter();
    while let Some(arg) = iter.next() {
        match arg.as_str() {
            "--format" => {
                let value = iter
                    .next()
                    .ok_or_else(|| "--format requires an argument".to_string())?;
                format = parse_record_format(value)?;
            }
            "--codepage" => {
                let value = iter
                    .next()
                    .ok_or_else(|| "--codepage requires an argument".to_string())?;
                codepage = parse_codepage(value)?;
            }
            other if other.starts_with('-') => {
                return Err(format!("Unsupported option: {other}"));
            }
            value => positional.push(value.to_string()),
        }
    }

    Ok((format, codepage, positional))
}

fn decode_command(args: &[String]) -> Result<(), Box<dyn std::error::Error>> {
    let mut output_path: Option<PathBuf> = None;
    let mut remaining = Vec::new();
    let mut iter = args.iter();
    while let Some(arg) = iter.next() {
        match arg.as_str() {
            "--output" => {
                let value = iter.next().ok_or_else(|| {
                    io::Error::new(io::ErrorKind::InvalidInput, "--output requires a path")
                })?;
                output_path = Some(PathBuf::from(value));
            }
            _ => remaining.push(arg.to_string()),
        }
    }

    let (format, codepage, positional) = parse_common_args(&remaining)?;
    if positional.len() < 2 {
        return Err("decode requires <copybook> and <data> arguments".into());
    }

    let copybook_path = PathBuf::from(&positional[0]);
    let data_path = PathBuf::from(&positional[1]);
    let schema_text = fs::read_to_string(copybook_path)?;
    let schema = parse_copybook(&schema_text)?;

    let options = DecodeOptions::new()
        .with_format(format)
        .with_codepage(codepage)
        .with_json_number_mode(copybook_codec::JsonNumberMode::Lossless);

    let mut output: Box<dyn Write> = match output_path {
        Some(path) => Box::new(BufWriter::new(File::create(path)?)),
        None => Box::new(BufWriter::new(io::stdout())),
    };

    let input = BufReader::new(File::open(&data_path)?);
    decode_file_to_jsonl(&schema, input, &mut output, &options)?;
    output.flush()?;
    Ok(())
}

fn encode_command(args: &[String]) -> Result<(), Box<dyn std::error::Error>> {
    let (format, codepage, positional) = parse_common_args(args)?;
    if positional.len() < 3 {
        return Err("encode requires <copybook> <jsonl> <output> arguments".into());
    }

    let copybook_path = PathBuf::from(&positional[0]);
    let json_path = PathBuf::from(&positional[1]);
    let output_path = PathBuf::from(&positional[2]);

    let schema_text = fs::read_to_string(copybook_path)?;
    let schema = parse_copybook(&schema_text)?;

    let options = EncodeOptions::new()
        .with_format(format)
        .with_codepage(codepage)
        .with_use_raw(false)
        .with_bwz_encode(false);

    let input = BufReader::new(File::open(&json_path)?);
    let output = BufWriter::new(File::create(&output_path)?);
    encode_jsonl_to_file(&schema, input, output, &options)?;
    Ok(())
}

fn run() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().skip(1).collect();
    if args.is_empty() {
        return Err("expected a command (decode|encode)".into());
    }

    let command = &args[0];
    match command.as_str() {
        "decode" => decode_command(&args[1..])?,
        "encode" => encode_command(&args[1..])?,
        other => return Err(format!("unknown command: {other}").into()),
    }

    Ok(())
}

fn main() {
    if let Err(err) = run() {
        eprintln!("{err}");
        std::process::exit(1);
    }
}
