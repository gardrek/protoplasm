use clap::Parser;

#[derive(Parser)]
#[command(name = "protoplasm")]
#[command(version = "0.1.0")]
#[command(about = "Configurable assembler for hobbby CPU architectures", long_about = None)]
pub struct ArgStruct {
    /// File describing the target architecture.
    /// If not given, the file is not compiled, only checked for errors.
    #[arg(short, long)]
    pub recipe: Option<std::path::PathBuf>,

    /// The file to assemble
    pub source_file: std::path::PathBuf,
}
