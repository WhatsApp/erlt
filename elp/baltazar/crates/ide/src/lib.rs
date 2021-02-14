use std::sync::Arc;

use baltazar_syntax::{Parser, ast};
use vfs::FileId;

#[salsa::database(SourceDatabaseStorage)]
#[derive(Default)]
pub struct RootDatabase {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for RootDatabase {}

#[salsa::query_group(SourceDatabaseStorage)]
pub trait SourceDatabase: salsa::Database {
    fn parse(&self, file_id: FileId) -> ast::SourceFile;

    #[salsa::input]
    fn file_text(&self, file_id: FileId) -> Arc<String>;
}

fn parse(db: &dyn SourceDatabase, file_id: FileId) -> ast::SourceFile {
    let text = db.file_text(file_id);
    let mut parser = Parser::new();
    parser.parse(&text)
}
