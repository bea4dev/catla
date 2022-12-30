
fn main() {
    cc::Build::new()
        .file("src/wrapper.cpp")
        .cpp(true)
        .compile("wrapper");
}