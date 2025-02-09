#![crate_type = "dylib"]

#[no_mangle]
pub extern fn rust_test() {
   println!("Hello from Rust!");
}
