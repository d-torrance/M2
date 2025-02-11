#![crate_type = "staticlib"]

#[no_mangle]
pub extern fn rust_test() {
   println!("Hello from Rust!");
}
