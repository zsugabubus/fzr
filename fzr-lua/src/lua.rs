use core::ffi::{CStr, c_char, c_double, c_int, c_void};
use std::mem::{size_of, transmute};

#[allow(non_camel_case_types)]
type c_size_t = usize;

#[repr(C)]
pub struct lua_State(());

pub type CFunction = unsafe extern "C" fn(StateRef) -> c_int;
pub type Number = c_double;

const LUA_TNIL: c_int = 0;

unsafe extern "C" {
    fn luaL_newmetatable(lua: *mut lua_State, tname: *const c_char) -> c_int;
    fn luaL_setmetatable(lua: *mut lua_State, tname: *const c_char);
    fn lua_createtable(lua: *mut lua_State, narr: c_int, nrec: c_int);
    fn lua_newuserdata(lua: *mut lua_State, sz: c_size_t) -> *mut c_void;
    fn lua_objlen(lua: *mut lua_State, idx: c_int) -> c_size_t;
    fn lua_pushboolean(lua: *mut lua_State, b: c_int);
    fn lua_pushcclosure(lua: *mut lua_State, f: CFunction, n: c_int);
    fn lua_pushlstring(lua: *mut lua_State, s: *const u8, l: c_size_t);
    fn lua_pushnumber(lua: *mut lua_State, n: Number);
    fn lua_rawequal(lua: *mut lua_State, idx1: c_int, idx2: c_int) -> c_int;
    fn lua_rawgeti(lua: *mut lua_State, idx: c_int, n: c_int);
    fn lua_rawseti(lua: *mut lua_State, idx: c_int, n: c_int);
    fn lua_settable(lua: *mut lua_State, idx: c_int);
    fn lua_gettable(lua: *mut lua_State, idx: c_int);
    fn lua_settop(lua: *mut lua_State, idx: c_int);
    fn lua_tolstring(lua: *mut lua_State, idx: c_int, l: *mut c_size_t) -> *const u8;
    fn lua_tonumber(lua: *mut lua_State, idx: c_int) -> Number;
    fn lua_toboolean(lua: *mut lua_State, idx: c_int) -> c_int;
    fn lua_touserdata(lua: *mut lua_State, arg: c_int, tname: *const c_char) -> *mut c_void;
    fn lua_type(lua: *mut lua_State, idx: c_int) -> c_int;
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct StateRef(*mut lua_State);

impl StateRef {
    pub unsafe fn push_array(&self, capacity: usize) {
        unsafe {
            lua_createtable(self.0, capacity as c_int, 0);
        }
    }

    pub unsafe fn push_map(&self, capacity: usize) {
        unsafe {
            lua_createtable(self.0, 0, capacity as c_int);
        }
    }

    pub unsafe fn push_str(&self, value: &str) {
        unsafe {
            lua_pushlstring(self.0, value.as_ptr(), value.len());
        }
    }

    pub unsafe fn push_number(&self, value: Number) {
        unsafe {
            lua_pushnumber(self.0, value);
        }
    }

    pub unsafe fn push_fn(&self, value: CFunction) {
        unsafe {
            lua_pushcclosure(self.0, value, 0);
        }
    }

    pub unsafe fn push_bool(&self, value: bool) {
        unsafe {
            lua_pushboolean(self.0, if value { 1 } else { 0 });
        }
    }

    pub unsafe fn push_userdata<T: Userdata>(&self, value: T) {
        unsafe {
            let ptr = lua_newuserdata(self.0, size_of::<T>());
            std::ptr::write(ptr as *mut _, value);
            luaL_setmetatable(self.0, T::NAME.as_ptr());
        }
    }

    #[cold]
    pub unsafe fn false_error(&self, s: &str) -> c_int {
        unsafe {
            self.push_bool(false);
            self.push_str(s);
            2
        }
    }

    pub unsafe fn create_usermeta<T: Userdata>(&self) {
        unsafe {
            let created = luaL_newmetatable(self.0, T::NAME.as_ptr()) == 1;
            assert!(created);
            self.set_table_str_fn("__gc", T::gc);
        }
    }

    pub unsafe fn pop(&self) {
        unsafe {
            lua_settop(self.0, -2);
        }
    }

    pub unsafe fn get_str<'a>(&self, index: c_int) -> Option<&'a str> {
        unsafe {
            let mut len: c_size_t = 0;
            let ptr = lua_tolstring(self.0, index, &mut len).as_ref()?;
            std::str::from_utf8(std::slice::from_raw_parts(ptr, len)).ok()
        }
    }

    pub unsafe fn get_number(&self, index: c_int) -> Number {
        unsafe { lua_tonumber(self.0, index) }
    }

    pub unsafe fn get_boolean(&self, index: c_int) -> bool {
        unsafe { lua_toboolean(self.0, index) != 0 }
    }

    pub unsafe fn get_userdata<T: Userdata>(&self, index: c_int) -> Option<&'static mut T> {
        unsafe { transmute(lua_touserdata(self.0, index, T::NAME.as_ptr()).as_ref()?) }
    }

    pub unsafe fn get_table_str(&self, index: c_int, key: &str) {
        unsafe {
            self.push_str(key);
            lua_gettable(self.0, index)
        }
    }

    pub unsafe fn get_index_raw(&self, array: c_int, index: c_int) {
        unsafe {
            lua_rawgeti(self.0, array, index);
        }
    }

    pub unsafe fn get_len(&self, index: c_int) -> usize {
        unsafe { lua_objlen(self.0, index) }
    }

    pub unsafe fn set_index_raw(&self, index: c_int, n: c_int) {
        unsafe {
            lua_rawseti(self.0, index, n);
        }
    }

    pub unsafe fn set_table_str_fn(&self, key: &str, value: CFunction) {
        unsafe {
            self.push_str(key);
            self.push_fn(value);
            lua_settable(self.0, -3);
        }
    }

    pub unsafe fn is_nil(&self, index: c_int) -> bool {
        unsafe { lua_type(self.0, index) == LUA_TNIL }
    }

    pub unsafe fn is_equal_raw(&self, index1: c_int, index2: c_int) -> bool {
        unsafe { lua_rawequal(self.0, index1, index2) == 1 }
    }
}

pub trait Userdata: Sized
where
    Self: 'static,
{
    const NAME: &CStr;

    unsafe extern "C" fn gc(lua: StateRef) -> c_int {
        unsafe {
            std::ptr::drop_in_place(lua.get_userdata::<Self>(1).unwrap());
        }
        0
    }
}
