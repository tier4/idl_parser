mod annotations;
mod components;
mod core;
mod extended_data_types;
mod interfaces;
mod template;
mod value_types;

use crate::{expr::Definition, parser::core::skip_space_and_comment0};
use nom::{
    combinator::eof,
    error::{Error, VerboseError},
    IResult,
};

type PResult<'a, OUT> = IResult<&'a str, OUT, VerboseError<&'a str>>;

pub fn parse(mut input: &str) -> PResult<Vec<Definition>> {
    let mut result = Vec::new();
    loop {
        let (i, _) = skip_space_and_comment0(input)?;
        let (i, m) = core::parse_definition(i)?;
        let (i, _) = skip_space_and_comment0(i)?;

        result.push(m);

        if let Ok((_, _)) = eof::<&str, Error<&str>>(i) {
            break;
        }

        input = i;
    }

    Ok((input, result))
}

#[cfg(test)]
mod tests {
    use super::core::skip_space_and_comment0;
    use crate::parse;
    use nom::{error::convert_error, Finish};

    #[test]
    fn parser() {
        let input = r#"
// generated from rosidl_adapter/resource/msg.idl.em
// with input from example_msg/bar/Buz.msg
// generated code does not contain a copyright notice


module example_msg {
  typedef int32 int32__10[10];
  module msg {
    struct Buz {
      string c;
      @verbatim (language="comment", text=
          "http://wiki.ros.org/std_msgs")
      sequence<int32> o;
    };
  };
};"#;

        match parse(input).finish() {
            Ok((_, definitions)) => {
                println!("{:?}", definitions);
            }
            Err(e) => {
                let msg = convert_error(input, e);
                eprintln!("{msg}");
                panic!();
            }
        }
    }

    #[test]
    fn comment() {
        let input = r#"/*
abc */

// abc
// def"#;

        match skip_space_and_comment0(input) {
            Ok((input, _)) => {
                println!("{input}");
            }
            Err(nom::Err::Error(e)) => {
                let msg = convert_error(input, e);
                eprintln!("{msg}");
                panic!();
            }
            Err(_) => (),
        }
    }

    #[test]
    fn parser_complex() {
        let input = r#"module example_msg {
    module msg {
        typedef int32 int32__10[10];
        module StdMsg_Constants {
            @verbatim (language="comment", text=
                "constant")
            const int32 XX = 20;
            const string INITIALIZING_VEHICLE = "InitializingVehicle a\"a";
            const string WAITING_FOR_ROUTE = "WaitingForRoute";
            const string PLANNING = "Planning";
        };
        @verbatim (language="comment", text=
        "http://wiki.ros.org/msg")
        struct StdMsg {
            boolean a;

            int8 b;

            uint8 c;

            int16 d;

            uint16 e;

            int32 f;

            uint32 g;

            int64 h;

            uint64 i;

            float j;

            double k;

            string l;

            @verbatim (language="comment", text=
                "time m" "\n"
                "duration n" "\n"
                "array")
            sequence<int32> o;

            int32__10 p;

            sequence<int32, 5> limited;

            @verbatim (language="comment", text=
                "http://wiki.ros.org/std_msgs")
            std_msgs::msg::Bool q;

            std_msgs::msg::Byte r;

            std_msgs::msg::ByteMultiArray s;

            std_msgs::msg::Char t;

            std_msgs::msg::ColorRGBA u;

            @verbatim (language="comment", text=
                "std_msgs/Duration v")
            std_msgs::msg::Empty w;

            std_msgs::msg::Float32 x;

            std_msgs::msg::Float32MultiArray y;

            std_msgs::msg::Float64 z;

            std_msgs::msg::Float64MultiArray aa;

            std_msgs::msg::Header bb;

            std_msgs::msg::Int16 cc;

            std_msgs::msg::Int16MultiArray dd;

            std_msgs::msg::Int32 ee;

            std_msgs::msg::Int32MultiArray ff;

            std_msgs::msg::Int64 gg;

            std_msgs::msg::Int64MultiArray hh;

            std_msgs::msg::Int8 ii;

            std_msgs::msg::Int8MultiArray jj;

            std_msgs::msg::MultiArrayDimension kk;

            std_msgs::msg::MultiArrayLayout ll;

            std_msgs::msg::String mm;

            @verbatim (language="comment", text=
                "std_msgs/Time nn")
            std_msgs::msg::UInt16 oo;

            std_msgs::msg::UInt16MultiArray pp;

            std_msgs::msg::UInt32 qq;

            std_msgs::msg::UInt32MultiArray rr;

            std_msgs::msg::UInt64 ss;

            std_msgs::msg::UInt64MultiArray tt;

            std_msgs::msg::UInt8 uu;

            std_msgs::msg::UInt8MultiArray vv;

            @verbatim (language="comment", text=
                "default")
            @default (value=40)
            int32 ww;
        };
    };
};"#;

        match parse(input).finish() {
            Ok((_, definitions)) => {
                println!("{:?}", definitions);
            }
            Err(e) => {
                let msg = convert_error(input, e);
                eprintln!("{msg}");
                panic!();
            }
        }
    }
}
