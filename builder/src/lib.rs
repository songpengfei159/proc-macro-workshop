use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::ToTokens;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{parse_macro_input, DeriveInput, Field, Token, LitStr};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = parse_macro_input!(input as DeriveInput);
    match do_expand(&st) {
        Ok(expanded) => expanded.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

type StructFields = Punctuated<Field, Token![,]>;
fn get_field_names_from_derive_input(st: &DeriveInput) -> syn::Result<&StructFields> {
    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = st.data
    {
        return Ok(named);
    };
    Err(syn::Error::new_spanned(
        st,
        "Must define a struct with named fields",
    ))
}

// 产生builder结构体的字段定义
fn generate_builder_struct_fields_def(
    st: &syn::DeriveInput,
) -> syn::Result<proc_macro2::TokenStream> {
    let fields = get_field_names_from_derive_input(&st)?;
    let idents: Vec<_> = fields.iter().map(|f| &f.ident).collect();
    let types: syn::Result<Vec<proc_macro2::TokenStream>> = fields.iter().map(|f| {
        if let Some(inter_type)= get_option_inner_type(&f.ty,"Option") {
            Ok(quote::quote! {
                std::option::Option<#inter_type>
            })
        } else if get_user_specified_ident_for_vec(f)?.is_some(){
            let origin_ty = &f.ty;
            Ok(quote::quote!(#origin_ty))
        }else {
            let original_type = &f.ty;
            Ok(quote::quote! {
                std::option::Option<#original_type>
            })
        }
    }).collect();
    let types = types?;
    let ret = quote::quote! {
        #(
            #idents: #types,
        )*
    };
    Ok(ret)
}

// 产生builder结构体的初始化语句
fn generate_builder_struct_factory_init_clauses(
    st: &syn::DeriveInput,
) -> syn::Result<Vec<proc_macro2::TokenStream>> {
    let fields = get_field_names_from_derive_input(&st)?;
    let idents: syn::Result<Vec<proc_macro2::TokenStream>> = fields.iter().map(|f|{
        let ident = &f.ident;
        if get_user_specified_ident_for_vec(f)?.is_some() {
            Ok(quote::quote!{
                #ident: std::vec::Vec::new()
            })
        } else {
            Ok(quote::quote!{
                #ident: std::option::Option::None
            })
        }
    }).collect();

    Ok(idents?)
}

// 产生builder结构体的setter方法
fn generate_builder_setters(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let fields = get_field_names_from_derive_input(&st)?;
    let idents: Vec<_> = fields.iter().map(|f| &f.ident).collect();
    let types: Vec<_> = fields.iter().map(|f|&f.ty ).collect();
    let mut ret = proc_macro2::TokenStream::new();
    for (idx,(ident, type_)) in idents.iter().zip(types.iter()).enumerate() {
        let token_stream = if let Some(inner_type) = get_option_inner_type(type_,"Option"){
            quote::quote! (
                pub fn #ident(&mut self, #ident: #inner_type) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            )
        } else if let Some(ref use_specified_ident) = get_user_specified_ident_for_vec(&fields[idx])?{
            let inner_type = get_option_inner_type(&type_,"Vec").ok_or(
                syn::Error::new(fields[idx].span(),"Vec type must have inner type"))?;
            let mut t_token_stream_piece = quote::quote! {
                pub fn #use_specified_ident(&mut self, #use_specified_ident: #inner_type) -> &mut Self {
                    self.#ident.push(#use_specified_ident);
                    self
                }
            };
            if use_specified_ident != ident.as_ref().unwrap() {
                let token_stream = quote::quote! {
                    pub fn #ident(&mut self, #ident: #inner_type) -> &mut Self {
                        self.#ident.push(#ident);
                        self
                    }
                };
                t_token_stream_piece.extend(token_stream);
            }
            t_token_stream_piece
        } else{
            quote::quote! (
                pub fn #ident(&mut self, #ident: #type_) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            )
        };
        ret.extend(token_stream);
    }
    Ok(ret)
}

// 产生builder结构体的验证和构建方法
fn generate_builder_verify_and_build_method(
    st: &syn::DeriveInput,
) -> syn::Result<proc_macro2::TokenStream> {
    let fields = get_field_names_from_derive_input(&st)?;
    let types: Vec<_> = fields.iter().map(|f|&f.ty ).collect();
    let mut verify_ident = proc_macro2::TokenStream::new();
    for idx in 0..fields.len() {
        let ident = &fields[idx].ident;
        let type_ = &types[idx];
        if get_option_inner_type(type_,"Option").is_none()&& get_user_specified_ident_for_vec(&fields[idx])?.is_none() {
            let token_stream = quote::quote! {
            if self.#ident.is_none() {
                let err = format!("Field {} not set", stringify!(#ident));
                return std::result::Result::Err(err.into())
            }
        };
            verify_ident.extend(token_stream);
        }
    }
    let mut fill_struct = proc_macro2::TokenStream::new();
    for idx in 0..fields.len() {
        let ident = &fields[idx].ident;
        let type_ = &types[idx];
        if get_user_specified_ident_for_vec(&fields[idx])?.is_some(){
            let token_stream = quote::quote! {
                #ident: self.#ident.clone(),
            };
            fill_struct.extend(token_stream);
        } else if get_option_inner_type(type_, "Option").is_some() {
            let token_stream = quote::quote! {
                #ident: self.#ident.clone(),
            };
            fill_struct.extend(token_stream);
        } else {
            let token_stream = quote::quote! {
                #ident: self.#ident.clone().unwrap(),
            };
            fill_struct.extend(token_stream);
        }

    }

    let original_struct_ident = &st.ident;
    let to = quote::quote! {
         pub fn build(&mut self) -> std::result::Result<#original_struct_ident, std::boxed::Box<dyn std::error::Error>> {
            #verify_ident
            let ret = #original_struct_ident {
              #fill_struct
            };
            Ok(ret)
         }
    };
    Ok(to.into())
}

fn get_option_inner_type<'a>(t: &'a syn::Type, outer_ident_name: &str) -> Option<&'a syn::Type> {
    if let syn::Type::Path(syn::TypePath{
            path: syn::Path{
                segments,
                ..
            },
            ..
        }) = t {
        if let Some(seg) = segments.last(){
            if seg.ident == outer_ident_name  {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments{
                    args,
                    ..
                }) = &seg.arguments {
                    if let Some(syn::GenericArgument::Type(t)) = args.first(){
                        return Some(t);
                    }
                }
            }
        }
    }
    None
}

fn get_user_specified_ident_for_vec(field: &syn::Field) -> syn::Result<Option<Ident>> {
    let mut align: Option<syn::Ident> = None;
    for attr in &field.attrs {
        if attr.path().is_ident("builder") {         // this parses the `builder`
            attr.parse_nested_meta(|meta| {      // this parses the `(`
                let value = meta.value()?;   // this parses the `=`
                let s: LitStr = value.parse()?;  // this parses `""`
                if meta.path.is_ident("each") {  // this parses the `each`
                    align = Some(syn::Ident::new(&s.value(), s.span()));
                    Ok(())
                } else {
                    Err(syn::Error::new_spanned(attr.meta.to_token_stream(),r#"expected `builder(each = "...")`"#))
                }
            })?;
        }
    }
    Ok(align)
}

fn do_expand(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_name_literal = &st.ident.to_string();
    let builder_name_literal = format!("{}Builder", struct_name_literal);
    let builder_name_ident = syn::Ident::new(&builder_name_literal, st.span());
    let struct_ident = &st.ident;
    let builder_fields = generate_builder_struct_fields_def(&st)?;
    let builder_init_clauses = generate_builder_struct_factory_init_clauses(&st)?;
    let builder_setters = generate_builder_setters(&st)?;
    let builder_verify_and_build_method = generate_builder_verify_and_build_method(&st)?;
    let ret = quote::quote! {
        impl #struct_ident {
            pub fn builder() -> #builder_name_ident {
                #builder_name_ident {
                    #(#builder_init_clauses),*
                }
            }
        }
        pub struct #builder_name_ident {
            #builder_fields
        }
        impl #builder_name_ident {
            #builder_setters
            #builder_verify_and_build_method
        }
    };
    Ok(ret)
}
