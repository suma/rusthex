fn main() {
    #[cfg(target_os = "windows")]
    {
        use image::GenericImageView;
        use std::io::Write;
        use std::path::Path;

        let out_dir = std::env::var("OUT_DIR").unwrap();
        let ico_path = Path::new(&out_dir).join("icon.ico");

        // Generate .ico from assets/icon.png
        let img = image::open("assets/icon.png").expect("Failed to open assets/icon.png");

        // ICO sizes: 16, 32, 48, 64, 128, 256
        let sizes: &[u32] = &[16, 32, 48, 64, 128, 256];
        let mut png_data: Vec<Vec<u8>> = Vec::new();

        for &s in sizes {
            let resized = img.resize_exact(s, s, image::imageops::FilterType::Lanczos3);
            let mut buf = std::io::Cursor::new(Vec::new());
            resized
                .write_to(&mut buf, image::ImageFormat::Png)
                .expect("Failed to encode PNG");
            png_data.push(buf.into_inner());
        }

        // Write ICO file
        let num_images = sizes.len() as u16;
        let mut ico = Vec::new();

        // ICO header: reserved(2) + type(2) + count(2) = 6 bytes
        ico.write_all(&0u16.to_le_bytes()).unwrap(); // reserved
        ico.write_all(&1u16.to_le_bytes()).unwrap(); // type: 1 = ICO
        ico.write_all(&num_images.to_le_bytes()).unwrap();

        // Calculate data offset: header(6) + entries(16 * count)
        let mut data_offset = 6u32 + 16 * num_images as u32;

        // Write directory entries
        for (i, &s) in sizes.iter().enumerate() {
            let w = if s >= 256 { 0u8 } else { s as u8 }; // 0 means 256
            let h = w;
            ico.push(w); // width
            ico.push(h); // height
            ico.push(0); // color palette count
            ico.push(0); // reserved
            ico.write_all(&1u16.to_le_bytes()).unwrap(); // color planes
            ico.write_all(&32u16.to_le_bytes()).unwrap(); // bits per pixel
            ico.write_all(&(png_data[i].len() as u32).to_le_bytes())
                .unwrap(); // data size
            ico.write_all(&data_offset.to_le_bytes()).unwrap(); // data offset
            data_offset += png_data[i].len() as u32;
        }

        // Write PNG data
        for data in &png_data {
            ico.write_all(data).unwrap();
        }

        std::fs::write(&ico_path, &ico).expect("Failed to write icon.ico");

        // Embed icon in executable
        let mut res = winres::WindowsResource::new();
        res.set_icon(ico_path.to_str().unwrap());
        res.compile().expect("Failed to compile Windows resources");
    }
}
