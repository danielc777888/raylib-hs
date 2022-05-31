// core
void C_ClearBackground(Color *color);

// texture
Texture2D *C_LoadTexture(const char *fileName);
void C_UnloadTexture(Texture2D *texture);
void C_DrawTextureEx(Texture2D *texture, Vector2 *position, float rotation, float scale, Color *tint);
void C_DrawTexturePro(Texture2D *texture, Rectangle *source, Rectangle *dest, Vector2 *origin, float rotation, Color *tint);

// text
Font *C_LoadFont(const char *fileName);
void C_UnloadFont(Font *font);
void C_DrawText(const char *text, int posX, int posY, int fontSize, Color *color);
void C_DrawTextEx(Font *font, const char *text, Vector2 *position, float fontSize, float spacing, Color *tint);

// audio
Sound *C_LoadSound(const char *fileName);
void C_UnloadSound(Sound *sound);
void C_PlaySound(Sound *sound);
