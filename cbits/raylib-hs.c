#include <stdlib.h>
#include "raylib.h"

// core
void C_ClearBackground(Color *color)
{
  ClearBackground(*color);
}

// texture
Texture2D *C_LoadTexture(const char *fileName)
{
  Texture2D *result = malloc(sizeof *result);
  *result = LoadTexture(fileName);
  return result;
}

void C_UnloadTexture(Texture2D *texture)
{
  UnloadTexture(*texture);
  free(texture);
}

void C_DrawTextureEx(Texture2D *texture, Vector2 *position, float rotation, float scale, Color *tint)
{
  DrawTextureEx(*texture, *position, rotation, scale, *tint);
}

void C_DrawTexturePro(Texture2D *texture, Rectangle *source, Rectangle *dest, Vector2 *origin, float rotation, Color *tint)
{
  DrawTexturePro(*texture, *source, *dest, *origin, rotation, *tint);
}

// text
Font *C_LoadFont(const char *fileName)
{
  Font *result = malloc(sizeof *result);
  *result = LoadFont(fileName);
  return result;
}

void C_UnloadFont(Font *font)
{
  UnloadFont(*font);
  free(font);
}

void C_DrawText(const char *text, int posX, int posY, int fontSize, Color *color)
{
  DrawText(text, posX, posY, fontSize, *color);
}

void C_DrawTextEx(Font *font, const char *text, Vector2 *position, float fontSize, float spacing, Color *tint)
{
  DrawTextEx(*font, text, *position, fontSize, spacing, *tint);
}

// audio
Sound *C_LoadSound(const char *fileName)
{
  Sound *result = malloc(sizeof *result);
  *result = LoadSound(fileName);
  return result;
}

void C_UnloadSound(Sound *sound)
{
  UnloadSound(*sound);
  free(sound);
}

void C_PlaySound(Sound *sound)
{
  PlaySound(*sound);
}
