import os
import logging
import base64
from pathlib import Path
import asyncio
import pandas as pd
from anthropic import AsyncAnthropic
from dotenv import load_dotenv
from PyPDF2 import PdfReader, PdfWriter
import io

# Setup logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
load_dotenv()
CLAUDE_APIKEY = os.getenv('CLAUDE_APIKEY')
client = AsyncAnthropic(api_key=CLAUDE_APIKEY)

MAX_RETRIES = 3
RETRY_BACKOFF_BASE = 3

PROMPT = """IMPORTANT: Output ONLY the text of the uploaded bill with the specified markup. DO NOT summarize, analyze, or add any commentary.
The uploaded pdf is a legislative bill. Your task is to process it as follows:
1. Identify any text that is underlined (indicating insertion) and wrap it with: <u class="amendmentInsertedText"> and </u>
2. Identify any text that is with strikethrough/strikeout (indicating deletion) and wrap it with: <strike class="amendmentDeletedText"> and </strike>
3. Remove any line numbers appearing in the original document.
4. Output the complete bill text with these markup tags in place.

Carefully examine the entire document for ANY text formatting including underlines, strikethroughs, bold, or italics, even if they span multiple lines or paragraphs. 

If the bill contains no underlines, strikethroughs, or strikeouts, simply output the plain text of the bill without any markup. 

Remove any boilerplate information like page numbers and line numbers. Output only the substantive legislative content.

Pay special attention to long paragraphs or sections that may be entirely underlined or struck through, not just individual words or phrases. For substantial insertions that span multiple lines or paragraphs, identify the beginning and end of the entire amended block. Wrap the whole block with a single set of <u class="amendmentInsertedText"> and </u> tags (for insertions) or <strike class="amendmentDeletedText"> and </strike> (for deletions). Do not tag each line individually if they are part of the same continuous inserted or deleted section. Make sure you pay close attention to instances where deletions and insertions appear next to each other. Make sure to tag them correctly.

Your response must contain ONLY the processed bill text - no introduction, explanation, or commentary of any kind."""

async def scrape_text(pdf_path, semaphore):
    async with semaphore:
        try:
            await asyncio.sleep(10)  # Initial sleep to respect rate limit

            reader = PdfReader(pdf_path)
            total_pages = len(reader.pages)

            if total_pages <= 40:
                # Handle small PDFs as before
                logging.info(f"Uploading PDF file: {pdf_path}")
                with open(pdf_path, "rb") as pdf_file:
                    base64_string = base64.b64encode(pdf_file.read()).decode("utf-8")
                
                response_text = await process_pdf_chunk(base64_string, pdf_path, 1, 1)
            
            else:
                # Break into 40-page chunks
                chunk_size = 40
                num_chunks = (total_pages + chunk_size - 1) // chunk_size  # Ceiling division
                logging.info(f"Breaking {pdf_path} into {num_chunks} chunks ({total_pages} pages)")
                
                all_responses = []
                
                for chunk_num in range(num_chunks):
                    start_page = chunk_num * chunk_size
                    end_page = min(start_page + chunk_size, total_pages)
                    
                    logging.info(f"Processing chunk {chunk_num + 1}/{num_chunks} (pages {start_page + 1}-{end_page}) of {pdf_path}")
                    
                    # Create chunk PDF
                    writer = PdfWriter()
                    for i in range(start_page, end_page):
                        writer.add_page(reader.pages[i])
                    
                    # Write to bytes buffer
                    pdf_buffer = io.BytesIO()
                    writer.write(pdf_buffer)
                    pdf_buffer.seek(0)
                    
                    # Encode chunk
                    base64_string = base64.b64encode(pdf_buffer.read()).decode("utf-8")
                    
                    # Process chunk with Claude
                    chunk_response = await process_pdf_chunk(base64_string, pdf_path, chunk_num + 1, num_chunks)
                    all_responses.append(chunk_response)
                    
                    # Add delay between chunks to respect rate limits
                    if chunk_num < num_chunks - 1:  # Don't sleep after last chunk
                        await asyncio.sleep(5)
                
                # Combine all responses
                response_text = combine_responses(all_responses, pdf_path)

            # Save combined response
            txt_path = f"{os.path.splitext(pdf_path)[0]}_html.txt"
            with open(txt_path, "w", encoding="utf-8") as f:
                f.write(response_text)

            logging.info(f"Completed: {pdf_path}")
            return {"pdf_path": pdf_path, "status": "success", "text_path": txt_path}

        except Exception as e:
            logging.error(f"Failed on {pdf_path}: {e}")
            return {"pdf_path": pdf_path, "status": "failed", "error": str(e)}


async def process_pdf_chunk(base64_string, pdf_path, chunk_num, total_chunks):
    """Process a single PDF chunk with Claude"""
    messages = [
        {
            "role": "user",
            "content": [
                {
                    "type": "document",
                    "source": {
                        "type": "base64",
                        "media_type": "application/pdf",
                        "data": base64_string,
                    },
                },
                {"type": "text", "text": PROMPT},
            ],
        }
    ]

    for attempt in range(1, MAX_RETRIES + 1):
        try:
            logging.info(f"Streaming Claude response for chunk {chunk_num}/{total_chunks} of {pdf_path} (attempt {attempt})")
            async with client.messages.stream(
                model="claude-3-7-sonnet-20250219",
                max_tokens=64000,
                messages=messages,
            ) as stream:
                response_text = ""
                async for chunk in stream.text_stream:
                    response_text += chunk
            return response_text
        except Exception as stream_error:
            logging.warning(f"Stream attempt {attempt} failed for chunk {chunk_num} of {pdf_path}: {stream_error}")
            if attempt == MAX_RETRIES:
                raise  # Final failure
            backoff_time = RETRY_BACKOFF_BASE * (attempt + 1)
            logging.info(f"Retrying in {backoff_time}s...")
            await asyncio.sleep(backoff_time)


def combine_responses(responses, pdf_path):
    """Combine multiple chunk responses into one coherent response"""
    if len(responses) == 1:
        return responses[0]
    
    # Add chunk headers and combine
    combined = f"# Combined Analysis for {os.path.basename(pdf_path)}\n\n"
    
    for i, response in enumerate(responses, 1):
        combined += f"## Part {i}\n\n"
        combined += response
        combined += "\n\n---\n\n"
    
    return combined.rstrip("\n\n---\n\n")  # Remove trailing separator


async def main():
    df = pd.read_csv("text/state-scrapers/vt_bill_text_files.csv")

    text_dir = Path("/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/vermont")
    # Regex pattern to remove '_<number>_html.txt' at the end
    existing_uuids = {
        f.parent.name for f in text_dir.rglob("*_html.txt")
    }
   
    df = df[~df["UUID"].isin(existing_uuids)]

    pdf_paths = [p for p in df["file_path"].dropna().tolist() if os.path.exists(p)]

    logging.info(f"Processing {len(pdf_paths)} files with async Claude...")

    # Limit to 6 concurrent jobs
    semaphore = asyncio.Semaphore(6)

    tasks = [scrape_text(path, semaphore) for path in pdf_paths]
    results = await asyncio.gather(*tasks)

if __name__ == "__main__":
    asyncio.run(main())
    