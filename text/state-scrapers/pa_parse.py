import os
import logging
import asyncio
import pandas as pd
from pathlib import Path
from dotenv import load_dotenv
from PyPDF2 import PdfReader, PdfWriter
import io
from google import genai
from concurrent.futures import ThreadPoolExecutor

# Setup logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
load_dotenv()
GEMINI_APIKEY = os.getenv('GEMINI_APIKEY')
client = genai.Client(api_key=GEMINI_APIKEY)

MAX_RETRIES = 3
RETRY_BACKOFF_BASE = 3

PROMPT = """IMPORTANT: Output ONLY the text of the uploaded bill with the specified markup. DO NOT summarize, analyze, or add any commentary. The uploaded pdf is a legislative bill. Your task is to process it as follows: 
1. Identify any text that is underlined (indicating insertion) and wrap it with: <u class="amendmentInsertedText"> and </u> 
2. Identify any text that is surrounded in brackets (indicating deletion) and wrap it with: <strike class="amendmentDeletedText"> and </strike> 
3. Remove any line numbers appearing in the original document. 
4. Output the complete bill text with these markup tags in place.

Carefully examine the entire document for ANY text formatting including underlines and brackets, even if they span multiple lines or paragraphs. If the bill contains no underlines or brackets, simply output the plain text of the bill without any markup.

Remove any boilerplate information like page numbers and line numbers. Remove all horizontal lines, decorative borders, and visual separators from the original document. Output only the substantive legislative content.

Pay special attention to long paragraphs or sections that may be entirely underlined or struck through, not just individual words or phrases. For substantial insertions or deletions that span multiple lines or paragraphs, identify the beginning and end of the entire amended block. Wrap the whole block with a single set of <u class="amendmentInsertedText"> and </u> tags (for insertions) or <strike class="amendmentDeletedText"> and </strike> (for deletions). Do not tag each line individually if they are part of the same continuous inserted or deleted section. Make sure you pay close attention to instances where deletions and insertions appear next to each other. Make sure to tag them correctly.

Your response must contain ONLY the processed bill text - no introduction, explanation, or commentary of any kind."""

async def scrape_text(pdf_path, semaphore):
    async with semaphore:
        try:
            await asyncio.sleep(5)  # Initial sleep to respect rate limit

            reader = PdfReader(pdf_path)
            total_pages = len(reader.pages)

            if total_pages <= 40:
                # Handle small PDFs as before
                logging.info(f"Processing PDF file: {pdf_path}")
                response_text = await process_pdf_chunk(pdf_path, pdf_path, 1, 1)
            
            else:
                # Break into 40-page chunks
                chunk_size = 40
                num_chunks = (total_pages + chunk_size - 1) // chunk_size  # Ceiling division
                logging.info(f"Breaking {pdf_path} into {num_chunks} chunks ({total_pages} pages)")
                logging.info(f"Chunk calculation: {total_pages} pages ÷ {chunk_size} = {num_chunks} chunks")
                
                all_responses = []
                
                for chunk_num in range(num_chunks):
                    start_page = chunk_num * chunk_size
                    end_page = min(start_page + chunk_size, total_pages)
                    
                    logging.info(f"Processing chunk {chunk_num + 1}/{num_chunks} (pages {start_page + 1}-{end_page}) of {pdf_path}")
                    logging.info(f"  -> This chunk will have {end_page - start_page} pages")
                    
                    # Create chunk PDF
                    writer = PdfWriter()
                    for i in range(start_page, end_page):
                        writer.add_page(reader.pages[i])
                    
                    # Create chunk PDF file
                    writer = PdfWriter()
                    for i in range(start_page, end_page):
                        writer.add_page(reader.pages[i])
                    
                    # Write chunk to temporary file
                    chunk_path = f"{pdf_path}_chunk_{chunk_num + 1}.pdf"
                    with open(chunk_path, "wb") as chunk_file:
                        writer.write(chunk_file)
                    
                    try:
                        # Process chunk with Gemini
                        chunk_response = await process_pdf_chunk(chunk_path, pdf_path, chunk_num + 1, num_chunks)
                        all_responses.append(chunk_response)
                    finally:
                        # Clean up temporary chunk file
                        if os.path.exists(chunk_path):
                            os.remove(chunk_path)
                    
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


async def process_pdf_chunk(chunk_pdf_path, original_pdf_path, chunk_num, total_chunks):
    """Process a single PDF chunk with Gemini API"""
    
    def _sync_gemini_call():
        """Synchronous wrapper for Gemini API call"""
        # Upload PDF to Gemini
        uploaded_file = client.files.upload(file=chunk_pdf_path)
        
        try:
            # Generate content using Gemini
            response = client.models.generate_content(
                model="gemini-2.5-flash-preview-04-17",
                contents=[PROMPT, uploaded_file]
            )
            return response.text
        finally:
            # Clean up uploaded file
            try:
                client.files.delete(name=uploaded_file.name)
            except Exception as cleanup_error:
                logging.warning(f"Failed to delete uploaded file {uploaded_file.name}: {cleanup_error}")

    for attempt in range(1, MAX_RETRIES + 1):
        try:
            logging.info(f"Processing with Gemini for chunk {chunk_num}/{total_chunks} of {original_pdf_path} (attempt {attempt})")
            
            # Run the synchronous Gemini call in a thread pool
            loop = asyncio.get_event_loop()
            with ThreadPoolExecutor(max_workers=1) as executor:
                response_text = await loop.run_in_executor(executor, _sync_gemini_call)
            
            return response_text
            
        except Exception as gemini_error:
            logging.warning(f"Gemini attempt {attempt} failed for chunk {chunk_num} of {original_pdf_path}: {gemini_error}")
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
    df = pd.read_csv("text/state-scrapers/pa_bill_text_files.csv")

    text_dir = Path("/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/election_bill_text/data/pennsylvania")
    # Regex pattern to remove '_<number>_html.txt' at the end
    existing_uuids = {
        f.parent.name for f in text_dir.rglob("*_html.txt")
    }
   
    df = df[~df["UUID"].isin(existing_uuids)]

    pdf_paths = [p for p in df["file_path"].dropna().tolist() if os.path.exists(p)]

    logging.info(f"Processing {len(pdf_paths)} files with async Gemini...")

    # Limit to 6 concurrent jobs (you may want to reduce this for Gemini rate limits)
    semaphore = asyncio.Semaphore(6)

    tasks = [scrape_text(path, semaphore) for path in pdf_paths]
    results = await asyncio.gather(*tasks)

if __name__ == "__main__":
    asyncio.run(main())