"""
ユーティリティ関数モジュール
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023"

import os
import logging
import pandas as pd
from pathlib import Path
from typing import List, Union

logger = logging.getLogger(__name__)


def read_text_file(filepath: str, encoding: str = 'utf-8') -> List[str]:
    """
    テキストファイルを読み込んで行のリストを返す

    Parameters:
        filepath (str): ファイルパス
        encoding (str): 文字エンコーディング

    Returns:
        List[str]: 行のリスト

    Raises:
        FileNotFoundError: ファイルが存在しない場合
        PermissionError: ファイルの読み込み権限がない場合
        UnicodeDecodeError: エンコーディングが正しくない場合
    """
    try:
        with open(filepath, 'r', encoding=encoding) as f:
            lines = [line.strip() for line in f if line.strip()]
        return lines
    except FileNotFoundError:
        logger.error(f"File not found: {filepath}")
        raise
    except PermissionError:
        logger.error(f"Permission denied: {filepath}")
        raise
    except UnicodeDecodeError as e:
        logger.error(f"Encoding error in {filepath}: {e}")
        raise


def read_csv_file(filepath: str, column: str = None, encoding: str = 'utf-8') -> List[str]:
    """
    CSVファイルを読み込んでテキストのリストを返す

    Parameters:
        filepath (str): ファイルパス
        column (str): 抽出する列名（Noneの場合は最初の列）
        encoding (str): 文字エンコーディング

    Returns:
        List[str]: テキストのリスト

    Raises:
        FileNotFoundError: ファイルが存在しない場合
        pd.errors.EmptyDataError: ファイルが空の場合
        KeyError: 指定した列が存在しない場合
    """
    try:
        df = pd.read_csv(filepath, encoding=encoding)
        if column is None:
            column = df.columns[0]
        return df[column].dropna().astype(str).tolist()
    except FileNotFoundError:
        logger.error(f"CSV file not found: {filepath}")
        raise
    except pd.errors.EmptyDataError:
        logger.error(f"CSV file is empty: {filepath}")
        raise
    except KeyError:
        logger.error(f"Column '{column}' not found in {filepath}")
        raise


def read_tsv_file(filepath: str, column: str = None, encoding: str = 'utf-8') -> List[str]:
    """
    TSVファイルを読み込んでテキストのリストを返す

    Parameters:
        filepath (str): ファイルパス
        column (str): 抽出する列名（Noneの場合は最初の列）
        encoding (str): 文字エンコーディング

    Returns:
        List[str]: テキストのリスト

    Raises:
        FileNotFoundError: ファイルが存在しない場合
        pd.errors.EmptyDataError: ファイルが空の場合
        KeyError: 指定した列が存在しない場合
    """
    try:
        df = pd.read_csv(filepath, sep='\t', encoding=encoding)
        if column is None:
            column = df.columns[0]
        return df[column].dropna().astype(str).tolist()
    except FileNotFoundError:
        logger.error(f"TSV file not found: {filepath}")
        raise
    except pd.errors.EmptyDataError:
        logger.error(f"TSV file is empty: {filepath}")
        raise
    except KeyError:
        logger.error(f"Column '{column}' not found in {filepath}")
        raise


def read_file(filepath: str, column: str = None, encoding: str = 'utf-8') -> List[str]:
    """
    ファイルを読み込んでテキストのリストを返す
    拡張子に応じて適切な読み込み方法を選択

    Parameters:
        filepath (str): ファイルパス
        column (str): CSV/TSVの場合の列名
        encoding (str): 文字エンコーディング

    Returns:
        List[str]: テキストのリスト
    """
    ext = Path(filepath).suffix.lower()

    if ext == '.csv':
        return read_csv_file(filepath, column, encoding)
    elif ext == '.tsv':
        return read_tsv_file(filepath, column, encoding)
    elif ext in ['.txt', '.text']:
        return read_text_file(filepath, encoding)
    else:
        # デフォルトはテキストファイルとして扱う
        return read_text_file(filepath, encoding)


def read_files(filepaths: List[str], column: str = None, encoding: str = 'utf-8') -> List[str]:
    """
    複数のファイルを読み込んでテキストのリストを返す

    Parameters:
        filepaths (List[str]): ファイルパスのリスト
        column (str): CSV/TSVの場合の列名
        encoding (str): 文字エンコーディング

    Returns:
        List[str]: テキストのリスト
    """
    all_texts = []
    for filepath in filepaths:
        texts = read_file(filepath, column, encoding)
        all_texts.extend(texts)
    return all_texts


def export_to_csv(df: pd.DataFrame, filepath: str, encoding: str = 'utf-8-sig'):
    """
    DataFrameをCSVファイルに出力

    Parameters:
        df (pd.DataFrame): 出力するDataFrame
        filepath (str): 出力先ファイルパス
        encoding (str): 文字エンコーディング（デフォルトはBOM付きUTF-8）
    """
    df.to_csv(filepath, index=False, encoding=encoding)


def export_to_json(df: pd.DataFrame, filepath: str, encoding: str = 'utf-8'):
    """
    DataFrameをJSONファイルに出力

    Parameters:
        df (pd.DataFrame): 出力するDataFrame
        filepath (str): 出力先ファイルパス
        encoding (str): 文字エンコーディング
    """
    df.to_json(filepath, orient='records', force_ascii=False, indent=2)


def export_to_excel(df: pd.DataFrame, filepath: str):
    """
    DataFrameをExcelファイルに出力

    Parameters:
        df (pd.DataFrame): 出力するDataFrame
        filepath (str): 出力先ファイルパス
    """
    df.to_excel(filepath, index=False, engine='openpyxl')


def ensure_directory(filepath: str):
    """
    ファイルパスのディレクトリが存在することを確認し、なければ作成

    Parameters:
        filepath (str): ファイルパス
    """
    directory = os.path.dirname(filepath)
    if directory and not os.path.exists(directory):
        os.makedirs(directory)
