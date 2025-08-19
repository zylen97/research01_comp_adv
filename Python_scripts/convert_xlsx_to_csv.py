import pandas as pd
import os

def convert_xlsx_to_csv():
    """
    将rev06.xlsx转换为rev06.csv文件
    """
    print("=== 开始转换rev06.xlsx到CSV格式 ===")
    
    # 设置文件路径
    xlsx_file = '../data/rev06.xlsx'
    csv_file = '../data/rev06.csv'
    
    try:
        # 读取Excel文件
        print("正在读取rev06.xlsx...")
        df = pd.read_excel(xlsx_file)
        
        print(f"数据形状: {df.shape}")
        print(f"列名: {list(df.columns)}")
        
        # 保存为CSV文件
        print("正在保存为CSV格式...")
        df.to_csv(csv_file, index=False, encoding='utf-8-sig')
        
        # 验证文件是否创建成功
        if os.path.exists(csv_file):
            file_size = os.path.getsize(csv_file)
            print(f"文件大小: {file_size} bytes")
        
        print("转换完成: " + csv_file)
        print(f"数据包含 {len(df)} 行, {len(df.columns)} 列")
        
        # 显示前几行数据
        print("\n前5行数据预览:")
        print(df.head())
        
        return True
        
    except Exception as e:
        print(f"转换失败: {str(e)}")
        return False

if __name__ == "__main__":
    convert_xlsx_to_csv()