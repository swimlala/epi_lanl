CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  $   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-09-18T22:00:28Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  B    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  D$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  L�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  N�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Wh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  _�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  b   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  j�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  l�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  u`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  }�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20190918220028  20190918220028  5904057 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5006                            2B  A   NAVIS_A                         0305                            082713                          863 @�ݞ��_R1   @�ݟ����@6��1&��e$�hr�1   GPS     Primary sampling: mixed [deep: discrete, shallow: continuous]                                                                                                                                                                                                      �A   A   A   @���@���@���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB  B   B(  B/��B8  B@  BH  BP  BX  B`  Bh  Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D&��D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�3D�#311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@p��@��@�A\)A7\)AW\)Aw\)A��A��A��A��AˮAۮA�A��B�
B=pB�
B�
B%�
B-p�B5�
B=�
BE�
BM�
BU�
B]�
Be�
Bmp�Bu�
B}�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��Cu�Cu�Cu�Cu�C	u�Cu�Cu�Cu�Cu�Cu�Cu�Cu�Cu�Cu�Cu�Cu�C!u�C#u�C%u�C'u�C)u�C+u�C-u�C/u�C1u�C3u�C5u�C7u�C9u�C;u�C=u�C?u�CAu�CCu�CEu�CGu�CIu�CKu�CMu�COu�CQu�CSu�CUu�CWu�CYu�C[u�C]u�C_u�Cau�Ccu�Ceu�Cgu�Ciu�Cku�Cmu�Cou�Cqu�Csu�Cuu�Cwu�Cyu�C{u�C}u�Cu�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ǮC���C���Cº�Cú�Cĺ�Cź�Cƺ�CǺ�CȮCɺ�Cʺ�C˺�C̺�Cͺ�Cκ�CϺ�Cк�CѺ�CҺ�CӺ�CԺ�Cպ�Cֺ�C׺�Cغ�Cٺ�Cں�Cۺ�C�ǮCݺ�C޺�Cߺ�C��C��C��C��C��C��C��C��C��C�ǮC��C��C��C���C��C��C��C��C��C��C���C���C���C���C���C���C���C���C���C���C���C���D ]qD �qD]qD�qD]qD�qD]qD�qD]qD�qD]qD�qD]qD�qD]qD�qD]qD�qD	]qD	�qD
]qD
�qD]qD�qD]qD�qD]qD�qD]qD�qD]qD�qD]qD�qD]qD�qD]qD�qD]qD�qD]qD�qD]qD�qD]qD�qD]qD�qD]qD�qD]qD�qD]qD�qD]qD�qD]qD�qD]qD�qD]qD�qD]qD�qD ]qD �qD!]qD!�qD"]qD"�qD#]qD#�qD$]qD$�qD%]qD%�qD&]qD&�D']qD'�qD(]qD(�qD)]qD)�qD*]qD*�qD+]qD+�qD,]qD,�qD-]qD-�qD.]qD.�qD/]qD/�qD0]qD0�qD1]qD1�qD2]qD2�qD3]qD3�qD4]qD4�qD5]qD5�qD6]qD6�qD7]qD7�qD8]qD8�qD9]qD9�qD:]qD:�qD;]qD;�qD<]qD<�qD=]qD=�qD>]qD>�qD?]qD?�qD@]qD@�qDA]qDA�qDB]qDB�qDC]qDC�qDD]qDD�qDE]qDE�qDF]qDF�qDG]qDG�qDH]qDH�qDI]qDI�qDJ]qDJ�qDK]qDK�qDL]qDL�qDM]qDM�qDN]qDN�qDO]qDO�qDP]qDP�qDQ]qDQ�qDR]qDR�qDS]qDS�qDT]qDT�qDU]qDU�qDV]qDV�qDW]qDW�qDX]qDX�qDY]qDY�qDZ]qDZ�qD[]qD[�qD\]qD\�qD]]qD]�qD^]qD^�qD_]qD_�qD`]qD`�qDa]qDa�qDb]qDb�qDc]qDc�qDd]qDd�qDe]qDe�qDf]qDf�qDg]qDg�qDh]qDh�qDi]qDi�qDj]qDj�qDk]qDk�qDl]qDl�qDm]qDm�qDn]qDn�qDo]qDo�qDp]qDp�qDq]qDq�qDr]qDr�qDs]qDs�qDt]qDt�qDu]qDu�qDv]qDv�qDw]qDw�qDx]qDx�qDy]qDy�qDz]qDz�qD{]qD{�qD|]qD|�qD}]qD}�qD~]qD~�qD]qD�qD�.�D�n�D���D��D�.�D�n�D���D��D�.�D�n�D���D��D�.�D�n�D���D��D�.�D�n�D���D��D�.�D�n�D���D��D�.�D�n�D���D��D�.�D�n�D���D���D�.�D�n�D���D���D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�S�A�S�A�VA�bNA��uA���A��A���A��\A�9A�^A���A���A��
A���A���A��/A��A���A�A���A�%A���A�p�A�+A�A�^5AͬA˙�A��A��Aț�AǙ�A���Aź^A�G�A���A���A�5?A�1'A�/A��jA�
=A��jA��!A��A�O�A�$�A���A�1A��DA�hsA�?}A��A��DA�33A��9A��FA�
=A��A���A�jA�5?A��hA���A�$�A��DA��7A��DA�ĜA�-A���A�{A��A��7A�5?A��A�\)A�l�A�?}A���A��PA��7A�dZA�bNA�  A���A�`BA�p�A�%A��7A�hsA�E�A�t�A�A���A�M�A�1A��FA�1A�bA���A�^5A���A�VA���A�~�A~��A|�jAy�AwƨAw33Av�At~�AtM�As�FAr�yAp�9Am��Ak��Aj��Aj$�AhbAgO�AfE�Ad��Ac�
AaA^-A\ȴA[�mAZ�AW
=ATZAR�`AP�AN�9AM�AL-AK|�AI�wAH��AG�AE&�AB�HA@VA?&�A>r�A=�A;�A;VA:�A9VA7XA6^5A4ffA0�\A.��A.M�A.=qA.$�A-�TA-��A-��A-C�A,ȴA+�;A+%A*1'A)K�A(v�A'�#A'p�A&�yA&~�A&5?A%��A$��A$�A#�7A#VA" �A I�AS�A�
A�PAdZA�A�+A��A��AdZA��A%A �A��A"�A��A(�A�A�7AoA�uA�FAS�AK�A/A�A�A��AA|�A��A�\A{A�
A�A33A
bNA	l�A�TA�mAAjA �A ĜA �9A Q�@���@���@�J@���@��@���@�
=@�O�@�+@��@��@�@���@�@�j@��m@�M�@�"�@�/@���@�j@�(�@��@��@�@�dZ@�{@ܬ@� �@�  @�  @۾w@�n�@�p�@�Z@ם�@��y@�M�@��@Ցh@�%@�ƨ@��@�%@�+@�^5@���@̬@�+@�=q@ȴ9@�ƨ@��H@�E�@ź^@��@�r�@��@�;d@�V@�/@�1'@�@���@��@�dZ@��!@�^5@��@��7@���@�C�@�@��7@���@���@��w@�+@���@��+@�n�@�7L@�1@�t�@��y@���@�G�@��@���@�ƨ@�@��y@��!@�=q@�5?@��@�@��-@�hs@��9@�Q�@�9X@��@�C�@��H@���@�-@��@���@�z�@�bN@�bN@�  @�  @���@���@��
@�C�@���@���@�~�@�V@�V@��+@���@���@��@��/@���@��@��/@�z�@�ƨ@��@��@�^5@���@��-@��#@�J@��/@���@�S�@�l�@��@�l�@�@�-@���@�@���@��7@��7@��-@��@��@��`@���@�r�@��
@���@�S�@�C�@�
=@��!@�E�@�x�@��j@�b@���@�|�@���@�v�@�n�@�n�@�ff@�ff@�ff@�n�@�^5@�V@�=q@�@��#@�@���@��h@�x�@�X@��@��`@���@�z�@�Z@�9X@��;@���@�|�@�33@���@�n�@�M�@�=q@�{@��@�@�`B@�V@���@��D@�Z@�I�@�  @���@�+@�"�@���@��@��+@�=q@��@���@��7@�7L@���@��u@��D@�r�@�Z@�1'@�1@���@�K�@�"�@���@��@�ȴ@��!@���@�v�@�V@�=q@�@��T@���@�p�@�p�@�O�@�V@�Ĝ@���@�bN@�(�@���@��@�l�@�;d@��@�
=@��H@��\@�~�@�E�@�{@���@���@��7@�p�@�?}@�V@��/@�Ĝ@���@��u@�z�@�j@�A�@��@�@|�@�@~��@~V@~$�@}�@}@}@}�h@}V@|I�@{��@z�@z��@zM�@zJ@y�7@x��@xbN@x �@w�;@wK�@v��@vȴ@v��@vff@u@u�h@uO�@uV@tz�@t(�@s�m@s�@s33@r��@rJ@qG�@q�@p��@pĜ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�S�A�S�A�VA�bNA��uA���A��A���A��\A�9A�^A���A���A��
A���A���A��/A��A���A�A���A�%A���A�p�A�+A�A�^5AͬA˙�A��A��Aț�AǙ�A���Aź^A�G�A���A���A�5?A�1'A�/A��jA�
=A��jA��!A��A�O�A�$�A���A�1A��DA�hsA�?}A��A��DA�33A��9A��FA�
=A��A���A�jA�5?A��hA���A�$�A��DA��7A��DA�ĜA�-A���A�{A��A��7A�5?A��A�\)A�l�A�?}A���A��PA��7A�dZA�bNA�  A���A�`BA�p�A�%A��7A�hsA�E�A�t�A�A���A�M�A�1A��FA�1A�bA���A�^5A���A�VA���A�~�A~��A|�jAy�AwƨAw33Av�At~�AtM�As�FAr�yAp�9Am��Ak��Aj��Aj$�AhbAgO�AfE�Ad��Ac�
AaA^-A\ȴA[�mAZ�AW
=ATZAR�`AP�AN�9AM�AL-AK|�AI�wAH��AG�AE&�AB�HA@VA?&�A>r�A=�A;�A;VA:�A9VA7XA6^5A4ffA0�\A.��A.M�A.=qA.$�A-�TA-��A-��A-C�A,ȴA+�;A+%A*1'A)K�A(v�A'�#A'p�A&�yA&~�A&5?A%��A$��A$�A#�7A#VA" �A I�AS�A�
A�PAdZA�A�+A��A��AdZA��A%A �A��A"�A��A(�A�A�7AoA�uA�FAS�AK�A/A�A�A��AA|�A��A�\A{A�
A�A33A
bNA	l�A�TA�mAAjA �A ĜA �9A Q�@���@���@�J@���@��@���@�
=@�O�@�+@��@��@�@���@�@�j@��m@�M�@�"�@�/@���@�j@�(�@��@��@�@�dZ@�{@ܬ@� �@�  @�  @۾w@�n�@�p�@�Z@ם�@��y@�M�@��@Ցh@�%@�ƨ@��@�%@�+@�^5@���@̬@�+@�=q@ȴ9@�ƨ@��H@�E�@ź^@��@�r�@��@�;d@�V@�/@�1'@�@���@��@�dZ@��!@�^5@��@��7@���@�C�@�@��7@���@���@��w@�+@���@��+@�n�@�7L@�1@�t�@��y@���@�G�@��@���@�ƨ@�@��y@��!@�=q@�5?@��@�@��-@�hs@��9@�Q�@�9X@��@�C�@��H@���@�-@��@���@�z�@�bN@�bN@�  @�  @���@���@��
@�C�@���@���@�~�@�V@�V@��+@���@���@��@��/@���@��@��/@�z�@�ƨ@��@��@�^5@���@��-@��#@�J@��/@���@�S�@�l�@��@�l�@�@�-@���@�@���@��7@��7@��-@��@��@��`@���@�r�@��
@���@�S�@�C�@�
=@��!@�E�@�x�@��j@�b@���@�|�@���@�v�@�n�@�n�@�ff@�ff@�ff@�n�@�^5@�V@�=q@�@��#@�@���@��h@�x�@�X@��@��`@���@�z�@�Z@�9X@��;@���@�|�@�33@���@�n�@�M�@�=q@�{@��@�@�`B@�V@���@��D@�Z@�I�@�  @���@�+@�"�@���@��@��+@�=q@��@���@��7@�7L@���@��u@��D@�r�@�Z@�1'@�1@���@�K�@�"�@���@��@�ȴ@��!@���@�v�@�V@�=q@�@��T@���@�p�@�p�@�O�@�V@�Ĝ@���@�bN@�(�@���@��@�l�@�;d@��@�
=@��H@��\@�~�@�E�@�{@���@���@��7@�p�@�?}@�V@��/@�Ĝ@���@��u@�z�@�j@�A�@��@�@|�@�@~��@~V@~$�@}�@}@}@}�h@}V@|I�@{��@z�@z��@zM�@zJ@y�7@x��@xbN@x �@w�;@wK�@v��@vȴ@v��@vff@u@u�h@uO�@uV@tz�@t(�@s�m@s�@s33@r��@rJ@qG�@q�@p��@pĜ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�3B�3B�3B�XB��B��B�
B��B��B�/B�;B�mB�sB�yB�fB�sB�B�B��B��B��B#�BS�B�\B��B��B��B��B�B�B�B�9B�LB�RB�RBBŢBɺB��B��BɺBȴBB��B��BB�dB�-B��B��B��B��B��B�\B�PB�1B�B�B�7B�+B�%B�B� By�Bu�B`BBH�B>wB6FB/B'�B �B�B�B{B\B
=BB��B�yB�TB�5B��B�RB�B��B��B��Bz�BXBP�BM�BJ�B?}B+B&�B�B�BB
�yB
�B
ɺB
�FB
�B
�\B
l�B
P�B
?}B
1'B
�B
uB
VB

=B	��B	��B	��B	�B	�TB	��B	ƨB	��B	�^B	�!B	��B	��B	��B	�uB	�B	t�B	l�B	ffB	]/B	N�B	B�B	=qB	9XB	-B	&�B	�B	�B	VB	+B��B��B�B�B�sB�fB�mB�NB�5B�)B�#B�B��B��BĜB��B��B��B�}B�wB�qB�qB�dB�RB�9B�!B�B��B��B��B��B��B��B��B��B��B��B��B�uB�\B�=B�+B�B�B�B�B�B~�Bx�Bw�Bt�Bp�Bm�Bk�Bk�BjBjBjBl�Bl�BjBgmBffBffBffBgmBdZBe`BgmBffBe`BffBffBffBe`Be`BiyBffBbNB[#BS�BW
B]/B]/B]/B]/B^5B^5B^5B^5B]/B]/B\)B\)B\)B]/B^5B^5B`BB`BB_;B^5B_;BaHBaHBgmBjBjBl�Bm�Bo�Bp�Bq�Bs�Bt�Bt�Bt�Bs�Bv�Bx�Bz�B|�B~�B� B�B�B�B�B�B�%B�=B�DB�DB�PB�bB�hB��B��B��B��B��B��B��B��B��B��B�B�!B�-B�?B�XB�jB�}B��B��BBƨB��B��B��B��B�B�B�#B�/B�/B�/B�NB�mB�yB�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	B	%B	1B	1B	
=B	DB	DB	DB	JB	bB	oB	�B	�B	�B	#�B	$�B	&�B	'�B	(�B	+B	,B	,B	,B	-B	.B	/B	/B	/B	.B	/B	0!B	5?B	:^B	>wB	C�B	C�B	D�B	C�B	B�B	C�B	E�B	G�B	G�B	G�B	G�B	H�B	I�B	I�B	M�B	P�B	Q�B	W
B	YB	\)B	^5B	cTB	e`B	gmB	iyB	k�B	l�B	m�B	n�B	o�B	q�B	t�B	u�B	v�B	v�B	w�B	x�B	x�B	y�B	y�B	{�B	}�B	~�B	�B	�B	�%B	�1B	�=B	�DB	�PB	�bB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�3B	�9B	�FB	�LB	�^B	�dB	�jB	�jB	�wB	��B	ÖB	ŢB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�5B	�;B	�HB	�HB	�HB	�NB	�TB	�`B	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
+B
1B
	7B
	7B

=B
DB
DB
DB
DB
PB
PB
PB
VB
\B
\B
bB
bB
bB
hB
oB
uB
uB
uB
u11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�3B�3B�3B�XB��B��B�
B��B��B�/B�;B�mB�sB�yB�fB�sB�B�B��B��B��B#�BS�B�\B��B��B��B��B�B�B�B�9B�LB�RB�RBBŢBɺB��B��BɺBȴBB��B��BB�dB�-B��B��B��B��B��B�\B�PB�1B�B�B�7B�+B�%B�B� By�Bu�B`BBH�B>wB6FB/B'�B �B�B�B{B\B
=BB��B�yB�TB�5B��B�RB�B��B��B��Bz�BXBP�BM�BJ�B?}B+B&�B�B�BB
�yB
�B
ɺB
�FB
�B
�\B
l�B
P�B
?}B
1'B
�B
uB
VB

=B	��B	��B	��B	�B	�TB	��B	ƨB	��B	�^B	�!B	��B	��B	��B	�uB	�B	t�B	l�B	ffB	]/B	N�B	B�B	=qB	9XB	-B	&�B	�B	�B	VB	+B��B��B�B�B�sB�fB�mB�NB�5B�)B�#B�B��B��BĜB��B��B��B�}B�wB�qB�qB�dB�RB�9B�!B�B��B��B��B��B��B��B��B��B��B��B��B�uB�\B�=B�+B�B�B�B�B�B~�Bx�Bw�Bt�Bp�Bm�Bk�Bk�BjBjBjBl�Bl�BjBgmBffBffBffBgmBdZBe`BgmBffBe`BffBffBffBe`Be`BiyBffBbNB[#BS�BW
B]/B]/B]/B]/B^5B^5B^5B^5B]/B]/B\)B\)B\)B]/B^5B^5B`BB`BB_;B^5B_;BaHBaHBgmBjBjBl�Bm�Bo�Bp�Bq�Bs�Bt�Bt�Bt�Bs�Bv�Bx�Bz�B|�B~�B� B�B�B�B�B�B�%B�=B�DB�DB�PB�bB�hB��B��B��B��B��B��B��B��B��B��B�B�!B�-B�?B�XB�jB�}B��B��BBƨB��B��B��B��B�B�B�#B�/B�/B�/B�NB�mB�yB�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	B	%B	1B	1B	
=B	DB	DB	DB	JB	bB	oB	�B	�B	�B	#�B	$�B	&�B	'�B	(�B	+B	,B	,B	,B	-B	.B	/B	/B	/B	.B	/B	0!B	5?B	:^B	>wB	C�B	C�B	D�B	C�B	B�B	C�B	E�B	G�B	G�B	G�B	G�B	H�B	I�B	I�B	M�B	P�B	Q�B	W
B	YB	\)B	^5B	cTB	e`B	gmB	iyB	k�B	l�B	m�B	n�B	o�B	q�B	t�B	u�B	v�B	v�B	w�B	x�B	x�B	y�B	y�B	{�B	}�B	~�B	�B	�B	�%B	�1B	�=B	�DB	�PB	�bB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�3B	�9B	�FB	�LB	�^B	�dB	�jB	�jB	�wB	��B	ÖB	ŢB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�5B	�;B	�HB	�HB	�HB	�NB	�TB	�`B	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
+B
1B
	7B
	7B

=B
DB
DB
DB
DB
PB
PB
PB
VB
\B
\B
bB
bB
bB
hB
oB
uB
uB
uB
u11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.54 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20190918220028                              AO  ARCAADJP                                                                    20190918220028    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20190918220028  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20190918220028  QCF$                G�O�G�O�G�O�0               