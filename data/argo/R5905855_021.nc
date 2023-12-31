CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:14:25Z creation;2022-06-04T19:14:26Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604191425  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @��,�{B_1   @��-��\�@.ݲ-V�d�/��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bo��Bx  B�33B���B���B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�33B�  B�  B���B�  B�  B�  B�  B�  B�33B�ffB�  B���B�  B�  B�  C   C  C  C�fC  C
  C  C  C  C  C�C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:L�C;��C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn�Cp  Cr  Ct  Cv  Cw�fCz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DOfDO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʃ3D��3D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�p 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�H@���@���A Q�A Q�A@Q�A`Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B@{BH{BP{BX{B`{Bh{Bo�Bx{B�=pB���B���B��
B���B��
B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B���B�=pB�
=B�
=B��
B�
=B�
=B�
=B�
=B�
=B�=pB�p�B�
=B��
B�
=B�
=B�
=C CCC�CC
CCCCC�C�CCCCC C"C$C&C(C*C,C.C0C2C4C6C8C:Q�C;��C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjCl�Cn�CpCrCtCvCw�CzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDO�DO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʃ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�@�D怤D���D� �D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�}qD���D� �D�@�D���D���D� �D�@�D�p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��;A���A��A��A��A��JA���A��]A��.A���A���A���A��yA��AฆA���A�kQA��A���A���A��A��A߽AߢhAߍPA�gA�1�A��[AޔFA�lWA�0UA��;A�a�A��mA���Aӏ�A�e`A�#�A��KA�PHA�`vA�V�A�OAǱ'A�[WA�2�A�"4Aö�A�l"A���A�!bA��A��tA��5A��2A�l�A�{�A�XEA���A�,qA��jA�E�A��^A�[�A�J�A�ɺA��WA�B�A�	A��5A�7A���A��A���A�G�A�/OA��'A��(A��A�gmA���A��DA��BA�z�A��|A�ںA���A��tA��A���A��jA��A�TaA��A���A��A�)�AzFAwE�At�jAq/�Ao��AmAi�7Ae�AbQ�A^�A\=qAX^�AW��AVsAR�AP��ALoiAH��AE~�AA�A@�AA?ƨA>FtA<�A:s�A7?A4�A1�]A13�A0)_A.��A,MjA(�rA'�!A'�A&[�A%�A$9XA$�A#]�A"��A"�A"��A"xA!F�A!$A ��A�]A&A�bAn/A�LA��AS&ARTA�A1'A��A�AںA9XAh�AVmAA��A  A�A5?A�ZA��A�A˒A�A8�A�zA�XA��AXA��A��A�.A�rAK�AxA�CA@OAA�mA��A�<A��A�#AE�A
�A	bNA�5AںAiDA�AݘA�RA��A˒As�A�TAVA�0AL0A�]A�TA�UA�A#:A�&A��AA Xy@�u%@�f�@��O@�O@���@�6z@���@�5?@�v`@�!�@��@�@��@�g�@�@��@�+k@�E9@���@�u%@�@��f@�z�@��@�m�@�"h@틬@�\@��@�L�@�V@��@�5?@��K@�@�h�@䅈@�%@�@�Z�@���@�7@�2a@���@���@�v`@�S&@��@ީ�@ކY@�A�@�  @�*0@�=q@ۅ�@���@�[�@��@��A@��Q@�}�@�9�@�]d@��j@�s@��@ַ�@��@�n/@Ե@�c @�L0@���@ӌ~@�dZ@��@�ߤ@��X@ҭ�@�~�@�+k@��P@�ȴ@�h�@��>@�7L@�e@�%F@�}V@�=q@�M@���@�x�@�Y@���@���@ʑ�@��@��@ɜ@�O@ȧ@���@�+@��[@�d�@���@�J�@�%@Ĳ�@�Z�@â�@��@�@�@��}@�(@�j@�c�@�Ta@��@�P�@�{�@��;@�dZ@���@�  @�j@���@��.@�c�@�=q@��0@��=@�~�@��@��@�2�@���@��f@���@�j@�G@��@��@�O@�;@��4@�ff@�5?@��@��X@�C@���@��}@���@�\�@�C-@� �@��@��S@��8@��y@��@�H�@���@���@��@�N<@�@���@��8@���@�4@��)@���@�@�v�@�=q@�	@��@��@��}@���@�!-@���@�A�@��r@�@���@�*0@���@�(�@��>@��@��n@���@�s�@�:*@��@��@�}�@�6z@��@���@�z@��@��@��=@�&�@���@�H�@���@���@�RT@�%F@���@���@���@�?@�@��H@�f�@�C@��H@���@�V�@�!�@��@��~@�g�@��@���@�#:@���@��q@���@�ѷ@��h@��@�v�@�	@���@��{@�=�@�(@��$@�V@��A@���@�w2@�0�@���@���@�]d@�(�@��'@�RT@�;d@��@��`@�u%@���@�_p@�33@��5@��x@�c�@�6@��@��T@���@�qv@�:�@���@�ѷ@��@�{�@�Ft@��@��@��
@���@�+�@���@�1�@�
�@���@��M@�A @���@��e@�z�@�_�@�M@��@���@�ƨ@��t@���@�}�@� i@�ѷ@��@�M�@�2�@��@��@���@��M@�b�@�=@��@��y@���@�y>@�L0@�J�@�1�@�˒@�N<@�(@�V@���@�҉@��@�8�@��@���@�B�@��@��@��#@���@�F@��@�%@���@���@�j@�GE@�$�@��@��@���@�iD@�A @��@��@�ȴ@��<@��4@�!@�q@��@v`@�@~��@~_�@}�-@}O�@|�@|Q�@|7�@{�+@{��@{@zR�@z)�@z �@yϫ@yx�@y�@x��@xC-@w�;@w�*@w�@v��@v($@u�@u0�@t�@t��@t'R@s��@sRT@sJ#@r�s@rv�@q�'@q�@p��@p`�@o8@n�s@n�+@nJ�@nJ@m�T@mԕ@m�@m�h@mq@l��@l�z@lg8@l'R@k��@k�P@kS�@j�"@j�B@jp;@i��@i�7@i@@h��@g�Q@g�f@gA�@f�<@e�@e��@eDg@e%@d�@dV�@c�A@ca@b��@bR�@bu@a��@a|@`�@`�Y@`j@`$@_�}@_�@^�@^xl@^�@]��@]��@]%@\z�@\tT@\"h@[RT@[$t@Z�@Z�h@Z3�@Y�@Y�3@Y��@YS&@Y%@X�@W��@W��@W;d@V�8@Vp;@V&�@U�#@U�@UrG@Uq@T�@T�u@T'R@T  @S�6@S�@R��@R�<@R�'@R��@R;�@Ru@Q��@Qc�@QB�@Q�@P��@P��@P>B@O��@O��@Ox@OW?@O>�@O8@O!-@N�]@Nn�@N �@M��@M�M@L�5@L��@L��@Lz�@LA�@K�@KS�@K i@J�r@J:*@I�9@IQ�@H��@H�@G�q@Gv`@G;d@F҉@F:*@Ec@Ex�@E`B@E%@D�5@DɆ@D�@DU2@DD�@DG@C��@C�$@C��@CMj@C�@B�]@B�F@B:*@B!�@A��@Ak�@@��@@1'@?��@?��@?iD@?P�@?.I@?�@>�c@>�B@>��@>�r@>5?@=�3@=c�@=7L@=#�@<��@<�z@<%�@;�{@:�M@:��@:^5@:0U@9Vm@8�j@8�e@86@7��@7_p@6��@6�<@6�A@6�@5��@5�n@5J�@5q@4�@4��@4��@4��@4[�@3�@3�4@2�@2q�@2
�@1�-@1Y�@1&�@0�@0�D@0`�@/�@/�V@/"�@.~�@.3�@-�@-T�@-�@,�@,Q�@,M@+�*@+,�@*��@*�b@*O@)�@)zx@)J�@)!�@(�f@(�@(D�@'�Q@'��@'�@&�M@&�s@&��@&kQ@&+k@%�@%�'@%e,@%�@$��@$m�@$�@#��@#y�@#E9@"��@"�@"Ov@"
�@!��@!��@!c@!IR@!A @!7L@!&�@ �	@ ѷ@ m�@ �@�@�:@v`@U�@)_@��@��@ff@J�@4@��@x�@#�@�|@�U@u�@A�@6@�@��@�*@��@.I@�@�M@�@��@^5@+k@��@��@Q�@0�@V@��@q@>B@!@�@�q@��@U�@9�@)_@Y@��@҉@��@_�@6�@
�@��@�@@��@^�@7L@@�@�p@�I@_@9X@�W@�K@�@@��@dZ@C�@�@��@�A@n�@Z�@B[@�@�@�3@�7@j@#�@�@��@��@U2@�A@��@�V@�f@t�@e�@E9@,�@�@Y@�c@��@�}@� @{�@W�@+k@��@�T@@�-@��@�@Y�@�5@��@�.@|�@h�@S�@1'@@1@�m@�a@�@�{@g�@]�@J#@
�]@
�+@
�A@
i�@
E�@
�@	�#@	�H@	�M@	^�@	:�@	#�@�	@�v1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��;A���A��A��A��A��JA���A��]A��.A���A���A���A��yA��AฆA���A�kQA��A���A���A��A��A߽AߢhAߍPA�gA�1�A��[AޔFA�lWA�0UA��;A�a�A��mA���Aӏ�A�e`A�#�A��KA�PHA�`vA�V�A�OAǱ'A�[WA�2�A�"4Aö�A�l"A���A�!bA��A��tA��5A��2A�l�A�{�A�XEA���A�,qA��jA�E�A��^A�[�A�J�A�ɺA��WA�B�A�	A��5A�7A���A��A���A�G�A�/OA��'A��(A��A�gmA���A��DA��BA�z�A��|A�ںA���A��tA��A���A��jA��A�TaA��A���A��A�)�AzFAwE�At�jAq/�Ao��AmAi�7Ae�AbQ�A^�A\=qAX^�AW��AVsAR�AP��ALoiAH��AE~�AA�A@�AA?ƨA>FtA<�A:s�A7?A4�A1�]A13�A0)_A.��A,MjA(�rA'�!A'�A&[�A%�A$9XA$�A#]�A"��A"�A"��A"xA!F�A!$A ��A�]A&A�bAn/A�LA��AS&ARTA�A1'A��A�AںA9XAh�AVmAA��A  A�A5?A�ZA��A�A˒A�A8�A�zA�XA��AXA��A��A�.A�rAK�AxA�CA@OAA�mA��A�<A��A�#AE�A
�A	bNA�5AںAiDA�AݘA�RA��A˒As�A�TAVA�0AL0A�]A�TA�UA�A#:A�&A��AA Xy@�u%@�f�@��O@�O@���@�6z@���@�5?@�v`@�!�@��@�@��@�g�@�@��@�+k@�E9@���@�u%@�@��f@�z�@��@�m�@�"h@틬@�\@��@�L�@�V@��@�5?@��K@�@�h�@䅈@�%@�@�Z�@���@�7@�2a@���@���@�v`@�S&@��@ީ�@ކY@�A�@�  @�*0@�=q@ۅ�@���@�[�@��@��A@��Q@�}�@�9�@�]d@��j@�s@��@ַ�@��@�n/@Ե@�c @�L0@���@ӌ~@�dZ@��@�ߤ@��X@ҭ�@�~�@�+k@��P@�ȴ@�h�@��>@�7L@�e@�%F@�}V@�=q@�M@���@�x�@�Y@���@���@ʑ�@��@��@ɜ@�O@ȧ@���@�+@��[@�d�@���@�J�@�%@Ĳ�@�Z�@â�@��@�@�@��}@�(@�j@�c�@�Ta@��@�P�@�{�@��;@�dZ@���@�  @�j@���@��.@�c�@�=q@��0@��=@�~�@��@��@�2�@���@��f@���@�j@�G@��@��@�O@�;@��4@�ff@�5?@��@��X@�C@���@��}@���@�\�@�C-@� �@��@��S@��8@��y@��@�H�@���@���@��@�N<@�@���@��8@���@�4@��)@���@�@�v�@�=q@�	@��@��@��}@���@�!-@���@�A�@��r@�@���@�*0@���@�(�@��>@��@��n@���@�s�@�:*@��@��@�}�@�6z@��@���@�z@��@��@��=@�&�@���@�H�@���@���@�RT@�%F@���@���@���@�?@�@��H@�f�@�C@��H@���@�V�@�!�@��@��~@�g�@��@���@�#:@���@��q@���@�ѷ@��h@��@�v�@�	@���@��{@�=�@�(@��$@�V@��A@���@�w2@�0�@���@���@�]d@�(�@��'@�RT@�;d@��@��`@�u%@���@�_p@�33@��5@��x@�c�@�6@��@��T@���@�qv@�:�@���@�ѷ@��@�{�@�Ft@��@��@��
@���@�+�@���@�1�@�
�@���@��M@�A @���@��e@�z�@�_�@�M@��@���@�ƨ@��t@���@�}�@� i@�ѷ@��@�M�@�2�@��@��@���@��M@�b�@�=@��@��y@���@�y>@�L0@�J�@�1�@�˒@�N<@�(@�V@���@�҉@��@�8�@��@���@�B�@��@��@��#@���@�F@��@�%@���@���@�j@�GE@�$�@��@��@���@�iD@�A @��@��@�ȴ@��<@��4@�!@�q@��@v`@�@~��@~_�@}�-@}O�@|�@|Q�@|7�@{�+@{��@{@zR�@z)�@z �@yϫ@yx�@y�@x��@xC-@w�;@w�*@w�@v��@v($@u�@u0�@t�@t��@t'R@s��@sRT@sJ#@r�s@rv�@q�'@q�@p��@p`�@o8@n�s@n�+@nJ�@nJ@m�T@mԕ@m�@m�h@mq@l��@l�z@lg8@l'R@k��@k�P@kS�@j�"@j�B@jp;@i��@i�7@i@@h��@g�Q@g�f@gA�@f�<@e�@e��@eDg@e%@d�@dV�@c�A@ca@b��@bR�@bu@a��@a|@`�@`�Y@`j@`$@_�}@_�@^�@^xl@^�@]��@]��@]%@\z�@\tT@\"h@[RT@[$t@Z�@Z�h@Z3�@Y�@Y�3@Y��@YS&@Y%@X�@W��@W��@W;d@V�8@Vp;@V&�@U�#@U�@UrG@Uq@T�@T�u@T'R@T  @S�6@S�@R��@R�<@R�'@R��@R;�@Ru@Q��@Qc�@QB�@Q�@P��@P��@P>B@O��@O��@Ox@OW?@O>�@O8@O!-@N�]@Nn�@N �@M��@M�M@L�5@L��@L��@Lz�@LA�@K�@KS�@K i@J�r@J:*@I�9@IQ�@H��@H�@G�q@Gv`@G;d@F҉@F:*@Ec@Ex�@E`B@E%@D�5@DɆ@D�@DU2@DD�@DG@C��@C�$@C��@CMj@C�@B�]@B�F@B:*@B!�@A��@Ak�@@��@@1'@?��@?��@?iD@?P�@?.I@?�@>�c@>�B@>��@>�r@>5?@=�3@=c�@=7L@=#�@<��@<�z@<%�@;�{@:�M@:��@:^5@:0U@9Vm@8�j@8�e@86@7��@7_p@6��@6�<@6�A@6�@5��@5�n@5J�@5q@4�@4��@4��@4��@4[�@3�@3�4@2�@2q�@2
�@1�-@1Y�@1&�@0�@0�D@0`�@/�@/�V@/"�@.~�@.3�@-�@-T�@-�@,�@,Q�@,M@+�*@+,�@*��@*�b@*O@)�@)zx@)J�@)!�@(�f@(�@(D�@'�Q@'��@'�@&�M@&�s@&��@&kQ@&+k@%�@%�'@%e,@%�@$��@$m�@$�@#��@#y�@#E9@"��@"�@"Ov@"
�@!��@!��@!c@!IR@!A @!7L@!&�@ �	@ ѷ@ m�@ �@�@�:@v`@U�@)_@��@��@ff@J�@4@��@x�@#�@�|@�U@u�@A�@6@�@��@�*@��@.I@�@�M@�@��@^5@+k@��@��@Q�@0�@V@��@q@>B@!@�@�q@��@U�@9�@)_@Y@��@҉@��@_�@6�@
�@��@�@@��@^�@7L@@�@�p@�I@_@9X@�W@�K@�@@��@dZ@C�@�@��@�A@n�@Z�@B[@�@�@�3@�7@j@#�@�@��@��@U2@�A@��@�V@�f@t�@e�@E9@,�@�@Y@�c@��@�}@� @{�@W�@+k@��@�T@@�-@��@�@Y�@�5@��@�.@|�@h�@S�@1'@@1@�m@�a@�@�{@g�@]�@J#@
�]@
�+@
�A@
i�@
E�@
�@	�#@	�H@	�M@	^�@	:�@	#�@�	@�v1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	D�B	D�B	D�B	D�B	D�B	D�B	D�B	D�B	D�B	D�B	D�B	D�B	E9B	EB	E�B	F%B	GB	I�B	K�B	L�B	N<B	N�B	MPB	K�B	J�B	I�B	I�B	I�B	J�B	LB	K�B	K^B	J=B	VSB	��B
=B
9�B
�B
�B	�0B	�]B	��B	� B	�.B	�xB
#B
A�B
K�B
x�B
�2B
�FB
��B
�B
�B�BU�BkB�tB�$B��B�LB�cB��B�dB�~B�B�B��B�/B�aB�GB�wB�B�B�CBخB�B�B��B��B��B��B�B�{Bi_B.IB0B
�TB
��B
A�B
B	�B	�B	�bB	�xB	�IB	�2B	��B	�$B	�,B	|B	r�B	e�B	S�B	O�B	N�B	>BB	(�B	�B	hB	�B��B�B�yB��BȚB�ZB��B�nB�[B��B��B��B�
B�8B��B��B��B�0B��B�.B��B��BݲB߾B��B��B��B�%B		�B	"�B	)�B	*KB	)�B	($B	*�B	33B	-B	%�B	'�B	/5B	1�B	.�B	3B	5�B	LdB	^OB	f2B	jB	k�B	o�B	uB	u�B	zDB	|�B	��B	�B	��B	��B	��B	�NB	��B	�B	�7B	��B	�=B	��B	�dB	�OB	��B	�B	��B	�#B	��B	�uB	ԕB	ԯB	ңB	�[B	бB	�B	��B	��B	�<B	�B	�jB	�B	өB	��B	ϑB	ΊB	��B	��B	ȴB	�B	�B	�B	�B	��B	ȀB	��B	�B	��B	�KB	�B	ɆB	��B	��B	ɺB	�lB	�lB	��B	��B	�)B	�~B	�DB	��B	��B	̘B	��B	�BB	��B	��B	�B	�DB	ʌB	͹B	ΊB	��B	��B	��B	�.B	�HB	��B	�B	�&B	ѝB	ЗB	ЗB	��B	�4B	�hB	�hB	уB	��B	��B	ѷB	�:B	�[B	��B	��B	��B	��B	��B	��B	ԯB	�mB	յB	��B	յB	�sB	רB	��B	רB	רB	�?B	��B	�
B	��B	�EB	�+B	ؓB	�+B	�yB	�EB	�+B	�_B	��B	�+B	ؓB	خB	ؓB	ؓB	��B	�B	��B	�#B	�qB	�qB	�CB	��B	�OB	��B	�;B	�;B	�VB	ߊB	�B	��B	��B	��B	�B	��B	�\B	�vB	�HB	�4B	��B	�B	�B	�B	�B	�B	��B	�,B	�`B	�FB	��B	�fB	�B	�sB	�sB	�>B	�
B	��B	��B	�*B	�B	�B	�B	�B	��B	�=B	�B	�B	�B	�cB	��B	��B	�/B	�B	� B	�OB	�OB	��B	�B	�B	�B	��B	��B	�FB	��B	�tB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�zB	�2B	�2B	��B	��B	��B	�lB	�B	��B	��B	�$B	�XB	�*B	��B	�^B	��B	��B	�xB	�^B	�*B	��B	�B	�*B	��B	��B	�JB	�dB	�B	�6B	�<B	�B	�(B	��B	��B	�}B	��B
 4B
  B
 4B
 �B
 �B
 �B
 �B
'B
uB
[B
�B
GB
{B
�B
3B
�B
�B
�B
B
�B
SB
B
%B
�B
B
�B
�B
�B
�B
�B
�B
fB
KB
�B
	�B

�B

rB

rB

�B
B
)B
)B
DB
�B
�B
B
PB
�B
�B
�B
�B
(B
�B
B
�B
}B
�B
�B
�B
hB
 B
�B
bB
�B
hB
4B
 B
B
�B
�B
B
B
�B

B
?B
�B
B
+B
EB
�B
yB
�B
�B
�B
KB
eB
7B
QB
�B
=B
�B
xB
CB
�B
�B
�B
xB
�B
�B
/B
B
�B
;B
�B
 'B
 �B
!-B
!B
 �B
 \B
 vB
 \B
 \B
 vB
 �B
!B
!|B
!�B
"4B
"B
"B
"hB
#TB
#�B
#�B
#�B
$&B
$�B
%�B
%�B
&�B
&�B
'�B
(XB
(sB
(�B
)*B
)�B
)�B
*�B
+QB
+kB
+6B
+B
+kB
,=B
,qB
,�B
,�B
,�B
-wB
-�B
-�B
-�B
/ B
/5B
/OB
/�B
0!B
0!B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
1B
1�B
1�B
1vB
1�B
1�B
1�B
1�B
3hB
3�B
4B
4B
3�B
3MB
3�B
3�B
3�B
3�B
3�B
4TB
4nB
4�B
5�B
5�B
5�B
6+B
6`B
6`B
6`B
72B
72B
7�B
7�B
8B
8B
88B
88B
8RB
8�B
9	B
9	B
9>B
9�B
9�B
9rB
9�B
9�B
:DB
:�B
;0B
;�B
;�B
<PB
<�B
<�B
<�B
=B
=�B
=qB
=�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
?}B
@4B
A;B
AoB
AUB
A�B
AUB
A�B
B'B
BAB
B�B
B�B
CGB
CB
C�B
D3B
DB
DgB
DgB
D�B
DgB
D�B
ESB
E�B
E�B
E�B
EmB
E�B
FYB
FYB
F�B
F�B
GB
G_B
G+B
F�B
G+B
F�B
G+B
GEB
GEB
G�B
HKB
IB
I�B
I�B
JXB
JrB
J�B
J�B
KB
K)B
KDB
K�B
K�B
LJB
LJB
L~B
L�B
L�B
L�B
L�B
MB
MPB
MjB
M�B
M�B
NB
N"B
N"B
NB
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
P}B
P�B
P�B
P�B
P�B
QhB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
RB
R:B
R B
R�B
R�B
R�B
R�B
R�B
SB
S[B
S�B
S�B
S�B
T{B
TFB
T�B
T�B
UB
U2B
UgB
UMB
UgB
U�B
U�B
U�B
U�B
U�B
U�B
VB
V�B
V�B
V�B
V�B
V�B
W?B
W�B
X_B
X�B
X�B
X�B
YB
Y�B
Y�B
ZQB
Z�B
Z�B
[	B
[	B
[=B
[�B
[�B
[�B
\CB
\)B
\xB
\xB
\�B
\�B
\�B
\�B
]dB
^5B
^jB
^�B
^�B
_B
_!B
_;B
_pB
_pB
`B
`'B
`vB
aB
a-B
abB
a�B
a�B
bB
b�B
b�B
cB
c:B
cB
c:B
c�B
c�B
dB
dZB
dZB
d�B
d�B
e`B
e�B
e�B
fB
fLB
fLB
ffB
f�B
f�B
f�B
gRB
gRB
g�B
g�B
g�B
h>B
hsB
h�B
h�B
iB
iDB
i�B
i�B
j0B
jKB
jB
j�B
j�B
j�B
j�B
j�B
kB
kQB
k�B
k�B
k�B
k�B
lB
l"B
l�B
l�B
l�B
l�B
l�B
m)B
m]B
m�B
m�B
nB
ncB
ncB
n}B
n�B
n�B
n�B
o B
oOB
oOB
o�B
o�B
o�B
o�B
o�B
pUB
poB
p�B
poB
p�B
p�B
q'B
q[B
qvB
q�B
rB
rB
raB
raB
r|B
r|B
r�B
r�B
r�B
sB
s3B
s�B
shB
s�B
s�B
s�B
tB
tB
t9B
tnB
tTB
t�B
t�B
t�B
u%B
u?B
utB
utB
u�B
u�B
u�B
vFB
vzB
v�B
v�B
v�B
v�B
v�B
wB
wLB
wfB
w�B
w�B
xB
xB
xRB
x�B
y	B
y	B
y	B
y>B
y>B
yrB
yrB
yrB
yrB
y�B
y�B
y�B
zB
zB
z*B
zDB
z^B
zxB
z�B
z�B
z�B
z�B
z�B
{dB
{�B
{�B
{�B
{�B
|B
|6B
|PB
|6B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}qB
}VB
}VB
}<B
}qB
~B
~]B
~�B
~�B
~�B
~�B
~�B
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	D�B	D�B	D�B	D�B	D�B	D�B	D�B	D�B	D�B	D�B	D�B	D�B	E9B	EB	E�B	F%B	GB	I�B	K�B	L�B	N<B	N�B	MPB	K�B	J�B	I�B	I�B	I�B	J�B	LB	K�B	K^B	J=B	VSB	��B
=B
9�B
�B
�B	�0B	�]B	��B	� B	�.B	�xB
#B
A�B
K�B
x�B
�2B
�FB
��B
�B
�B�BU�BkB�tB�$B��B�LB�cB��B�dB�~B�B�B��B�/B�aB�GB�wB�B�B�CBخB�B�B��B��B��B��B�B�{Bi_B.IB0B
�TB
��B
A�B
B	�B	�B	�bB	�xB	�IB	�2B	��B	�$B	�,B	|B	r�B	e�B	S�B	O�B	N�B	>BB	(�B	�B	hB	�B��B�B�yB��BȚB�ZB��B�nB�[B��B��B��B�
B�8B��B��B��B�0B��B�.B��B��BݲB߾B��B��B��B�%B		�B	"�B	)�B	*KB	)�B	($B	*�B	33B	-B	%�B	'�B	/5B	1�B	.�B	3B	5�B	LdB	^OB	f2B	jB	k�B	o�B	uB	u�B	zDB	|�B	��B	�B	��B	��B	��B	�NB	��B	�B	�7B	��B	�=B	��B	�dB	�OB	��B	�B	��B	�#B	��B	�uB	ԕB	ԯB	ңB	�[B	бB	�B	��B	��B	�<B	�B	�jB	�B	өB	��B	ϑB	ΊB	��B	��B	ȴB	�B	�B	�B	�B	��B	ȀB	��B	�B	��B	�KB	�B	ɆB	��B	��B	ɺB	�lB	�lB	��B	��B	�)B	�~B	�DB	��B	��B	̘B	��B	�BB	��B	��B	�B	�DB	ʌB	͹B	ΊB	��B	��B	��B	�.B	�HB	��B	�B	�&B	ѝB	ЗB	ЗB	��B	�4B	�hB	�hB	уB	��B	��B	ѷB	�:B	�[B	��B	��B	��B	��B	��B	��B	ԯB	�mB	յB	��B	յB	�sB	רB	��B	רB	רB	�?B	��B	�
B	��B	�EB	�+B	ؓB	�+B	�yB	�EB	�+B	�_B	��B	�+B	ؓB	خB	ؓB	ؓB	��B	�B	��B	�#B	�qB	�qB	�CB	��B	�OB	��B	�;B	�;B	�VB	ߊB	�B	��B	��B	��B	�B	��B	�\B	�vB	�HB	�4B	��B	�B	�B	�B	�B	�B	��B	�,B	�`B	�FB	��B	�fB	�B	�sB	�sB	�>B	�
B	��B	��B	�*B	�B	�B	�B	�B	��B	�=B	�B	�B	�B	�cB	��B	��B	�/B	�B	� B	�OB	�OB	��B	�B	�B	�B	��B	��B	�FB	��B	�tB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�zB	�2B	�2B	��B	��B	��B	�lB	�B	��B	��B	�$B	�XB	�*B	��B	�^B	��B	��B	�xB	�^B	�*B	��B	�B	�*B	��B	��B	�JB	�dB	�B	�6B	�<B	�B	�(B	��B	��B	�}B	��B
 4B
  B
 4B
 �B
 �B
 �B
 �B
'B
uB
[B
�B
GB
{B
�B
3B
�B
�B
�B
B
�B
SB
B
%B
�B
B
�B
�B
�B
�B
�B
�B
fB
KB
�B
	�B

�B

rB

rB

�B
B
)B
)B
DB
�B
�B
B
PB
�B
�B
�B
�B
(B
�B
B
�B
}B
�B
�B
�B
hB
 B
�B
bB
�B
hB
4B
 B
B
�B
�B
B
B
�B

B
?B
�B
B
+B
EB
�B
yB
�B
�B
�B
KB
eB
7B
QB
�B
=B
�B
xB
CB
�B
�B
�B
xB
�B
�B
/B
B
�B
;B
�B
 'B
 �B
!-B
!B
 �B
 \B
 vB
 \B
 \B
 vB
 �B
!B
!|B
!�B
"4B
"B
"B
"hB
#TB
#�B
#�B
#�B
$&B
$�B
%�B
%�B
&�B
&�B
'�B
(XB
(sB
(�B
)*B
)�B
)�B
*�B
+QB
+kB
+6B
+B
+kB
,=B
,qB
,�B
,�B
,�B
-wB
-�B
-�B
-�B
/ B
/5B
/OB
/�B
0!B
0!B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
1B
1�B
1�B
1vB
1�B
1�B
1�B
1�B
3hB
3�B
4B
4B
3�B
3MB
3�B
3�B
3�B
3�B
3�B
4TB
4nB
4�B
5�B
5�B
5�B
6+B
6`B
6`B
6`B
72B
72B
7�B
7�B
8B
8B
88B
88B
8RB
8�B
9	B
9	B
9>B
9�B
9�B
9rB
9�B
9�B
:DB
:�B
;0B
;�B
;�B
<PB
<�B
<�B
<�B
=B
=�B
=qB
=�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
?}B
@4B
A;B
AoB
AUB
A�B
AUB
A�B
B'B
BAB
B�B
B�B
CGB
CB
C�B
D3B
DB
DgB
DgB
D�B
DgB
D�B
ESB
E�B
E�B
E�B
EmB
E�B
FYB
FYB
F�B
F�B
GB
G_B
G+B
F�B
G+B
F�B
G+B
GEB
GEB
G�B
HKB
IB
I�B
I�B
JXB
JrB
J�B
J�B
KB
K)B
KDB
K�B
K�B
LJB
LJB
L~B
L�B
L�B
L�B
L�B
MB
MPB
MjB
M�B
M�B
NB
N"B
N"B
NB
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
P}B
P�B
P�B
P�B
P�B
QhB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
RB
R:B
R B
R�B
R�B
R�B
R�B
R�B
SB
S[B
S�B
S�B
S�B
T{B
TFB
T�B
T�B
UB
U2B
UgB
UMB
UgB
U�B
U�B
U�B
U�B
U�B
U�B
VB
V�B
V�B
V�B
V�B
V�B
W?B
W�B
X_B
X�B
X�B
X�B
YB
Y�B
Y�B
ZQB
Z�B
Z�B
[	B
[	B
[=B
[�B
[�B
[�B
\CB
\)B
\xB
\xB
\�B
\�B
\�B
\�B
]dB
^5B
^jB
^�B
^�B
_B
_!B
_;B
_pB
_pB
`B
`'B
`vB
aB
a-B
abB
a�B
a�B
bB
b�B
b�B
cB
c:B
cB
c:B
c�B
c�B
dB
dZB
dZB
d�B
d�B
e`B
e�B
e�B
fB
fLB
fLB
ffB
f�B
f�B
f�B
gRB
gRB
g�B
g�B
g�B
h>B
hsB
h�B
h�B
iB
iDB
i�B
i�B
j0B
jKB
jB
j�B
j�B
j�B
j�B
j�B
kB
kQB
k�B
k�B
k�B
k�B
lB
l"B
l�B
l�B
l�B
l�B
l�B
m)B
m]B
m�B
m�B
nB
ncB
ncB
n}B
n�B
n�B
n�B
o B
oOB
oOB
o�B
o�B
o�B
o�B
o�B
pUB
poB
p�B
poB
p�B
p�B
q'B
q[B
qvB
q�B
rB
rB
raB
raB
r|B
r|B
r�B
r�B
r�B
sB
s3B
s�B
shB
s�B
s�B
s�B
tB
tB
t9B
tnB
tTB
t�B
t�B
t�B
u%B
u?B
utB
utB
u�B
u�B
u�B
vFB
vzB
v�B
v�B
v�B
v�B
v�B
wB
wLB
wfB
w�B
w�B
xB
xB
xRB
x�B
y	B
y	B
y	B
y>B
y>B
yrB
yrB
yrB
yrB
y�B
y�B
y�B
zB
zB
z*B
zDB
z^B
zxB
z�B
z�B
z�B
z�B
z�B
{dB
{�B
{�B
{�B
{�B
|B
|6B
|PB
|6B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}qB
}VB
}VB
}<B
}qB
~B
~]B
~�B
~�B
~�B
~�B
~�B
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105230  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191425  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191426  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191426                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041433  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041433  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                