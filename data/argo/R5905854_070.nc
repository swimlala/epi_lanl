CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:57:11Z creation;2022-06-04T17:57:12Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604175711  20220610141506  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               FA   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�Td�o{1   @�TefOD@/O�;dZ�c�`A�71   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�33@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BR  BXffB^  Bi��Bq33Bv��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C�fC  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0�C1�fC4  C6  C8  C:  C<�C>  C?�fCA�fCD  CF  CH  CI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"y�D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\fD\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�s31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�H@��
@���@�p�A Q�A@Q�A`Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B@{BH{BR{BXz�B^{Bi�BqG�Bv�HB�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�p�B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=C �C�CCCC
CCCCCCCCCCC C"C$C&C(C*C,C.�C0�C1�C4C6C8C:C<�C>C?�CA�CDCFCHCI�CLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCn�CpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"z�D#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\�D\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�C�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�=qD���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D��D���D� �D�@�D値D���D� �D�@�D怤D���D� �D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���Aɽ�Aɺ*Aɻ0AɾwA���A���A�ǮA��)A��6A��A�͟A��vA��BA���A�уA�҉A��TA�ҽA��[A�ԕA���A���A���A�՛AɽqAǊrAŨ�A��iA��A�AA��A���A��A���A�n/A�]�A��2A�#A���A�L0A��	A���A���A��A�Y�A���A�$�A�jA��fA�[WA���A�$�A��A�ܒA�?HA��,A�EmA���A�T�A�?}A���A�Z�A���A��A�u�A��A� 'A�U�A�!A��{A�"�A�ncA��NA�*�A�bA�8�A�e�A�	A�DA�n/A�o A�+�A�ǮA~�>A}��Az��Au_Aq��Ao˒An^5Al��Aku�Ah��A`�pAVc�AS*�AP,�ANXAK�AI;�AG��AG�~AF��AF<6AEiDADo�AC>�ABzxAB6zAB�AA�tA@��A>��A<��A;PHA:j�A5�A/�	A+��A)�A(��A']dA&�A%&�A$��A$�A#��A"�fA"�A"e�A"�A"�A$-wA%��A&$�A& iA#��A"W�A"k�A"�A"��A"7LA!g�A ��A �kA &�A �=A ��A :�Af�A]�AxATaA�BA��Au�A�BA)_AYAn/AkQA�8A�AqvAGA�A�>A��A�A��A��AS�A9XAA�9A��A��A��A�rA�A \AߤA�dAZ�A&Az�A@OA�A�A.�A�A/�AJA
�AA
�[A	�'A�BAH�A�EA~(AP�AbA�A+At�A�A�AzA.�A�A%A-wA ��A :�@�;@�0U@���@�@���@��4@��@�!@��@@��M@�J�@�b@���@��M@�N�@�@�Dg@��P@�>B@�A�@�Z@�X@���@�Y@�@��@��'@�2�@�J@�@O@��@�r@�@�@�Z@�!@��Z@�1@��5@�h@�n�@�L0@�&�@�A@�7@��@���@�E9@�bN@�c�@���@��Q@�]�@�4@�(�@��@�w�@��o@�A�@ڳh@�C�@�|�@ج@�8�@�/�@�^5@�q�@�l�@׸�@��@֕�@��+@�f�@�B�@��@���@�p�@�$t@Ҁ�@�:�@���@Ф�@�l�@�Xy@�'R@��A@�rG@�C@�@��@ͽ�@�l�@��[@�<�@�ϫ@ˊ	@�	l@�oi@��Z@ɇ�@�u�@�dZ@�J#@�~�@Ǻ^@�t�@��5@ƣ@�y>@��@�IR@��@�z�@�=q@�/�@��+@�s@�W?@�.I@��,@�3�@��@�Vm@���@���@�C�@���@��f@�@��[@��@��<@��9@���@�x@���@�`B@�Y@���@���@�}V@�@��@���@�s�@�E9@��@�<�@��w@�]�@�B�@�(@��]@���@�{�@��@���@���@���@���@�~�@�:*@�ϫ@�'�@��<@�/�@�w2@���@�g8@���@�_p@��@���@�`�@��@���@�N<@��@��A@�.�@�p�@��@���@�}V@��@��K@���@�iD@�8�@�	l@���@�.�@��@@�c�@�;d@��y@��@���@���@�e�@�Y@���@�ѷ@��@�Xy@�L0@��>@��7@�5�@��@���@��@�ԕ@�Q�@��m@�N�@�4@��]@�� @���@�X�@��@��@�\�@�6�@�b@���@��@��}@���@���@�E9@��@�֡@�~(@�W�@��@��f@�(�@���@���@�D�@��@���@�=@��@��]@���@��+@�j@�!�@���@�@@��I@�:*@���@�Z�@��H@���@�YK@��
@�iD@��@�Ɇ@��R@��L@�y>@�_�@�Q�@�)�@��@���@�zx@�5�@��?@���@�Z@��@��@��:@�K�@��@���@���@�Z�@�!�@�ԕ@��@�[W@���@��b@�Ov@��D@���@�8�@�)_@��@�@@���@���@��!@�~�@�*�@���@���@�e�@��@��,@���@�D�@�4@�x@�خ@��*@�u�@�-w@��@���@�B[@�%�@���@��k@��@��M@�C�@�o@��v@��e@��A@�l�@�Z@��@��6@�_p@���@�p;@�+k@��@��.@��
@�~�@�C�@�@���@���@��9@��@�Q@�;�@�%�@�@���@��K@���@�[W@�<6@���@���@�p;@�	@�
@@~�@~�@}��@}j@|��@|  @{��@{��@{�w@{C@z�@zh
@y��@yzx@y<6@y@@x~(@xb@w��@w�V@w�@v�@v�m@v~�@v0U@u�>@uo @tɆ@s��@ss@r�@r��@r!�@q��@qzx@q�@p�O@p2�@o�
@oo�@o33@n�1@n{�@ns�@ns�@n?@m�o@m��@l�e@l>B@k��@kn/@j�y@j�+@i�Z@i�@i�@h�z@hx@g�	@g=@g�@f��@f��@f1�@e�M@eO�@e2a@dѷ@d��@d`�@dV�@dFt@cƨ@cj�@b�<@b	@a�C@`�/@`�.@`'R@_qv@_�@^�'@^GE@]�S@]S&@]�@\]d@\b@[��@[~�@[.I@Zߤ@Z��@Z6�@Y��@Y��@Y&�@X�@Xѷ@X��@Xl"@X@W��@W_p@WO@W i@V��@V-@U��@U��@U+@T�@T�z@TU2@S��@Sƨ@St�@S�@Rȴ@R��@R;�@R
�@Q��@QN<@P�@P֡@P�D@P`�@PM@P*�@O�@O��@O.I@N��@N�H@N��@N��@Nff@N8�@N-@N�@M�@M��@L�o@L�@K�@K�@KMj@K+@K$t@J�!@J�@I��@I��@I�'@IVm@H�/@H�z@H��@H]d@G�@G�	@GZ�@GY@F��@F\�@F
�@E�T@Ej@E�@D�U@D�@C�W@C]�@C.I@C�@B�6@Bq�@B=q@B�@A�n@AY�@A+@@��@@�4@@"h@?�6@?n/@?F�@?@>�}@>!�@=��@=�9@=|@=0�@<�@<�@<�@<H@<1@;�K@;�{@;�@:�m@:��@:��@9��@9�h@9hs@98�@8��@8u�@8-�@8*�@8(�@7��@7s@7�@7�@6�M@6�2@6�F@6 �@5�M@5c�@52a@4��@4�[@4��@4V�@4!@3��@3�4@36z@2�@2�b@2~�@2@�@1�o@1�t@1x�@1�@0�o@0 �@/�6@/A�@.�'@.n�@-�T@-w2@-8�@-5�@--w@,�9@,A�@,7@+��@+˒@+�@+�k@+��@+s@+dZ@+33@*�H@*��@*1�@)�N@)�"@)�@(�5@(��@(l"@(�@'��@'��@'>�@&��@&O@%��@%��@%��@%-w@$��@$�I@$�@$bN@$,=@#��@#��@#l�@#33@"��@"ff@"�@!�o@!�z@!�M@!J�@!�@ �`@ oi@ A�@ �@�@n/@'�@��@��@GE@;�@GE@;�@�@��@�@��@(�@��@K^@!@  @��@Z�@O@>�@
=@�@�B@��@	@�@�M@f�@Q�@��@l"@�@��@a@A�@�"@�@�A@Q@@�@��@�X@L�@@��@��@h�@g8@`�@H@,=@G@��@��@l�@)_@�!@H�@	@�Z@�@��@��@��@hs@5�@��@�j@�D@/�@��@��@��@{J@J#@�@͟@��@^5@+k@�Z@��@�T@@�@��@zx@?}@�@�U@�$@�@4n@7@��@��@��@v`@W?@@O@8@.I@)_@'�@�@
��@
�@
d�@
L0@
:*@
($@
 �@	�o@	�@	�@	��@	j@	Dg@֡1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���Aɽ�Aɺ*Aɻ0AɾwA���A���A�ǮA��)A��6A��A�͟A��vA��BA���A�уA�҉A��TA�ҽA��[A�ԕA���A���A���A�՛AɽqAǊrAŨ�A��iA��A�AA��A���A��A���A�n/A�]�A��2A�#A���A�L0A��	A���A���A��A�Y�A���A�$�A�jA��fA�[WA���A�$�A��A�ܒA�?HA��,A�EmA���A�T�A�?}A���A�Z�A���A��A�u�A��A� 'A�U�A�!A��{A�"�A�ncA��NA�*�A�bA�8�A�e�A�	A�DA�n/A�o A�+�A�ǮA~�>A}��Az��Au_Aq��Ao˒An^5Al��Aku�Ah��A`�pAVc�AS*�AP,�ANXAK�AI;�AG��AG�~AF��AF<6AEiDADo�AC>�ABzxAB6zAB�AA�tA@��A>��A<��A;PHA:j�A5�A/�	A+��A)�A(��A']dA&�A%&�A$��A$�A#��A"�fA"�A"e�A"�A"�A$-wA%��A&$�A& iA#��A"W�A"k�A"�A"��A"7LA!g�A ��A �kA &�A �=A ��A :�Af�A]�AxATaA�BA��Au�A�BA)_AYAn/AkQA�8A�AqvAGA�A�>A��A�A��A��AS�A9XAA�9A��A��A��A�rA�A \AߤA�dAZ�A&Az�A@OA�A�A.�A�A/�AJA
�AA
�[A	�'A�BAH�A�EA~(AP�AbA�A+At�A�A�AzA.�A�A%A-wA ��A :�@�;@�0U@���@�@���@��4@��@�!@��@@��M@�J�@�b@���@��M@�N�@�@�Dg@��P@�>B@�A�@�Z@�X@���@�Y@�@��@��'@�2�@�J@�@O@��@�r@�@�@�Z@�!@��Z@�1@��5@�h@�n�@�L0@�&�@�A@�7@��@���@�E9@�bN@�c�@���@��Q@�]�@�4@�(�@��@�w�@��o@�A�@ڳh@�C�@�|�@ج@�8�@�/�@�^5@�q�@�l�@׸�@��@֕�@��+@�f�@�B�@��@���@�p�@�$t@Ҁ�@�:�@���@Ф�@�l�@�Xy@�'R@��A@�rG@�C@�@��@ͽ�@�l�@��[@�<�@�ϫ@ˊ	@�	l@�oi@��Z@ɇ�@�u�@�dZ@�J#@�~�@Ǻ^@�t�@��5@ƣ@�y>@��@�IR@��@�z�@�=q@�/�@��+@�s@�W?@�.I@��,@�3�@��@�Vm@���@���@�C�@���@��f@�@��[@��@��<@��9@���@�x@���@�`B@�Y@���@���@�}V@�@��@���@�s�@�E9@��@�<�@��w@�]�@�B�@�(@��]@���@�{�@��@���@���@���@���@�~�@�:*@�ϫ@�'�@��<@�/�@�w2@���@�g8@���@�_p@��@���@�`�@��@���@�N<@��@��A@�.�@�p�@��@���@�}V@��@��K@���@�iD@�8�@�	l@���@�.�@��@@�c�@�;d@��y@��@���@���@�e�@�Y@���@�ѷ@��@�Xy@�L0@��>@��7@�5�@��@���@��@�ԕ@�Q�@��m@�N�@�4@��]@�� @���@�X�@��@��@�\�@�6�@�b@���@��@��}@���@���@�E9@��@�֡@�~(@�W�@��@��f@�(�@���@���@�D�@��@���@�=@��@��]@���@��+@�j@�!�@���@�@@��I@�:*@���@�Z�@��H@���@�YK@��
@�iD@��@�Ɇ@��R@��L@�y>@�_�@�Q�@�)�@��@���@�zx@�5�@��?@���@�Z@��@��@��:@�K�@��@���@���@�Z�@�!�@�ԕ@��@�[W@���@��b@�Ov@��D@���@�8�@�)_@��@�@@���@���@��!@�~�@�*�@���@���@�e�@��@��,@���@�D�@�4@�x@�خ@��*@�u�@�-w@��@���@�B[@�%�@���@��k@��@��M@�C�@�o@��v@��e@��A@�l�@�Z@��@��6@�_p@���@�p;@�+k@��@��.@��
@�~�@�C�@�@���@���@��9@��@�Q@�;�@�%�@�@���@��K@���@�[W@�<6@���@���@�p;@�	@�
@@~�@~�@}��@}j@|��@|  @{��@{��@{�w@{C@z�@zh
@y��@yzx@y<6@y@@x~(@xb@w��@w�V@w�@v�@v�m@v~�@v0U@u�>@uo @tɆ@s��@ss@r�@r��@r!�@q��@qzx@q�@p�O@p2�@o�
@oo�@o33@n�1@n{�@ns�@ns�@n?@m�o@m��@l�e@l>B@k��@kn/@j�y@j�+@i�Z@i�@i�@h�z@hx@g�	@g=@g�@f��@f��@f1�@e�M@eO�@e2a@dѷ@d��@d`�@dV�@dFt@cƨ@cj�@b�<@b	@a�C@`�/@`�.@`'R@_qv@_�@^�'@^GE@]�S@]S&@]�@\]d@\b@[��@[~�@[.I@Zߤ@Z��@Z6�@Y��@Y��@Y&�@X�@Xѷ@X��@Xl"@X@W��@W_p@WO@W i@V��@V-@U��@U��@U+@T�@T�z@TU2@S��@Sƨ@St�@S�@Rȴ@R��@R;�@R
�@Q��@QN<@P�@P֡@P�D@P`�@PM@P*�@O�@O��@O.I@N��@N�H@N��@N��@Nff@N8�@N-@N�@M�@M��@L�o@L�@K�@K�@KMj@K+@K$t@J�!@J�@I��@I��@I�'@IVm@H�/@H�z@H��@H]d@G�@G�	@GZ�@GY@F��@F\�@F
�@E�T@Ej@E�@D�U@D�@C�W@C]�@C.I@C�@B�6@Bq�@B=q@B�@A�n@AY�@A+@@��@@�4@@"h@?�6@?n/@?F�@?@>�}@>!�@=��@=�9@=|@=0�@<�@<�@<�@<H@<1@;�K@;�{@;�@:�m@:��@:��@9��@9�h@9hs@98�@8��@8u�@8-�@8*�@8(�@7��@7s@7�@7�@6�M@6�2@6�F@6 �@5�M@5c�@52a@4��@4�[@4��@4V�@4!@3��@3�4@36z@2�@2�b@2~�@2@�@1�o@1�t@1x�@1�@0�o@0 �@/�6@/A�@.�'@.n�@-�T@-w2@-8�@-5�@--w@,�9@,A�@,7@+��@+˒@+�@+�k@+��@+s@+dZ@+33@*�H@*��@*1�@)�N@)�"@)�@(�5@(��@(l"@(�@'��@'��@'>�@&��@&O@%��@%��@%��@%-w@$��@$�I@$�@$bN@$,=@#��@#��@#l�@#33@"��@"ff@"�@!�o@!�z@!�M@!J�@!�@ �`@ oi@ A�@ �@�@n/@'�@��@��@GE@;�@GE@;�@�@��@�@��@(�@��@K^@!@  @��@Z�@O@>�@
=@�@�B@��@	@�@�M@f�@Q�@��@l"@�@��@a@A�@�"@�@�A@Q@@�@��@�X@L�@@��@��@h�@g8@`�@H@,=@G@��@��@l�@)_@�!@H�@	@�Z@�@��@��@��@hs@5�@��@�j@�D@/�@��@��@��@{J@J#@�@͟@��@^5@+k@�Z@��@�T@@�@��@zx@?}@�@�U@�$@�@4n@7@��@��@��@v`@W?@@O@8@.I@)_@'�@�@
��@
�@
d�@
L0@
:*@
($@
 �@	�o@	�@	�@	��@	j@	Dg@֡1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�zB�FB��B�LB�B�B�B�B�B��B�B�B��B�B�B�8B�B��B��B�B��B�kB�B[B�|Bz^B
��B
�1B6B?�B;�B?.BC{BF%BG+BIBNBP�BX�B\]Bw�B��B~BB{�B��B�XB�uB�YB�8B�B�B�xB�*B[B�B�B��B�B3�BG�B88B�BB�+B��B�'B�[B��B��B��B��B�`B�WB�	ByXBY�B"�B
�B
�IB
y�B
F�B

XB	�[B	��B	��B	�B	�hB	��B	z�B	p�B	g�B	Z7B	D�B	0�B	/5B	5�B	9$B	C-B	K�B	NB	M�B	M6B	M�B	Q�B	[#B	��B	��B	��B	��B	��B	�B	�sB	��B	��B	wB	P�B	�B��B�IB�hB�B�B��B	�B	SB	B	�B	�B	:^B	^B	l�B	��B	�<B	�aB	��B	��B	ŢB	� B	��B	�B	��B	��B	��B	�B	��B	��B	�BB	��B	�(B
 OB
FB
(B
B

�B
�B
�B
	B
vB
B
IB
kB
�B
<B
�B
vB
}B
�B
mB
NB
	�B
mB	��B	��B	�B
oB
B
	7B

#B
�B	�>B	�B
 OB
�B
�B
�B
�B

XB

�B
	7B
fB
YB
�B
B
�B
�B
�B	�>B	�+B	�B	�B	�B	�sB	�B	�B	�B	�B	��B	�-B	�zB	�B	�B	��B	�`B	�NB	�|B	ބB	�B	�/B	�xB	�CB	�qB	ڠB	�B	��B	ևB	��B	�:B	уB	� B	�HB	�\B	��B	̈́B	��B	��B	�7B	�PB	�JB	��B	��B	�oB	��B	�oB	�[B	�[B	҉B	�B	�)B	��B	�gB	ŢB	�?B	�mB	�%B	ɠB	͟B	�#B	�+B	�zB	ƨB	�MB	��B	��B	�3B	�MB	�B	�?B	�%B	��B	ǮB	ƨB	�%B	ƎB	�tB	�?B	�SB	�tB	ȀB	�#B	��B	�DB	̘B	�~B	ΥB	�<B	�B	�bB	��B	�B	��B	�B	�7B	��B	�=B	�KB	��B	ڠB	یB	��B	�dB	�B	�dB	�!B	�B	ݘB	�dB	ܒB	��B	��B	��B	�\B	��B	�B	��B	�B	�'B	�B	�B	�B	�B	�B	�&B	�B	�B	��B	�`B	�,B	�B	�B	��B	�DB	��B	�B	�eB	��B	��B	�0B	�B	�B	�QB	�B	�"B	�=B	�=B	�WB	�qB	�qB	�B	�)B	��B	�/B	�/B	�B	�OB	�B	� B	�5B	�OB	� B	��B	�/B	�]B	�CB	��B	��B	�B	�B	��B	�B	��B	�aB	�B	�nB	��B	�2B	�B	��B	��B	�	B	�$B	��B	�rB	��B	�DB	��B	��B	�DB	�*B	��B	��B	��B	�DB	�DB	��B	��B	��B	�0B	��B	��B	�B	��B	�VB	��B	��B	�(B	�wB	��B	��B	��B	�]B	��B	�]B	��B	��B	�cB	��B	��B	�}B
  B	��B	�HB	�}B
 4B
UB
'B
�B
�B
GB
B
-B
-B
GB
�B
�B
B
�B
�B
�B
B
�B
�B
B
?B
�B
�B
YB
YB
tB
�B
B
zB
�B
fB
�B
	B
	�B
	RB
	�B

�B
B

�B

�B

rB

�B
�B
�B
6B
�B
�B
�B
�B
vB
�B
hB
�B
�B
:B
 B
:B
TB
oB
�B
�B
�B
�B
�B
@B
�B
�B
�B
aB
{B
�B
B
gB
�B
B
9B
B
mB
�B

B
�B
�B
�B
EB
B
�B
	B
	B
#B
�B
�B
�B
qB
�B
)B
]B
IB
B
OB
5B
�B
B
B
VB
�B
�B
 BB
 BB
!|B
!bB
!HB
"hB
"hB
"NB
"�B
"�B
"�B
"�B
#:B
#�B
#�B
#�B
#�B
#TB
#TB
#�B
#�B
$B
$B
$@B
$ZB
$�B
$�B
$�B
$�B
%FB
%zB
%�B
&�B
&�B
&�B
&�B
'B
'RB
'�B
(sB
(�B
)B
)*B
)�B
)�B
*�B
*�B
+B
+�B
,"B
,"B
,�B
-)B
-CB
-CB
-CB
.B
.IB
.IB
.�B
/5B
/OB
/iB
0!B
0;B
0;B
0;B
0�B
0�B
0�B
1'B
1AB
1[B
1vB
2B
2-B
2aB
2�B
3MB
3�B
3�B
3�B
4B
4TB
4�B
5B
5ZB
5�B
6+B
6B
6B
5�B
6`B
6�B
7B
8B
8lB
8lB
8�B
9	B
9XB
:*B
:xB
:�B
:�B
;0B
;�B
;�B
<6B
<6B
<�B
=�B
>wB
>]B
>wB
?.B
?HB
?}B
?cB
?cB
?�B
?�B
@�B
@�B
AB
A�B
A�B
A�B
B[B
B'B
B'B
B[B
B�B
B�B
B�B
B�B
CaB
C�B
D3B
DB
D3B
DMB
DgB
D�B
D�B
EB
E9B
E9B
EB
EmB
E�B
E�B
E�B
E�B
F?B
FYB
F�B
GB
F�B
G+B
G+B
G+B
G_B
G�B
G�B
HfB
H�B
IB
I7B
IRB
IlB
I�B
I�B
J#B
J#B
JrB
J�B
J�B
J�B
J�B
J�B
KDB
K^B
KDB
KDB
KDB
K�B
K�B
K�B
KxB
K�B
K�B
L�B
MB
MB
MPB
M�B
M�B
M�B
N"B
NpB
N�B
N�B
N�B
N�B
OB
OB
O(B
OBB
O�B
O�B
O�B
O�B
PHB
P�B
P�B
P�B
Q4B
Q�B
Q�B
RoB
RoB
R�B
R�B
R�B
S[B
S�B
S�B
S�B
S�B
T,B
TFB
TaB
T�B
T�B
UB
U�B
U�B
U�B
U�B
VmB
V9B
V9B
V�B
V�B
V�B
V�B
W$B
W?B
WsB
WsB
W�B
XB
X+B
X+B
X+B
X�B
X�B
X�B
YB
Y1B
Y�B
Y�B
Y�B
Y�B
Y�B
ZkB
Z�B
Z�B
Z�B
Z�B
[=B
[�B
\]B
\xB
\�B
\�B
]IB
]~B
]�B
]�B
^B
^�B
^�B
_VB
_�B
_�B
_�B
`B
`B
`'B
`'B
`vB
`�B
`�B
`�B
`�B
`�B
aB
aHB
aHB
a-B
a-B
a|B
a�B
a�B
bB
b4B
b4B
bNB
bhB
bhB
bhB
b�B
b�B
c:B
c�B
c�B
c�B
d&B
d&B
d@B
dtB
d�B
d�B
d�B
e`B
e�B
fLB
f�B
f�B
f�B
g�B
h>B
h>B
h�B
h�B
h�B
h�B
h�B
i*B
i_B
i�B
i�B
i�B
i�B
i�B
jKB
jeB
j�B
j�B
kB
kQB
kkB
k�B
k�B
lqB
l�B
l�B
mCB
mwB
m�B
m�B
m�B
m�B
m�B
n/B
n�B
oiB
oOB
oiB
oOB
o�B
o�B
o�B
o�B
p;B
p;B
pUB
poB
q'B
qvB
q�B
q�B
q�B
rB
r|B
r�B
s3B
s�B
s�B
s�B
tB
t9B
tnB
t�B
t�B
t�B
t�B
t�B
uB
u%B
u�B
u�B
u�B
u�B
vB
v+B
v+B
vFB
vB
vB
v+B
v�B
v�B
wLB
wLB
wfB
w�B
w�B
w�B
w�B
w�B
xRB
xlB
x�B
x�B
y$B
y$B
y�B
y�B
y�B
zB
z�B
z�B
z�B
{0B
{�B
{�B
{�B
{�B
{�B
{�B
|B
|6B
|jB
|�B
|�B
|�B
}"B
}"B
}qB
}�B
}�B
~B
~BB
~BB
~]B
~BB
~]B
~BB
~]B
~�B
~�B
.B
.B
HB
HB
}B
}B
}B
�B
�B
�B
�B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�zB�FB��B�LB�B�B�B�B�B��B�B�B��B�B�B�8B�B��B��B�B��B�kB�B[B�|Bz^B
��B
�1B6B?�B;�B?.BC{BF%BG+BIBNBP�BX�B\]Bw�B��B~BB{�B��B�XB�uB�YB�8B�B�B�xB�*B[B�B�B��B�B3�BG�B88B�BB�+B��B�'B�[B��B��B��B��B�`B�WB�	ByXBY�B"�B
�B
�IB
y�B
F�B

XB	�[B	��B	��B	�B	�hB	��B	z�B	p�B	g�B	Z7B	D�B	0�B	/5B	5�B	9$B	C-B	K�B	NB	M�B	M6B	M�B	Q�B	[#B	��B	��B	��B	��B	��B	�B	�sB	��B	��B	wB	P�B	�B��B�IB�hB�B�B��B	�B	SB	B	�B	�B	:^B	^B	l�B	��B	�<B	�aB	��B	��B	ŢB	� B	��B	�B	��B	��B	��B	�B	��B	��B	�BB	��B	�(B
 OB
FB
(B
B

�B
�B
�B
	B
vB
B
IB
kB
�B
<B
�B
vB
}B
�B
mB
NB
	�B
mB	��B	��B	�B
oB
B
	7B

#B
�B	�>B	�B
 OB
�B
�B
�B
�B

XB

�B
	7B
fB
YB
�B
B
�B
�B
�B	�>B	�+B	�B	�B	�B	�sB	�B	�B	�B	�B	��B	�-B	�zB	�B	�B	��B	�`B	�NB	�|B	ބB	�B	�/B	�xB	�CB	�qB	ڠB	�B	��B	ևB	��B	�:B	уB	� B	�HB	�\B	��B	̈́B	��B	��B	�7B	�PB	�JB	��B	��B	�oB	��B	�oB	�[B	�[B	҉B	�B	�)B	��B	�gB	ŢB	�?B	�mB	�%B	ɠB	͟B	�#B	�+B	�zB	ƨB	�MB	��B	��B	�3B	�MB	�B	�?B	�%B	��B	ǮB	ƨB	�%B	ƎB	�tB	�?B	�SB	�tB	ȀB	�#B	��B	�DB	̘B	�~B	ΥB	�<B	�B	�bB	��B	�B	��B	�B	�7B	��B	�=B	�KB	��B	ڠB	یB	��B	�dB	�B	�dB	�!B	�B	ݘB	�dB	ܒB	��B	��B	��B	�\B	��B	�B	��B	�B	�'B	�B	�B	�B	�B	�B	�&B	�B	�B	��B	�`B	�,B	�B	�B	��B	�DB	��B	�B	�eB	��B	��B	�0B	�B	�B	�QB	�B	�"B	�=B	�=B	�WB	�qB	�qB	�B	�)B	��B	�/B	�/B	�B	�OB	�B	� B	�5B	�OB	� B	��B	�/B	�]B	�CB	��B	��B	�B	�B	��B	�B	��B	�aB	�B	�nB	��B	�2B	�B	��B	��B	�	B	�$B	��B	�rB	��B	�DB	��B	��B	�DB	�*B	��B	��B	��B	�DB	�DB	��B	��B	��B	�0B	��B	��B	�B	��B	�VB	��B	��B	�(B	�wB	��B	��B	��B	�]B	��B	�]B	��B	��B	�cB	��B	��B	�}B
  B	��B	�HB	�}B
 4B
UB
'B
�B
�B
GB
B
-B
-B
GB
�B
�B
B
�B
�B
�B
B
�B
�B
B
?B
�B
�B
YB
YB
tB
�B
B
zB
�B
fB
�B
	B
	�B
	RB
	�B

�B
B

�B

�B

rB

�B
�B
�B
6B
�B
�B
�B
�B
vB
�B
hB
�B
�B
:B
 B
:B
TB
oB
�B
�B
�B
�B
�B
@B
�B
�B
�B
aB
{B
�B
B
gB
�B
B
9B
B
mB
�B

B
�B
�B
�B
EB
B
�B
	B
	B
#B
�B
�B
�B
qB
�B
)B
]B
IB
B
OB
5B
�B
B
B
VB
�B
�B
 BB
 BB
!|B
!bB
!HB
"hB
"hB
"NB
"�B
"�B
"�B
"�B
#:B
#�B
#�B
#�B
#�B
#TB
#TB
#�B
#�B
$B
$B
$@B
$ZB
$�B
$�B
$�B
$�B
%FB
%zB
%�B
&�B
&�B
&�B
&�B
'B
'RB
'�B
(sB
(�B
)B
)*B
)�B
)�B
*�B
*�B
+B
+�B
,"B
,"B
,�B
-)B
-CB
-CB
-CB
.B
.IB
.IB
.�B
/5B
/OB
/iB
0!B
0;B
0;B
0;B
0�B
0�B
0�B
1'B
1AB
1[B
1vB
2B
2-B
2aB
2�B
3MB
3�B
3�B
3�B
4B
4TB
4�B
5B
5ZB
5�B
6+B
6B
6B
5�B
6`B
6�B
7B
8B
8lB
8lB
8�B
9	B
9XB
:*B
:xB
:�B
:�B
;0B
;�B
;�B
<6B
<6B
<�B
=�B
>wB
>]B
>wB
?.B
?HB
?}B
?cB
?cB
?�B
?�B
@�B
@�B
AB
A�B
A�B
A�B
B[B
B'B
B'B
B[B
B�B
B�B
B�B
B�B
CaB
C�B
D3B
DB
D3B
DMB
DgB
D�B
D�B
EB
E9B
E9B
EB
EmB
E�B
E�B
E�B
E�B
F?B
FYB
F�B
GB
F�B
G+B
G+B
G+B
G_B
G�B
G�B
HfB
H�B
IB
I7B
IRB
IlB
I�B
I�B
J#B
J#B
JrB
J�B
J�B
J�B
J�B
J�B
KDB
K^B
KDB
KDB
KDB
K�B
K�B
K�B
KxB
K�B
K�B
L�B
MB
MB
MPB
M�B
M�B
M�B
N"B
NpB
N�B
N�B
N�B
N�B
OB
OB
O(B
OBB
O�B
O�B
O�B
O�B
PHB
P�B
P�B
P�B
Q4B
Q�B
Q�B
RoB
RoB
R�B
R�B
R�B
S[B
S�B
S�B
S�B
S�B
T,B
TFB
TaB
T�B
T�B
UB
U�B
U�B
U�B
U�B
VmB
V9B
V9B
V�B
V�B
V�B
V�B
W$B
W?B
WsB
WsB
W�B
XB
X+B
X+B
X+B
X�B
X�B
X�B
YB
Y1B
Y�B
Y�B
Y�B
Y�B
Y�B
ZkB
Z�B
Z�B
Z�B
Z�B
[=B
[�B
\]B
\xB
\�B
\�B
]IB
]~B
]�B
]�B
^B
^�B
^�B
_VB
_�B
_�B
_�B
`B
`B
`'B
`'B
`vB
`�B
`�B
`�B
`�B
`�B
aB
aHB
aHB
a-B
a-B
a|B
a�B
a�B
bB
b4B
b4B
bNB
bhB
bhB
bhB
b�B
b�B
c:B
c�B
c�B
c�B
d&B
d&B
d@B
dtB
d�B
d�B
d�B
e`B
e�B
fLB
f�B
f�B
f�B
g�B
h>B
h>B
h�B
h�B
h�B
h�B
h�B
i*B
i_B
i�B
i�B
i�B
i�B
i�B
jKB
jeB
j�B
j�B
kB
kQB
kkB
k�B
k�B
lqB
l�B
l�B
mCB
mwB
m�B
m�B
m�B
m�B
m�B
n/B
n�B
oiB
oOB
oiB
oOB
o�B
o�B
o�B
o�B
p;B
p;B
pUB
poB
q'B
qvB
q�B
q�B
q�B
rB
r|B
r�B
s3B
s�B
s�B
s�B
tB
t9B
tnB
t�B
t�B
t�B
t�B
t�B
uB
u%B
u�B
u�B
u�B
u�B
vB
v+B
v+B
vFB
vB
vB
v+B
v�B
v�B
wLB
wLB
wfB
w�B
w�B
w�B
w�B
w�B
xRB
xlB
x�B
x�B
y$B
y$B
y�B
y�B
y�B
zB
z�B
z�B
z�B
{0B
{�B
{�B
{�B
{�B
{�B
{�B
|B
|6B
|jB
|�B
|�B
|�B
}"B
}"B
}qB
}�B
}�B
~B
~BB
~BB
~]B
~BB
~]B
~BB
~]B
~�B
~�B
.B
.B
HB
HB
}B
}B
}B
�B
�B
�B
�B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105003  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175711  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175712  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175712                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025720  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025720  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141506                      G�O�G�O�G�O�                