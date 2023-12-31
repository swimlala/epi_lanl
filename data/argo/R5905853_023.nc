CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:26:41Z creation;2022-06-04T17:26:42Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �8   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �H   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �P   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220604172641  20220610131506  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @��Ϥ�P1   @��(��m@/��/���cL���S�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   AA��A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�ffB�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���C�fC  C  C  C
  C  C  C  C  C  C� C�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB33CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�C3D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�H@���@���A Q�A Q�AA�A`Q�A~�RA�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B@{BH{BP{BX{B`{Bh{Bp{Bx{B�
=B�
=B�
=B�
=B�
=B�
=B�
=B���B��
B�
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
=B��
B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�=pB�p�B��
C�CCCC
CCCCCC�C�CCCC C"C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@CB8RCC�CFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D��D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�C�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�@�D怤D���D� �D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D��q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��A��A��A��A��2A��A��A��sA��]A��cA��A��A��ZA��)A���A���A���A޻0A޺�A޺�A޷LA޴9A޲-A޲�Aޯ�Aީ�Aޠ�Aޘ+AތA�tTA�P�A�=�A��A��fA�o A�<A�S&A���A��A��"A�xlÁoAˤtA��}AǠ'A��Aº*A���A�h
A�>�A��'A�9�A� iA��A�pA�Q�A��zA��]A�'�A�бA���A��A��RA��A�:*A�=qA��A�
�A�W�A�ܒA� iA�.A�?�A�O�A��A���A���A�%A�A��HA���A�)�A���A�6�A��1A�"A{{�Ay$�AwFtAs'�An��Aj�Ah�,Ag?�Ad�DA]�AYVmAR��AN��AKs�AH��AG{�AG?}AF�AF�ABn/A@A>�AA>�)A>$tA<�mA9�A8^5A6zxA4Z�A3��A2�YA0A.a|A,YA*dZA))�A'�A&|A%�+A%��A$�XA$:*A#�A"��A"a�A �aA~�AݘA�A!A"z�A#c�A$33A#l�A"��A"5�A"O�A"�FA"$�A!�]A �AV�A_pA��A[WA�HA|A"hA�}A-�A�A*0A�~A	A��AJ�A�A�{A7LA��A^�A%A�}Ae�A(�AC-A�A��A�"A]dA{�A��AƨA��AZA��A�	A�rAk�A
��A
�A
��A
i�A
VA	�MA	��A	qA	&A�A��A��A�AYKA�A�A.IAیAt�AA�'A�A`�AAԕA��An/A&�Ad�A�A\�A �A [�@���@��@�qv@���@�c�@���@�ی@��@�^5@��@��@�@�W?@���@��@�?@�@�+�@�@��@�a�@�N�@��)@���@�4@�IR@썹@��@�k�@� �@���@���@�O@��@�Ɇ@�Ft@�x@���@��@䛦@�R�@�v`@�@�{@ᙚ@�Y�@��@��?@��1@��W@�ѷ@� �@��.@��@��
@ݨX@�X�@��@��@ܕ@�+k@�n/@�r�@��@ي	@��@�Z�@׊�@��@��@��)@���@�}V@���@���@�|�@���@�\�@�?@�"h@���@Ӯ@�*0@�-w@�ѷ@��@��H@ЂA@�b@�a@�Ta@���@���@��3@́�@��@̎�@̃@�v�@�J@˼@�b�@�	l@ʹ$@ʕ�@�l"@�1�@�	@ɫ�@�4�@��@�U2@Ǽ@�P�@�'�@��@��H@��@Ƨ@�M@�)�@�خ@Ŋ	@�@Đ.@��@Ð�@�~�@�K�@��[@�h
@��o@��P@�&�@��c@��@�!-@��R@�Z@�_@�u�@�&@��K@���@�:�@��w@��@�+�@��@���@�L0@�
�@��@���@�&@���@���@���@���@��@��.@�;�@�
�@�_p@�Q�@�A @�"�@�ѷ@�`�@�N<@��@�l�@�b@��@�O@���@���@�9X@���@�c@�)_@�r�@��@��h@�=�@��@��!@�6�@��@�&@���@���@���@�`�@���@�4@��@�;@�ߤ@��h@��o@��9@��M@��"@���@�
�@�˒@���@�9�@��@��u@�J�@���@�|@���@��6@�~@��@���@��a@�rG@�A�@�%F@��K@��u@���@�P�@�"�@��y@��o@�@�@� �@�خ@��*@��'@�X�@��@�3�@�@���@�4�@��/@���@�Q�@���@���@���@��@��@���@��*@���@�K�@�F�@�A @�:�@��@���@���@��@�� @���@��"@��!@���@��+@�h�@�4n@�˒@�|�@�?}@��@�V@���@�z@���@�ϫ@���@�}�@�?}@��@��c@�Ĝ@���@�*�@�J@�u@��z@��h@�8@��@��K@��z@�Ta@���@�c@�4�@�Y@�S@���@���@�M@��@���@�hs@�>�@�@�֡@�|�@�%�@��+@���@��~@�e,@�B�@�+@��@���@��s@��@��+@�X�@���@�Ta@�H�@�J�@��@��@��{@�<6@���@��,@���@�w�@�M@��@��D@���@�N<@���@���@���@�q�@�Ta@�(�@��T@��'@�P�@�!�@�Dg@�)_@��,@�oi@�PH@��g@��*@�zx@�\�@���@�^5@�Q@�7�@�b@��@S@}��@}p�@}:�@|��@|~(@{�W@{Mj@z�}@zOv@z:*@y�@y/@yV@x�`@x�I@wX�@w�@v�,@vH�@u�-@u*0@t��@t��@t�@tu�@t_@tPH@t1'@t"h@t�@s�q@s\)@r��@rL0@qG�@p�@o�}@oj�@nTa@m}�@mT�@m4@m�@lS�@k˒@k]�@k$t@j�"@j�R@j6�@i�D@i��@iF@h�@h��@htT@g�@g
=@f�m@f{�@e^�@d�@d*�@c�}@c��@c!-@b��@bYK@be@a��@a��@ae,@a�@`��@`��@`_@`�@_�w@_J#@^��@^6�@]��@]x�@]�@\�@\c�@\�@[{J@[�@Z�b@Zu@Y�@Y�=@Y}�@YT�@X��@XXy@XI�@X*�@W�@W� @W�F@Wx@V�,@V-@U}�@UV@T��@T�e@Tu�@TS�@TFt@T�@S�F@Sqv@SP�@S9�@S&@R�"@R� @Q�H@Qo @Q0�@PĜ@P��@P~(@P*�@O�K@O�@O>�@O!-@OY@N�M@N�}@N��@N}V@N.�@Mԕ@Mx�@Mf�@MVm@M�@L�/@L�@L9X@L�@Kخ@K�k@KZ�@K)_@J�@J��@J�x@Jv�@J.�@J$�@I��@Ij@I5�@I�@H��@H��@H@G�A@G�F@G i@E�>@EA @E�@D�P@D��@D"h@C�W@C��@C�Q@Cخ@C�}@C�[@C�$@C8@C�@B��@B@A�S@Aq@@�j@@��@@�O@@j@@b@?� @?�q@?�@>��@>�@=�N@=m]@=Vm@=G�@=�@<�?@<r�@;��@;P�@:v�@:@�@:�@9�'@9+�@8Ĝ@8I�@8	�@7��@7��@7b�@7�@7�@6��@6�\@6l�@5��@5��@5��@5�@5��@5m]@5*0@4�@4�_@41@3��@3�k@3�$@3y�@3K�@2�@2xl@2a|@2�@1ϫ@1��@1}�@1f�@0�@0�$@0��@0��@0,=@/ݘ@/y�@/P�@/�@/�@.��@.n�@.B[@-��@-��@-o @-&�@-&�@-q@,�	@,��@,y>@,2�@,1@+��@+�w@+�@+��@+j�@+33@+!-@+@+�@*��@*�y@*ں@*�'@*�h@*�!@*��@*��@*z@*kQ@*_�@*&�@)�@)�@)��@)}�@)L�@(��@(y>@(6@(�@'��@'��@'�}@'��@'�V@'~�@'
=@&�@&��@&_�@&?@%��@%^�@%J�@%/@%�@%	l@$�/@$�@$H@$�@#�6@#��@#y�@#�@#�@#�@# i@"�@"��@"J�@"!�@!��@!�=@![W@!�@ �|@ �p@ �o@�
@�*@��@�f@b�@H�@/�@�@��@��@GE@�@�d@��@f�@A @&�@V@�P@��@��@e�@?�@  @��@��@qv@RT@�@�M@�@�b@i�@.�@��@�'@\�@(�@��@�j@��@j@Q�@K^@Ft@>B@"h@�a@��@F�@�@��@��@GE@{@ �@��@�n@�@��@o @8�@�|@�U@��@�I@tT@S�@�@�@�0@�@{J@H�@9�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��A��A��A��A��2A��A��A��sA��]A��cA��A��A��ZA��)A���A���A���A޻0A޺�A޺�A޷LA޴9A޲-A޲�Aޯ�Aީ�Aޠ�Aޘ+AތA�tTA�P�A�=�A��A��fA�o A�<A�S&A���A��A��"A�xlÁoAˤtA��}AǠ'A��Aº*A���A�h
A�>�A��'A�9�A� iA��A�pA�Q�A��zA��]A�'�A�бA���A��A��RA��A�:*A�=qA��A�
�A�W�A�ܒA� iA�.A�?�A�O�A��A���A���A�%A�A��HA���A�)�A���A�6�A��1A�"A{{�Ay$�AwFtAs'�An��Aj�Ah�,Ag?�Ad�DA]�AYVmAR��AN��AKs�AH��AG{�AG?}AF�AF�ABn/A@A>�AA>�)A>$tA<�mA9�A8^5A6zxA4Z�A3��A2�YA0A.a|A,YA*dZA))�A'�A&|A%�+A%��A$�XA$:*A#�A"��A"a�A �aA~�AݘA�A!A"z�A#c�A$33A#l�A"��A"5�A"O�A"�FA"$�A!�]A �AV�A_pA��A[WA�HA|A"hA�}A-�A�A*0A�~A	A��AJ�A�A�{A7LA��A^�A%A�}Ae�A(�AC-A�A��A�"A]dA{�A��AƨA��AZA��A�	A�rAk�A
��A
�A
��A
i�A
VA	�MA	��A	qA	&A�A��A��A�AYKA�A�A.IAیAt�AA�'A�A`�AAԕA��An/A&�Ad�A�A\�A �A [�@���@��@�qv@���@�c�@���@�ی@��@�^5@��@��@�@�W?@���@��@�?@�@�+�@�@��@�a�@�N�@��)@���@�4@�IR@썹@��@�k�@� �@���@���@�O@��@�Ɇ@�Ft@�x@���@��@䛦@�R�@�v`@�@�{@ᙚ@�Y�@��@��?@��1@��W@�ѷ@� �@��.@��@��
@ݨX@�X�@��@��@ܕ@�+k@�n/@�r�@��@ي	@��@�Z�@׊�@��@��@��)@���@�}V@���@���@�|�@���@�\�@�?@�"h@���@Ӯ@�*0@�-w@�ѷ@��@��H@ЂA@�b@�a@�Ta@���@���@��3@́�@��@̎�@̃@�v�@�J@˼@�b�@�	l@ʹ$@ʕ�@�l"@�1�@�	@ɫ�@�4�@��@�U2@Ǽ@�P�@�'�@��@��H@��@Ƨ@�M@�)�@�خ@Ŋ	@�@Đ.@��@Ð�@�~�@�K�@��[@�h
@��o@��P@�&�@��c@��@�!-@��R@�Z@�_@�u�@�&@��K@���@�:�@��w@��@�+�@��@���@�L0@�
�@��@���@�&@���@���@���@���@��@��.@�;�@�
�@�_p@�Q�@�A @�"�@�ѷ@�`�@�N<@��@�l�@�b@��@�O@���@���@�9X@���@�c@�)_@�r�@��@��h@�=�@��@��!@�6�@��@�&@���@���@���@�`�@���@�4@��@�;@�ߤ@��h@��o@��9@��M@��"@���@�
�@�˒@���@�9�@��@��u@�J�@���@�|@���@��6@�~@��@���@��a@�rG@�A�@�%F@��K@��u@���@�P�@�"�@��y@��o@�@�@� �@�خ@��*@��'@�X�@��@�3�@�@���@�4�@��/@���@�Q�@���@���@���@��@��@���@��*@���@�K�@�F�@�A @�:�@��@���@���@��@�� @���@��"@��!@���@��+@�h�@�4n@�˒@�|�@�?}@��@�V@���@�z@���@�ϫ@���@�}�@�?}@��@��c@�Ĝ@���@�*�@�J@�u@��z@��h@�8@��@��K@��z@�Ta@���@�c@�4�@�Y@�S@���@���@�M@��@���@�hs@�>�@�@�֡@�|�@�%�@��+@���@��~@�e,@�B�@�+@��@���@��s@��@��+@�X�@���@�Ta@�H�@�J�@��@��@��{@�<6@���@��,@���@�w�@�M@��@��D@���@�N<@���@���@���@�q�@�Ta@�(�@��T@��'@�P�@�!�@�Dg@�)_@��,@�oi@�PH@��g@��*@�zx@�\�@���@�^5@�Q@�7�@�b@��@S@}��@}p�@}:�@|��@|~(@{�W@{Mj@z�}@zOv@z:*@y�@y/@yV@x�`@x�I@wX�@w�@v�,@vH�@u�-@u*0@t��@t��@t�@tu�@t_@tPH@t1'@t"h@t�@s�q@s\)@r��@rL0@qG�@p�@o�}@oj�@nTa@m}�@mT�@m4@m�@lS�@k˒@k]�@k$t@j�"@j�R@j6�@i�D@i��@iF@h�@h��@htT@g�@g
=@f�m@f{�@e^�@d�@d*�@c�}@c��@c!-@b��@bYK@be@a��@a��@ae,@a�@`��@`��@`_@`�@_�w@_J#@^��@^6�@]��@]x�@]�@\�@\c�@\�@[{J@[�@Z�b@Zu@Y�@Y�=@Y}�@YT�@X��@XXy@XI�@X*�@W�@W� @W�F@Wx@V�,@V-@U}�@UV@T��@T�e@Tu�@TS�@TFt@T�@S�F@Sqv@SP�@S9�@S&@R�"@R� @Q�H@Qo @Q0�@PĜ@P��@P~(@P*�@O�K@O�@O>�@O!-@OY@N�M@N�}@N��@N}V@N.�@Mԕ@Mx�@Mf�@MVm@M�@L�/@L�@L9X@L�@Kخ@K�k@KZ�@K)_@J�@J��@J�x@Jv�@J.�@J$�@I��@Ij@I5�@I�@H��@H��@H@G�A@G�F@G i@E�>@EA @E�@D�P@D��@D"h@C�W@C��@C�Q@Cخ@C�}@C�[@C�$@C8@C�@B��@B@A�S@Aq@@�j@@��@@�O@@j@@b@?� @?�q@?�@>��@>�@=�N@=m]@=Vm@=G�@=�@<�?@<r�@;��@;P�@:v�@:@�@:�@9�'@9+�@8Ĝ@8I�@8	�@7��@7��@7b�@7�@7�@6��@6�\@6l�@5��@5��@5��@5�@5��@5m]@5*0@4�@4�_@41@3��@3�k@3�$@3y�@3K�@2�@2xl@2a|@2�@1ϫ@1��@1}�@1f�@0�@0�$@0��@0��@0,=@/ݘ@/y�@/P�@/�@/�@.��@.n�@.B[@-��@-��@-o @-&�@-&�@-q@,�	@,��@,y>@,2�@,1@+��@+�w@+�@+��@+j�@+33@+!-@+@+�@*��@*�y@*ں@*�'@*�h@*�!@*��@*��@*z@*kQ@*_�@*&�@)�@)�@)��@)}�@)L�@(��@(y>@(6@(�@'��@'��@'�}@'��@'�V@'~�@'
=@&�@&��@&_�@&?@%��@%^�@%J�@%/@%�@%	l@$�/@$�@$H@$�@#�6@#��@#y�@#�@#�@#�@# i@"�@"��@"J�@"!�@!��@!�=@![W@!�@ �|@ �p@ �o@�
@�*@��@�f@b�@H�@/�@�@��@��@GE@�@�d@��@f�@A @&�@V@�P@��@��@e�@?�@  @��@��@qv@RT@�@�M@�@�b@i�@.�@��@�'@\�@(�@��@�j@��@j@Q�@K^@Ft@>B@"h@�a@��@F�@�@��@��@GE@{@ �@��@�n@�@��@o @8�@�|@�U@��@�I@tT@S�@�@�@�0@�@{J@H�@9�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	��B	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�xB	�B	��B	��B	��B	��B	��B	��B	��B	�)B	��B	�B	�B	�B	�PB	��B	�PB	�B	��B	��B	��B	�@B	�{B	��B	i*B	b4B	B�B	=<B	?B	FYB	[�B	{�B	��B	��B	��B	��B	��B
�B
�B
bhB
��B
�|B
�}B
�BB�B8RBN�BU�B^5Bd�Bp�BshBxRBv�By�B�B��Bw�BJ�B&2B
�B
�MB
��B
i�B
YB
Q�B
IB
>BB
-�B
�B	��B	��B	��B	��B	�zB	�AB	�!B	�
B	�B	_�B	Q�B	H�B	9>B	qB	�B�B��B�B�BݘBܬB�WB�B��BʌB�~B�NB֡B�B�B�B�B��B��B�B�bB�yB̈́B�rB�BB��B�$B��BٚB�vB�vB�'B�B�$B�6B��B�	B	'�B	MjB	y�B	��B	�$B	�kB	�mB	�DB	��B	ȀB	�fB	˒B	�+B	�B	ƎB	�[B	�B	��B	��B	�<B	��B	�B	��B	�FB	�B	��B	�fB	��B	�	B	��B	��B	��B	�(B	��B	�B	�3B	��B	��B	�XB	�zB	�B	��B	�B	̳B	�^B	��B	��B	ѝB	� B	��B	�,B	��B	�,B	��B	�aB	��B	��B	��B	��B	�B	��B	�B	چB	�#B	�B	�#B	ٚB	ٴB	�EB	�yB	ؓB	��B	�7B	�B	��B	��B	�B	��B	��B	�hB	��B	��B	�B	�IB	�=B	�B	�
B	�B	�B	��B	ۦB	�B	��B	�9B	��B	ՁB	�_B	��B	�FB	��B	�B	�fB	�B	�B	�B	��B	�vB	�B	��B	��B	�B	�B	��B	�B	�)B	�5B	�|B	�B	�B	�HB	��B	��B	��B	�B	��B	�zB	�B	��B	��B	�FB	�B	�B	�B	�B	��B	�B	�B	�B	�B	�=B	�CB	��B	��B	�=B	�B	�B	�B	�mB	�RB	�mB	�B	�B	�B	�B	�
B	�sB	�B	��B	��B	�B	��B	�B	��B	�B	�DB	�yB	�B	�B	�cB	�UB	�-B	��B	��B	�oB	�OB	�B	��B	�|B	�B	�B	�B	�B	��B	�B	�B	�B	�MB	�3B	�3B	�3B	�B	�9B	�tB	�tB	�%B	��B	�TB	�B	�TB	��B	��B	��B	��B	�>B	��B	�B	�>B	��B	��B	�rB	�lB	�B	�8B	��B	�B	��B	��B	��B	�8B	�RB	��B	��B	�2B	�2B	��B	��B	�B	�B	�B	��B	��B	�XB	��B	�*B	�xB	�dB	�]B	��B
 �B
 �B
 �B
 OB
 iB
 �B
 �B
�B
uB
�B
uB
�B
[B
�B
�B
GB
AB
�B
�B
B
gB
�B
B
[B
 �B
 4B
 iB	��B	��B	��B
 �B
;B
�B
�B
�B
�B
�B
�B
�B
uB
B
�B
�B
�B
�B
�B
�B
B
tB
�B
�B
	B
	�B
	�B
	�B

�B

�B

�B

�B
B
)B
xB
�B
6B
6B
6B
B
<B
�B
�B
B
�B
�B
�B
�B
�B
�B
B
PB
PB
B
�B
�B
�B
�B
vB
�B
�B
.B
}B
NB
�B
FB
�B
B
�B
B
]B
�B
xB
xB
CB
�B
�B
�B
/B
�B
IB
�B
�B
�B
�B
�B
�B
OB
jB
�B
�B
OB
OB
B
;B
;B
pB
�B
 B
 'B
 'B
 BB
 BB
!bB
 �B
 �B
!HB
!bB
!�B
"�B
"�B
#:B
# B
#�B
#�B
$@B
$&B
$B
$�B
$�B
%�B
&�B
&LB
'B
'B
'�B
'�B
($B
(�B
(�B
)B
)*B
)yB
)�B
)�B
)�B
)�B
)�B
)�B
)�B
*�B
+B
+kB
-CB
.�B
.�B
.�B
/B
/�B
/�B
/�B
/�B
.�B
.�B
.IB
./B
-�B
-wB
-�B
.cB
.}B
.�B
.�B
.�B
/ B
/OB
/�B
0!B
1�B
2�B
2�B
2aB
2-B
1�B
2B
1�B
1vB
2aB
2B
2�B
3�B
3�B
3�B
3�B
2aB
1�B
1[B
1�B
1�B
2�B
33B
33B
2�B
3�B
5�B
6`B
6FB
6B
5�B
7B
72B
6�B
7�B
8B
8�B
8�B
9�B
9�B
9�B
9�B
9�B
:^B
:�B
:�B
:�B
:�B
;�B
<PB
="B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
?.B
?}B
?�B
?�B
?�B
?�B
?�B
?�B
@ B
@4B
@B
@B
@iB
@OB
@�B
@�B
@�B
@�B
@�B
@�B
AUB
A�B
A�B
A�B
A�B
BB
BB
B[B
B�B
CB
C�B
C�B
C�B
C�B
C�B
D3B
DgB
D�B
D�B
EB
EB
E9B
E9B
E�B
FtB
F?B
F?B
FYB
FtB
F�B
F�B
GzB
G�B
G�B
G�B
HB
G�B
HB
H1B
H�B
IRB
J	B
J#B
JXB
J=B
J�B
J�B
J�B
J�B
KB
KB
KB
KB
KB
KDB
LB
L�B
M�B
N<B
NpB
N�B
N�B
N�B
OB
OBB
OvB
O�B
O\B
O�B
O�B
O�B
O�B
O�B
P.B
P.B
PB
PB
P�B
P�B
QB
QB
QB
QNB
Q�B
Q�B
RB
R:B
RTB
R:B
RTB
RoB
RoB
R�B
R�B
R�B
R�B
R:B
R B
Q�B
Q�B
Q�B
R B
S[B
S�B
S�B
S�B
S�B
S�B
T,B
T,B
T,B
T,B
T,B
TFB
TaB
T�B
T�B
T�B
U�B
VB
V�B
V�B
V�B
V�B
V�B
W
B
W
B
W?B
W�B
W�B
XB
XEB
X�B
X�B
X�B
X�B
X�B
YKB
YB
ZB
ZQB
Z7B
ZQB
Z�B
Z�B
Z�B
[=B
[qB
[�B
[�B
\B
\xB
\]B
\�B
\�B
\�B
]dB
]�B
]~B
]�B
]dB
]�B
]�B
]�B
^B
^�B
_B
^�B
^�B
^�B
_!B
_�B
_�B
_�B
_�B
`B
`'B
`BB
`BB
`�B
`�B
`�B
`�B
abB
a|B
b4B
bNB
b�B
bhB
b�B
c B
c:B
c�B
c�B
c�B
c�B
d&B
d&B
d&B
d�B
d�B
d�B
d�B
d�B
eB
eB
eFB
ezB
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
fB
fB
fB
f2B
f2B
ffB
f�B
f�B
gB
gB
g8B
g�B
g�B
h
B
h$B
h$B
h>B
hXB
hXB
hXB
hsB
iB
h�B
iyB
i_B
i_B
jeB
j0B
j0B
jeB
jeB
jeB
j�B
j�B
kB
kQB
kkB
kkB
k�B
k�B
k�B
k�B
k�B
k�B
l"B
l�B
lqB
l�B
l�B
m)B
m]B
mCB
m]B
m�B
ncB
n}B
n/B
ncB
n�B
n�B
n�B
n�B
oB
oB
oiB
o�B
o�B
o�B
o�B
p!B
p!B
p;B
p!B
poB
p�B
p�B
p�B
p�B
q'B
qvB
qvB
q�B
q�B
q�B
q�B
rB
r-B
raB
r�B
r�B
r�B
s3B
sMB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
tTB
t�B
t�B
t�B
t�B
u?B
u�B
u�B
u�B
v+B
vFB
v+B
v+B
v`B
v�B
v�B
v�B
v�B
v�B
w2B
w2B
w�B
w�B
w�B
w�B
xB
xRB
x8B
xR111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	��B	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�xB	�B	��B	��B	��B	��B	��B	��B	��B	�)B	��B	�B	�B	�B	�PB	��B	�PB	�B	��B	��B	��B	�@B	�{B	��B	i*B	b4B	B�B	=<B	?B	FYB	[�B	{�B	��B	��B	��B	��B	��B
�B
�B
bhB
��B
�|B
�}B
�BB�B8RBN�BU�B^5Bd�Bp�BshBxRBv�By�B�B��Bw�BJ�B&2B
�B
�MB
��B
i�B
YB
Q�B
IB
>BB
-�B
�B	��B	��B	��B	��B	�zB	�AB	�!B	�
B	�B	_�B	Q�B	H�B	9>B	qB	�B�B��B�B�BݘBܬB�WB�B��BʌB�~B�NB֡B�B�B�B�B��B��B�B�bB�yB̈́B�rB�BB��B�$B��BٚB�vB�vB�'B�B�$B�6B��B�	B	'�B	MjB	y�B	��B	�$B	�kB	�mB	�DB	��B	ȀB	�fB	˒B	�+B	�B	ƎB	�[B	�B	��B	��B	�<B	��B	�B	��B	�FB	�B	��B	�fB	��B	�	B	��B	��B	��B	�(B	��B	�B	�3B	��B	��B	�XB	�zB	�B	��B	�B	̳B	�^B	��B	��B	ѝB	� B	��B	�,B	��B	�,B	��B	�aB	��B	��B	��B	��B	�B	��B	�B	چB	�#B	�B	�#B	ٚB	ٴB	�EB	�yB	ؓB	��B	�7B	�B	��B	��B	�B	��B	��B	�hB	��B	��B	�B	�IB	�=B	�B	�
B	�B	�B	��B	ۦB	�B	��B	�9B	��B	ՁB	�_B	��B	�FB	��B	�B	�fB	�B	�B	�B	��B	�vB	�B	��B	��B	�B	�B	��B	�B	�)B	�5B	�|B	�B	�B	�HB	��B	��B	��B	�B	��B	�zB	�B	��B	��B	�FB	�B	�B	�B	�B	��B	�B	�B	�B	�B	�=B	�CB	��B	��B	�=B	�B	�B	�B	�mB	�RB	�mB	�B	�B	�B	�B	�
B	�sB	�B	��B	��B	�B	��B	�B	��B	�B	�DB	�yB	�B	�B	�cB	�UB	�-B	��B	��B	�oB	�OB	�B	��B	�|B	�B	�B	�B	�B	��B	�B	�B	�B	�MB	�3B	�3B	�3B	�B	�9B	�tB	�tB	�%B	��B	�TB	�B	�TB	��B	��B	��B	��B	�>B	��B	�B	�>B	��B	��B	�rB	�lB	�B	�8B	��B	�B	��B	��B	��B	�8B	�RB	��B	��B	�2B	�2B	��B	��B	�B	�B	�B	��B	��B	�XB	��B	�*B	�xB	�dB	�]B	��B
 �B
 �B
 �B
 OB
 iB
 �B
 �B
�B
uB
�B
uB
�B
[B
�B
�B
GB
AB
�B
�B
B
gB
�B
B
[B
 �B
 4B
 iB	��B	��B	��B
 �B
;B
�B
�B
�B
�B
�B
�B
�B
uB
B
�B
�B
�B
�B
�B
�B
B
tB
�B
�B
	B
	�B
	�B
	�B

�B

�B

�B

�B
B
)B
xB
�B
6B
6B
6B
B
<B
�B
�B
B
�B
�B
�B
�B
�B
�B
B
PB
PB
B
�B
�B
�B
�B
vB
�B
�B
.B
}B
NB
�B
FB
�B
B
�B
B
]B
�B
xB
xB
CB
�B
�B
�B
/B
�B
IB
�B
�B
�B
�B
�B
�B
OB
jB
�B
�B
OB
OB
B
;B
;B
pB
�B
 B
 'B
 'B
 BB
 BB
!bB
 �B
 �B
!HB
!bB
!�B
"�B
"�B
#:B
# B
#�B
#�B
$@B
$&B
$B
$�B
$�B
%�B
&�B
&LB
'B
'B
'�B
'�B
($B
(�B
(�B
)B
)*B
)yB
)�B
)�B
)�B
)�B
)�B
)�B
)�B
*�B
+B
+kB
-CB
.�B
.�B
.�B
/B
/�B
/�B
/�B
/�B
.�B
.�B
.IB
./B
-�B
-wB
-�B
.cB
.}B
.�B
.�B
.�B
/ B
/OB
/�B
0!B
1�B
2�B
2�B
2aB
2-B
1�B
2B
1�B
1vB
2aB
2B
2�B
3�B
3�B
3�B
3�B
2aB
1�B
1[B
1�B
1�B
2�B
33B
33B
2�B
3�B
5�B
6`B
6FB
6B
5�B
7B
72B
6�B
7�B
8B
8�B
8�B
9�B
9�B
9�B
9�B
9�B
:^B
:�B
:�B
:�B
:�B
;�B
<PB
="B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
?.B
?}B
?�B
?�B
?�B
?�B
?�B
?�B
@ B
@4B
@B
@B
@iB
@OB
@�B
@�B
@�B
@�B
@�B
@�B
AUB
A�B
A�B
A�B
A�B
BB
BB
B[B
B�B
CB
C�B
C�B
C�B
C�B
C�B
D3B
DgB
D�B
D�B
EB
EB
E9B
E9B
E�B
FtB
F?B
F?B
FYB
FtB
F�B
F�B
GzB
G�B
G�B
G�B
HB
G�B
HB
H1B
H�B
IRB
J	B
J#B
JXB
J=B
J�B
J�B
J�B
J�B
KB
KB
KB
KB
KB
KDB
LB
L�B
M�B
N<B
NpB
N�B
N�B
N�B
OB
OBB
OvB
O�B
O\B
O�B
O�B
O�B
O�B
O�B
P.B
P.B
PB
PB
P�B
P�B
QB
QB
QB
QNB
Q�B
Q�B
RB
R:B
RTB
R:B
RTB
RoB
RoB
R�B
R�B
R�B
R�B
R:B
R B
Q�B
Q�B
Q�B
R B
S[B
S�B
S�B
S�B
S�B
S�B
T,B
T,B
T,B
T,B
T,B
TFB
TaB
T�B
T�B
T�B
U�B
VB
V�B
V�B
V�B
V�B
V�B
W
B
W
B
W?B
W�B
W�B
XB
XEB
X�B
X�B
X�B
X�B
X�B
YKB
YB
ZB
ZQB
Z7B
ZQB
Z�B
Z�B
Z�B
[=B
[qB
[�B
[�B
\B
\xB
\]B
\�B
\�B
\�B
]dB
]�B
]~B
]�B
]dB
]�B
]�B
]�B
^B
^�B
_B
^�B
^�B
^�B
_!B
_�B
_�B
_�B
_�B
`B
`'B
`BB
`BB
`�B
`�B
`�B
`�B
abB
a|B
b4B
bNB
b�B
bhB
b�B
c B
c:B
c�B
c�B
c�B
c�B
d&B
d&B
d&B
d�B
d�B
d�B
d�B
d�B
eB
eB
eFB
ezB
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
fB
fB
fB
f2B
f2B
ffB
f�B
f�B
gB
gB
g8B
g�B
g�B
h
B
h$B
h$B
h>B
hXB
hXB
hXB
hsB
iB
h�B
iyB
i_B
i_B
jeB
j0B
j0B
jeB
jeB
jeB
j�B
j�B
kB
kQB
kkB
kkB
k�B
k�B
k�B
k�B
k�B
k�B
l"B
l�B
lqB
l�B
l�B
m)B
m]B
mCB
m]B
m�B
ncB
n}B
n/B
ncB
n�B
n�B
n�B
n�B
oB
oB
oiB
o�B
o�B
o�B
o�B
p!B
p!B
p;B
p!B
poB
p�B
p�B
p�B
p�B
q'B
qvB
qvB
q�B
q�B
q�B
q�B
rB
r-B
raB
r�B
r�B
r�B
s3B
sMB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
tTB
t�B
t�B
t�B
t�B
u?B
u�B
u�B
u�B
v+B
vFB
v+B
v+B
v`B
v�B
v�B
v�B
v�B
v�B
w2B
w2B
w�B
w�B
w�B
w�B
xB
xRB
x8B
xR111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104851  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172641  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172641  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172642                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022649  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605022649  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131506                      G�O�G�O�G�O�                