CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:56:15Z creation;2022-06-04T17:56:15Z conversion to V3.1      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604175615  20220610141506  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               AA   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�GԪz1   @�G�""""@0�fffff�b�n��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A��A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bw��B�  B�  B�  B�  B���B���B�ffB�33B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B���C   C  C  C  C  C
  C  C  C  C  C  C  C  C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CC�fCF  CH�CJ�CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx�Cz  C|  C~  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DCfDC�fDD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D�|�D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�H@���@���A�A Q�A@Q�A`Q�A�(�A�(�A�(�A�(�A���A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B@{BHz�BP{BX{B`{Bh{Bp{Bw�B�
=B�
=B�
=B�
=B��
B��
B�p�B�=pB��
B�
=B��
B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�=pB��
B��
B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B��
C CCCCC
CCCCCCCC�CCC C"C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@CBCC�CFCH�CJ�CLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCx�CzC|C~C��C��C��C��C��C�\C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C���C��C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD�D�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDC�DC��DDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D��qD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�C�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D�}qD���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�@�D怤D���D� �D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�D�A�GzA�R�A�T�A�S&A�\�A�_�A�_�A�e`A�j�A�_pA�bNA�m)A�|PAЁoA�t�A�xAІ�AЏ�AВoAБhAА.AЎ�AЌ�AЈ�AЇ�AЈ1AЇ�A��A�y�A�qAA�
rAϨ�A��iA�/�A̅SA���A��A���A�A��A�=<A�o5A��+A��EA�d�A��ZA��"A�-CA��A�C-A��BA�W
A��:A�A���A�-�A��}A���A��A�B�A�;�A���A��A���A�L0A���A���A��A�T�A���A�m�A�$@A���A�w�A���A��A��MA��{A�u�A�aA�8RA���A�F?A�`A�DgA�@�A�1A�AUA��A��A�r�A��A��*A��}A���A��A�]dA��A�ĜA�b�A�k�A�"hA��dA��A�0�A��SA��6A�F�A}�IAx�ApOvAk/Aie�AfVA`�	A\n/AV�xAR�APffAN��AL��AK�!AL*�AKu%AI@�AE�oAD�FADMACqvA@	A;�PA:��A9��A9XA9�A7�HA6�A5a|A5:�A5�A4a|A1��A/($A.r�A.$A-�"A-O�A+x�A*M�A*W�A*�dA*xA)�A)��A*�A*-A+~�A+��A*�4A)]�A)��A*j�A*�A)�KA)8�A(ffA'�A#�A"[�A!VA!W�A"7LA"�A#@�A#��A#}VA"�0A!�A �A�<A�A�RA�>A��A�6AB�A��A��A|�A�A�A�	A��A�eA6�A1'AE9A!�A�A��A�;AV�A|A�jAԕA��A!�A�A!A��AA?�A
��A
OA	J�A��A��AoA�4A�zAuA�A�A �<@���@�ߤ@�q�@�m�@��Q@�ݘ@��D@���@�>B@��q@��@���@��N@�U2@�x@���@�0@�6@�@�d�@�ݘ@�/@���@�.@�@���@�V�@�@�ϫ@�!�@��@�@��.@�A @���@�@�N<@��v@�O@�Q@�'@��@�+k@�?}@�{@�F@��@�v�@�V�@�q@�@�6�@��@��f@�c @�9X@��m@�J#@�!�@�&�@��]@��&@��@�($@ݻ0@�2a@�7�@�@��X@�@���@ؖ�@�b�@�%@֥z@�#:@Շ�@���@Ӳ-@��)@�*�@��r@Ѽ@�rG@���@�\�@�_@υ�@��@�e�@�	�@���@̈́M@��c@��m@̞@�?@�ϫ@�@�S�@�9�@��K@�L0@�	@��@�l�@�K�@�l�@��@Ȉ�@�>B@��@ǰ�@��j@���@�zx@��@��@�Ĝ@ƛ�@��@���@��@Ĩ�@�H�@�M@��N@��d@î@�_p@¤�@���@�Mj@���@���@�hs@��z@��@��@���@�l�@�6z@��@�}V@�(�@��3@�~�@�]d@�A�@��}@�-�@��q@�6z@���@�a@�O@�:�@�S�@�6z@��	@�|�@�Xy@���@�H�@��@��/@��.@��@��^@���@��f@�N<@���@�Q�@�8�@��)@�,�@��@��!@���@�	@��t@���@�]�@��@��z@���@�%�@��T@��@���@�_p@�o@��M@��+@��g@��-@���@�.I@�
=@��K@���@�d�@�O@��o@��;@���@���@�zx@�_p@�Q�@�33@��M@��@�n�@�<�@�,=@�O@��@�c�@��f@�p;@�4@��@�L�@��@��2@���@�y>@�H@��r@��:@�4�@��p@���@�x@��{@�K�@���@��o@�$@�@�zx@�-w@�.I@�+@��v@��@�	@���@�qv@�O�@��@�ߤ@��r@�?�@���@��@�%F@��@��?@�oi@��A@��-@�|@�;d@��[@�c @��@��@��j@���@��k@�g�@��@���@��F@�_@�1'@�x@��T@���@�e�@�:�@��@�ѷ@��@�]d@�:*@��}@���@�^�@�4@��P@��@�p;@�c @�_�@�!@��w@���@�_p@�/�@�S@���@���@�M@��H@��X@�L�@�+�@��8@�U2@��@��h@�;d@�)_@��f@��/@��y@��4@�Xy@�H�@�8�@��j@��z@��{@�/@��"@��,@��@��1@�~(@�Z@�%�@��@�o @�-w@�ߤ@���@��F@�Q@��@�ԕ@��@�Q�@��@��@��h@��1@�D�@�1'@�+k@�	@��@���@��{@�c@��M@�o�@�/@���@���@��!@��Y@�C�@���@��;@�\�@��@�A�@��@�ں@�q@�7@���@�o�@��P@��b@�S�@�J@E9@~.�@}�@|ی@|��@|H@|4n@|�@{��@{e�@{Y@zߤ@z�\@zYK@y��@yu�@x�5@x'R@w��@wb�@uu�@tz�@t�9@t%�@s��@s i@r҉@r��@r��@rQ@q�@q\�@q=�@q=�@q%@pq@o��@o�@n�F@n�@m�3@m�M@mG�@m \@l_@k�]@k�@kC�@k�@j�@j�2@j��@j�@i��@i(�@hĜ@h�z@hbN@g� @g>�@f��@f;�@e�C@ezx@eVm@e#�@d|�@d%�@d@d�@c�@cn/@b�@a�#@a�=@a;@`*�@`�@_�m@_�k@_/�@^8�@]m]@\�@\�4@\�D@\�Y@\�Y@\�Y@\Xy@[�r@Zȴ@Yԕ@Y��@Y�"@Y[W@Y%@Xq@X�@WMj@V��@V{@U��@U��@U*0@T��@T��@T�@S�4@SS�@S
=@R��@Rp;@Q�D@Q��@P�O@O�@O~�@OF�@O@N��@M��@M��@M4@L�`@L��@L6@K�@Kx@K�@J�1@I��@I��@IIR@H��@H��@H>B@HM@G�@G�@GH�@G�@F�@E�)@Ec@E!�@E�@D�@D�.@Db@C��@C�V@Co�@C@B�@Bߤ@B��@B��@B��@B�1@B��@B3�@A@A[W@A;@@��@@�@@>B@?�
@?g�@?33@>��@>�@>}V@>M�@>8�@=��@=c�@<��@<1'@<�@;��@;b�@:�"@:�\@:a|@:($@9��@9��@9��@9hs@92a@9�@8��@8�z@8,=@7��@7W?@7&@6�y@6�h@6��@6H�@6�@5�d@5`B@5q@4�[@4��@4��@4Xy@4 �@4@4�@3�m@3�6@3��@3�:@3RT@3,�@2��@2�R@2i�@2.�@2@1��@1Dg@0�4@0Xy@0~@0�@/�W@/��@/A�@/o@.��@.v�@.	@-�)@-��@-��@-��@-}�@-s�@-o @-Y�@,�@,u�@,Ft@,�@+�+@+�Q@+�0@+e�@*�"@*�@*��@*J�@)��@)IR@)q@(�P@(��@(�u@(y>@(_@(C-@(-�@("h@( �@'�@'�;@'�}@'��@'�q@'�@@'e�@&�@&��@%�@%o @$��@$�$@$�@$Xy@#�@#�K@#��@#F�@"�c@"ں@"�!@"a|@!�D@!@!}�@!IR@!(�@!@!+@!@ ��@ ��@ �/@ ֡@ �@ PH@ ,=@ G@�@��@8@�@(@S@��@�s@�x@�+@YK@)�@�>@��@�C@�'@�"@Vm@8�@�@��@�@�@r�@  @��@O@�8@�H@҉@�+@�@hs@�[@y>@N�@x@�W@��@��@�6@��@�f@C�@ȴ@��@p;@ff@Ta@)�@��@hs@0�@@@�@�j@w�@Ft@�@ƨ@�{@RT@"�@�@�<@�R@�@{�@{�@v�@n�@a|@3�@�D@��@��@a�@^�@X@J�@G�@4@+@�@��@e�@(�@ �@�@��@X�@$t11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�D�A�GzA�R�A�T�A�S&A�\�A�_�A�_�A�e`A�j�A�_pA�bNA�m)A�|PAЁoA�t�A�xAІ�AЏ�AВoAБhAА.AЎ�AЌ�AЈ�AЇ�AЈ1AЇ�A��A�y�A�qAA�
rAϨ�A��iA�/�A̅SA���A��A���A�A��A�=<A�o5A��+A��EA�d�A��ZA��"A�-CA��A�C-A��BA�W
A��:A�A���A�-�A��}A���A��A�B�A�;�A���A��A���A�L0A���A���A��A�T�A���A�m�A�$@A���A�w�A���A��A��MA��{A�u�A�aA�8RA���A�F?A�`A�DgA�@�A�1A�AUA��A��A�r�A��A��*A��}A���A��A�]dA��A�ĜA�b�A�k�A�"hA��dA��A�0�A��SA��6A�F�A}�IAx�ApOvAk/Aie�AfVA`�	A\n/AV�xAR�APffAN��AL��AK�!AL*�AKu%AI@�AE�oAD�FADMACqvA@	A;�PA:��A9��A9XA9�A7�HA6�A5a|A5:�A5�A4a|A1��A/($A.r�A.$A-�"A-O�A+x�A*M�A*W�A*�dA*xA)�A)��A*�A*-A+~�A+��A*�4A)]�A)��A*j�A*�A)�KA)8�A(ffA'�A#�A"[�A!VA!W�A"7LA"�A#@�A#��A#}VA"�0A!�A �A�<A�A�RA�>A��A�6AB�A��A��A|�A�A�A�	A��A�eA6�A1'AE9A!�A�A��A�;AV�A|A�jAԕA��A!�A�A!A��AA?�A
��A
OA	J�A��A��AoA�4A�zAuA�A�A �<@���@�ߤ@�q�@�m�@��Q@�ݘ@��D@���@�>B@��q@��@���@��N@�U2@�x@���@�0@�6@�@�d�@�ݘ@�/@���@�.@�@���@�V�@�@�ϫ@�!�@��@�@��.@�A @���@�@�N<@��v@�O@�Q@�'@��@�+k@�?}@�{@�F@��@�v�@�V�@�q@�@�6�@��@��f@�c @�9X@��m@�J#@�!�@�&�@��]@��&@��@�($@ݻ0@�2a@�7�@�@��X@�@���@ؖ�@�b�@�%@֥z@�#:@Շ�@���@Ӳ-@��)@�*�@��r@Ѽ@�rG@���@�\�@�_@υ�@��@�e�@�	�@���@̈́M@��c@��m@̞@�?@�ϫ@�@�S�@�9�@��K@�L0@�	@��@�l�@�K�@�l�@��@Ȉ�@�>B@��@ǰ�@��j@���@�zx@��@��@�Ĝ@ƛ�@��@���@��@Ĩ�@�H�@�M@��N@��d@î@�_p@¤�@���@�Mj@���@���@�hs@��z@��@��@���@�l�@�6z@��@�}V@�(�@��3@�~�@�]d@�A�@��}@�-�@��q@�6z@���@�a@�O@�:�@�S�@�6z@��	@�|�@�Xy@���@�H�@��@��/@��.@��@��^@���@��f@�N<@���@�Q�@�8�@��)@�,�@��@��!@���@�	@��t@���@�]�@��@��z@���@�%�@��T@��@���@�_p@�o@��M@��+@��g@��-@���@�.I@�
=@��K@���@�d�@�O@��o@��;@���@���@�zx@�_p@�Q�@�33@��M@��@�n�@�<�@�,=@�O@��@�c�@��f@�p;@�4@��@�L�@��@��2@���@�y>@�H@��r@��:@�4�@��p@���@�x@��{@�K�@���@��o@�$@�@�zx@�-w@�.I@�+@��v@��@�	@���@�qv@�O�@��@�ߤ@��r@�?�@���@��@�%F@��@��?@�oi@��A@��-@�|@�;d@��[@�c @��@��@��j@���@��k@�g�@��@���@��F@�_@�1'@�x@��T@���@�e�@�:�@��@�ѷ@��@�]d@�:*@��}@���@�^�@�4@��P@��@�p;@�c @�_�@�!@��w@���@�_p@�/�@�S@���@���@�M@��H@��X@�L�@�+�@��8@�U2@��@��h@�;d@�)_@��f@��/@��y@��4@�Xy@�H�@�8�@��j@��z@��{@�/@��"@��,@��@��1@�~(@�Z@�%�@��@�o @�-w@�ߤ@���@��F@�Q@��@�ԕ@��@�Q�@��@��@��h@��1@�D�@�1'@�+k@�	@��@���@��{@�c@��M@�o�@�/@���@���@��!@��Y@�C�@���@��;@�\�@��@�A�@��@�ں@�q@�7@���@�o�@��P@��b@�S�@�J@E9@~.�@}�@|ی@|��@|H@|4n@|�@{��@{e�@{Y@zߤ@z�\@zYK@y��@yu�@x�5@x'R@w��@wb�@uu�@tz�@t�9@t%�@s��@s i@r҉@r��@r��@rQ@q�@q\�@q=�@q=�@q%@pq@o��@o�@n�F@n�@m�3@m�M@mG�@m \@l_@k�]@k�@kC�@k�@j�@j�2@j��@j�@i��@i(�@hĜ@h�z@hbN@g� @g>�@f��@f;�@e�C@ezx@eVm@e#�@d|�@d%�@d@d�@c�@cn/@b�@a�#@a�=@a;@`*�@`�@_�m@_�k@_/�@^8�@]m]@\�@\�4@\�D@\�Y@\�Y@\�Y@\Xy@[�r@Zȴ@Yԕ@Y��@Y�"@Y[W@Y%@Xq@X�@WMj@V��@V{@U��@U��@U*0@T��@T��@T�@S�4@SS�@S
=@R��@Rp;@Q�D@Q��@P�O@O�@O~�@OF�@O@N��@M��@M��@M4@L�`@L��@L6@K�@Kx@K�@J�1@I��@I��@IIR@H��@H��@H>B@HM@G�@G�@GH�@G�@F�@E�)@Ec@E!�@E�@D�@D�.@Db@C��@C�V@Co�@C@B�@Bߤ@B��@B��@B��@B�1@B��@B3�@A@A[W@A;@@��@@�@@>B@?�
@?g�@?33@>��@>�@>}V@>M�@>8�@=��@=c�@<��@<1'@<�@;��@;b�@:�"@:�\@:a|@:($@9��@9��@9��@9hs@92a@9�@8��@8�z@8,=@7��@7W?@7&@6�y@6�h@6��@6H�@6�@5�d@5`B@5q@4�[@4��@4��@4Xy@4 �@4@4�@3�m@3�6@3��@3�:@3RT@3,�@2��@2�R@2i�@2.�@2@1��@1Dg@0�4@0Xy@0~@0�@/�W@/��@/A�@/o@.��@.v�@.	@-�)@-��@-��@-��@-}�@-s�@-o @-Y�@,�@,u�@,Ft@,�@+�+@+�Q@+�0@+e�@*�"@*�@*��@*J�@)��@)IR@)q@(�P@(��@(�u@(y>@(_@(C-@(-�@("h@( �@'�@'�;@'�}@'��@'�q@'�@@'e�@&�@&��@%�@%o @$��@$�$@$�@$Xy@#�@#�K@#��@#F�@"�c@"ں@"�!@"a|@!�D@!@!}�@!IR@!(�@!@!+@!@ ��@ ��@ �/@ ֡@ �@ PH@ ,=@ G@�@��@8@�@(@S@��@�s@�x@�+@YK@)�@�>@��@�C@�'@�"@Vm@8�@�@��@�@�@r�@  @��@O@�8@�H@҉@�+@�@hs@�[@y>@N�@x@�W@��@��@�6@��@�f@C�@ȴ@��@p;@ff@Ta@)�@��@hs@0�@@@�@�j@w�@Ft@�@ƨ@�{@RT@"�@�@�<@�R@�@{�@{�@v�@n�@a|@3�@�D@��@��@a�@^�@X@J�@G�@4@+@�@��@e�@(�@ �@�@��@X�@$t11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
Q�B
QhB
QB
Q B
QNB
Q�B
Q�B
R�B
R�B
T,B
Q�B
Q�B
V�B
_�B
kkB
z�B
� B
��B
�B
�B
��B
��B
��B
��B
�HB
�hB
�&B
�[B
��B
��B
��B
�uB
�FB
��B
�SB
��B
i�B
g�B
g�B
p;B
��B
�'B
��B
�1B
��B
��B
�B
��B
�B3BmB-�BAUBdB��B�CB�&B�wB�jB�	B�nB�B��BSB
rB�BqB#�B$�B$B'�B*�B)�B*B'mB"�B BBpB�B	�B�B��B�B�UB�
B��B�"B��B��B� B��B|�Bq�BabB;�B#�B�B�B
�JB
�5B
�PB
��B
�xB
�9B
�B
z�B
o�B
b�B
<B
KB	�B	��B	��B	v`B	_�B	;�B	# B	�B�AB�B�B��B�B	KB	�B��B�5B�nB	AB�cB�|B��B��B��B	�B	�B	AB	T�B	ZB	_�B	e�B	nIB	oiB	f�B	f�B	cTB	`�B	[�B	PHB	V�B	h�B	~(B	�B	�<B	��B	��B	�jB	�/B	�B	�B	�NB	�tB
EB
B
�B
B
�B
"B	��B	�
B	��B	��B	��B
UB

=B
}B
�B
}B
6B
�B
�B	�B	�GB	�2B	�B	�uB	�;B	�%B	�0B	̳B	�JB	��B	бB	�9B	��B	�eB	�B	��B	�>B	�B	�B	�B	�jB	ؓB	��B	ϑB	�"B	�\B	�}B	уB	��B	��B	ۦB	ۦB	��B	�=B	چB	��B	��B	��B	�SB	ðB	�6B	�9B	��B	��B	�|B	��B	��B	��B	��B	�mB	��B	��B	�B	��B	��B	�>B	��B	�B	��B	�'B	��B	�B	�B	��B	�KB	�=B	��B	�oB	�RB	�DB	�^B	�B	�}B	��B	�UB	��B	��B	�9B	��B	�B	��B	��B	�B	�jB	��B	�JB	��B	��B	�JB	͟B	� B	�NB	�hB	�HB	��B	�TB	�B	�@B	�FB	�gB	�KB	�7B	ڠB	�#B	��B	یB	چB	�B	ٴB	�$B	�MB	�2B	��B	�SB	ևB	��B	�[B	�[B	�[B	��B	��B	ՁB	�MB	ՁB	ՁB	ՁB	�B	�9B	�SB	�$B	�yB	רB	��B	��B	�7B	�=B	��B	ڠB	ڠB	یB	��B	ݲB	�VB	��B	��B	��B	��B	�BB	��B	�BB	�B	�'B	߾B	�VB	��B	�-B	��B	�B	�B	�B	�@B	�@B	�nB	�B	�~B	�OB	�\B	�B	�BB	�B	�B	�B	�2B	�zB	�B	�B	�B	�bB	�B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�tB	�B	�2B	��B	�B	�@B	�2B	�$B	��B	��B	�;B	�oB	��B	��B	�ZB	��B	�B	�?B	��B	��B	��B	�2B	��B	�B	�B	�RB	�8B	�$B	��B	�rB	�XB	��B	��B	�0B	��B	��B	��B	��B	��B	��B	�JB	�"B	�B	�cB	�}B	�cB	��B	��B	�}B	�cB	��B	�HB	��B	��B	��B	��B
  B
  B
 �B
 �B
 �B
 �B
 �B
 �B
 B
;B
 B
UB
�B
'B
�B
�B
�B
�B
B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
YB
YB
�B
+B
�B
�B
B
	B
�B
	B
	B
	�B
	lB
	�B

	B

=B
	�B

#B

�B

�B

�B
B
)B
�B
�B
B
�B
PB
jB
PB
PB
B
pB
�B
�B
vB
�B
B
HB
.B
.B
HB
HB
�B
 B
 B
hB
�B
�B
�B
 B
TB
�B
�B
�B
�B
�B
&B
�B
aB
{B
�B
B
�B
�B
�B
�B
9B
�B
�B

B

B
$B
�B
+B
�B
�B
�B
�B
�B
�B
1B
�B
�B
B
�B
KB
B
)B
�B
xB
�B
~B
�B
IB
�B
B
�B
�B
�B
�B
!B
�B
�B
!B
�B
�B
�B
 �B
!-B
!|B
!|B
!bB
!�B
"B
"4B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
%B
%B
%,B
%zB
%`B
%`B
%�B
&B
%�B
%�B
&�B
&LB
%�B
&B
%,B
$�B
&�B
(>B
(>B
(�B
(�B
(�B
'�B
'B
'�B
(�B
)yB
)yB
(�B
'mB
'�B
(
B
(XB
(sB
(�B
(�B
(�B
(�B
*�B
*�B
+�B
+QB
,=B
-)B
-wB
,�B
+�B
)�B
(XB
*KB
)�B
)�B
)�B
)�B
)�B
)�B
)�B
*B
+kB
+�B
+�B
+kB
*�B
)�B
)_B
)�B
)�B
)�B
*eB
*�B
*�B
+QB
+B
+6B
+kB
+kB
+�B
+�B
,B
+�B
,�B
-CB
-�B
-�B
-�B
.�B
/iB
/�B
0B
/�B
0�B
0�B
0�B
1'B
1vB
1vB
1vB
1�B
1�B
2�B
3MB
3hB
4B
4�B
4�B
4�B
4�B
5B
6`B
6�B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
9$B
:*B
9�B
:B
9�B
:^B
:�B
;B
;�B
<6B
<�B
<�B
="B
=qB
=�B
>B
>wB
>�B
?B
?HB
?�B
?�B
@ B
@OB
@�B
AUB
A�B
A�B
A�B
B[B
B�B
C-B
C{B
C�B
C�B
DgB
D�B
EB
EB
E�B
FYB
F�B
F�B
GzB
G�B
G�B
HB
H1B
H�B
H�B
H�B
IlB
I�B
JrB
J�B
J�B
J�B
K)B
K�B
KxB
K�B
K�B
LdB
LdB
L~B
L�B
L�B
L�B
L�B
L�B
MB
MjB
M�B
NB
NpB
NpB
N�B
O(B
OvB
O�B
P.B
PHB
PbB
P�B
PbB
P�B
Q4B
RoB
R�B
R�B
R�B
SB
SuB
S�B
S�B
TB
T�B
T�B
T�B
T�B
T�B
T�B
T�B
UB
U�B
VB
VB
VmB
V�B
V�B
V�B
W
B
W?B
WsB
W�B
XB
XEB
XyB
XyB
X�B
X�B
X�B
YB
YB
Y1B
YKB
YeB
Y�B
Y�B
ZB
ZQB
Z�B
Z�B
[#B
[=B
[�B
\�B
\�B
]B
\�B
]B
]dB
]�B
]�B
]�B
^OB
^�B
^�B
^�B
^�B
^�B
_B
_B
_B
_B
_�B
`B
`'B
`\B
`vB
`vB
`�B
`�B
abB
aHB
a�B
a�B
b�B
b�B
b�B
cB
cTB
cnB
c�B
c�B
c�B
c�B
c�B
c�B
dB
d&B
dB
d@B
d&B
dB
dtB
d�B
d�B
e`B
ezB
e�B
e�B
e�B
f2B
f�B
f�B
f�B
gRB
g�B
gRB
g�B
h
B
hXB
h�B
h�B
h�B
i*B
i*B
i*B
iDB
iDB
i_B
iyB
iyB
i�B
jB
jKB
jeB
jeB
j�B
k6B
k6B
k6B
kQB
kQB
k�B
k�B
k�B
k�B
lB
lqB
l�B
l�B
l�B
l�B
mB
mB
m)B
mwB
m�B
m�B
m�B
nIB
n�B
n�B
o5B
oOB
o5B
o�B
p!B
p�B
q'B
qvB
q�B
q�B
q�B
q�B
q�B
rB
rB
rGB
r|B
sB
s3B
shB
sMB
shB
shB
tB
t�B
t�B
uB
uZB
u�B
vB
v+B
v�B
v�B
wB
wB
wfB
w�B
w�B
w�B
w�B
xB
xB
xB
xB
xB
x8B
x�B
x�B
x�B
y	B
y	B
y	B
y$B
y	B
y>B
y>B
yXB
yrB
y�B
y�B
y�B
y�B
zDB
z�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
Q�B
QhB
QB
Q B
QNB
Q�B
Q�B
R�B
R�B
T,B
Q�B
Q�B
V�B
_�B
kkB
z�B
� B
��B
�B
�B
��B
��B
��B
��B
�HB
�hB
�&B
�[B
��B
��B
��B
�uB
�FB
��B
�SB
��B
i�B
g�B
g�B
p;B
��B
�'B
��B
�1B
��B
��B
�B
��B
�B3BmB-�BAUBdB��B�CB�&B�wB�jB�	B�nB�B��BSB
rB�BqB#�B$�B$B'�B*�B)�B*B'mB"�B BBpB�B	�B�B��B�B�UB�
B��B�"B��B��B� B��B|�Bq�BabB;�B#�B�B�B
�JB
�5B
�PB
��B
�xB
�9B
�B
z�B
o�B
b�B
<B
KB	�B	��B	��B	v`B	_�B	;�B	# B	�B�AB�B�B��B�B	KB	�B��B�5B�nB	AB�cB�|B��B��B��B	�B	�B	AB	T�B	ZB	_�B	e�B	nIB	oiB	f�B	f�B	cTB	`�B	[�B	PHB	V�B	h�B	~(B	�B	�<B	��B	��B	�jB	�/B	�B	�B	�NB	�tB
EB
B
�B
B
�B
"B	��B	�
B	��B	��B	��B
UB

=B
}B
�B
}B
6B
�B
�B	�B	�GB	�2B	�B	�uB	�;B	�%B	�0B	̳B	�JB	��B	бB	�9B	��B	�eB	�B	��B	�>B	�B	�B	�B	�jB	ؓB	��B	ϑB	�"B	�\B	�}B	уB	��B	��B	ۦB	ۦB	��B	�=B	چB	��B	��B	��B	�SB	ðB	�6B	�9B	��B	��B	�|B	��B	��B	��B	��B	�mB	��B	��B	�B	��B	��B	�>B	��B	�B	��B	�'B	��B	�B	�B	��B	�KB	�=B	��B	�oB	�RB	�DB	�^B	�B	�}B	��B	�UB	��B	��B	�9B	��B	�B	��B	��B	�B	�jB	��B	�JB	��B	��B	�JB	͟B	� B	�NB	�hB	�HB	��B	�TB	�B	�@B	�FB	�gB	�KB	�7B	ڠB	�#B	��B	یB	چB	�B	ٴB	�$B	�MB	�2B	��B	�SB	ևB	��B	�[B	�[B	�[B	��B	��B	ՁB	�MB	ՁB	ՁB	ՁB	�B	�9B	�SB	�$B	�yB	רB	��B	��B	�7B	�=B	��B	ڠB	ڠB	یB	��B	ݲB	�VB	��B	��B	��B	��B	�BB	��B	�BB	�B	�'B	߾B	�VB	��B	�-B	��B	�B	�B	�B	�@B	�@B	�nB	�B	�~B	�OB	�\B	�B	�BB	�B	�B	�B	�2B	�zB	�B	�B	�B	�bB	�B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�tB	�B	�2B	��B	�B	�@B	�2B	�$B	��B	��B	�;B	�oB	��B	��B	�ZB	��B	�B	�?B	��B	��B	��B	�2B	��B	�B	�B	�RB	�8B	�$B	��B	�rB	�XB	��B	��B	�0B	��B	��B	��B	��B	��B	��B	�JB	�"B	�B	�cB	�}B	�cB	��B	��B	�}B	�cB	��B	�HB	��B	��B	��B	��B
  B
  B
 �B
 �B
 �B
 �B
 �B
 �B
 B
;B
 B
UB
�B
'B
�B
�B
�B
�B
B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
YB
YB
�B
+B
�B
�B
B
	B
�B
	B
	B
	�B
	lB
	�B

	B

=B
	�B

#B

�B

�B

�B
B
)B
�B
�B
B
�B
PB
jB
PB
PB
B
pB
�B
�B
vB
�B
B
HB
.B
.B
HB
HB
�B
 B
 B
hB
�B
�B
�B
 B
TB
�B
�B
�B
�B
�B
&B
�B
aB
{B
�B
B
�B
�B
�B
�B
9B
�B
�B

B

B
$B
�B
+B
�B
�B
�B
�B
�B
�B
1B
�B
�B
B
�B
KB
B
)B
�B
xB
�B
~B
�B
IB
�B
B
�B
�B
�B
�B
!B
�B
�B
!B
�B
�B
�B
 �B
!-B
!|B
!|B
!bB
!�B
"B
"4B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
%B
%B
%,B
%zB
%`B
%`B
%�B
&B
%�B
%�B
&�B
&LB
%�B
&B
%,B
$�B
&�B
(>B
(>B
(�B
(�B
(�B
'�B
'B
'�B
(�B
)yB
)yB
(�B
'mB
'�B
(
B
(XB
(sB
(�B
(�B
(�B
(�B
*�B
*�B
+�B
+QB
,=B
-)B
-wB
,�B
+�B
)�B
(XB
*KB
)�B
)�B
)�B
)�B
)�B
)�B
)�B
*B
+kB
+�B
+�B
+kB
*�B
)�B
)_B
)�B
)�B
)�B
*eB
*�B
*�B
+QB
+B
+6B
+kB
+kB
+�B
+�B
,B
+�B
,�B
-CB
-�B
-�B
-�B
.�B
/iB
/�B
0B
/�B
0�B
0�B
0�B
1'B
1vB
1vB
1vB
1�B
1�B
2�B
3MB
3hB
4B
4�B
4�B
4�B
4�B
5B
6`B
6�B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
9$B
:*B
9�B
:B
9�B
:^B
:�B
;B
;�B
<6B
<�B
<�B
="B
=qB
=�B
>B
>wB
>�B
?B
?HB
?�B
?�B
@ B
@OB
@�B
AUB
A�B
A�B
A�B
B[B
B�B
C-B
C{B
C�B
C�B
DgB
D�B
EB
EB
E�B
FYB
F�B
F�B
GzB
G�B
G�B
HB
H1B
H�B
H�B
H�B
IlB
I�B
JrB
J�B
J�B
J�B
K)B
K�B
KxB
K�B
K�B
LdB
LdB
L~B
L�B
L�B
L�B
L�B
L�B
MB
MjB
M�B
NB
NpB
NpB
N�B
O(B
OvB
O�B
P.B
PHB
PbB
P�B
PbB
P�B
Q4B
RoB
R�B
R�B
R�B
SB
SuB
S�B
S�B
TB
T�B
T�B
T�B
T�B
T�B
T�B
T�B
UB
U�B
VB
VB
VmB
V�B
V�B
V�B
W
B
W?B
WsB
W�B
XB
XEB
XyB
XyB
X�B
X�B
X�B
YB
YB
Y1B
YKB
YeB
Y�B
Y�B
ZB
ZQB
Z�B
Z�B
[#B
[=B
[�B
\�B
\�B
]B
\�B
]B
]dB
]�B
]�B
]�B
^OB
^�B
^�B
^�B
^�B
^�B
_B
_B
_B
_B
_�B
`B
`'B
`\B
`vB
`vB
`�B
`�B
abB
aHB
a�B
a�B
b�B
b�B
b�B
cB
cTB
cnB
c�B
c�B
c�B
c�B
c�B
c�B
dB
d&B
dB
d@B
d&B
dB
dtB
d�B
d�B
e`B
ezB
e�B
e�B
e�B
f2B
f�B
f�B
f�B
gRB
g�B
gRB
g�B
h
B
hXB
h�B
h�B
h�B
i*B
i*B
i*B
iDB
iDB
i_B
iyB
iyB
i�B
jB
jKB
jeB
jeB
j�B
k6B
k6B
k6B
kQB
kQB
k�B
k�B
k�B
k�B
lB
lqB
l�B
l�B
l�B
l�B
mB
mB
m)B
mwB
m�B
m�B
m�B
nIB
n�B
n�B
o5B
oOB
o5B
o�B
p!B
p�B
q'B
qvB
q�B
q�B
q�B
q�B
q�B
rB
rB
rGB
r|B
sB
s3B
shB
sMB
shB
shB
tB
t�B
t�B
uB
uZB
u�B
vB
v+B
v�B
v�B
wB
wB
wfB
w�B
w�B
w�B
w�B
xB
xB
xB
xB
xB
x8B
x�B
x�B
x�B
y	B
y	B
y	B
y$B
y	B
y>B
y>B
yXB
yrB
y�B
y�B
y�B
y�B
zDB
z�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105001  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175615  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175615  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175615                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025623  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025623  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141506                      G�O�G�O�G�O�                