CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-10-31T00:35:14Z creation;2017-10-31T00:35:18Z conversion to V3.1;2019-12-19T07:53:47Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20171031003514  20200116221516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_175                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�1�_#�1   @�1��l @4��5�Xy�d�V�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>�C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?y�D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�<�D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ Dؼ�D�  D�@ Dـ D��3D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�<�D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߃3D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��
@���A Q�A Q�A@Q�A`Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B@{BH{BP{BX{B`{Bh{Bp{Bx{B�
=B�
=B�
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
=C CCCCC
CCCCCCCCCCC C"C$C&C(C*C,C.C0C2C4C6C8C:C<�C>�C@CBCDCFCHCJCLCNCPCRCTCVCW�CZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?z�D@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�}qD���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�=qD̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�DؽqD� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�=qD܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߃�D���D� �D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�@�D怤D���D� �D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�C�D�z=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AׁA�x�A�p�A�p�A�n�A�p�A�p�A�l�A�l�A�l�A�n�A�n�A�l�A�jA�hsA�hsA�ffA�bNA�`BA�Q�A�O�A�K�A�I�A�E�A�?}A�;dA�7LA�K�AӅA҇+A���A���A��A�Aǰ!A��AƧ�A��mA��A�^5A�?}A�A�ȴA�
=A�^5A�{A�$�A���A��yA� �A�Q�A���A���A��A�E�A�A�z�A�K�A���A�bA��A���A��-A��9A���A��A�A�ĜA���A�O�A�|�A�{A���A�l�A��A�XA��`A��A�r�A�dZA��A�9XA�1A��HA�|�A��A��\A�=qA�S�A�v�A���A��yA��wA��yA�n�A�oA���A�VA��A��A��-A���A�oA�{A�/A�jA��/A�+A~�RA}��Az�Aw�Av�As7LAq|�ApZAo
=Al�`Aj$�Ahn�Af�HAf1Ad��Aa��A`��A_��A^��A]oA[G�AYG�AVĜAU��AT�AO��AK�TAJAI\)AHbNAG��AF5?AD �AB�DAA7LA@bA>JA<I�A;/A9ƨA8v�A7��A733A6�DA5G�A4�A4A3VA1�-A0^5A.��A-�A,�+A+�A+�A*��A*  A)�A)�PA'��A%��A%hsA%/A$��A#�7A r�A 1'AG�A�^A��An�A�A7LA�!A�AȴAZA�
A%A?}Az�A9XA�A(�A��AG�A"�A��A{AG�A��A�AoAv�A�^A
��A
(�A	�;A	K�A��Ax�A��A�
AjA��AVAƨA�PA&�A �uA J@���@���@�|�@���@��^@�x�@���@��@�ȴ@�5?@���@��@� �@��@�  @�@�(�@�`B@��@�r�@�+@�7L@߅@��y@�ff@�@���@ܛ�@��;@�E�@�S�@�I�@��H@҇+@��#@щ7@�r�@��@̛�@�|�@�M�@�%@ȓu@�1'@�E�@�?}@�r�@ÍP@�S�@�^5@�&�@��F@�ff@�?}@�Ĝ@��u@�bN@���@���@�{@�?}@��@��y@�ff@��@��^@��@��@�Z@�1'@���@�33@�@���@�M�@�J@��-@�&�@��/@��j@�A�@�ƨ@��@�+@��H@�ȴ@���@���@�v�@�^5@��#@��@��@��@�j@� �@�ƨ@��@�dZ@��@�^5@�$�@�J@�@��^@�`B@�&�@���@��@��j@�9X@���@��@�t�@�dZ@�l�@��y@�|�@��@���@��@���@�Z@�I�@��@�  @��P@��y@�ff@��T@���@��@�G�@��@���@��9@��9@�Ĝ@���@��@��@��-@���@��#@�@���@��@�x�@�O�@�&�@�Ĝ@�r�@�1'@��@���@��@�K�@�ȴ@�~�@��@���@���@�p�@�G�@��@���@���@��D@�I�@��
@��w@���@�|�@�C�@�o@��!@�@�@�`B@��@���@���@��@�j@�bN@�Q�@�9X@�  @��;@�1'@��
@�
=@��R@�ȴ@���@���@���@�ȴ@���@��@��!@�M�@�-@�@���@���@�x�@�`B@�X@��@�%@��/@���@���@���@��u@�z�@�j@��
@��F@���@�C�@�^5@��h@�`B@���@���@��@��h@�p�@�&�@���@���@�r�@�Q�@� �@�  @�ƨ@���@�K�@��y@���@�v�@�M�@���@���@�hs@�/@��@��9@��u@�bN@�Q�@� �@���@���@�@��\@��\@��\@�ff@�$�@��-@�G�@�/@��`@���@�Q�@��@�1@��
@�l�@�+@��@�o@��R@�-@���@���@��@���@��@�7L@��@���@��/@���@��@���@��@�K�@�S�@�;d@���@��\@��\@�~�@�{@�%@��/@���@���@�Ĝ@�Q�@�  @
=@~�y@~�R@~$�@~E�@}�@}@}`B@|��@|�@|I�@|1@{�
@{t�@{dZ@{"�@z�!@z�@yhs@xr�@xb@w�w@w�w@w�@w�P@w|�@wl�@w\)@w;d@w+@w�@v��@vv�@v{@u�@t�@t9X@t1@s��@s�m@s�m@s�
@s��@s�@sdZ@s33@r�H@r��@r^5@q�#@q&�@p��@p��@p�u@pbN@p1'@p �@o�@o|�@nv�@m�T@m`B@mV@l�@l1@k��@ko@j=q@j-@j=q@jn�@j-@jJ@i��@i&�@hr�@gl�@fv�@f5?@f@e��@e�h@c��@c��@ct�@cC�@c@b�\@a��@ahs@ax�@a�@`A�@_
=@^�+@^{@]��@]��@^@^{@]��@]��@]�@]`B@]O�@]/@\�/@\��@\�D@\Z@\(�@\�@[�m@[C�@[@Z�@Z��@Y&�@Xr�@W�P@W+@Vȴ@Vff@U�@U/@Tj@T(�@T1@S��@SdZ@SC�@R��@Q��@Qhs@QG�@QG�@Q7L@Q�@Q%@P�`@P�9@P �@O��@O+@O
=@N�@N��@N�+@N@M��@M�@M�@M�@M`B@L�j@Lz�@Lj@L9X@L(�@L(�@L(�@K�m@KdZ@K"�@J�!@J^5@JJ@I�#@I�^@I��@I��@I�7@Ix�@Ihs@H��@H1'@G��@G|�@Gl�@G;d@G�@F�y@F�@Fȴ@F��@F��@Fv�@Fff@FE�@F@E�T@E@E�@E?}@D�@Dz�@D(�@C�@B�@B�\@BJ@Ax�@AX@A�@@�`@@�9@@Q�@@A�@@ �@@b@?�;@?l�@?
=@>ȴ@>�+@>v�@>v�@>v�@>ff@>V@>{@=�-@=��@=p�@<��@<�/@<�j@<�j@<��@<��@<��@<�j@<z�@<Z@<I�@;��@;�
@;�@;dZ@;33@:�H@:J@9��@9X@9&�@8��@8r�@8bN@8Q�@8A�@81'@81'@8  @7K�@7�@7
=@6��@6��@6�y@6�R@6�+@6$�@5�T@5�h@5p�@5`B@4��@4z�@49X@4�@3��@3�
@3��@3dZ@3C�@3o@2��@2~�@2^5@2M�@2=q@1�#@1X@0��@0�@0r�@0 �@/��@/��@/��@/��@/�w@/�w@/�@/��@/|�@/;d@/�@.��@.�y@.E�@-�T@-p�@-O�@-O�@-�@,�/@,�D@,I�@,(�@,(�@,�@+�m@+��@+��@+S�@+o@*��@*^5@*�@)�@)��@)x�@)hs@)7L@(��@(�@(1'@(b@(  @'��@'l�@'\)@'K�@'
=@&�@&�+@&ff@&E�@&$�@%�T@%@%�@%/@$��@$��@$�/@$�D@$z�@$Z@$I�@$9X@#�m@#��@#C�@#o@"�@"�H@"�!@"~�@"n�@"=q@"�@!�@!�^@!�7@!hs@!&�@ �@ b@�@��@�@��@K�@�@
=@�@��@5?@��@@�-@��@�@�@�@p�@�@��@z�@(�@ƨ@dZ@@��@�!@��@�\@��@��@~�@M�@-@�@�^@�7@X@7L@7L@Ĝ@r�@A�@�;@�P@+@��@ȴ@v�@V@E�@5?@5?@@�T@��@@��@�@p�@`B@/@��@�@�/@�j@j@(�@�
@��@33@@�H@��@��@��@M�@�@�#@�^@��@��@��@��@x�@hs@X@G�@�@��@Ĝ@A�@b@�@�@��@�P@\)@;d@�@�y@ȴ@�R@�R@�R@ȴ@ȴ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AׁA�x�A�p�A�p�A�n�A�p�A�p�A�l�A�l�A�l�A�n�A�n�A�l�A�jA�hsA�hsA�ffA�bNA�`BA�Q�A�O�A�K�A�I�A�E�A�?}A�;dA�7LA�K�AӅA҇+A���A���A��A�Aǰ!A��AƧ�A��mA��A�^5A�?}A�A�ȴA�
=A�^5A�{A�$�A���A��yA� �A�Q�A���A���A��A�E�A�A�z�A�K�A���A�bA��A���A��-A��9A���A��A�A�ĜA���A�O�A�|�A�{A���A�l�A��A�XA��`A��A�r�A�dZA��A�9XA�1A��HA�|�A��A��\A�=qA�S�A�v�A���A��yA��wA��yA�n�A�oA���A�VA��A��A��-A���A�oA�{A�/A�jA��/A�+A~�RA}��Az�Aw�Av�As7LAq|�ApZAo
=Al�`Aj$�Ahn�Af�HAf1Ad��Aa��A`��A_��A^��A]oA[G�AYG�AVĜAU��AT�AO��AK�TAJAI\)AHbNAG��AF5?AD �AB�DAA7LA@bA>JA<I�A;/A9ƨA8v�A7��A733A6�DA5G�A4�A4A3VA1�-A0^5A.��A-�A,�+A+�A+�A*��A*  A)�A)�PA'��A%��A%hsA%/A$��A#�7A r�A 1'AG�A�^A��An�A�A7LA�!A�AȴAZA�
A%A?}Az�A9XA�A(�A��AG�A"�A��A{AG�A��A�AoAv�A�^A
��A
(�A	�;A	K�A��Ax�A��A�
AjA��AVAƨA�PA&�A �uA J@���@���@�|�@���@��^@�x�@���@��@�ȴ@�5?@���@��@� �@��@�  @�@�(�@�`B@��@�r�@�+@�7L@߅@��y@�ff@�@���@ܛ�@��;@�E�@�S�@�I�@��H@҇+@��#@щ7@�r�@��@̛�@�|�@�M�@�%@ȓu@�1'@�E�@�?}@�r�@ÍP@�S�@�^5@�&�@��F@�ff@�?}@�Ĝ@��u@�bN@���@���@�{@�?}@��@��y@�ff@��@��^@��@��@�Z@�1'@���@�33@�@���@�M�@�J@��-@�&�@��/@��j@�A�@�ƨ@��@�+@��H@�ȴ@���@���@�v�@�^5@��#@��@��@��@�j@� �@�ƨ@��@�dZ@��@�^5@�$�@�J@�@��^@�`B@�&�@���@��@��j@�9X@���@��@�t�@�dZ@�l�@��y@�|�@��@���@��@���@�Z@�I�@��@�  @��P@��y@�ff@��T@���@��@�G�@��@���@��9@��9@�Ĝ@���@��@��@��-@���@��#@�@���@��@�x�@�O�@�&�@�Ĝ@�r�@�1'@��@���@��@�K�@�ȴ@�~�@��@���@���@�p�@�G�@��@���@���@��D@�I�@��
@��w@���@�|�@�C�@�o@��!@�@�@�`B@��@���@���@��@�j@�bN@�Q�@�9X@�  @��;@�1'@��
@�
=@��R@�ȴ@���@���@���@�ȴ@���@��@��!@�M�@�-@�@���@���@�x�@�`B@�X@��@�%@��/@���@���@���@��u@�z�@�j@��
@��F@���@�C�@�^5@��h@�`B@���@���@��@��h@�p�@�&�@���@���@�r�@�Q�@� �@�  @�ƨ@���@�K�@��y@���@�v�@�M�@���@���@�hs@�/@��@��9@��u@�bN@�Q�@� �@���@���@�@��\@��\@��\@�ff@�$�@��-@�G�@�/@��`@���@�Q�@��@�1@��
@�l�@�+@��@�o@��R@�-@���@���@��@���@��@�7L@��@���@��/@���@��@���@��@�K�@�S�@�;d@���@��\@��\@�~�@�{@�%@��/@���@���@�Ĝ@�Q�@�  @
=@~�y@~�R@~$�@~E�@}�@}@}`B@|��@|�@|I�@|1@{�
@{t�@{dZ@{"�@z�!@z�@yhs@xr�@xb@w�w@w�w@w�@w�P@w|�@wl�@w\)@w;d@w+@w�@v��@vv�@v{@u�@t�@t9X@t1@s��@s�m@s�m@s�
@s��@s�@sdZ@s33@r�H@r��@r^5@q�#@q&�@p��@p��@p�u@pbN@p1'@p �@o�@o|�@nv�@m�T@m`B@mV@l�@l1@k��@ko@j=q@j-@j=q@jn�@j-@jJ@i��@i&�@hr�@gl�@fv�@f5?@f@e��@e�h@c��@c��@ct�@cC�@c@b�\@a��@ahs@ax�@a�@`A�@_
=@^�+@^{@]��@]��@^@^{@]��@]��@]�@]`B@]O�@]/@\�/@\��@\�D@\Z@\(�@\�@[�m@[C�@[@Z�@Z��@Y&�@Xr�@W�P@W+@Vȴ@Vff@U�@U/@Tj@T(�@T1@S��@SdZ@SC�@R��@Q��@Qhs@QG�@QG�@Q7L@Q�@Q%@P�`@P�9@P �@O��@O+@O
=@N�@N��@N�+@N@M��@M�@M�@M�@M`B@L�j@Lz�@Lj@L9X@L(�@L(�@L(�@K�m@KdZ@K"�@J�!@J^5@JJ@I�#@I�^@I��@I��@I�7@Ix�@Ihs@H��@H1'@G��@G|�@Gl�@G;d@G�@F�y@F�@Fȴ@F��@F��@Fv�@Fff@FE�@F@E�T@E@E�@E?}@D�@Dz�@D(�@C�@B�@B�\@BJ@Ax�@AX@A�@@�`@@�9@@Q�@@A�@@ �@@b@?�;@?l�@?
=@>ȴ@>�+@>v�@>v�@>v�@>ff@>V@>{@=�-@=��@=p�@<��@<�/@<�j@<�j@<��@<��@<��@<�j@<z�@<Z@<I�@;��@;�
@;�@;dZ@;33@:�H@:J@9��@9X@9&�@8��@8r�@8bN@8Q�@8A�@81'@81'@8  @7K�@7�@7
=@6��@6��@6�y@6�R@6�+@6$�@5�T@5�h@5p�@5`B@4��@4z�@49X@4�@3��@3�
@3��@3dZ@3C�@3o@2��@2~�@2^5@2M�@2=q@1�#@1X@0��@0�@0r�@0 �@/��@/��@/��@/��@/�w@/�w@/�@/��@/|�@/;d@/�@.��@.�y@.E�@-�T@-p�@-O�@-O�@-�@,�/@,�D@,I�@,(�@,(�@,�@+�m@+��@+��@+S�@+o@*��@*^5@*�@)�@)��@)x�@)hs@)7L@(��@(�@(1'@(b@(  @'��@'l�@'\)@'K�@'
=@&�@&�+@&ff@&E�@&$�@%�T@%@%�@%/@$��@$��@$�/@$�D@$z�@$Z@$I�@$9X@#�m@#��@#C�@#o@"�@"�H@"�!@"~�@"n�@"=q@"�@!�@!�^@!�7@!hs@!&�@ �@ b@�@��@�@��@K�@�@
=@�@��@5?@��@@�-@��@�@�@�@p�@�@��@z�@(�@ƨ@dZ@@��@�!@��@�\@��@��@~�@M�@-@�@�^@�7@X@7L@7L@Ĝ@r�@A�@�;@�P@+@��@ȴ@v�@V@E�@5?@5?@@�T@��@@��@�@p�@`B@/@��@�@�/@�j@j@(�@�
@��@33@@�H@��@��@��@M�@�@�#@�^@��@��@��@��@x�@hs@X@G�@�@��@Ĝ@A�@b@�@�@��@�P@\)@;d@�@�y@ȴ@�R@�R@�R@ȴ@ȴ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�NB�BuB$�B�B�B9XBI�BW
BcTB`BB]/BcTBQ�BZB]/BbNBdZBdZBXBS�B�B�1Bw�Br�B�%B�Bw�B�Bu�BiyBo�BiyBt�Bs�Br�BaHBJ�BS�BW
BQ�B@�B�BoBPB�B�BhB%BBB��B�NB�wB�9B��BǮB�}B�9B�B��B�PBhsBF�BO�BE�B&�B.B(�BhB
�B
�^B
�B
�-B
�'B
��B
�VB
}�B
^5B
O�B
L�B
@�B
6FB
�B
B	��B	�ZB	�/B	�B	��B	�jB	��B	��B	��B	��B	�VB	v�B	�+B	�B	z�B	k�B	\)B	N�B	>wB	=qB	,B	+B��B	B	JB	B��B�B�5B�)B�B��BŢBÖB��B�qB�qB��B�}B�dB�-B�-B�-B��B��B��B��B��B��B��B��B��B��B��B��B�VB�B�hB�oB�PB�Bo�B�B~�Bw�By�Bz�Bw�Br�Br�BjBq�Bp�Bl�BgmB[#BbNBdZB_;B]/BdZBe`Be`B`BBZBXBS�B\)BZBXBS�BT�BR�BW
BR�BN�BI�BK�BL�BI�BL�BL�BQ�BW
BS�BP�BN�BJ�BO�BO�BQ�BR�BVBS�BP�BQ�BT�BT�BR�BO�BJ�BG�BN�BI�BE�BN�BO�BXBS�BYBaHBbNBbNBaHBcTB_;BZBXB^5Bn�Bw�Bu�Bu�Br�Bk�Bw�B|�B� B�B�7B�7B�B�DB�bB�bB��B�oB�oB��B��B��B�B�B�B�B�B�3B�9B�FB�qBƨBɺB��B��B��B��B��B��B�B�#B�)B�;B�HB�NB�`B�B�B�B�B��B��B��B	  B	B	B	B	B	B	%B	
=B	
=B	PB	\B	bB	�B	�B	�B	�B	"�B	%�B	%�B	$�B	%�B	)�B	-B	/B	.B	/B	2-B	49B	8RB	9XB	9XB	<jB	H�B	J�B	H�B	I�B	M�B	P�B	R�B	R�B	T�B	T�B	T�B	VB	ZB	`BB	cTB	dZB	ffB	hsB	l�B	n�B	p�B	x�B	�B	�+B	�7B	�=B	�DB	�JB	�PB	�VB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�3B	�9B	�9B	�9B	�9B	�?B	�XB	�XB	�^B	�^B	�^B	�XB	�RB	�wB	�wB	�wB	�}B	�}B	�}B	��B	B	B	B	B	ŢB	ǮB	ƨB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�B	�#B	�)B	�)B	�5B	�;B	�5B	�/B	�/B	�#B	�/B	�/B	�B	�B	�B	�B	�)B	�BB	�NB	�fB	�`B	�`B	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
%B
B
B
+B
1B
1B
%B
%B
1B

=B
	7B
1B
1B
	7B

=B

=B
	7B
	7B
+B
+B
1B
	7B

=B
DB

=B
JB
VB
PB

=B
1B
PB
\B
\B
\B
VB
bB
bB
oB
oB
hB
uB
uB
uB
oB
hB
oB
oB
oB
uB
uB
uB
uB
oB
hB
hB
hB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
!�B
"�B
"�B
"�B
!�B
!�B
!�B
 �B
�B
 �B
 �B
#�B
#�B
"�B
"�B
!�B
 �B
"�B
"�B
$�B
%�B
%�B
$�B
#�B
!�B
 �B
�B
"�B
#�B
#�B
"�B
�B
#�B
$�B
$�B
$�B
$�B
$�B
&�B
'�B
&�B
%�B
%�B
'�B
(�B
)�B
+B
-B
/B
/B
/B
/B
/B
/B
/B
.B
0!B
/B
/B
/B
0!B
/B
.B
0!B
0!B
/B
,B
.B
/B
0!B
0!B
0!B
0!B
/B
0!B
2-B
33B
2-B
33B
33B
2-B
1'B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
5?B
5?B
5?B
6FB
8RB
8RB
8RB
8RB
8RB
9XB
;dB
;dB
;dB
:^B
9XB
;dB
<jB
=qB
=qB
=qB
=qB
<jB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
>wB
>wB
?}B
A�B
B�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
A�B
@�B
A�B
A�B
@�B
A�B
A�B
B�B
B�B
D�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
D�B
C�B
D�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
F�B
F�B
H�B
H�B
H�B
J�B
K�B
K�B
K�B
K�B
K�B
J�B
J�B
J�B
K�B
J�B
J�B
J�B
K�B
K�B
J�B
I�B
K�B
K�B
L�B
L�B
M�B
N�B
N�B
N�B
N�B
N�B
M�B
L�B
O�B
P�B
P�B
P�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
P�B
P�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
S�B
S�B
S�B
VB
W
B
VB
W
B
XB
XB
XB
XB
XB
XB
W
B
W
B
W
B
W
B
W
B
W
B
VB
W
B
W
B
XB
YB
XB
XB
XB
YB
ZB
ZB
ZB
YB
YB
ZB
ZB
YB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
[#B
[#B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
^5B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
`BB
`BB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
bNB
cTB
cTB
bNB
cTB
cTB
cTB
bNB
cTB
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
e`B
ffB
ffB
hsB
hsB
hsB
hsB
hsB
hsB
gmB
ffB
gmB
gmB
hsB
gmB
hsB
iyB
jB
jB
k�B
k�B
k�B
k�B
k�B
jB
k�B
jB
k�B
k�B
k�B
l�B
l�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
o�B
p�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
t�B
u�B
u�B
u�B
v�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�,B��BgB&�B�B;B@OBLJBXyBd&Ba�B_Bd�BTaB[�B^�Bc�Be�BezBZkBWYB��B�XB{�Bv+B�B��Bz�B��Bx�Bl=Bq�BlWBu�Bt�Bs�BdBN�BVBX+BS@BC�B�B9BBmB+BTB�BB�B�$B�`BðB��B�JB�1B��B��B�B�@B�BmCBLdBQ�BHfB*�B/iB*�BMB
�'B
��B
��B
�B
��B
�`B
��B
��B
bNB
S[B
N�B
B�B
8B
CB
�B	�VB	�B	�;B	ڠB	��B	�.B	��B	��B	��B	��B	�bB	zB	�KB	�aB	|6B	m�B	^�B	Q�B	A�B	?cB	.�B	�B�"B	B	PB	�B�HB�B��B�5B��BԯB�KBżB�-B�HB�B��B�OB�jB��B�B�MB�yB��B��B�|B�BB�]B��B�bB�bB��B�)B�QB�}B�SB�B��B�VB�YBsB��B�iBy�Bz�B{Bx�Bs�Bs�BlWBr-Bq[Bm�Bh�B]dBc:BeB`�B^jBeBe�Be�B`�B[=BYKBU�B\�BZ�BX�BUBVBS�BW�BS�BO�BKDBMBN"BK�BN"BNVBR�BWsBT�BQ�BO�BL0BP�BP�BR�BS�BVSBT{BQ�BR�BUgBUgBS�BP�BLBI�BO�BK�BG�BP�BQhBYBU2BZ7Ba�Bb�Bb�Ba�Bc�B`B[�BZB`'BoiBxBvFBvFBs�BmCBx�B}�B��B��B��B��B�tB��B� B� B��B�@B�uB�sB��B��B�QB�IB�cB��B��B��B��B�2B�(B��B�#B�B�dB�4B�2B�2B�aB�eB�WB�xBߊB�B�B��B�B��B�B�B�B�B�B	 B	AB	3B	MB	MB	{B	tB	
�B	
�B	�B	�B	�B	�B	�B	
B	B	#B	%�B	%�B	%B	&2B	*0B	-CB	/5B	.cB	/�B	2|B	4nB	8lB	9rB	9rB	<�B	H�B	KDB	I�B	J#B	N"B	Q B	SB	S&B	U2B	UgB	U�B	VmB	Z�B	`\B	cnB	d�B	f�B	h�B	l�B	n�B	p�B	x�B	��B	��B	�B	�=B	�)B	�dB	�jB	�pB	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�:B	�B	�LB	�CB	�5B	�[B	�MB	�hB	�9B	�TB	�nB	�nB	��B	�rB	�rB	�xB	��B	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ªB	ªB	ªB	��B	żB	ǔB	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�&B	�:B	�B	�B	�9B	�EB	�1B	�7B	�#B	�QB	�=B	�CB	�]B	�5B	�;B	�OB	�IB	�IB	یB	�/B	�IB	چB	֡B	ևB	�1B	�]B	�BB	�4B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�DB	�6B	��B
B
;B
AB
aB
aB
B
MB
3B
MB
MB
%B
SB
mB
_B
1B
KB
tB
tB
KB

=B
	RB
KB
fB
	lB

XB

XB
	lB
	lB
�B
_B
fB
	RB

=B
^B

�B
dB
pB
�B

�B
�B
jB
\B
vB
vB
�B
�B
�B
�B
oB
�B
uB
�B
�B
�B
�B
�B
�B
oB
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
sB
yB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
 B
!�B
"�B
"�B
"�B
!�B
!�B
!�B
 �B
!B
 �B
 �B
$B
$B
#B
"�B
"B
 �B
"�B
"�B
$�B
%�B
%�B
%B
$B
"B
!-B
!B
"�B
#�B
$B
# G�O�B
#�B
$�B
$�B
$�B
%B
%,B
'B
'�B
'B
&LB
&LB
($B
)*B
*B
+B
,�B
/B
/5B
/5B
/5B
/5B
/5B
/B
./B
0;B
/5B
/B
/B
0;B
/5B
.IB
0;B
0;B
/iG�O�B
.cB
/iB
0;B
0UB
0UB
0UB
/iB
0UB
2GB
3MB
2aB
3MB
3MB
2|B
1�B
5ZB
6FB
6FB
6FB
6`B
6`B
6`B
5ZB
5tB
5tB
6zB
8lB
8lB
8lB
8lB
8�B
9rB
;dB
;JB
;dB
:xB
9�B
;dB
<�B
=�B
=qB
=VB
=qB
<�B
;�B
<�B
<�B
<�B
<�B
=�B
=�B
>]B
>wB
?}B
?}B
?�B
>�B
>�B
?�B
A�B
BuB
A�B
B�B
B�B
B�B
BuB
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
A�B
@�B
A�B
A�B
@�B
A�B
A�B
B�B
B�B
D�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
D�B
C�B
D�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
F�B
F�B
H�B
H�B
H�B
J�B
K�B
K�B
K�B
K�B
K�B
J�B
J�B
J�B
K�B
J�B
J�B
J�B
K�B
K�B
J�B
J	B
K�B
K�B
L�B
MB
M�B
N�B
N�B
N�B
N�B
N�B
M�B
MB
O�B
P�B
P�B
P�B
O�B
O�B
O�B
PB
Q B
Q B
Q�B
RB
Q B
QB
SB
SB
SB
R�B
SB
R�B
TB
S�B
S�B
TB
UB
UB
T�B
T,B
TB
T,B
VB
W
B
VB
W$B
XB
XB
XB
W�B
W�B
XB
W$B
W$B
W
B
W$B
W
B
W$B
V9B
W?B
W?B
X+B
YB
X+B
X+B
X+B
Y1B
ZB
ZB
ZB
YB
YB
Z7B
Z7B
Y1B
Z7B
Z7B
[=B
[#B
[=B
\CB
\CB
\CB
[=B
[WB
\CB
]IB
]IB
]IB
]IB
^5B
^5B
]IB
]IB
^OB
^5B
^OB
^OB
^OB
_VB
^OB
_VB
`\B
`BB
`\B
`BB
aHB
abB
aHB
abB
`\B
`\B
abB
aHB
bNB
bhB
bhB
bhB
cTB
bhB
cnB
cnB
bNB
cnB
cnB
cnB
b�B
c�B
e`B
e`B
ezB
ezB
ezB
ezB
ffB
f�B
ezB
f�B
f�B
hsB
hsB
hsB
hsB
hsB
hsB
g�B
f�B
g�B
g�B
h�B
g�B
h�B
i�B
j�B
j�B
k�B
k�B
k�B
kkB
k�B
j�B
k�B
j�B
k�B
k�B
k�B
l�B
l�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
o�B
p�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
t�B
u�B
u�B
u�B
v�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.02(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201711040037392017110400373920171104003739201806221321192018062213211920180622132119201804050724082018040507240820180405072408  JA  ARFMdecpA19c                                                                20171031093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171031003514  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171031003516  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171031003516  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171031003517  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171031003517  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171031003517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171031003517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171031003517  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171031003518                      G�O�G�O�G�O�                JA  ARUP                                                                        20171031005600                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171101154420  CV  JULD            G�O�G�O�F���                JM  ARSQJMQC2.0                                                                 20171102000000  CF  PSAL_ADJUSTED_QCD�� D�� G�O�                JM  ARCAJMQC2.0                                                                 20171103153739  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171103153739  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222408  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042119  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221516                      G�O�G�O�G�O�                