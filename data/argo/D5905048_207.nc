CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-02-04T00:35:42Z creation;2018-02-04T00:35:47Z conversion to V3.1;2019-12-19T07:46:02Z update;     
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
_FillValue                 �  I,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �T   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ݔ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �$   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �d   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �t   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �x   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180204003542  20200116221517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_207                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�I��2� 1   @�I�}'Ҁ@3�v_ح��d`Z�11   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.y�D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DC��DDy�DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DNy�DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� DgfDg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ Dȃ3D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dσ3D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�C3DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�3D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�fD�#3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@���A Q�A Q�A@Q�A`Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B{B{Bz�B {B({B0{B8{B@{BH{BP{BX{B`{Bh{Bp{Bx{B�
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
=B�
=B�=qB�
=B�
=B�
=B�
=C CCCCC
CCCCCCCCCCC C"C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.z�D/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDC��DDz�DEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDNz�DOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDg�Dg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D��qD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D��D�C�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D��qD�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D��qD�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�Dȃ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dσ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�C�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D��D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D��qD�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�@�D怤D�qD� �D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D�
D�#�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��#A��A��A���A�JA��A��A�1A��A��A���A���A���Aɛ�A�C�A�%A�AȸRAȏ\A�M�A��Aǣ�A�v�A��TAƇ+A�ZA�(�A�VA���A�z�A�JAľwA��A��`A��HA��HA��mA���A���Aú^AöFAìAÕ�AÁA�`BA�
=AA��`A�;dA��
A���A�hsA�^5A�VA�M�A�;dA��A��A�{A��PA��FA���A��+A��hA� �A��HA�ZA��A��RA��yA�dZA���A���A�~�A�ZA�ȴA�Q�A���A��9A�VA��A���A�XA�C�A�;dA�XA��;A�;dA��A���A�bNA�ĜA�dZA���A��\A�hsA�;dA�Q�A�M�A��A���A�r�A�A��A�  A���A�1A���A�S�A�JA��+A��A���A�bA���A��A�5?A�l�A�K�A�VA���A�(�A}�7A{�
Aw�Ap$�AnĜAmp�AkAh��Ad�RA`��A`�+A_�
A_l�A\�AYK�AW7LAT�DAQ�-APbNAO
=AN(�AM
=AK�AK33AJ^5AIVAE��AE�AD�HAD�ADbAC7LAB�!AAA>�yA=;dA;��A9�
A8��A7�FA6-A3?}A1��A/�
A/VA.^5A-%A*�A)|�A'��A%��A$�HA#O�A!�A �/A��AK�A��AVAv�AVA�A�Av�AVAQ�A��Ax�AoA{A�A%AG�A9XA�hAG�A
Q�A
bA	��AĜA{Ap�A�A�A�A�TA�A�AVA��A�A%A �j@�ƨ@��y@�hs@�V@�I�@���@�Z@�~�@�5?@��@�x�@���@�9X@�\)@�7@��@�
=@��T@�u@�;d@�r�@�ȴ@��`@���@⟾@ߍP@ް!@�$�@ݲ-@��@ٺ^@ج@֧�@�=q@�5?@թ�@Դ9@�(�@�S�@��@ϥ�@��@�O�@̬@� �@˾w@��H@�&�@ǝ�@�~�@ŉ7@�V@�A�@¸R@���@�7L@���@��@�S�@�v�@�%@��F@�S�@�o@��+@�@�`B@�z�@���@�ƨ@�@�{@��T@��#@�%@�ƨ@�"�@���@�n�@�=q@��@�hs@��
@�@��H@��h@�Q�@�b@��u@��
@�@��@�
=@���@�t�@�;d@�"�@�
=@��H@���@���@��j@���@��D@�z�@��@�j@�(�@��m@��w@���@�S�@�@���@�E�@��^@�p�@�V@�r�@�Q�@�b@��m@���@���@��w@���@�1@���@���@��@��w@��@��P@�t�@�+@�+@��@�ȴ@�E�@��@��+@��H@��@���@��+@�=q@�-@�x�@�/@�V@�Ĝ@��D@��w@��@�C�@��@���@�"�@��@��@���@�~�@�J@��-@�`B@��@�%@�Ĝ@��D@�j@�Z@��m@�|�@�
=@�@�@�@���@���@�E�@�^5@�-@�{@�{@��@��@�hs@�G�@�Ĝ@��`@�z�@�A�@�(�@�1@��;@���@�;d@���@�~�@�V@�-@�@��@�X@�/@��/@��@��D@�r�@�Z@�1@�|�@�+@��@��y@��y@��@���@���@��+@��\@�ff@�{@���@��@�&�@�&�@�&�@��@���@���@��D@� �@���@��F@�dZ@��@�
=@�
=@��H@���@�5?@�-@�E�@�M�@��@�O�@���@��9@�A�@��m@��@�o@��R@��\@�^5@�J@�@��h@�`B@�V@��/@���@�Q�@�1'@��;@��
@��w@��P@�l�@�K�@�C�@�"�@���@�n�@�=q@��@�{@��@���@��7@�hs@�hs@�x�@�hs@�%@��j@�z�@��@��@��
@�ƨ@��w@���@�o@�v�@�-@�@���@���@��7@�`B@��@��j@��u@�j@�Z@�A�@�(�@�b@�@�w@|�@�@}�T@}�@}p�@|��@|9X@{��@z��@z=q@z-@y�@x�`@xbN@w
=@u�-@t�j@t�@s��@sdZ@r��@q�@qx�@qX@qG�@q&�@q%@q&�@p�u@pQ�@p1'@pb@o�@n��@n�+@nff@n@m�@lj@l�@k��@kƨ@k33@ko@j�!@jM�@j�@ix�@h�`@hr�@h �@hb@g�@g|�@gK�@f�y@f5?@e�@e�T@e�h@e?}@d�D@d�@c��@cS�@co@b~�@a��@a�7@aG�@a&�@`��@`��@`r�@`b@_�w@_l�@_
=@^�+@^$�@^{@^@]�@]��@]�-@]O�@\��@\Z@\�@[�m@[�
@[ƨ@[�F@[��@[t�@[C�@Z��@Y��@YX@Y�@X��@X��@XbN@X �@W�@W�P@W|�@W;d@V�y@V�R@V�+@VV@VV@U�@T��@Tz�@TI�@S��@St�@SC�@SC�@S33@S@RM�@Q�#@Qx�@QX@Q&�@P��@O�;@O�@N�@N�R@Nv�@N5?@M��@M/@L��@L�D@LI�@L1@Kƨ@KS�@K33@J��@J=q@I�@I�#@I��@Ix�@I�@H��@HbN@H �@H  @G�;@G�P@G;d@G
=@F�R@FE�@F$�@E�T@E�-@E?}@D�j@D�D@DZ@DZ@DZ@D(�@C��@Co@B�!@B��@B~�@BM�@B=q@B-@BJ@A�@A��@Ax�@A7L@@�`@@�@@ �@?��@?l�@?+@>�@>ff@=@=��@=��@=�h@=�@=p�@=/@<��@<��@<��@<9X@<�@;�F@;�@;S�@;"�@:�H@:^5@9��@9hs@9hs@9hs@9&�@9�@9%@8�`@8��@8r�@8Q�@8A�@8b@7��@7�@7\)@7
=@6ȴ@6��@6�+@6V@6{@5�T@5��@5�-@5p�@5�@4�j@4I�@3�
@3ƨ@3��@3t�@3dZ@333@3@2��@2~�@2^5@2=q@2J@1�^@1x�@0�`@0��@0�@0�@0�@0Q�@/��@/l�@.��@.v�@.@-��@-�-@-��@-�@-O�@,��@,��@,Z@+��@+�F@+��@+dZ@+C�@+"�@*�@*��@*�\@*�\@*~�@*J@)��@)7L@(��@(�9@(�@(bN@(A�@(1'@(b@'��@'�@'|�@'l�@'\)@';d@'
=@&V@%�@%�-@%��@%p�@%O�@$��@$��@$�j@$�@$�D@$(�@#�F@#��@#dZ@#C�@#o@"��@"n�@"-@!�@!�^@!x�@!%@ �u@ 1'@ b@�w@\)@��@�@ȴ@ȴ@�R@ff@E�@5?@{@�T@��@�@�@V@��@�D@j@I�@�
@��@t�@S�@C�@"�@�H@�!@�\@n�@^5@M�@M�@=q@-@J@�@�^@�7@G�@&�@�@�@%@�`@Ĝ@Q�@ �@�@�w@��@|�@K�@
=@��@��@��@��@��@��@��@�y@�y@��@@��@��@p�@?}@V@��@�j@�D@��@�
@ƨ@�@dZ@S�@C�@o@@@��@^5@=q@�@��@X@�`@Ĝ@�9@�9@��@�u@�u@�u@�@bN@Q�@b@�w@��@�P@\)@+@�@�@�@��@�y@�R@ff@E�@$�@$�@@�@@�-@�-@�-@�h@/@�j@��@�D@z�@Z@9X@�
@�F@�@S�@o@
��@
�!@
�\@
M�@
J@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��#A��A��A���A�JA��A��A�1A��A��A���A���A���Aɛ�A�C�A�%A�AȸRAȏ\A�M�A��Aǣ�A�v�A��TAƇ+A�ZA�(�A�VA���A�z�A�JAľwA��A��`A��HA��HA��mA���A���Aú^AöFAìAÕ�AÁA�`BA�
=AA��`A�;dA��
A���A�hsA�^5A�VA�M�A�;dA��A��A�{A��PA��FA���A��+A��hA� �A��HA�ZA��A��RA��yA�dZA���A���A�~�A�ZA�ȴA�Q�A���A��9A�VA��A���A�XA�C�A�;dA�XA��;A�;dA��A���A�bNA�ĜA�dZA���A��\A�hsA�;dA�Q�A�M�A��A���A�r�A�A��A�  A���A�1A���A�S�A�JA��+A��A���A�bA���A��A�5?A�l�A�K�A�VA���A�(�A}�7A{�
Aw�Ap$�AnĜAmp�AkAh��Ad�RA`��A`�+A_�
A_l�A\�AYK�AW7LAT�DAQ�-APbNAO
=AN(�AM
=AK�AK33AJ^5AIVAE��AE�AD�HAD�ADbAC7LAB�!AAA>�yA=;dA;��A9�
A8��A7�FA6-A3?}A1��A/�
A/VA.^5A-%A*�A)|�A'��A%��A$�HA#O�A!�A �/A��AK�A��AVAv�AVA�A�Av�AVAQ�A��Ax�AoA{A�A%AG�A9XA�hAG�A
Q�A
bA	��AĜA{Ap�A�A�A�A�TA�A�AVA��A�A%A �j@�ƨ@��y@�hs@�V@�I�@���@�Z@�~�@�5?@��@�x�@���@�9X@�\)@�7@��@�
=@��T@�u@�;d@�r�@�ȴ@��`@���@⟾@ߍP@ް!@�$�@ݲ-@��@ٺ^@ج@֧�@�=q@�5?@թ�@Դ9@�(�@�S�@��@ϥ�@��@�O�@̬@� �@˾w@��H@�&�@ǝ�@�~�@ŉ7@�V@�A�@¸R@���@�7L@���@��@�S�@�v�@�%@��F@�S�@�o@��+@�@�`B@�z�@���@�ƨ@�@�{@��T@��#@�%@�ƨ@�"�@���@�n�@�=q@��@�hs@��
@�@��H@��h@�Q�@�b@��u@��
@�@��@�
=@���@�t�@�;d@�"�@�
=@��H@���@���@��j@���@��D@�z�@��@�j@�(�@��m@��w@���@�S�@�@���@�E�@��^@�p�@�V@�r�@�Q�@�b@��m@���@���@��w@���@�1@���@���@��@��w@��@��P@�t�@�+@�+@��@�ȴ@�E�@��@��+@��H@��@���@��+@�=q@�-@�x�@�/@�V@�Ĝ@��D@��w@��@�C�@��@���@�"�@��@��@���@�~�@�J@��-@�`B@��@�%@�Ĝ@��D@�j@�Z@��m@�|�@�
=@�@�@�@���@���@�E�@�^5@�-@�{@�{@��@��@�hs@�G�@�Ĝ@��`@�z�@�A�@�(�@�1@��;@���@�;d@���@�~�@�V@�-@�@��@�X@�/@��/@��@��D@�r�@�Z@�1@�|�@�+@��@��y@��y@��@���@���@��+@��\@�ff@�{@���@��@�&�@�&�@�&�@��@���@���@��D@� �@���@��F@�dZ@��@�
=@�
=@��H@���@�5?@�-@�E�@�M�@��@�O�@���@��9@�A�@��m@��@�o@��R@��\@�^5@�J@�@��h@�`B@�V@��/@���@�Q�@�1'@��;@��
@��w@��P@�l�@�K�@�C�@�"�@���@�n�@�=q@��@�{@��@���@��7@�hs@�hs@�x�@�hs@�%@��j@�z�@��@��@��
@�ƨ@��w@���@�o@�v�@�-@�@���@���@��7@�`B@��@��j@��u@�j@�Z@�A�@�(�@�b@�@�w@|�@�@}�T@}�@}p�@|��@|9X@{��@z��@z=q@z-@y�@x�`@xbN@w
=@u�-@t�j@t�@s��@sdZ@r��@q�@qx�@qX@qG�@q&�@q%@q&�@p�u@pQ�@p1'@pb@o�@n��@n�+@nff@n@m�@lj@l�@k��@kƨ@k33@ko@j�!@jM�@j�@ix�@h�`@hr�@h �@hb@g�@g|�@gK�@f�y@f5?@e�@e�T@e�h@e?}@d�D@d�@c��@cS�@co@b~�@a��@a�7@aG�@a&�@`��@`��@`r�@`b@_�w@_l�@_
=@^�+@^$�@^{@^@]�@]��@]�-@]O�@\��@\Z@\�@[�m@[�
@[ƨ@[�F@[��@[t�@[C�@Z��@Y��@YX@Y�@X��@X��@XbN@X �@W�@W�P@W|�@W;d@V�y@V�R@V�+@VV@VV@U�@T��@Tz�@TI�@S��@St�@SC�@SC�@S33@S@RM�@Q�#@Qx�@QX@Q&�@P��@O�;@O�@N�@N�R@Nv�@N5?@M��@M/@L��@L�D@LI�@L1@Kƨ@KS�@K33@J��@J=q@I�@I�#@I��@Ix�@I�@H��@HbN@H �@H  @G�;@G�P@G;d@G
=@F�R@FE�@F$�@E�T@E�-@E?}@D�j@D�D@DZ@DZ@DZ@D(�@C��@Co@B�!@B��@B~�@BM�@B=q@B-@BJ@A�@A��@Ax�@A7L@@�`@@�@@ �@?��@?l�@?+@>�@>ff@=@=��@=��@=�h@=�@=p�@=/@<��@<��@<��@<9X@<�@;�F@;�@;S�@;"�@:�H@:^5@9��@9hs@9hs@9hs@9&�@9�@9%@8�`@8��@8r�@8Q�@8A�@8b@7��@7�@7\)@7
=@6ȴ@6��@6�+@6V@6{@5�T@5��@5�-@5p�@5�@4�j@4I�@3�
@3ƨ@3��@3t�@3dZ@333@3@2��@2~�@2^5@2=q@2J@1�^@1x�@0�`@0��@0�@0�@0�@0Q�@/��@/l�@.��@.v�@.@-��@-�-@-��@-�@-O�@,��@,��@,Z@+��@+�F@+��@+dZ@+C�@+"�@*�@*��@*�\@*�\@*~�@*J@)��@)7L@(��@(�9@(�@(bN@(A�@(1'@(b@'��@'�@'|�@'l�@'\)@';d@'
=@&V@%�@%�-@%��@%p�@%O�@$��@$��@$�j@$�@$�D@$(�@#�F@#��@#dZ@#C�@#o@"��@"n�@"-@!�@!�^@!x�@!%@ �u@ 1'@ b@�w@\)@��@�@ȴ@ȴ@�R@ff@E�@5?@{@�T@��@�@�@V@��@�D@j@I�@�
@��@t�@S�@C�@"�@�H@�!@�\@n�@^5@M�@M�@=q@-@J@�@�^@�7@G�@&�@�@�@%@�`@Ĝ@Q�@ �@�@�w@��@|�@K�@
=@��@��@��@��@��@��@��@�y@�y@��@@��@��@p�@?}@V@��@�j@�D@��@�
@ƨ@�@dZ@S�@C�@o@@@��@^5@=q@�@��@X@�`@Ĝ@�9@�9@��@�u@�u@�u@�@bN@Q�@b@�w@��@�P@\)@+@�@�@�@��@�y@�R@ff@E�@$�@$�@@�@@�-@�-@�-@�h@/@�j@��@�D@z�@Z@9X@�
@�F@�@S�@o@
��@
�!@
�\@
M�@
J@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
��B
��B
��B�B�B)�B2-B8RB<jBC�BH�BH�BH�B\)Bw�B�VB��B��B��B�B�FBǮB�B�B�B  BBDBDB
=BoB�B�B49B@�BA�BA�BA�BI�BVB[#B[#B\)B`BB_;BW
BO�BN�BW
BjBw�B� B�=B�JB�PB�JB�=B�+By�Bw�Bo�BbNBN�B.B5?BhsB_;BO�BG�BP�BXBO�BI�BZBW
BI�BA�B0!B�B49B5?B �B�BhB%B��B�5B�BBÖB��B�=B��B��B�DB�Bk�BA�B_;BQ�BYBN�B6FB33B$�B{B
=B
��B+B
��B
��B
�mB
��B
��B
�+B
p�B
\)B
bNB
]/B
H�B
O�B
G�B
6FB
�B

=B	�fB	��B	�RB	�!B	�oB	� B	]/B	K�B	iyB	`BB	S�B	6FB	�B	!�B	VB	+B	oB	\B	DB	%B��B��B��B�fB��B�sB�B�B�fB�#B�BɺB�B�!B�3B��B��B��B��B}�B�7B�+B��B�VB�%Bv�B}�Bv�Bo�B{�Bm�Bo�Bo�Bn�Bq�Bp�BjBYBYB_;B_;B_;BT�B\)B\)B_;BXBI�BA�BH�B@�BI�BL�B<jBQ�B]/BZBT�BT�BVBVBO�BG�BW
BXBe`BbNB[#BR�BZB^5BT�BZBXB\)BYBP�BJ�BP�BcTBcTBbNBaHB^5B\)BS�BT�BT�BT�BM�BL�BC�BM�BK�BQ�BP�B@�BVBW
BT�BP�BL�BVBW
BgmBk�BhsBffBiyBhsBffBe`Bv�Bt�B}�B�B�B|�By�B}�B�%B�DB�\B�VB�JB�{B��B��B��B��B��B��B��B�-B�9B�3B�3B�XB�RB�wBÖB��BĜB��B��B��B��B�B�;B�`B�B�B�B�B��B	DB	
=B		7B	hB	�B	�B	�B	�B	!�B	(�B	-B	1'B	49B	6FB	7LB	9XB	6FB	6FB	?}B	A�B	A�B	F�B	G�B	H�B	J�B	K�B	L�B	M�B	N�B	Q�B	R�B	T�B	YB	ZB	ZB	bNB	bNB	e`B	gmB	iyB	jB	l�B	u�B	w�B	{�B	}�B	�B	�B	�%B	�=B	�JB	�bB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�3B	�qB	�qB	�wB	��B	�}B	��B	ÖB	ĜB	ĜB	ƨB	ŢB	ƨB	ǮB	ȴB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�#B	�#B	�/B	�;B	�BB	�BB	�BB	�;B	�5B	�HB	�NB	�TB	�TB	�`B	�TB	�sB	�yB	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B
B
  B
B
B
B
B
1B
1B
+B
1B
	7B

=B

=B
DB
DB

=B
JB
DB
PB
VB
PB
PB
VB
VB
VB
PB
PB
\B
bB
oB
hB
bB
oB
oB
uB
uB
oB
bB
hB
uB
uB
�B
�B
�B
�B
{B
hB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
&�B
&�B
%�B
#�B
&�B
&�B
%�B
#�B
%�B
'�B
(�B
(�B
'�B
(�B
'�B
'�B
'�B
&�B
'�B
(�B
(�B
+B
)�B
)�B
)�B
(�B
'�B
)�B
+B
)�B
+B
)�B
,B
,B
-B
-B
,B
.B
.B
0!B
0!B
0!B
0!B
/B
/B
0!B
0!B
0!B
0!B
1'B
33B
33B
2-B
2-B
2-B
1'B
1'B
2-B
33B
49B
49B
49B
49B
49B
33B
2-B
1'B
1'B
33B
49B
5?B
49B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
6FB
5?B
7LB
8RB
9XB
8RB
9XB
:^B
9XB
8RB
7LB
8RB
9XB
:^B
9XB
9XB
7LB
9XB
<jB
=qB
<jB
<jB
<jB
<jB
=qB
>wB
>wB
>wB
?}B
>wB
?}B
>wB
>wB
?}B
A�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
A�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
C�B
E�B
F�B
G�B
F�B
E�B
D�B
E�B
F�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
G�B
H�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
H�B
H�B
H�B
K�B
L�B
L�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
L�B
L�B
L�B
N�B
P�B
P�B
O�B
P�B
P�B
O�B
O�B
O�B
P�B
P�B
P�B
O�B
P�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
Q�B
Q�B
Q�B
Q�B
R�B
T�B
T�B
T�B
T�B
T�B
S�B
T�B
T�B
VB
VB
T�B
T�B
T�B
T�B
W
B
XB
XB
XB
VB
T�B
VB
VB
VB
XB
YB
ZB
ZB
ZB
YB
YB
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
]/B
]/B
\)B
[#B
[#B
\)B
\)B
^5B
^5B
^5B
^5B
_;B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
^5B
]/B
_;B
`BB
aHB
`BB
`BB
`BB
aHB
bNB
aHB
aHB
`BB
`BB
bNB
bNB
bNB
bNB
aHB
bNB
cTB
cTB
cTB
cTB
bNB
cTB
cTB
e`B
dZB
dZB
e`B
ffB
gmB
gmB
gmB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
gmB
hsB
hsB
hsB
gmB
hsB
iyB
iyB
jB
iyB
iyB
iyB
jB
jB
k�B
k�B
k�B
k�B
k�B
jB
jB
jB
k�B
jB
k�B
l�B
l�B
l�B
k�B
k�B
jB
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
n�B
n�B
n�B
n�B
n�B
m�B
l�B
n�B
n�B
o�B
o�B
o�B
p�B
o�B
o�B
n�B
q�B
q�B
q�B
q�B
r�B
r�B
q�B
r�B
r�B
q�B
q�B
q�B
q�B
p�B
q�B
q�B
s�B
t�B
t�B
t�B
t�B
u�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
u�B
u�B
v�B
v�B
w�B
w�B
v�B
v�B
w�B
w�B
w�B
v�B
u�B
v�B
w�B
w�B
x�B
w�B
w�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
y�B
y�B
y�B
z�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
��B
��B
��B2B�B)�B2-B8lB<�BC�BH�BH�BIRB\�Bx�B��B�B�B�fB��B�B�KBևB�1B�;B OB�B�B�B^B[BdB�B4nB@iBA�BA�BA�BI�BVB[#B[=B\]B`vB_�BW�BQBPHBXyBk6Bx8B�iB�=B�dB�jB�~B��B��B{�By>Bq�Bd�BR�B4B9�Bh�B`�BR�BJrBR�BYKBQ�BK�BZkBW�BKDBCaB3�B#nB5B6`B"�B!|B�B�B�B�nB��BǔB��B��B��B�B��B��G�O�BF?B`�BT,BZBPB8�B4�B&�B�B~B  B�B
��B
��B
�_B
�YG�O�B
�xB
u�B
_�B
dZB
_!B
KB
P�B
H�B
8B
#:B
�B	�B	�OB	�B	�B	��B	�-B	b4B	O\B	jB	a�B	VG�O�B	�B	$tB	�B	
rB	,B	�B	�B	�B�]B�B�2B�sB�_B�yB��B��B�RB�]B�1B�0G�O�B�|B�?B�`B�yB�nB�	B��B�DB�lB��B��B�1By�B�By$BrB}<Bo�Bq[BqBo�Br|BqvBk�B[�BZ�B`�B`vB`vBV�B]B\�B_�BX�BK�BD3BJXBB�BK)BNB?cBSB]�BZ�BV9BVBV�BV�BQ4BI�BXBX�Be`Bb�B[�BTaBZ�B^�BV9BZ�BX�B\xBY�BR BLJBRBcnBc�Bb�Ba�B^�B\�BUMBVBU�BU�BN�BNBEmBN�BMBR�BRBB�BVSBW�BU�BR:BNpBV�BX+Bg�Bk�Bh�BgBi�Bi*Bg�BgBwfBu�B~wB�oB�uB}�B{BB��B��B��B�B�PB�2B�B�5B�;B�VB�vB��B��B�aB��B��B��B��B�	B��B��B�'B�9B��B�B�xB̳B�yBߊB�B��B��B�GB�B��B	xB	)B	
	B	�B	B	B	$B	�B	!�B	(�B	-CB	1[B	4TB	6`B	7fB	9�B	6�B	6�B	?�B	A�B	A�B	F�B	G�B	H�B	J�B	K�B	MB	NB	O(B	R B	S@B	UgB	YKB	ZkB	Z�B	bhB	b�B	ezB	g�B	i�B	j�B	l�B	u�B	xB	{�B	}�B	�B	�B	�?B	�=B	�~B	�}B	��B	��B	��B	��B	�qB	��B	��B	��B	��B	��B	�B	�NB	�B	�B	�DB	�DB	�FB	�2B	�QB	��B	��B	�"B	��B	��B	��B	��B	��B	��B	��B	��B	ƨB	��B	��B	ǮB	��B	�B	��B	�B	��B	��B	��B	��B	�"B	�"B	��B	�B	��B	�B	�,B	�&B	�B	�9B	�aB	�	B	�qB	�dB	�;B	�\B	�vB	�vB	ߊB	ޞB	�|B	�hB	�nB	�B	�zB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	��B	�B	�B	��B	�B	�"B	�0B	��B
  B
 B	�VB	�<B	�.B
;B
 OB
aB
aB
gB
SB
KB
fB
zB
fB
	lB

XB

rB
xB
^B

rB
dB
xB
jB
pB
�B
jB
pB
pB
�B
�B
�B
vB
bB
�B
�B
}B
�B
�B
uB
[B
�G�O�B
�B
�B
�B
�B
�B
�B
�G�O�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�G�O�B
�B
�B
�B
�B
�B
G�O�B
�G�O�B
B
�B
�B
�B
�B
�B
�B
 �B
#�B
#�B
#�B
#�B
$�B
%B
%�B
'B
'B
&B
$&B
'B
'B
%�B
$&B
&B
(
B
)B
)B
($B
)B
($B
(
B
(
B
'8B
($B
)*B
)B
+B
*0B
*B
*B
)*B
(>B
*B
+B
*0B
+6B
*KB
,=B
,=B
-)B
-)B
,=B
.IB
.IB
0!B
0;B
0!B
0!B
/OB
/5B
0;B
0UB
0UB
0UB
1AB
3B
33B
2GB
2GB
2GB
1[B
1[B
2aB
3MB
4TB
49B
4B
4TB
4TB
3MB
2GB
1vB
1[B
3hB
4TB
5ZB
4TB
5ZB
5ZB
5tB
6`B
7fB
7LB
7fB
8RB
8RB
8RB
8RG�O�B
5tB
7fB
8lB
9rB
8�B
9rB
:^B
9rB
8lB
7�B
8�B
9rB
:xB
9rB
9�G�O�B
9�B
<jB
=�B
<jB
<�B
<�B
<�B
=�B
>�B
>wB
>�B
?�B
>�B
?�B
>�B
>�B
?�B
A�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
A�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
C�B
E�B
F�B
G�B
F�B
E�B
D�B
E�B
F�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
G�B
H�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
H�B
H�B
H�B
K�B
L�B
L�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
L�B
MB
MB
N�B
P�B
P�B
O�B
P�B
Q B
O�B
O�B
O�B
P�B
Q B
Q B
O�B
Q B
O�B
Q B
Q B
RB
RB
RB
RB
SB
SB
SB
RB
Q�B
R B
R B
SB
T�B
UB
UB
UB
UB
TB
UB
T�B
VB
VB
UB
UB
UB
U2B
W$B
W�B
XB
XG�O�B
U2B
V9B
V9B
V9B
X+B
Y1B
Z7B
Z7B
ZB
Y1B
Y1B
YB
ZB
Z7B
[=B
[=B
[=B
\CB
\CB
\)B
[=B
]/B
]/B
\CB
[WB
[WB
\]B
\]B
^OB
^OB
^OB
^5B
_!B
^OB
^5B
_VB
_VB
_;B
_VB
_VB
^OB
]~B
_VB
`\B
aHB
`\B
`\B
`\B
aHB
bNB
abB
aHB
`vB
`vB
bNB
bhB
bhB
bhB
a|B
bhB
cnB
cnB
cnB
cnB
b�B
cnB
cnB
ezB
dtB
d�B
ezB
f�B
gmB
gmB
g�B
ffB
gmB
gmB
g�B
g�B
g�B
g�B
g�B
h�B
g�B
h�B
h�B
h�B
g�B
h�B
iyB
iyB
jeB
i�B
i�B
i�B
j�B
jB
k�B
k�B
k�B
k�B
k�B
j�B
j�B
j�B
k�B
j�B
k�B
l�B
l�B
l�B
k�B
k�B
j�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
n}B
n�B
n�B
n�B
n�B
m�B
l�B
n�B
n�B
o�B
o�B
o�B
p�B
o�B
o�B
n�B
q�B
q�B
q�B
q�B
r�B
r�B
q�B
r�B
r�B
q�B
q�B
q�B
q�B
p�B
q�B
q�B
s�B
t�B
t�B
t�B
t�B
u�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
u�B
u�B
v�B
v�B
w�B
w�B
v�B
v�B
w�B
w�B
w�B
v�B
u�B
v�B
w�B
w�B
x�B
w�B
w�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
y�B
y�B
y�B
z�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111141111111111111111111111411111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111144111111111111111111111141111114141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�<#�
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.02(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201802080042032018020800420320180208004203201806221325502018062213255020180622132550201804050729232018040507292320180405072923  JA  ARFMdecpA19c                                                                20180204093521  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180204003542  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180204003545  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180204003545  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180204003546  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180204003546  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180204003546  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180204003546  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180204003547  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180204003547                      G�O�G�O�G�O�                JA  ARUP                                                                        20180204005614                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180204153135  CV  JULD            G�O�G�O�F�M�                JM  ARSQJMQC2.0                                                                 20180205000000  CF  PSAL_ADJUSTED_QCCB  D�� G�O�                JM  ARCAJMQC2.0                                                                 20180207154203  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180207154203  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222923  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042550  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221517                      G�O�G�O�G�O�                