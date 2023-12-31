CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-01-02T21:42:19Z creation;2023-01-02T21:42:20Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230102214219  20230102222635  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               vA   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @�
-��GM1   @�
. �.E@;��9Xb�d~��"�1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�  B���B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33C �C  C  C  C  C
  C  C  C  C  C  C�fC  C  C�C�C   C"  C$�C&  C'�fC)�fC+�fC.  C0  C2  C4  C6  C8  C:�C<  C=�fC@  CB  CD�CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C��3C�  C��3C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C��C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C�  C�  C��3C��3C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C�  D   D � D  D� D  D� D  D�fD  D� D  D�fD  D� D  D� DfD�fD	  D	� D
  D
� D  D� D  D� D  D� D  Dy�D  Dy�D��D� D  D� D  D�fD  Dy�D  D� D  Dy�D��D� D  D� D��D� D  D� D  D�fD  Dy�D  D� D  D� DfD� D  D� D   D � D!  D!� D"  D"� D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,�fD-  D-� D.  D.� D/  D/� D0  D0y�D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D=��D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX�fDY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� DcfDc�fDd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� DifDi� Dj  Djy�Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�Ds��Dt� Du  Du� Du��Dvy�Dw  Dw� Dx  Dx� Dy  Dy� DzfDz� D{  D{� D|  D|� D}  D}� D~  D~� D  Dy�D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D���D�@ D�� D�� D���D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D��3D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D���D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�C3D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D��3D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D��3D�� D���D�@ D D��3D�  D�@ DÀ D��3D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D���D�@ DȀ D�� D�3D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�<�Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�<�Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D��3D�3D�@ D� D�� D�  D�@ D� D�� D�3D�C3D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�s3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�=q@���A Q�A Q�A>�RA`Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B@{BH{BP{BX{B`{Bhz�Bp{Bx{B�B�
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
=B�=pB�=pB�=pB�
=B��
B�
=B�=pB�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�=pB�=pB�=pC �CCCCC
CCCCCC�CCC�C�C C"C$�C&C'�C)�C+�C.C0C2C4C6C8C:�C<C=�C@CBCD�CFCHCJCLCNCPCRCTCVCXCZC[�C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C���C��C���C��C���C��C���C��C��C��C��C�\C��C��C��C��C���C�\C��C��C��C���C��C��C��C��C���C���C��C��C��C���C��C��C���C���C��C�\C��C��C�\C��C��C��C��C��C��C��C�\C��C��C��C�\C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C���C��C��C��C��D HD �HDHD�HDHD�HDHD��DHD�HDHD��DHD�HDHD�HD�D��D	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHDz�DHDz�D��D�HDHD�HDHD��DHDz�DHD�HDHDz�D��D�HDHD�HD��D�HDHD�HDHD��DHDz�DHD�HDHD�HD�D�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#��D$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,��D-HD-�HD.HD.�HD/HD/�HD0HD0z�D1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD=��D>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX��DYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDc�Dc��DdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDi�Di�HDjHDjz�DkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDsz�Ds��Dt�HDuHDu�HDu��Dvz�DwHDw�HDxHDx�HDyHDy�HDz�Dz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHDz�D� �D�@�D���D���D� �D�@�D���D���D��D�@�D���D���D� �D�@�D���D��qD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D��qD�@�D���D���D��qD�@�D���D���D� �D�C�D���D���D� �D�@�D���D���D� �D�=qD���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D��qD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�C�D���D���D��qD�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�}qD���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D��D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D��qD�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�C�D���D���D� �D�C�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D��qD�@�D���D���D��qD�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�C�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D��qD�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D��qD�@�DȀ�D���D��D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�=qDр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�=qDڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D�}qD���D� �D�@�D怤D���D� �D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D��qD�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D��D�@�D�D���D� �D�@�D�D���D��D�C�D�D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�}qD���D� �D�@�D���D���D� �D�@�D���D���D� �D�=qD���D���D� �D�@�D�s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�Y�A�YA�Z�A�XyA�Z�A�Z�A�]/A�_;A�^�A�_A�_pA�^�A�\�A�X�A�VmA�<A��A�\A� �A���A��A��aA��A���A��A��A�|�A�UgA��&A��A��A�p�A�*�A���A�4�A�,�A��A��A���A�A A�MA�PA�9�A�]�A��A�tA��?A�I�A��A���A�1�A�lWA���A�.}A�sA��A�o�A��yA�poA� 4A�A��^A�t�A���A�ZQA��6A��+A�wfA�>wA�OvA�GzA�A��FA�K�A�U�A��xA��<A��?A���A��A�v�A�W�A�"A�r|A�:�A��A��A��UA�49A���A�oA��A�B�A��{A�u�A��$A��A�!-A��=A�d&A�0!A��hA�|�A���A�?�A��}A��tA��nA��OA���A�BA�:AHA~�MA}m�A{�!Az��Ay|AwVmAs�nArW?Aq`BApf�Am�kAk��Aj@OAi��AhѷAf*0Ad��Ac($Aa�A`�HA`cA`R�A`	A_!�A\��A[8�AX��AV�AUbNATqvAS��ASc AS
�AR��AR�AR��AR�NAR�'AR��ARAQ�<AQZ�AP�}AN@AK�
AK�AJ�AG8AEX�AD�aADA�AD_AC�6ACMjACVAB��AB=qAA�AA~A@|�A?o A<��A;~(A:��A:L0A9&A8O�A7�A7��A7�6A7��A7� A7DgA6w2A5�A5jA57LA4خA4@�A3�|A3�A3��A3A2�0A1�IA1VA/v`A.�A-�<A-_pA,�AA*��A)�4A(��A(qvA'rGA&��A&M�A%��A$�YA#�
A#4�A"� A"1'A!��A!4A a�A��A9�A��A��A4�A� A,�A�TA8A~A�A9�A�qA�3A��A+kA;�A��A�A  A��A��Ay�A�zAE9A	A
یA
�DA	�A	�$A	�4A	�A�UAQA�A	�Al�A�.A }VA �@��@�3�@���@��f@��r@�4@�S@�H�@��Q@�Vm@�҉@�~�@� �@��@��@��@���@�A @��@�A @�}�@���@��]@�|@�p;@�u@��@��@��@��z@�'�@�x�@��?@ټ@ذ�@׌~@�_�@��+@��>@�ƨ@ձ[@Ԥ�@���@�?�@ъ	@��@�kQ@�֡@�n�@�`B@�T�@�Z@Ʌ@ǟV@ż@�)�@�@�e�@��O@�s�@�RT@�T�@�S�@�0�@�H@�RT@�P�@�P�@�J#@�J#@�9�@�Z@��	@��s@�~�@��@��9@��C@��h@���@�($@���@�j�@���@�H�@��@��@��'@�:�@�8@���@���@�D�@�m]@���@�zx@�u�@��@���@�خ@��H@���@���@��V@�t�@��@��-@��B@��@��@�6�@��@��9@���@�P�@�%F@���@��F@�[W@��F@�l�@�Mj@�IR@�q@��@���@��@�_@�O�@���@�q�@���@���@��q@���@���@��7@�{J@�x@�8@��/@�K^@��w@��k@��@��x@�5?@��^@���@�~�@�1�@�e�@�ߤ@���@�[�@�1�@�@��@��@���@���@�N<@��v@��+@��~@�*0@�!�@��@�o@��@��M@���@�N�@��K@���@���@�,�@��]@���@��\@�ff@�<�@��D@��6@�9�@���@�H�@��@�u@���@��q@�x@�J�@�#�@��c@�|�@�7�@�J@���@�n/@��@��@�~(@�?@�G@��j@���@�7L@��}@�v�@�Q@�%�@{J@�@~�@~�<@~M�@}�@}��@}\�@|��@|Xy@|(�@{��@{��@z��@z.�@y�@y��@x��@w�@wS@v��@vGE@u�@ttT@t�@s��@r@�@q��@qO�@p��@ph�@p�@o�{@n�]@n��@n_�@nW�@nE�@n5?@m��@m��@m�=@mL�@l��@l֡@l֡@l�/@l�/@lی@l��@l��@l�[@lĜ@l��@l�@k��@k�W@k��@k�[@ko�@kJ#@k+@k"�@k�@k�@k�@k�@j��@jR�@i�j@i�H@ie,@i#�@h�_@g��@g/�@f�@f�H@f��@f��@f��@fq�@f^5@fe@e�~@e/@e%F@e�@e;@d��@d`�@c��@cA�@b�@bp;@a�@a*0@aq@a@`��@`'R@_.I@^B[@]N<@\�.@\M@[�[@[b�@Z��@ZC�@Y�@Z_@Y�T@Y��@Y�d@Y�3@Y�3@Y�X@Y��@Y��@Y�M@YT�@X�I@W��@Wt�@W)_@WS@V�h@V{�@V-@U��@Tz�@S��@So�@S@RZ�@Qj@Q+�@Q \@Q \@Q@Q�@Q;@P�@PA�@O��@OS�@O�@N�@N�b@N:*@M��@MDg@L�@L�@L`�@K�]@KMj@Ko@J��@J�@J�@J��@J�@J��@J��@J��@Jȴ@J��@Jc @I�@I�X@I�h@Iw2@Ihs@I?}@H�@Hoi@H!@G�A@G��@Gg�@GC�@G�@Fs�@E�@Ej@EL�@E5�@E&�@E%F@E	l@D��@D�e@DS�@C�@C�f@C\)@C@O@Bl�@A�j@A��@Ap�@AN<@A \@@Ɇ@@��@@~(@@[�@@�@?��@?>�@>�m@>��@>l�@>8�@>	@=�@=��@=Y�@=?}@=2a@=2a@=+@=;@<ѷ@<z�@< �@<�@;�a@;�@;J#@:�h@:��@:�+@:c @:ff@:ff@:.�@9�@8Ĝ@8��@8>B@7�@7��@7dZ@7�@6��@6��@6�'@6��@6kQ@5�@5c�@5 \@4��@4�v@4�@4q@4�@3��@3��@3��@3�@3�@3�@3�$@3��@3x@3dZ@3X�@39�@3.I@3"�@3�@2��@2�6@2�@2��@2� @2h
@2.�@1[W@0��@0��@/��@/�k@/�@.d�@-��@-��@-x�@,�`@,/�@+��@+�0@+]�@*��@*��@*p;@*Ov@*1�@)�@)@)�"@)o @)X@)Dg@)(�@)%@(�v@(�9@(��@(V�@(1'@( �@(!@'�r@'��@'dZ@&��@&�X@&��@&i�@&W�@&=q@&�@%�#@%�"@%!�@$�p@$w�@$2�@$�@#�K@#dZ@#�@"�<@"}V@"M�@"8�@"=q@"Q@"V@"W�@"R�@"R�@"B[@"($@!�d@!��@!k�@!(�@!+@!�@ �O@��@�@s�@��@��@�.@l"@S�@Ft@<�@(�@�@�@��@��@a@8@@�2@�m@�b@��@v�@c @GE@�@�T@�z@��@��@��@�'@��@�M@s�@Dg@+�@�K@��@r�@6@	�@�&@�:@S�@E9@��@��@s�@.�@�@��@rG@N<@A @4@/@*0@ \@%@�U@��@y>@oi@6@��@|�@y�@iD@e�@U�@O@33@$t@"�@"�@�@�B@�!@�6@��@��@p;@GE@�.@�-@�X@��@|@x�@o @[W@IR@L�@B�@:�@(�@�|@��@��@�j@��@�I@bN@�@�a@�k@�f@S�@;d@,�@!-@S@��@҉@�}@��@u%@;�@�3@�S@m]@;@�U@u�@7@��@��@,�@
�y@
͟@
�h@
��@
��@
_�@
W�@	�d@	B�@	%F@�f@�U@�D@/�@��@��@�@�w@~�@P�@C@�@��@��@��@�1@W�@&�@$�@{@�o@�d@��@Xy@ �@b@@�+@�+@�W@��@��@�a@��@�[@�@x@b�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�Y�A�YA�Z�A�XyA�Z�A�Z�A�]/A�_;A�^�A�_A�_pA�^�A�\�A�X�A�VmA�<A��A�\A� �A���A��A��aA��A���A��A��A�|�A�UgA��&A��A��A�p�A�*�A���A�4�A�,�A��A��A���A�A A�MA�PA�9�A�]�A��A�tA��?A�I�A��A���A�1�A�lWA���A�.}A�sA��A�o�A��yA�poA� 4A�A��^A�t�A���A�ZQA��6A��+A�wfA�>wA�OvA�GzA�A��FA�K�A�U�A��xA��<A��?A���A��A�v�A�W�A�"A�r|A�:�A��A��A��UA�49A���A�oA��A�B�A��{A�u�A��$A��A�!-A��=A�d&A�0!A��hA�|�A���A�?�A��}A��tA��nA��OA���A�BA�:AHA~�MA}m�A{�!Az��Ay|AwVmAs�nArW?Aq`BApf�Am�kAk��Aj@OAi��AhѷAf*0Ad��Ac($Aa�A`�HA`cA`R�A`	A_!�A\��A[8�AX��AV�AUbNATqvAS��ASc AS
�AR��AR�AR��AR�NAR�'AR��ARAQ�<AQZ�AP�}AN@AK�
AK�AJ�AG8AEX�AD�aADA�AD_AC�6ACMjACVAB��AB=qAA�AA~A@|�A?o A<��A;~(A:��A:L0A9&A8O�A7�A7��A7�6A7��A7� A7DgA6w2A5�A5jA57LA4خA4@�A3�|A3�A3��A3A2�0A1�IA1VA/v`A.�A-�<A-_pA,�AA*��A)�4A(��A(qvA'rGA&��A&M�A%��A$�YA#�
A#4�A"� A"1'A!��A!4A a�A��A9�A��A��A4�A� A,�A�TA8A~A�A9�A�qA�3A��A+kA;�A��A�A  A��A��Ay�A�zAE9A	A
یA
�DA	�A	�$A	�4A	�A�UAQA�A	�Al�A�.A }VA �@��@�3�@���@��f@��r@�4@�S@�H�@��Q@�Vm@�҉@�~�@� �@��@��@��@���@�A @��@�A @�}�@���@��]@�|@�p;@�u@��@��@��@��z@�'�@�x�@��?@ټ@ذ�@׌~@�_�@��+@��>@�ƨ@ձ[@Ԥ�@���@�?�@ъ	@��@�kQ@�֡@�n�@�`B@�T�@�Z@Ʌ@ǟV@ż@�)�@�@�e�@��O@�s�@�RT@�T�@�S�@�0�@�H@�RT@�P�@�P�@�J#@�J#@�9�@�Z@��	@��s@�~�@��@��9@��C@��h@���@�($@���@�j�@���@�H�@��@��@��'@�:�@�8@���@���@�D�@�m]@���@�zx@�u�@��@���@�خ@��H@���@���@��V@�t�@��@��-@��B@��@��@�6�@��@��9@���@�P�@�%F@���@��F@�[W@��F@�l�@�Mj@�IR@�q@��@���@��@�_@�O�@���@�q�@���@���@��q@���@���@��7@�{J@�x@�8@��/@�K^@��w@��k@��@��x@�5?@��^@���@�~�@�1�@�e�@�ߤ@���@�[�@�1�@�@��@��@���@���@�N<@��v@��+@��~@�*0@�!�@��@�o@��@��M@���@�N�@��K@���@���@�,�@��]@���@��\@�ff@�<�@��D@��6@�9�@���@�H�@��@�u@���@��q@�x@�J�@�#�@��c@�|�@�7�@�J@���@�n/@��@��@�~(@�?@�G@��j@���@�7L@��}@�v�@�Q@�%�@{J@�@~�@~�<@~M�@}�@}��@}\�@|��@|Xy@|(�@{��@{��@z��@z.�@y�@y��@x��@w�@wS@v��@vGE@u�@ttT@t�@s��@r@�@q��@qO�@p��@ph�@p�@o�{@n�]@n��@n_�@nW�@nE�@n5?@m��@m��@m�=@mL�@l��@l֡@l֡@l�/@l�/@lی@l��@l��@l�[@lĜ@l��@l�@k��@k�W@k��@k�[@ko�@kJ#@k+@k"�@k�@k�@k�@k�@j��@jR�@i�j@i�H@ie,@i#�@h�_@g��@g/�@f�@f�H@f��@f��@f��@fq�@f^5@fe@e�~@e/@e%F@e�@e;@d��@d`�@c��@cA�@b�@bp;@a�@a*0@aq@a@`��@`'R@_.I@^B[@]N<@\�.@\M@[�[@[b�@Z��@ZC�@Y�@Z_@Y�T@Y��@Y�d@Y�3@Y�3@Y�X@Y��@Y��@Y�M@YT�@X�I@W��@Wt�@W)_@WS@V�h@V{�@V-@U��@Tz�@S��@So�@S@RZ�@Qj@Q+�@Q \@Q \@Q@Q�@Q;@P�@PA�@O��@OS�@O�@N�@N�b@N:*@M��@MDg@L�@L�@L`�@K�]@KMj@Ko@J��@J�@J�@J��@J�@J��@J��@J��@Jȴ@J��@Jc @I�@I�X@I�h@Iw2@Ihs@I?}@H�@Hoi@H!@G�A@G��@Gg�@GC�@G�@Fs�@E�@Ej@EL�@E5�@E&�@E%F@E	l@D��@D�e@DS�@C�@C�f@C\)@C@O@Bl�@A�j@A��@Ap�@AN<@A \@@Ɇ@@��@@~(@@[�@@�@?��@?>�@>�m@>��@>l�@>8�@>	@=�@=��@=Y�@=?}@=2a@=2a@=+@=;@<ѷ@<z�@< �@<�@;�a@;�@;J#@:�h@:��@:�+@:c @:ff@:ff@:.�@9�@8Ĝ@8��@8>B@7�@7��@7dZ@7�@6��@6��@6�'@6��@6kQ@5�@5c�@5 \@4��@4�v@4�@4q@4�@3��@3��@3��@3�@3�@3�@3�$@3��@3x@3dZ@3X�@39�@3.I@3"�@3�@2��@2�6@2�@2��@2� @2h
@2.�@1[W@0��@0��@/��@/�k@/�@.d�@-��@-��@-x�@,�`@,/�@+��@+�0@+]�@*��@*��@*p;@*Ov@*1�@)�@)@)�"@)o @)X@)Dg@)(�@)%@(�v@(�9@(��@(V�@(1'@( �@(!@'�r@'��@'dZ@&��@&�X@&��@&i�@&W�@&=q@&�@%�#@%�"@%!�@$�p@$w�@$2�@$�@#�K@#dZ@#�@"�<@"}V@"M�@"8�@"=q@"Q@"V@"W�@"R�@"R�@"B[@"($@!�d@!��@!k�@!(�@!+@!�@ �O@��@�@s�@��@��@�.@l"@S�@Ft@<�@(�@�@�@��@��@a@8@@�2@�m@�b@��@v�@c @GE@�@�T@�z@��@��@��@�'@��@�M@s�@Dg@+�@�K@��@r�@6@	�@�&@�:@S�@E9@��@��@s�@.�@�@��@rG@N<@A @4@/@*0@ \@%@�U@��@y>@oi@6@��@|�@y�@iD@e�@U�@O@33@$t@"�@"�@�@�B@�!@�6@��@��@p;@GE@�.@�-@�X@��@|@x�@o @[W@IR@L�@B�@:�@(�@�|@��@��@�j@��@�I@bN@�@�a@�k@�f@S�@;d@,�@!-@S@��@҉@�}@��@u%@;�@�3@�S@m]@;@�U@u�@7@��@��@,�@
�y@
͟@
�h@
��@
��@
_�@
W�@	�d@	B�@	%F@�f@�U@�D@/�@��@��@�@�w@~�@P�@C@�@��@��@��@�1@W�@&�@$�@{@�o@�d@��@Xy@ �@b@@�+@�+@�W@��@��@�a@��@�[@�@x@b�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B?�B@ B@ B@B@B@ B@OB@ B@4B@OB@OB@ B@4B@�BABCaBDMBDBC�BCGBB�BA�B@�B?cB=<B<�B;�B8�B3�B/ B*�B&LB!�BkBvB�cB��B�B�,B�|B�B��B��B��B�-B�_B�B�XB~�B{Bv�BnBeFB[�BR:BJ�BE�B>�B7�B($B�BxB)BB$B�BoB BBjB
#B]BBMB�tB�gB�BϑB�pB�B�lB��BªB��B��B�/B�B��B�#B�mBq'BIB"B�B�B��B�OB��B�vB��B��B��B�IB��B��Bl�B^�BPHB>�B(�BzB%BMB�B
�jB
�B
�KB
��B
�7B
�MB
��B
�LB
�[B
��B
�EB
�\B
�7B
�MB
|6B
n�B
ffB
_�B
Y�B
V�B
U2B
RB
MPB
@�B
8�B
.�B
!�B
WB
�B
oB
bB
BB
pB
�B
�B
<B
jB
JB

�B
�B
%B
�B	�0B	�B	�KB	�*B	�SB	�B	�=B	�RB	��B	�YB	��B	�B	� B	�OB	�jB	�B	�B	�B	��B	��B	��B	��B	�B	�[B	��B	��B	��B	��B	��B	�"B	�xB	�B	��B	��B	�uB	�UB	~wB	~(B	}�B	z�B	y$B	sB	o�B	hsB	`\B	_�B	_!B	]dB	VB	OvB	K�B	H�B	F?B	AUB	?�B	=<B	:�B	6�B	5%B	2B	0oB	.cB	-wB	)yB	(�B	%�B	%,B	#�B	"NB	 BB	�B	�B	B	�B	�B	B	KB	;B��B�jB�B�rB��B�ZB�?B�9B��B�!B��B��B�cB�cB�wB�B�B�kB�B�XB�mB�B�`B�HB�'B�;B�pBބB�B�B�~BܬBܬB�qB��B�B�1BٚB׍B��B��B��B��B�$B�
B�?B��B��B��B՛B�{B�FBյB��B�B�B�SB�+B��B�kB��B��B�/BݘB�B��B�CB�5B�B��B��B�\B��B��B�B�`B�$B�$B��B�B�B��B��B��B��B�$B�rB�>B�$B�>B�6B��B��B��B��B��B�B��B�]B	 �B	�B	�B	�B	�B	�B	B	�B	�B	�B	�B	 B	�B	�B	9B	�B	�B	�B	�B	"hB	$B	%,B	*B	.IB	/�B	4�B	<�B	<jB	<�B	<�B	<�B	="B	>�B	CaB	E�B	G�B	K�B	O�B	P�B	Q B	RB	R�B	S@B	S�B	T�B	Z�B	b�B	d&B	d�B	d�B	ffB	h
B	h�B	j�B	qAB	u%B	w�B	y�B	}�B	~BB	~BB	~]B	~�B	.B	HB	.B	��B	�uB	��B	��B	��B	�B	��B	��B	�FB	�KB	�CB	��B	�NB	�,B	�RB	��B	��B	��B	��B	�6B	��B	�qB	�/B	��B	�|B	�B	�B	��B	��B	�0B	�JB	�B	��B	�oB	żB	��B	ƨB	ʦB	�jB	��B	�}B	� B	��B	�mB	�
B	�#B	��B	��B	�B	�B	�ZB	��B	�B	�"B	�CB	�B	��B	�aB	�MB	�MB	�`B	��B	��B	�<B	�HB
 �B
UB
�B
_B

�B
�B
"B
�B
+B
�B
B
B
�B
CB
�B
;B
 �B
#B
#�B
$@B
%,B
(�B
,"B
-CB
-�B
1B
3�B
7�B
9$B
9�B
:�B
>(B
>�B
@�B
C�B
D�B
EB
F�B
HfB
J�B
M6B
PB
Q4B
QhB
Q�B
Q�B
Q�B
RoB
S�B
S�B
T�B
V9B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W?B
WsB
W�B
ZB
Z7B
ZB
[=B
\]B
\�B
]IB
]dB
]dB
]~B
]~B
]�B
]�B
_�B
`�B
abB
bhB
b�B
d�B
gmB
h�B
i�B
i�B
jB
jKB
j�B
kB
kQB
l�B
o�B
p�B
p�B
qB
qB
q�B
s3B
u�B
wLB
x�B
zB
{dB
}�B
}�B
~B
~�B
��B
��B
�tB
�RB
�xB
�B
�"B
��B
�NB
��B
�@B
�B
�uB
��B
��B
��B
��B
�{B
��B
��B
��B
��B
�?B
�B
��B
�qB
��B
��B
�IB
�B
�VB
��B
�@B
�B
��B
��B
�0B
��B
��B
��B
��B
�B
�B
�B
�)B
��B
�OB
��B
�!B
��B
��B
��B
�B
�B
�+B
�+B
�LB
�	B
��B
��B
��B
��B
�B
��B
�B
�B
��B
�B
�*B
��B
�B
��B
��B
�B
�"B
�qB
��B
�.B
��B
� B
�B
�oB
��B
�AB
�{B
�gB
�mB
ňB
��B
�B
��B
�?B
ƎB
��B
�zB
�1B
�B
�RB
�lB
�^B
�JB
��B
�B
�6B
͟B
�VB
ΥB
ΥB
��B
�BB
ϑB
�}B
��B
�4B
�hB
ѝB
ѷB
��B
�TB
ңB
ҽB
ңB
ңB
ҽB
ҽB
��B
�@B
�FB
��B
�mB
ևB
��B
��B
׍B
�EB
�yB
�yB
�EB
ؓB
�B
�=B
یB
��B
�]B
ܒB
�IB
ݲB
��B
��B
�B
�OB
��B
��B
�\B
�B
��B
��B
�-B
�|B
��B
��B
�NB
��B
��B
��B
��B
��B
�B
� B
�:B
�:B
�nB
�B
�B
�B
�B
�B
�B
�&B
�B
�@B
�@B
�B
��B
�B
�8B
�B
�B
��B
�B
�B
�6B
�"B
�B
�CB
�]B
��B
�}B
��B
�iB
�B
��B
�UB
��B
��B
�B
�'B
�AB
�vB
�B
��B
�-B
�aB
��B
�B
�3B
�B
�hB
��B
�TB
��B
�%B
�?B
�tB
��B
��B
��B
�+B
�zB
�B
�LB
��B
�B
�8B
��B
�	B
�XB
��B
�B
�^B
�xB
�^B
�DB
�^B
�^B
�^B
�^B
�xB
�xB
��B
�JB
�B
�B
�B
�6B
�jB
�B BB�B�B[BuB�B�B�B�B�B�BB�B�B3BMB�B�B�BBBB9BSB�B�B�B�B�B�B�B�B�B%B%BYB�B�B+B+BzBB�BfB	lB	�B	�B
	B
�B
�BB)BDBDB^B^B^B�B�BJB~BdB�B�B�B�B�BB"B"BVBpBpBVB<B�BB�BBB(B\B�BHBHBHB�B�B�B�B B�B B BBNB�B�B�BNB�B�B�B�BB@BuB�B�B�BB,BFB{B�B�BB�B�BB
B?B�B_B�B�B�BBBQBkBQB�BkB�B)B)B�B�BIB�BBB5B5B�B�BpB�B�B�B�B B vB �B vB �B �B �B!�B"hB"�B"�B"�B"�B"�B"�B"�B"�B# B#B# B"�B#TB#�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   B?�B@ B@ B@B@B@ B@OB@ B@4B@OB@OB@ B@4B@�BABCaBDMBDBC�BCGBB�BA�B@�B?cB=<B<�B;�B8�B3�B/ B*�B&LB!�BkBvB�cB��B�B�,B�|B�B��B��B��B�-B�_B�B�XB~�B{Bv�BnBeFB[�BR:BJ�BE�B>�B7�B($B�BxB)BB$B�BoB BBjB
#B]BBMB�tB�gB�BϑB�pB�B�lB��BªB��B��B�/B�B��B�#B�mBq'BIB"B�B�B��B�OB��B�vB��B��B��B�IB��B��Bl�B^�BPHB>�B(�BzB%BMB�B
�jB
�B
�KB
��B
�7B
�MB
��B
�LB
�[B
��B
�EB
�\B
�7B
�MB
|6B
n�B
ffB
_�B
Y�B
V�B
U2B
RB
MPB
@�B
8�B
.�B
!�B
WB
�B
oB
bB
BB
pB
�B
�B
<B
jB
JB

�B
�B
%B
�B	�0B	�B	�KB	�*B	�SB	�B	�=B	�RB	��B	�YB	��B	�B	� B	�OB	�jB	�B	�B	�B	��B	��B	��B	��B	�B	�[B	��B	��B	��B	��B	��B	�"B	�xB	�B	��B	��B	�uB	�UB	~wB	~(B	}�B	z�B	y$B	sB	o�B	hsB	`\B	_�B	_!B	]dB	VB	OvB	K�B	H�B	F?B	AUB	?�B	=<B	:�B	6�B	5%B	2B	0oB	.cB	-wB	)yB	(�B	%�B	%,B	#�B	"NB	 BB	�B	�B	B	�B	�B	B	KB	;B��B�jB�B�rB��B�ZB�?B�9B��B�!B��B��B�cB�cB�wB�B�B�kB�B�XB�mB�B�`B�HB�'B�;B�pBބB�B�B�~BܬBܬB�qB��B�B�1BٚB׍B��B��B��B��B�$B�
B�?B��B��B��B՛B�{B�FBյB��B�B�B�SB�+B��B�kB��B��B�/BݘB�B��B�CB�5B�B��B��B�\B��B��B�B�`B�$B�$B��B�B�B��B��B��B��B�$B�rB�>B�$B�>B�6B��B��B��B��B��B�B��B�]B	 �B	�B	�B	�B	�B	�B	B	�B	�B	�B	�B	 B	�B	�B	9B	�B	�B	�B	�B	"hB	$B	%,B	*B	.IB	/�B	4�B	<�B	<jB	<�B	<�B	<�B	="B	>�B	CaB	E�B	G�B	K�B	O�B	P�B	Q B	RB	R�B	S@B	S�B	T�B	Z�B	b�B	d&B	d�B	d�B	ffB	h
B	h�B	j�B	qAB	u%B	w�B	y�B	}�B	~BB	~BB	~]B	~�B	.B	HB	.B	��B	�uB	��B	��B	��B	�B	��B	��B	�FB	�KB	�CB	��B	�NB	�,B	�RB	��B	��B	��B	��B	�6B	��B	�qB	�/B	��B	�|B	�B	�B	��B	��B	�0B	�JB	�B	��B	�oB	żB	��B	ƨB	ʦB	�jB	��B	�}B	� B	��B	�mB	�
B	�#B	��B	��B	�B	�B	�ZB	��B	�B	�"B	�CB	�B	��B	�aB	�MB	�MB	�`B	��B	��B	�<B	�HB
 �B
UB
�B
_B

�B
�B
"B
�B
+B
�B
B
B
�B
CB
�B
;B
 �B
#B
#�B
$@B
%,B
(�B
,"B
-CB
-�B
1B
3�B
7�B
9$B
9�B
:�B
>(B
>�B
@�B
C�B
D�B
EB
F�B
HfB
J�B
M6B
PB
Q4B
QhB
Q�B
Q�B
Q�B
RoB
S�B
S�B
T�B
V9B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W?B
WsB
W�B
ZB
Z7B
ZB
[=B
\]B
\�B
]IB
]dB
]dB
]~B
]~B
]�B
]�B
_�B
`�B
abB
bhB
b�B
d�B
gmB
h�B
i�B
i�B
jB
jKB
j�B
kB
kQB
l�B
o�B
p�B
p�B
qB
qB
q�B
s3B
u�B
wLB
x�B
zB
{dB
}�B
}�B
~B
~�B
��B
��B
�tB
�RB
�xB
�B
�"B
��B
�NB
��B
�@B
�B
�uB
��B
��B
��B
��B
�{B
��B
��B
��B
��B
�?B
�B
��B
�qB
��B
��B
�IB
�B
�VB
��B
�@B
�B
��B
��B
�0B
��B
��B
��B
��B
�B
�B
�B
�)B
��B
�OB
��B
�!B
��B
��B
��B
�B
�B
�+B
�+B
�LB
�	B
��B
��B
��B
��B
�B
��B
�B
�B
��B
�B
�*B
��B
�B
��B
��B
�B
�"B
�qB
��B
�.B
��B
� B
�B
�oB
��B
�AB
�{B
�gB
�mB
ňB
��B
�B
��B
�?B
ƎB
��B
�zB
�1B
�B
�RB
�lB
�^B
�JB
��B
�B
�6B
͟B
�VB
ΥB
ΥB
��B
�BB
ϑB
�}B
��B
�4B
�hB
ѝB
ѷB
��B
�TB
ңB
ҽB
ңB
ңB
ҽB
ҽB
��B
�@B
�FB
��B
�mB
ևB
��B
��B
׍B
�EB
�yB
�yB
�EB
ؓB
�B
�=B
یB
��B
�]B
ܒB
�IB
ݲB
��B
��B
�B
�OB
��B
��B
�\B
�B
��B
��B
�-B
�|B
��B
��B
�NB
��B
��B
��B
��B
��B
�B
� B
�:B
�:B
�nB
�B
�B
�B
�B
�B
�B
�&B
�B
�@B
�@B
�B
��B
�B
�8B
�B
�B
��B
�B
�B
�6B
�"B
�B
�CB
�]B
��B
�}B
��B
�iB
�B
��B
�UB
��B
��B
�B
�'B
�AB
�vB
�B
��B
�-B
�aB
��B
�B
�3B
�B
�hB
��B
�TB
��B
�%B
�?B
�tB
��B
��B
��B
�+B
�zB
�B
�LB
��B
�B
�8B
��B
�	B
�XB
��B
�B
�^B
�xB
�^B
�DB
�^B
�^B
�^B
�^B
�xB
�xB
��B
�JB
�B
�B
�B
�6B
�jB
�B BB�B�B[BuB�B�B�B�B�B�BB�B�B3BMB�B�B�BBBB9BSB�B�B�B�B�B�B�B�B�B%B%BYB�B�B+B+BzBB�BfB	lB	�B	�B
	B
�B
�BB)BDBDB^B^B^B�B�BJB~BdB�B�B�B�B�BB"B"BVBpBpBVB<B�BB�BBB(B\B�BHBHBHB�B�B�B�B B�B B BBNB�B�B�BNB�B�B�B�BB@BuB�B�B�BB,BFB{B�B�BB�B�BB
B?B�B_B�B�B�BBBQBkBQB�BkB�B)B)B�B�BIB�BBB5B5B�B�BpB�B�B�B�B B vB �B vB �B �B �B!�B"hB"�B"�B"�B"�B"�B"�B"�B"�B# B#B# B"�B#TB#�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230102214209  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230102214219  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230102214220  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230102214220                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230102214221  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230102214221  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230102222635                      G�O�G�O�G�O�                