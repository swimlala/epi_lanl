CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-20T09:43:23Z creation;2022-06-20T09:43:23Z conversion to V3.1      
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
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ph   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tT   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Έ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �h   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �h   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �h   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220620094323  20220620095819  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               bA   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @��`X�1   @��`�Eg�@<�r� Ĝ�c�����1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@�  A   A   AA��A`  A�  A���A�  A�  A�33A�33A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C�C�C�C�C  C  C�fC  C �C"�C$  C&  C'�fC*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Cg�fCj  Cl  Cn  Cp  Cr  Ct�Cv  Cw�fCz  C|  C~  C�  C��C��3C�  C�  C�  C�  C��C�  C��C�  C�  C��C�  C��3C�  C�  C�  C�  C��C��C�  C��3C��3C�  C�  C�  C��C�  C��3C��3C��3C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D
��Dy�D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D��D� D  D� D  D� DfD�fD  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.y�D/  D/� D0fD0� D1  D1y�D1��D2� D3  D3� D4  D4� D5  D5� D6fD6� D7  D7�fD8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D>��D?� D@  D@� DA  DA� DB  DB� DB��DC� DD  DDy�DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DL  DL�fDM  DM� DN  DN� DO  DO� DP  DP�fDQfDQ� DQ��DR� DS  DS� DTfDT� DT��DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\y�D]  D]� D^  D^y�D_  D_� D`  D`� D`��Da� Db  Db�fDc  Dc� Dd  Dd� De  De� DffDf�fDg  Dg� Dh  Dh� Di  Di� DjfDj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Doy�Dp  Dp� Dq  Dq� Dr  Dry�Ds  Ds� Dt  Dt� Du  Du� Du��Dv� Dw  Dw� Dw��Dxy�Dy  Dy� DzfDz� D{  D{� D{��D|� D}  D}� D~  D~� D  D� D��D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�3D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D���D�<�D�� D���D���D�@ D�� D��3D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ D�|�D�� D�  D�@ D̀ D��3D�  D�@ D̓3D�� D���D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�3D�@ DҀ D�� D�  D�@ DӀ D�� D�3D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�<�D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�3D�@ Dڀ D��3D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݃3D��3D�3D�@ Dހ D�� D�  D�C3D߀ D߼�D�  D�@ D�|�D��D�  D�@ D� D�� D�3D�C3D� D��D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D���D�)�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�
=@���A Q�A Q�AA�A`Q�A�(�A���A�(�A�(�A�\)A�\)A���A�(�B {B{B{B{B {B({B0{B8{B@{BH{BP{BX{B`{Bh{Bp{Bx{B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�=pB�
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
=B�
=C CCCCC
CC�C�C�C�C�CCC�CC �C"�C$C&C'�C*C,C.C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfCg�CjClCnCpCrCt�CvCw�CzC|C~C��C�\C���C��C��C��C��C�\C��C�\C��C��C�\C��C���C��C��C��C��C�\C�\C��C���C���C��C��C��C�\C��C���C���C���C���C���C���C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C�\C��C��C��C���C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C�\C��C��C�\C�\C�\C��C��C�\C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HD
��Dz�DHD�HDHD�HDHD�HD�D�HDHD�HDHD�HDHD�HDHD�HD��D�HDHD�HDHD�HD�D��DHDz�DHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.z�D/HD/�HD0�D0�HD1HD1z�D1��D2�HD3HD3�HD4HD4�HD5HD5�HD6�D6�HD7HD7��D8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD>��D?�HD@HD@�HDAHDA�HDBHDB�HDB��DC�HDDHDDz�DEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ��DKHDK�HDLHDL��DMHDM�HDNHDN�HDOHDO�HDPHDP��DQ�DQ�HDQ��DR�HDSHDS�HDT�DT�HDT��DU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\z�D]HD]�HD^HD^z�D_HD_�HD`HD`�HD`��Da�HDbHDb��DcHDc�HDdHDd�HDeHDe�HDf�Df��DgHDg�HDhHDh�HDiHDi�HDj�Dj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDoz�DpHDp�HDqHDq�HDrHDrz�DsHDs�HDtHDt�HDuHDu�HDu��Dv�HDwHDw�HDw��Dxz�DyHDy�HDz�Dz�HD{HD{�HD{��D|�HD}HD}�HD~HD~�HDHD�HD��D�=qD���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D��D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�C�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�}qD��qD� �D�@�D���D���D� �D�@�D���D���D� �D�C�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�C�D���D���D��D�@�D�}qD���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�}qD��qD��qD�=qD���D��qD��qD�@�D���D���D��D�C�D���D���D� �D�@�D���D���D� �D�@�D�}qD���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D��qD�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�}qD��qD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�D�}qD���D� �D�@�D̀�D���D� �D�@�D̓�D���D��qD�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D��D�@�DҀ�D���D� �D�@�DӀ�D���D��D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�=qD׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D��D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݃�D���D��D�@�Dހ�D���D� �D�C�D߀�D߽qD� �D�@�D�}qD�qD� �D�@�DဤD���D��D�C�D․D�qD� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�=qD怤D���D� �D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D��qD�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D��D���D� �D�@�D�D���D� �D�C�D�D���D� �D�@�D�D���D��qD�@�D���D���D� �D�@�D���D���D� �D�@�D���D��qD� �D�@�D���D���D��D�@�D���D���D� �D�@�D���D���D��qD�*>11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�^A�_�A�aA�bA�^�A�bA�]�A�[#A�aA�c�A�7A�MA��A�NA���Aƕ�AĀ�A�a�A�AA��A��A��*A���A��A��'A�W?A���A�S[A��LA��A�%A��A���A�+�A�%�A��A�d�A�jKA�1'A�\A���A�7�A���A�kQA�	lA��+A���A��8A�7�A�	�A��A��A�v+A��A���A��\A�ںA�	�A�A��}A�}VA���A�͟A�-wA�یA�6�A���A��A�J�A��A�[WA�v`A�l�A�ȀA�`�A��bA�D�A�1A��oA�_A�?HA�GzA�B'A�h�A��7A�͟A���A���A��A�;�A��gA��A�\)A�bA��A��qA��A��`A��A��xA� 4A��.A}�DA{�&A{MAyI�Axw�Av��Au�At��As�*ArMAqV�Ao�AnbAl�Ak�BAk	�AjzAh��Acn/Ab>�Aa-wA_��A_�A^�A^^5A^	�A]e,A\AZ��AY��AX��AXjAWDgAVw2AU��AUffAT��AS��AR��AQ!�AO�ANs�AMq�ALݘALGAKJ�AJ�AAI8�AGffAF�2AF��AF�hAF�AD8AC$tAA��AA
=A@<�A?jA>�KA>}�A>{A=A�A<�[A<S�A:��A9&�A8qvA81A7sA6E�A6%A5b�A4�wA3�	A2�	A2Q�A1dZA0'�A/x�A/_�A/�A.��A-hsA,.IA+��A*�FA*-�A(�]A(@OA':�A&��A&p;A%�DA%+kA$ffA$�A#�A#;A"m�A!�A!J�A �A� A�ZA�YAMA��A�BA-�A��A�XA�A�uAFtA��Ao AjAE9A��AYAHAF�A>�AMjA��A��AeA�VAY�A
��A	��A	U�A�8A��A iAY�A�>AS&A/�AݘA��A  A��A�@��@�i�@� �@��@��@���@���@�V@���@�~@�S�@�ѷ@��@�_�@��@��@�}V@�s@���@���@�	l@�[@���@椩@�)�@��@�S@�/@���@�%@�GE@ݯ�@�{�@�x�@�@��c@�!�@�a@��@�l"@ϗ�@��@ν<@Χ@΂A@�:�@��@�g�@ʷ�@Ɍ~@�R�@�u@��@��@���@�#�@��@���@�($@�  @�RT@�S�@�Q@��@�!-@�#:@��@�4@�l�@��m@���@�s�@�Y@�!�@���@���@���@��D@��@���@�$t@��'@��b@�`�@�?@��h@�%�@�U�@�	l@�ff@��P@�!�@���@���@���@��*@��M@�o@��"@��2@�e�@��7@��@���@�l�@��<@��@��;@���@�|@�b�@�K�@��@��u@�?�@��$@��8@�0U@��=@�O@�%@��o@�+@�u@��A@��&@���@��'@�=@�X@�8�@�S@��@��z@�H@��>@���@�rG@�b�@�9�@��M@�kQ@���@���@�|�@�Vm@���@�:*@���@��z@�Z�@��5@���@�҉@���@��@��e@���@��@���@�m]@�U�@�Dg@�9�@��@��u@��@���@���@�\�@�33@��@��$@���@�3�@�1@�Vm@���@���@���@���@��@���@��.@�R�@�>B@�!�@��@�
�@�@��@��C@�Z�@�%F@��@�S�@���@�u@��9@�ϫ@��w@�x@�:�@��y@��@��@��,@��)@��!@���@�i�@�-�@�M@��@�@�	�@�	�@�
�@�_@�@ݘ@˒@��@Z�@~n�@}��@}^�@|��@|�z@|��@|��@||�@|I�@{�@{C@z��@y*0@x/�@w��@w��@wY@v��@vi�@vJ�@v �@u�@u��@u�~@ue,@tz�@t,=@s�A@s�{@r^5@q��@q��@qzx@q�@p�@p��@p|�@pN�@p*�@o�@o��@oJ#@n�b@nkQ@n$�@n�@m�.@m�@m�T@m�N@m��@l�U@k�m@k.I@j��@jZ�@j6�@jO@i��@i�H@i�^@i��@iY�@h|�@gƨ@g�$@g��@gS�@f��@f��@f��@f_�@e�@ec�@dy>@c�+@c��@cn/@c'�@b��@b�c@b�@bȴ@b��@b�@bl�@bp;@bc @b_�@b-@`�5@_��@_+@^	@]�'@]X@]#�@\��@\tT@\G@[�f@Z�F@Y�d@Y�@Y`B@Y�@X�)@X�o@W�;@W��@WdZ@WA�@V�,@V��@V��@Vi�@U�@Uzx@UVm@U�@T�@T��@Tq@T�@SH�@R�F@R@�@R&�@R@Q�@Q�M@Q4@P��@P�@P"h@O��@Og�@OH�@N��@N��@N��@N��@N�F@NJ�@M��@M�@Mo @M`B@MS&@M=�@M/@Mq@L�v@L�O@L�@L*�@Lx@L@Kخ@K�@K$t@J��@J��@Ju%@JV@Iԕ@Is�@IT�@I	l@H]d@H*�@HG@G�:@GZ�@G;d@G/�@G,�@G@O@Go�@G,�@F��@F��@Fs�@F.�@E��@E��@E�~@E=�@E/@EV@D�`@D��@D��@D]d@D4n@D�@C�&@Cƨ@C�F@C��@Cn/@C33@B��@B��@B��@B}V@B^5@B0U@BJ@A��@A4@@�Y@?خ@?��@?_p@?8@>��@>��@>8�@>e@=��@=�~@=}�@=`B@=&�@<�/@<��@<��@<�o@<V�@;�@;�@;6z@:�F@:\�@:?@:0U@:J@9�@9��@9w2@92a@8��@8��@8�z@8��@8|�@8tT@8D�@8!@7��@7��@7��@79�@7�@6��@6�@6\�@5ϫ@5S&@4�@4��@4�e@4�I@4�@4�I@4z�@3�K@3qv@39�@3
=@2��@2��@1��@1Vm@1 \@0��@0�I@0��@0Xy@01'@0b@/�A@/j�@/
=@.�@.��@.ȴ@.�@.��@.�A@.Q@.e@-�3@-A @,��@,��@,oi@,7�@,4n@,'R@+��@+�K@+��@+�k@+g�@+ i@*ں@*��@*n�@*e@)��@)[W@)!�@(ѷ@(�u@(A�@(~@(M@(�@'� @'��@'�	@'o�@'+@'
=@&�8@&��@&�@'
=@&�'@&��@&z@&e@%�@%�@%��@%x�@%IR@%0�@$��@$�@$�U@$��@$I�@#�w@#��@#s@#=@#�@"�y@"�h@"s�@"&�@!�@!�h@!k�@!Dg@!5�@!�@!@ �P@ �9@ 7�@ (�@ 	�@��@n/@9�@�@��@�X@�@u%@:*@�@�#@k�@7L@@	l@��@�5@�@��@�@�@PH@'R@��@��@��@X�@H�@C�@33@�@�M@�]@�R@Z�@��@ϫ@��@�'@��@��@ �@˒@�*@�P@g�@�@��@�@�L@^5@5?@#:@��@|@c�@:�@V@�v@�?@��@�@:�@"h@�@�@��@˒@��@��@��@��@RT@(@��@��@ff@YK@Q@GE@0U@_@�9@��@S&@+@��@�5@�U@�@�.@g8@9X@%�@M@�@��@�;@��@� @��@��@_p@�c@�@�1@i�@R�@�@��@ԕ@=�@�f@�5@ی@�@>B@x@�K@�@dZ@4�@
��@
�X@
�A@
V@
8�@
1�@
&�@
J@	��@	�t@	��@	��@	m]@	G�@	(�@�v@Ɇ@��@��@��@��@�@��@l"@H@�Q@�V@y�@]�@J#@C�@8@,�@
=@�M@ߤ@�@�,@�X@�}@��@��@Z�@M�@@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�^A�_�A�aA�bA�^�A�bA�]�A�[#A�aA�c�A�7A�MA��A�NA���Aƕ�AĀ�A�a�A�AA��A��A��*A���A��A��'A�W?A���A�S[A��LA��A�%A��A���A�+�A�%�A��A�d�A�jKA�1'A�\A���A�7�A���A�kQA�	lA��+A���A��8A�7�A�	�A��A��A�v+A��A���A��\A�ںA�	�A�A��}A�}VA���A�͟A�-wA�یA�6�A���A��A�J�A��A�[WA�v`A�l�A�ȀA�`�A��bA�D�A�1A��oA�_A�?HA�GzA�B'A�h�A��7A�͟A���A���A��A�;�A��gA��A�\)A�bA��A��qA��A��`A��A��xA� 4A��.A}�DA{�&A{MAyI�Axw�Av��Au�At��As�*ArMAqV�Ao�AnbAl�Ak�BAk	�AjzAh��Acn/Ab>�Aa-wA_��A_�A^�A^^5A^	�A]e,A\AZ��AY��AX��AXjAWDgAVw2AU��AUffAT��AS��AR��AQ!�AO�ANs�AMq�ALݘALGAKJ�AJ�AAI8�AGffAF�2AF��AF�hAF�AD8AC$tAA��AA
=A@<�A?jA>�KA>}�A>{A=A�A<�[A<S�A:��A9&�A8qvA81A7sA6E�A6%A5b�A4�wA3�	A2�	A2Q�A1dZA0'�A/x�A/_�A/�A.��A-hsA,.IA+��A*�FA*-�A(�]A(@OA':�A&��A&p;A%�DA%+kA$ffA$�A#�A#;A"m�A!�A!J�A �A� A�ZA�YAMA��A�BA-�A��A�XA�A�uAFtA��Ao AjAE9A��AYAHAF�A>�AMjA��A��AeA�VAY�A
��A	��A	U�A�8A��A iAY�A�>AS&A/�AݘA��A  A��A�@��@�i�@� �@��@��@���@���@�V@���@�~@�S�@�ѷ@��@�_�@��@��@�}V@�s@���@���@�	l@�[@���@椩@�)�@��@�S@�/@���@�%@�GE@ݯ�@�{�@�x�@�@��c@�!�@�a@��@�l"@ϗ�@��@ν<@Χ@΂A@�:�@��@�g�@ʷ�@Ɍ~@�R�@�u@��@��@���@�#�@��@���@�($@�  @�RT@�S�@�Q@��@�!-@�#:@��@�4@�l�@��m@���@�s�@�Y@�!�@���@���@���@��D@��@���@�$t@��'@��b@�`�@�?@��h@�%�@�U�@�	l@�ff@��P@�!�@���@���@���@��*@��M@�o@��"@��2@�e�@��7@��@���@�l�@��<@��@��;@���@�|@�b�@�K�@��@��u@�?�@��$@��8@�0U@��=@�O@�%@��o@�+@�u@��A@��&@���@��'@�=@�X@�8�@�S@��@��z@�H@��>@���@�rG@�b�@�9�@��M@�kQ@���@���@�|�@�Vm@���@�:*@���@��z@�Z�@��5@���@�҉@���@��@��e@���@��@���@�m]@�U�@�Dg@�9�@��@��u@��@���@���@�\�@�33@��@��$@���@�3�@�1@�Vm@���@���@���@���@��@���@��.@�R�@�>B@�!�@��@�
�@�@��@��C@�Z�@�%F@��@�S�@���@�u@��9@�ϫ@��w@�x@�:�@��y@��@��@��,@��)@��!@���@�i�@�-�@�M@��@�@�	�@�	�@�
�@�_@�@ݘ@˒@��@Z�@~n�@}��@}^�@|��@|�z@|��@|��@||�@|I�@{�@{C@z��@y*0@x/�@w��@w��@wY@v��@vi�@vJ�@v �@u�@u��@u�~@ue,@tz�@t,=@s�A@s�{@r^5@q��@q��@qzx@q�@p�@p��@p|�@pN�@p*�@o�@o��@oJ#@n�b@nkQ@n$�@n�@m�.@m�@m�T@m�N@m��@l�U@k�m@k.I@j��@jZ�@j6�@jO@i��@i�H@i�^@i��@iY�@h|�@gƨ@g�$@g��@gS�@f��@f��@f��@f_�@e�@ec�@dy>@c�+@c��@cn/@c'�@b��@b�c@b�@bȴ@b��@b�@bl�@bp;@bc @b_�@b-@`�5@_��@_+@^	@]�'@]X@]#�@\��@\tT@\G@[�f@Z�F@Y�d@Y�@Y`B@Y�@X�)@X�o@W�;@W��@WdZ@WA�@V�,@V��@V��@Vi�@U�@Uzx@UVm@U�@T�@T��@Tq@T�@SH�@R�F@R@�@R&�@R@Q�@Q�M@Q4@P��@P�@P"h@O��@Og�@OH�@N��@N��@N��@N��@N�F@NJ�@M��@M�@Mo @M`B@MS&@M=�@M/@Mq@L�v@L�O@L�@L*�@Lx@L@Kخ@K�@K$t@J��@J��@Ju%@JV@Iԕ@Is�@IT�@I	l@H]d@H*�@HG@G�:@GZ�@G;d@G/�@G,�@G@O@Go�@G,�@F��@F��@Fs�@F.�@E��@E��@E�~@E=�@E/@EV@D�`@D��@D��@D]d@D4n@D�@C�&@Cƨ@C�F@C��@Cn/@C33@B��@B��@B��@B}V@B^5@B0U@BJ@A��@A4@@�Y@?خ@?��@?_p@?8@>��@>��@>8�@>e@=��@=�~@=}�@=`B@=&�@<�/@<��@<��@<�o@<V�@;�@;�@;6z@:�F@:\�@:?@:0U@:J@9�@9��@9w2@92a@8��@8��@8�z@8��@8|�@8tT@8D�@8!@7��@7��@7��@79�@7�@6��@6�@6\�@5ϫ@5S&@4�@4��@4�e@4�I@4�@4�I@4z�@3�K@3qv@39�@3
=@2��@2��@1��@1Vm@1 \@0��@0�I@0��@0Xy@01'@0b@/�A@/j�@/
=@.�@.��@.ȴ@.�@.��@.�A@.Q@.e@-�3@-A @,��@,��@,oi@,7�@,4n@,'R@+��@+�K@+��@+�k@+g�@+ i@*ں@*��@*n�@*e@)��@)[W@)!�@(ѷ@(�u@(A�@(~@(M@(�@'� @'��@'�	@'o�@'+@'
=@&�8@&��@&�@'
=@&�'@&��@&z@&e@%�@%�@%��@%x�@%IR@%0�@$��@$�@$�U@$��@$I�@#�w@#��@#s@#=@#�@"�y@"�h@"s�@"&�@!�@!�h@!k�@!Dg@!5�@!�@!@ �P@ �9@ 7�@ (�@ 	�@��@n/@9�@�@��@�X@�@u%@:*@�@�#@k�@7L@@	l@��@�5@�@��@�@�@PH@'R@��@��@��@X�@H�@C�@33@�@�M@�]@�R@Z�@��@ϫ@��@�'@��@��@ �@˒@�*@�P@g�@�@��@�@�L@^5@5?@#:@��@|@c�@:�@V@�v@�?@��@�@:�@"h@�@�@��@˒@��@��@��@��@RT@(@��@��@ff@YK@Q@GE@0U@_@�9@��@S&@+@��@�5@�U@�@�.@g8@9X@%�@M@�@��@�;@��@� @��@��@_p@�c@�@�1@i�@R�@�@��@ԕ@=�@�f@�5@ی@�@>B@x@�K@�@dZ@4�@
��@
�X@
�A@
V@
8�@
1�@
&�@
J@	��@	�t@	��@	��@	m]@	G�@	(�@�v@Ɇ@��@��@��@��@�@��@l"@H@�Q@�V@y�@]�@J#@C�@8@,�@
=@�M@ߤ@�@�,@�X@�}@��@��@Z�@M�@@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BΥB��BΊB�VB�pBΥB�pB͹B͟B�~B��B��B��B}qByrBrBj0Bl�BoOBl"Bh�B_VB_VBg�BlqBw�B{�B|�B~(B�[B~�B}�B|�B{�Bz�ByrBw�Bw�BwLBwB{�B��B�B��B��B�[B~�B{�Bx�Bv+Bs�Bl�B`vBS�BB�B<B/iB#�BuBB	�B�BB��B�B��BևB��B�SB�B�aB�)B�@B��B��B��By�Bg�BUMBL�BF%B8�B+B)B�BB�*B��B�B�{B�VB�jB�B��B�{B��B�By�B[=BH�B;JB4B#�B�B
��B
�XB
��B
�B
�B
ңB
��B
ðB
�LB
�B
�nB
��B
�JB
�B
~�B
w�B
poB
MjB
D�B
?cB
6�B
6+B
3B
JrB
O�B
J�B
C-B
5%B
/�B
&fB
#nB
VB
B
=B
!HB
 vB
 B
�B
�B
�B
�B	�<B	�rB	��B	��B	�B	��B	�B	�&B	��B	�\B	ݲB	��B	�B	�DB	ȀB	�9B	�[B	��B	�qB	��B	�	B	��B	��B	�3B	��B	�yB	�RB	�B	�bB	��B	�pB	�B	�/B	��B	�aB	�BB	�)B	�B	�B	�-B	�OB	{�B	s�B	p!B	k6B	g8B	aHB	\�B	\CB	]dB	]�B	\CB	Z7B	V�B	T�B	SB	PbB	M�B	JXB	HfB	BB	=�B	;B	9$B	7�B	5�B	2aB	0!B	-�B	+�B	*KB	#�B	�B	xB	�B	�B	�B	(B	B	
�B		B	�B	�B	 iB�BB�B��B�>B��B�`B��B�B��B�vB�B�IB�QB��B�B��B��B��B�B�B��B�B�kBٚB�B׍B׍BևB��B�MB��B�B�@B�TBѝB�NB�bBϑBΊB�VB̈́B�B�JB��B˒B��B��B�B�B�B��B�lB�B�YB�mB�%B��B�BĶBðB��BªBB�AB�B� B�iB��B��B��B��BĶB��BƨB�+B��B��BǮB�+B��B˒B��B�dB͹B�BB�BѝBңB�BөBөBյB��B��B��B��B��B�pB�B�WB�qB�]B��B�;B��B��B�oB�;B��B�B�B�FB��B�fB��B	�B	�B	�B	
=B	~B	jB	�B	�B	�B	B	B	YB	B	�B	5B	�B	;B	�B	B	jB	xB	!B	&2B	(>B	+�B	1�B	/�B	1vB	9�B	A�B	B'B	B�B	CaB	D�B	FtB	E�B	EmB	EmB	H1B	JXB	J�B	KxB	K�B	MjB	O�B	S[B	XEB	Z�B	\CB	]IB	a-B	d�B	e�B	ffB	h>B	j0B	m�B	n/B	o B	q�B	v+B	v�B	x�B	z^B	|�B	~(B	�B	��B	�oB	��B	��B	�?B	��B	�B	�#B	�xB	��B	�B	��B	�B	�oB	�YB	��B	�/B	�B	�tB	��B	��B	��B	�iB	��B	��B	��B	�%B	�`B	�B	�*B	��B	��B	� B	�YB	�uB	��B	��B	��B	�EB	�CB	�BB	��B	�B	��B	�B	�B	��B	�B	��B	�&B	�ZB	�B	�B	��B	�B	�,B	�`B	��B	�B	�$B	��B	�B	�B	�tB	��B	�XB	�XB	�XB	��B	��B	�B	�}B
 �B
�B

rB
�B
�B
�B
}B
NB
�B
�B
uB
�B
,B
{B
�B
�B
1B
�B
 �B
"�B
"�B
#:B
$�B
%,B
%�B
&�B
'RB
'�B
(�B
)�B
+�B
/OB
0UB
1vB
1�B
2-B
2�B
2�B
3B
4�B
8lB
;B
<�B
>]B
?�B
@B
@�B
AoB
BuB
B�B
C�B
F�B
LJB
OBB
OBB
OBB
P�B
R:B
R�B
SB
UMB
W
B
Y�B
^B
`�B
cB
c�B
e,B
e�B
fB
ffB
f�B
g�B
h�B
j0B
jB
j�B
jB
kB
n�B
r�B
s�B
w2B
x�B
zDB
{B
|�B
~�B
��B
�AB
��B
�+B
��B
��B
�B
��B
�B
��B
��B
�B
��B
��B
�&B
��B
�aB
�9B
��B
�B
��B
�B
�kB
��B
��B
�OB
�pB
�'B
�\B
��B
�bB
�hB
�:B
��B
�B
��B
�XB
�B
�B
�B
�QB
�B
��B
�)B
�CB
�}B
��B
��B
�B
�OB
�iB
��B
��B
��B
��B
��B
��B
�hB
�B
��B
�tB
�+B
��B
�B
�fB
��B
��B
��B
�rB
�*B
��B
�6B
�B
�]B
�OB
��B
� B
�;B
��B
ªB
ÖB
ðB
��B
�gB
��B
�YB
ƎB
��B
�_B
ǮB
�B
�1B
ȚB
�lB
��B
�	B
ʌB
��B
�DB
�DB
˒B
�0B
��B
��B
�pB
��B
�\B
�vB
ϫB
��B
� B
ѷB
��B
�,B
ԕB
��B
��B
�9B
ּB
�sB
׍B
�+B
ؓB
خB
��B
�1B
ٴB
�7B
�QB
�kB
ڠB
�qB
�)B
��B
�OB
ޞB
��B
��B
�VB
ߤB
�B
�vB
�B
�bB
��B
��B
�4B
�NB
�4B
�B
��B
�:B
��B
�@B
��B
�,B
�B
��B
�LB
�mB
�$B
�DB
�B
��B
��B
��B
��B
��B
�QB
�B
�B
�"B
�B
�B
�}B
��B
�B
�B
�B
�B
��B
��B
�B
�B
�[B
�B
�aB
�GB
�B
�B
��B
��B
�MB
�B
��B
��B
�tB
��B
��B
�B
�B
�FB
��B
��B
��B
��B
�B
��B
��B
�8B
�RB
��B
��B
��B
�^B
��B
�0B
��B
�B
�PB
�<B
��B
��B
��B
�B
��B
��B
��B
��B B OBBBUB'B�B�B�B�B�BBmBmB�B�B?B_B_B�B�B1B�B�B	B	�B
	B
rB
�B
�B
�BB
�B)B�BdBJB~BBjB�B�BB<BVB�BB\BvB.BbBbB}B}BbB}B�B BhB�B�B B�B:BTBoBoB�B�B�B&B�B,B�B{B�B�B�B�B
BYB�B�B+B�B�B�B1B�B�B�BB�B�B	BWB�B�B�BCB�B�B�B�BB/B~B~B~B~B�BB�B�B!B!B;BVBpB�B�B 'B �B �B!B!-B!|B!�B!�B!�B"4B"NB"hB"hB"�B"�B"�B"�B"�B"�B#TB$&B$@B$�B$�B$�B%B%B%`B&�B&�B&�B&�B&�B'�B'�B($B(�B(�B(�B)*B)DB)�B)�B*0B*B*0B*KB*�B*�B+B+6B+kB+�B+�B,WB,WB,WB,�B,qB,�B,�B,�B,�B-B-�B-�B.IB.cB.�B.�B.�B.�B.�B/ B/B/5B/5B/5B/OB/iB/�B/�B/�B/�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444BΥB��BΊB�VB�pBΥB�pB͹B͟B�~B��B��B��B}qByrBrBj0Bl�BoOBl"Bh�B_VB_VBg�BlqBw�B{�B|�B~(B�[B~�B}�B|�B{�Bz�ByrBw�Bw�BwLBwB{�B��B�B��B��B�[B~�B{�Bx�Bv+Bs�Bl�B`vBS�BB�B<B/iB#�BuBB	�B�BB��B�B��BևB��B�SB�B�aB�)B�@B��B��B��By�Bg�BUMBL�BF%B8�B+B)B�BB�*B��B�B�{B�VB�jB�B��B�{B��B�By�B[=BH�B;JB4B#�B�B
��B
�XB
��B
�B
�B
ңB
��B
ðB
�LB
�B
�nB
��B
�JB
�B
~�B
w�B
poB
MjB
D�B
?cB
6�B
6+B
3B
JrB
O�B
J�B
C-B
5%B
/�B
&fB
#nB
VB
B
=B
!HB
 vB
 B
�B
�B
�B
�B	�<B	�rB	��B	��B	�B	��B	�B	�&B	��B	�\B	ݲB	��B	�B	�DB	ȀB	�9B	�[B	��B	�qB	��B	�	B	��B	��B	�3B	��B	�yB	�RB	�B	�bB	��B	�pB	�B	�/B	��B	�aB	�BB	�)B	�B	�B	�-B	�OB	{�B	s�B	p!B	k6B	g8B	aHB	\�B	\CB	]dB	]�B	\CB	Z7B	V�B	T�B	SB	PbB	M�B	JXB	HfB	BB	=�B	;B	9$B	7�B	5�B	2aB	0!B	-�B	+�B	*KB	#�B	�B	xB	�B	�B	�B	(B	B	
�B		B	�B	�B	 iB�BB�B��B�>B��B�`B��B�B��B�vB�B�IB�QB��B�B��B��B��B�B�B��B�B�kBٚB�B׍B׍BևB��B�MB��B�B�@B�TBѝB�NB�bBϑBΊB�VB̈́B�B�JB��B˒B��B��B�B�B�B��B�lB�B�YB�mB�%B��B�BĶBðB��BªBB�AB�B� B�iB��B��B��B��BĶB��BƨB�+B��B��BǮB�+B��B˒B��B�dB͹B�BB�BѝBңB�BөBөBյB��B��B��B��B��B�pB�B�WB�qB�]B��B�;B��B��B�oB�;B��B�B�B�FB��B�fB��B	�B	�B	�B	
=B	~B	jB	�B	�B	�B	B	B	YB	B	�B	5B	�B	;B	�B	B	jB	xB	!B	&2B	(>B	+�B	1�B	/�B	1vB	9�B	A�B	B'B	B�B	CaB	D�B	FtB	E�B	EmB	EmB	H1B	JXB	J�B	KxB	K�B	MjB	O�B	S[B	XEB	Z�B	\CB	]IB	a-B	d�B	e�B	ffB	h>B	j0B	m�B	n/B	o B	q�B	v+B	v�B	x�B	z^B	|�B	~(B	�B	��B	�oB	��B	��B	�?B	��B	�B	�#B	�xB	��B	�B	��B	�B	�oB	�YB	��B	�/B	�B	�tB	��B	��B	��B	�iB	��B	��B	��B	�%B	�`B	�B	�*B	��B	��B	� B	�YB	�uB	��B	��B	��B	�EB	�CB	�BB	��B	�B	��B	�B	�B	��B	�B	��B	�&B	�ZB	�B	�B	��B	�B	�,B	�`B	��B	�B	�$B	��B	�B	�B	�tB	��B	�XB	�XB	�XB	��B	��B	�B	�}B
 �B
�B

rB
�B
�B
�B
}B
NB
�B
�B
uB
�B
,B
{B
�B
�B
1B
�B
 �B
"�B
"�B
#:B
$�B
%,B
%�B
&�B
'RB
'�B
(�B
)�B
+�B
/OB
0UB
1vB
1�B
2-B
2�B
2�B
3B
4�B
8lB
;B
<�B
>]B
?�B
@B
@�B
AoB
BuB
B�B
C�B
F�B
LJB
OBB
OBB
OBB
P�B
R:B
R�B
SB
UMB
W
B
Y�B
^B
`�B
cB
c�B
e,B
e�B
fB
ffB
f�B
g�B
h�B
j0B
jB
j�B
jB
kB
n�B
r�B
s�B
w2B
x�B
zDB
{B
|�B
~�B
��B
�AB
��B
�+B
��B
��B
�B
��B
�B
��B
��B
�B
��B
��B
�&B
��B
�aB
�9B
��B
�B
��B
�B
�kB
��B
��B
�OB
�pB
�'B
�\B
��B
�bB
�hB
�:B
��B
�B
��B
�XB
�B
�B
�B
�QB
�B
��B
�)B
�CB
�}B
��B
��B
�B
�OB
�iB
��B
��B
��B
��B
��B
��B
�hB
�B
��B
�tB
�+B
��B
�B
�fB
��B
��B
��B
�rB
�*B
��B
�6B
�B
�]B
�OB
��B
� B
�;B
��B
ªB
ÖB
ðB
��B
�gB
��B
�YB
ƎB
��B
�_B
ǮB
�B
�1B
ȚB
�lB
��B
�	B
ʌB
��B
�DB
�DB
˒B
�0B
��B
��B
�pB
��B
�\B
�vB
ϫB
��B
� B
ѷB
��B
�,B
ԕB
��B
��B
�9B
ּB
�sB
׍B
�+B
ؓB
خB
��B
�1B
ٴB
�7B
�QB
�kB
ڠB
�qB
�)B
��B
�OB
ޞB
��B
��B
�VB
ߤB
�B
�vB
�B
�bB
��B
��B
�4B
�NB
�4B
�B
��B
�:B
��B
�@B
��B
�,B
�B
��B
�LB
�mB
�$B
�DB
�B
��B
��B
��B
��B
��B
�QB
�B
�B
�"B
�B
�B
�}B
��B
�B
�B
�B
�B
��B
��B
�B
�B
�[B
�B
�aB
�GB
�B
�B
��B
��B
�MB
�B
��B
��B
�tB
��B
��B
�B
�B
�FB
��B
��B
��B
��B
�B
��B
��B
�8B
�RB
��B
��B
��B
�^B
��B
�0B
��B
�B
�PB
�<B
��B
��B
��B
�B
��B
��B
��B
��B B OBBBUB'B�B�B�B�B�BBmBmB�B�B?B_B_B�B�B1B�B�B	B	�B
	B
rB
�B
�B
�BB
�B)B�BdBJB~BBjB�B�BB<BVB�BB\BvB.BbBbB}B}BbB}B�B BhB�B�B B�B:BTBoBoB�B�B�B&B�B,B�B{B�B�B�B�B
BYB�B�B+B�B�B�B1B�B�B�BB�B�B	BWB�B�B�BCB�B�B�B�BB/B~B~B~B~B�BB�B�B!B!B;BVBpB�B�B 'B �B �B!B!-B!|B!�B!�B!�B"4B"NB"hB"hB"�B"�B"�B"�B"�B"�B#TB$&B$@B$�B$�B$�B%B%B%`B&�B&�B&�B&�B&�B'�B'�B($B(�B(�B(�B)*B)DB)�B)�B*0B*B*0B*KB*�B*�B+B+6B+kB+�B+�B,WB,WB,WB,�B,qB,�B,�B,�B,�B-B-�B-�B.IB.cB.�B.�B.�B.�B.�B/ B/B/5B/5B/5B/OB/iB/�B/�B/�B/�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220620094133  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220620094323  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220620094323  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220620094323                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220620184328  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220620184328  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20220620095819                      G�O�G�O�G�O�                