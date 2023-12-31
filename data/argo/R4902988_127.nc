CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-04-02T12:42:37Z creation;2023-04-02T12:42:38Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230402124237  20230402125720  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @� ����1   @� ��-��@;�Q���c����o1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�33A�  A�  B   B  B  B��B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�33B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�fC  C  C  C  C�fC  C   C"�C$  C&  C(  C)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C=�fC@  CB  CD  CF  CH  CJ�CL�CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��3C�  C�  C�  C��C�  C�  C��3C�  C��C�  C�  C�  C��C�  C��3C�  C�  C�  C��C�  C�  C��C��3C�  C�  C�  C��C�  C��3C��C�  C�  C�  C�  C�  C��3C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��D   D � D  D� D  D� D  D� D  D� D��Dy�D  D� D  D� D  D� D	  D	�fD
  D
� D  Dy�D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'y�D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9fD9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� D@��DA� DB  DB� DC  DCy�DD  DD� DEfDE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DW��DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^�fD_fD_� D`  D`� Da  Da� Db  Db� Db��Dcy�Dc��Ddy�Dd��Dey�Df  Df�fDg  Dgy�Dh  Dh� DifDi� Dj  Dj� Dk  Dk� Dl  Dl� DmfDm� Dn  Dn� Dn��Do� Dp  Dp� Dq  Dq� Dr  Dr� DsfDs� Dt  Dt� Du  Du� Dv  Dvy�Dw  Dw� DxfDx� DyfDy�fDzfDz�fD{fD{� D|  D|y�D}  D}� D~  D~� D  D� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D��3D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D���D�<�D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D���D�@ D D�� D�  D�@ DÀ D�� D�  D�@ Dă3D��3D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�3D�@ D�|�D�� D�  D�@ D�|�D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D�|�D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�3D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D��3D�  D�@ Dـ D�� D�  D�@ D�|�D�� D�  D�<�D�|�D�� D�  D�C3D܃3D��3D�  D�@ D݀ D�� D�  D�@ Dހ D��3D�3D�@ D߀ D��3D�3D�@ D�� D�� D�  D�@ D� D��3D�3D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D�3D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D�3D��3D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D���D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�)�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�p�@���A Q�A Q�A@Q�A`Q�A�(�A�(�A�(�A�(�A�(�A�\)A�(�A�(�B {B{B{B�B�B({B0{B8{B@{BH{BP{BX{B`{Bh{Bp{Bx{B�
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
=B�=pB�
=B�
=B�
=B�=pB�=pB�
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
=C CCCCC
CCCC�CCCCC�CC C"�C$C&C(C)�C,C.C0C2C4C6C8C:C<C=�C@CBCDCFCHCJ�CL�CNCPCRCTCVCXCZC\�C^C`CbCdCfChCjCl�Cn�CpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C���C��C��C��C�\C��C��C���C��C�\C��C��C��C�\C��C���C��C��C��C�\C��C��C�\C���C��C��C��C�\C��C���C�\C��C��C��C��C��C���C��C�\C�\C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C���C��C��C��C��C��C�\C�\C��C��C��C��C��C���C��C��C���C��C�\C��C��C��C��C��C��C��C��C��C�\C�\C��C��C��C��C��C��C��C��C���C��C�\D HD �HDHD�HDHD�HDHD�HDHD�HD��Dz�DHD�HDHD�HDHD�HD	HD	��D
HD
�HDHDz�DHD�HDHD�HDHD�HDHD�HDHD�HD�D�HDHD�HDHD�HDHD�HD��D�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'z�D(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9�D9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HD@��DA�HDBHDB�HDCHDCz�DDHDD�HDE�DE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDW��DX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^��D_�D_�HD`HD`�HDaHDa�HDbHDb�HDb��Dcz�Dc��Ddz�Dd��Dez�DfHDf��DgHDgz�DhHDh�HDi�Di�HDjHDj�HDkHDk�HDlHDl�HDm�Dm�HDnHDn�HDn��Do�HDpHDp�HDqHDq�HDrHDr�HDs�Ds�HDtHDt�HDuHDu�HDvHDvz�DwHDw�HDx�Dx�HDy�Dy��Dz�Dz��D{�D{�HD|HD|z�D}HD}�HD~HD~�HDHD�HD� �D�@�D���D��qD� �D�@�D���D���D� �D�C�D���D���D� �D�@�D���D���D� �D�@�D���D���D��D�C�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D��qD��qD�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D��qD� �D�@�D���D���D��D�@�D���D���D� �D�@�D���D���D� �D�@�D�}qD���D� �D�=qD�}qD���D� �D�@�D���D���D� �D�@�D�}qD��qD� �D�@�D���D���D� �D�@�D�}qD��qD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D��qD�=qD���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D��qD� �D�@�D���D���D��qD�@�D���D���D� �D�@�D���D���D� �D�@�D�}qD���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D��qD�=qD���D���D� �D�@�D���D���D��D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�=qD�}qD���D� �D�@�D���D���D��qD�@�D�D���D� �D�@�DÀ�D���D� �D�@�Dă�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D��D�@�D�}qD���D� �D�@�D�}qD���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D�}qD���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D��D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�D�}qD���D� �D�=qD�}qD���D� �D�C�D܃�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D��D�@�D߀�D���D��D�@�D���D���D� �D�@�DဤD���D��D�@�D�}qD���D� �D�@�D〤D���D� �D�@�D��D���D� �D�@�D値D���D� �D�@�D怤D���D� �D�@�D瀤D���D� �D�@�D��D���D� �D�@�D逤D���D� �D�=qDꀤD���D� �D�@�D�}qD���D� �D�@�D쀤D���D� �D�@�D��D���D��D�@�DD���D� �D�@�DD���D� �D�@�D���D���D��qD�@�D�D���D� �D�@�D�}qD���D� �D�@�D�D���D� �D�=qD�D���D� �D�@�D���D���D� �D�C�D���D���D� �D�@�D�}qD���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�*>11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��NA��;A�MA�MjA��+A���A��A���A�P�A�!�A���A��pA���A��@A���A�m]A�Y�A�2aA��KA��_A�VA���A�~]A���A�y�A��	A���A�xA�PA���A�A�A��UA�dZA��WA��mA���A���A�g�A�K^A�g8A�1'A�AUA�B�A�3�A�($A��A���A���A�e,A��A��A��4A�XyA�hA���A��A��:A��A�OBA��aA���A��A��4A�n�A�;�A��A�_pA�m�A�W�A��UA���A���A���A�$@A��
A���A�y�A��jA�~]A��AA��A�=qA��qA�s�A���A��$A��'A���A��tA��"A��vA�8RA��A�y�A�v�A��A��]A��A�u�A�v`A�!�A��A�jA~��A|�:A{5�Az@�Ax.IAvN<Au��AuE9At��As��Ar�Ap�Ao҉An��Am�AjK^Aih�Ag�9Ae�`AcݘAb��AasA`��A_��A^�cA\��AY&�AV��AVGEAS�AR�FAQ��ANw�AL�mAL[�ALPHALAK�AKMAJ�_AJJ�AHM�AG($AFxAEC�ADT�AC>BABA�AAjAAPHAA�A@�&A?ɆA>3�A=_A<�A<SA;��A;3�A:�A:33A9�6A9�$A8�yA6x�A4�bA4:�A3�2A3xlA3�A2��A2 �A1��A0�hA0-�A/��A/�A/-A.SA-j�A-�A,g8A+�A+�A*\�A(&�A'��A'~�A&��A&��A&%�A%>�A$�A#E�A"M�A!qA!�A �[A�,A�	AU2A�A�A��A��AkQA/A�Ao�A��A�A��A��A�A�A��A�|A�AN<A4A�A�rA�A�"A͟AN<A�}A
��A�A{JA4A��A1'A�oA�A�XA|�A6�AkQA iDA 	A �@���@���@�E9@��}@�!@��}@�w2@�4@�ߤ@�ݘ@���@�Ov@��@�J#@�~@�Mj@�+@�rG@��@�4@�#:@�@�f�@�($@��@�:�@�q@�j@��@���@�{�@�B[@ߴ�@�YK@���@��@��+@�GE@��9@�O@���@�!�@�q@ѥ@͗$@�1@�~(@�V@��c@ƕ�@��@��f@���@�H�@���@���@���@�F�@�	l@�8�@�E9@��[@�E�@��$@�j@���@�{�@�6�@�-@�x�@��@��,@��z@��A@��@@�y�@�S&@��+@�Ov@�C�@�<�@��@�� @�0�@�ȴ@��@��'@�|@�c�@�33@��F@���@��.@�x�@�Q�@�	l@�ѷ@��@��@@��s@�r�@��m@��t@�e�@�_@��n@��j@�U2@�7�@��@��@�J�@��@��E@���@��M@�@@���@��<@�G@���@�%@��D@�A @�-�@�u%@��@���@���@��4@�s�@�l�@�e�@�E9@��m@�=q@��@��S@�W?@��<@���@�F@�1�@�+@�c@��P@�K�@�4n@��
@���@���@���@�v`@��8@�͟@��b@��h@���@��K@�x�@���@���@�u@�"h@�:*@�1@��]@��-@�	l@��z@�b@�ԕ@��$@�b�@�U�@�C�@�5�@�)_@� \@�o@��@���@�A�@���@��@@��@�t�@�]�@�B�@��@��@�p;@�;�@���@��;@��H@��*@��=@�@��@���@�'�@��!@�@��:@��3@��X@�Mj@�+@���@��@��Y@��r@���@�`B@��@��_@�U2@�~@�~�@��@�6@���@�`B@�
=@��@���@��@�|�@�I�@��@>�@~Ta@}�#@}��@}T�@|��@|4n@{j�@z�@z�x@za|@y�>@y��@y@x�@xI�@w��@wt�@v�@v��@vh
@v)�@u�@u��@u��@u^�@uY�@uVm@uIR@u��@uN<@u!�@t��@t�@s��@sF�@r҉@r�X@r��@r��@r
�@p��@o�@o��@o��@oƨ@o�6@o��@oo�@o(@n}V@n{@m�@m�7@mf�@mDg@l�5@l�?@l��@l��@l��@l�Y@l7�@k˒@j��@i�@i��@h�5@h1@g��@h  @g�
@g�F@gJ#@f�]@f�@e�@eu�@e@dĜ@d�@c��@c�F@c��@cb�@c1�@b�'@a�@a�7@a`B@`�f@`l"@` �@_�a@_1�@^͟@^�L@^��@^�r@]��@]o @]�@\�@\��@\:�@[�q@[/�@Z?@Y�~@YIR@Y(�@X��@XXy@X �@XM@X�@W�;@W�@Ws@V�H@V �@U�^@U[W@U%F@T��@S>�@Rff@Rh
@RGE@Qԕ@QT�@P��@P�_@PU2@PFt@P<�@O�@O!-@N��@N��@N��@N�+@M��@Mf�@M[W@MB�@L�@M�@L��@L>B@K�A@K��@KC@J��@J0U@I��@I�@I�^@I�-@Ik�@H��@H:�@Gخ@G,�@Eϫ@D��@C��@C=@B��@B�'@B�m@B��@B�]@B�H@B�@B1�@A�^@AA @@�|@@�$@@��@@�@@u�@@w�@@|�@@j@@�@?C�@?�@?�@>�s@>��@>u%@>a|@>8�@=�Z@=@=��@=�'@=�@=��@=��@=p�@=/@<��@<��@<q@<_@<]d@<7@;ݘ@;�;@;˒@;��@;K�@;6z@;)_@;)_@;+@;$t@:��@9��@9��@9e,@9#�@8�@8��@8�[@8�_@8Z@8  @7�
@7ݘ@7� @7��@7��@7�@7��@7v`@7]�@7/�@7)_@7C@7�@6��@6)�@5��@5\�@5#�@5+@5�@4�@4��@4Q�@4�@3خ@3�K@3��@3O@2�M@2�X@2��@2�@2J�@1��@1��@1�@0�@0�4@0q@0 �@/��@/Mj@/A�@/@O@/$t@.�8@.��@.��@.��@.��@.��@.u%@.J�@-�>@-�@-��@-N<@,�|@,��@,��@,j@,PH@,2�@+�;@+��@+��@+t�@+;d@+Y@+@+S@*�R@*��@*^5@)�@(�O@(7@'�@'�F@'��@'��@'�{@'x@'o�@'A�@'�@&��@&ں@&��@&1�@%��@%��@$ی@$�@$Ft@$�@$�@$�@$  @#�@#�+@#�@#�@#�g@#��@#x@#_p@#dZ@#a@#F�@#+@#S@"xl@"p;@"p;@"q�@"^5@"1�@"�@!��@!V@ ��@ ��@ �O@ u�@ /�@ �@�@|�@]�@C�@)_@�@C�@@�@a�@�K@�u@~(@l"@>B@%�@�@{J@�@�}@h
@5?@�@�.@��@��@��@��@^�@ \@��@j@6@�+@�g@�@�@\)@Mj@@�@�s@�b@=q@
�@�3@��@L�@?}@<6@+�@��@�O@�I@�@��@��@oi@D�@�+@�[@��@�:@s@\)@RT@A�@4�@�@�h@�+@Ov@1�@�@4@u@��@��@��@��@�@�@�@�4@�.@�@C-@�@��@��@��@|�@W?@H�@;d@�@@��@��@��@��@s�@ff@M�@6�@@��@p�@+�@�@֡@�U@��@��@�4@6@�@�@�@7@ �@x@��@�Q@��@��@g�@8@�@
�"@
�@
��@
ff@	��@	^�@	A @	5�@	-w@	 \@	�@	%@�[@��@�9@��@~(@D�@,=@*�@%�@ �@�@  @�@�a@��@�@@��@�P@n/@H�@�@��@��@��@�}@�@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��NA��;A�MA�MjA��+A���A��A���A�P�A�!�A���A��pA���A��@A���A�m]A�Y�A�2aA��KA��_A�VA���A�~]A���A�y�A��	A���A�xA�PA���A�A�A��UA�dZA��WA��mA���A���A�g�A�K^A�g8A�1'A�AUA�B�A�3�A�($A��A���A���A�e,A��A��A��4A�XyA�hA���A��A��:A��A�OBA��aA���A��A��4A�n�A�;�A��A�_pA�m�A�W�A��UA���A���A���A�$@A��
A���A�y�A��jA�~]A��AA��A�=qA��qA�s�A���A��$A��'A���A��tA��"A��vA�8RA��A�y�A�v�A��A��]A��A�u�A�v`A�!�A��A�jA~��A|�:A{5�Az@�Ax.IAvN<Au��AuE9At��As��Ar�Ap�Ao҉An��Am�AjK^Aih�Ag�9Ae�`AcݘAb��AasA`��A_��A^�cA\��AY&�AV��AVGEAS�AR�FAQ��ANw�AL�mAL[�ALPHALAK�AKMAJ�_AJJ�AHM�AG($AFxAEC�ADT�AC>BABA�AAjAAPHAA�A@�&A?ɆA>3�A=_A<�A<SA;��A;3�A:�A:33A9�6A9�$A8�yA6x�A4�bA4:�A3�2A3xlA3�A2��A2 �A1��A0�hA0-�A/��A/�A/-A.SA-j�A-�A,g8A+�A+�A*\�A(&�A'��A'~�A&��A&��A&%�A%>�A$�A#E�A"M�A!qA!�A �[A�,A�	AU2A�A�A��A��AkQA/A�Ao�A��A�A��A��A�A�A��A�|A�AN<A4A�A�rA�A�"A͟AN<A�}A
��A�A{JA4A��A1'A�oA�A�XA|�A6�AkQA iDA 	A �@���@���@�E9@��}@�!@��}@�w2@�4@�ߤ@�ݘ@���@�Ov@��@�J#@�~@�Mj@�+@�rG@��@�4@�#:@�@�f�@�($@��@�:�@�q@�j@��@���@�{�@�B[@ߴ�@�YK@���@��@��+@�GE@��9@�O@���@�!�@�q@ѥ@͗$@�1@�~(@�V@��c@ƕ�@��@��f@���@�H�@���@���@���@�F�@�	l@�8�@�E9@��[@�E�@��$@�j@���@�{�@�6�@�-@�x�@��@��,@��z@��A@��@@�y�@�S&@��+@�Ov@�C�@�<�@��@�� @�0�@�ȴ@��@��'@�|@�c�@�33@��F@���@��.@�x�@�Q�@�	l@�ѷ@��@��@@��s@�r�@��m@��t@�e�@�_@��n@��j@�U2@�7�@��@��@�J�@��@��E@���@��M@�@@���@��<@�G@���@�%@��D@�A @�-�@�u%@��@���@���@��4@�s�@�l�@�e�@�E9@��m@�=q@��@��S@�W?@��<@���@�F@�1�@�+@�c@��P@�K�@�4n@��
@���@���@���@�v`@��8@�͟@��b@��h@���@��K@�x�@���@���@�u@�"h@�:*@�1@��]@��-@�	l@��z@�b@�ԕ@��$@�b�@�U�@�C�@�5�@�)_@� \@�o@��@���@�A�@���@��@@��@�t�@�]�@�B�@��@��@�p;@�;�@���@��;@��H@��*@��=@�@��@���@�'�@��!@�@��:@��3@��X@�Mj@�+@���@��@��Y@��r@���@�`B@��@��_@�U2@�~@�~�@��@�6@���@�`B@�
=@��@���@��@�|�@�I�@��@>�@~Ta@}�#@}��@}T�@|��@|4n@{j�@z�@z�x@za|@y�>@y��@y@x�@xI�@w��@wt�@v�@v��@vh
@v)�@u�@u��@u��@u^�@uY�@uVm@uIR@u��@uN<@u!�@t��@t�@s��@sF�@r҉@r�X@r��@r��@r
�@p��@o�@o��@o��@oƨ@o�6@o��@oo�@o(@n}V@n{@m�@m�7@mf�@mDg@l�5@l�?@l��@l��@l��@l�Y@l7�@k˒@j��@i�@i��@h�5@h1@g��@h  @g�
@g�F@gJ#@f�]@f�@e�@eu�@e@dĜ@d�@c��@c�F@c��@cb�@c1�@b�'@a�@a�7@a`B@`�f@`l"@` �@_�a@_1�@^͟@^�L@^��@^�r@]��@]o @]�@\�@\��@\:�@[�q@[/�@Z?@Y�~@YIR@Y(�@X��@XXy@X �@XM@X�@W�;@W�@Ws@V�H@V �@U�^@U[W@U%F@T��@S>�@Rff@Rh
@RGE@Qԕ@QT�@P��@P�_@PU2@PFt@P<�@O�@O!-@N��@N��@N��@N�+@M��@Mf�@M[W@MB�@L�@M�@L��@L>B@K�A@K��@KC@J��@J0U@I��@I�@I�^@I�-@Ik�@H��@H:�@Gخ@G,�@Eϫ@D��@C��@C=@B��@B�'@B�m@B��@B�]@B�H@B�@B1�@A�^@AA @@�|@@�$@@��@@�@@u�@@w�@@|�@@j@@�@?C�@?�@?�@>�s@>��@>u%@>a|@>8�@=�Z@=@=��@=�'@=�@=��@=��@=p�@=/@<��@<��@<q@<_@<]d@<7@;ݘ@;�;@;˒@;��@;K�@;6z@;)_@;)_@;+@;$t@:��@9��@9��@9e,@9#�@8�@8��@8�[@8�_@8Z@8  @7�
@7ݘ@7� @7��@7��@7�@7��@7v`@7]�@7/�@7)_@7C@7�@6��@6)�@5��@5\�@5#�@5+@5�@4�@4��@4Q�@4�@3خ@3�K@3��@3O@2�M@2�X@2��@2�@2J�@1��@1��@1�@0�@0�4@0q@0 �@/��@/Mj@/A�@/@O@/$t@.�8@.��@.��@.��@.��@.��@.u%@.J�@-�>@-�@-��@-N<@,�|@,��@,��@,j@,PH@,2�@+�;@+��@+��@+t�@+;d@+Y@+@+S@*�R@*��@*^5@)�@(�O@(7@'�@'�F@'��@'��@'�{@'x@'o�@'A�@'�@&��@&ں@&��@&1�@%��@%��@$ی@$�@$Ft@$�@$�@$�@$  @#�@#�+@#�@#�@#�g@#��@#x@#_p@#dZ@#a@#F�@#+@#S@"xl@"p;@"p;@"q�@"^5@"1�@"�@!��@!V@ ��@ ��@ �O@ u�@ /�@ �@�@|�@]�@C�@)_@�@C�@@�@a�@�K@�u@~(@l"@>B@%�@�@{J@�@�}@h
@5?@�@�.@��@��@��@��@^�@ \@��@j@6@�+@�g@�@�@\)@Mj@@�@�s@�b@=q@
�@�3@��@L�@?}@<6@+�@��@�O@�I@�@��@��@oi@D�@�+@�[@��@�:@s@\)@RT@A�@4�@�@�h@�+@Ov@1�@�@4@u@��@��@��@��@�@�@�@�4@�.@�@C-@�@��@��@��@|�@W?@H�@;d@�@@��@��@��@��@s�@ff@M�@6�@@��@p�@+�@�@֡@�U@��@��@�4@6@�@�@�@7@ �@x@��@�Q@��@��@g�@8@�@
�"@
�@
��@
ff@	��@	^�@	A @	5�@	-w@	 \@	�@	%@�[@��@�9@��@~(@D�@,=@*�@%�@ �@�@  @�@�a@��@�@@��@�P@n/@H�@�@��@��@��@�}@�@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B,�B.B0�B4�B:�BBABKBIBN�BQ�BXBshB��B�bB�B�RB�B�"B��B�RB��B��B��B�B�'B�HB�B�<B�WB�PB��B��B��B�'B�jB��B��B�	B��B�B��B��B�dB�B��B��B��B��B�=B��B��B��B�~B�B��Bu?Be�BW?BD3B2-B!�B	7B�aB�B�4B�xBרB�	B�[B��Bg�BJrBI7B>BB8RB%�B"�B*eB;B�B�;B�B�B�TB��B��B�B��B��BwLBkQBZ7BN�BL~B;�B%`B �BCB�B{B�B �B
�BB
��B
�B
�B
�NB
��B
ѷB
ĜB
��B
�jB
��B
��B
�qB
�tB
�B
�,B
��B
v�B
p�B
c�B
X�B
MB
N�B
L�B
G�B
@iB
<B
-�B
�B	�<B	��B	�qB	�	B	�B	�RB	ƎB	��B	̘B	��B	�B	��B	��B	��B	��B	��B	�B	��B	�B	�oB	��B	�`B	�:B	�bB	�/B	��B	��B	�3B	�[B	��B	�OB	�4B	|PB	zxB	z^B	wB	y	B	u%B	n/B	mB	kQB	k�B	jKB	iDB	ffB	d�B	`\B	]�B	\�B	[=B	YB	R�B	N�B	K�B	KB	G�B	F�B	G_B	B'B	@�B	@4B	>(B	<B	;B	8�B	4TB	1�B	/5B	+�B	(�B	&�B	"�B	�B	�B	B	sB	?B	�B	�B	�B	B	�B	HB	�B		lB	�B	 B�B�B��B�>B��B�fB�fB�fB��B��B��B�9B�B�-B�B�B��B��B�B�B��B��B�B�B�mB�2B��B��B��B�&B�@B�B�nB�B��B�B�NB�B�B�\BߤB��B��BݲBچBیB��BٴB�KB�KB�yB�KB�yB��B��B�yB�EB�kBؓB�+B׍B�B�$B�B�?B׍BרB�KB��BؓBؓB�7B�B�pB��B�B�NB�NB��B�ZB��B�B��B�"B�]B�}B�IB��B�B�5B��B�B�8B��B�<B�"B��B	  B	 �B	B	oB	MB	zB		�B	xB	B	PB	�B	�B	�B	.B	�B	�B	�B	�B	B	�B	�B	�B	 B	#�B	$&B	$@B	$�B	%�B	%�B	)B	)DB	)B	*eB	+B	+�B	+�B	+kB	1AB	0�B	1[B	5?B	:^B	;�B	<6B	?}B	AUB	B�B	B'B	BAB	BB	C�B	DB	E�B	I�B	L~B	R�B	_�B	b4B	d&B	d�B	eFB	e�B	e�B	e�B	f�B	l"B	p�B	s�B	uB	v�B	z^B	|�B	}�B	�AB	�B	��B	�'B	�aB	�"B	�B	�TB	��B	�B	��B	��B	��B	�@B	��B	�B	��B	�ZB	��B	�^B	��B	�B	�wB	��B	�'B	�zB	̳B	��B	ңB	�,B	�sB	ٚB	�B	��B	�=B	�=B	یB	��B	�B	ބB	��B	��B	��B	��B	�>B	�*B	�B	��B	�)B	�B	�B	��B	�B	�TB	�TB	�%B	��B	�xB	�B	��B	��B	�VB
 iB
�B
�B
�B
	B

#B
�B
�B
�B
 B
&B
�B
sB
�B
�B
B
�B
�B
B
pB
#nB
$tB
%,B
%�B
'RB
(�B
)�B
,WB
.B
2-B
2�B
3�B
6+B
7LB
8�B
:�B
;�B
<6B
=�B
>]B
?�B
BAB
CB
G�B
I�B
J�B
K�B
L�B
M�B
N�B
P}B
Q4B
R�B
T{B
VB
Y�B
]IB
]IB
^5B
_�B
c:B
c�B
d�B
e�B
e�B
e�B
e�B
ffB
iDB
j0B
j�B
j�B
j�B
j�B
kQB
k�B
lB
m)B
n/B
o�B
pUB
qB
q�B
s3B
s�B
s�B
s�B
s�B
tnB
u?B
v+B
x8B
zDB
z�B
{dB
~wB
HB
}B
��B
� B
�uB
��B
��B
�B
��B
�%B
��B
��B
�B
��B
�	B
��B
�B
��B
�BB
�.B
��B
��B
��B
��B
�FB
��B
��B
��B
�$B
�$B
��B
��B
�7B
�kB
�7B
��B
��B
�xB
�VB
��B
��B
�4B
�nB
��B
�FB
��B
��B
��B
�2B
��B
�
B
�KB
�B
��B
�"B
��B
��B
�MB
�B
�MB
��B
��B
��B
�LB
��B
��B
��B
�RB
�$B
�^B
��B
�B
��B
�]B
��B
��B
� B
��B
��B
�B
��B
ĶB
ňB
�YB
�zB
��B
�B
�fB
�fB
�KB
��B
�#B
ʌB
�^B
̳B
�\B
�\B
�B
��B
�4B
ѝB
��B
��B
�oB
�TB
� B
��B
� B
�uB
�B
ԯB
��B
�2B
�MB
�MB
�2B
�MB
�B
��B
��B
�QB
�7B
�B
�7B
�QB
ڠB
�	B
��B
��B
�B
�CB
�CB
�xB
ܬB
��B
�VB
ߊB
ߤB
��B
��B
�vB
��B
��B
��B
�HB
�B
�|B
�B
�|B
�|B
�bB
�B
�B
�@B
�B
�B
�B
�`B
�B
��B
�B
�RB
�B
�mB
�B
�B
�B
��B
��B
�XB
�B
��B
��B
��B
��B
�B
�B
�B
��B
�=B
�WB
�WB
�B
��B
�wB
��B
�IB
�IB
�}B
�OB
�B
�;B
�B
�UB
��B
�B
�-B
��B
�B
�B
�B
�B
��B
�?B
�?B
�?B
��B
��B
��B
�FB
�FB
�FB
�zB
��B
�LB
�B
�B
�8B
�$B
�rB
��B
��B
�DB
�^B
��B
�B
�B
��B
��B
��B
��B
�B
�"B
��B
��B
��B
�.BBBAB�B�B�B�BB-BaB�B�B�BB�B�B9B�B�BEB�B�B�B�B�B�B�B�B�B1B�B�B�B�B�B�B�B	�B	�B	�B	�B	�B	�B	�B
�B^B^B�B�B�BdBdB�B�BB6BB6B�B�B�B�BHB�B�B�BB4BBTB�B&B�B�BB,BBBB�B�B2B�BBSB�B�B
B?BsB�B�BBB_B�B1B�BB7BQBQBkB#B=BqBqBqB�B�B�B�B�B�B�B/BIB~B~B~B�BjB�B�B�BB!B;B;B;B;B�B �B �B �B!B!-B!-B!�B"�B#B"�B# B#TB#�B#�B#�B#�B#�B$@B$tB$�B$�B$�B$�B$�B%B%FB%�B&LB&�B&�B'B'8B'mB'RB'8B($B(
B($B($B($B($B(>B(XB(sB(�B(�B)*B)_B)�B)�B)�B*B*�B+QB,=B,=B,=B,qB,qB,qB,�B-B-)B-]B-CB-�B.B.IB.IB.IB.IB.IB.�B.�B.�B/B/5B/OB/OB/�B/�B/�B/�B/�B/�B/�B0!B0!B0o44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444B,�B.B0�B4�B:�BBABKBIBN�BQ�BXBshB��B�bB�B�RB�B�"B��B�RB��B��B��B�B�'B�HB�B�<B�WB�PB��B��B��B�'B�jB��B��B�	B��B�B��B��B�dB�B��B��B��B��B�=B��B��B��B�~B�B��Bu?Be�BW?BD3B2-B!�B	7B�aB�B�4B�xBרB�	B�[B��Bg�BJrBI7B>BB8RB%�B"�B*eB;B�B�;B�B�B�TB��B��B�B��B��BwLBkQBZ7BN�BL~B;�B%`B �BCB�B{B�B �B
�BB
��B
�B
�B
�NB
��B
ѷB
ĜB
��B
�jB
��B
��B
�qB
�tB
�B
�,B
��B
v�B
p�B
c�B
X�B
MB
N�B
L�B
G�B
@iB
<B
-�B
�B	�<B	��B	�qB	�	B	�B	�RB	ƎB	��B	̘B	��B	�B	��B	��B	��B	��B	��B	�B	��B	�B	�oB	��B	�`B	�:B	�bB	�/B	��B	��B	�3B	�[B	��B	�OB	�4B	|PB	zxB	z^B	wB	y	B	u%B	n/B	mB	kQB	k�B	jKB	iDB	ffB	d�B	`\B	]�B	\�B	[=B	YB	R�B	N�B	K�B	KB	G�B	F�B	G_B	B'B	@�B	@4B	>(B	<B	;B	8�B	4TB	1�B	/5B	+�B	(�B	&�B	"�B	�B	�B	B	sB	?B	�B	�B	�B	B	�B	HB	�B		lB	�B	 B�B�B��B�>B��B�fB�fB�fB��B��B��B�9B�B�-B�B�B��B��B�B�B��B��B�B�B�mB�2B��B��B��B�&B�@B�B�nB�B��B�B�NB�B�B�\BߤB��B��BݲBچBیB��BٴB�KB�KB�yB�KB�yB��B��B�yB�EB�kBؓB�+B׍B�B�$B�B�?B׍BרB�KB��BؓBؓB�7B�B�pB��B�B�NB�NB��B�ZB��B�B��B�"B�]B�}B�IB��B�B�5B��B�B�8B��B�<B�"B��B	  B	 �B	B	oB	MB	zB		�B	xB	B	PB	�B	�B	�B	.B	�B	�B	�B	�B	B	�B	�B	�B	 B	#�B	$&B	$@B	$�B	%�B	%�B	)B	)DB	)B	*eB	+B	+�B	+�B	+kB	1AB	0�B	1[B	5?B	:^B	;�B	<6B	?}B	AUB	B�B	B'B	BAB	BB	C�B	DB	E�B	I�B	L~B	R�B	_�B	b4B	d&B	d�B	eFB	e�B	e�B	e�B	f�B	l"B	p�B	s�B	uB	v�B	z^B	|�B	}�B	�AB	�B	��B	�'B	�aB	�"B	�B	�TB	��B	�B	��B	��B	��B	�@B	��B	�B	��B	�ZB	��B	�^B	��B	�B	�wB	��B	�'B	�zB	̳B	��B	ңB	�,B	�sB	ٚB	�B	��B	�=B	�=B	یB	��B	�B	ބB	��B	��B	��B	��B	�>B	�*B	�B	��B	�)B	�B	�B	��B	�B	�TB	�TB	�%B	��B	�xB	�B	��B	��B	�VB
 iB
�B
�B
�B
	B

#B
�B
�B
�B
 B
&B
�B
sB
�B
�B
B
�B
�B
B
pB
#nB
$tB
%,B
%�B
'RB
(�B
)�B
,WB
.B
2-B
2�B
3�B
6+B
7LB
8�B
:�B
;�B
<6B
=�B
>]B
?�B
BAB
CB
G�B
I�B
J�B
K�B
L�B
M�B
N�B
P}B
Q4B
R�B
T{B
VB
Y�B
]IB
]IB
^5B
_�B
c:B
c�B
d�B
e�B
e�B
e�B
e�B
ffB
iDB
j0B
j�B
j�B
j�B
j�B
kQB
k�B
lB
m)B
n/B
o�B
pUB
qB
q�B
s3B
s�B
s�B
s�B
s�B
tnB
u?B
v+B
x8B
zDB
z�B
{dB
~wB
HB
}B
��B
� B
�uB
��B
��B
�B
��B
�%B
��B
��B
�B
��B
�	B
��B
�B
��B
�BB
�.B
��B
��B
��B
��B
�FB
��B
��B
��B
�$B
�$B
��B
��B
�7B
�kB
�7B
��B
��B
�xB
�VB
��B
��B
�4B
�nB
��B
�FB
��B
��B
��B
�2B
��B
�
B
�KB
�B
��B
�"B
��B
��B
�MB
�B
�MB
��B
��B
��B
�LB
��B
��B
��B
�RB
�$B
�^B
��B
�B
��B
�]B
��B
��B
� B
��B
��B
�B
��B
ĶB
ňB
�YB
�zB
��B
�B
�fB
�fB
�KB
��B
�#B
ʌB
�^B
̳B
�\B
�\B
�B
��B
�4B
ѝB
��B
��B
�oB
�TB
� B
��B
� B
�uB
�B
ԯB
��B
�2B
�MB
�MB
�2B
�MB
�B
��B
��B
�QB
�7B
�B
�7B
�QB
ڠB
�	B
��B
��B
�B
�CB
�CB
�xB
ܬB
��B
�VB
ߊB
ߤB
��B
��B
�vB
��B
��B
��B
�HB
�B
�|B
�B
�|B
�|B
�bB
�B
�B
�@B
�B
�B
�B
�`B
�B
��B
�B
�RB
�B
�mB
�B
�B
�B
��B
��B
�XB
�B
��B
��B
��B
��B
�B
�B
�B
��B
�=B
�WB
�WB
�B
��B
�wB
��B
�IB
�IB
�}B
�OB
�B
�;B
�B
�UB
��B
�B
�-B
��B
�B
�B
�B
�B
��B
�?B
�?B
�?B
��B
��B
��B
�FB
�FB
�FB
�zB
��B
�LB
�B
�B
�8B
�$B
�rB
��B
��B
�DB
�^B
��B
�B
�B
��B
��B
��B
��B
�B
�"B
��B
��B
��B
�.BBBAB�B�B�B�BB-BaB�B�B�BB�B�B9B�B�BEB�B�B�B�B�B�B�B�B�B1B�B�B�B�B�B�B�B	�B	�B	�B	�B	�B	�B	�B
�B^B^B�B�B�BdBdB�B�BB6BB6B�B�B�B�BHB�B�B�BB4BBTB�B&B�B�BB,BBBB�B�B2B�BBSB�B�B
B?BsB�B�BBB_B�B1B�BB7BQBQBkB#B=BqBqBqB�B�B�B�B�B�B�B/BIB~B~B~B�BjB�B�B�BB!B;B;B;B;B�B �B �B �B!B!-B!-B!�B"�B#B"�B# B#TB#�B#�B#�B#�B#�B$@B$tB$�B$�B$�B$�B$�B%B%FB%�B&LB&�B&�B'B'8B'mB'RB'8B($B(
B($B($B($B($B(>B(XB(sB(�B(�B)*B)_B)�B)�B)�B*B*�B+QB,=B,=B,=B,qB,qB,qB,�B-B-)B-]B-CB-�B.B.IB.IB.IB.IB.IB.�B.�B.�B/B/5B/OB/OB/�B/�B/�B/�B/�B/�B/�B0!B0!B0o44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230402124236  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230402124237  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230402124238  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230402124238                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230402124238  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230402124238  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230402125720                      G�O�G�O�G�O�                