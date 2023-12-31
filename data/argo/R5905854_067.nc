CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:56:38Z creation;2022-06-04T17:56:38Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604175638  20220610141506  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               CA   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�L�Q���1   @�L�Ɗ�@0����m�b�ě��T1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @&ff@�ff@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B���B�  B���B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�33B�33Bߙ�B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C33C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C)�fC+�fC-�fC/�fC2  C4  C6  C8  C:�C<�C>  C@  CB  CD  CF  CH  CJ  CL  CM�fCP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl33Cm�fCo�fCq�fCt  Cv  Cw�fCy�fC|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw��Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D̼�D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @'�@�
=@�p�A Q�A Q�A@Q�A`Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B@{BH{BP{BX{B`{Bh{Bp{Bx{B�
=B�
=B�
=B�
=B�
=B�=pB��
B�
=B��
B�
=B��
B���B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�=pB�
=B�=pB�=pBߣ�B��
B�
=B�
=B�
=B�
=B�
=B�
=C CCCCC
CCC8RC�CCCCCCC C"C$C&C(C)�C+�C-�C/�C2C4C6C8C:�C<�C>C@CBCDCFCHCJCLCM�CPCRCTCVCXCZC\C^C`CbCdCfChCjCl8RCm�Co�Cq�CtCvCw�Cy�C|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDw��Dx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�C�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D̽qD� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�@�D怤D���D� �D�@�D瀤D���D� �D�@�D耤D���D��D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�}q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�,�A�,A�/�A�6zA�A�A�B�A�I�A�HKA�H�A�GzA�;0A��A�YA��GA��EA��gAλdAδ9A΢4AΟ�AΠ�AΡ�AΦAΨ�Aά�Aέ�Aή}Aί�Aΰ�AίOAίAέ�AΫ�AΫkAΪ�AΫAάA�oAÂA��A��mA���A��/A���A�zDA���A�\]A�AA�|�A���A���A�LdA�e,A���A�S�A�P�A�ffA�u%A�ĜA��kA�%�A��A���A�<6A��,A��A�VA�Z�A�A��A��A���A�;A�֡A�]�A�ߤA�8�A��A���A�v�A��A�`vA�($A���A��tA��<A��A���A�p;A�K)A� �A��A�ѷA�l"A���A��A���A�A|��AzcAy�Awh
Au�:Ao~�Ai;Ag=�AfZAa�AXG�AVO�AS�AO0�AK{AH�uAE��AD�`AD	AB|�A??�A;,=A:�A:>�A9��A;�A<��A<u%A<qvA;�A;aA:A7�'A7b�A7	A6�RA5��A5"hA6�A6�A5��A4�-A23�A0�A0��A0��A/�HA/�_A.XyA-	lA-H�A-MA,��A,zA+��A+"hA*֡A*�nA)��A(qvA'�ZA&�A&-wA&;A%�/A%A�A$*0A!VA�A�A*0A�hA&�A�Az�AA�A?�AVAk�A�^A"hA$tA��A�A�DAJ�A�YA4nA��A��A�yA��AuA�=A?A�A%A�9A<�Am]Aw�A�4Ab�A�A	kQA	/�A	U�A;dA�A��A��A��A/�A��A��A��Ap;A9XAAzxA�A��A��A��A�A͟Af�A�A��A��AqvAz�A�uA^5A"�A�hAG�@�g�@���@�@�@��C@��D@�-w@�n/@��@�c�@�z@�l�@�8�@���@���@��O@�$@�z@�k@��@���@��@��W@�9X@�]�@�֡@�>B@��@炪@��@�@栐@�E�@���@��@��@�w�@��r@㫟@�{J@�Dg@���@�@�}@�@ᴢ@��@᯸@��@�|�@�<@�6z@�7@��]@�8�@�ں@܉�@�xl@��X@��@ܒ�@ܑ�@�7�@��#@ۂ�@�n/@�b�@��@ڱ�@�%�@ً�@��K@ص@�_@�	�@�T�@�Y@��s@�I�@՝�@��@�bN@�خ@�@O@ң@�Q�@�1@���@ї$@��@��@���@Ϥ@@���@�֡@ΰ�@��@���@�7L@̧@�1'@�x�@�J�@ʳh@�s@��P@��U@�S�@�.I@�1@�m]@��@ĂA@�d�@�G@�|�@�@���@§�@�c�@���@��@�.�@��@��K@���@��y@���@�~�@�l�@�e,@�&@��@���@�:*@���@�4�@���@�3�@�B�@���@�N�@��@���@�x�@�T�@���@�?@�n/@���@�~�@�[�@�@�@�{@���@�hs@�6z@��]@��@�oi@�H@���@���@���@�.I@���@��]@���@���@�@��@��I@�_@�$@��@���@��H@�n/@���@��x@��@�S�@��@��D@��@�ݘ@���@��X@�[W@��@��"@��@�H�@��#@�|�@��@��@���@�8�@�@���@��S@�.I@���@�q�@�#:@��^@�{J@�G�@��P@��h@�u%@�E�@���@��@@�`B@�"�@�;@���@���@���@�S�@�	�@�x@���@���@�K^@��@���@�`B@�P�@�!�@��B@��e@���@�p;@�($@���@��n@�X�@���@���@��o@�"h@���@���@�Y�@���@�K^@�e@��@�o @�@O@��@��@���@�E�@��]@��@�v`@� \@�ѷ@��b@��A@�8�@��#@��7@�dZ@��@���@��@��_@�`�@�b@���@�e�@�S&@�Q�@�?}@��/@���@�V�@���@���@���@��"@�iD@���@��L@��r@�(�@���@�s�@�1�@��@��@� i@���@��@��x@�@�@�J@���@�g�@�6z@��c@�ȴ@���@��1@�R�@��@���@�v`@�&�@���@���@���@�`�@�.�@��@��0@�~�@�Q�@���@�:*@���@�a@�6z@�4@��@��U@�ȴ@���@�{�@�#:@��9@���@��h@�a�@�RT@�>�@��@���@���@��}@���@�?�@�˒@��k@��@�n/@�C�@��@�ѷ@��u@�$@��g@���@��^@��'@�]�@��+@�6@�%�@ƨ@~�@}�@}*0@|��@|-�@{��@{6z@zߤ@z\�@y�@y4@x��@xی@x�e@x!@w�;@w�:@wH�@v@�@uVm@t �@s�[@s�@@s!-@r�@r��@r}V@q�>@p�Y@p-�@p2�@p$@o�g@n�2@nc @n�@m��@m�"@m%F@l�@l��@lu�@lM@l  @k��@k!-@j�b@jC�@j6�@j�@i��@iq@hFt@h7@h�@g�;@g�K@g��@gv`@gH�@f�@fxl@f�@e��@eu�@e\�@d�P@d��@dS�@dG@c�@c�@b�B@b��@bTa@a�N@a��@aw2@aa�@a0�@`ѷ@`Xy@_�;@_��@_4�@^�L@^1�@]�@]c�@]�@\�@[�w@[��@[e�@Z��@Z@�@Y�j@Y*0@Xh�@X�@W�V@V��@Vh
@U�.@U�@U��@U5�@T��@T�@T�@S�W@S�	@R�<@RTa@R#:@Q�Z@Q��@QS&@P�K@Pq@P�@Og�@N��@N҉@N��@NW�@N�@M�@MN<@L�_@Lg8@K�@K;d@K�@J�@J��@J��@J��@Jz@J)�@I��@I%@H��@H��@H@Gt�@GC@F��@FGE@F_@F_@E�.@E�@Ej@E=�@E�@D��@D �@C�F@CRT@B��@B��@B}V@BOv@A�@Ao @@�K@@y>@@4n@@�@?˒@?>�@>ں@>�1@=�)@=`B@<�5@<:�@;˒@;�	@;+@:�@:�r@:H�@:{@9�.@9�D@9�9@9�@97L@8Ĝ@8c�@7�
@7t�@7F�@6��@6�x@6e@6@5�@5��@5w2@5e,@5[W@5L�@5=�@4�v@4tT@4PH@47�@3�W@3�*@3s@3Z�@3�@2�B@2�L@2xl@21�@1�-@1�@0�@0�4@0�Y@0[�@/��@/J#@/�@/S@.��@.�M@.�H@.�m@.u%@.Q@.	@-��@-rG@-%@,��@,  @+��@+��@+dZ@+@O@*�s@*�@*kQ@)�@)�H@)o @)+�@(�/@(��@(Ft@'ݘ@'�F@'�{@'Z�@'�@&�1@&�@%�'@%Y�@%8�@$��@$e�@$%�@#��@#t�@#H�@#)_@#o@"��@!�.@!��@!`B@!*0@!�@ ��@ 6@ 	�@��@�a@O@҉@��@��@.�@��@��@��@�h@w2@*0@%@�@��@I�@�+@�Q@��@�6@�@�*@��@��@�P@x@C�@4�@o@ȴ@�6@u%@=q@_@��@��@�-@�7@?}@�/@�?@�U@�j@��@��@�I@�D@l"@2�@�r@خ@]�@�@�F@^5@a|@c @E�@($@	@�d@u�@0�@�@��@�@�@ѷ@�j@�I@��@�@�@�u@��@oi@]d@Q�@,=@�+@�F@,�@�M@�@�M@�@��@�r@^5@�@��@��@��@�'@`B@=�@4@!�@�@;@ѷ@�$@�4@e�@�@��@��@H�@A�@;d@!-@�@��@�'@��@B[@��@}�@m]@e,@Y�@Vm@+�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�,�A�,A�/�A�6zA�A�A�B�A�I�A�HKA�H�A�GzA�;0A��A�YA��GA��EA��gAλdAδ9A΢4AΟ�AΠ�AΡ�AΦAΨ�Aά�Aέ�Aή}Aί�Aΰ�AίOAίAέ�AΫ�AΫkAΪ�AΫAάA�oAÂA��A��mA���A��/A���A�zDA���A�\]A�AA�|�A���A���A�LdA�e,A���A�S�A�P�A�ffA�u%A�ĜA��kA�%�A��A���A�<6A��,A��A�VA�Z�A�A��A��A���A�;A�֡A�]�A�ߤA�8�A��A���A�v�A��A�`vA�($A���A��tA��<A��A���A�p;A�K)A� �A��A�ѷA�l"A���A��A���A�A|��AzcAy�Awh
Au�:Ao~�Ai;Ag=�AfZAa�AXG�AVO�AS�AO0�AK{AH�uAE��AD�`AD	AB|�A??�A;,=A:�A:>�A9��A;�A<��A<u%A<qvA;�A;aA:A7�'A7b�A7	A6�RA5��A5"hA6�A6�A5��A4�-A23�A0�A0��A0��A/�HA/�_A.XyA-	lA-H�A-MA,��A,zA+��A+"hA*֡A*�nA)��A(qvA'�ZA&�A&-wA&;A%�/A%A�A$*0A!VA�A�A*0A�hA&�A�Az�AA�A?�AVAk�A�^A"hA$tA��A�A�DAJ�A�YA4nA��A��A�yA��AuA�=A?A�A%A�9A<�Am]Aw�A�4Ab�A�A	kQA	/�A	U�A;dA�A��A��A��A/�A��A��A��Ap;A9XAAzxA�A��A��A��A�A͟Af�A�A��A��AqvAz�A�uA^5A"�A�hAG�@�g�@���@�@�@��C@��D@�-w@�n/@��@�c�@�z@�l�@�8�@���@���@��O@�$@�z@�k@��@���@��@��W@�9X@�]�@�֡@�>B@��@炪@��@�@栐@�E�@���@��@��@�w�@��r@㫟@�{J@�Dg@���@�@�}@�@ᴢ@��@᯸@��@�|�@�<@�6z@�7@��]@�8�@�ں@܉�@�xl@��X@��@ܒ�@ܑ�@�7�@��#@ۂ�@�n/@�b�@��@ڱ�@�%�@ً�@��K@ص@�_@�	�@�T�@�Y@��s@�I�@՝�@��@�bN@�خ@�@O@ң@�Q�@�1@���@ї$@��@��@���@Ϥ@@���@�֡@ΰ�@��@���@�7L@̧@�1'@�x�@�J�@ʳh@�s@��P@��U@�S�@�.I@�1@�m]@��@ĂA@�d�@�G@�|�@�@���@§�@�c�@���@��@�.�@��@��K@���@��y@���@�~�@�l�@�e,@�&@��@���@�:*@���@�4�@���@�3�@�B�@���@�N�@��@���@�x�@�T�@���@�?@�n/@���@�~�@�[�@�@�@�{@���@�hs@�6z@��]@��@�oi@�H@���@���@���@�.I@���@��]@���@���@�@��@��I@�_@�$@��@���@��H@�n/@���@��x@��@�S�@��@��D@��@�ݘ@���@��X@�[W@��@��"@��@�H�@��#@�|�@��@��@���@�8�@�@���@��S@�.I@���@�q�@�#:@��^@�{J@�G�@��P@��h@�u%@�E�@���@��@@�`B@�"�@�;@���@���@���@�S�@�	�@�x@���@���@�K^@��@���@�`B@�P�@�!�@��B@��e@���@�p;@�($@���@��n@�X�@���@���@��o@�"h@���@���@�Y�@���@�K^@�e@��@�o @�@O@��@��@���@�E�@��]@��@�v`@� \@�ѷ@��b@��A@�8�@��#@��7@�dZ@��@���@��@��_@�`�@�b@���@�e�@�S&@�Q�@�?}@��/@���@�V�@���@���@���@��"@�iD@���@��L@��r@�(�@���@�s�@�1�@��@��@� i@���@��@��x@�@�@�J@���@�g�@�6z@��c@�ȴ@���@��1@�R�@��@���@�v`@�&�@���@���@���@�`�@�.�@��@��0@�~�@�Q�@���@�:*@���@�a@�6z@�4@��@��U@�ȴ@���@�{�@�#:@��9@���@��h@�a�@�RT@�>�@��@���@���@��}@���@�?�@�˒@��k@��@�n/@�C�@��@�ѷ@��u@�$@��g@���@��^@��'@�]�@��+@�6@�%�@ƨ@~�@}�@}*0@|��@|-�@{��@{6z@zߤ@z\�@y�@y4@x��@xی@x�e@x!@w�;@w�:@wH�@v@�@uVm@t �@s�[@s�@@s!-@r�@r��@r}V@q�>@p�Y@p-�@p2�@p$@o�g@n�2@nc @n�@m��@m�"@m%F@l�@l��@lu�@lM@l  @k��@k!-@j�b@jC�@j6�@j�@i��@iq@hFt@h7@h�@g�;@g�K@g��@gv`@gH�@f�@fxl@f�@e��@eu�@e\�@d�P@d��@dS�@dG@c�@c�@b�B@b��@bTa@a�N@a��@aw2@aa�@a0�@`ѷ@`Xy@_�;@_��@_4�@^�L@^1�@]�@]c�@]�@\�@[�w@[��@[e�@Z��@Z@�@Y�j@Y*0@Xh�@X�@W�V@V��@Vh
@U�.@U�@U��@U5�@T��@T�@T�@S�W@S�	@R�<@RTa@R#:@Q�Z@Q��@QS&@P�K@Pq@P�@Og�@N��@N҉@N��@NW�@N�@M�@MN<@L�_@Lg8@K�@K;d@K�@J�@J��@J��@J��@Jz@J)�@I��@I%@H��@H��@H@Gt�@GC@F��@FGE@F_@F_@E�.@E�@Ej@E=�@E�@D��@D �@C�F@CRT@B��@B��@B}V@BOv@A�@Ao @@�K@@y>@@4n@@�@?˒@?>�@>ں@>�1@=�)@=`B@<�5@<:�@;˒@;�	@;+@:�@:�r@:H�@:{@9�.@9�D@9�9@9�@97L@8Ĝ@8c�@7�
@7t�@7F�@6��@6�x@6e@6@5�@5��@5w2@5e,@5[W@5L�@5=�@4�v@4tT@4PH@47�@3�W@3�*@3s@3Z�@3�@2�B@2�L@2xl@21�@1�-@1�@0�@0�4@0�Y@0[�@/��@/J#@/�@/S@.��@.�M@.�H@.�m@.u%@.Q@.	@-��@-rG@-%@,��@,  @+��@+��@+dZ@+@O@*�s@*�@*kQ@)�@)�H@)o @)+�@(�/@(��@(Ft@'ݘ@'�F@'�{@'Z�@'�@&�1@&�@%�'@%Y�@%8�@$��@$e�@$%�@#��@#t�@#H�@#)_@#o@"��@!�.@!��@!`B@!*0@!�@ ��@ 6@ 	�@��@�a@O@҉@��@��@.�@��@��@��@�h@w2@*0@%@�@��@I�@�+@�Q@��@�6@�@�*@��@��@�P@x@C�@4�@o@ȴ@�6@u%@=q@_@��@��@�-@�7@?}@�/@�?@�U@�j@��@��@�I@�D@l"@2�@�r@خ@]�@�@�F@^5@a|@c @E�@($@	@�d@u�@0�@�@��@�@�@ѷ@�j@�I@��@�@�@�u@��@oi@]d@Q�@,=@�+@�F@,�@�M@�@�M@�@��@�r@^5@�@��@��@��@�'@`B@=�@4@!�@�@;@ѷ@�$@�4@e�@�@��@��@H�@A�@;d@!-@�@��@�'@��@B[@��@}�@m]@e,@Y�@Vm@+�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	�hB	�hB	��B	��B	�B	�3B	�B	�B	�*B

	B
#�B
1�B
;0B
GzB
R�B
W�B
]/B
k�B
qAB
qvB
r�B
t�B
u�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
v�B
MjB
A B
>]B
=B
4�B
:�B
MB
U�B
i�B
o�B
y�B
�0B
��B
�+B
�IB
�B
��B?BB-�B7fB:B?�B?cBAUBF�BI�BQNBU�BT�BT�BS�BKDB?}BC�BCBIlBF�BGEBL�BJ�BDgBBB=�B2�B,�B_B
��B
�GB
��B
��B
y>B
g�B
K�B
2aB
�B
�B
�B	�LB	��B	ּB	�lB	�.B	��B	��B	�YB	_�B	S�B	K^B	6`B	
rB�HB��B�BϑB�+B��B�6B�wB�B�%B�BB�6B�8B�eB	�B	[�B	o�B	|B	�B	�B	��B	��B	��B	�:B	�kB	��B	�B	�QB	��B	��B	�xB	��B	�B	�+B
�B
�B
�B
;B	�HB
KB
%�B
&LB
'B
!B
�B
B
�B
�B
 B
<B

�B
�B
PB
JB
KB	�cB	��B	յB	� B	οB	̳B	�\B	՛B	ևB	��B	�9B	ңB	�(B	��B	�pB	�:B	ԯB	��B	бB	�HB	уB	҉B	�B	бB	ΥB	�lB	�_B	��B	ɆB	��B	ɆB	��B	�%B	��B	ÖB	�+B	�%B	�B	�oB	�}B	�B	��B	��B	�qB	̘B	ˬB	ʌB	�KB	�KB	�B	�B	��B	�+B	�EB	�_B	��B	ɠB	��B	�XB	�fB	�YB	ðB	ƨB	�dB	�#B	��B	��B	ΥB	�6B	��B	ˬB	�mB	�;B	��B	�B	��B	�jB	��B	�4B	��B	�B	�zB	�3B	��B	�B	�zB	˒B	żB	��B	�zB	�8B	��B	��B	��B	�B	�UB	��B	�[B	��B	��B	�B	�fB	��B	�LB	��B	�B	�	B	��B	��B	�B	��B	�<B	�HB	ªB	��B	ǮB	ȚB	��B	�JB	ˬB	ɆB	�1B	�{B	��B	�B	�B	��B	� B	żB	�B	ɆB	��B	ЗB	�&B	յB	�mB	ևB	ؓB	�B	��B	�IB	��B	�B	޸B	޸B	�pB	�VB	�!B	�;B	�!B	��B	�!B	ߊB	�B	�'B	�BB	��B	��B	�HB	��B	��B	�bB	�|B	�NB	��B	�B	�4B	��B	�TB	� B	� B	�B	� B	��B	�B	�@B	�@B	�&B	�FB	�B	��B	�B	�B	��B	�B	��B	�B	�B	�B	�B	�DB	�KB	�KB	�0B	�eB	�B	�B	�B	��B	�B	�B	�UB	��B	��B	�vB	�aB	�B	�B	�MB	�B	��B	��B	��B	�zB	��B	�B	�2B	��B	�8B	��B	��B	��B	��B	�	B	�rB	��B	��B	��B	�B	�B	�dB	�B	��B	��B	�0B	��B	��B	�B	��B	�6B	�6B	�B	��B	��B	��B	�B	��B	�qB	��B	�BB	�BB	��B	��B	��B	�B	�HB	�}B	�}B	��B	�.B	�B	�HB
  B
 4B
 �B
 B
UB
oB
B
'B
'B
�B
{B
B
MB
gB
�B
B
SB
�B
�B
�B
�B
tB
tB
�B
�B
�B
�B
�B
�B
�B
�B
EB
�B
�B
�B
EB
�B
�B
+B
�B
�B
B
�B
	7B
	7B
	B
	RB
	�B

rB

�B

rB

�B
�B
�B
�B
xB
�B
B
�B
pB
�B
B
�B
�B
�B
B
�B
\B
.B
HB
.B
.B
�B
�B
 B
:B
TB
�B
�B
B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
$B

B
?B
�B
_B
EB
�B
EB
�B
�B
�B
�B
�B
YB
?B
YB
sB
sB
�B
B
_B
�B
B
B
1B
�B
�B
�B
WB
�B
�B
xB
/B
�B
�B
dB
dB
IB
�B
�B
)B
�B
�B
OB
�B
 \B
 vB
!�B
"�B
#B
#�B
$&B
$B
$@B
$�B
$�B
$�B
%,B
%�B
%�B
%�B
%�B
&LB
&�B
'B
'8B
'8B
'�B
'�B
(
B
(XB
(�B
(XB
(sB
(�B
(�B
)B
)�B
)�B
)�B
*�B
+�B
,"B
,"B
,�B
-CB
-wB
-�B
-�B
.}B
.�B
.�B
.�B
.�B
.�B
/ B
.�B
.}B
./B
./B
-CB
,WB
+�B
+kB
+QB
*�B
+6B
+�B
+6B
*�B
)�B
*B
+B
+B
*�B
*�B
*�B
*B
*�B
*�B
*�B
*�B
*�B
+B
+6B
+�B
,=B
,�B
,�B
,�B
,�B
-CB
-�B
.cB
.cB
.cB
.�B
.�B
.�B
/ B
.�B
/iB
/�B
/�B
0oB
0!B
0UB
0�B
1'B
1[B
1�B
2B
2aB
2�B
2�B
2�B
3�B
3�B
3�B
3�B
3�B
4TB
4�B
5?B
5�B
6FB
6�B
6�B
7fB
8B
7�B
9$B
9	B
9	B
9>B
9�B
9�B
9�B
:DB
;0B
;�B
;�B
<jB
<�B
<�B
=<B
=<B
=�B
=�B
=�B
>]B
>BB
>wB
?.B
?}B
?�B
?�B
?�B
@B
@�B
@�B
AUB
A�B
B[B
B[B
B[B
B�B
B�B
C-B
C�B
DMB
D3B
D�B
E�B
E�B
E�B
E�B
E�B
F%B
F?B
FtB
F�B
GzB
G�B
G�B
HKB
H�B
I7B
I�B
J	B
J#B
J#B
J	B
JrB
J�B
J�B
J�B
KDB
K�B
LB
L~B
L�B
MB
MB
MB
M�B
M�B
NVB
N�B
N�B
OB
OBB
O�B
PB
P.B
P�B
QNB
Q�B
R�B
R�B
R�B
S[B
S�B
S�B
TB
T,B
TB
TB
T,B
T{B
T�B
UB
UMB
U�B
VB
VSB
V�B
W
B
W�B
W�B
W�B
XB
XEB
XEB
XEB
XEB
X+B
X�B
YB
Y1B
Y1B
Y�B
Y�B
Y�B
ZB
ZQB
Z�B
Z�B
Z�B
Z�B
[qB
\)B
\)B
\xB
\]B
\�B
]IB
]dB
]�B
]�B
]�B
]�B
]�B
^B
^OB
^OB
^�B
^�B
^�B
_;B
_�B
`\B
`�B
`�B
`�B
`�B
aHB
abB
abB
a�B
a�B
b4B
bNB
b�B
b�B
c B
c�B
c�B
c�B
c�B
d@B
d�B
eB
e�B
e�B
e�B
fLB
f�B
f�B
gRB
g�B
g�B
g�B
g�B
h>B
hsB
h�B
h�B
iDB
iDB
iyB
i�B
j0B
jKB
jeB
j�B
k6B
k6B
k6B
k�B
k�B
l=B
lWB
lWB
lqB
l�B
l�B
l�B
m]B
m�B
nB
nB
n/B
n/B
n/B
nIB
nIB
ncB
ncB
ncB
n�B
n�B
n�B
oB
oB
oiB
o�B
o�B
p!B
p!B
p!B
p;B
p�B
qB
qB
q'B
q'B
q'B
q'B
qAB
q[B
q[B
q�B
rB
q�B
r�B
sB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t9B
t�B
t�B
t�B
uB
u%B
u%B
u?B
uZB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v+B
v`B
v�B
w2B
wLB
wLB
wLB
wLB
w�B
w�B
w�B
xB
x8B
x8B
xlB
xlB
x�B
x�B
x�B
x�B
x�B
y	B
yXB
yXB
yXB
yrB
y�B
y�B
zDB
zxB
z�B
z�B
z�B
z�B
z�B
z�B
{B
{dB
{�B
|PB
|PB
|jB
|PB
|PB
|�B
|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	�hB	�hB	��B	��B	�B	�3B	�B	�B	�*B

	B
#�B
1�B
;0B
GzB
R�B
W�B
]/B
k�B
qAB
qvB
r�B
t�B
u�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
v�B
MjB
A B
>]B
=B
4�B
:�B
MB
U�B
i�B
o�B
y�B
�0B
��B
�+B
�IB
�B
��B?BB-�B7fB:B?�B?cBAUBF�BI�BQNBU�BT�BT�BS�BKDB?}BC�BCBIlBF�BGEBL�BJ�BDgBBB=�B2�B,�B_B
��B
�GB
��B
��B
y>B
g�B
K�B
2aB
�B
�B
�B	�LB	��B	ּB	�lB	�.B	��B	��B	�YB	_�B	S�B	K^B	6`B	
rB�HB��B�BϑB�+B��B�6B�wB�B�%B�BB�6B�8B�eB	�B	[�B	o�B	|B	�B	�B	��B	��B	��B	�:B	�kB	��B	�B	�QB	��B	��B	�xB	��B	�B	�+B
�B
�B
�B
;B	�HB
KB
%�B
&LB
'B
!B
�B
B
�B
�B
 B
<B

�B
�B
PB
JB
KB	�cB	��B	յB	� B	οB	̳B	�\B	՛B	ևB	��B	�9B	ңB	�(B	��B	�pB	�:B	ԯB	��B	бB	�HB	уB	҉B	�B	бB	ΥB	�lB	�_B	��B	ɆB	��B	ɆB	��B	�%B	��B	ÖB	�+B	�%B	�B	�oB	�}B	�B	��B	��B	�qB	̘B	ˬB	ʌB	�KB	�KB	�B	�B	��B	�+B	�EB	�_B	��B	ɠB	��B	�XB	�fB	�YB	ðB	ƨB	�dB	�#B	��B	��B	ΥB	�6B	��B	ˬB	�mB	�;B	��B	�B	��B	�jB	��B	�4B	��B	�B	�zB	�3B	��B	�B	�zB	˒B	żB	��B	�zB	�8B	��B	��B	��B	�B	�UB	��B	�[B	��B	��B	�B	�fB	��B	�LB	��B	�B	�	B	��B	��B	�B	��B	�<B	�HB	ªB	��B	ǮB	ȚB	��B	�JB	ˬB	ɆB	�1B	�{B	��B	�B	�B	��B	� B	żB	�B	ɆB	��B	ЗB	�&B	յB	�mB	ևB	ؓB	�B	��B	�IB	��B	�B	޸B	޸B	�pB	�VB	�!B	�;B	�!B	��B	�!B	ߊB	�B	�'B	�BB	��B	��B	�HB	��B	��B	�bB	�|B	�NB	��B	�B	�4B	��B	�TB	� B	� B	�B	� B	��B	�B	�@B	�@B	�&B	�FB	�B	��B	�B	�B	��B	�B	��B	�B	�B	�B	�B	�DB	�KB	�KB	�0B	�eB	�B	�B	�B	��B	�B	�B	�UB	��B	��B	�vB	�aB	�B	�B	�MB	�B	��B	��B	��B	�zB	��B	�B	�2B	��B	�8B	��B	��B	��B	��B	�	B	�rB	��B	��B	��B	�B	�B	�dB	�B	��B	��B	�0B	��B	��B	�B	��B	�6B	�6B	�B	��B	��B	��B	�B	��B	�qB	��B	�BB	�BB	��B	��B	��B	�B	�HB	�}B	�}B	��B	�.B	�B	�HB
  B
 4B
 �B
 B
UB
oB
B
'B
'B
�B
{B
B
MB
gB
�B
B
SB
�B
�B
�B
�B
tB
tB
�B
�B
�B
�B
�B
�B
�B
�B
EB
�B
�B
�B
EB
�B
�B
+B
�B
�B
B
�B
	7B
	7B
	B
	RB
	�B

rB

�B

rB

�B
�B
�B
�B
xB
�B
B
�B
pB
�B
B
�B
�B
�B
B
�B
\B
.B
HB
.B
.B
�B
�B
 B
:B
TB
�B
�B
B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
$B

B
?B
�B
_B
EB
�B
EB
�B
�B
�B
�B
�B
YB
?B
YB
sB
sB
�B
B
_B
�B
B
B
1B
�B
�B
�B
WB
�B
�B
xB
/B
�B
�B
dB
dB
IB
�B
�B
)B
�B
�B
OB
�B
 \B
 vB
!�B
"�B
#B
#�B
$&B
$B
$@B
$�B
$�B
$�B
%,B
%�B
%�B
%�B
%�B
&LB
&�B
'B
'8B
'8B
'�B
'�B
(
B
(XB
(�B
(XB
(sB
(�B
(�B
)B
)�B
)�B
)�B
*�B
+�B
,"B
,"B
,�B
-CB
-wB
-�B
-�B
.}B
.�B
.�B
.�B
.�B
.�B
/ B
.�B
.}B
./B
./B
-CB
,WB
+�B
+kB
+QB
*�B
+6B
+�B
+6B
*�B
)�B
*B
+B
+B
*�B
*�B
*�B
*B
*�B
*�B
*�B
*�B
*�B
+B
+6B
+�B
,=B
,�B
,�B
,�B
,�B
-CB
-�B
.cB
.cB
.cB
.�B
.�B
.�B
/ B
.�B
/iB
/�B
/�B
0oB
0!B
0UB
0�B
1'B
1[B
1�B
2B
2aB
2�B
2�B
2�B
3�B
3�B
3�B
3�B
3�B
4TB
4�B
5?B
5�B
6FB
6�B
6�B
7fB
8B
7�B
9$B
9	B
9	B
9>B
9�B
9�B
9�B
:DB
;0B
;�B
;�B
<jB
<�B
<�B
=<B
=<B
=�B
=�B
=�B
>]B
>BB
>wB
?.B
?}B
?�B
?�B
?�B
@B
@�B
@�B
AUB
A�B
B[B
B[B
B[B
B�B
B�B
C-B
C�B
DMB
D3B
D�B
E�B
E�B
E�B
E�B
E�B
F%B
F?B
FtB
F�B
GzB
G�B
G�B
HKB
H�B
I7B
I�B
J	B
J#B
J#B
J	B
JrB
J�B
J�B
J�B
KDB
K�B
LB
L~B
L�B
MB
MB
MB
M�B
M�B
NVB
N�B
N�B
OB
OBB
O�B
PB
P.B
P�B
QNB
Q�B
R�B
R�B
R�B
S[B
S�B
S�B
TB
T,B
TB
TB
T,B
T{B
T�B
UB
UMB
U�B
VB
VSB
V�B
W
B
W�B
W�B
W�B
XB
XEB
XEB
XEB
XEB
X+B
X�B
YB
Y1B
Y1B
Y�B
Y�B
Y�B
ZB
ZQB
Z�B
Z�B
Z�B
Z�B
[qB
\)B
\)B
\xB
\]B
\�B
]IB
]dB
]�B
]�B
]�B
]�B
]�B
^B
^OB
^OB
^�B
^�B
^�B
_;B
_�B
`\B
`�B
`�B
`�B
`�B
aHB
abB
abB
a�B
a�B
b4B
bNB
b�B
b�B
c B
c�B
c�B
c�B
c�B
d@B
d�B
eB
e�B
e�B
e�B
fLB
f�B
f�B
gRB
g�B
g�B
g�B
g�B
h>B
hsB
h�B
h�B
iDB
iDB
iyB
i�B
j0B
jKB
jeB
j�B
k6B
k6B
k6B
k�B
k�B
l=B
lWB
lWB
lqB
l�B
l�B
l�B
m]B
m�B
nB
nB
n/B
n/B
n/B
nIB
nIB
ncB
ncB
ncB
n�B
n�B
n�B
oB
oB
oiB
o�B
o�B
p!B
p!B
p!B
p;B
p�B
qB
qB
q'B
q'B
q'B
q'B
qAB
q[B
q[B
q�B
rB
q�B
r�B
sB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t9B
t�B
t�B
t�B
uB
u%B
u%B
u?B
uZB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v+B
v`B
v�B
w2B
wLB
wLB
wLB
wLB
w�B
w�B
w�B
xB
x8B
x8B
xlB
xlB
x�B
x�B
x�B
x�B
x�B
y	B
yXB
yXB
yXB
yrB
y�B
y�B
zDB
zxB
z�B
z�B
z�B
z�B
z�B
z�B
{B
{dB
{�B
|PB
|PB
|jB
|PB
|PB
|�B
|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105002  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175638  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175638  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175638                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025646  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025646  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141506                      G�O�G�O�G�O�                