CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:26:53Z creation;2022-06-04T17:26:54Z conversion to V3.1      
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
_FillValue                 �  I(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pP   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t<   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �L   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �P   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �T   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �X   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �\   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220604172653  20220610131506  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @����N��1   @����?V@.�bM���cCC��%1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBO��BX  B`  Bh  Bp  Bz  B}��B�  B�  B�  B�  B�  B�  B�  B���B���B���B�  B�33B�ffB���B���B�  B�  B�  B�  B�  B�  B�  Bۙ�B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6�C7�fC9�fC<  C>  C@  CB  CD  CF  CH  CJ  CK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  D   D � D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&fD&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�C3DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�ff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�H@���@���A Q�A Q�A>�RA`Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B@{BHz�BO�BX{B`{Bh{Bp{Bz{B}�B�
=B�
=B�
=B�
=B�
=B�
=B�
=B��
B��
B��
B�
=B�=pB�p�B���B��
B�
=B�
=B�
=B�
=B�
=B�
=B�
=Bۣ�B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=C CCCCC
CCC�C�CCCCCCC C"C$C&C(C*C,C.C0C2C4�C6�C7�C9�C<C>C@CBCDCFCHCJCK�CNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C���C��C��D HD �HDHD�HD�D�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&�D&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�C�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�@�D怤D���D� �D�@�D��D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�g
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A�wAⲖA�A⵨A�nA�r�A��A�+A�҉A��@A��A�\�A޾�A޸�A�!�A���A�^�A�B'A��A���Aܪ�A۔{A�M6Aד�AӁA�!�A���A��AʰUA�xA���A���A�~(A��Aŋ�A�z�A�B�A�m�A�=qA��A� \A��0A��DA���A���A�AA��A��9A��A�"4A�9�A�G�A��A��}A��KA�E�A���A���A��A�^A�=<A�бA�"�A��A���A�^�A���A��CA�ĜA���A�
�A��A�gmA���A�͟A��VA��$A���A�ѷA��hA��A��aA��SA���A}��Az8Ar�HAl��Ah��Ae~A`+A\A�AX�KAWjAV��AVAS�AQm�AO0�AMOAK�AB��A@oiA>�LA=A:�cA9~A8�nA7�A4s�A3}�A2�A0��A-v`A*bA)g�A'~�A'>�A'��A'�|A'�A%��A%�vA%l�A$�A$�6A$�>A$�NA$��A%
=A$�SA$��A$��A$l�A$oiA$�A#ȴA#JA"C�A"5?A!&�A zxA �A��AC�A�A��A/�A�AMjAѷA��A;�A�MAA�A�9AU2ATaA�A��AH�A�Ao�A��A��Am]A6�AoiA[�Al�A=�A�A��A�OA{JA��AOvA�!A�+A(�A�A	�A�A�"A��A��A
�A
�nA
�oA
'�A	��A	��A	?}A��A˒A��A�AںAD�A�QAo�AMA��Ag8A�A��A�AxlA1�AیA�EA��AA �nA @��+@��C@�S@���@�4�@�|�@��$@��@���@�]d@�B[@�2�@��@��@�Vm@�@�;�@���@�-w@�-@�j@�@�\@��@���@��@�w2@��[@��@�8@�4@�S�@��y@�q@�#:@�@���@�_@�Z@��@�H�@�W�@�!-@��c@�4n@���@�y�@�@��o@߃{@�7L@ޞ@��.@��@ܵ�@�n�@۩*@��@�e�@��@��@���@�p�@�c @��D@���@פ@@�s@�#�@և+@��r@�=@ԡb@��@�|�@�+@���@�͟@ҝI@�K^@я�@�U�@�?}@���@�G@��@���@�{@ͪ�@�6z@̬@�tT@�/�@˩�@�a�@�)_@�Ĝ@�c�@ɯ�@�~�@�0�@���@Ȗ�@�~@�|@�!�@��@��H@ƒ�@�x@�˒@ť�@��"@�%�@ò�@�;d@¶�@�6@�ϫ@�dZ@�8@���@�I�@��]@���@��@�:�@���@���@��@��]@�`B@�Xy@���@���@��@�I�@�Y�@��@��@�=q@��@�� @��@��w@��$@�e�@�F�@�@O@��9@��#@�B�@�@���@��@�M@���@�T�@��@���@��+@��&@��h@�O�@��@��@�C@�%@��@��@��;@��@�t�@�\�@�[W@�.I@��@�$�@�T�@��e@�`�@�:�@��9@��@��D@��V@���@�^�@�	l@��y@��D@�2�@��6@�|�@�q@��]@��!@�`�@��@���@�ƨ@�|�@�C�@�;@���@�@�� @��=@�dZ@�4@�@���@�N�@�_@��@�O�@��@��@���@��@�B[@��@���@���@��@�\�@��@��@��@�	l@���@���@�GE@� �@��A@���@�F@��K@���@��'@���@���@�/�@��@�_p@�J�@�F@�/�@�ѷ@���@�W�@�_@�L�@���@��@�n�@��@��Q@���@�x@�n/@�N<@���@�z�@�V@��@�@�Z�@���@���@��@��k@��	@��H@���@���@�L0@��@��0@�u�@�F@�"�@���@�c�@�6@���@��f@�l�@�@O@�@��E@���@���@�p;@�D�@�4@���@��3@�g�@��@���@�<�@�,=@��@���@�O�@�@@��z@�6�@��@��n@���@�x�@�:�@�Y@��@� i@���@�҉@���@�c�@�\�@�1'@��@�ϫ@���@���@�zx@�O@�'�@��@���@�_@�6@��@�خ@��*@�o@��@��U@��4@�L0@��@��@��	@�9�@��@��@���@�z@�9X@��@���@�zx@�A�@�P�@�Y@��@��I@�C�@��@�n/@�]�@�P�@�@���@�bN@�Ft@�	@�@ݘ@�@�f@J#@~�8@~��@}��@}X@|�P@|�Y@|@{�V@{_p@z��@z�L@z.�@yrG@y/@y!�@y�@y�@y�@x��@x��@xh�@x!@w�A@w�}@ws@v�y@vL0@u��@u�n@uVm@t�f@t��@tV�@s�r@r��@r�@r+k@q�N@q��@p�5@o�@o��@ov`@oE9@n�H@nd�@m��@mq@l��@l�@ll"@lN�@l	�@j��@j&�@j4@irG@h��@h�D@h	�@ge�@g�@f�]@e�o@ew2@e+@d��@d_@d6@d�@c��@cC�@b��@b}V@b5?@a��@a<6@`�4@`tT@`PH@`:�@`x@_O@^�B@^�L@^�+@^1�@]�)@^ �@]�@]�@]&�@\��@[�&@[�
@[�@@[&@Z�@Z��@Z�+@ZZ�@Z$�@Z@Y�)@Y�9@Y��@Y(�@X|�@X'R@X�@W� @W��@WP�@W�@V��@V{�@U�@U�@T�z@S��@S6z@S i@R�F@R�@Q�d@QO�@P�`@P�.@Pc�@P'R@O��@O{J@O)_@N�M@N��@N�@M�~@MIR@Lی@L��@Loi@L[�@L:�@Kn/@J_�@I�j@I�M@I8�@I@@H�@Hoi@HH@H�@G��@G9�@F_�@E��@E8�@D��@D�.@D-�@Dx@C�A@C��@C��@CH�@C$t@B��@Bu%@B4@Aw2@A!�@@Xy@@,=@?�+@?��@?��@?RT@? i@>�h@>:*@=�z@=m]@=q@<�	@<�z@<U2@<(�@<�@;�w@;�@:�@:�@:�@:�@:�@:l�@:Ta@: �@9��@9T�@9%F@9	l@9;@8�/@8��@8�@8bN@7�]@7�@7�	@7/�@6�<@6��@6c @6)�@5��@5S&@5%@4��@4�O@4H@3� @3J#@3@2��@2YK@25?@2	@1�N@1��@1m]@1(�@0�j@0m�@0G@/��@/�	@/�@.��@.��@.C�@.$�@-�@-�^@-�'@-m]@-Y�@-Dg@-*0@,��@,�@,K^@,$@+�w@+_p@+&@*�@*ȴ@*�h@*��@*u@)��@)O�@(�@(�)@(��@(H@'�@'�Q@'�f@'F�@' i@&��@&s�@&5?@&4@%�9@%��@%f�@%4@%;@$��@$S�@$�@#�[@#iD@#8@"�@"^5@"H�@"6�@"!�@!�.@!�Z@!�T@!��@!IR@!@@ �)@ ��@ ~(@ S�@ �@��@j�@)_@�H@�m@�h@�@_�@+k@�@��@��@�T@:�@�O@u�@U2@M@�@��@t�@U�@4�@o@�@��@H�@($@u@�@��@�=@a�@2a@��@�[@�z@��@�.@oi@1'@��@��@|�@S�@H�@E9@H�@F�@!-@��@��@�+@L0@&�@
�@��@�@�@�C@�@��@L�@@�@ی@�z@l"@ �@��@��@RT@ i@�X@�}@�\@�A@��@s�@L0@?@J@�Z@�@�N@��@��@�S@c@p�@*0@��@C-@7@  @ݘ@�@�Q@��@�$@��@x@RT@P�@K�@H�@;d@�]111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A�wAⲖA�A⵨A�nA�r�A��A�+A�҉A��@A��A�\�A޾�A޸�A�!�A���A�^�A�B'A��A���Aܪ�A۔{A�M6Aד�AӁA�!�A���A��AʰUA�xA���A���A�~(A��Aŋ�A�z�A�B�A�m�A�=qA��A� \A��0A��DA���A���A�AA��A��9A��A�"4A�9�A�G�A��A��}A��KA�E�A���A���A��A�^A�=<A�бA�"�A��A���A�^�A���A��CA�ĜA���A�
�A��A�gmA���A�͟A��VA��$A���A�ѷA��hA��A��aA��SA���A}��Az8Ar�HAl��Ah��Ae~A`+A\A�AX�KAWjAV��AVAS�AQm�AO0�AMOAK�AB��A@oiA>�LA=A:�cA9~A8�nA7�A4s�A3}�A2�A0��A-v`A*bA)g�A'~�A'>�A'��A'�|A'�A%��A%�vA%l�A$�A$�6A$�>A$�NA$��A%
=A$�SA$��A$��A$l�A$oiA$�A#ȴA#JA"C�A"5?A!&�A zxA �A��AC�A�A��A/�A�AMjAѷA��A;�A�MAA�A�9AU2ATaA�A��AH�A�Ao�A��A��Am]A6�AoiA[�Al�A=�A�A��A�OA{JA��AOvA�!A�+A(�A�A	�A�A�"A��A��A
�A
�nA
�oA
'�A	��A	��A	?}A��A˒A��A�AںAD�A�QAo�AMA��Ag8A�A��A�AxlA1�AیA�EA��AA �nA @��+@��C@�S@���@�4�@�|�@��$@��@���@�]d@�B[@�2�@��@��@�Vm@�@�;�@���@�-w@�-@�j@�@�\@��@���@��@�w2@��[@��@�8@�4@�S�@��y@�q@�#:@�@���@�_@�Z@��@�H�@�W�@�!-@��c@�4n@���@�y�@�@��o@߃{@�7L@ޞ@��.@��@ܵ�@�n�@۩*@��@�e�@��@��@���@�p�@�c @��D@���@פ@@�s@�#�@և+@��r@�=@ԡb@��@�|�@�+@���@�͟@ҝI@�K^@я�@�U�@�?}@���@�G@��@���@�{@ͪ�@�6z@̬@�tT@�/�@˩�@�a�@�)_@�Ĝ@�c�@ɯ�@�~�@�0�@���@Ȗ�@�~@�|@�!�@��@��H@ƒ�@�x@�˒@ť�@��"@�%�@ò�@�;d@¶�@�6@�ϫ@�dZ@�8@���@�I�@��]@���@��@�:�@���@���@��@��]@�`B@�Xy@���@���@��@�I�@�Y�@��@��@�=q@��@�� @��@��w@��$@�e�@�F�@�@O@��9@��#@�B�@�@���@��@�M@���@�T�@��@���@��+@��&@��h@�O�@��@��@�C@�%@��@��@��;@��@�t�@�\�@�[W@�.I@��@�$�@�T�@��e@�`�@�:�@��9@��@��D@��V@���@�^�@�	l@��y@��D@�2�@��6@�|�@�q@��]@��!@�`�@��@���@�ƨ@�|�@�C�@�;@���@�@�� @��=@�dZ@�4@�@���@�N�@�_@��@�O�@��@��@���@��@�B[@��@���@���@��@�\�@��@��@��@�	l@���@���@�GE@� �@��A@���@�F@��K@���@��'@���@���@�/�@��@�_p@�J�@�F@�/�@�ѷ@���@�W�@�_@�L�@���@��@�n�@��@��Q@���@�x@�n/@�N<@���@�z�@�V@��@�@�Z�@���@���@��@��k@��	@��H@���@���@�L0@��@��0@�u�@�F@�"�@���@�c�@�6@���@��f@�l�@�@O@�@��E@���@���@�p;@�D�@�4@���@��3@�g�@��@���@�<�@�,=@��@���@�O�@�@@��z@�6�@��@��n@���@�x�@�:�@�Y@��@� i@���@�҉@���@�c�@�\�@�1'@��@�ϫ@���@���@�zx@�O@�'�@��@���@�_@�6@��@�خ@��*@�o@��@��U@��4@�L0@��@��@��	@�9�@��@��@���@�z@�9X@��@���@�zx@�A�@�P�@�Y@��@��I@�C�@��@�n/@�]�@�P�@�@���@�bN@�Ft@�	@�@ݘ@�@�f@J#@~�8@~��@}��@}X@|�P@|�Y@|@{�V@{_p@z��@z�L@z.�@yrG@y/@y!�@y�@y�@y�@x��@x��@xh�@x!@w�A@w�}@ws@v�y@vL0@u��@u�n@uVm@t�f@t��@tV�@s�r@r��@r�@r+k@q�N@q��@p�5@o�@o��@ov`@oE9@n�H@nd�@m��@mq@l��@l�@ll"@lN�@l	�@j��@j&�@j4@irG@h��@h�D@h	�@ge�@g�@f�]@e�o@ew2@e+@d��@d_@d6@d�@c��@cC�@b��@b}V@b5?@a��@a<6@`�4@`tT@`PH@`:�@`x@_O@^�B@^�L@^�+@^1�@]�)@^ �@]�@]�@]&�@\��@[�&@[�
@[�@@[&@Z�@Z��@Z�+@ZZ�@Z$�@Z@Y�)@Y�9@Y��@Y(�@X|�@X'R@X�@W� @W��@WP�@W�@V��@V{�@U�@U�@T�z@S��@S6z@S i@R�F@R�@Q�d@QO�@P�`@P�.@Pc�@P'R@O��@O{J@O)_@N�M@N��@N�@M�~@MIR@Lی@L��@Loi@L[�@L:�@Kn/@J_�@I�j@I�M@I8�@I@@H�@Hoi@HH@H�@G��@G9�@F_�@E��@E8�@D��@D�.@D-�@Dx@C�A@C��@C��@CH�@C$t@B��@Bu%@B4@Aw2@A!�@@Xy@@,=@?�+@?��@?��@?RT@? i@>�h@>:*@=�z@=m]@=q@<�	@<�z@<U2@<(�@<�@;�w@;�@:�@:�@:�@:�@:�@:l�@:Ta@: �@9��@9T�@9%F@9	l@9;@8�/@8��@8�@8bN@7�]@7�@7�	@7/�@6�<@6��@6c @6)�@5��@5S&@5%@4��@4�O@4H@3� @3J#@3@2��@2YK@25?@2	@1�N@1��@1m]@1(�@0�j@0m�@0G@/��@/�	@/�@.��@.��@.C�@.$�@-�@-�^@-�'@-m]@-Y�@-Dg@-*0@,��@,�@,K^@,$@+�w@+_p@+&@*�@*ȴ@*�h@*��@*u@)��@)O�@(�@(�)@(��@(H@'�@'�Q@'�f@'F�@' i@&��@&s�@&5?@&4@%�9@%��@%f�@%4@%;@$��@$S�@$�@#�[@#iD@#8@"�@"^5@"H�@"6�@"!�@!�.@!�Z@!�T@!��@!IR@!@@ �)@ ��@ ~(@ S�@ �@��@j�@)_@�H@�m@�h@�@_�@+k@�@��@��@�T@:�@�O@u�@U2@M@�@��@t�@U�@4�@o@�@��@H�@($@u@�@��@�=@a�@2a@��@�[@�z@��@�.@oi@1'@��@��@|�@S�@H�@E9@H�@F�@!-@��@��@�+@L0@&�@
�@��@�@�@�C@�@��@L�@@�@ی@�z@l"@ �@��@��@RT@ i@�X@�}@�\@�A@��@s�@L0@?@J@�Z@�@�N@��@��@�S@c@p�@*0@��@C-@7@  @ݘ@�@�Q@��@�$@��@x@RT@P�@K�@H�@;d@�]111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
B
DB
xB
^B
DB

�B
1B
B	��B	�zB	�AB	�B	҉B	��B	��B	��B	��B	��B	��B	�B	��B	�GB	��B	�[B	~BB	�B	��B	�&B	��B	�B	}�B	�OB
�B
oB
'B
A�B
b�B
��B
��B
��B
��B
��B
�dB
�gB
�fB
��B
�B
҉B
��B
�B �B
=B$�B7�B8�BcBp�BZkBN�BK�B1B'B2B�B!�B,�B�B
�$B
�0B
��B
��B
�MB
�"B
�B
��B
��B
�B
��B
��B
{�B
Q�B
=<B
3�B
(�B
�B	��B	�lB	��B	h�B	N"B	6�B	�B	�B��B�B�iB�B�'B��B�2B�bB��B��B��B��B��B�B��B��B�B�B�	B��B��B�B��B��B�	B�HB�%B��B�}B�B�xB��B��B�B	�B	�B	$�B	5�B	>�B	O�B	p!B	HB	��B	�kB	�:B	��B	��B	��B	�iB	��B	��B	�MB	�?B	�DB	ȀB	�JB	��B	��B	�6B	�B	��B	�rB	�B	��B	��B	��B	�B	�B	ʦB	ɠB	��B	�B	�GB	��B	�OB	�}B	�[B	�iB	��B	�cB	��B	̈́B	ϫB	��B	֡B	��B	�B	��B	�;B	�B	�:B	�B	�`B	�B	�B	�4B	�B	�:B	��B	�TB	�B	�,B	�B	��B	�B	��B	��B	�pB	��B	��B	�B	��B	��B	�7B	ٴB	��B	ٴB	�	B	��B	�]B	��B	�B	�/B	�]B	�B	�B	��B	��B	��B	�+B	�eB	�eB	��B	�QB	�kB	چB	ںB	�WB	��B	ۦB	��B	�QB	ܒB	�B	��B	�B	�dB	��B	�/B	�]B	��B	��B	��B	�dB	ޞB	޸B	�jB	ޞB	ݘB	��B	�B	�B	�B	�,B	�ZB	�B	�B	��B	�B	߾B	��B	�pB	�'B	�B	�|B	�B	�B	�tB	��B	�`B	�`B	�fB	�mB	�
B	��B	��B	�8B	�B	��B	�B	��B	�LB	�B	�,B	�B	�,B	��B	�B	��B	�B	�B	�B	�mB	��B	�B	�>B	�B	��B	��B	�>B	�XB	�B	��B	�B	�B	�
B	�>B	��B	��B	�RB	�B	��B	��B	�B	��B	�QB	��B	�B	�B	�B	�6B	�B	�]B	��B	�5B	�B	�3B	�B	�B	�B	�TB	��B	�%B	��B	��B	��B	��B	��B	��B	��B	�hB	��B	��B	�+B	�zB	�B	�B	��B	��B	��B	�B	��B	�lB	��B	�`B	��B	�B	�xB	�B	�jB	��B	��B	�6B	�B	��B	�JB	��B	��B	��B	��B	�(B	�B	��B	��B	��B	�"B	��B	��B	��B
 B
�B
B
�B
�B
9B
�B
�B
�B
�B
�B
�B
�B
B
B
MB
�B
�B
�B
�B
�B
�B
MB
3B
MB
gB
gB
�B
gB
3B
�B
�B
B
B
9B
SB
�B
SB
�B
?B
?B
?B
%B
YB
YB
zB
�B
�B
B
	�B

rB
^B
�B
�B
�B
�B
�B
�B
�B
�B
PB
B
B
PB
�B
�B
�B
B
B
B
�B
�B
�B
�B
�B
�B
BB
BB
\B
BB
B
B
�B
B
B
.B
hB
hB
�B
�B
TB
oB
�B
B
�B
B
�B
B
FB
�B
�B
�B
�B
B
�B
B
B
B
�B
�B
;B
!B
�B
�B
�B
�B
 �B
 �B
 �B
!�B
"4B
"NB
"hB
"�B
"�B
"�B
"�B
"�B
#TB
#TB
#:B
#�B
#�B
$@B
%FB
%�B
%zB
%�B
%�B
&�B
&�B
'�B
'�B
(�B
(�B
)B
)_B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
*B
*B
*0B
*�B
*�B
*�B
*�B
*�B
+�B
,qB
,�B
,�B
-B
-�B
.�B
.�B
.�B
.cB
-�B
-�B
-wB
-�B
-�B
-�B
.B
.B
./B
.IB
.cB
.cB
.�B
/B
/�B
0!B
0�B
1�B
1�B
1'B
0�B
1vB
1[B
0�B
1'B
1'B
0�B
/iB
/B
/OB
/�B
/�B
/�B
/�B
/�B
/�B
/�B
0!B
0�B
0�B
1[B
1�B
1�B
2-B
2-B
2|B
2�B
2�B
3�B
4TB
4nB
4�B
5�B
6`B
6�B
7�B
7�B
88B
9XB
9rB
9�B
:^B
:�B
;0B
;JB
;�B
;�B
;�B
;�B
;�B
<jB
<jB
<�B
<�B
<�B
="B
=�B
=�B
=�B
=�B
=�B
>BB
>�B
?B
>�B
>�B
>�B
>�B
>�B
?}B
?cB
?HB
?}B
?�B
?�B
?�B
@4B
@4B
@OB
@�B
@�B
@�B
A�B
A�B
BAB
B�B
B�B
C-B
CGB
B�B
B�B
CB
C{B
C-B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
EB
E�B
F�B
F�B
F�B
F%B
FYB
G+B
GzB
GB
F�B
F�B
F�B
F�B
F�B
GB
G+B
G�B
G�B
H1B
HB
HB
H1B
HKB
H�B
IB
I7B
I�B
J#B
KxB
K�B
K�B
L�B
L�B
L�B
MB
MPB
MB
MB
M6B
MB
L�B
MB
M�B
M�B
NB
N<B
NVB
N�B
N�B
OB
OBB
OBB
O\B
OBB
OB
O�B
PbB
PbB
P�B
P�B
Q B
Q4B
Q�B
Q�B
Q�B
Q�B
Q�B
RoB
R�B
S[B
S�B
S�B
T,B
T,B
T,B
TaB
TaB
T�B
T{B
T�B
T�B
T�B
UMB
V9B
V9B
VB
V9B
V9B
V�B
V�B
W?B
WsB
X+B
XyB
X_B
X_B
X_B
X�B
X�B
YB
YB
YB
ZB
ZQB
Z7B
ZQB
ZQB
Z�B
Z�B
Z�B
[	B
[=B
[qB
[qB
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\]B
\�B
]IB
]IB
]B
]�B
^B
_!B
_;B
_VB
_�B
_�B
`'B
`�B
`�B
`�B
aB
abB
a�B
a|B
a�B
a�B
a�B
b4B
bhB
b�B
b�B
cB
c:B
c�B
c�B
c�B
d&B
d&B
dZB
dZB
dZB
d�B
d�B
d�B
d�B
d�B
eFB
ezB
ezB
fB
f�B
f�B
f�B
gB
g8B
g8B
g�B
h$B
hsB
h�B
h�B
h�B
i*B
i�B
i�B
jeB
jB
j�B
j�B
j�B
j�B
kB
k6B
k6B
kkB
kQB
k6B
kkB
k�B
k�B
k�B
k�B
k�B
k�B
lWB
lqB
m]B
mCB
mCB
m)B
m]B
m�B
n}B
o5B
o�B
o�B
o�B
o�B
pB
p!B
pB
p!B
pUB
poB
poB
p�B
p�B
p�B
p�B
p�B
p�B
q'B
qB
p�B
p�B
p�B
p�B
q'B
q�B
r|B
sMB
shB
sMB
sMB
r�B
raB
rGB
rGB
r|B
r|B
r�B
sB
s3B
sMB
s�B
s�B
s�B
s�B
s�B
tB
t�B
t�B
t�B
u%B
u?B
u%B
u%B
u%B
utB
u�B
u�B
u�B
v+B
v+B
v+B
vFB
vzB
v�B
v�B
v�B
v�B
v�B
w2B
wLB
wLB
w�B
w�B
w�B
xB
x8B
x�B
x�B
y	B
y$B
yXB
yrB
yXB
yrB
y�B
y�B
y�B
y�B
y�B
zB
z*B
zDB
zDB
z^B
z*B
zxB
{0B
{JB
{B
{B
{�B
{�B
{�B
{�B
|PB
|�B
}VB
}�B
}�B
}�B
}�B
}�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
B
DB
xB
^B
DB

�B
1B
B	��B	�zB	�AB	�B	҉B	��B	��B	��B	��B	��B	��B	�B	��B	�GB	��B	�[B	~BB	�B	��B	�&B	��B	�B	}�B	�OB
�B
oB
'B
A�B
b�B
��B
��B
��B
��B
��B
�dB
�gB
�fB
��B
�B
҉B
��B
�B �B
=B$�B7�B8�BcBp�BZkBN�BK�B1B'B2B�B!�B,�B�B
�$B
�0B
��B
��B
�MB
�"B
�B
��B
��B
�B
��B
��B
{�B
Q�B
=<B
3�B
(�B
�B	��B	�lB	��B	h�B	N"B	6�B	�B	�B��B�B�iB�B�'B��B�2B�bB��B��B��B��B��B�B��B��B�B�B�	B��B��B�B��B��B�	B�HB�%B��B�}B�B�xB��B��B�B	�B	�B	$�B	5�B	>�B	O�B	p!B	HB	��B	�kB	�:B	��B	��B	��B	�iB	��B	��B	�MB	�?B	�DB	ȀB	�JB	��B	��B	�6B	�B	��B	�rB	�B	��B	��B	��B	�B	�B	ʦB	ɠB	��B	�B	�GB	��B	�OB	�}B	�[B	�iB	��B	�cB	��B	̈́B	ϫB	��B	֡B	��B	�B	��B	�;B	�B	�:B	�B	�`B	�B	�B	�4B	�B	�:B	��B	�TB	�B	�,B	�B	��B	�B	��B	��B	�pB	��B	��B	�B	��B	��B	�7B	ٴB	��B	ٴB	�	B	��B	�]B	��B	�B	�/B	�]B	�B	�B	��B	��B	��B	�+B	�eB	�eB	��B	�QB	�kB	چB	ںB	�WB	��B	ۦB	��B	�QB	ܒB	�B	��B	�B	�dB	��B	�/B	�]B	��B	��B	��B	�dB	ޞB	޸B	�jB	ޞB	ݘB	��B	�B	�B	�B	�,B	�ZB	�B	�B	��B	�B	߾B	��B	�pB	�'B	�B	�|B	�B	�B	�tB	��B	�`B	�`B	�fB	�mB	�
B	��B	��B	�8B	�B	��B	�B	��B	�LB	�B	�,B	�B	�,B	��B	�B	��B	�B	�B	�B	�mB	��B	�B	�>B	�B	��B	��B	�>B	�XB	�B	��B	�B	�B	�
B	�>B	��B	��B	�RB	�B	��B	��B	�B	��B	�QB	��B	�B	�B	�B	�6B	�B	�]B	��B	�5B	�B	�3B	�B	�B	�B	�TB	��B	�%B	��B	��B	��B	��B	��B	��B	��B	�hB	��B	��B	�+B	�zB	�B	�B	��B	��B	��B	�B	��B	�lB	��B	�`B	��B	�B	�xB	�B	�jB	��B	��B	�6B	�B	��B	�JB	��B	��B	��B	��B	�(B	�B	��B	��B	��B	�"B	��B	��B	��B
 B
�B
B
�B
�B
9B
�B
�B
�B
�B
�B
�B
�B
B
B
MB
�B
�B
�B
�B
�B
�B
MB
3B
MB
gB
gB
�B
gB
3B
�B
�B
B
B
9B
SB
�B
SB
�B
?B
?B
?B
%B
YB
YB
zB
�B
�B
B
	�B

rB
^B
�B
�B
�B
�B
�B
�B
�B
�B
PB
B
B
PB
�B
�B
�B
B
B
B
�B
�B
�B
�B
�B
�B
BB
BB
\B
BB
B
B
�B
B
B
.B
hB
hB
�B
�B
TB
oB
�B
B
�B
B
�B
B
FB
�B
�B
�B
�B
B
�B
B
B
B
�B
�B
;B
!B
�B
�B
�B
�B
 �B
 �B
 �B
!�B
"4B
"NB
"hB
"�B
"�B
"�B
"�B
"�B
#TB
#TB
#:B
#�B
#�B
$@B
%FB
%�B
%zB
%�B
%�B
&�B
&�B
'�B
'�B
(�B
(�B
)B
)_B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
*B
*B
*0B
*�B
*�B
*�B
*�B
*�B
+�B
,qB
,�B
,�B
-B
-�B
.�B
.�B
.�B
.cB
-�B
-�B
-wB
-�B
-�B
-�B
.B
.B
./B
.IB
.cB
.cB
.�B
/B
/�B
0!B
0�B
1�B
1�B
1'B
0�B
1vB
1[B
0�B
1'B
1'B
0�B
/iB
/B
/OB
/�B
/�B
/�B
/�B
/�B
/�B
/�B
0!B
0�B
0�B
1[B
1�B
1�B
2-B
2-B
2|B
2�B
2�B
3�B
4TB
4nB
4�B
5�B
6`B
6�B
7�B
7�B
88B
9XB
9rB
9�B
:^B
:�B
;0B
;JB
;�B
;�B
;�B
;�B
;�B
<jB
<jB
<�B
<�B
<�B
="B
=�B
=�B
=�B
=�B
=�B
>BB
>�B
?B
>�B
>�B
>�B
>�B
>�B
?}B
?cB
?HB
?}B
?�B
?�B
?�B
@4B
@4B
@OB
@�B
@�B
@�B
A�B
A�B
BAB
B�B
B�B
C-B
CGB
B�B
B�B
CB
C{B
C-B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
EB
E�B
F�B
F�B
F�B
F%B
FYB
G+B
GzB
GB
F�B
F�B
F�B
F�B
F�B
GB
G+B
G�B
G�B
H1B
HB
HB
H1B
HKB
H�B
IB
I7B
I�B
J#B
KxB
K�B
K�B
L�B
L�B
L�B
MB
MPB
MB
MB
M6B
MB
L�B
MB
M�B
M�B
NB
N<B
NVB
N�B
N�B
OB
OBB
OBB
O\B
OBB
OB
O�B
PbB
PbB
P�B
P�B
Q B
Q4B
Q�B
Q�B
Q�B
Q�B
Q�B
RoB
R�B
S[B
S�B
S�B
T,B
T,B
T,B
TaB
TaB
T�B
T{B
T�B
T�B
T�B
UMB
V9B
V9B
VB
V9B
V9B
V�B
V�B
W?B
WsB
X+B
XyB
X_B
X_B
X_B
X�B
X�B
YB
YB
YB
ZB
ZQB
Z7B
ZQB
ZQB
Z�B
Z�B
Z�B
[	B
[=B
[qB
[qB
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\]B
\�B
]IB
]IB
]B
]�B
^B
_!B
_;B
_VB
_�B
_�B
`'B
`�B
`�B
`�B
aB
abB
a�B
a|B
a�B
a�B
a�B
b4B
bhB
b�B
b�B
cB
c:B
c�B
c�B
c�B
d&B
d&B
dZB
dZB
dZB
d�B
d�B
d�B
d�B
d�B
eFB
ezB
ezB
fB
f�B
f�B
f�B
gB
g8B
g8B
g�B
h$B
hsB
h�B
h�B
h�B
i*B
i�B
i�B
jeB
jB
j�B
j�B
j�B
j�B
kB
k6B
k6B
kkB
kQB
k6B
kkB
k�B
k�B
k�B
k�B
k�B
k�B
lWB
lqB
m]B
mCB
mCB
m)B
m]B
m�B
n}B
o5B
o�B
o�B
o�B
o�B
pB
p!B
pB
p!B
pUB
poB
poB
p�B
p�B
p�B
p�B
p�B
p�B
q'B
qB
p�B
p�B
p�B
p�B
q'B
q�B
r|B
sMB
shB
sMB
sMB
r�B
raB
rGB
rGB
r|B
r|B
r�B
sB
s3B
sMB
s�B
s�B
s�B
s�B
s�B
tB
t�B
t�B
t�B
u%B
u?B
u%B
u%B
u%B
utB
u�B
u�B
u�B
v+B
v+B
v+B
vFB
vzB
v�B
v�B
v�B
v�B
v�B
w2B
wLB
wLB
w�B
w�B
w�B
xB
x8B
x�B
x�B
y	B
y$B
yXB
yrB
yXB
yrB
y�B
y�B
y�B
y�B
y�B
zB
z*B
zDB
zDB
z^B
z*B
zxB
{0B
{JB
{B
{B
{�B
{�B
{�B
{�B
|PB
|�B
}VB
}�B
}�B
}�B
}�B
}�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104852  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172653  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172654  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172654                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022701  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605022701  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131506                      G�O�G�O�G�O�                