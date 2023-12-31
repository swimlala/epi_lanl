CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:25:22Z creation;2022-06-04T17:25:22Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604172522  20220610131506  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @��v3���1   @��v���@,ݲ-V�c����F1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @@  @y��@�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BJffBPffBV  B`  Bh  Bp  BzffB��B�ffB���B�  B�  B�33B�ffB�ffB���B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B���Bϙ�B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  CL�C	��C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C+�fC.  C0ffC1��C3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D �fD!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DA��DBy�DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DSfDS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dyy�Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�p 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @AG�@z�H@���A Q�A Q�A>�RA`Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B@{BJz�BPz�BV{B`{Bh{Bp{Bzz�B�B�p�B��
B�
=B�
=B�=pB�p�B�p�B��
B��
B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�=pB��
Bϣ�B�
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
=C CCCCQ�C	��CCCCCCCCCCC C"C$C&C(C*C+�C.C0k�C1��C3�C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfCh�Cj�ClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C��C��C��C��C�\C��C��C��C��C�\C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD ��D!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDA��DBz�DCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDS�DS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDyz�DzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D��qD�=qD���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�C�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D��D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�@�D怤D���D� �D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D��D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aݕ�Aݐ.A݈�A�|�A�{�A�x�A�wfA�y>A�{JA�{�A�|�A�|�A�}�A��A݀ A݀�A݂A݃GA݃�A݁�A�~�A�~(A�~�A�.A�R�A��Aٯ�A��A��TA���A�6�A�xAʺ^A�ʌAŴ�Aċ�A��A�c�A�5�A�8�A��hA���A��RA��A�7�A���A��A��A��fA�|PA��A��FA���A�oiA�$tA�FtA��<A��}A���A�خA�	7A��sA��6A���A���A�"�A�bA���A��}A�2�A�9�A��A��A���A��WA��kA��qA�ƨA�A��%A��A��RA��A~�{A{{�Ay�}Aw��Aq��Ap]�Am�Agu�AbG�A\e,AX8�AU��ATw2AQ��AP*�AM�mAL�AK�oAH�AE�gADo�AB1�A@�:A?��A>�7A>!A<A:�fA:YA8��A8j�A7w�A5��A3,=A1�YA1L�A0��A/�EA.�A.�A-�A,�RA,�A+��A+a�A*��A*D�A)�A(�A'�	A'VA&}VA%~(A$�A$)_A#��A#�7A#��A#��A"��A!��A �A OvA��A��A�~A_A�A	A�MAr�A%�Ar�Ar�A&�A��A�A=�A�@A8A�A�A��AqAl�AW?AS�A��A��A�A��A��A�A�XAZA4�Ac�A6zA��ARTA�sA��Ag8AuA5?A
$tA	!A�gAn�AA�A��A%�A'�A��A��AK�A�AɆA:�A��A-A�AxA!�A͟AxlA8�A�A�jA ��A �oA ?@��@�c�@�&@���@��&@���@���@�,=@�@��&@�8�@��@�]�@���@�xl@��@���@�?}@�@��@�P@�@��@��3@��@�p�@��@�q@���@��2@�l"@��@��9@�F@��@�$t@� @��K@�L�@���@�}V@��@�F�@���@�F@�5?@��@��@Ṍ@�c�@�d�@�&@�PH@ݻ0@�q@ܫ6@�&�@��g@�1�@�M�@ٟV@���@�($@��N@�{J@֡b@�-�@է�@�+@��5@��s@Զ�@��@�%@�V�@��@��@ы�@��@�Q@���@�o�@�X@�@�N�@́@�c�@��@�ff@�6@˄M@�	l@�1�@ʕ@��@�'�@Ƚ<@ȵ�@��@�y>@ǫ�@�(@�e�@��@Š�@�q@��E@�u@Ì~@�'�@�`�@���@�hs@��@�-@���@�,�@��@�Vm@�\�@�=�@��@���@�
�@�(@�?�@��@�Z�@�C-@��@��$@�ی@��W@��0@�=�@��x@�!@��r@��#@���@���@���@���@�t�@��@��@���@��@���@��	@�:�@���@��@�33@�#�@��@��b@�Z@�y>@�~(@��@�[�@��+@��@@���@�\�@��@�tT@�1�@�@��@��I@�_@�C�@�#:@�"h@��g@�q@���@�C-@��+@�|@�)_@���@�h�@���@��x@�:*@��W@���@��/@���@�bN@�A�@���@���@�\�@��M@���@��@�W�@�~@��Q@��@�g�@�=@��@��M@�=q@�˒@�0�@��,@�J�@�/@���@��'@��b@�Q@��.@��C@��V@���@���@��4@�X�@�ߤ@��@���@�t�@�m]@�K�@��@�W�@��@��@���@�%F@���@��@���@�;�@�*�@�
�@��Q@���@�^�@��1@�S�@�(�@��@���@���@��@�^�@�/@���@���@�|�@� �@���@��^@�v`@�E9@��@��9@���@�Ft@�O@��K@�t�@��@��@��z@�}V@�6�@���@�m]@�V@���@���@��+@�y>@�q@�]d@��@���@��@��@���@�G�@��@��K@���@���@�B[@��@��@���@�P�@�@��c@���@���@���@��:@�=�@��@���@�g8@�4@��H@�e�@�;d@��@���@���@�D�@��@�\�@��@���@��I@���@�y>@�L0@��@��A@�o�@�*0@���@�Ɇ@���@���@���@��\@�ff@��@���@�zx@�dZ@�S�@�N<@�IR@��@���@��v@���@�PH@�6�@��@��o@��@��@��|@��}@���@�_@�1'@��@j�@@~;�@}�@}Vm@} \@|�@|e�@{��@{y�@z��@zq�@z#:@y��@x��@x��@x�@x`�@x?�@x�@wE9@v�+@u��@uG�@u/@u�@t��@tA�@s�@rc @rGE@r?@q�@q(�@poi@oݘ@o1�@n͟@nz@n�@m�@l��@l%�@k�@kn/@j�c@j��@jB[@i��@iL�@hbN@g�@@g+@fl�@f1�@f@ep�@e:�@d�@d�.@dM@c�0@c\)@cC@b��@bQ@a�3@`�[@_�W@_�@_g�@^�2@^�R@^�@^Ov@]�d@]|@]Dg@\�@[�q@[A�@[�@Z?@Ym]@YF@Y!�@X|�@X	�@W�g@W��@Wn/@V�]@Vp;@V0U@U�o@UA @U�@T�[@T>B@S��@Sb�@SO@S�@R��@RV@Q��@Q�H@Q�'@Q�M@Qa�@QDg@Q�@Py>@O�Q@Oj�@O�@N�,@N8�@M�3@MJ�@L��@L�@L��@LZ@L�@K�
@K�V@KW?@J��@JL0@I��@Iw2@H�/@H%�@G�@Gخ@G�@G��@G��@G��@GZ�@G6z@GC@F�c@F��@FJ�@E��@E��@E�7@Ec�@D��@D�?@D��@Dc�@D?�@D"h@C��@C�V@C!-@B�6@A�o@A}�@AB�@@��@@�@@[�@@"h@?�F@?dZ@?
=@>�R@>3�@=�@=�C@=x�@=T�@= \@<�@<�9@<�@;��@;|�@;J#@;�@:��@:M�@:e@9�>@9��@8��@8�p@8��@8�@8V�@7��@76z@6��@6��@6�b@6u%@60U@5�N@5�'@5��@5�h@5�@4��@41'@3|�@3P�@3�@2s�@2&�@1��@1c@1#�@0��@0�j@0>B@/��@/�@/"�@/�@.�c@.�6@.��@.E�@-�^@-m]@,w�@,I�@,�@+�6@+�:@+j�@+@O@+�@*��@*E�@*!�@*@)zx@)�@(�U@(�.@(c�@(	�@'�[@'e�@';d@&҉@&p;@&GE@&5?@&�@%�@%f�@%	l@$��@$��@$K^@$$@$G@$6@$*�@#��@#�@"��@"W�@"-@!��@!��@!�t@!��@!4@ �|@ ��@ z�@ `�@ 6@�m@��@g�@e�@g�@b�@Y@�L@��@��@�+@W�@@�^@��@^�@Dg@ \@��@�)@�9@|�@]d@?�@'R@�@��@��@t�@O@o@��@p;@H�@)�@�#@�d@��@k�@X@Q�@:�@@��@�I@y>@A�@x@�@�6@��@��@@O@�@��@��@_�@
�@�t@��@w2@T�@#�@�p@��@�?@z�@[�@N�@M@U2@6@�m@�q@s@RT@K�@6z@ i@�'@�}@� @Z�@	@��@�@�@�t@��@j@\�@B�@�/@�z@tT@,=@��@�m@�[@�$@RT@A�@�@��@h
@0U@��@�@��@m]@(�@�/@Ĝ@��@�@�.@A�@@��@��@��@�{@RT@&@
�@
�}@
�r@
�r@
H�@
$�@	��@	��@	c�@	Dg@	F@	J�@	:�@	�@�|@��@Ĝ@�o@1'@M@�@�
@��@��@t�@j�@dZ@W?1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aݕ�Aݐ.A݈�A�|�A�{�A�x�A�wfA�y>A�{JA�{�A�|�A�|�A�}�A��A݀ A݀�A݂A݃GA݃�A݁�A�~�A�~(A�~�A�.A�R�A��Aٯ�A��A��TA���A�6�A�xAʺ^A�ʌAŴ�Aċ�A��A�c�A�5�A�8�A��hA���A��RA��A�7�A���A��A��A��fA�|PA��A��FA���A�oiA�$tA�FtA��<A��}A���A�خA�	7A��sA��6A���A���A�"�A�bA���A��}A�2�A�9�A��A��A���A��WA��kA��qA�ƨA�A��%A��A��RA��A~�{A{{�Ay�}Aw��Aq��Ap]�Am�Agu�AbG�A\e,AX8�AU��ATw2AQ��AP*�AM�mAL�AK�oAH�AE�gADo�AB1�A@�:A?��A>�7A>!A<A:�fA:YA8��A8j�A7w�A5��A3,=A1�YA1L�A0��A/�EA.�A.�A-�A,�RA,�A+��A+a�A*��A*D�A)�A(�A'�	A'VA&}VA%~(A$�A$)_A#��A#�7A#��A#��A"��A!��A �A OvA��A��A�~A_A�A	A�MAr�A%�Ar�Ar�A&�A��A�A=�A�@A8A�A�A��AqAl�AW?AS�A��A��A�A��A��A�A�XAZA4�Ac�A6zA��ARTA�sA��Ag8AuA5?A
$tA	!A�gAn�AA�A��A%�A'�A��A��AK�A�AɆA:�A��A-A�AxA!�A͟AxlA8�A�A�jA ��A �oA ?@��@�c�@�&@���@��&@���@���@�,=@�@��&@�8�@��@�]�@���@�xl@��@���@�?}@�@��@�P@�@��@��3@��@�p�@��@�q@���@��2@�l"@��@��9@�F@��@�$t@� @��K@�L�@���@�}V@��@�F�@���@�F@�5?@��@��@Ṍ@�c�@�d�@�&@�PH@ݻ0@�q@ܫ6@�&�@��g@�1�@�M�@ٟV@���@�($@��N@�{J@֡b@�-�@է�@�+@��5@��s@Զ�@��@�%@�V�@��@��@ы�@��@�Q@���@�o�@�X@�@�N�@́@�c�@��@�ff@�6@˄M@�	l@�1�@ʕ@��@�'�@Ƚ<@ȵ�@��@�y>@ǫ�@�(@�e�@��@Š�@�q@��E@�u@Ì~@�'�@�`�@���@�hs@��@�-@���@�,�@��@�Vm@�\�@�=�@��@���@�
�@�(@�?�@��@�Z�@�C-@��@��$@�ی@��W@��0@�=�@��x@�!@��r@��#@���@���@���@���@�t�@��@��@���@��@���@��	@�:�@���@��@�33@�#�@��@��b@�Z@�y>@�~(@��@�[�@��+@��@@���@�\�@��@�tT@�1�@�@��@��I@�_@�C�@�#:@�"h@��g@�q@���@�C-@��+@�|@�)_@���@�h�@���@��x@�:*@��W@���@��/@���@�bN@�A�@���@���@�\�@��M@���@��@�W�@�~@��Q@��@�g�@�=@��@��M@�=q@�˒@�0�@��,@�J�@�/@���@��'@��b@�Q@��.@��C@��V@���@���@��4@�X�@�ߤ@��@���@�t�@�m]@�K�@��@�W�@��@��@���@�%F@���@��@���@�;�@�*�@�
�@��Q@���@�^�@��1@�S�@�(�@��@���@���@��@�^�@�/@���@���@�|�@� �@���@��^@�v`@�E9@��@��9@���@�Ft@�O@��K@�t�@��@��@��z@�}V@�6�@���@�m]@�V@���@���@��+@�y>@�q@�]d@��@���@��@��@���@�G�@��@��K@���@���@�B[@��@��@���@�P�@�@��c@���@���@���@��:@�=�@��@���@�g8@�4@��H@�e�@�;d@��@���@���@�D�@��@�\�@��@���@��I@���@�y>@�L0@��@��A@�o�@�*0@���@�Ɇ@���@���@���@��\@�ff@��@���@�zx@�dZ@�S�@�N<@�IR@��@���@��v@���@�PH@�6�@��@��o@��@��@��|@��}@���@�_@�1'@��@j�@@~;�@}�@}Vm@} \@|�@|e�@{��@{y�@z��@zq�@z#:@y��@x��@x��@x�@x`�@x?�@x�@wE9@v�+@u��@uG�@u/@u�@t��@tA�@s�@rc @rGE@r?@q�@q(�@poi@oݘ@o1�@n͟@nz@n�@m�@l��@l%�@k�@kn/@j�c@j��@jB[@i��@iL�@hbN@g�@@g+@fl�@f1�@f@ep�@e:�@d�@d�.@dM@c�0@c\)@cC@b��@bQ@a�3@`�[@_�W@_�@_g�@^�2@^�R@^�@^Ov@]�d@]|@]Dg@\�@[�q@[A�@[�@Z?@Ym]@YF@Y!�@X|�@X	�@W�g@W��@Wn/@V�]@Vp;@V0U@U�o@UA @U�@T�[@T>B@S��@Sb�@SO@S�@R��@RV@Q��@Q�H@Q�'@Q�M@Qa�@QDg@Q�@Py>@O�Q@Oj�@O�@N�,@N8�@M�3@MJ�@L��@L�@L��@LZ@L�@K�
@K�V@KW?@J��@JL0@I��@Iw2@H�/@H%�@G�@Gخ@G�@G��@G��@G��@GZ�@G6z@GC@F�c@F��@FJ�@E��@E��@E�7@Ec�@D��@D�?@D��@Dc�@D?�@D"h@C��@C�V@C!-@B�6@A�o@A}�@AB�@@��@@�@@[�@@"h@?�F@?dZ@?
=@>�R@>3�@=�@=�C@=x�@=T�@= \@<�@<�9@<�@;��@;|�@;J#@;�@:��@:M�@:e@9�>@9��@8��@8�p@8��@8�@8V�@7��@76z@6��@6��@6�b@6u%@60U@5�N@5�'@5��@5�h@5�@4��@41'@3|�@3P�@3�@2s�@2&�@1��@1c@1#�@0��@0�j@0>B@/��@/�@/"�@/�@.�c@.�6@.��@.E�@-�^@-m]@,w�@,I�@,�@+�6@+�:@+j�@+@O@+�@*��@*E�@*!�@*@)zx@)�@(�U@(�.@(c�@(	�@'�[@'e�@';d@&҉@&p;@&GE@&5?@&�@%�@%f�@%	l@$��@$��@$K^@$$@$G@$6@$*�@#��@#�@"��@"W�@"-@!��@!��@!�t@!��@!4@ �|@ ��@ z�@ `�@ 6@�m@��@g�@e�@g�@b�@Y@�L@��@��@�+@W�@@�^@��@^�@Dg@ \@��@�)@�9@|�@]d@?�@'R@�@��@��@t�@O@o@��@p;@H�@)�@�#@�d@��@k�@X@Q�@:�@@��@�I@y>@A�@x@�@�6@��@��@@O@�@��@��@_�@
�@�t@��@w2@T�@#�@�p@��@�?@z�@[�@N�@M@U2@6@�m@�q@s@RT@K�@6z@ i@�'@�}@� @Z�@	@��@�@�@�t@��@j@\�@B�@�/@�z@tT@,=@��@�m@�[@�$@RT@A�@�@��@h
@0U@��@�@��@m]@(�@�/@Ĝ@��@�@�.@A�@@��@��@��@�{@RT@&@
�@
�}@
�r@
�r@
H�@
$�@	��@	��@	c�@	Dg@	F@	J�@	:�@	�@�|@��@Ĝ@�o@1'@M@�@�
@��@��@t�@j�@dZ@W?1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B�B�NB�4B�B��B��B��B�4B�B�4B�NB�hB�B�B�B��B�B�B�hB��B��B��B��B�
B�B	�B	k�B	w2B	�B	�B	��B	� B
*B
cTB
�B
�B
��B
�B
ΥB�B"�B0�B6`B?BC�BO�BZBj�Bv�Bz^B|�B�B{B��B��B�?B�oBk�Bc�BqvB~�B}�BzBshBoOBfBPHB;dB
�B
�'B
�NB
�B
�mB
�FB
��B
w�B
lWB
S&B
0�B
�B	��B	��B	ȀB	��B	��B	��B	��B	y�B	d�B	DB	)�B	BB	�B	 iB��B��B�5B��B�ZB�\B�HB�QB�,B�"B�	B�1B��B�MB��B�%B��B̳BˬB�jB��BŢB�{B�B�-B�aB��B��B�VB��B�<B	�B	�B	KB	 �B	*0B	0�B	4�B	:�B	<jB	F�B	I�B	HB	HKB	_�B	�B	�AB	��B	��B	}qB	y�B	tTB	oB	_;B	LdB	G�B	BAB	=�B	B[B	FB	f�B	~�B	�{B	��B	�B	��B	�WB	�0B	�0B	��B	�GB	��B	��B	�B	��B	��B	�'B	�B	��B	�fB	��B	��B	�	B	�qB	�%B	�%B	��B	��B	�6B	�VB	��B	̳B	��B	�9B	�B	��B	��B	�pB	�)B	�lB	ɺB	�B	��B	οB	οB	οB	̈́B	̳B	��B	��B	�B	ʦB	�jB	�pB	�B	ΊB	͟B	�DB	ɆB	ɺB	ʦB	�^B	ϫB	� B	��B	�NB	��B	�:B	�@B	�oB	��B	�NB	� B	��B	ңB	�B	�{B	өB	�,B	�$B	�mB	ؓB	�B	��B	�B	�!B	ݘB	��B	��B	��B	�_B	ּB	�aB	��B	� B	�B	ӏB	�&B	ңB	��B	��B	՛B	յB	��B	��B	׍B	רB	��B	��B	�9B	՛B	�2B	�B	�2B	�B	��B	��B	��B	�KB	�EB	�EB	��B	خB	��B	��B	��B	�KB	�B	ٴB	�7B	�B	ٴB	��B	ٚB	ٚB	��B	ںB	�WB	یB	�]B	�]B	��B	�'B	�-B	��B	ݲB	�/B	�dB	��B	ߊB	�\B	�B	��B	��B	��B	�B	�B	��B	��B	�KB	�KB	��B	�B	�B	��B	��B	��B	�B	�"B	�WB	�kB	�B	�'B	�B	�B	�'B	��B	�B	�UB	�B	�MB	��B	�|B	�MB	�B	��B	�B	�tB	��B	��B	�-B	�%B	�B	��B	�B	�B	��B	�[B	�B	�B	�9B	�ZB	��B	��B	�tB	�B	�tB	��B	��B	��B	��B	�B	�RB	��B	��B	�B	�^B	�JB	�VB	��B	�B	�cB
 B
 �B
 �B
 B
 �B	��B	��B	�BB	�"B	�jB	�B	�B	�}B
�B
UB
 B
 �B
�B
AB
�B
�B
�B
�B
�B
	RB
B

�B

�B

�B

	B

	B

#B
~B
�B
xB
DB
B
dB
PB
B
VB
�B
�B
4B
�B
B
�B
�B
2B
�B
�B
B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
yB
�B
KB
KB
eB
�B
kB
�B
�B
#B
WB
=B
WB
WB
=B
�B
xB
)B
xB
�B
]B
CB
)B
�B
B
]B
CB
�B
�B
�B
/B
�B
~B
5B
�B
�B
�B
!B
pB
�B
VB
!B
�B
�B
�B
 �B
 �B
!�B
!�B
!�B
!�B
"�B
#�B
$�B
%`B
%�B
%�B
&2B
&fB
&�B
&�B
'B
(�B
)B
)yB
)_B
(�B
(�B
)DB
)�B
)�B
*eB
*B
+B
+B
+QB
+�B
,B
,qB
,�B
-)B
-�B
-�B
-�B
-�B
.IB
.�B
.�B
/5B
/�B
0�B
0�B
0�B
0�B
0�B
0�B
1B
1�B
1�B
2GB
2|B
2|B
2�B
2�B
2|B
2�B
3MB
3�B
3�B
3�B
3�B
3�B
3�B
4B
4B
4B
4�B
4�B
4�B
4�B
4�B
4�B
5?B
5�B
5�B
5�B
6B
6`B
6+B
6B
5�B
6�B
6`B
6�B
6�B
6�B
6�B
6�B
7�B
8B
7�B
8B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
9XB
9�B
:xB
:^B
:^B
:xB
;B
:�B
;�B
<6B
<B
<B
<jB
<�B
=VB
=�B
>B
>B
>(B
>BB
>BB
>�B
>�B
?HB
?}B
@B
@OB
@OB
@�B
@�B
A�B
A�B
BuB
B�B
B�B
B�B
CGB
CGB
C�B
C�B
C�B
DB
DMB
D�B
D�B
D�B
D�B
ESB
E�B
E�B
FB
FtB
FtB
FtB
F�B
F�B
F�B
F�B
GzB
G�B
G�B
G�B
H�B
IB
IB
IB
I�B
I�B
I�B
J	B
J#B
J�B
KB
KB
KDB
K�B
K�B
K�B
LdB
L�B
L�B
MB
MB
M6B
M�B
M�B
NB
N"B
N<B
NVB
N<B
N�B
OB
OvB
O�B
O�B
PB
P�B
P�B
QNB
Q�B
QhB
Q�B
Q�B
R B
R B
RTB
RTB
R�B
S&B
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
T�B
T�B
UB
U2B
UMB
U�B
U�B
VB
VB
VB
VmB
V�B
V�B
V�B
V�B
V�B
W$B
WYB
W�B
W�B
XyB
X�B
X�B
YB
Y1B
YeB
YeB
Y�B
Y�B
ZB
Z7B
Z�B
Z�B
[	B
[	B
[#B
[=B
[WB
[WB
[qB
[�B
\CB
\CB
\xB
\�B
]/B
]IB
]dB
]�B
^B
]�B
^B
^5B
^5B
^�B
_;B
_VB
_pB
_�B
_�B
_�B
`'B
`B
`B
`B
`vB
`�B
`�B
abB
abB
a�B
a�B
bB
b�B
b�B
b�B
b�B
c B
c�B
c�B
d@B
d�B
d�B
d�B
d�B
d�B
eFB
e�B
e�B
gB
gB
g8B
g�B
g�B
g�B
g�B
g�B
hXB
h�B
h�B
h�B
iyB
i�B
i�B
i�B
i�B
jB
j�B
j�B
j�B
k�B
l"B
l=B
lqB
lWB
l=B
l�B
m)B
m]B
m�B
m�B
n/B
o B
pB
pUB
p!B
o�B
o B
o5B
oOB
pB
p!B
p!B
pB
p;B
pUB
p�B
qB
qAB
q�B
q�B
q�B
rGB
s3B
tB
tB
tB
t9B
t�B
t�B
t�B
u?B
u?B
u?B
u�B
u�B
u�B
u�B
vFB
v�B
v�B
wB
wB
w2B
w2B
w�B
xB
xRB
xlB
x�B
x�B
y>B
yXB
y�B
y�B
y�B
y�B
y�B
y�B
zB
zDB
z^B
zxB
z�B
z�B
{B
{JB
{B
{�B
{�B
{�B
{�B
|6B
|6B
|B
|jB
|�B
|PB
|jB
|�B
|�B
|�B
|�B
|�B
}"B
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~]B
~wB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
�B
�B
�OB
�B
�B
��B
��B
��B
� B
��B
�B
�B
�AB
�uB
�[B
�AB
��B
��B
��B
�-B
�{B
��B
��B
��B
��B
��B
��B
��B
�B
�3B
�3B
��B
��B
��B
��B
�9B
��B
��B
��B
��B
��B
��B
��B
�B
�%B
�?B
��B
�YB
�tB
��B
��B
��B
��B
�B
��B
��B
�EB
��B
��B
��B
�B
�fB
�K1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B�B�NB�4B�B��B��B��B�4B�B�4B�NB�hB�B�B�B��B�B�B�hB��B��B��B��B�
B�B	�B	k�B	w2B	�B	�B	��B	� B
*B
cTB
�B
�B
��B
�B
ΥB�B"�B0�B6`B?BC�BO�BZBj�Bv�Bz^B|�B�B{B��B��B�?B�oBk�Bc�BqvB~�B}�BzBshBoOBfBPHB;dB
�B
�'B
�NB
�B
�mB
�FB
��B
w�B
lWB
S&B
0�B
�B	��B	��B	ȀB	��B	��B	��B	��B	y�B	d�B	DB	)�B	BB	�B	 iB��B��B�5B��B�ZB�\B�HB�QB�,B�"B�	B�1B��B�MB��B�%B��B̳BˬB�jB��BŢB�{B�B�-B�aB��B��B�VB��B�<B	�B	�B	KB	 �B	*0B	0�B	4�B	:�B	<jB	F�B	I�B	HB	HKB	_�B	�B	�AB	��B	��B	}qB	y�B	tTB	oB	_;B	LdB	G�B	BAB	=�B	B[B	FB	f�B	~�B	�{B	��B	�B	��B	�WB	�0B	�0B	��B	�GB	��B	��B	�B	��B	��B	�'B	�B	��B	�fB	��B	��B	�	B	�qB	�%B	�%B	��B	��B	�6B	�VB	��B	̳B	��B	�9B	�B	��B	��B	�pB	�)B	�lB	ɺB	�B	��B	οB	οB	οB	̈́B	̳B	��B	��B	�B	ʦB	�jB	�pB	�B	ΊB	͟B	�DB	ɆB	ɺB	ʦB	�^B	ϫB	� B	��B	�NB	��B	�:B	�@B	�oB	��B	�NB	� B	��B	ңB	�B	�{B	өB	�,B	�$B	�mB	ؓB	�B	��B	�B	�!B	ݘB	��B	��B	��B	�_B	ּB	�aB	��B	� B	�B	ӏB	�&B	ңB	��B	��B	՛B	յB	��B	��B	׍B	רB	��B	��B	�9B	՛B	�2B	�B	�2B	�B	��B	��B	��B	�KB	�EB	�EB	��B	خB	��B	��B	��B	�KB	�B	ٴB	�7B	�B	ٴB	��B	ٚB	ٚB	��B	ںB	�WB	یB	�]B	�]B	��B	�'B	�-B	��B	ݲB	�/B	�dB	��B	ߊB	�\B	�B	��B	��B	��B	�B	�B	��B	��B	�KB	�KB	��B	�B	�B	��B	��B	��B	�B	�"B	�WB	�kB	�B	�'B	�B	�B	�'B	��B	�B	�UB	�B	�MB	��B	�|B	�MB	�B	��B	�B	�tB	��B	��B	�-B	�%B	�B	��B	�B	�B	��B	�[B	�B	�B	�9B	�ZB	��B	��B	�tB	�B	�tB	��B	��B	��B	��B	�B	�RB	��B	��B	�B	�^B	�JB	�VB	��B	�B	�cB
 B
 �B
 �B
 B
 �B	��B	��B	�BB	�"B	�jB	�B	�B	�}B
�B
UB
 B
 �B
�B
AB
�B
�B
�B
�B
�B
	RB
B

�B

�B

�B

	B

	B

#B
~B
�B
xB
DB
B
dB
PB
B
VB
�B
�B
4B
�B
B
�B
�B
2B
�B
�B
B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
yB
�B
KB
KB
eB
�B
kB
�B
�B
#B
WB
=B
WB
WB
=B
�B
xB
)B
xB
�B
]B
CB
)B
�B
B
]B
CB
�B
�B
�B
/B
�B
~B
5B
�B
�B
�B
!B
pB
�B
VB
!B
�B
�B
�B
 �B
 �B
!�B
!�B
!�B
!�B
"�B
#�B
$�B
%`B
%�B
%�B
&2B
&fB
&�B
&�B
'B
(�B
)B
)yB
)_B
(�B
(�B
)DB
)�B
)�B
*eB
*B
+B
+B
+QB
+�B
,B
,qB
,�B
-)B
-�B
-�B
-�B
-�B
.IB
.�B
.�B
/5B
/�B
0�B
0�B
0�B
0�B
0�B
0�B
1B
1�B
1�B
2GB
2|B
2|B
2�B
2�B
2|B
2�B
3MB
3�B
3�B
3�B
3�B
3�B
3�B
4B
4B
4B
4�B
4�B
4�B
4�B
4�B
4�B
5?B
5�B
5�B
5�B
6B
6`B
6+B
6B
5�B
6�B
6`B
6�B
6�B
6�B
6�B
6�B
7�B
8B
7�B
8B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
9XB
9�B
:xB
:^B
:^B
:xB
;B
:�B
;�B
<6B
<B
<B
<jB
<�B
=VB
=�B
>B
>B
>(B
>BB
>BB
>�B
>�B
?HB
?}B
@B
@OB
@OB
@�B
@�B
A�B
A�B
BuB
B�B
B�B
B�B
CGB
CGB
C�B
C�B
C�B
DB
DMB
D�B
D�B
D�B
D�B
ESB
E�B
E�B
FB
FtB
FtB
FtB
F�B
F�B
F�B
F�B
GzB
G�B
G�B
G�B
H�B
IB
IB
IB
I�B
I�B
I�B
J	B
J#B
J�B
KB
KB
KDB
K�B
K�B
K�B
LdB
L�B
L�B
MB
MB
M6B
M�B
M�B
NB
N"B
N<B
NVB
N<B
N�B
OB
OvB
O�B
O�B
PB
P�B
P�B
QNB
Q�B
QhB
Q�B
Q�B
R B
R B
RTB
RTB
R�B
S&B
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
T�B
T�B
UB
U2B
UMB
U�B
U�B
VB
VB
VB
VmB
V�B
V�B
V�B
V�B
V�B
W$B
WYB
W�B
W�B
XyB
X�B
X�B
YB
Y1B
YeB
YeB
Y�B
Y�B
ZB
Z7B
Z�B
Z�B
[	B
[	B
[#B
[=B
[WB
[WB
[qB
[�B
\CB
\CB
\xB
\�B
]/B
]IB
]dB
]�B
^B
]�B
^B
^5B
^5B
^�B
_;B
_VB
_pB
_�B
_�B
_�B
`'B
`B
`B
`B
`vB
`�B
`�B
abB
abB
a�B
a�B
bB
b�B
b�B
b�B
b�B
c B
c�B
c�B
d@B
d�B
d�B
d�B
d�B
d�B
eFB
e�B
e�B
gB
gB
g8B
g�B
g�B
g�B
g�B
g�B
hXB
h�B
h�B
h�B
iyB
i�B
i�B
i�B
i�B
jB
j�B
j�B
j�B
k�B
l"B
l=B
lqB
lWB
l=B
l�B
m)B
m]B
m�B
m�B
n/B
o B
pB
pUB
p!B
o�B
o B
o5B
oOB
pB
p!B
p!B
pB
p;B
pUB
p�B
qB
qAB
q�B
q�B
q�B
rGB
s3B
tB
tB
tB
t9B
t�B
t�B
t�B
u?B
u?B
u?B
u�B
u�B
u�B
u�B
vFB
v�B
v�B
wB
wB
w2B
w2B
w�B
xB
xRB
xlB
x�B
x�B
y>B
yXB
y�B
y�B
y�B
y�B
y�B
y�B
zB
zDB
z^B
zxB
z�B
z�B
{B
{JB
{B
{�B
{�B
{�B
{�B
|6B
|6B
|B
|jB
|�B
|PB
|jB
|�B
|�B
|�B
|�B
|�B
}"B
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~]B
~wB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
�B
�B
�OB
�B
�B
��B
��B
��B
� B
��B
�B
�B
�AB
�uB
�[B
�AB
��B
��B
��B
�-B
�{B
��B
��B
��B
��B
��B
��B
��B
�B
�3B
�3B
��B
��B
��B
��B
�9B
��B
��B
��B
��B
��B
��B
��B
�B
�%B
�?B
��B
�YB
�tB
��B
��B
��B
��B
�B
��B
��B
�EB
��B
��B
��B
�B
�fB
�K1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104848  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172522  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172522  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172522                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022530  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605022530  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131506                      G�O�G�O�G�O�                