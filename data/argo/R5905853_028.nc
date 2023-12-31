CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:27:38Z creation;2022-06-04T17:27:38Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604172738  20220802011501  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @����1   @��l�d @-�1&�x��c��l�D1   GPS     A   B   B   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B��B��B(  B0  B9��B>ffBH  BP  BX  B`  Bh  Bp��Bw��B�  B�  B�  B�33B���B���B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  Bę�BǙ�B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  CffC	�fC�fC  C  C  C�fC  C  C  C  C  C�fC"  C$  C&  C(  C*  C,  C.�C0�C2  C4  C5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C^  C`�Cb�Cc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1fD1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D��3D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�H@���@���A Q�A Q�A@Q�Aa�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B{B�B�B�B({B0{B9�B>z�BH{BP{BX{B`{Bh{Bp�HBw�B�
=B�
=B�
=B�=pB��
B��
B�=pB���B��
B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=Bģ�Bǣ�B�
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
=C CCCCk�C	�C�CCCC�CCCCCC�C"C$C&C(C*C,C.�C0�C2C4C5�C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCW�CZC\C^C`�Cb�Cc�CfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1�D1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�@�D怤D���D� �D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D��>11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�x8A�xA�zA�z�A�{A��4A��oA�x�A�~�A��A���A��_A��A��rA��lA��A��(A�� A�_�A�&LAߔFA�IA��A�c�A��dAՆYAҐ�AЯ�A�OBA˶zA��(A���A��AŜCA��A��5A��$A���A��OA�EA���A��A��6A���A��A�!-A��A�!�A���A��;A���A��A�>�A��A��A��A�qA��A���A��`A���A��A��-A��A���A�U�A��A��@A���A��A�+6A��A�1�A�	A���A�XA�~�A��BA��A�@A|4Ay-wAu�Ar&�Am��Aj�Ag�zAe��Ad^�Ab�A_J�AZe�AW��AUi�AQ�}AO�.AN[�AL�AJ�AH�yAG�AD(�ABL�A>��A;�rA:MjA8$tA5�A2�XA0.�A.��A-y�A-�A,�DA+r�A*l�A*�LA)_A'��A&�A$��A#A"kQA"1'A!?�A!?}A!C�A!(�A �.A!'RA �hA OvA ��A ��A ��A �A ҉A �A �wA!�A OAqvA5?A_�AxAffA�jA?�A�gA��A��AAu�A%FA1A�)A�?A(�A�A�A�tA�AffAB[A�A�9A�SA�A�PA�A�=AV�A�A�]A�AA
=A��A�:A:�A��A�:APHA
ѷA
�jA
��A
�A	�A	RTA	�A�pAG�AA�A��A?}A��A�-A�A�A�\A�vA<6A�TA�A]dA�>A��A~A�A�A��A_pA�'A��A��A�A �vA ��A /�@��@���@��@�{J@���@���@�IR@�;d@�!�@��@�)�@�4�@�J�@��q@�-w@���@��m@��@��4@��@�j@���@�c @���@�w2@�~@@��>@�o @��Q@�V@�+k@�7@�1@��@���@�K�@貖@�  @�[@�m]@�z@�L0@���@�\�@� i@��@��	@໙@��.@��@�4@�|�@ߥ@޲�@�1'@ݼ@ޤ�@�;@޴9@�خ@��`@�خ@ڂA@�O�@ؕ@�4n@��
@�� @���@׻0@ׂ�@��@�\�@�+k@�u@ռ�@՚k@�j�@�@ԉ�@�~@��z@Ӌ�@�_p@�a@�8�@�ߤ@�5?@�c@���@�c @�	@���@ϻ0@ϣn@�iD@�j@�e�@�W?@�A�@�	l@δ9@��@��/@�D�@˽�@ˏ�@�'�@ʴ9@��@�Dg@���@�h�@���@ǁ@�9�@���@�[�@���@ů�@�_p@��@ı�@�D�@��}@Úk@�&@�^5@��A@��^@���@�4@���@�m�@�$@��*@�+�@��R@�xl@�1'@��Q@���@�;d@���@�)�@���@���@�
=@�W�@���@�dZ@�+@��'@�z@�7@���@�=@���@��H@�+�@���@��@��=@��@�ȴ@���@�'R@�@�t�@�33@��@��m@�{�@�M@�#:@���@���@�F�@��@�e�@���@��@@�O�@�ں@��9@�m�@�#:@���@��#@��[@�Vm@��@��x@�@���@��@�8�@���@���@�R�@���@��"@�@�u�@��+@���@�<6@��f@���@�u%@�!@��@�\)@��@���@�z@�:*@���@��@�\)@�@��p@��@�S�@�#:@��@���@�s�@�33@���@��9@�j@�,=@��@��@�K�@�C@���@��@��5@��@���@��c@��m@��@�Q@���@�8�@���@�ff@��j@�~�@�e�@�O@��@��\@�?�@�4@��P@�J�@�q@�	l@��[@��Y@��@��o@��g@���@�F�@�	l@�ں@��h@���@�_�@��@���@���@�X@�@���@�g8@�:�@��@��@��@@�S&@�!�@��)@�v�@�c�@�2�@�خ@���@�j@�N<@��@��@�l"@�h�@�-@���@�@O@��@��@���@�w�@�M�@�!�@�"�@�xl@�Ta@��@���@�a@�+@���@���@��+@�6�@��@��@���@��S@�o @�[W@�҉@��@�Z@�/�@���@�}�@�IR@�Y@�Ɇ@�_@�-@��@�f�@�=@�,�@�@��@��v@��x@�h
@�Ta@�O@��>@�@���@��V@�Z�@��@�&�@�0@�@@�@~Z�@~O@}�t@}�@|�z@|[�@|?�@{�]@{�@zߤ@z�@z��@y�o@y�"@y	l@x�f@x�I@xM@x�@w��@w��@wy�@wC�@w$t@v�]@v{�@uc@t�/@t�@s��@siD@sS�@sH�@s@O@s$t@rȴ@r@�@r�@q�d@q�=@qO�@p�@p]d@o��@o��@n�@n)�@mw2@m@l�O@lz�@l@k�{@kX�@kK�@kY@j��@j� @j@�@i��@ip�@h�@h��@h�o@g�W@gK�@g,�@f�@f�X@f��@f4@e^�@d�K@dXy@d	�@c��@c�@c� @c��@c�@c\)@c�@b�R@a�@a�@`!@_{J@_o@^�r@^3�@]�@]�=@]s�@\�v@\6@\N�@\6@\1@[��@[v`@[U�@Z��@Z��@Zl�@Z��@ZH�@Y�t@X�|@X��@X%�@W��@WK�@V�y@V}V@U�>@U?}@T��@Tی@T�o@S��@S|�@S
=@R{�@Q�o@Q�@P�@P�)@P�e@PU2@P"h@PG@Oe�@N҉@Nc @M��@M��@M��@MB�@L��@Lh�@L  @K�{@K@J�s@J{�@J�@I�@I�@H��@H��@H�D@H �@G�w@G�@F�c@F�'@F��@Fv�@FL0@F	@E��@E�@ES&@E;@D�?@D�@Dm�@D%�@D  @C��@Cl�@C=@C,�@B�y@B�6@Bi�@B:*@A��@AVm@A�@@�u@@[�@@'R@@�@?�Q@?��@?S�@?o@>�M@>͟@>��@>@�@=�@=k�@<��@<��@<-�@;�@;�{@;Mj@:��@:�}@:Ta@:�@9�j@9�'@9�@8ی@8��@8_@8/�@8~@8b@8�@7�q@6�@6s�@6@5��@5J�@4�`@4�u@4]d@4(�@4x@3��@3Mj@3H�@3>�@2�B@2� @2p;@2J�@1��@1�=@1<6@0�@0�.@0M@0  @/��@/!-@.��@.p;@-��@-�-@-��@-Dg@,��@,��@,w�@,_@,	�@+��@+��@+iD@+.I@*�s@*�@*�@*($@)�j@)�t@)q@(��@(u�@(:�@'�@'s@'6z@&�y@&�R@&�A@&V@&;�@&�@%�T@%��@%\�@%S&@%�@$��@$~(@$Xy@$@#˒@#b�@#"�@"��@"��@"R�@"O@"_@!��@!��@!��@!zx@![W@!�@ �@ �@ �@ �D@ Xy@ D�@ ~@��@�V@qv@F�@(@�B@��@u%@?@�.@�N@�'@��@0�@�|@��@�`@��@2�@��@��@�V@��@~�@F�@�m@��@R�@J�@C�@)�@�9@rG@Q�@J�@4@&�@�f@��@j@!@�@��@ݘ@�@��@��@��@��@t�@S�@=@'�@��@�\@i�@3�@�D@�>@�@m]@[W@%F@!�@�@��@�@��@�@�K@@O@)_@o@�H@ȴ@�L@{�@8�@{@�@��@�z@�~@j@hs@a�@	l@�9@��@c�@Xy@I�@9X@$@�A@��@�@��@X�@�@�c@�@��@R�@�@�o@�N@�@�M@=�@�@֡@��@|�@S�@Ft@*�@b@�]@�+@��@��@��@o�11111111111111111111111111111111111111111111111111111111111111111441444441111111111111144411111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�x8A�xA�zA�z�A�{A��4A��oA�x�A�~�A��A���A��_A��A��rA��lA��A��(A�� A�_�A�&LAߔFA�IA��A�c�A��dAՆYAҐ�AЯ�A�OBA˶zA��(A���A��AŜCA��A��5A��$A���A��OA�EA���A��A��6A���A��A�!-A��A�!�A���A��;A���A��A�>�A��A��A��A�qA��A���A��`A���A��A��-A��A���A�U�A��A��@A���A��A�+6A��A�1�A�	A���A�XA�~�A��BA��A�@A|4Ay-wAu�Ar&�Am��Aj�Ag�zAe��Ad^�Ab�A_J�AZe�AW��AUi�AQ�}AO�.AN[�AL�AJ�AH�yAG�AD(�ABL�A>��A;�rA:MjA8$tA5�A2�XA0.�A.��A-y�A-�A,�DA+r�A*l�A*�LA)_A'��A&�A$��A#A"kQA"1'A!?�A!?}A!C�A!(�A �.A!'RA �hA OvA ��A ��A ��A �A ҉A �A �wA!�A OAqvA5?A_�AxAffA�jA?�A�gA��A��AAu�A%FA1A�)A�?A(�A�A�A�tA�AffAB[A�A�9A�SA�A�PA�A�=AV�A�A�]A�AA
=A��A�:A:�A��A�:APHA
ѷA
�jA
��A
�A	�A	RTA	�A�pAG�AA�A��A?}A��A�-A�A�A�\A�vA<6A�TA�A]dA�>A��A~A�A�A��A_pA�'A��A��A�A �vA ��A /�@��@���@��@�{J@���@���@�IR@�;d@�!�@��@�)�@�4�@�J�@��q@�-w@���@��m@��@��4@��@�j@���@�c @���@�w2@�~@@��>@�o @��Q@�V@�+k@�7@�1@��@���@�K�@貖@�  @�[@�m]@�z@�L0@���@�\�@� i@��@��	@໙@��.@��@�4@�|�@ߥ@޲�@�1'@ݼ@ޤ�@�;@޴9@�خ@��`@�خ@ڂA@�O�@ؕ@�4n@��
@�� @���@׻0@ׂ�@��@�\�@�+k@�u@ռ�@՚k@�j�@�@ԉ�@�~@��z@Ӌ�@�_p@�a@�8�@�ߤ@�5?@�c@���@�c @�	@���@ϻ0@ϣn@�iD@�j@�e�@�W?@�A�@�	l@δ9@��@��/@�D�@˽�@ˏ�@�'�@ʴ9@��@�Dg@���@�h�@���@ǁ@�9�@���@�[�@���@ů�@�_p@��@ı�@�D�@��}@Úk@�&@�^5@��A@��^@���@�4@���@�m�@�$@��*@�+�@��R@�xl@�1'@��Q@���@�;d@���@�)�@���@���@�
=@�W�@���@�dZ@�+@��'@�z@�7@���@�=@���@��H@�+�@���@��@��=@��@�ȴ@���@�'R@�@�t�@�33@��@��m@�{�@�M@�#:@���@���@�F�@��@�e�@���@��@@�O�@�ں@��9@�m�@�#:@���@��#@��[@�Vm@��@��x@�@���@��@�8�@���@���@�R�@���@��"@�@�u�@��+@���@�<6@��f@���@�u%@�!@��@�\)@��@���@�z@�:*@���@��@�\)@�@��p@��@�S�@�#:@��@���@�s�@�33@���@��9@�j@�,=@��@��@�K�@�C@���@��@��5@��@���@��c@��m@��@�Q@���@�8�@���@�ff@��j@�~�@�e�@�O@��@��\@�?�@�4@��P@�J�@�q@�	l@��[@��Y@��@��o@��g@���@�F�@�	l@�ں@��h@���@�_�@��@���@���@�X@�@���@�g8@�:�@��@��@��@@�S&@�!�@��)@�v�@�c�@�2�@�خ@���@�j@�N<@��@��@�l"@�h�@�-@���@�@O@��@��@���@�w�@�M�@�!�@�"�@�xl@�Ta@��@���@�a@�+@���@���@��+@�6�@��@��@���@��S@�o @�[W@�҉@��@�Z@�/�@���@�}�@�IR@�Y@�Ɇ@�_@�-@��@�f�@�=@�,�@�@��@��v@��x@�h
@�Ta@�O@��>@�@���@��V@�Z�@��@�&�@�0@�@@�@~Z�@~O@}�t@}�@|�z@|[�@|?�@{�]@{�@zߤ@z�@z��@y�o@y�"@y	l@x�f@x�I@xM@x�@w��@w��@wy�@wC�@w$t@v�]@v{�@uc@t�/@t�@s��@siD@sS�@sH�@s@O@s$t@rȴ@r@�@r�@q�d@q�=@qO�@p�@p]d@o��@o��@n�@n)�@mw2@m@l�O@lz�@l@k�{@kX�@kK�@kY@j��@j� @j@�@i��@ip�@h�@h��@h�o@g�W@gK�@g,�@f�@f�X@f��@f4@e^�@d�K@dXy@d	�@c��@c�@c� @c��@c�@c\)@c�@b�R@a�@a�@`!@_{J@_o@^�r@^3�@]�@]�=@]s�@\�v@\6@\N�@\6@\1@[��@[v`@[U�@Z��@Z��@Zl�@Z��@ZH�@Y�t@X�|@X��@X%�@W��@WK�@V�y@V}V@U�>@U?}@T��@Tی@T�o@S��@S|�@S
=@R{�@Q�o@Q�@P�@P�)@P�e@PU2@P"h@PG@Oe�@N҉@Nc @M��@M��@M��@MB�@L��@Lh�@L  @K�{@K@J�s@J{�@J�@I�@I�@H��@H��@H�D@H �@G�w@G�@F�c@F�'@F��@Fv�@FL0@F	@E��@E�@ES&@E;@D�?@D�@Dm�@D%�@D  @C��@Cl�@C=@C,�@B�y@B�6@Bi�@B:*@A��@AVm@A�@@�u@@[�@@'R@@�@?�Q@?��@?S�@?o@>�M@>͟@>��@>@�@=�@=k�@<��@<��@<-�@;�@;�{@;Mj@:��@:�}@:Ta@:�@9�j@9�'@9�@8ی@8��@8_@8/�@8~@8b@8�@7�q@6�@6s�@6@5��@5J�@4�`@4�u@4]d@4(�@4x@3��@3Mj@3H�@3>�@2�B@2� @2p;@2J�@1��@1�=@1<6@0�@0�.@0M@0  @/��@/!-@.��@.p;@-��@-�-@-��@-Dg@,��@,��@,w�@,_@,	�@+��@+��@+iD@+.I@*�s@*�@*�@*($@)�j@)�t@)q@(��@(u�@(:�@'�@'s@'6z@&�y@&�R@&�A@&V@&;�@&�@%�T@%��@%\�@%S&@%�@$��@$~(@$Xy@$@#˒@#b�@#"�@"��@"��@"R�@"O@"_@!��@!��@!��@!zx@![W@!�@ �@ �@ �@ �D@ Xy@ D�@ ~@��@�V@qv@F�@(@�B@��@u%@?@�.@�N@�'@��@0�@�|@��@�`@��@2�@��@��@�V@��@~�@F�@�m@��@R�@J�@C�@)�@�9@rG@Q�@J�@4@&�@�f@��@j@!@�@��@ݘ@�@��@��@��@��@t�@S�@=@'�@��@�\@i�@3�@�D@�>@�@m]@[W@%F@!�@�@��@�@��@�@�K@@O@)_@o@�H@ȴ@�L@{�@8�@{@�@��@�z@�~@j@hs@a�@	l@�9@��@c�@Xy@I�@9X@$@�A@��@�@��@X�@�@�c@�@��@R�@�@�o@�N@�@�M@=�@�@֡@��@|�@S�@Ft@*�@b@�]@�+@��@��@��@o�11111111111111111111111111111111111111111111111111111111111111111441444441111111111111144411111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B�hB�B�4B�bB�bB�.B�hB�oB��B�aB��B��B��B�SB�kB��B��BٚB	0�B	��B	��B	�uB	��B	�B	��B	� B	��B	�B	��B	ߤB	�B
)B
\)B
�B
��B
�aB
��B�B/iB,WB49BX+BW
BPBR�BCaBZB`'Bl�B��B��B�kB�B��B�;B{BfLBrGB,"B
��B
ðB
ȚB
�*B
��B�#BQ�B(XBK)B�=B}B��B�yB��B�B��B�B�xB�B+�B/�B]�B��B'B�B�cB]BՁBjB8�B�BxlBeB�B��B�B�zB��B��B��B9�B��B�>B�
B�]BSuB�B;�B|�B��B�RB	�B��B��B�6B	�B��B��B��B�QB�cB��B�VB	dB	�B	&�B	.�B	6+B	;B	:xB	C�B	V�B	abB	i�B	{B	�uB	��B	�NB	��B	�2B	��B	��B	��B	��B	��B	�B	��B	�>B	��B	�cB	�iB	�oB	�vB	��B	�AB	�ZB	�B	�MB	��B	�B	�TB	��B	�`B	�RB	�$B	��B	��B	�MB	��B	�ZB	�B	��B	�?B	��B	��B	��B	��B	�wB	B	��B	��B	ȴB	�)B	�HB	��B	�TB	�{B	��B	ՁB	ٴB	�B	�B	�'B	�B	��B	��B	�B	�*B	�eB	��B	��B	��B	�CB	�5B	�UB	�|B	�vB	��B	�?B	��B	��B	�dB	��B	�*B	��B	�xB	��B	�dB	�JB	��B	�JB	��B	��B	��B	�B	�fB	��B	��B	��B	��B	�B	��B	�>B	��B	�	B	��B	��B	�RB	�fB	�zB	��B	�ZB	�B	�nB	��B	�B	�LB	�hB	��B	�xB	��B	�LB	�B	�hB	�UB	�B	�UB	��B	�B	�-B	�9B	�TB	��B	�-B	�WB	��B	�6B	�CB	�UB	�oB	��B	�CB	�=B	�B	�;B	�LB	�B	�%B	�3B	��B	�B	��B	�&B	�B	�B	�B	�B	�B	�fB	�,B	�B	��B	�B	�sB	��B	�*B	��B	�B	��B	��B	�IB	�B	�B	��B	�B	�B	�vB	�[B	�UB	��B	�B	�3B	�B	�9B	��B	�FB	�B	�B	�'B	�B	��B	��B	��B	�RB	�=B	�B	�B	�'B	��B	�BB	��B	�B	�yB	רB	�4B	ϫB	�@B	�mB	�
B	�yB	�QB	��B	ؓB	רB	��B	ϑB	�BB	�B	��B	��B	�@B	�qB	�B	�B	�B	��B	�zB	�8B	��B	�xB	��B	�lB	�>B
 �B
�B	��B	�wB	�BB
 B
uB
 B
 �B
3B
 OB
 �B	�3B	�8B	��B	�B	�B	�B	�=B	�AB	�'B	��B	��B	�!B	�hB	�ZB	��B	��B	��B	��B	��B
UB
�B
B
�B
 �B
 �B
 OB	��B
�B
AB
UB
�B
�B
�B	�cB	�dB	�dB	��B	��B	��B	�B	�<B
�B
�B
[B
 �B
 �B
�B
	�B
�B
�B
�B
�B

�B
	�B

XB
B
�B
^B
B
	�B
_B
�B
�B

	B
�B
�B

�B

#B
�B
�B
�B
	RB

=B
	lB
�B
+B
mB
B
�B
	B
�B
B
B
B
�B
	7B
6B
\B
�B
�B
(B
\B
(B
(B
B
}B
}B
bB
B
 B
TB
�B
[B
�B
,B
B
MB
�B
�B
�B
�B
�B
�B
B
SB
9B
B
9B
�B
�B
�B
?B
�B
+B
�B
�B
�B
�B
B
�B
�B
�B
;B
;B
�B
 vB
 �B
!�B
"�B
"�B
"�B
#�B
#�B
$&B
$�B
%,B
%FB
%`B
%�B
&B
&B
%�B
&�B
&�B
&�B
&�B
&�B
'mB
'�B
'�B
'mB
(
B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
)DB
)�B
*B
*�B
+QB
+6B
+QB
+QB
+�B
+�B
,�B
-�B
-�B
-�B
-�B
-�B
-�B
.�B
/5B
0oB
0�B
1'B
1AB
1[B
1AB
1�B
1�B
1�B
1�B
1vB
2-B
2-B
2-B
2�B
3B
3�B
4B
4B
4TB
4�B
4�B
4�B
5%B
5?B
5ZB
5�B
5�B
5�B
6+B
6+B
6`B
6`B
6�B
6�B
6�B
6�B
6�B
7�B
8B
8�B
9	B
9	B
9$B
9$B
9$B
9	B
9rB
9�B
9�B
9�B
:*B
:DB
:�B
:�B
;B
;JB
;�B
<jB
<�B
=B
="B
="B
=VB
=�B
=�B
=�B
=�B
>B
>�B
>�B
>�B
?.B
?}B
?�B
?�B
@ B
@�B
@�B
@�B
@�B
@�B
A B
AoB
A�B
A�B
BB
BB
B'B
B'B
A�B
BAB
BAB
BAB
BuB
CB
B�B
B�B
A�B
BAB
BuB
B�B
B�B
B�B
B�B
CGB
C�B
DMB
D�B
D�B
DB
C�B
EB
D�B
E9B
ESB
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G+B
F�B
GEB
G+B
G�B
G�B
G�B
HfB
HB
G�B
H�B
I�B
I�B
JXB
JrB
JXB
J�B
K)B
KxB
KxB
K�B
L�B
MB
M�B
M�B
M�B
N"B
N<B
N�B
OB
OBB
O�B
O�B
O�B
P.B
P}B
P�B
QB
Q B
Q4B
QhB
Q�B
R:B
RTB
RTB
R�B
R�B
R�B
R�B
SB
S&B
S&B
SuB
S�B
S�B
S�B
S�B
S�B
TFB
T�B
T�B
T�B
T�B
UMB
UMB
UgB
VSB
V�B
V�B
W$B
W?B
W$B
W?B
W
B
W
B
W$B
WsB
WsB
WsB
WsB
W�B
W�B
XEB
X�B
YB
Y�B
Y�B
ZB
ZQB
Y�B
Z7B
ZkB
ZkB
Z�B
Z�B
[WB
[�B
[�B
[�B
[�B
\B
[�B
[�B
\�B
]B
]dB
]�B
^B
^jB
^�B
_!B
_�B
_�B
`B
`vB
`�B
`�B
`�B
a-B
abB
abB
a�B
a�B
a�B
a�B
a�B
b�B
b�B
b�B
b�B
cB
b�B
cnB
dtB
d�B
d�B
dZB
e,B
d�B
d�B
d�B
e�B
f2B
f�B
gB
gB
gmB
g�B
gmB
g�B
g�B
g�B
h>B
h�B
h�B
h�B
iB
i�B
jB
jKB
jB
j�B
j�B
j�B
j�B
kB
kB
kkB
k6B
k�B
k�B
k�B
k�B
l=B
l�B
l�B
mB
mCB
mB
nB
nB
nB
nB
n/B
ncB
n�B
n�B
o5B
o B
oB
o5B
oiB
o�B
o�B
o�B
o�B
pB
p!B
p;B
poB
p�B
p�B
p�B
qB
q[B
qvB
q�B
q�B
q�B
rB
r-B
rB
r-B
r�B
r�B
r�B
sB
r�B
sB
s3B
s�B
tB
tTB
t9B
t9B
tTB
t�B
t�B
u%B
u%B
u%B
u%B
uZB
u�B
u�B
u�B
u�B
vB
vB
v+B
v+B
v+B
v+B
vFB
vFB
v`B
v`B
vzB
v�B
wB
v�B
wB
w2B
w2B
w�B
w�B
w�B
x8B
xB
x8B
xB
xlB
x�B
yXB
y�B
zDB
z*B
z*B
z�B
z�B
z�B
z�B
z�B
z�B
{JB
{�B
{�B
{�B
{�B
{�B
{�B
|6B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}"B
}<B
}B
}�B
}�B
}�B
}�B
}�B
~B
~wB
~wB
~�B
~�B
~�B
~�B
cB
}B
�B
�B
� B
�4B
�4B
�OB
�iB
��B
��B
��B
��B
��B
�;11111111111111111111111111111111111111111111111111111111111111111444444444444444444444444444444444444444444444111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B�hB�B�4B�bB�bB�.B�hB�oB��B�aB��B��B��B�SB�kB��B��BٚB	0�B	��B	��B	�uB	��B	�B	��B	� B	��B	�B	��B	ߤB	�B
)B
\)B
�B
��B
�aB
��B�B/iB,WB49BX+BW
BPBR�BCaBZB`'Bl�B��B��B�kB�B��B�;B{BfLBrGB,"B
��B
ðB
ȚB
�*B
��B�#BQ�B(XBK)B�=B}B��B�yB��B�B��B�B�xB�B+�B/�B]�B��B'B�B�cB]BՁBjB8�B�BxlBeB�B��B�B�zB��B��B��B9�B��B�>B�
B�]BSuB�B;�B|�B��B�RB	�B��B��B�6B	�B��B��B��B�QB�cB��B�VB	dB	�B	&�B	.�B	6+B	;B	:xB	C�B	V�B	abB	i�B	{B	�uB	��B	�NB	��B	�2B	��B	��B	��B	��B	��B	�B	��B	�>B	��B	�cB	�iB	�oB	�vB	��B	�AB	�ZB	�B	�MB	��B	�B	�TB	��B	�`B	�RB	�$B	��B	��B	�MB	��B	�ZB	�B	��B	�?B	��B	��B	��B	��B	�wB	B	��B	��B	ȴB	�)B	�HB	��B	�TB	�{B	��B	ՁB	ٴB	�B	�B	�'B	�B	��B	��B	�B	�*B	�eB	��B	��B	��B	�CB	�5B	�UB	�|B	�vB	��B	�?B	��B	��B	�dB	��B	�*B	��B	�xB	��B	�dB	�JB	��B	�JB	��B	��B	��B	�B	�fB	��B	��B	��B	��B	�B	��B	�>B	��B	�	B	��B	��B	�RB	�fB	�zB	��B	�ZB	�B	�nB	��B	�B	�LB	�hB	��B	�xB	��B	�LB	�B	�hB	�UB	�B	�UB	��B	�B	�-B	�9B	�TB	��B	�-B	�WB	��B	�6B	�CB	�UB	�oB	��B	�CB	�=B	�B	�;B	�LB	�B	�%B	�3B	��B	�B	��B	�&B	�B	�B	�B	�B	�B	�fB	�,B	�B	��B	�B	�sB	��B	�*B	��B	�B	��B	��B	�IB	�B	�B	��B	�B	�B	�vB	�[B	�UB	��B	�B	�3B	�B	�9B	��B	�FB	�B	�B	�'B	�B	��B	��B	��B	�RB	�=B	�B	�B	�'B	��B	�BB	��B	�B	�yB	רB	�4B	ϫB	�@B	�mB	�
B	�yB	�QB	��B	ؓB	רB	��B	ϑB	�BB	�B	��B	��B	�@B	�qB	�B	�B	�B	��B	�zB	�8B	��B	�xB	��B	�lB	�>B
 �B
�B	��B	�wB	�BB
 B
uB
 B
 �B
3B
 OB
 �B	�3B	�8B	��B	�B	�B	�B	�=B	�AB	�'B	��B	��B	�!B	�hB	�ZB	��B	��B	��B	��B	��B
UB
�B
B
�B
 �B
 �B
 OB	��B
�B
AB
UB
�B
�B
�B	�cB	�dB	�dB	��B	��B	��B	�B	�<B
�B
�B
[B
 �B
 �B
�B
	�B
�B
�B
�B
�B

�B
	�B

XB
B
�B
^B
B
	�B
_B
�B
�B

	B
�B
�B

�B

#B
�B
�B
�B
	RB

=B
	lB
�B
+B
mB
B
�B
	B
�B
B
B
B
�B
	7B
6B
\B
�B
�B
(B
\B
(B
(B
B
}B
}B
bB
B
 B
TB
�B
[B
�B
,B
B
MB
�B
�B
�B
�B
�B
�B
B
SB
9B
B
9B
�B
�B
�B
?B
�B
+B
�B
�B
�B
�B
B
�B
�B
�B
;B
;B
�B
 vB
 �B
!�B
"�B
"�B
"�B
#�B
#�B
$&B
$�B
%,B
%FB
%`B
%�B
&B
&B
%�B
&�B
&�B
&�B
&�B
&�B
'mB
'�B
'�B
'mB
(
B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
)DB
)�B
*B
*�B
+QB
+6B
+QB
+QB
+�B
+�B
,�B
-�B
-�B
-�B
-�B
-�B
-�B
.�B
/5B
0oB
0�B
1'B
1AB
1[B
1AB
1�B
1�B
1�B
1�B
1vB
2-B
2-B
2-B
2�B
3B
3�B
4B
4B
4TB
4�B
4�B
4�B
5%B
5?B
5ZB
5�B
5�B
5�B
6+B
6+B
6`B
6`B
6�B
6�B
6�B
6�B
6�B
7�B
8B
8�B
9	B
9	B
9$B
9$B
9$B
9	B
9rB
9�B
9�B
9�B
:*B
:DB
:�B
:�B
;B
;JB
;�B
<jB
<�B
=B
="B
="B
=VB
=�B
=�B
=�B
=�B
>B
>�B
>�B
>�B
?.B
?}B
?�B
?�B
@ B
@�B
@�B
@�B
@�B
@�B
A B
AoB
A�B
A�B
BB
BB
B'B
B'B
A�B
BAB
BAB
BAB
BuB
CB
B�B
B�B
A�B
BAB
BuB
B�B
B�B
B�B
B�B
CGB
C�B
DMB
D�B
D�B
DB
C�B
EB
D�B
E9B
ESB
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G+B
F�B
GEB
G+B
G�B
G�B
G�B
HfB
HB
G�B
H�B
I�B
I�B
JXB
JrB
JXB
J�B
K)B
KxB
KxB
K�B
L�B
MB
M�B
M�B
M�B
N"B
N<B
N�B
OB
OBB
O�B
O�B
O�B
P.B
P}B
P�B
QB
Q B
Q4B
QhB
Q�B
R:B
RTB
RTB
R�B
R�B
R�B
R�B
SB
S&B
S&B
SuB
S�B
S�B
S�B
S�B
S�B
TFB
T�B
T�B
T�B
T�B
UMB
UMB
UgB
VSB
V�B
V�B
W$B
W?B
W$B
W?B
W
B
W
B
W$B
WsB
WsB
WsB
WsB
W�B
W�B
XEB
X�B
YB
Y�B
Y�B
ZB
ZQB
Y�B
Z7B
ZkB
ZkB
Z�B
Z�B
[WB
[�B
[�B
[�B
[�B
\B
[�B
[�B
\�B
]B
]dB
]�B
^B
^jB
^�B
_!B
_�B
_�B
`B
`vB
`�B
`�B
`�B
a-B
abB
abB
a�B
a�B
a�B
a�B
a�B
b�B
b�B
b�B
b�B
cB
b�B
cnB
dtB
d�B
d�B
dZB
e,B
d�B
d�B
d�B
e�B
f2B
f�B
gB
gB
gmB
g�B
gmB
g�B
g�B
g�B
h>B
h�B
h�B
h�B
iB
i�B
jB
jKB
jB
j�B
j�B
j�B
j�B
kB
kB
kkB
k6B
k�B
k�B
k�B
k�B
l=B
l�B
l�B
mB
mCB
mB
nB
nB
nB
nB
n/B
ncB
n�B
n�B
o5B
o B
oB
o5B
oiB
o�B
o�B
o�B
o�B
pB
p!B
p;B
poB
p�B
p�B
p�B
qB
q[B
qvB
q�B
q�B
q�B
rB
r-B
rB
r-B
r�B
r�B
r�B
sB
r�B
sB
s3B
s�B
tB
tTB
t9B
t9B
tTB
t�B
t�B
u%B
u%B
u%B
u%B
uZB
u�B
u�B
u�B
u�B
vB
vB
v+B
v+B
v+B
v+B
vFB
vFB
v`B
v`B
vzB
v�B
wB
v�B
wB
w2B
w2B
w�B
w�B
w�B
x8B
xB
x8B
xB
xlB
x�B
yXB
y�B
zDB
z*B
z*B
z�B
z�B
z�B
z�B
z�B
z�B
{JB
{�B
{�B
{�B
{�B
{�B
{�B
|6B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}"B
}<B
}B
}�B
}�B
}�B
}�B
}�B
~B
~wB
~wB
~�B
~�B
~�B
~�B
cB
}B
�B
�B
� B
�4B
�4B
�OB
�iB
��B
��B
��B
��B
��B
�;11111111111111111111111111111111111111111111111111111111111111111444444444444444444444444444444444444444444444111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104854  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172738  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172738  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172738                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022746  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605022746  QCF$                G�O�G�O�G�O�            4000JA  ARUP                                                                        20220610131506                      G�O�G�O�G�O�                JA  COOAcooa1.0                                                                 20220802003157  CF  PSAL            CffCff?�                  JA  COOAcooa1.0                                                                 20220802003157  CF  PSAL            C�fC.�?�                  JA  COOAcooa1.0                                                                 20220802003157  CF  PSAL            C5�fCR  ?�                  JA  COOAcooa1.0                                                                 20220802003157  CF  PSAL            CW�fC\  ?�                  JA  COOAcooa1.0                                                                 20220802003157  CF  PSAL_ADJUSTED   CffCff?�                  JA  COOAcooa1.0                                                                 20220802003157  CF  PSAL_ADJUSTED   C�fC.�?�                  JA  COOAcooa1.0                                                                 20220802003157  CF  PSAL_ADJUSTED   C5�fCR  ?�                  JA  COOAcooa1.0                                                                 20220802003157  CF  PSAL_ADJUSTED   CW�fC\  ?�                  JA  ARUP                                                                        20220802011501                      G�O�G�O�G�O�                