CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-11-23T00:35:28Z creation;2016-11-23T00:35:32Z conversion to V3.1;2019-12-19T08:21:08Z update;     
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
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20161123003528  20200116211515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               =A   JA  I2_0577_061                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��4y��1   @��5:� @3 qu��d�?|�h1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@���A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C  C  C  C�C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!�fD"fD"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� DrfDr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��fD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�=q@�p�A Q�A Q�AA�A`Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B{B�B{B {B({B0{B8{B@{BH{BP{BX{B`{Bh{Bp{Bx{B�
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
=B�
=B�
=B�
=B�
=B�
=B�=qB�
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
CCC�CCCC�CCCC C"C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD��DHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!��D"�D"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDr�Dr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�C�D���D���D� �D�@�D���D���D��qD�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D�}qD���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�@�D怤D���D� �D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D��D���D� �D�@�DꀤD���D� �D�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D��
D��=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��yA��yA��yA��mA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��`A���A�ƨAլA�v�A��A�S�A�%A��A��;AӶFAӲ-AӰ!AӰ!AӰ!Aө�Aӡ�Aӕ�A�z�A�VA�G�A�=qA�-AҶFA�33Aк^A�O�A�z�A���A��Aİ!A�VA�ƨA�7LA�7LA���A��-A���A���A�{A��A�bA��A�`BA��A�t�A��PA�I�A��A�|�A��A�S�A�C�A��A���A���A�{A�v�A�Q�A���A���A�n�A�33A�bNA��-A�A��`A��#A�9XA�Q�A��FA�ĜA�A���A�33A��^A�oA�&�A�=qA�|�A���A���A�VA���A�dZA��HA�hsA���A��A|�A{VAy�Aw`BAu�PAsVAq��Ao�Am�TAmK�Al�\AiƨAhjAf��Ae�
Ac�A`��A]�#A[�A[/AY�#AY"�AWC�AT�\AS�ARȴAQ;dAP�DAO;dANn�AM�AL�`AKAH��AGVAFffAE�#AC��ABAA33A?XA>A;�A7t�A7hsA7A5��A4�A3�hA2�A1��A0jA.��A-�-A-7LA,�+A+G�A*�A(�HA'ƨA'�A'VA&Q�A%;dA$�A$=qA"1'A �A�A��AVA��A��AXA�DA�A�RA�-A��A1'A7LAv�A�Al�A;dA�9A��A�A1'AS�AC�AC�A�\AdZA
5?A
{A	��A	S�A�-A��AoAȴA=qA��A�FA ��@��
@�A�@�p�@�K�@�S�@�!@�7@�{@�p�@�G�@�1'@�33@柾@��@��@�J@��@�@��#@��/@�A�@�l�@���@�j@���@׶F@�|�@և+@��@Ԭ@�J@мj@�7L@с@���@�1@ϥ�@ΰ!@���@́@̼j@�V@��`@�b@��@�$�@ź^@�9X@�=q@�j@�|�@��+@���@�j@��P@�n�@�`B@�Q�@�K�@���@�J@���@�`B@�9X@�\)@���@���@��\@�E�@���@�@���@�/@���@�z�@�(�@��F@�S�@��@�ff@��h@�X@�O�@�/@���@��@��@�Ĝ@���@�r�@�1'@��@��@�|�@�;d@��y@��\@�J@�G�@��/@���@�9X@�|�@��@�v�@�5?@��@��7@�?}@���@��u@�z�@�Q�@�b@�33@�
=@��y@���@�n�@�E�@�=q@�5?@��@�@�?}@�Q�@��;@�ƨ@��@���@�"�@��H@�ȴ@��R@�~�@���@���@��9@�1@��m@��
@���@�ƨ@���@��P@�;d@��@�@��H@���@�^5@���@���@�-@��^@�O�@��9@��u@�j@� �@�1'@� �@�A�@� �@�(�@��j@��7@��T@�@��@�ff@���@��#@�O�@���@��9@�r�@�(�@���@�+@��@��@���@���@��7@�o@��@�r�@��`@�X@�`B@�V@��/@�Ĝ@�Z@��;@�o@��
@���@�dZ@��y@���@�$�@��@���@�@��7@�p�@�X@�?}@���@�r�@�I�@�9X@�1@���@��@���@��@�+@���@�ff@�^5@�v�@�~�@�~�@��@��@��@��T@�@�?}@��/@���@�bN@�b@���@���@�l�@��@��+@�V@�5?@��#@��7@��@�Ĝ@��@���@��D@��@�j@�Z@�1'@��m@��
@�ƨ@�t�@��@��R@��@�p�@�O�@��@�z�@�1@�l�@���@���@�v�@�=q@�{@��T@�@��@�/@��@�Q�@�1@��@�|�@�S�@�+@�@���@�ȴ@���@�~�@�5?@�@�x�@�?}@�&�@��@��@�  @�P@�P@�P@l�@|�@l�@l�@l�@\)@;d@~�R@~5?@}/@|��@|(�@|1@{�F@{"�@z~�@z-@y��@y��@y�^@y��@yX@x��@x��@x�@xA�@x1'@w�@w�@w
=@v{@v{@v{@v{@v@up�@t�@t��@t1@s�m@s�
@sƨ@s�F@s��@r�@r�\@rM�@q��@q��@qX@q%@pĜ@p�9@p�9@p��@p��@p1'@n�y@n�R@nV@n5?@n@m�h@l��@l�@lz�@l(�@kS�@j�@j�\@j^5@j�@i��@i7L@h��@h1'@g�@g�P@g+@f�y@e�@e�@e�@d��@d1@cƨ@c��@c��@c��@c�@cdZ@co@b�H@b��@b�\@a��@ax�@`��@`r�@`A�@_��@_l�@_+@^�y@^��@]�-@]p�@]O�@]V@\�/@\��@\I�@[ƨ@[S�@["�@Z��@Z^5@Z�@Y�#@Yx�@Y&�@X��@X��@X�u@Xb@W�@W\)@V��@V�R@V��@Vff@U�@U�h@U�@T��@T(�@St�@SC�@S33@So@R��@RM�@RJ@Q��@Q��@RJ@RJ@Q�@Q��@Q�@P��@O�;@O��@Ol�@N�y@N@M�@M/@L�/@L�@L��@L�D@Lj@L9X@L9X@L9X@L�@K�
@KS�@J=q@Ix�@I&�@H��@H�`@H��@H�9@H�9@H�9@H�9@H�9@H�u@Hr�@H1'@Hb@G��@G\)@Fȴ@E�@E�T@E�-@E@E��@E`B@E/@E/@E�@E�@E�@E�@E�@E�@E�@D��@D�/@D�@Dj@D(�@C��@B�\@B^5@BM�@A��@A7L@@��@?|�@?+@?
=@>�@>��@>v�@>5?@=��@=�h@=�@=p�@=V@<�@<�j@<9X@;�F@;dZ@;o@:��@:�@9hs@9�@8�9@8��@8��@8�u@8�@8r�@8A�@81'@8b@7�@7�;@7��@7|�@7|�@7K�@6��@6V@6@5�-@5`B@5?}@4��@4�@4�/@4�/@4�/@4�@4z�@4j@4j@4I�@4(�@41@3��@3�
@3�F@3�F@3��@3��@3"�@2�@2�H@2�!@2J@1�^@1�7@0��@/�@/�P@/l�@/K�@/+@/
=@.�@.�R@.��@.V@.{@-`B@,�@,�/@,�j@,�D@,z�@,z�@,j@,9X@,1@+C�@+@*�@*�H@*��@*~�@*=q@)�^@)&�@)�@)%@(��@(�@'�P@'|�@'\)@&��@&ȴ@&�R@&�+@&E�@&{@&@&@%�h@%O�@%/@$�@$j@$�@#��@#t�@#C�@#"�@"�@"�@"�H@"��@"�\@"n�@"J@!��@!�^@!��@!7L@ ��@ Q�@��@�@
=@��@ȴ@��@�+@@@�-@��@�h@�h@�@?}@/@�@��@�@�@(�@��@�m@��@"�@o@@�@��@~�@~�@J@�@�#@7L@�9@�9@��@�u@1'@ �@ �@b@b@b@b@b@ �@b@�;@|�@K�@��@E�@@�@@p�@V@��@��@�D@I�@9X@�m@ƨ@�F@�@o@�\@�@��@��@�#@��@�^@��@X@��@Q�@1'@ �@b@  @  @�@�;@�w@�P@�P@�P@|�@K�@��@��@�+@E�@$�@{@{@{@@�T@�@V@�/@��@��@�@j@j@I�@(�@1@�m@ƨ@t�@o@@@@
�@
�H@
��@
��@	��@	X@	&�@��@Ĝ@bN@Q�@1'@b@  @�w@�P@�@�y111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��yA��yA��yA��mA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��`A���A�ƨAլA�v�A��A�S�A�%A��A��;AӶFAӲ-AӰ!AӰ!AӰ!Aө�Aӡ�Aӕ�A�z�A�VA�G�A�=qA�-AҶFA�33Aк^A�O�A�z�A���A��Aİ!A�VA�ƨA�7LA�7LA���A��-A���A���A�{A��A�bA��A�`BA��A�t�A��PA�I�A��A�|�A��A�S�A�C�A��A���A���A�{A�v�A�Q�A���A���A�n�A�33A�bNA��-A�A��`A��#A�9XA�Q�A��FA�ĜA�A���A�33A��^A�oA�&�A�=qA�|�A���A���A�VA���A�dZA��HA�hsA���A��A|�A{VAy�Aw`BAu�PAsVAq��Ao�Am�TAmK�Al�\AiƨAhjAf��Ae�
Ac�A`��A]�#A[�A[/AY�#AY"�AWC�AT�\AS�ARȴAQ;dAP�DAO;dANn�AM�AL�`AKAH��AGVAFffAE�#AC��ABAA33A?XA>A;�A7t�A7hsA7A5��A4�A3�hA2�A1��A0jA.��A-�-A-7LA,�+A+G�A*�A(�HA'ƨA'�A'VA&Q�A%;dA$�A$=qA"1'A �A�A��AVA��A��AXA�DA�A�RA�-A��A1'A7LAv�A�Al�A;dA�9A��A�A1'AS�AC�AC�A�\AdZA
5?A
{A	��A	S�A�-A��AoAȴA=qA��A�FA ��@��
@�A�@�p�@�K�@�S�@�!@�7@�{@�p�@�G�@�1'@�33@柾@��@��@�J@��@�@��#@��/@�A�@�l�@���@�j@���@׶F@�|�@և+@��@Ԭ@�J@мj@�7L@с@���@�1@ϥ�@ΰ!@���@́@̼j@�V@��`@�b@��@�$�@ź^@�9X@�=q@�j@�|�@��+@���@�j@��P@�n�@�`B@�Q�@�K�@���@�J@���@�`B@�9X@�\)@���@���@��\@�E�@���@�@���@�/@���@�z�@�(�@��F@�S�@��@�ff@��h@�X@�O�@�/@���@��@��@�Ĝ@���@�r�@�1'@��@��@�|�@�;d@��y@��\@�J@�G�@��/@���@�9X@�|�@��@�v�@�5?@��@��7@�?}@���@��u@�z�@�Q�@�b@�33@�
=@��y@���@�n�@�E�@�=q@�5?@��@�@�?}@�Q�@��;@�ƨ@��@���@�"�@��H@�ȴ@��R@�~�@���@���@��9@�1@��m@��
@���@�ƨ@���@��P@�;d@��@�@��H@���@�^5@���@���@�-@��^@�O�@��9@��u@�j@� �@�1'@� �@�A�@� �@�(�@��j@��7@��T@�@��@�ff@���@��#@�O�@���@��9@�r�@�(�@���@�+@��@��@���@���@��7@�o@��@�r�@��`@�X@�`B@�V@��/@�Ĝ@�Z@��;@�o@��
@���@�dZ@��y@���@�$�@��@���@�@��7@�p�@�X@�?}@���@�r�@�I�@�9X@�1@���@��@���@��@�+@���@�ff@�^5@�v�@�~�@�~�@��@��@��@��T@�@�?}@��/@���@�bN@�b@���@���@�l�@��@��+@�V@�5?@��#@��7@��@�Ĝ@��@���@��D@��@�j@�Z@�1'@��m@��
@�ƨ@�t�@��@��R@��@�p�@�O�@��@�z�@�1@�l�@���@���@�v�@�=q@�{@��T@�@��@�/@��@�Q�@�1@��@�|�@�S�@�+@�@���@�ȴ@���@�~�@�5?@�@�x�@�?}@�&�@��@��@�  @�P@�P@�P@l�@|�@l�@l�@l�@\)@;d@~�R@~5?@}/@|��@|(�@|1@{�F@{"�@z~�@z-@y��@y��@y�^@y��@yX@x��@x��@x�@xA�@x1'@w�@w�@w
=@v{@v{@v{@v{@v@up�@t�@t��@t1@s�m@s�
@sƨ@s�F@s��@r�@r�\@rM�@q��@q��@qX@q%@pĜ@p�9@p�9@p��@p��@p1'@n�y@n�R@nV@n5?@n@m�h@l��@l�@lz�@l(�@kS�@j�@j�\@j^5@j�@i��@i7L@h��@h1'@g�@g�P@g+@f�y@e�@e�@e�@d��@d1@cƨ@c��@c��@c��@c�@cdZ@co@b�H@b��@b�\@a��@ax�@`��@`r�@`A�@_��@_l�@_+@^�y@^��@]�-@]p�@]O�@]V@\�/@\��@\I�@[ƨ@[S�@["�@Z��@Z^5@Z�@Y�#@Yx�@Y&�@X��@X��@X�u@Xb@W�@W\)@V��@V�R@V��@Vff@U�@U�h@U�@T��@T(�@St�@SC�@S33@So@R��@RM�@RJ@Q��@Q��@RJ@RJ@Q�@Q��@Q�@P��@O�;@O��@Ol�@N�y@N@M�@M/@L�/@L�@L��@L�D@Lj@L9X@L9X@L9X@L�@K�
@KS�@J=q@Ix�@I&�@H��@H�`@H��@H�9@H�9@H�9@H�9@H�9@H�u@Hr�@H1'@Hb@G��@G\)@Fȴ@E�@E�T@E�-@E@E��@E`B@E/@E/@E�@E�@E�@E�@E�@E�@E�@D��@D�/@D�@Dj@D(�@C��@B�\@B^5@BM�@A��@A7L@@��@?|�@?+@?
=@>�@>��@>v�@>5?@=��@=�h@=�@=p�@=V@<�@<�j@<9X@;�F@;dZ@;o@:��@:�@9hs@9�@8�9@8��@8��@8�u@8�@8r�@8A�@81'@8b@7�@7�;@7��@7|�@7|�@7K�@6��@6V@6@5�-@5`B@5?}@4��@4�@4�/@4�/@4�/@4�@4z�@4j@4j@4I�@4(�@41@3��@3�
@3�F@3�F@3��@3��@3"�@2�@2�H@2�!@2J@1�^@1�7@0��@/�@/�P@/l�@/K�@/+@/
=@.�@.�R@.��@.V@.{@-`B@,�@,�/@,�j@,�D@,z�@,z�@,j@,9X@,1@+C�@+@*�@*�H@*��@*~�@*=q@)�^@)&�@)�@)%@(��@(�@'�P@'|�@'\)@&��@&ȴ@&�R@&�+@&E�@&{@&@&@%�h@%O�@%/@$�@$j@$�@#��@#t�@#C�@#"�@"�@"�@"�H@"��@"�\@"n�@"J@!��@!�^@!��@!7L@ ��@ Q�@��@�@
=@��@ȴ@��@�+@@@�-@��@�h@�h@�@?}@/@�@��@�@�@(�@��@�m@��@"�@o@@�@��@~�@~�@J@�@�#@7L@�9@�9@��@�u@1'@ �@ �@b@b@b@b@b@ �@b@�;@|�@K�@��@E�@@�@@p�@V@��@��@�D@I�@9X@�m@ƨ@�F@�@o@�\@�@��@��@�#@��@�^@��@X@��@Q�@1'@ �@b@  @  @�@�;@�w@�P@�P@�P@|�@K�@��@��@�+@E�@$�@{@{@{@@�T@�@V@�/@��@��@�@j@j@I�@(�@1@�m@ƨ@t�@o@@@@
�@
�H@
��@
��@	��@	X@	&�@��@Ĝ@bN@Q�@1'@b@  @�w@�P@�@�y111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bz�B{�B{�B{�B{�B{�B|�B|�B|�B|�B|�B{�B|�B~�B~�B{�B}�B~�B�B�PB�oB��B�'B��B�B2-B7LB=qBL�BP�BP�BQ�BR�BS�BT�BXB^5B`BB`BB`BB`BBe`Bk�Bs�Bw�Bt�Bq�Bx�B�Br�Bx�B�7B��B�B�^B��BƨBǮB��B��BȴBƨBÖB�qB�B��B��B�bB�Bt�B_;BI�B33B!�B�B\B�B�B�
B��B��B��BǮB��B��B�bB�1By�Bw�Bk�BS�BJ�B:^B33B,B!�B�BDB
��B
�B
�yB
�HB
��B
��B
�LB
�B
��B
�7B
n�B
\)B
L�B
>wB
+B
�B
PB	��B	��B	�B	�5B	��B	ƨB	�qB	�9B	��B	��B	�%B	� B	w�B	o�B	ffB	R�B	N�B	I�B	<jB	9XB	1'B	+B	%�B	�B	�B	VB	+B	B	B��B��B�B�B�`B�HB��B�B�B�
BɺBŢBƨBÖB�jB�-B��B��B��B��B��B��B��B��B��B��B�uB�hB�bB�PB�7B�%B�B�B�B|�B|�B{�By�Bw�Bw�Bu�Bs�Bs�Bq�Bp�Bo�Bn�Bn�Bl�Bk�BjBhsBhsBffBgmBiyBe`BdZBcTBcTBcTBbNB_;B^5B^5B_;B]/B\)B\)B[#BZBVBYBQ�BN�BJ�BJ�BK�BQ�BT�BZBffBk�BiyBjBhsBk�Bo�Br�Bu�B|�B�B�B�B�%B�JB�PB�JB�1B�1B�oB��B��B��B��B��B��B��B��B�B�B�B�B�!B�!B�3B�LB�wB��BŢBǮB��B��B�B�B�/B�NB�ZB�mB�yB�B�B��B��B��B	  B	B	B	B	B	%B	1B	
=B	PB	oB	�B	�B	�B	�B	!�B	"�B	"�B	$�B	&�B	(�B	+B	+B	,B	.B	/B	0!B	1'B	2-B	49B	6FB	;dB	@�B	B�B	D�B	I�B	L�B	O�B	Q�B	R�B	S�B	XB	[#B	_;B	cTB	dZB	ffB	hsB	m�B	n�B	p�B	r�B	t�B	u�B	u�B	v�B	v�B	x�B	y�B	~�B	�B	�B	�B	�B	�%B	�%B	�+B	�+B	�1B	�7B	�=B	�PB	�VB	�VB	�\B	�\B	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�LB	�dB	�jB	�wB	��B	ĜB	ŢB	ƨB	ƨB	ŢB	ŢB	ĜB	ÖB	B	ÖB	ƨB	ǮB	ȴB	��B	��B	�)B	�5B	�BB	�ZB	�yB	�B	�B	�B	�B	�B	�yB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
	7B

=B

=B
JB
JB
JB
JB
JB
JB
JB
PB
PB
PB
PB
VB
VB
VB
\B
bB
bB
hB
hB
oB
oB
uB
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
,B
,B
,B
,B
-B
-B
-B
.B
.B
.B
.B
/B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
2-B
2-B
33B
33B
33B
33B
33B
33B
33B
33B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
F�B
E�B
F�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
XB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
l�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
u�B
u�B
u�B
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
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
~�B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bz�B{�B{�B{�B{�B{�B|�B|�B|�B|�B|�B{�B|�B~�B~�B|B}�B.B�MB��B��B�dB�GB�mB�B2-B7�B=�BL�BP�BP�BQ�BSBTBU2BX_B^jB`\B`vB`�Ba|BgBn�Bv�Bz^ByXBy$B�GB�xBv+B|�B��B�kB�[B��B��BȚBˬB�TB��B��B�lB�B�oB�'B�_B�\B�B��By�Bd�BNB5�B#�B�BB�|BخBרB��B��B̳B�#BĜB�DB�:B�=B{�By�Bm�BU�BL~B;�B5B./B#�BsBPB
��B
�B
�6B
�B
�<B
��B
�>B
�'B
�eB
��B
qB
^jB
OBB
A;B
,�B
�B
vB	�B	�fB	�B	�B	��B	ȴB	��B	��B	�
B	��B	�zB	��B	y>B	r-B	iB	T,B	P}B	KxB	=�B	:�B	2GB	,WB	'8B	!HB	IB	.B	1B	gB	�B	 �B�FB�B��B��B�B�bB��BۦB��BʦB��B�KB�SB�]B��B��B�*B��B�tB�HB��B�B�eB��B��B�FB��B��B�\B��B�B�SB��B�[B}�B~BB}�Bz�By>By	Bv�BuBt�BraBq[Bp!Bo�Bo�Bm�Bl�Bk�Bh�BiBh>Bj�Bj�Be�Bd�Bd�Be�Be�Bc B_�B_pB`BB`vB^�B^B^�B]/B\BX�B[�BS&BP�BKDBK)BL�BR�BU�B[WBg�Bl"BjKBlBi_Bl"Bp;Bs�Bv�B}�B�gB�mB��B��B�B�pB��B��B��B�oB�B�+B�B�dB�bB�NB��B�fB��B��B��B��B��B�AB��B��B�B�;B�?BȴBΊBѷB��B��B��B�B��B�B��B�CB�3B�0B�B�B	 OB	UB	GB	GB	�B	tB	�B	
rB	�B	�B	�B	B	B	�B	!�B	"�B	#B	$�B	'B	)*B	+B	+6B	,=B	./B	/OB	0UB	1vB	2|B	4�B	6�B	;�B	@�B	B�B	EB	J=B	M6B	P.B	R B	S@B	T,B	XEB	[qB	_pB	cnB	d�B	f�B	h�B	m�B	n�B	p�B	r�B	t�B	u�B	u�B	v�B	w2B	y$B	zxB	HB	�'B	�'B	�GB	�aB	�YB	�?B	�EB	�zB	��B	��B	��B	��B	�pB	�pB	�vB	�vB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�&B	�@B	��B	�B	�B	��B	��B	�B	�B	��B	��B	��B	�B	�JB	�jB	�]B	��B	�B	�B	��B	��B	żB	��B	�B	��B	��B	�3B	ƨB	��B	ȀB	��B	�aB	��B	��B	��B	�ZB	�B	�B	��B	��B	��B	��B	�*B	�B	�B	�B	��B	�+B	��B	��B	��B	�*B	��B	��B	��B	�0B	�B	�B	��B	�B	�B	��B	�B	�B	�6B	�6B	�B	��B	��B	��B
 B
 OB
 B
�B
'B
AB
uB
[B
GB
GB
gB
SB
9B
SB
�B
_B
EB
_B
�B
	�B

�B

rB
dB
dB
dB
dB
dB
dB
~B
�B
PB
jB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
!B
!B
!�B
!�B
!�B
!�B
!�B
#B
#�B
$B
#�B
#�B
$�B
$�B
#�B
$B
$�B
$�B
%�B
%�B
&B
%�B
'B
&�B
&�B
'B
'B
'B
'RB
(
B
(
B
'�B
)B
)*B
)*B
)B
)B
)*B
)DB
*B
*B
+B
+B
+6B
+B
,"B
,=B
,"B
,=B
-CB
-CB
-]B
./B
.IB
.IB
./B
/5B
/5B
/B
/B
/5B
/5B
/5B
0;B
0;B
0;B
0UB
0UB
0UB
1AB
2GB
2aB
3MB
33B
3MB
3hB
3�B
3MB
3MB
3MB
4TB
4TB
4nB
4nB
5ZB
5?B
5ZB
6`B
6`B
6`B
6zB
7fB
7fB
7fB
7fB
7�B
8lB
8lB
8lB
8lB
8lB
8lB
9�B
9�B
9�B
:xB
:xB
:�B
;B
;B
;B
;B
;�B
<�B
<PB
<jB
<PB
<jB
<�B
<�B
<�B
<�B
=�B
=�B
=qB
>�B
>�B
?�B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
B�B
B�B
C�B
C�B
C{B
C�B
C�B
C�B
C{B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
F�B
E�B
F�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
J	B
J	B
J#B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
NB
NB
N�B
N�B
OB
OB
PB
Q B
Q B
P�B
P�B
P�B
P�B
Q B
RB
Q�B
RB
RB
RB
RB
RB
Q�B
Q�B
RB
S&B
SB
S�B
TB
S�B
TB
T�B
T�B
T�B
T�B
UB
UB
T�B
T�B
UB
UB
T�B
T�B
UB
VB
U�B
VB
VB
V9B
VB
VB
VB
V9B
W$B
W?B
WYB
X+B
Y1B
YB
Y1B
Y1B
Y1B
Z7B
Z7B
Z7B
Z7B
Z7B
Z7B
[=B
[#B
[=B
\CB
\)B
\)B
\CB
\CB
\CB
\]B
]IB
]/B
]/B
]IB
]IB
]IB
]dB
^�B
^5B
^5B
^OB
^jB
^jB
_VB
_VB
_VB
`\B
`\B
`\B
`\B
`BB
aHB
abB
abB
aHB
abB
bhB
b�B
b�B
c�B
cnB
cnB
cnB
dtB
dZB
dZB
dtB
dtB
dtB
dtB
ezB
ezB
ezB
e�B
e�B
f�B
f�B
g�B
gmB
g�B
gmB
gmB
gmB
g�B
hsB
hsB
hsB
hXB
hsB
h�B
hsB
hsB
hsB
h�B
h�B
h�B
i�B
iyB
iyB
i�B
i�B
jB
jB
jB
j�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
l�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
lqB
l�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
u�B
u�B
u�B
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
w�B
xB
xB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
zB
{B
{�B
{�B
|�B
}B
|�B
|�B
}B
~B
~B
}�B
~B
B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<'�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.02(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201611270037232016112700372320161127003723201806221305252018062213052520180622130525201804050705282018040507052820180405070528  JA  ARFMdecpA19c                                                                20161123093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161123003528  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161123003529  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161123003530  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161123003531  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161123003531  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161123003531  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161123003531  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161123003531  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161123003532                      G�O�G�O�G�O�                JA  ARUP                                                                        20161123013045                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161123153403  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20161126153723  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161126153723  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220528  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040525  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211515                      G�O�G�O�G�O�                