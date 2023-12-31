CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-11-20T00:35:37Z creation;2016-11-20T00:35:39Z conversion to V3.1;2019-12-19T08:21:21Z update;     
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
resolution        =���   axis      Z        @  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     @  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     @  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  n�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     @  r�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     @  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     @  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     @  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     @  �<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     @  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ټ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �,   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �D   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �LArgo profile    3.1 1.2 19500101000000  20161120003537  20200116211515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               <A   JA  I2_0577_060                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��u�i� 1   @��v�b��@3!-w1���d�U2a|1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A���B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D'��D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�C3Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D��3D�3D�P 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@���A Q�A Q�A>�RA`Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A���B z�B{B{B{B {B({B0{B8{B@{BH{BP{BX{B`{Bh{Bp{Bx{B�
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
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=C CCCCC
CCCCCCCCCCC C"C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD��DHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD'��D(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�=qD���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�=qD���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�C�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�@�D怤D���D� �D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D��D���D��D�P�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AՕ�A՛�A՝�A՝�A՟�Aա�Aա�Aա�Aգ�Aե�Aե�Aե�Aէ�Aէ�Aէ�Aթ�Aթ�Aթ�AլAթ�Aթ�AլAծAլAա�A�hsA�ȴAԇ+A�z�A�x�A�v�A�p�A�O�A�/A��A�5?AґhA� �A�ffA�z�A���A��Aɝ�A�ZA¾wA�
=A�ffA��jA���A��PA�(�A��
A��
A���A���A��TA��wA��!A�C�A�x�A��-A���A�bA��PA���A�A�A�E�A���A���A�jA��TA�?}A��A���A��`A��RA�jA���A���A���A���A��-A���A�p�A��9A�$�A�1A�dZA�l�A��TA��hA�x�A���A��yA�A�A�ZA��A�K�A��A�A���A�ĜA���A���A�ƨA���A~E�Az��AvbNAu�hAt��AoS�AnVAmVAjI�AhVAhA�Ah-AgO�AeVAc��Ab�/A`�/A]��A[��AX�AVȴAU�AU7LATv�ASoAQ�PAPz�AN1'AL�`AKl�AI�
AGx�AD��AB�\AA?}A@A�A?l�A>Q�A=+A;�A:jA9��A9O�A8��A8A6�DA6-A5��A2��A0ffA.��A-K�A,r�A*�`A)�A)x�A( �A'"�A&��A&�+A&JA%�A$r�A#/A!��A!`BA A�A^5A5?AƨA{A�HA�hAS�A��AA�yAn�A-A��AC�A�A�DA9XAC�A�;A/A	|�AbNAJA��A��AS�A"�A��A��A��A{A�A"�A ��@��7@��P@��y@��@��/@��@�;d@��@�\@�5?@�?}@�w@�l�@�@�1@�u@�|�@��#@��@�1'@旍@�9@�9@�ƨ@�M�@�7@�G�@�%@�%@�1'@�
=@޸R@�^5@�A�@���@�ƨ@١�@�Z@�1@և+@�/@��/@ԣ�@� �@�S�@҇+@�X@��`@� �@�
=@�?}@̋D@��;@��H@ʗ�@�ff@�$�@ɺ^@��;@�V@�@��@ģ�@�C�@��@��9@�b@�ƨ@��H@��h@��`@�A�@��P@��H@�M�@�@�%@�o@��y@��@���@�5?@���@�7L@�r�@�;d@�v�@�M�@�5?@��-@���@�Ĝ@���@�Z@�b@�t�@�dZ@�+@��@���@�G�@�&�@�V@��j@��u@�1'@���@�
=@��H@���@���@��u@�j@�I�@��@�  @��
@���@�\)@�t�@��m@�1@��@�Q�@�ƨ@���@���@�hs@�7L@��@�1'@��
@�"�@�@�o@��y@��+@���@�O�@�/@���@��P@�C�@�~�@���@���@�|�@�|�@��P@�"�@���@�v�@�V@��@��@��9@��m@��@��@�l�@�dZ@�K�@�C�@�(�@�r�@���@���@�`B@�-@���@��@�ȴ@��\@��T@��-@�p�@�j@�~�@��h@���@��+@�ȴ@��H@�@��@�"�@�o@�+@�33@�33@�o@��\@�$�@��@��#@���@���@�hs@��@���@���@��@���@��u@� �@��P@�o@���@���@�M�@�{@���@���@�@��T@���@�X@���@��@�(�@�ƨ@�|�@�33@�"�@��@�o@�@���@��H@���@�n�@���@��7@�&�@���@���@�Q�@��m@��@�33@��@�@��y@��@���@��@���@�@�@��^@���@��7@�X@�/@�V@�Ĝ@���@�l�@��\@���@��@�`B@�O�@�?}@��@��u@�j@�I�@���@�t�@�\)@�C�@�+@��y@�ȴ@��!@��+@�~�@�v�@�-@���@�X@��@��@��u@�r�@�A�@�A�@�1'@��@���@��P@�|�@�K�@�o@��@�v�@�M�@��@���@�p�@�G�@���@���@� �@�P@K�@+@�@~�y@~�+@~E�@}�T@}��@|�D@{�@z��@z^5@y�^@y7L@x�`@x��@x�9@x1'@w�@w;d@v��@v5?@u�@u@u�@u/@t��@tj@t�@s�m@sdZ@s33@r�H@r~�@rn�@rn�@r^5@p�9@pb@o�;@o�@ol�@nȴ@n5?@m�@m?}@l��@k�F@k"�@j�\@i�@h�u@h  @gK�@f�R@fff@e�h@d��@dZ@c�
@b�H@b�\@b^5@a�^@aG�@a%@`�9@`A�@_�@_��@_\)@_+@^�@^�R@^ff@^V@^$�@]��@]O�@\��@\��@\�D@\Z@\Z@\�@[�m@[ƨ@[��@[dZ@[33@Z�@Z�!@Zn�@Y�^@X��@XA�@W�@W�@W��@W�P@W;d@V�@VV@U��@UO�@U?}@T��@T1@S�F@SC�@S"�@R�!@Q��@Qx�@QX@P�`@P�@P  @Ol�@Nȴ@N��@Nff@NV@N5?@N{@M@M�@M?}@L�j@L�@Kƨ@K�@K@J��@J��@I��@H�9@HA�@HA�@H1'@H1'@H �@Hb@Hb@G�;@G;d@F�y@F�+@FV@FE�@F@E�@EO�@D1@C�m@C��@Ct�@B��@Bn�@BM�@B�@A�@A�^@A&�@A�@@�@?�@?|�@>�@>V@=��@=��@=�@=?}@<�/@<�j@<��@<j@<9X@;�
@;@:M�@9�7@9G�@9�@8��@8Ĝ@8�9@8bN@8b@7�w@7|�@7K�@7+@6�y@6ȴ@6�R@6��@6�+@6ff@6V@6E�@5�@5/@3�m@3�F@3�@333@2^5@1��@1�^@1��@1X@1�@0Ĝ@01'@0  @/|�@/;d@/�@/
=@/
=@.��@.��@.�y@.�@.��@.v�@.V@.V@.V@.5?@.{@-�T@-�h@-V@,��@,z�@,(�@,�@+��@+�
@+dZ@+@*�@*�@*��@*�@)�^@)�7@)hs@)hs@)X@)G�@)G�@)&�@(��@(�`@(Ĝ@(��@(�u@(�@(Q�@(1'@(  @'��@'+@&�@&v�@%@%p�@%/@$��@$��@$�@$�D@$�D@$z�@$Z@$9X@$1@#��@#��@#ƨ@#ƨ@#ƨ@#�F@#��@#�@#�@#t�@#S�@#"�@"�!@"-@!��@!�@!�#@!��@!x�@ ��@ Ĝ@ r�@ A�@ b@�@��@��@K�@;d@+@��@�@�R@ff@�@��@p�@/@��@I�@9X@9X@9X@�
@��@��@�@"�@�H@�!@�\@n�@-@��@�7@�@��@�9@1'@�;@��@�w@�@�P@l�@\)@\)@K�@+@�@��@�@�R@�+@ff@$�@��@`B@`B@`B@?}@��@��@��@z�@I�@�@��@�
@��@t�@dZ@S�@S�@"�@�@��@��@�@�@�#@��@X@��@��@�@bN@A�@1'@  @  @  @�;@�@��@��@v�@v�@ff@ff@V@$�@@�@�h@p�@O�@�/@Z@9X@1@��@��@�m@ƨ@��@t�@S�@33@
�@
��@
��@
�\@
~�@
=q@
J@
J@	�@	�#@	�^@	��@	��@	�7@	x�@	hs@	X@	G�@	%@��@��@��@�@Q�@ �@�@�;@�;@�w@�@��@��@��@��@��@l�@+@
=@��@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AՕ�A՛�A՝�A՝�A՟�Aա�Aա�Aա�Aգ�Aե�Aե�Aե�Aէ�Aէ�Aէ�Aթ�Aթ�Aթ�AլAթ�Aթ�AլAծAլAա�A�hsA�ȴAԇ+A�z�A�x�A�v�A�p�A�O�A�/A��A�5?AґhA� �A�ffA�z�A���A��Aɝ�A�ZA¾wA�
=A�ffA��jA���A��PA�(�A��
A��
A���A���A��TA��wA��!A�C�A�x�A��-A���A�bA��PA���A�A�A�E�A���A���A�jA��TA�?}A��A���A��`A��RA�jA���A���A���A���A��-A���A�p�A��9A�$�A�1A�dZA�l�A��TA��hA�x�A���A��yA�A�A�ZA��A�K�A��A�A���A�ĜA���A���A�ƨA���A~E�Az��AvbNAu�hAt��AoS�AnVAmVAjI�AhVAhA�Ah-AgO�AeVAc��Ab�/A`�/A]��A[��AX�AVȴAU�AU7LATv�ASoAQ�PAPz�AN1'AL�`AKl�AI�
AGx�AD��AB�\AA?}A@A�A?l�A>Q�A=+A;�A:jA9��A9O�A8��A8A6�DA6-A5��A2��A0ffA.��A-K�A,r�A*�`A)�A)x�A( �A'"�A&��A&�+A&JA%�A$r�A#/A!��A!`BA A�A^5A5?AƨA{A�HA�hAS�A��AA�yAn�A-A��AC�A�A�DA9XAC�A�;A/A	|�AbNAJA��A��AS�A"�A��A��A��A{A�A"�A ��@��7@��P@��y@��@��/@��@�;d@��@�\@�5?@�?}@�w@�l�@�@�1@�u@�|�@��#@��@�1'@旍@�9@�9@�ƨ@�M�@�7@�G�@�%@�%@�1'@�
=@޸R@�^5@�A�@���@�ƨ@١�@�Z@�1@և+@�/@��/@ԣ�@� �@�S�@҇+@�X@��`@� �@�
=@�?}@̋D@��;@��H@ʗ�@�ff@�$�@ɺ^@��;@�V@�@��@ģ�@�C�@��@��9@�b@�ƨ@��H@��h@��`@�A�@��P@��H@�M�@�@�%@�o@��y@��@���@�5?@���@�7L@�r�@�;d@�v�@�M�@�5?@��-@���@�Ĝ@���@�Z@�b@�t�@�dZ@�+@��@���@�G�@�&�@�V@��j@��u@�1'@���@�
=@��H@���@���@��u@�j@�I�@��@�  @��
@���@�\)@�t�@��m@�1@��@�Q�@�ƨ@���@���@�hs@�7L@��@�1'@��
@�"�@�@�o@��y@��+@���@�O�@�/@���@��P@�C�@�~�@���@���@�|�@�|�@��P@�"�@���@�v�@�V@��@��@��9@��m@��@��@�l�@�dZ@�K�@�C�@�(�@�r�@���@���@�`B@�-@���@��@�ȴ@��\@��T@��-@�p�@�j@�~�@��h@���@��+@�ȴ@��H@�@��@�"�@�o@�+@�33@�33@�o@��\@�$�@��@��#@���@���@�hs@��@���@���@��@���@��u@� �@��P@�o@���@���@�M�@�{@���@���@�@��T@���@�X@���@��@�(�@�ƨ@�|�@�33@�"�@��@�o@�@���@��H@���@�n�@���@��7@�&�@���@���@�Q�@��m@��@�33@��@�@��y@��@���@��@���@�@�@��^@���@��7@�X@�/@�V@�Ĝ@���@�l�@��\@���@��@�`B@�O�@�?}@��@��u@�j@�I�@���@�t�@�\)@�C�@�+@��y@�ȴ@��!@��+@�~�@�v�@�-@���@�X@��@��@��u@�r�@�A�@�A�@�1'@��@���@��P@�|�@�K�@�o@��@�v�@�M�@��@���@�p�@�G�@���@���@� �@�P@K�@+@�@~�y@~�+@~E�@}�T@}��@|�D@{�@z��@z^5@y�^@y7L@x�`@x��@x�9@x1'@w�@w;d@v��@v5?@u�@u@u�@u/@t��@tj@t�@s�m@sdZ@s33@r�H@r~�@rn�@rn�@r^5@p�9@pb@o�;@o�@ol�@nȴ@n5?@m�@m?}@l��@k�F@k"�@j�\@i�@h�u@h  @gK�@f�R@fff@e�h@d��@dZ@c�
@b�H@b�\@b^5@a�^@aG�@a%@`�9@`A�@_�@_��@_\)@_+@^�@^�R@^ff@^V@^$�@]��@]O�@\��@\��@\�D@\Z@\Z@\�@[�m@[ƨ@[��@[dZ@[33@Z�@Z�!@Zn�@Y�^@X��@XA�@W�@W�@W��@W�P@W;d@V�@VV@U��@UO�@U?}@T��@T1@S�F@SC�@S"�@R�!@Q��@Qx�@QX@P�`@P�@P  @Ol�@Nȴ@N��@Nff@NV@N5?@N{@M@M�@M?}@L�j@L�@Kƨ@K�@K@J��@J��@I��@H�9@HA�@HA�@H1'@H1'@H �@Hb@Hb@G�;@G;d@F�y@F�+@FV@FE�@F@E�@EO�@D1@C�m@C��@Ct�@B��@Bn�@BM�@B�@A�@A�^@A&�@A�@@�@?�@?|�@>�@>V@=��@=��@=�@=?}@<�/@<�j@<��@<j@<9X@;�
@;@:M�@9�7@9G�@9�@8��@8Ĝ@8�9@8bN@8b@7�w@7|�@7K�@7+@6�y@6ȴ@6�R@6��@6�+@6ff@6V@6E�@5�@5/@3�m@3�F@3�@333@2^5@1��@1�^@1��@1X@1�@0Ĝ@01'@0  @/|�@/;d@/�@/
=@/
=@.��@.��@.�y@.�@.��@.v�@.V@.V@.V@.5?@.{@-�T@-�h@-V@,��@,z�@,(�@,�@+��@+�
@+dZ@+@*�@*�@*��@*�@)�^@)�7@)hs@)hs@)X@)G�@)G�@)&�@(��@(�`@(Ĝ@(��@(�u@(�@(Q�@(1'@(  @'��@'+@&�@&v�@%@%p�@%/@$��@$��@$�@$�D@$�D@$z�@$Z@$9X@$1@#��@#��@#ƨ@#ƨ@#ƨ@#�F@#��@#�@#�@#t�@#S�@#"�@"�!@"-@!��@!�@!�#@!��@!x�@ ��@ Ĝ@ r�@ A�@ b@�@��@��@K�@;d@+@��@�@�R@ff@�@��@p�@/@��@I�@9X@9X@9X@�
@��@��@�@"�@�H@�!@�\@n�@-@��@�7@�@��@�9@1'@�;@��@�w@�@�P@l�@\)@\)@K�@+@�@��@�@�R@�+@ff@$�@��@`B@`B@`B@?}@��@��@��@z�@I�@�@��@�
@��@t�@dZ@S�@S�@"�@�@��@��@�@�@�#@��@X@��@��@�@bN@A�@1'@  @  @  @�;@�@��@��@v�@v�@ff@ff@V@$�@@�@�h@p�@O�@�/@Z@9X@1@��@��@�m@ƨ@��@t�@S�@33@
�@
��@
��@
�\@
~�@
=q@
J@
J@	�@	�#@	�^@	��@	��@	�7@	x�@	hs@	X@	G�@	%@��@��@��@�@Q�@ �@�@�;@�;@�w@�@��@��@��@��@��@l�@+@
=@��@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B�B�!B�!B�!B�!B�'B�'B�'B�'B�-B�3B�9B�wB�B^5Bv�Bx�Bx�Bx�By�Bz�B{�B{�B� B� B~�B}�B|�B|�B}�B�B}�Bo�Bs�Bo�Bp�Be`BdZBt�B�uB��B�-BȴB��B��BŢBÖB�B��B��B�hB�VB�hB�uB�bB�JB�BW
B=qB+B#�B �B:^B49B)�B�B�BbB1B  B��B��B��B�HB�BƨB�^B�B��B��B�=B}�BhsBQ�B@�B-B�B1B
��B
�5B
��B
�wB
�B
��B
�DB
u�B
^5B
<jB
8RB
5?B
hB
B	��B	�yB	�
B	�B	�B	�B	ĜB	�RB	�-B	��B	�JB	~�B	m�B	_;B	YB	S�B	Q�B	H�B	@�B	9XB	1'B	'�B	!�B	�B	bB	B��B�B�B�mB�TB�BB�B��B��B��B��B��BƨBÖB��B�^B�-B�B��B��B��B��B��B��B��B��B��B��B��B��B�hB�DB�7B�%B�B~�B}�B}�By�Bx�Bv�Bu�Bt�Bt�Br�Bp�Bo�Bp�Bl�Bl�Bk�BjBjBffBgmBcTBbNBaHBaHBaHBaHBaHBaHB`BB`BB^5B\)BZB]/B]/B\)BZBYBVBQ�BM�BM�BH�BH�BH�BH�BH�BK�BXB`BBaHB_;B]/B[#BXBS�BW
BYB_;B_;B^5B^5B_;BcTBcTBe`Bp�Bz�Bz�By�B�B�B�B�7B�DB�=B�DB�\B�bB�\B�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�3B�9B�RB�dB�qB�}B��BĜB��B��B��B��B��B�B�B�)B�ZB�TB�ZB�ZB�`B�yB�B�B��B��B��B��B	B	+B	1B		7B	DB	VB	oB	�B	�B	�B	�B	�B	 �B	!�B	$�B	'�B	.B	0!B	33B	33B	9XB	=qB	=qB	>wB	>wB	@�B	E�B	F�B	G�B	J�B	O�B	T�B	W
B	XB	YB	]/B	`BB	^5B	\)B	_;B	_;B	^5B	cTB	gmB	hsB	iyB	l�B	o�B	u�B	v�B	x�B	�B	�JB	�DB	�7B	�B	� B	�B	�B	�%B	�%B	�%B	�%B	�B	�B	�+B	�1B	�JB	�\B	�oB	�uB	�{B	��B	��B	��B	��B	��B	�B	�'B	�^B	ÖB	ŢB	ɺB	��B	��B	��B	��B	��B	ǮB	ƨB	��B	��B	�
B	�
B	�B	�B	�#B	�#B	�/B	�;B	�;B	�HB	�fB	�mB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
+B
1B
	7B

=B

=B

=B

=B

=B

=B

=B
DB
DB
DB
PB
PB
\B
bB
bB
bB
bB
bB
hB
oB
oB
oB
oB
uB
uB
uB
uB
uB
uB
uB
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
"�B
#�B
#�B
#�B
$�B
$�B
#�B
#�B
%�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
,B
,B
,B
-B
-B
-B
.B
.B
/B
/B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
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
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
@�B
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
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
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
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
T�B
T�B
T�B
T�B
VB
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
XB
XB
XB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
ZB
YB
YB
ZB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
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
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
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
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
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
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
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
u�B
t�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
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
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B�B�!B�!B�!B�!B�'B�'B�'B�'B�-B�MB��B�HB�CB]�Bv�Bx�Bx�Bx�Bz*B{JB|�B}�B�UB�;B��B��B��B�[B�B��B�BrBv�BrGBu�Bh�Bh
By>B��B�tB�`B��BѝB�@BɠB�fB��B��B�CB��B�B��B�aB�B��B�XBZ�B@iB,qB$�B"4B<6B6�B,qB�B�B�B	�B iB�}B�HB�B� B�_B�fB�jB�cB�-B�OB��B��BlBTaBCGB/iB�BB
��B
��B
ЗB
�B
�cB
��B
�(B
zB
bhB
=�B
:�B
:*B
&B
-B
�B	�QB	׍B	ٚB	ڠB	�yB	�tB	��B	�B	�NB	�B	�AB	o�B	`\B	Z7B	UMB	S�B	J�B	B[B	<B	2�B	*B	$&B	 �B	�B	{B�lB��B��B��B��B��B��B��BѷBοB�"B�dBǔB�SB��B�B�TB� B�kB��B�B��B�VB��B�BB�5B�~B��B�B�?B�B�~B�xB�7B��B�BB�B{BzDBwfBv�Bu�BvBs�Bq�Bq�Br-BmCBm)BlqBl=Bl�Bi�BiyBd�Bb�Ba�Ba�Ba�Ba�Ba�Ba�Ba�BbB_;B]~B]IB_VB^jB\�B[	BZQBXEBS@BOBO�BI7BI�BI�BH�BH�BK^BXBaBbNB_�B^B\CBYBTFBW�BZB_�B_�B^�B^�B_�Bc�Bc�Be�Bq�B{0B{dB{0B��B��B�B�	B�xB��B��B��B�B�.B��B�9B�B��B�TB�tB��B�DB�0B�eB��B�KB��B��B��B��B�?B�>B�6B��B��B�;BňB�DB�PB�BB�hB�uBևBٴB�IB�tB�B�B�B��B��B�CB�B�]B�.B�.B�}B	{B	_B	fB		lB	xB	�B	�B	�B	�B	EB	�B	�B	 �B	"B	%B	(XB	.cB	0�B	3hB	4B	9�B	=�B	=�B	>�B	>�B	@�B	E�B	F�B	G�B	J�B	O�B	T�B	W
B	XB	YeB	]dB	`�B	^�B	\xB	_�B	_�B	^�B	c�B	g�B	h�B	i�B	l�B	p!B	u�B	v�B	xB	��B	��B	��B	�XB	��B	�4B	�'B	�3B	�tB	�tB	�YB	�YB	��B	��B	��B	��B	�dB	�vB	��B	��B	�{B	�gB	�$B	��B	��B	��B	��B	��B	�B	ÖB	żB	��B	�0B	�B	�<B	ϫB	��B	�1B	�tB	�^B	ҽB	��B	�
B	�B	�B	�#B	�	B	�/B	�VB	�pB	�B	�B	�B	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	��B	�B	��B	�B	�B	�B	��B	�	B	��B	��B	�B
 B
B
 B
AB
AB
oB
[B
[B
GB
MB
gB
SB
tB
YB
EB
EB
EB
EB
_B
�B
	lB

XB

=B

XB

XB

XB

rB

rB
xB
�B
�B
�B
�B
�B
}B
}B
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
B
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
!�B
"�B
"�B
#B
#�B
#�B
"�B
#�B
#�B
#�B
$�B
$�B
$B
$ZB
&B
'B
(
B
($B
($B
($B
($B
($B
($B
)DB
*0B
*KB
*B
,=B
,=B
,=B
-CB
-CB
-]B
.IB
.IB
/OB
/iB
0;B
0;B
0UB
1[B
1'B
2GB
2GB
2GB
2GB
2GB
2GB
3MB
3MB
3MB
3MB
3MB
3hB
49B
4TB
4TB
4TB
49B
4B
4TB
4TB
4TB
5?B
5?B
5?B
5?B
5ZB
5tB
5�B
6�B
7�B
7fB
7LB
7fB
7LB
7�B
8lB
8�B
8�B
9rB
9XB
9�B
9�B
:xB
:xB
:xB
:�B
;�B
;�B
;B
;�B
<�B
<�B
<�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
?}B
@�B
@�B
@�B
@�B
@�B
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
D�B
D�B
E�B
E�B
E�B
E�B
E�B
GB
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
NB
N"B
OB
O(B
O�B
O�B
O�B
P�B
Q B
Q B
Q B
Q B
RB
RB
RB
RB
RB
RB
R�B
SB
SB
SB
R�B
S&B
S@B
TaB
T�B
T�B
UB
UMB
VB
W$B
W$B
W$B
W$B
W$B
XEB
X+B
XEB
YB
Y1B
X�B
YB
YB
YB
YB
Y1B
Y1B
Y1B
YB
ZB
YB
Y1B
Z7B
YB
Z7B
ZQB
Z7B
[=B
[=B
[=B
[=B
[WB
[WB
\CB
\B
\CB
\]B
]dB
]IB
]IB
^B
^5B
^B
^5B
^OB
^OB
^OB
^5B
^OB
^OB
^OB
_;B
_VB
_VB
_VB
_pB
_pB
`\B
`\B
abB
abB
abB
bhB
bhB
bhB
bNB
bNB
b4B
bhB
bhB
bhB
cTB
cTB
cnB
cTB
cTB
cTB
cTB
cnB
cTB
cnB
cnB
cTB
cnB
d�B
dtB
dZB
dZB
dtB
d�B
e�B
ezB
ezB
ffB
ffB
ffB
f�B
f�B
f�B
ffB
gmB
g�B
g�B
gmB
g�B
h�B
h�B
h�B
h�B
h�B
i�B
iyB
iyB
i�B
i�B
j�B
jB
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
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
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
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
u�B
t�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
xB
xB
w�B
w�B
w�B
w�B
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
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
{�B
{�B
|B
{�B
{�B
{�B
}B
|�B
}B
|�B
|�B
|�B
|�B
|�B
}B
}B
~B
~B
}�B
}�B
}�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.02(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201611240034322016112400343220161124003432201806221305172018062213051720180622130517201804050705172018040507051720180405070517  JA  ARFMdecpA19c                                                                20161120093507  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161120003537  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161120003538  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161120003538  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161120003539  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161120003539  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161120003539  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161120003539  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161120003539  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161120003539                      G�O�G�O�G�O�                JA  ARUP                                                                        20161120013148                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161120153620  CV  JULD            G�O�G�O�F�ۭ                JM  ARCAJMQC2.0                                                                 20161123153432  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161123153432  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220517  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040517  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211515                      G�O�G�O�G�O�                