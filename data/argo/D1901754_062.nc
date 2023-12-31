CDF   	   
      N_PROF        N_LEVELS  �   N_CALIB       STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         	DATE_TIME         N_PARAM       	N_HISTORY             	   title         Argo float vertical profile    institution       CSIRO      source        
Argo float     history       2021-09-23T05:42:30Z creation      
references        (http://www.argodatamgt.org/Documentation   user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         HPRIMARY | https://orcid.org/0000-0002-5914-7512 | Lyudmyla Koziy | CSIRO      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME       
         	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION         
         	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE       
         	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                    	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                   	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                    	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS                        conventions       Argo reference table 3     	long_name         ,List of available parameters for the station   
_FillValue                  0  7�   CYCLE_NUMBER                	long_name         Float cycle number     
_FillValue         ��   conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle           7�   	DIRECTION                   	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                    	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                   	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                   	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                   	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                      	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                    	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                   	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                      	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD                standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    conventions       8Relative julian days with decimal part (as parts of day)   units         "days since 1950-01-01 00:00:00 UTC     
resolution        >�����h�   
_FillValue        A.�~       axis      T           8|   JULD_QC                 	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                   	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   LATITUDE                	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            8�   	LONGITUDE                   	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            8�   POSITION_QC                 	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                     	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                 	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                 	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                 	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME          	         	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                   	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES                
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    axis      Z      
_FillValue        G�O�   	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  9�   PRES_QC                    	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED                   	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    
_FillValue        G�O�   	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  L�   PRES_ADJUSTED_QC                   	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \D   TEMP                	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  `   TEMP_QC                    	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ot   TEMP_ADJUSTED                   	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  sL   TEMP_ADJUSTED_QC                   	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL                	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  �|   PSAL_QC                    	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED                   	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  ��   PSAL_ADJUSTED_QC                   	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR                    	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   units         decibar    
_FillValue        G�O�     X  ��   TEMP_ADJUSTED_ERROR                    	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   
_FillValue        G�O�     X  �4   PSAL_ADJUSTED_ERROR                    	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   
_FillValue        G�O�     X  ˌ   	PARAMETER                            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION                   	         	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT                	         	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT                	         	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE                   
         	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                       	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                      	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                      	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                      	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                         	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE             
         	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                        	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                         	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                     	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                      	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                     	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                        	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1  1.219500101000000  20210923010347  20221011040002  1901754 Argo Australia                                                  Peter Oke                                                       PRES            TEMP            PSAL               >A   CS  1901754/62                      2C  D   NAVIS_EBR                       1061                            170425                          869 @ٕ�նl�1   @ٕ�l���DK��Z��@D�d��7�1   GPS     A   A   A   Primary sampling: averaged []                                                                                                                                                                                                                                      @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\�fD]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� DmfDm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D��3D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�
=@���A Q�A Q�A@Q�A`Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B@{BH{BP{BX{B`{Bh{Bp{Bx{B�
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
CCCCCCCCCCC C"C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`�CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\��D]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDm�Dm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy�HDzHDz�HD{HD{�HD|HD|�HD}HD}�HD~HD~�HDHD�HD� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�C�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�DဤD���D� �D�@�D․D���D� �D�@�D〤D���D� �D�@�D䀤D���D� �D�@�D値D���D� �D�@�D怤D���D� �D�@�D瀤D���D� �D�@�D耤D���D� �D�@�D逤D���D� �D�@�DꀤD���D� �D�@�D뀤D���D� �D�@�D쀤D���D� �D�@�D퀤D���D� �D�@�DD���D� �D�@�DD���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D�D���D� �D�@�D���D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�hsA�jA�jA�l�A�l�A�l�A�l�A�l�A�jA�jA�l�A�l�A�n�A�n�A�l�A�p�A�p�A�p�A�n�A�p�A�n�A�p�A�p�A�x�A�x�A�x�A�z�A�z�A�x�A�z�A�|�A�|�A�|�A�~�A�z�A�z�A�z�A�x�A�x�A�v�A�x�A��A��A��A��+A��7A��7A��DA��DA��DA��7A��7A��+A��+A��7A��7A��7A�~�A�VA�O�A�?}A�$�A��A�%A�A��A��A��
A�ĜA��A��DA�p�A�p�A�\)A�dZA�M�A�C�A�=qA�5?A�5?A�1'A��A�%A���A���A��A��`A��`A��/A��A�`BA�7LA�&�A�  A�
A��AA�-A�hAl�AK�A/A"�A~��A~ĜA~�9A~n�A~VA~M�A~$�A~$�A~1A}�wA}�A}��A}�FA}`BA}XA}\)A}O�A}C�A}?}A}K�A|�/A|��A{�PA{�Az9XAy��Ay��Ay��Ay�7AyhsAy+Ax�Ax�Aw?}Av�DAv-Au33As�mAr��ArQ�ArJAq�#Aqp�Ap�DAot�AoVAn��Am�Ak��Aj�`Aj�Aj�yAj�`Aj��Ai�;AidZAi�Ah�`Ah��AhJAg/Af��Afv�Af �Ae��Ad��Ad�RAd�9Ac�FAc7LAb�/Ab�\AbAa�7Aal�Aa%A`~�A_�A_hsA_+A^��A^�\A]ƨA]t�A\�A[�#A[��A[33A[�AZ��AZ�HAZn�AZAY7LAX�AX{AW��AWx�AV1'AU�mAUƨAUhsAU"�AT�AT��AT�+ATffAS��AS�;AS�
AS��AS��ASp�AS�ARA�AQ�AQ|�AQ7LAP��AP��AP�uAPJAO�AN��AN��ANVAM��AM��AMAM\)ALr�AL9XAL1'AL$�AK�AKK�AJ9XAI��AIK�AH9XAG�#AGt�AF��AF�jAF��AF-AE��AE�AEoAE
=AD��ADv�ADbNADZADQ�AD=qAD9XAD$�AC��ACK�AC
=AB�ABA�AA�FAA�A@bNA?�A?�FA?XA>z�A> �A>�A>bA>A=�A=ƨA=S�A=33A=VA<�A<�DA<�+A<z�A<bNA;��A;��A;K�A:�`A:=qA9�TA9t�A9�A8�A8JA7|�A6��A6 �A5��A4�HA4v�A4$�A3�TA3��A3�hA3p�A3VA2�RA2�uA2I�A1�A1�PA1A0��A09XA/�#A/��A/hsA/?}A.��A.��A.�A-�mA-A-�A-C�A,��A,ZA+�
A+dZA*�/A*�+A*�A)�#A)XA(�A'��A'/A'�A'%A&��A&�A&�+A&�A&ffA&{A%�;A%��A%/A$��A$n�A$(�A#�A#�FA"�HA"�!A"��A"I�A!�A!�FA!t�A ��A �A A�A $�AAC�A�9AZA�;A�wA�7A��A�A
=AA�A��A�mA�#A�-At�AO�A"�A�A�A�\AJA��A�
A�-A��Al�A(�AO�AbA�hA/A�yA��AffAbA��A��AdZA33A��A�AbNAbA��A��A�FA
�RA
r�A	�mA	�hA	x�A	C�A��AM�A�A��A��A��A\)A�A��Ar�AbNAE�A�A�#A|�Az�A��AdZA��AbNAbNAbNAM�A�TA �y@���@��T@��9@�dZ@�M�@�x�@���@�S�@�J@�`B@�@���@�F@�C�@��@��@�^5@���@� �@�F@띲@�C�@�V@��#@䛦@䛦@�b@�ff@���@�1@��m@߮@�K�@�n�@��;@��@ڏ\@��@��`@�Z@� �@���@ץ�@�ȴ@��@���@��@���@д9@� �@���@��@��@��
@ɺ^@�Z@���@ǶF@�t�@�"�@�@ċD@Å@��@�`B@���@��D@��@���@�&�@���@�l�@�ff@�V@� �@��
@��w@���@��@�dZ@���@���@��/@�ƨ@�t�@��H@�ff@�M�@���@��-@��h@�X@��@��/@�1@��@��P@�C�@�@�n�@�@�7L@��D@�1'@��m@���@���@�{@��-@��/@�9X@��@���@�;d@��@��T@�/@��9@�ƨ@���@���@��+@�v�@��@�`B@���@��u@�Q�@��w@��@�/@�A�@��
@��P@�K�@�o@��H@���@��@��@���@���@�9X@��;@��w@�;d@�ȴ@�v�@�^5@�$�@���@���@���@�A�@�  @�  @���@�;d@���@�^5@�@�O�@���@�j@��@�  @���@���@���@�dZ@�;d@�o@��@��@��\@�$�@���@���@�O�@�/@���@��D@�(�@��
@��P@�
=@��@��R@�{@���@�%@��D@�bN@�1'@�(�@� �@�  @�|�@�
=@��@���@�~�@�^5@�E�@�{@��T@���@�?}@��9@�Z@�Q�@�I�@�9X@�1'@�b@��
@�ƨ@�|�@�+@�
=@���@���@���@��\@�v�@�ff@�M�@�-@��#@��-@���@��h@��h@��@�p�@�hs@���@~�@}�@|�@{C�@z�H@z��@z~�@y�#@y�7@y7L@x�`@x�u@x �@w|�@w+@vȴ@v5?@t��@s�
@s��@sdZ@s@r�!@r^5@r-@rJ@q�@q��@p�`@pb@o��@o�w@o�@o�@o��@o�@nff@m@mV@l(�@k�
@k@j-@i��@iX@h�u@g�@f��@d�j@c��@b�@b-@a��@ax�@aG�@a7L@a%@`Ĝ@`�u@`Ĝ@`Q�@_�P@^��@^v�@^v�@^v�@^E�@]�@]�T@]�T@]��@]�@^@^{@]�-@]/@\��@\�@\z�@\9X@[�
@[ƨ@[t�@Z��@Zn�@ZM�@Z�@Y�#@YG�@X��@X��@X  @V��@V{@U��@Up�@UV@T�@TI�@S�m@SS�@So@R�@R�!@Rn�@Q�@P��@Pr�@PA�@P �@OK�@N��@N$�@M��@Mp�@L��@L�D@LI�@LI�@L9X@L(�@L�@Kƨ@K33@Ko@J�H@J��@J�\@J^5@J=q@J-@J�@J�@JJ@I��@I�#@I�^@I��@I�7@I7L@H��@H�`@H��@H�9@HbN@H1'@H �@G��@G�@G�@G�@G�@G��@Gl�@G�@Fv�@E�T@E��@E�@E�@Ep�@E`B@E/@EV@D��@DZ@D1@C��@C�
@C�F@C��@Ct�@CS�@CC�@C33@C33@CC�@Co@B��@B��@B��@B��@B�\@B�\@B^5@BJ@A�#@A�^@A��@Ax�@AG�@A&�@A�@@��@@��@@�u@@Q�@@b@?��@?�@?l�@>�@>��@>��@>ff@>{@=�@=�T@=�T@=��@=�-@=��@=�h@=p�@=?}@=�@<��@<�/@<�@<�D@<�@;ƨ@;��@;S�@;@;o@:�H@:�H@:�H@:��@:��@:M�@9�@9��@9��@9��@9��@9��@9��@9�^@9��@9x�@9&�@9%@8��@8Ĝ@8��@8Q�@8Q�@8A�@8b@7�@7�;@7�w@7��@7\)@7+@7+@7+@7+@7+@7
=@7
=@6��@6��@6��@6��@6��@6��@6�y@6�y@6�R@6��@6v�@6V@6E�@6E�@65?@65?@6$�@5�@5`B@5�@4��@4�@4�/@4��@4��@4�@4z�@4z�@4z�@4z�@4z�@4Z@49X@4Z@4j@4Z@4z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�hsA�jA�jA�l�A�l�A�l�A�l�A�l�A�jA�jA�l�A�l�A�n�A�n�A�l�A�p�A�p�A�p�A�n�A�p�A�n�A�p�A�p�A�x�A�x�A�x�A�z�A�z�A�x�A�z�A�|�A�|�A�|�A�~�A�z�A�z�A�z�A�x�A�x�A�v�A�x�A��A��A��A��+A��7A��7A��DA��DA��DA��7A��7A��+A��+A��7A��7A��7A�~�A�VA�O�A�?}A�$�A��A�%A�A��A��A��
A�ĜA��A��DA�p�A�p�A�\)A�dZA�M�A�C�A�=qA�5?A�5?A�1'A��A�%A���A���A��A��`A��`A��/A��A�`BA�7LA�&�A�  A�
A��AA�-A�hAl�AK�A/A"�A~��A~ĜA~�9A~n�A~VA~M�A~$�A~$�A~1A}�wA}�A}��A}�FA}`BA}XA}\)A}O�A}C�A}?}A}K�A|�/A|��A{�PA{�Az9XAy��Ay��Ay��Ay�7AyhsAy+Ax�Ax�Aw?}Av�DAv-Au33As�mAr��ArQ�ArJAq�#Aqp�Ap�DAot�AoVAn��Am�Ak��Aj�`Aj�Aj�yAj�`Aj��Ai�;AidZAi�Ah�`Ah��AhJAg/Af��Afv�Af �Ae��Ad��Ad�RAd�9Ac�FAc7LAb�/Ab�\AbAa�7Aal�Aa%A`~�A_�A_hsA_+A^��A^�\A]ƨA]t�A\�A[�#A[��A[33A[�AZ��AZ�HAZn�AZAY7LAX�AX{AW��AWx�AV1'AU�mAUƨAUhsAU"�AT�AT��AT�+ATffAS��AS�;AS�
AS��AS��ASp�AS�ARA�AQ�AQ|�AQ7LAP��AP��AP�uAPJAO�AN��AN��ANVAM��AM��AMAM\)ALr�AL9XAL1'AL$�AK�AKK�AJ9XAI��AIK�AH9XAG�#AGt�AF��AF�jAF��AF-AE��AE�AEoAE
=AD��ADv�ADbNADZADQ�AD=qAD9XAD$�AC��ACK�AC
=AB�ABA�AA�FAA�A@bNA?�A?�FA?XA>z�A> �A>�A>bA>A=�A=ƨA=S�A=33A=VA<�A<�DA<�+A<z�A<bNA;��A;��A;K�A:�`A:=qA9�TA9t�A9�A8�A8JA7|�A6��A6 �A5��A4�HA4v�A4$�A3�TA3��A3�hA3p�A3VA2�RA2�uA2I�A1�A1�PA1A0��A09XA/�#A/��A/hsA/?}A.��A.��A.�A-�mA-A-�A-C�A,��A,ZA+�
A+dZA*�/A*�+A*�A)�#A)XA(�A'��A'/A'�A'%A&��A&�A&�+A&�A&ffA&{A%�;A%��A%/A$��A$n�A$(�A#�A#�FA"�HA"�!A"��A"I�A!�A!�FA!t�A ��A �A A�A $�AAC�A�9AZA�;A�wA�7A��A�A
=AA�A��A�mA�#A�-At�AO�A"�A�A�A�\AJA��A�
A�-A��Al�A(�AO�AbA�hA/A�yA��AffAbA��A��AdZA33A��A�AbNAbA��A��A�FA
�RA
r�A	�mA	�hA	x�A	C�A��AM�A�A��A��A��A\)A�A��Ar�AbNAE�A�A�#A|�Az�A��AdZA��AbNAbNAbNAM�A�TA �y@���@��T@��9@�dZ@�M�@�x�@���@�S�@�J@�`B@�@���@�F@�C�@��@��@�^5@���@� �@�F@띲@�C�@�V@��#@䛦@䛦@�b@�ff@���@�1@��m@߮@�K�@�n�@��;@��@ڏ\@��@��`@�Z@� �@���@ץ�@�ȴ@��@���@��@���@д9@� �@���@��@��@��
@ɺ^@�Z@���@ǶF@�t�@�"�@�@ċD@Å@��@�`B@���@��D@��@���@�&�@���@�l�@�ff@�V@� �@��
@��w@���@��@�dZ@���@���@��/@�ƨ@�t�@��H@�ff@�M�@���@��-@��h@�X@��@��/@�1@��@��P@�C�@�@�n�@�@�7L@��D@�1'@��m@���@���@�{@��-@��/@�9X@��@���@�;d@��@��T@�/@��9@�ƨ@���@���@��+@�v�@��@�`B@���@��u@�Q�@��w@��@�/@�A�@��
@��P@�K�@�o@��H@���@��@��@���@���@�9X@��;@��w@�;d@�ȴ@�v�@�^5@�$�@���@���@���@�A�@�  @�  @���@�;d@���@�^5@�@�O�@���@�j@��@�  @���@���@���@�dZ@�;d@�o@��@��@��\@�$�@���@���@�O�@�/@���@��D@�(�@��
@��P@�
=@��@��R@�{@���@�%@��D@�bN@�1'@�(�@� �@�  @�|�@�
=@��@���@�~�@�^5@�E�@�{@��T@���@�?}@��9@�Z@�Q�@�I�@�9X@�1'@�b@��
@�ƨ@�|�@�+@�
=@���@���@���@��\@�v�@�ff@�M�@�-@��#@��-@���@��h@��h@��@�p�@�hs@���@~�@}�@|�@{C�@z�H@z��@z~�@y�#@y�7@y7L@x�`@x�u@x �@w|�@w+@vȴ@v5?@t��@s�
@s��@sdZ@s@r�!@r^5@r-@rJ@q�@q��@p�`@pb@o��@o�w@o�@o�@o��@o�@nff@m@mV@l(�@k�
@k@j-@i��@iX@h�u@g�@f��@d�j@c��@b�@b-@a��@ax�@aG�@a7L@a%@`Ĝ@`�u@`Ĝ@`Q�@_�P@^��@^v�@^v�@^v�@^E�@]�@]�T@]�T@]��@]�@^@^{@]�-@]/@\��@\�@\z�@\9X@[�
@[ƨ@[t�@Z��@Zn�@ZM�@Z�@Y�#@YG�@X��@X��@X  @V��@V{@U��@Up�@UV@T�@TI�@S�m@SS�@So@R�@R�!@Rn�@Q�@P��@Pr�@PA�@P �@OK�@N��@N$�@M��@Mp�@L��@L�D@LI�@LI�@L9X@L(�@L�@Kƨ@K33@Ko@J�H@J��@J�\@J^5@J=q@J-@J�@J�@JJ@I��@I�#@I�^@I��@I�7@I7L@H��@H�`@H��@H�9@HbN@H1'@H �@G��@G�@G�@G�@G�@G��@Gl�@G�@Fv�@E�T@E��@E�@E�@Ep�@E`B@E/@EV@D��@DZ@D1@C��@C�
@C�F@C��@Ct�@CS�@CC�@C33@C33@CC�@Co@B��@B��@B��@B��@B�\@B�\@B^5@BJ@A�#@A�^@A��@Ax�@AG�@A&�@A�@@��@@��@@�u@@Q�@@b@?��@?�@?l�@>�@>��@>��@>ff@>{@=�@=�T@=�T@=��@=�-@=��@=�h@=p�@=?}@=�@<��@<�/@<�@<�D@<�@;ƨ@;��@;S�@;@;o@:�H@:�H@:�H@:��@:��@:M�@9�@9��@9��@9��@9��@9��@9��@9�^@9��@9x�@9&�@9%@8��@8Ĝ@8��@8Q�@8Q�@8A�@8b@7�@7�;@7�w@7��@7\)@7+@7+@7+@7+@7+@7
=@7
=@6��@6��@6��@6��@6��@6��@6�y@6�y@6�R@6��@6v�@6V@6E�@6E�@65?@65?@6$�@5�@5`B@5�@4��@4�@4�/@4��@4��@4�@4z�@4z�@4z�@4z�@4z�@4Z@49X@4Z@4j@4Z@4z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BQ�BQ�BQ�BQ�BQ�BQ�BQ�BP�BP�BP�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BP�BP�BP�BP�BP�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BP�BQ�BO�BM�BM�BJ�BJ�BI�BH�BG�BG�BF�BE�BD�BB�B@�B@�B@�B?}B?}B>wB=qB<jB<jB<jB;dB9XB8RB8RB7LB6FB6FB6FB49B0!B-B-B+B)�B(�B(�B+B+B)�B)�B)�B)�B'�B&�B&�B"�B!�B!�B�B�B�B�B�B�B�BuBoBoBoBhBhBhBDB+B��B��B��B��B��B��B�B�B�B�B�TB�)B��B��BȴB�wB�3B�B�B��B��B��B�uB�\B�DB�Br�BffBffBp�Bs�B}�Bu�Bn�Bk�BhsBe`BcTB\)BW
BR�BO�BM�BT�BYBYBW
BQ�BO�BM�BJ�BG�BF�BE�BB�B?}B=qB<jB<jB:^B49B1'B,B"�B!�B�B�B�B�B�B�BVBB  B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�sB�TB�5B�#B�B��B��B��B��BȴBĜB��B��B�wB�qB�jB�^B�-B�B�B�B�B��B��B��B�hB�=B�%B�B~�B{�Bz�Bw�Br�Bm�Bm�Bm�Bl�BhsBgmBgmBgmBffBffBe`BbNB^5B[#BYBS�BN�BH�B@�B:^B7LB49B.B-B/B/B/B.B.B+B(�B'�B#�B"�B!�B!�B �B�B�B�BhBDB1BB  B��B��B�B�yB�ZB�BB�B��B��B��B��B��B��BȴBĜBB��B�jB�XB�9B�'B�B��B��B��B��B��B��B��B��B��B��B�oB�\B�=B�B�B|�B{�Bx�Bu�Bp�Be`B_;B\)B[#B[#BZBXBYBYBYBW
BT�BR�BO�BK�BH�BF�BD�BB�B=qB:^B:^B8RB49B2-B1'B,B(�B&�B%�B$�B!�B�B�B�B{BuBVB+BB
��B
��B
��B
��B
��B
��B
�B
�B
�B
�yB
�NB
�/B
�/B
�)B
�#B
�B
�B
��B
��B
ÖB
�}B
�jB
�XB
�LB
�?B
�-B
�!B
�B
�B
��B
��B
��B
��B
��B
��B
�oB
�=B
�B
�B
� B
}�B
|�B
|�B
|�B
{�B
z�B
z�B
y�B
y�B
y�B
y�B
y�B
x�B
w�B
v�B
u�B
t�B
r�B
m�B
ffB
dZB
bNB
_;B
_;B
_;B
^5B
\)B
W
B
K�B
E�B
A�B
=qB
:^B
7LB
5?B
2-B
-B
+B
'�B
$�B
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
�B
\B
	7B
1B
%B
B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�yB	�yB	�yB	�yB	�sB	�mB	�fB	�ZB	�HB	�)B	�
B	�B	��B	��B	��B	��B	��B	ĜB	�}B	�wB	�qB	�qB	�jB	�^B	�LB	�?B	�3B	�3B	�'B	�!B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�oB	�oB	�oB	�oB	�oB	�hB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�3B	�-B	�3B	�9B	�?B	�RB	�^B	�dB	�dB	�dB	�dB	�jB	�wB	�}B	�}B	��B	��B	��B	��B	��B	ÖB	ÖB	ŢB	ȴB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�5B	�;B	�BB	�BB	�BB	�HB	�HB	�NB	�NB	�;B	�HB	�HB	�NB	�HB	�NB	�NB	�TB	�TB	�TB	�ZB	�`B	�fB	�mB	�yB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
+B
1B
1B
	7B
	7B
	7B
	7B

=B

=B

=B
DB

=B

=B
DB
DB
PB
\B
hB
oB
uB
�B
�B
�B
�B
�B
�B
 �B
 �B
"�B
%�B
&�B
(�B
)�B
)�B
+B
.B
1'B
49B
5?B
6FB
7LB
8RB
:^B
:^B
;dB
>wB
?}B
?}B
@�B
A�B
C�B
C�B
D�B
E�B
F�B
G�B
F�B
G�B
F�B
G�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
K�B
K�B
L�B
L�B
K�B
L�B
M�B
M�B
M�B
M�B
N�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
T�B
T�B
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
XB
XB
YB
YB
ZB
ZB
ZB
[#B
\)B
]/B
^5B
_;B
`BB
_;B
aHB
bNB
bNB
bNB
cTB
dZB
e`B
gmB
jB
m�B
n�B
o�B
o�B
o�B
p�B
q�B
r�B
r�B
u�B
v�B
v�B
w�B
w�B
x�B
x�B
y�B
y�B
y�B
z�B
y�B
z�B
|�B
|�B
|�B
}�B
}�B
}�B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�1B
�1B
�=B
�=B
�JB
�\B
�bB
�bB
�oB
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�'B
�-B
�-B
�-B
�3B
�3B
�-B
�3B
�3B
�9B
�9B
�FB
�LB
�LB
�XB
�^B
�qB
�qB
�wB
�}B
��B
��B
��B
B
ĜB
ŢB
ĜB
ŢB
ŢB
ŢB
ŢB
ŢB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ǮB
ǮB
ȴB
ɺB
ɺB
ɺB
ɺB
ɺB
ɺB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�
B
�B
�B
�
B
�
B
�
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BQ�BQ�BQ�BQ�BQ�BQ�BQ�BP�BP�BP�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BP�BP�BP�BP�BP�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQBR�BO�BN$BNFBJ�BK%BI�BIBG�BH%BGBFBE4BCB@�B@�B@`B?�B?�B>�B=�B<mB<~B<�B;�B9�B8{B8~B7gB6GB6qB7!B5�B0�B-hB-�B+fB*B)
B)"B+IB+WB*IB*?B*B*�B(B'B'~B#B!�B")B�B�BQB�BDB�BHB�BdB�B�BrBSBdB�B	�B 6B��B�qB�+B�=B��B�B�4B�8B�mB�jB��B� B�7B��B�nB�&B��B��B�
B�
B�BB��B�cB��B�yBt�Bg
Be�Bp�BtB�Bv�BoPBl	BiBf�BeiB]BW�BS�BP�BO�BU�BY.B[kBX?BR�BP�BO!BK�BG�BG�BF�BC�B@�B>B<�B=fB<=B5B2�B.YB#�B"�B�B�B�B�B�BjBB:B �B��B��B��B�/B��B�zB�FB�"B�6B�B��B��B�B�B�B�nB�qB�|B�3B�IB��B�-B�LB�B�(B�,B�B�mB�RB�\B��B��B�kB�{B��B�.B�:B��B��B�uB�[B�5B��B�;B�(B�GB�B|=B{�ByBtBm�Bm�BnBmhBh�Bg�Bg�Bg�BftBf�Bf=Bc�B^�B\BZBUWBPYBJ�BA�B:�B8DB6OB/	B-/B/,B/9B/UB.xB/#B+TB)YB(�B$3B"�B!�B"B!�B�BuB�B�B7B	EB�B �B��B�AB��B��B�B�#B�-B��BҘB�zB�B�+B̹BɈB�B�JB�oB�`B��B�'B�?B��B��B�qB�IB�yB�8B��B�,B��B�+B�(B�+B��B��B�JB�YB}�B|�By{BwBs�Bf�B`>B\qB[RB[�BZ�BXBYBYbBY�BW�BU�BTBP�BL�BIiBGEBE2BD�B=�B:�B;&B9;B4�B2�B2�B,�B)�B':B&�B&B#5B�B�B�BBLB�B	�BB
��B
�B
��B
�4B
�cB
�!B
�.B
�B
��B
��B
�B
�uB
�B
܅B
�ZB
ڲB
�1B
�2B
��B
��B
�~B
�"B
�"B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
�oB
��B
~=B
}~B
~B
~DB
|wB
{9B
{KB
z@B
z�B
{(B
z|B
zBB
yB
xB
w>B
vgB
u�B
u>B
o�B
f�B
f:B
b�B
_FB
_;B
_xB
_TB
^�B
Z�B
MFB
G6B
CAB
>�B
;wB
8.B
71B
3�B
.B
-=B
*8B
'�B
!tB
�B
MB
B
�B
!�B
!PB
�B
=B
�B
�B
�B
	PB
�B
5B
@B	��B	�B	�$B	�\B	�B	�B	�	B	��B	�\B	��B	�4B	��B	�B	��B	�B	�TB	�B	�
B	ݧB	�NB	ֿB	�7B	�B	�cB	��B	ͦB	�pB	�B	��B	��B	��B	��B	�QB	��B	�4B	�%B	��B	��B	�xB	�/B	�5B	��B	��B	�6B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�'B	�B	�B	�YB	�3B	��B	�B	��B	��B	��B	�B	�B	��B	�BB	��B	�,B	�'B	��B	�_B	��B	��B	�EB	�+B	�/B	��B	��B	�SB	��B	��B	�#B	�'B	�6B	�B	�B	��B	�XB	��B	��B	��B	��B	��B	�@B	�SB	�B	�B	��B	�XB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�[B	��B	��B	��B	�B	� B	��B	�>B	�,B	�B	��B	��B	��B	�1B	��B	��B	�B	��B	��B	�tB	�XB	�8B	�4B	��B	�=B	�{B	�2B	��B	��B	��B	��B	�VB	�B	�
B	�B	��B	�CB	�uB	�jB	�?B	�`B	�.B	�TB	��B	��B	��B	��B	��B	�UB	��B	�B	��B	�!B	��B	��B	��B	�nB	�nB	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�#B	�`B	�*B	��B	��B	��B	��B	��B	�B	��B	�,B	�=B	�B	��B	��B	�9B	�AB	�+B	�(B	�6B	�FB	ڑB	�dB	�[B	�KB	�AB	�YB	�ZB	�ZB	�,B	�B	��B	�'B	�<B	�B	�eB	�yB	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	�B	�;B	��B	��B	�B	�B	�B	��B	��B	��B	�B	�oB	��B
 *B
B
B
B
B
aB
�B
�B
�B
�B
lB
�B
	�B
	|B
	�B
	�B

�B
.B
�B
B

�B

�B
�B
}B
pB
gB
�B
�B
�B
_B
�B
"B
BB
�B
�B
 �B
 �B
#B
%�B
&�B
(�B
)�B
)�B
*�B
.VB
1�B
4wB
5VB
6fB
7yB
8�B
:eB
:�B
;�B
>�B
?�B
?�B
@�B
A�B
C�B
C�B
EB
F�B
GB
G�B
F�B
G�B
F�B
G�B
G�B
IB
I�B
I�B
I�B
I�B
JB
LmB
L"B
L�B
L�B
L\B
M>B
N+B
NB
NB
N#B
O'B
PB
P�B
P�B
P�B
P�B
Q B
RSB
UB
UB
VB
V$B
V$B
VB
WB
WB
WB
XB
XB
X'B
Y(B
Y#B
Z3B
ZTB
ZKB
[-B
\3B
]EB
^kB
_\B
`IB
_rB
a[B
bIB
bKB
bMB
c^B
d{B
e�B
g�B
j�B
m�B
n�B
o�B
o�B
o�B
p�B
q�B
r�B
r�B
u�B
v�B
v�B
w�B
w�B
x�B
x�B
y�B
y�B
y�B
z�B
z B
{B
|�B
|�B
}B
}�B
}�B
~B
0B
�!B
�B
� B
�$B
�/B
�)B
�#B
�FB
�?B
�.B
�SB
�[B
�bB
�NB
�oB
��B
�yB
�jB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�6B
�%B
�B
�)B
�9B
��B
�/B
�B
�B
�B
�8B
�UB
�hB
�AB
�+B
�-B
�.B
�0B
�/B
�9B
�JB
�NB
�rB
�YB
�TB
�pB
�nB
��B
�hB
�}B
��B
��B
��B
��B
��B
»B
ĺB
śB
ğB
ŞB
ŠB
ŵB
šB
ŭB
ƥB
ƥB
ƥB
ƥB
ƦB
ƱB
ƧB
��B
ǷB
��B
��B
��B
ɸB
��B
ɸB
��B
��B
�&B
��B
��B
��B
��B
��B
��B
�B
�B
��B
��B
��B
��B
�B
�B
��B
� B
�B
��B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES= PRES - [PRES_SurfaceOffsetNotTruncated_dbar]; PRES_ADJUSTED = PRES                                                                                                                                                                                        no change                                                                                                                                                                                                                                                       PSAL_ADJUSTED = sal(CNDC,TEMP,PRES_ADJUSTED); PSAL_ADJ corrects conductivity cell thermal mass (CTM), Johnson et al, 2007, JAOT                                                                                                                                 PRES_SurfaceOffsetNotTruncated_dbar in TECH file for N-1 profile                                                                                                                                                                                                no change                                                                                                                                                                                                                                                       same as for PRES_ADJUSTED; CTL: alpha=0.1410, tau=6.68;                                                                                                                                                                                                         Pressures adjusted using PRES_SurfaceOffsetNotTruncated_dbar; Pressure drift corrected; Manufacturers sensor accuracy;                                                                                                                                          No significant temperature drift detected; Manufacturers sensor accuracy                                                                                                                                                                                        No significant salinity drift detected; Manufacturers sensor accuracy                                                                                                                                                                                           202210110400022022101104000220221011040002  CS  ARFMCSQCV4.0                                                                20210923054230    IP                G�O�G�O�G�O�                CS  ARGQCSQCV4.0                                                                20210923054230    IP                G�O�G�O�G�O�                CS  ARCACSQCV4.0                                                                20210923054230    IP                G�O�G�O�G�O�                CS  ARUPCSQCV4.0                                                                20210923054230    IP                G�O�G�O�G�O�                CS  ARGQCSQCV4.0                                                                20210923054230  QCP$                G�O�G�O�G�O�20DDB7E         CS  ARGQCSQCV4.0                                                                20210923054230  QCF$                G�O�G�O�G�O�0               CS  ARSQPADJV1.0                                                                20221011035252  CV  PRES            @�
=D���G�O�                CS  ARSQCTL v2.0                                                                20221011035308  QC  PSAL            @�
=D���G�O�                CS  ARSQSIQCV2.0WOD2001 & Argo                                                  20221011035852  IP                  @�ffD�� G�O�                