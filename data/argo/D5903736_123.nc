CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-09-03T09:15:32Z AOML 3.0 creation; 2016-05-31T19:14:45Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150903091532  20160531121445  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               {A   AO  4051_7090_123                   2C  D   APEX                            5368                            041511                          846 @�l~�S�1   @�l�o�B@3�������da����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    {A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp��Bx  B��B���B���B�  B�33B���B���B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D>��D?y�D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV�fDW  DW�fDX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�Ds�fDy��D�fD�9�D�|�D��fD�3D�33D�p D���D�	�D�S3D��fD�ٚD�3D�&fDڃ3D��fD�fD�C3D�vfD��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��
@���A Q�A Q�A@Q�A`Q�A�(�A�(�A�(�A�(�A�(�A�(�A���A���B {B{B{B{B {B({B0{B8{B@{BH{BP{BX{B`{Bh{Bp�HBx{B�B��
B��
B�
=B�=pB��
B��
B�
=B�
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
=B�
=B�
=B�
=B�=pB��
B��
B�
=B�
=B�
=B�
=C CCCCC
CCCCCCCCCCC C"C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD>��D?z�D@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV��DWHDW��DXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDsz�Ds�Dy�D�
D�:>D�}qD��
D��D�3�D�p�D��qD�
>D�S�D��
D��>D��D�'
Dڃ�D��
D�
D�C�D�w
D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�\A�  A��yA��/A���A�ĜA�A镁A�A��mA�+A�S�A��mA�A�hA�l�A�dZA�\)A�C�A�(�A�{A�VA���A�A�A�G�A�33A�&�A���A�!A�"�A㗍A�(�A�G�A��A�bA�r�A֏\A�?}A��A�E�Aΰ!A�I�A̅A�hsAȃA�{A���AǗ�A�t�A��;A�v�A�M�A�bAĸRA�C�A�(�A��mA���A���A�&�A�x�A�5?A��7A� �A��#A���A�n�A�K�A�oA��/A�v�A��#A�p�A�JA��`A�Q�A�ZA���A�VA��-A�t�A�`BA�1A���A��DA�`BA��TA�bA��A��PA� �A��jA���A�9XA�^5A��A�bA��hA�9XA���A�ƨA��#A�C�A�M�A���A�t�A��HA�/A�(�A�;dA�33A���A�JA�ffA�G�A���A��FA���A���A���A�&�A�ȴA���A��uA��DA�A}l�A{�Az=qAx�/Av�/AudZAtr�Ar��Ap�Ao�^Aop�Al�Aj�\Ai�mAh�Ag�Ae?}Ac�Ab�A`�jA]��AYXAV-AS��AQ�^AM��AKt�AI�hAHbNAC�
AA�7A?��A<M�A:5?A8�+A7
=A5K�A4  A2�A1S�A/�A-��A,Q�A)��A&��A%�A$�!A#?}A"r�A �+A bA��A�7AK�A��A�7A^5A�AVA�mA��A33A��AĜA��AȴAE�AAJA�
A��Al�A��A�uA�A�HA��A�A�AQ�A
�A
 �A
ĜA
�A�AXA
�yA�A�DA�A�\A�`A��@��y@���@���@��@���@�dZ@���@��^@��j@�ƨ@��@���@�x�@��@�A�@ꗍ@�/@�Z@�M�@��@�-@�hs@�Q�@���@��@���@�A�@�33@�Ĝ@۾w@�l�@���@�V@ٙ�@��@�Q�@�~�@Ձ@�ƨ@�dZ@��H@�V@ѡ�@�Ĝ@�I�@�C�@�ȴ@·+@���@�hs@�b@�\)@�E�@ɺ^@�hs@���@��@��y@ư!@ř�@�V@þw@�|�@��H@+@���@�I�@�@���@��/@�(�@���@���@��T@��@�z�@�9X@��F@��y@��!@�E�@���@���@��7@�hs@�V@���@�I�@��w@��P@�|�@�S�@�K�@�
=@���@���@�E�@��^@���@�`B@�G�@�`B@�V@��@�z�@��@��F@���@�t�@�;d@���@�^5@��#@�`B@��@���@�Q�@�9X@� �@��@��@���@�J@���@��^@��-@�V@�r�@�1'@�bN@�A�@��F@�\)@���@�E�@�X@��@��9@���@�j@��
@�\)@�33@�+@�o@���@��+@�n�@���@���@�+@�+@�@��@�%@��u@��@��@�\)@���@���@��+@�ff@�E�@�E�@�=q@�-@��@���@�?}@�/@���@��9@�Z@�9X@�b@��F@��@��y@���@�~�@�^5@�V@�V@�M�@�$�@���@�`B@�&�@��`@��j@��@��u@�  @�\)@�"�@���@���@���@���@��@��^@���@�%@�I�@�(�@�b@��;@��P@�K�@�33@�+@�"�@�
=@��+@�{@���@�G�@��@��@��/@�z�@�Z@�A�@� �@� �@�1@�  @��;@�ƨ@�t�@�
=@��@���@�ff@�M�@�5?@��@��-@�x�@��@���@�Ĝ@��9@�z�@�Z@�  @��@��@��m@��
@��@�S�@�C�@�
=@�ȴ@��+@���@���@��h@�p�@�?}@�%@���@��9@���@�r�@���@�@{t�@so@ko@b�@XQ�@O;d@G��@@Q�@9�@3�m@+33@$1@�;@33@��@��@
=@�F@ �1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�\A�  A��yA��/A���A�ĜA�A镁A�A��mA�+A�S�A��mA�A�hA�l�A�dZA�\)A�C�A�(�A�{A�VA���A�A�A�G�A�33A�&�A���A�!A�"�A㗍A�(�A�G�A��A�bA�r�A֏\A�?}A��A�E�Aΰ!A�I�A̅A�hsAȃA�{A���AǗ�A�t�A��;A�v�A�M�A�bAĸRA�C�A�(�A��mA���A���A�&�A�x�A�5?A��7A� �A��#A���A�n�A�K�A�oA��/A�v�A��#A�p�A�JA��`A�Q�A�ZA���A�VA��-A�t�A�`BA�1A���A��DA�`BA��TA�bA��A��PA� �A��jA���A�9XA�^5A��A�bA��hA�9XA���A�ƨA��#A�C�A�M�A���A�t�A��HA�/A�(�A�;dA�33A���A�JA�ffA�G�A���A��FA���A���A���A�&�A�ȴA���A��uA��DA�A}l�A{�Az=qAx�/Av�/AudZAtr�Ar��Ap�Ao�^Aop�Al�Aj�\Ai�mAh�Ag�Ae?}Ac�Ab�A`�jA]��AYXAV-AS��AQ�^AM��AKt�AI�hAHbNAC�
AA�7A?��A<M�A:5?A8�+A7
=A5K�A4  A2�A1S�A/�A-��A,Q�A)��A&��A%�A$�!A#?}A"r�A �+A bA��A�7AK�A��A�7A^5A�AVA�mA��A33A��AĜA��AȴAE�AAJA�
A��Al�A��A�uA�A�HA��A�A�AQ�A
�A
 �A
ĜA
�A�AXA
�yA�A�DA�A�\A�`A��@��y@���@���@��@���@�dZ@���@��^@��j@�ƨ@��@���@�x�@��@�A�@ꗍ@�/@�Z@�M�@��@�-@�hs@�Q�@���@��@���@�A�@�33@�Ĝ@۾w@�l�@���@�V@ٙ�@��@�Q�@�~�@Ձ@�ƨ@�dZ@��H@�V@ѡ�@�Ĝ@�I�@�C�@�ȴ@·+@���@�hs@�b@�\)@�E�@ɺ^@�hs@���@��@��y@ư!@ř�@�V@þw@�|�@��H@+@���@�I�@�@���@��/@�(�@���@���@��T@��@�z�@�9X@��F@��y@��!@�E�@���@���@��7@�hs@�V@���@�I�@��w@��P@�|�@�S�@�K�@�
=@���@���@�E�@��^@���@�`B@�G�@�`B@�V@��@�z�@��@��F@���@�t�@�;d@���@�^5@��#@�`B@��@���@�Q�@�9X@� �@��@��@���@�J@���@��^@��-@�V@�r�@�1'@�bN@�A�@��F@�\)@���@�E�@�X@��@��9@���@�j@��
@�\)@�33@�+@�o@���@��+@�n�@���@���@�+@�+@�@��@�%@��u@��@��@�\)@���@���@��+@�ff@�E�@�E�@�=q@�-@��@���@�?}@�/@���@��9@�Z@�9X@�b@��F@��@��y@���@�~�@�^5@�V@�V@�M�@�$�@���@�`B@�&�@��`@��j@��@��u@�  @�\)@�"�@���@���@���@���@��@��^@���@�%@�I�@�(�@�b@��;@��P@�K�@�33@�+@�"�@�
=@��+@�{@���@�G�@��@��@��/@�z�@�Z@�A�@� �@� �@�1@�  @��;@�ƨ@�t�@�
=@��@���@�ff@�M�@�5?@��@��-@�x�@��@���@�Ĝ@��9@�z�@�Z@�  @��@��@��m@��
@��@�S�@�C�@�
=@�ȴ@��+@���@���@��h@�p�@�?}@�%@���@��9@���@�r�@���@�@{t�@so@ko@b�@XQ�@O;d@G��@@Q�@9�@3�m@+33@$1@�;@33@��@��@
=@�F@ �1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
J�B
K�B
K�B
K�B
I�B
I�B
J�B
K�B
J�B
ZB
e`B
l�B
z�B
�B
�JB
��B
��B
��B
��B
��B
��B
��B
�B
�RB
ǮB
�
B
�5B
�TB
�BDB�B)�BC�B��B�?B�mB"�B�7B�B��B�BgmB�B��B��B�FB�B��B��B��B�!B�dBB�B{B��B�BB�yB�B%�B2-B7LB8RB8RB7LB5?B6FB;dB<jB=qBA�B,BVBPBPBDBJBVB
=B��B��BPB��B��B��B�B�B��B�yB�
B�5BBDBhB
=BB��B�fB��B��B�?B��B`BB$�B�BŢB�^B�'B�RBƨB��BŢB��B�hB�B^5B?}B�B+B
�B
�ZB
ǮB
��B
�1B
x�B
s�B
dZB
P�B
I�B
=qB
.B
 �B
�B
DB	��B	�B	�B	�;B	��B	ȴB	�wB	�?B	��B	��B	�hB	�B	k�B	O�B	?}B	33B	#�B	�B	PB	%B��B��B�B�`B�#B��B��BɺBĜBB�wB�^B�?B�!B�B��B��B��B��B��B��B�{B�oB�hB�hB�\B�PB�=B�%B�B�B�B� B|�B|�B�B�B�%B�+B�DB�{B��B��B��B��B��B�B�B�'B�B��B��B��B�B�qB��BǮB�B�B��B�jB�9B�B��B��B�hB�\B�hB�oB�oB�hB�hB�bB�\B�\B�\B�VB�\B�bB�hB�oB�hB�bB�oB�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�'B�3B�3B�9B�9B�?B�FB�LB�RB�RB�dB�jB�}B��BÖBƨBɺBɺB��B��B��B�B�B�B�#B�)B�5B�BB�`B�fB�yB�B�B�B�B��B��B	  B	B	B	%B	JB	JB	PB	PB	VB	hB	hB	oB	{B	{B	�B	�B	�B	�B	�B	�B	 �B	"�B	"�B	$�B	'�B	(�B	,B	.B	1'B	2-B	33B	49B	6FB	7LB	;dB	@�B	C�B	D�B	E�B	H�B	H�B	I�B	L�B	P�B	Q�B	S�B	YB	YB	ZB	\)B	^5B	aHB	ffB	jB	k�B	k�B	n�B	o�B	n�B	m�B	m�B	m�B	m�B	n�B	o�B	p�B	p�B	p�B	r�B	u�B	w�B	~�B	�B	�B	�B	�%B	�%B	�1B	�=B	�=B	�DB	�VB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�?B	�FB	�FB	�FB	�FB	�FB	�RB	�^B	�^B	�^B	�dB	�jB	�jB	�jB	��B	B	ĜB	ŢB	ƨB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�)B	�5B	�;B	�BB	�HB	�HB	�TB	�ZB	�fB	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
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
JB
VB
uB
�B
"�B
&�B
/B
:^B
A�B
D�B
G�B
P�B
XB
`BB
ffB
jB
n�B
q�B
v�B
y�B
{�B
� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
J�B
K�B
K�B
K�B
I�B
I�B
J�B
K�B
J�B
Z B
edB
l�B
z�B
�B
�MB
��B
��B
��B
��B
��B
��B
��B
�B
�UB
ǯB
�	B
�7B
�TB
�BDB�B)�BC�B��B�=B�kB"�B�8B�B��B�BgkB�!B��B��B�FB�B��B��B��B�!B�dBB�B|B��B�BB�xB�B%�B2-B7OB8TB8RB7NB5BB6GB;eB<jB=qBA�B,	BTBNBQBCBKBUB
<B��B��BPB��B��B��B�B�B��B�xB�B�8BBFBhB
>BB��B�gB��B��B�?B��B`@B$�B�BŝB�_B�%B�QBƦB��BŢB��B�dB�B^2B?|B�B*B
�B
�[B
ǬB
��B
�2B
x�B
s�B
d[B
P�B
I�B
=vB
.B
 �B
�B
JB	��B	�B	�B	�@B	��B	ȼB	��B	�GB	��B	��B	�qB	�(B	k�B	O�B	?�B	3AB	#�B	�B	_B	4B�B��B�B�nB�3B�B��B��BĮB¡B��B�oB�OB�4B�B��B��B��B��B��B��B��B��B�{B�}B�oB�aB�PB�:B�-B�B�B�B}B}B�B�3B�:B�?B�WB��B��B��B��B��B��B�B�B�9B�B��B��B��B�B��B��BǽB�.B�%B��B�zB�KB�B��B��B�zB�qB�{B��B��B�{B�}B�rB�pB�nB�qB�hB�oB�rB�}B��B�}B�sB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�%B�&B�7B�8B�CB�BB�IB�HB�OB�TB�[B�cB�bB�tB�xB��B��BåBƷB��B��B��B��B��B�B�B�$B�0B�9B�BB�OB�nB�vB�B�B�B�B��B��B��B	 B	B	%B	1B	SB	VB	]B	[B	bB	tB	uB	zB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	"�B	$�B	'�B	)B	,B	.B	11B	28B	3<B	4FB	6OB	7UB	;pB	@�B	C�B	D�B	E�B	H�B	H�B	I�B	L�B	P�B	Q�B	TB	Y#B	Y B	Z'B	\3B	^?B	aRB	fnB	j�B	k�B	k�B	n�B	o�B	n�B	m�B	m�B	m�B	m�B	n�B	o�B	p�B	p�B	p�B	r�B	u�B	w�B	B	�B	�B	�"B	�.B	�-B	�7B	�EB	�EB	�KB	�^B	�uB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�&B	�1B	�EB	�JB	�JB	�MB	�LB	�LB	�YB	�eB	�eB	�cB	�lB	�pB	�qB	�pB	��B	B	ĢB	ŧB	ƮB	ǴB	ǵB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�"B	�#B	�,B	�:B	�?B	�EB	�OB	�LB	�WB	�^B	�iB	�iB	�lB	�rB	�}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 B
 B
	B
B
B
B
B
"B
)B
2B
1B
1B
1B
.B
7B
	;B

AB

AB
MB
[B
yB
�B
"�B
&�B
/B
:bB
A�B
D�B
G�B
P�B
XB
`AB
fhB
jB
n�B
q�B
v�B
y�B
{�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.02 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214452016053112144520160531121445  AO  ARCAADJP                                                                    20150903091532    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150903091532  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150903091532  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121445  IP                  G�O�G�O�G�O�                