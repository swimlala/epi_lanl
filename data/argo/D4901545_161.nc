CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-10-02T17:02:27Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  fH   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0        �0Argo profile    3.1 1.2 19500101000000  20171002170227  20181025093510  4901545 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  4741                            2C  D   NAVIS_A                         0185                            052512                          863 @�*�1�m�1   @�*����`@9����+�c6��n�1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      �A   A   A   @���@�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB ffB(ffB0  B8  B@  BH  BP  BX  B`  Bh  BpffBw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DXfDX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy�fDz31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�=q@���A Q�A Q�A>�RA`Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B{B{Bz�B z�B(z�B0{B8{B@{BH{BP{BX{B`{Bh{Bpz�Bw�B�
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
=B��
B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=C CCCCC
CCCCCCCCCCC C"C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDX�DX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt�HDuHDu�HDvHDv�HDwHDw�HDxHDx�HDyHDy��Dz{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AہAہA�~�AۅAۗ�AۮA۬A۰!A۴9A�A�7LA�O�A�?}A�"�A���A�l�A�33A�{A��A��`A��;A���A���AڼjAڣ�A�&�A�ffA�x�AήAɡ�A�hsAé�A�hsA�XA��!A�p�A���A���A��^A� �A�\)A���A���A��A��A��HA���A��`A��mA��A��A��7A�ȴA�M�A��7A��A�S�A�C�A���A�-A��A�S�A�C�A�9XA���A���A���A���A�oA��!A�/A���A��A��A�=qA���A�$�A��TA�I�A�|�A���A�
=A�E�A���A��+A��TA�ȴA�O�A���A�S�A�;dA�ĜA���A�|�A��A�S�A�S�A�7LA�;dA�p�A��A}ƨA|JA{��A{AxM�Au��At�As|�Arr�Aq�FAp�uAoG�An=qAl��Ai�#Ag�;Ag
=Ae�Ab�\Aa�-Aa;dA]dZA[S�AY��AWx�AUS�AT-AT��AU/AUS�AVVAU��AT�AR9XAPbNAO33ANA�AN��AN��AMt�AM33AL-AJȴAIx�AH��AGG�AFbAE"�AC�AA�#A@  A>A�A<�A:�yA9�FA:bA9��A9��A8�/A7�A7��A7;dA61'A5&�A41A3�;A3�7A2�/A1�A1l�A0z�A/�hA.�A.bA-XA,I�A+�
A+��A+`BA*�A)�A)�A'�FA'`BA'G�A'"�A&ZA%C�A#t�A" �A!�A!/A!%A �+A�wA�RAJA"�A�TA`BA�A�9A1'A|�AO�A�RA�FA%A��A�!A��A-A��Az�A�A��A�9A��A/A��AXA�RA�#A��Ax�AG�A9XA��A
�9A
Q�A	��A	�AȴAffA\)AbNAO�A��A�AdZA��A��A �@�;d@�%@���@�n�@��@���@���@�@�O�@�@���@��H@��@�%@�@��#@�h@�/@�z�@畁@�5?@���@��@�v�@��#@�Q�@�v�@�%@�b@�n�@�1@�G�@ӝ�@�-@�V@��;@�$�@��@ͩ�@�  @�@�|�@�@���@�o@��@��u@�ȴ@��^@���@�  @�\)@��@��^@���@���@��H@��@��j@��@���@�t�@�+@��@���@�bN@��@�J@�p�@�%@���@�9X@��@���@�@��!@�M�@��7@�7L@�Ĝ@��u@�z�@�9X@��;@���@�\)@�o@�@���@��+@�-@�@�&�@��`@��9@���@��@�j@��;@�;d@��@���@�J@��^@�p�@���@�z�@��;@�l�@��y@�ff@�x�@��@��u@��@�j@���@�"�@�$�@��/@�b@��F@�"�@��y@���@��@�x�@�&�@�Ĝ@�Z@��w@�|�@�33@�
=@�n�@��^@��7@���@��7@�7L@��/@��D@�b@��@�S�@�C�@�K�@��P@�|�@�K�@�"�@��y@���@�V@�@�7L@���@���@�bN@�9X@�b@���@��w@�
=@�E�@�-@�J@��T@���@���@���@��h@�O�@���@�bN@��@��@�33@��@���@���@��\@��@��^@��h@�p�@�7L@��`@�j@�1@���@�@���@���@���@�-@��@��T@���@�&�@���@�bN@�Z@� �@~�@~E�@}p�@|�j@{�F@{o@z�!@zJ@yG�@x�`@x�u@xr�@xQ�@x  @w��@w;d@vȴ@v��@v��@vff@u��@up�@u/@u�@t�/@tj@s��@sdZ@r�!@r^5@q�7@q%@p�9@pbN@p  @o��@o�w@o��@o|�@o\)@o;d@o��@p�u@q7L@p�`@p�9@p�@pr�@pA�@o�@ol�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AہAہA�~�AۅAۗ�AۮA۬A۰!A۴9A�A�7LA�O�A�?}A�"�A���A�l�A�33A�{A��A��`A��;A���A���AڼjAڣ�A�&�A�ffA�x�AήAɡ�A�hsAé�A�hsA�XA��!A�p�A���A���A��^A� �A�\)A���A���A��A��A��HA���A��`A��mA��A��A��7A�ȴA�M�A��7A��A�S�A�C�A���A�-A��A�S�A�C�A�9XA���A���A���A���A�oA��!A�/A���A��A��A�=qA���A�$�A��TA�I�A�|�A���A�
=A�E�A���A��+A��TA�ȴA�O�A���A�S�A�;dA�ĜA���A�|�A��A�S�A�S�A�7LA�;dA�p�A��A}ƨA|JA{��A{AxM�Au��At�As|�Arr�Aq�FAp�uAoG�An=qAl��Ai�#Ag�;Ag
=Ae�Ab�\Aa�-Aa;dA]dZA[S�AY��AWx�AUS�AT-AT��AU/AUS�AVVAU��AT�AR9XAPbNAO33ANA�AN��AN��AMt�AM33AL-AJȴAIx�AH��AGG�AFbAE"�AC�AA�#A@  A>A�A<�A:�yA9�FA:bA9��A9��A8�/A7�A7��A7;dA61'A5&�A41A3�;A3�7A2�/A1�A1l�A0z�A/�hA.�A.bA-XA,I�A+�
A+��A+`BA*�A)�A)�A'�FA'`BA'G�A'"�A&ZA%C�A#t�A" �A!�A!/A!%A �+A�wA�RAJA"�A�TA`BA�A�9A1'A|�AO�A�RA�FA%A��A�!A��A-A��Az�A�A��A�9A��A/A��AXA�RA�#A��Ax�AG�A9XA��A
�9A
Q�A	��A	�AȴAffA\)AbNAO�A��A�AdZA��A��A �@�;d@�%@���@�n�@��@���@���@�@�O�@�@���@��H@��@�%@�@��#@�h@�/@�z�@畁@�5?@���@��@�v�@��#@�Q�@�v�@�%@�b@�n�@�1@�G�@ӝ�@�-@�V@��;@�$�@��@ͩ�@�  @�@�|�@�@���@�o@��@��u@�ȴ@��^@���@�  @�\)@��@��^@���@���@��H@��@��j@��@���@�t�@�+@��@���@�bN@��@�J@�p�@�%@���@�9X@��@���@�@��!@�M�@��7@�7L@�Ĝ@��u@�z�@�9X@��;@���@�\)@�o@�@���@��+@�-@�@�&�@��`@��9@���@��@�j@��;@�;d@��@���@�J@��^@�p�@���@�z�@��;@�l�@��y@�ff@�x�@��@��u@��@�j@���@�"�@�$�@��/@�b@��F@�"�@��y@���@��@�x�@�&�@�Ĝ@�Z@��w@�|�@�33@�
=@�n�@��^@��7@���@��7@�7L@��/@��D@�b@��@�S�@�C�@�K�@��P@�|�@�K�@�"�@��y@���@�V@�@�7L@���@���@�bN@�9X@�b@���@��w@�
=@�E�@�-@�J@��T@���@���@���@��h@�O�@���@�bN@��@��@�33@��@���@���@��\@��@��^@��h@�p�@�7L@��`@�j@�1@���@�@���@���@���@�-@��@��T@���@�&�@���@�bN@�Z@� �@~�@~E�@}p�@|�j@{�F@{o@z�!@zJ@yG�@x�`@x�u@xr�@xQ�@x  @w��@w;d@vȴ@v��@v��@vff@u��@up�@u/@u�@t�/@tj@s��@sdZ@r�!@r^5@q�7@q%@p�9@pbN@p  @o��@o�w@o��@o|�@o\)@o;d@o��@p�u@q7L@p�`@p�9@p�@pr�@pA�@o�@ol�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB �B �B �B!�B#�B&�B)�B/BW
B�JB�`BBBBB+B+B+B+B+B1B	7B
=B
=B1BB��B�B�HB�/B�
B��B��B��B��B��B�jB��B��B�7Bz�By�Bw�Br�Bp�Bk�BcTB]/BVBK�BC�B?}B;dB:^B8RB7LB9XB6FB/B)�B�BVBB��B�B�BȴB�LB�B��B��B�+BjBQ�BE�B<jB33B%�B�B�BL�BB�B1'B+B&�B"�B�BhBPB%B
��B
�B
�B
�B
�fB
�)B
��B
�dB
�B
��B
��B
�oB
�B
~�B
w�B
dZB
R�B
I�B
@�B
9XB
49B
+B
"�B
�B
JB	��B	�B	�NB	��B	�LB	�B	��B	x�B	e`B	XB	C�B	0!B	)�B	K�B	^5B	ffB	~�B	{�B	p�B	ZB	F�B	<jB	6FB	@�B	I�B	?}B	F�B	>wB	0!B	"�B	�B	�B	uB	JB	  B�B�#B��B�qB�-B�B�XB�jB��B��B�qB�dB�XB�3B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�bB�JB�7B�%B�B�B�B~�Bz�Bu�Bq�Bo�Bn�Bl�BjBhsBffBffBcTB_;B]/B\)BZBXBW
BT�BQ�BO�BL�BL�BL�BK�BJ�BF�BC�BA�B?}B=qB:^B7LB5?B33B2-B0!B0!B/B.B,B+B)�B)�B(�B)�B(�B'�B'�B&�B$�B#�B"�B!�B�B�B�B�B�B�B�B�BuBoBhBbBbB\B\B\B\BVB\BbBhBhB\BVB\B\BVBVBVBPBPBJBJBJB\BbBbBbBbBoBoBhBhBVBJBJBPBPBPBbBuB{B�B�B�B�B�B�B�B�B�B�B �B!�B!�B!�B �B!�B#�B$�B&�B%�B$�B%�B)�B,B-B/B1'B2-B8RB;dB>wB?}B?}B?}BA�BB�BD�BG�BI�BK�BL�BN�BO�BT�BW
BXBXBXBXB[#B`BBbNBdZBffBgmBgmBiyBjBl�Bm�Bn�Bo�Bt�Bx�B{�B�B�B�1B�PB�VB�VB�bB�uB��B��B��B��B��B��B�B�B�B�!B�'B�-B�3B�LB�RB�jB��B��BBÖBŢBƨBȴBɺBɺB��B��B�B�B�B�#B�)B�;B�NB�ZB�`B�fB�mB�mB�B�B�B��B��B��B��B��B��B��B��B��B	B	%B	+B		7B	DB	VB	VB	\B	\B	oB	{B	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	#�B	%�B	'�B	)�B	)�B	,B	/B	49B	6FB	6FB	8RB	>wB	@�B	C�B	E�B	F�B	I�B	J�B	N�B	R�B	T�B	W
B	XB	YB	[#B	]/B	`BB	cTB	cTB	dZB	e`B	hsB	jB	l�B	m�B	n�B	p�B	u�B	v�B	x�B	z�B	}�B	~�B	� B	�B	�B	�B	�B	�B	�+B	�1B	�7B	�JB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B �B �B �B!�B#�B&�B)�B/	BV�B��B��BHB�BZB�BB�B�BWB<BIB	MB
`B
tB	?B�B~BKB��B�dB�B�9BؕB�~B�%BϫBʻB��B�B��B�BB��B{�BtXBs/Bp�Bf�Ba�B_BO@BF~BC(B=�B>�B:�B;�B?RB=�B4B1tB% BJB�B�(B�B��B�B��B��B��B�`B��Bn�BTEBG�B>�B7�B(_B"B�BQBF:B2�B-�B*WB(�BBlB)B4B
�OB
�MB
�B
��B
�B
�lB
эB
�~B
��B
��B
��B
�B
�_B
��B
~�B
j[B
U�B
M�B
C<B
;qB
7GB
.eB
%\B
&B
B	��B	�PB	�B	�B	�!B	�	B	�B	|�B	h�B	\DB	G�B	2_B	(�B	K
B	]�B	dHB	� B	~�B	u�B	^�B	IoB	>�B	5}B	@�B	LfB	@;B	IYB	A�B	3tB	$<B	!�B	�B	B	�B	�B�4B߮BϋB«B�(B� B��B��B��B��B�jB�aB��B��B��B�QB��B�rB��B��B�~B��B�UB��B��B��B��B��B�
B��B�B�\B�^B��B�SB�~B��B��BByBs4BpsBoBm�Bl�BkBh%Bh�Bf{B`�B^^B\�B[eBY�BW�BV}BTgBQwBMXBMBL�BL�BM�BG�BDyBB�BA�B?FB<AB:(B6�B4�B4IB0�B0mB/�B0�B-�B-jB+B+YB*�B*�B*B*�B*�B)�B&�B%�B$MB#�B"oB�B �B�BZB�BBqB7BSB�BvBiB BBByBB�BB�B�BvB�B�BBKB�B)BQB�B�B�BHB�B�BBB�B�B�BwB�B5B�B�BUB	BeBoB�B�B>BWBBB�BB�BsB�B�B!<B!�B"/B"EB"|B#qB$�B'%B'�B&vB%3B&�B*kB,�B-�B/�B1�B3YB8�B<
B>�B?�B?�B?�BA�BB�BD�BG�BI�BLBM<BO	BP�BUKBW?BX#BX6BX+BX�B[�B`�Bb�BeBf�Bg�BhBjBkBBmBn-Bo7Bp�BubBy@B{�B�'B��B�B��B��B�_B��B�6B��B��B��B�ZB�[B��B��B��B�xB��B�fB�B�:B��B��B��B��B�B�	B�MB�/B�,B��BɨB�WB��B�EB�BB�fBمBۘB�
B�B��B�B��B�B�B��B�B�B��B��B�B�B�4B��B��B� B�UB��B	wB	�B	�B		�B	�B	�B	�B	�B	;B	�B	�B	�B	�B		B	DB	8B	KB	�B	#B	#�B	$B	&{B	(BB	*B	*UB	,�B	/�B	4�B	6SB	6�B	9KB	>�B	AB	DB	F\B	GB	I�B	KB	OIB	S%B	U,B	WB	X B	YJB	[WB	]fB	`�B	cfB	c\B	dxB	e�B	h�B	j�B	l�B	m�B	n�B	q%B	u�B	w?B	y
B	{kB	~QB	2B	�9B	�OB	�/B	�B	�1B	�6B	�GB	�AB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�6�<=�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<sE=<C�$<.(?<#�
<#�
<1$�<#�
<#�
<#�
<#�
<#�
<#�
<&<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =-0.02 dbar                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201810231352072018102313520720181023135207  AO  ARCAADJP                                                                    20171002170227    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20171002170227  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20171002170227  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20181023135207  QC  PRES            @���Dz3G�O�                PM  ARSQCTM V1.1                                                                20181023135207  QC  PSAL            @���Dz3G�O�                PM  ARSQOWGUV1.0CTD_2017v1 + Argo_2017v02                                       20181025093510  IP                  G�O�G�O�G�O�                