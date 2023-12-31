CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-01-02T20:19:43Z AOML 3.0 creation; 2016-08-07T21:17:44Z UW 3.1 conversion     
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
resolution        =���     �  Cx   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KX   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  MP   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �p   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20160102201943  20160807141744  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ^A   AO  5285_8895_094                   2C  D   APEX                            6487                            072314                          846 @׊��.01   @׊؄��_@0)�^5?}�d[I�^1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ^A   B   B   @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�33B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�fD��D�@ D��fD��3D�,�D�S3D�y�D��3D��D�C3D�p D��3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@L��@���@���AffA&ffAFffAfffA�33A�33A�33A�33A�33A�33A�33A�33B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B���B���B���B���B���B���B���B�fgB�fgB���B���B���B�  B�  B���B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���C ffCffCffCffCffC
ffCffCffCffCffCffCffCffCffCffCffC ffC"ffC$ffC&ffC(ffC*ffC,ffC.ffC0ffC2ffC4ffC6ffC8ffC:ffC<ffC>� C@ffCBffCDffCFffCHffCJffCLffCNffCPffCRffCTffCVffCXffCZffC\ffC^ffC`ffCbffCdffCfffChffCjffCl� CnffCpffCrffCtffCvffCxffCzffC|ffC~ffC�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�gDy� D�)�D�L�D��3D�� D�9�D�` D��gD�� D�)�D�P D�|�D�� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��
A��A��/A��TA��`A��;A��mA��A��A��A��A��yA��A��A��A��A�VAЁA��/A��TA��A���A�A�JA��A�=qA�dZAэPAї�Aћ�Aї�A�r�A�ZA�`BA�z�A�x�A�jA�G�A� �A���A�t�A˸RA�1A��yAŏ\A���A�I�AÁA��A��A��A�&�A�oA���A�{A���A�33A�$�A���A�1'A�bA��9A�1'A���A��-A���A�;dA�(�A�XA�ffA��mA��A�oA�1'A��#A���A��A�x�A�Q�A�VA�9XA�G�A�bNA�I�A�%A��hA�~�A�ƨA��+A���A~ĜAxbNAu�hAr�AmVAh(�Af�9Aa�mA\AT�\AR��AN��AKK�AD��AB�A@�+A?�-A=�A8Q�A6��A6{A5�A4�9A3��A2��A21A0z�A0=qA/�hA.�9A-�
A-&�A,A�A+�A*�jA*ZA*JA)x�A)
=A(bNA'dZA%��A$�9A#/A"n�A#\)A#�TA$A�A$�A%&�A$��A$��A$�/A$�A$A#��A#�A"�9A!�
A!�A!?}A �A jA��A�PAt�Ap�AXAC�A?}A;dA��A �A�wA��Al�AS�A/A�!A�+Az�A�A�
A��At�AVA1A��AdZA7LAVA��A  A�wA�yAn�AffAffAQ�A-A��A\)A��A{A�A�/AjAQ�A{A�TA��A"�A�A�A�A
=A��A��AI�A{A��A�PA%AȴA��AE�AhsAG�A
$�A	�A	C�Az�AI�A��Al�A�A�/A�A^5A  A��A
=A�`A��A�+A�A�AhsA�A�!A�AZA$�A��A��AK�A ��A I�A @�\)@���@���@�X@��@�j@���@�33@��R@��@���@��u@�(�@���@�C�@�ȴ@�v�@��-@�D@�Z@���@�ƨ@�@�|�@�C�@���@�hs@�r�@�A�@�(�@��@�F@�\)@�+@�=q@��
@��@�7@��@�j@��@�F@�+@���@�!@柾@�ff@�M�@�^@�7L@�u@�+@��@�h@�7L@��@�\)@���@ޗ�@�@�x�@�z�@�ff@ٲ-@��@أ�@� �@���@ם�@�C�@�
=@���@�n�@��T@պ^@�`B@Դ9@Ӆ@�33@�ȴ@�v�@�5?@��@�@с@с@Ь@�(�@�b@���@υ@�;d@Χ�@�M�@�$�@��@�J@�7L@�bN@�9X@��@�  @˾w@�t�@ʧ�@ɲ-@�X@�/@��@���@���@�j@��@�b@��;@Ǯ@�t�@��@Ə\@Ł@�Ĝ@��m@ÍP@�\)@�"�@�$�@�hs@���@��u@��m@�o@�-@��7@��@�Ĝ@��@�9X@�ƨ@���@�l�@�o@�v�@�E�@���@��^@���@�G�@�V@��@�A�@��w@�|�@�C�@�M�@��T@�hs@���@�1@���@�t�@�S�@�33@�
=@���@�5?@��-@�G�@��`@���@���@�Q�@���@��@�"�@���@�n�@�=q@��@��^@���@���@��7@�`B@�7L@���@��@�I�@��m@�K�@�ȴ@�5?@��@�hs@��@�V@���@��@���@��u@��D@�I�@���@�\)@�33@��@��!@�^5@���@�?}@�%@��/@��@�z�@�9X@��@��
@���@��@��@�E�@��@��-@��7@�hs@�?}@��`@��9@��@��@��@��@��@���@��@�I�@�1'@�(�@��@��F@��@��@�t�@�/@���@��
@���@pr�@e��@^ȴ@U�T@Nv�@E?}@<�j@4��@/\)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111A��
A��A��/A��TA��`A��;A��mA��A��A��A��A��yA��A��A��A��A�VAЁA��/A��TA��A���A�A�JA��A�=qA�dZAэPAї�Aћ�Aї�A�r�A�ZA�`BA�z�A�x�A�jA�G�A� �A���A�t�A˸RA�1A��yAŏ\A���A�I�AÁA��A��A��A�&�A�oA���A�{A���A�33A�$�A���A�1'A�bA��9A�1'A���A��-A���A�;dA�(�A�XA�ffA��mA��A�oA�1'A��#A���A��A�x�A�Q�A�VA�9XA�G�A�bNA�I�A�%A��hA�~�A�ƨA��+A���A~ĜAxbNAu�hAr�AmVAh(�Af�9Aa�mA\AT�\AR��AN��AKK�AD��AB�A@�+A?�-A=�A8Q�A6��A6{A5�A4�9A3��A2��A21A0z�A0=qA/�hA.�9A-�
A-&�A,A�A+�A*�jA*ZA*JA)x�A)
=A(bNA'dZA%��A$�9A#/A"n�A#\)A#�TA$A�A$�A%&�A$��A$��A$�/A$�A$A#��A#�A"�9A!�
A!�A!?}A �A jA��A�PAt�Ap�AXAC�A?}A;dA��A �A�wA��Al�AS�A/A�!A�+Az�A�A�
A��At�AVA1A��AdZA7LAVA��A  A�wA�yAn�AffAffAQ�A-A��A\)A��A{A�A�/AjAQ�A{A�TA��A"�A�A�A�A
=A��A��AI�A{A��A�PA%AȴA��AE�AhsAG�A
$�A	�A	C�Az�AI�A��Al�A�A�/A�A^5A  A��A
=A�`A��A�+A�A�AhsA�A�!A�AZA$�A��A��AK�A ��A I�A @�\)@���@���@�X@��@�j@���@�33@��R@��@���@��u@�(�@���@�C�@�ȴ@�v�@��-@�D@�Z@���@�ƨ@�@�|�@�C�@���@�hs@�r�@�A�@�(�@��@�F@�\)@�+@�=q@��
@��@�7@��@�j@��@�F@�+@���@�!@柾@�ff@�M�@�^@�7L@�u@�+@��@�h@�7L@��@�\)@���@ޗ�@�@�x�@�z�@�ff@ٲ-@��@أ�@� �@���@ם�@�C�@�
=@���@�n�@��T@պ^@�`B@Դ9@Ӆ@�33@�ȴ@�v�@�5?@��@�@с@с@Ь@�(�@�b@���@υ@�;d@Χ�@�M�@�$�@��@�J@�7L@�bN@�9X@��@�  @˾w@�t�@ʧ�@ɲ-@�X@�/@��@���@���@�j@��@�b@��;@Ǯ@�t�@��@Ə\@Ł@�Ĝ@��m@ÍP@�\)@�"�@�$�@�hs@���@��u@��m@�o@�-@��7@��@�Ĝ@��@�9X@�ƨ@���@�l�@�o@�v�@�E�@���@��^@���@�G�@�V@��@�A�@��w@�|�@�C�@�M�@��T@�hs@���@�1@���@�t�@�S�@�33@�
=@���@�5?@��-@�G�@��`@���@���@�Q�@���@��@�"�@���@�n�@�=q@��@��^@���@���@��7@�`B@�7L@���@��@�I�@��m@�K�@�ȴ@�5?@��@�hs@��@�V@���@��@���@��u@��D@�I�@���@�\)@�33@��@��!@�^5@���@�?}@�%@��/@��@�z�@�9X@��@��
@���@��@��@�E�@��@��-@��7@�hs@�?}@��`@��9@��@��@��@��@��@���@��@�I�@�1'@�(�@��@��F@��G�O�@�t�@�/@���@��
@���@pr�@e��@^ȴ@U�T@Nv�@E?}@<�j@4��@/\)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	33B	2-B	33B	33B	33B	33B	49B	5?B	5?B	5?B	5?B	5?B	6FB	:^B	=qB	XB	�PB	�RB
bB
�B
$�B
)�B
+B
-B
0!B
A�B
O�B
^5B
bNB
cTB
cTB
`BB
]/B
aHB
m�B
w�B
�B
�hB
��B
��B�%B@�BJ�B;dBA�Be`B�+B�B��B�B��B�B�HB��B�RB��B��B�JB�JB��B�'B�wB��BdZBD�B'�BVB�B��B�BŢB�dB��B�uBv�BXBJ�B<jB5?B&�B+B%B
�HB
�qB
��B
~�B
bNB
K�B
D�B
5?B
�B	�B	�
B	��B	��B	u�B	dZB	=qB	JB�TB�mB�B�NB�fB�mB�B�B�B��B	  B	B	B	
=B	�B	$�B	1'B	E�B	G�B	O�B	W
B	e`B	n�B	q�B	u�B	{�B	}�B	~�B	�B	�B	�B	�%B	�+B	�1B	�B	�PB	�B	ĜB	�#B	�B	��B	��B
B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
#�B
$�B
&�B
'�B
(�B
(�B
(�B
)�B
)�B
(�B
'�B
(�B
)�B
)�B
+B
+B
,B
-B
-B
-B
.B
.B
/B
0!B
0!B
1'B
2-B
1'B
1'B
1'B
0!B
1'B
0!B
/B
0!B
0!B
0!B
/B
/B
.B
/B
33B
5?B
49B
49B
49B
49B
49B
49B
33B
33B
2-B
1'B
1'B
1'B
1'B
1'B
2-B
1'B
/B
-B
.B
/B
.B
-B
,B
+B
+B
)�B
'�B
'�B
'�B
&�B
&�B
%�B
%�B
%�B
$�B
#�B
"�B
"�B
"�B
!�B
!�B
!�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
uB
uB
uB
oB
oB
oB
hB
hB
hB
bB
\B
\B
\B
\B
\B
VB
VB
PB
JB
JB
JB
JB
DB
DB
DB

=B
1B
1B
+B
%B
%B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
  B
  B
  B
  B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
1B
1B
	7B

=B

=B

=B

=B

=B

=B
DB
DB
DB
JB
PB
PB
VB
VB
\B
\B
\B
bB
bB
bB
bB
bB
bB
hB
hB
hB
hB
hB
hB
oB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
1'B
�B
!�B
 �B
"�B
-B
6FB
;dB
@�B
D�B
J�B
O�B
VB
[#B
_;111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111B	3#B	2B	3%B	3#B	3$B	3#B	4+B	5/B	53B	52B	50B	51B	68B	:OB	=eB	X B	�=B	�<B
MB
�B
$�B
)�B
*�B
,�B
0B
AsB
O�B
^B
b3B
c=B
c=B
`+B
]B
a.B
mtB
w�B
��B
�NB
�yB
��B�B@`BJ�B;ABAfBe:B�
B��B��B��B��B�wB�$BʹB�1B��B��B�&B�*B��B�B�XB�dBd3BDwB'�B3B�_B��B��B�{B�@B��B�NBv�BW�BJ�B<HB5B&�BB B
�&B
�OB
��B
~�B
b+B
K�B
D{B
5B
qB	��B	��B	�cB	�|B	u�B	d=B	=TB	/B�9B�SB�pB�6B�MB�QB�vB�oB�}B��B��B	 �B	�B	
!B	nB	$�B	1B	E�B	G�B	O�B	V�B	e>B	nvB	q�B	u�B	{�B	}�B	~�B	��B	��B	��B	�B	�B	�B	��B	�-B	��B	�vB	��B	�jB	��B	��B
�B
kB
�B
�B
�B
�B
�B
�B
�B
 �B
#�B
$�B
&�B
'�B
(�B
(�B
(�B
)�B
)�B
(�B
'�B
(�B
)�B
)�B
*�B
*�B
+�B
,�B
,�B
,�B
-�B
-�B
.�B
/�B
/�B
0�B
2B
0�B
0�B
0�B
/�B
0�B
/�B
.�B
/�B
/�B
/�B
.�B
.�B
-�B
.�B
3B
5B
4B
4B
4B
4B
4B
4B
3B
3B
2B
0�B
0�B
0�B
0�B
0�B
2B
0�B
.�B
,�B
-�B
.�B
-�B
,�B
+�B
*�B
*�B
)�B
'�B
'�B
'�B
&�B
&�B
%�B
%�B
%�B
$�B
#�B
"�B
"�B
"�B
!�B
!�B
!�B
!�B
 �B
�B
�B
�B
�B
�B
�B
}B
uB
mB
pB
fB
iB
dB
aB
\B
[B
VB
NB
PB
JB
IB
LB
DB
EB
BB
=B
<B
:B
5B
2B
3B
1B
2B
0B
,B
*B
%B
B
!B
B
 B
B
B
B

B
B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
 B
�B
�B
B
B
		B

B

B

B

B

B

B
B
B
B
B
#B
"B
*B
(B
0B
/B
0B
4B
7B
7B
5B
7B
4B
;B
<B
;B
9B
;B
;B
BB
HB
IB
IB
HB
LB
MB
MB
NB
MB
LB
QB
[B
YB
YB
`B
`B
aB
bB
fB
fB
cB
fB
fB
dB
fB
fB
gB
hB
fB
^B
eB
mG�O�B
�B
!�B
 �B
"�B
,�B
6B
;6B
@SB
DlB
J�B
O�B
U�B
Z�B
_111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.4 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417442016080714174420160807141744  AO  ARCAADJP                                                                    20160102201943    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160102201943  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160102201943  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141744  IP                  G�O�G�O�G�O�                