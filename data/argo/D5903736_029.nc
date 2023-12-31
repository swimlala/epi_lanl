CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:21Z AOML 3.0 creation; 2016-05-31T19:14:29Z UW 3.1 conversion     
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
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20140721230521  20160531121429  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_029                   2C  D   APEX                            5368                            041511                          846 @�{�_$1   @�{����@4���vȴ�d�$�/�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @@  @�  @�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33Bי�B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO�fDPfDP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� DnfDn�fDo  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy�3D�3D�C3D�vfD��fD�  D�FfD���D���D�3D�FfD�� D�� D� D�I�Dڃ3D๚D��D�C3D� D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @?\)@\)@��@��A�
A?�
A_�
A~=pA��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B�.B�.B���B���B���B���B���B���B���B���B�.Bה{B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO��DP�DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dn�Dn��Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt��Dy��D��D�B�D�vD��D���D�FD��HD��{D��D�FD���D�ϮD��D�IHDڂ�D�HD�{D�B�D��D��{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�|�A�hsA�C�AζFAΕ�A�~�A�r�A�l�A�dZA�dZA�^5A�ZA�ZA�XA�VA�VA�VA�VA�VA�VA�VA�S�A�S�A�S�A�Q�A�Q�A�S�A�S�A�S�A�S�A�S�A�Q�A�S�A�Q�A�S�A�S�A�S�A�VA�VA�VA�VA�VA�VA�XA�XA�ZA�\)A�\)A�^5A�`BA�`BA�bA�{A��A�S�A�jA���A�~�A��FA��9A�r�A��A�%A�5?A�G�A��A�9XA��A���A��FA�/A��DA�9XA��`A���A���A�l�A�ȴA�5?A��;A�A�=qA��FA�A�33A�bA�ȴA��DA��!A��A�A�`BA��HA�C�A���A�~�A�\)A��yA���A��`A�^5A�`BA���A��!A�&�A�dZA�$�A�r�A�{A���A�?}A�VA��A��A���A�Q�A�$�A��RA�^5A���A�
=A���A�p�A��#A��+A��-A��A���A�\)A�oAG�A~��A}A|�RAy�TAyVAw��At5?AodZAk�Af��Ae+Ac33Ab-A`��A`{A^ĜA]�PA\�uA\�A\ZA[�PAZ��AX��AW��AV{ATI�AQ�AQ&�AP1'AM��ALv�AJ-AHȴAE�wAD�AB��AAVA?�A?XA>ĜA=�A;��A:ĜA:  A8�A7�^A6��A5�^A4�A3dZA2�A1�A09XA.bNA-�7A+;dA)&�A(�uA&�A&VA&A%�A$�A"��A!33Al�A�\A��AhsA"�A1'A�mA/AdZAJA+Az�A33AA�!AƨAt�A��AbNAdZAS�A9XA�FA
v�A�yA7LAhsAZAJA`BA5?A��AO�A �DA M�A A�@��@�Q�@�E�@�&�@�dZ@�&�@�Ĝ@��j@��@�ff@�@�n�@�?}@�9X@�33@�~�@�h@�I�@�\@�r�@�G�@ߥ�@�E�@�/@�&�@��@�1@��@�v�@�{@�p�@��m@��@�1'@�ƨ@�~�@�@��@Դ9@�\)@ҧ�@�/@�1@���@ϥ�@Л�@���@υ@�;d@�$�@�ȴ@��
@�@�V@�@ŉ7@�X@�?}@��@�V@�Q�@��@�E�@��h@�`B@���@��@�"�@�V@���@���@�A�@��
@��y@�{@��#@��u@�+@�$�@�%@��@�Q�@��;@�K�@���@�{@�O�@��@�z�@��m@�;d@���@�E�@��@��7@�7L@��9@��w@�K�@�o@�@��y@���@�E�@��^@�x�@�&�@��@��`@��j@���@�z�@�Q�@�b@��m@��P@�ȴ@�M�@��@���@��@�`B@�?}@��@���@��@�r�@�Q�@�I�@�A�@��@��@�dZ@���@��\@�5?@�J@��T@��^@���@���@��^@��-@�p�@���@�Z@�1'@�  @��
@��@�t�@�S�@��@��!@���@��+@�v�@�^5@�=q@��@�J@�@�hs@�G�@�/@��@�V@�%@��@��@��@�z�@�Z@�9X@� �@�b@���@��F@�t�@���@��F@���@�  @��@�r�@�j@�K�@�v�@��@��@��`@��@�Z@�1@��@�K�@��y@�ȴ@�~�@�$�@���@�X@�/@��@��@��@���@�j@��m@�|�@�33@�@��H@���@�V@��@���@��@�G�@���@���@���@��D@�Q�@�A�@���@��@�33@�ȴ@�ff@��@���@��#@���@��^@���@��@��@��j@���@�Z@��@��F@���@�t�@�dZ@�S�@�+@�
=@���@�~�@�E�@�{@��#@��h@��@�p�@�p�@�I�@���@y��@rn�@ix�@`  @W��@N�y@F�y@>{@7��@3o@.ff@)�#@%O�@;d@�@��@�@��@	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�|�A�hsA�C�AζFAΕ�A�~�A�r�A�l�A�dZA�dZA�^5A�ZA�ZA�XA�VA�VA�VA�VA�VA�VA�VA�S�A�S�A�S�A�Q�A�Q�A�S�A�S�A�S�A�S�A�S�A�Q�A�S�A�Q�A�S�A�S�A�S�A�VA�VA�VA�VA�VA�VA�XA�XA�ZA�\)A�\)A�^5A�`BA�`BA�bA�{A��A�S�A�jA���A�~�A��FA��9A�r�A��A�%A�5?A�G�A��A�9XA��A���A��FA�/A��DA�9XA��`A���A���A�l�A�ȴA�5?A��;A�A�=qA��FA�A�33A�bA�ȴA��DA��!A��A�A�`BA��HA�C�A���A�~�A�\)A��yA���A��`A�^5A�`BA���A��!A�&�A�dZA�$�A�r�A�{A���A�?}A�VA��A��A���A�Q�A�$�A��RA�^5A���A�
=A���A�p�A��#A��+A��-A��A���A�\)A�oAG�A~��A}A|�RAy�TAyVAw��At5?AodZAk�Af��Ae+Ac33Ab-A`��A`{A^ĜA]�PA\�uA\�A\ZA[�PAZ��AX��AW��AV{ATI�AQ�AQ&�AP1'AM��ALv�AJ-AHȴAE�wAD�AB��AAVA?�A?XA>ĜA=�A;��A:ĜA:  A8�A7�^A6��A5�^A4�A3dZA2�A1�A09XA.bNA-�7A+;dA)&�A(�uA&�A&VA&A%�A$�A"��A!33Al�A�\A��AhsA"�A1'A�mA/AdZAJA+Az�A33AA�!AƨAt�A��AbNAdZAS�A9XA�FA
v�A�yA7LAhsAZAJA`BA5?A��AO�A �DA M�A A�@��@�Q�@�E�@�&�@�dZ@�&�@�Ĝ@��j@��@�ff@�@�n�@�?}@�9X@�33@�~�@�h@�I�@�\@�r�@�G�@ߥ�@�E�@�/@�&�@��@�1@��@�v�@�{@�p�@��m@��@�1'@�ƨ@�~�@�@��@Դ9@�\)@ҧ�@�/@�1@���@ϥ�@Л�@���@υ@�;d@�$�@�ȴ@��
@�@�V@�@ŉ7@�X@�?}@��@�V@�Q�@��@�E�@��h@�`B@���@��@�"�@�V@���@���@�A�@��
@��y@�{@��#@��u@�+@�$�@�%@��@�Q�@��;@�K�@���@�{@�O�@��@�z�@��m@�;d@���@�E�@��@��7@�7L@��9@��w@�K�@�o@�@��y@���@�E�@��^@�x�@�&�@��@��`@��j@���@�z�@�Q�@�b@��m@��P@�ȴ@�M�@��@���@��@�`B@�?}@��@���@��@�r�@�Q�@�I�@�A�@��@��@�dZ@���@��\@�5?@�J@��T@��^@���@���@��^@��-@�p�@���@�Z@�1'@�  @��
@��@�t�@�S�@��@��!@���@��+@�v�@�^5@�=q@��@�J@�@�hs@�G�@�/@��@�V@�%@��@��@��@�z�@�Z@�9X@� �@�b@���@��F@�t�@���@��F@���@�  @��@�r�@�j@�K�@�v�@��@��@��`@��@�Z@�1@��@�K�@��y@�ȴ@�~�@�$�@���@�X@�/@��@��@��@���@�j@��m@�|�@�33@�@��H@���@�V@��@���@��@�G�@���@���@���@��D@�Q�@�A�@���@��@�33@�ȴ@�ff@��@���@��#@���@��^@���@��@��@��j@���@�Z@��@��F@���@�t�@�dZ@�S�@�+@�
=@���@�~�@�E�@�{@��#@��h@��@�p�@�p�@�I�@���@y��@rn�@ix�@`  @W��@N�y@F�y@>{@7��@3o@.ff@)�#@%O�@;d@�@��@�@��@	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBBBB  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  B  B��B  B  B  B  BBBBBBBB��B�B�B��B�TB�B��B��B��B��B��B��B�B�B�fB�TB�B�B�`B�B��B��B��B��B��B�B�sB�HB��BǮB�}B�'B��B��B�oB�PB�B~�B~�B|�Bt�B[#BB�B/B�BuBJBB��B�yB�HB�5B�BɺBŢB�dB�?B�'B��B��B�PB�Bs�BjBe`BN�B.B�BoB
��B
��B
�FB
�B
��B
��B
��B
�+B
M�B
49B
1'B
&�B
�B
+B
B	��B	�)B	�dB	��B	�JB	�+B	�B	�B	|�B	q�B	p�B	m�B	k�B	k�B	iyB	e`B	aHB	ZB	VB	M�B	D�B	:^B	5?B	/B	#�B	�B	uB	PB	B��B��B�B�B�yB�fB�5B�B�B��B��B��B��B��B��BĜB��B�}B�dB�FB�-B�B��B��B��B��B��B��B��B�{B�bB�JB�=B�7B�1B�+B�%B�B�B�B}�Bz�Bx�Bv�Bt�Bs�Bs�Br�Bq�Bq�Bm�BiyBgmBe`BcTBbNB`BBffBffBbNBaHBbNBbNBbNBdZBe`BcTBe`BhsBffBdZB_;B^5B_;BffBhsBe`BaHB\)B\)B]/B_;B_;B`BB`BB^5B]/B`BBbNBcTBe`Bk�Bl�Bl�BjBhsBgmBgmBn�Bu�B~�B~�Bz�By�B� Bz�Bz�Bz�Bw�Bu�By�B~�B�%B�DB�uB��B�{B�oB�PB�PB�bB�oB��B��B��B��B��B�!B�9B�9B�9B�FB�XB�jB��BĜBŢB��B��B��B��B�
B�
B�)B�NB�mB�B�B�B�B�B��B��B	  B	B	%B	
=B	\B	uB	�B	�B	�B	�B	!�B	(�B	-B	/B	0!B	0!B	33B	6FB	;dB	>wB	A�B	B�B	C�B	E�B	G�B	H�B	H�B	I�B	J�B	L�B	S�B	YB	[#B	\)B	^5B	bNB	cTB	e`B	iyB	k�B	o�B	p�B	q�B	q�B	t�B	u�B	u�B	u�B	w�B	y�B	y�B	y�B	|�B	�B	�B	�+B	�1B	�=B	�DB	�VB	�\B	�bB	�bB	�bB	�hB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�FB	�RB	�^B	�dB	�qB	�qB	�jB	�jB	�jB	�dB	�jB	�}B	��B	B	B	ÖB	ŢB	ŢB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�/B	�5B	�5B	�;B	�;B	�BB	�NB	�TB	�`B	�`B	�`B	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
PB
{B
�B
�B
(�B
0!B
9XB
@�B
G�B
M�B
Q�B
T�B
\)B
`BB
ffB
k�B
p�B
u�B
x�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BBBB  B��B� B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B B B B��B B B B B
BBBBBBB��B�B�B��B�WB�B��B��B��B��B��B��B�B�B�iB�WB�B�B�cB�B��B��B��B��B��B�B�tB�JB��BǰB��B�*B��B��B�oB�RB�B B~�B|�Bt�B[%BB�B/B�BtBHBB��B�yB�HB�6B�BɽBţB�eB�?B�(B��B��B�SB�Bs�Bj�BeaBN�B.B�BpB
��B
��B
�IB
�B
��B
��B
��B
�1B
M�B
4BB
1-B
&�B
�B
4B
!B	��B	�5B	�rB	��B	�XB	�7B	�!B	�B	|�B	q�B	p�B	m�B	k�B	k�B	i�B	enB	aVB	Z-B	VB	M�B	D�B	:oB	5NB	/,B	#�B	�B	�B	aB	B��B��B�B�B�B�yB�IB�,B�B�B�B�B�B��B��BĲB��B��B�zB�\B�BB� B��B��B��B��B��B��B��B��B�zB�aB�UB�PB�HB�DB�<B�2B�,B�B~Bz�Bx�Bv�Bt�Bs�Bs�Br�Bq�Bq�Bm�Bi�Bg�BezBcmBbeB`ZBf|Bf}BbeBabBbfBbgBbeBdtBezBclBexBh�Bf}BdrB_UB^NB_SBf~Bh�BevBa_B\@B\CB]EB_SB_QB`[B`WB^MB]HB`XBbgBckBewBk�Bl�Bl�Bj�Bh�Bg�Bg�Bn�Bu�BBBz�By�B�Bz�Bz�Bz�Bw�Bu�By�BB�;B�ZB��B��B��B��B�fB�gB�xB��B��B��B��B��B��B�4B�LB�NB�KB�YB�mB�}B��BıBŶB��B��B��B�
B�B�B�<B�bB�B�B��B�B�B��B��B��B	 B	#B	6B	
KB	lB	�B	�B	�B	�B	�B	!�B	)B	-B	/+B	0/B	01B	3@B	6WB	;qB	>�B	A�B	B�B	C�B	E�B	G�B	H�B	H�B	I�B	J�B	L�B	TB	Y%B	[.B	\8B	^CB	bZB	caB	emB	i�B	k�B	o�B	p�B	q�B	q�B	t�B	u�B	u�B	u�B	w�B	y�B	y�B	y�B	|�B	�B	�+B	�6B	�=B	�IB	�OB	�aB	�fB	�mB	�oB	�mB	�sB	�sB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�7B	�OB	�_B	�fB	�pB	�{B	�{B	�rB	�tB	�tB	�kB	�vB	��B	��B	B	B	àB	ŬB	ŪB	ƱB	ȻB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�%B	�*B	�6B	�>B	�>B	�DB	�BB	�KB	�RB	�\B	�hB	�iB	�hB	�vB	�{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
UB
�B
�B
�B
(�B
0'B
9^B
@�B
G�B
M�B
Q�B
UB
\-B
`IB
fkB
k�B
p�B
u�B
x�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.01 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214292016053112142920160531121429  AO  ARCAADJP                                                                    20140721230521    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230521  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230521  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121429  IP                  G�O�G�O�G�O�                