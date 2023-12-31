CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:08Z AOML 3.0 creation; 2016-05-31T19:14:25Z UW 3.1 conversion     
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
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230508  20160531121425  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_004                   2C  D   APEX                            5368                            041511                          846 @�;�㾿�1   @�;�m	?�@3{��S���dr5?|�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dyl�D�fD�I�D�ffD���D��D�C3D���D�� D���D�#3D�i�D��fD�fD�I�Dڐ D��fD��3D�I�D�s3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�z@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B�.B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Dyl)D�D�IHD�fD��{D�HD�B�D��HD�ϮD��HD�"�D�iHD��D�D�IHDڏ�D��D���D�IHD�r�D��{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�bA���A���A�ƨAȮAȇ+AȅA�~�A�v�A�p�A�n�A�l�A�jA�hsA�hsA�ffA�hsA�hsA�hsA�hsA�ffA�ffA�hsA�ffA�dZA�ffA�ffA�ffA�dZA�dZA�dZA�dZA�ffA�ffA�dZA�bNA�dZA�dZA�bNA�bNA�`BA�^5A�XA�VA�VA�S�A�A�A�/A�1A�ƨAǍPA�l�A�;dA���AƧ�A� �AŋDA��TA��#A�n�A�O�A���A�ȴA��-A��\A�S�A�{A�I�A��;A���A�%A�JA���A�S�A���A���A���A�S�A��A���A� �A��A�A�/A���A�?}A���A�9XA��`A�dZA���A�ƨA�-A��A�hsA�33A��DA�ȴA�v�A��A���A�E�A�v�A�33A�p�A��A��
A�ƨA��A�FA}Aw�PAv�jAul�Aq7LAo��Am�Al1Ai%Aep�A_�A]x�AZ�yAX�RAW��AWK�AVI�AUXAR��AO�#AM`BAK��AK33AJ��AJ^5AIK�AH9XAF�AE��AD�!AC�AC33ACVAA��A?�^A>��A<~�A:��A81'A7&�A5��A4��A4��A3/A2�\A2 �A0�9A.ĜA-��A,�A+�hA* �A(��A'�A'��A'�A#�#A"�+A"bNA"-A!�A ��A��AȴA��AƨA+A��AA�`AVA33A�9A=qA��A��A�wA\)A��A��AK�A�HA�;AZA�#A�PAĜA�TAS�A
�uA
�A	��Ar�A�A��AZA�A/A��A�A�A{A�#A`BA ff@�-@���@�K�@��@��R@��#@��@��^@���@�@�@�bN@�@�@�@���@�`B@��m@�l�@���@�\@��@��`@�I�@�A�@���@�Z@��
@��H@�^5@�Ĝ@��@�z�@�S�@ՙ�@�t�@���@�`B@Ο�@͑h@���@�r�@�A�@�b@˕�@��y@�E�@���@���@�t�@��@Ɵ�@�M�@ŉ7@ļj@��;@��H@°!@�hs@��@��;@���@��m@��H@��#@�J@�@�/@��`@��`@��/@�j@��@�l�@�K�@��@���@�Ĝ@�E�@�-@�=q@�^5@�n�@�M�@�5?@�~�@��-@��@�O�@��`@�z�@��
@���@�ȴ@�E�@�$�@�-@���@��@��j@�z�@�Z@�ƨ@�K�@�+@�
=@��@��^@��@�O�@��@���@���@�+@��@�v�@��-@�V@�j@��@��@�"�@��@��H@��H@��!@�~�@�M�@�@��#@��-@���@�x�@���@��D@�bN@�bN@� �@���@�C�@�o@�o@�;d@�;d@�+@�
=@��@�^5@���@�X@�`B@�O�@�r�@��@�  @��@�dZ@��@��@�"�@��@�K�@�
=@���@�-@��@��^@�@�$�@�$�@���@�p�@�%@�%@�%@���@���@�1@���@�"�@���@�J@�p�@��@�/@��@�@��@�Ĝ@��j@��u@�bN@�j@��u@��9@�&�@�O�@�X@���@���@�1'@��@��
@�S�@���@���@���@�-@���@���@���@��-@��h@��7@��@�/@���@��9@�I�@��@���@�S�@��@��@���@���@�E�@���@�x�@��7@��@�p�@�`B@��/@���@�I�@��@���@��F@�S�@�ȴ@�~�@�=q@�5?@��@��#@��^@���@��^@��-@��@�hs@�`B@�O�@�?}@��@���@��j@�I�@�9X@�(�@��w@�|�@�K�@�33@��@�@���@�$�@��T@���@�`B@��9@�"�@�-@z^5@p�9@i�@a�7@YX@M@Ix�@B��@=O�@6v�@0�`@*�!@$�D@ b@��@v�@M�@v�@S�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��A�bA���A���A�ƨAȮAȇ+AȅA�~�A�v�A�p�A�n�A�l�A�jA�hsA�hsA�ffA�hsA�hsA�hsA�hsA�ffA�ffA�hsA�ffA�dZA�ffA�ffA�ffA�dZA�dZA�dZA�dZA�ffA�ffA�dZA�bNA�dZA�dZA�bNA�bNA�`BA�^5A�XA�VA�VA�S�A�A�A�/A�1A�ƨAǍPA�l�A�;dA���AƧ�A� �AŋDA��TA��#A�n�A�O�A���A�ȴA��-A��\A�S�A�{A�I�A��;A���A�%A�JA���A�S�A���A���A���A�S�A��A���A� �A��A�A�/A���A�?}A���A�9XA��`A�dZA���A�ƨA�-A��A�hsA�33A��DA�ȴA�v�A��A���A�E�A�v�A�33A�p�A��A��
A�ƨA��A�FA}Aw�PAv�jAul�Aq7LAo��Am�Al1Ai%Aep�A_�A]x�AZ�yAX�RAW��AWK�AVI�AUXAR��AO�#AM`BAK��AK33AJ��AJ^5AIK�AH9XAF�AE��AD�!AC�AC33ACVAA��A?�^A>��A<~�A:��A81'A7&�A5��A4��A4��A3/A2�\A2 �A0�9A.ĜA-��A,�A+�hA* �A(��A'�A'��A'�A#�#A"�+A"bNA"-A!�A ��A��AȴA��AƨA+A��AA�`AVA33A�9A=qA��A��A�wA\)A��A��AK�A�HA�;AZA�#A�PAĜA�TAS�A
�uA
�A	��Ar�A�A��AZA�A/A��A�A�A{A�#A`BA ff@�-@���@�K�@��@��R@��#@��@��^@���@�@�@�bN@�@�@�@���@�`B@��m@�l�@���@�\@��@��`@�I�@�A�@���@�Z@��
@��H@�^5@�Ĝ@��@�z�@�S�@ՙ�@�t�@���@�`B@Ο�@͑h@���@�r�@�A�@�b@˕�@��y@�E�@���@���@�t�@��@Ɵ�@�M�@ŉ7@ļj@��;@��H@°!@�hs@��@��;@���@��m@��H@��#@�J@�@�/@��`@��`@��/@�j@��@�l�@�K�@��@���@�Ĝ@�E�@�-@�=q@�^5@�n�@�M�@�5?@�~�@��-@��@�O�@��`@�z�@��
@���@�ȴ@�E�@�$�@�-@���@��@��j@�z�@�Z@�ƨ@�K�@�+@�
=@��@��^@��@�O�@��@���@���@�+@��@�v�@��-@�V@�j@��@��@�"�@��@��H@��H@��!@�~�@�M�@�@��#@��-@���@�x�@���@��D@�bN@�bN@� �@���@�C�@�o@�o@�;d@�;d@�+@�
=@��@�^5@���@�X@�`B@�O�@�r�@��@�  @��@�dZ@��@��@�"�@��@�K�@�
=@���@�-@��@��^@�@�$�@�$�@���@�p�@�%@�%@�%@���@���@�1@���@�"�@���@�J@�p�@��@�/@��@�@��@�Ĝ@��j@��u@�bN@�j@��u@��9@�&�@�O�@�X@���@���@�1'@��@��
@�S�@���@���@���@�-@���@���@���@��-@��h@��7@��@�/@���@��9@�I�@��@���@�S�@��@��@���@���@�E�@���@�x�@��7@��@�p�@�`B@��/@���@�I�@��@���@��F@�S�@�ȴ@�~�@�=q@�5?@��@��#@��^@���@��^@��-@��@�hs@�`B@�O�@�?}@��@���@��j@�I�@�9X@�(�@��w@�|�@�K�@�33@��@�@���@�$�@��T@���@�`BG�O�@�"�@�-@z^5@p�9@i�@a�7@YX@M@Ix�@B��@=O�@6v�@0�`@*�!@$�D@ b@��@v�@M�@v�@S�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBC�BB�BB�BB�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BB�BA�BA�BB�BA�BA�BB�BC�BC�BC�BC�BI�BT�B~�B�sB  BBBDB�B�B�B�B{BDB
=B+BB��B��B��B��B1B+B�B�;B�BǮB�XB�?B�'B��B��B�{B�7B~�Bt�BhsBaHBR�BI�B6FB�B��B�BŢB�'B��B��B�PB~�Bw�Bn�BR�BH�B49B\B
�B
�5B
ƨB
�LB
�B
��B
�B
dZB
M�B
.B
%�B
�B
  B	��B	�B	�;B	ɺB	�-B	��B	�7B	|�B	u�B	p�B	o�B	m�B	gmB	W
B	D�B	<jB	:^B	6FB	2-B	0!B	)�B	$�B	 �B	 �B	�B	�B	�B	�B	hB	JB	1B	B��B��B�B�B�sB�fB�TB�BB�5B�B��B��B��BȴBĜB��B�wB�jB�RB�'B�B�B�B��B��B��B��B��B��B�{B�oB�\B�JB�+B�B�B�B� B~�B}�B|�Bz�By�Bw�Bu�Bs�Bq�Bp�Bn�Bm�Bk�BiyBhsBgmBe`BcTBbNBaHB`BB_;B^5B^5B]/B\)B[#B[#BZBYBW
B[#B\)B\)B[#B[#B^5B_;BaHBcTBgmBgmB\)BR�BQ�BQ�BVBcTBl�Bq�Bu�B{�B�1B��B��B��B�B�B��B�B�B��B��B��B��B�B��B��B�B�B�B�!B�!B�!B�-B�9B�9B�XB�dB�wB��BBÖBŢBĜBƨBǮBƨB��B��B��B��B��B�B�
B�mB�sB�yB�B�B�B��B��B	B	B	B	B	B	B	B	%B	1B	PB	\B	oB	�B	�B	#�B	%�B	&�B	&�B	(�B	+B	/B	1'B	33B	49B	5?B	5?B	5?B	6FB	9XB	>wB	C�B	D�B	D�B	D�B	M�B	N�B	O�B	O�B	O�B	Q�B	Q�B	Q�B	Q�B	S�B	XB	]/B	aHB	e`B	jB	jB	jB	jB	k�B	k�B	k�B	l�B	n�B	p�B	q�B	r�B	u�B	w�B	y�B	z�B	{�B	|�B	~�B	�B	�B	�1B	�7B	�7B	�=B	�=B	�DB	�JB	�VB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�?B	�?B	�FB	�^B	�wB	�}B	��B	��B	��B	B	ÖB	ŢB	ƨB	ŢB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�/B	�5B	�5B	�5B	�5B	�/B	�/B	�;B	�HB	�HB	�NB	�ZB	�`B	�fB	�mB	�mB	�mB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
+B
+B
+B
+B
1B
	7B
	7B
	7B
	7B
�B
PB
{B
�B
!�B
(�B
1'B
7LB
?}B
B�B
H�B
L�B
S�B
YB
^5B
cTB
gmB
m�B
o�B
s�B
v�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BC�BB�BB�BB�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BB�BA�BA�BB�BA�BA�BB�BC�BC�BC�BC�BI�BT�B~�B�vB��BB%BHB�B�B�B�B�BHB
=B,BB��B��B��B��B:B.B�B�>B�BǯB�\B�=B�)B��B��B�}B�6B~�Bt�BhvBaGBR�BI�B6JB�B��B�BŦB�&B��B��B�QB~�Bw�Bn�BR�BH�B4:B]B
�B
�;B
ƫB
�OB
�B
��B
�
B
dcB
M�B
.B
%�B
�B
 B	��B	�B	�DB	��B	�7B	��B	�DB	|�B	u�B	p�B	o�B	m�B	g{B	WB	D�B	<zB	:mB	6VB	2<B	03B	*B	$�B	 �B	 �B	�B	�B	�B	�B	zB	[B	BB	%B�B��B�B�B�B�yB�iB�WB�GB�*B�B��B��B��BİB��B��B�~B�fB�;B�#B�B�B�B��B��B��B��B��B��B��B�sB�aB�DB�)B�%B�B�BB~	B}Bz�By�Bw�Bu�Bs�Bq�Bp�Bn�Bm�Bk�Bi�Bh�Bg�BeyBckBbfBabB`\B_UB^NB^OB]FB\CB[>B[=BZ5BY/BW B[<B\CB\DB[<B[<B^LB_TBabBcmBg�Bg�B\BBS
BRBRBVBckBl�Bq�Bu�B{�B�HB��B��B� B�B�B�	B�B�B�B�B� B�B�B�B��B�B�*B�.B�5B�9B�4B�@B�KB�MB�iB�wB��B��B¡BèBŴBİBƻB��BƸB��B��B��B�B�B�B�B�~B�B�B�B�B��B��B�
B	B	B	"B	+B	!B	"B	0B	7B	AB	_B	nB	B	�B	�B	#�B	%�B	&�B	&�B	)B	+B	/(B	14B	3?B	4GB	5JB	5NB	5MB	6SB	9eB	>�B	C�B	D�B	D�B	D�B	M�B	N�B	O�B	O�B	O�B	Q�B	Q�B	Q�B	Q�B	T	B	XB	]<B	aVB	emB	j�B	j�B	j�B	j�B	k�B	k�B	k�B	l�B	n�B	p�B	q�B	r�B	u�B	w�B	y�B	z�B	{�B	|�B	B	�B	�$B	�>B	�CB	�BB	�JB	�JB	�PB	�VB	�aB	�qB	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�$B	�9B	�GB	�HB	�NB	�gB	��B	��B	��B	��B	��B	B	àB	ŪB	ƳB	ŬB	ŪB	ƴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�&B	�+B	�8B	�>B	�?B	�@B	�?B	�7B	�9B	�DB	�QB	�RB	�VB	�cB	�hB	�nB	�vB	�xB	�xB	�uB	�{B	�{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B
 B
 B
B
B
B
B
!B
'B
4B
5B
4B
5B
<B
	>B
	=B
	@B
	?G�O�B
YB
�B
�B
!�B
)B
1.B
7TB
?�B
B�B
H�B
L�B
S�B
YB
^<B
cZB
gtB
m�B
o�B
s�B
v�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.01 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214252016053112142520160531121425  AO  ARCAADJP                                                                    20140721230508    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230508  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230508  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121425  IP                  G�O�G�O�G�O�                