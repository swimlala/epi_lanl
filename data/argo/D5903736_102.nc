CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-01-31T03:15:23Z AOML 3.0 creation; 2016-05-31T19:14:41Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150131031523  20160531121441  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               fA   AO  4051_7090_102                   2C  D   APEX                            5368                            041511                          846 @�6����1   @�6�.u�@4����F�ddA�7K�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    fA   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"fD"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:fD:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy�3D�3D�S3D���D�ٚD�  D�FfD���D��fD� D�9�D���D���D�3D�6fD�vfD�ɚD�	�D�C3D�fD��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @\)@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCnCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D"�D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D:�D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ��DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt��Dy��D��D�R�D��HD��HD���D�FD��{D��D��D�9HD��{D��{D��D�6D�vD��HD�	HD�B�D�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�VA�%A�JA�oA�{A��A��A��A��A��A��A��A��A� �A��A��A��A��A� �A�"�A�"�A��A��A��A��A��A�VAÓuA��AuA�?}A�A��TA��-A�ffA�A��TA��\A�jA�bNA�M�A�"�A��A�bA��A���A��^A�t�A�C�A�hsA�ZA��PA�ZA�v�A��A���A�XA��yA��hA�|�A�{A��A��^A���A�VA���A�C�A�A�A�ffA��A�hsA���A�XA���A�VA�`BA���A��HA�ĜA�dZA�;dA���A�$�A�K�A�`BA�z�A�jA��hA�O�A�A�33A��9A���A��A�M�A��A��/A��DA��A�?}A���A�  A�x�A��DA��yA�jA���A�I�A�|�A���A�;dA�/A���A��yA��wA�t�A��A�p�A;dA{��Ay;dAw%Ar�/AqS�Ap��Ao�wAk�TAj�Af(�Ac��Aa��A`^5A_�7A^ȴA^ZA[t�AY/AV�`AUdZAT-AT1AS�
AR��AQ��AP��AP�AM�TAK�AJ~�AJI�AI;dAGVAD��AB�jAA��AAG�A@�DA?7LA=\)A:��A8�A6��A6$�A3ƨA1A/x�A.��A,��A,bA*�yA*$�A)��A'�FA&I�A%/A$�9A$~�A#/A!�TA!t�A!33A!"�A �HAS�AffAbA/A%Av�A=qA��AS�A�HA��A�Al�AC�A�TAAx�A~�A{A�FAn�A�A9XA�A��A
=qA	hsA��A�#A7LA�+A��A�9AXAVA��A �@��;@��7@��j@�V@�p�@�&�@���@���@�33@�E�@���@���@��@�\@�h@��`@�  @�\@�@�j@�dZ@�R@��T@�O�@蛦@�@�7@�A�@�h@��m@�J@�1'@ە�@�n�@׶F@�K�@և+@��@�t�@�n�@�?}@�1'@�\)@�ȴ@Ο�@�ff@��@�X@��@̛�@�  @˶F@�t�@��@��@ǥ�@�^5@�O�@�z�@�K�@�;d@�K�@+@��@��h@�7L@���@� �@�b@�o@��h@��@��@�9X@���@�+@�
=@��@�V@���@�@��7@�/@��@��j@��D@�I�@�K�@���@���@�v�@�v�@�n�@�5?@��@���@���@�X@�%@��@�(�@�;d@��@���@�M�@���@�O�@�/@��@�bN@�dZ@�S�@�+@���@�M�@��@��h@���@���@�Q�@�  @��F@�dZ@�"�@���@�V@��#@��`@���@�z�@�9X@�ƨ@��F@���@�|�@�S�@�o@��R@���@�~�@�=q@���@��-@�/@��j@���@���@��@�r�@�bN@�Q�@�1'@��m@���@��P@�\)@���@��+@�ff@�5?@�J@�@�@���@���@���@��h@�O�@��`@��u@�z�@�Q�@��@�  @��;@�ƨ@��@�\)@�33@��@���@�E�@�J@��#@���@��7@�7L@��@���@��D@�A�@�ƨ@�C�@�o@���@��!@��+@�J@��#@��^@�`B@�?}@��@�V@��@�V@��`@�z�@���@�l�@�S�@�o@��@��\@�E�@���@���@��h@��@��@��@�x�@�hs@�7L@�j@�Q�@�I�@�I�@�9X@��;@�S�@�+@�@�ȴ@��\@�ff@�V@�-@��#@�?}@��@���@��@�Z@�9X@�b@��m@�ƨ@��P@�+@�
=@��y@��+@�^5@�@�@�x�@�`B@�7L@���@��@��u@�j@�9X@���@�C�@�@�n�@�V@��/@�|�@~ȴ@uV@kt�@d1@]V@U�@M��@F��@?�@7�w@1%@*��@%�-@�P@^5@��@M�@l�@�
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�VA�%A�JA�oA�{A��A��A��A��A��A��A��A��A� �A��A��A��A��A� �A�"�A�"�A��A��A��A��A��A�VAÓuA��AuA�?}A�A��TA��-A�ffA�A��TA��\A�jA�bNA�M�A�"�A��A�bA��A���A��^A�t�A�C�A�hsA�ZA��PA�ZA�v�A��A���A�XA��yA��hA�|�A�{A��A��^A���A�VA���A�C�A�A�A�ffA��A�hsA���A�XA���A�VA�`BA���A��HA�ĜA�dZA�;dA���A�$�A�K�A�`BA�z�A�jA��hA�O�A�A�33A��9A���A��A�M�A��A��/A��DA��A�?}A���A�  A�x�A��DA��yA�jA���A�I�A�|�A���A�;dA�/A���A��yA��wA�t�A��A�p�A;dA{��Ay;dAw%Ar�/AqS�Ap��Ao�wAk�TAj�Af(�Ac��Aa��A`^5A_�7A^ȴA^ZA[t�AY/AV�`AUdZAT-AT1AS�
AR��AQ��AP��AP�AM�TAK�AJ~�AJI�AI;dAGVAD��AB�jAA��AAG�A@�DA?7LA=\)A:��A8�A6��A6$�A3ƨA1A/x�A.��A,��A,bA*�yA*$�A)��A'�FA&I�A%/A$�9A$~�A#/A!�TA!t�A!33A!"�A �HAS�AffAbA/A%Av�A=qA��AS�A�HA��A�Al�AC�A�TAAx�A~�A{A�FAn�A�A9XA�A��A
=qA	hsA��A�#A7LA�+A��A�9AXAVA��A �@��;@��7@��j@�V@�p�@�&�@���@���@�33@�E�@���@���@��@�\@�h@��`@�  @�\@�@�j@�dZ@�R@��T@�O�@蛦@�@�7@�A�@�h@��m@�J@�1'@ە�@�n�@׶F@�K�@և+@��@�t�@�n�@�?}@�1'@�\)@�ȴ@Ο�@�ff@��@�X@��@̛�@�  @˶F@�t�@��@��@ǥ�@�^5@�O�@�z�@�K�@�;d@�K�@+@��@��h@�7L@���@� �@�b@�o@��h@��@��@�9X@���@�+@�
=@��@�V@���@�@��7@�/@��@��j@��D@�I�@�K�@���@���@�v�@�v�@�n�@�5?@��@���@���@�X@�%@��@�(�@�;d@��@���@�M�@���@�O�@�/@��@�bN@�dZ@�S�@�+@���@�M�@��@��h@���@���@�Q�@�  @��F@�dZ@�"�@���@�V@��#@��`@���@�z�@�9X@�ƨ@��F@���@�|�@�S�@�o@��R@���@�~�@�=q@���@��-@�/@��j@���@���@��@�r�@�bN@�Q�@�1'@��m@���@��P@�\)@���@��+@�ff@�5?@�J@�@�@���@���@���@��h@�O�@��`@��u@�z�@�Q�@��@�  @��;@�ƨ@��@�\)@�33@��@���@�E�@�J@��#@���@��7@�7L@��@���@��D@�A�@�ƨ@�C�@�o@���@��!@��+@�J@��#@��^@�`B@�?}@��@�V@��@�V@��`@�z�@���@�l�@�S�@�o@��@��\@�E�@���@���@��h@��@��@��@�x�@�hs@�7L@�j@�Q�@�I�@�I�@�9X@��;@�S�@�+@�@�ȴ@��\@�ff@�V@�-@��#@�?}@��@���@��@�Z@�9X@�b@��m@�ƨ@��P@�+@�
=@��y@��+@�^5@�@�@�x�@�`B@�7L@���@��@��u@�j@�9X@���@�C�@�@�n�@�V@��/@�|�@~ȴ@uV@kt�@d1@]V@U�@M��@F��@?�@7�w@1%@*��@%�-@�P@^5@��@M�@l�@�
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB>wB?}B>wB>wB=qB>wB>wB>wB>wB?}B?}B?}B?}B>wB=qB=qB=qB=qB=qB=qB=qB<jB<jB<jB<jB<jB:^B2-B2-B7LB=qBC�BK�BM�BM�BN�BN�BQ�BQ�BQ�BP�BN�BM�BM�BL�BO�BXBXBZBZBW
BR�BK�BI�BI�BI�BH�BG�BE�BD�BA�B@�B>wB<jB:^B33B.B%�B �B �B�B�B,B%�B�BoBJB��B�`B��B��B�=B~�BjB1'B��B�B��B�B��B��B�BjBN�B:^BH�BT�BR�Bt�B�\B�%Bw�BhsB\)BL�B2-B�BDB
�fB
��B
�LB
�?B
�B
��B
�bB
�=B
�B
z�B
m�B
VB
C�B
2-B
�B
PB
	7B	��B	�fB	�
B	�qB	�B	��B	��B	�uB	�VB	�1B	w�B	jB	_;B	W
B	P�B	O�B	M�B	H�B	B�B	=qB	8RB	.B	!�B	�B	�B	�B	PB	B��B��B��B�B�B�fB�#B��B��BȴB��B�dB�?B�'B��B��B��B��B��B��B��B�{B�{B�oB�hB�\B�VB�VB�PB�DB�=B�1B�%B�B�B�B�B�B�B�B� B~�B{�B{�Bz�By�Bx�Bx�Bw�Bu�Bs�Bq�Bp�Bn�Bm�Bl�BjBiyBhsBgmBffBe`BdZBdZBcTBaHBaHBaHBbNBaHBbNBdZBdZBhsBp�Bs�Bu�B{�B�B�B�B�B�%B�%B�1B�=B�DB�JB�PB�VB�VB�VB�\B�oB�{B��B��B��B��B��B��B��B��B��B��B�B�'B�9B�^B�jB�wB�wB�}B��BÖBŢBǮBɺB��B��B��B��B��B��B�
B�
B�B�#B�/B�BB�ZB�mB�sB�B�B�B�B��B��B��B��B��B��B	  B	B	B	B	B	B	%B	+B	1B	
=B	JB	�B	�B	�B	�B	�B	 �B	!�B	"�B	"�B	#�B	$�B	&�B	'�B	+B	.B	/B	2-B	6FB	8RB	:^B	;dB	<jB	?}B	E�B	F�B	G�B	J�B	M�B	P�B	S�B	XB	YB	\)B	^5B	_;B	`BB	bNB	dZB	gmB	gmB	k�B	m�B	m�B	o�B	s�B	t�B	u�B	v�B	x�B	|�B	~�B	~�B	� B	�B	�B	�B	�+B	�JB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�'B	�3B	�9B	�?B	�?B	�LB	�LB	�RB	�XB	�dB	�dB	�jB	�qB	�}B	��B	ĜB	ŢB	ƨB	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�)B	�/B	�/B	�)B	�/B	�;B	�HB	�TB	�TB	�TB	�ZB	�`B	�fB	�fB	�mB	�mB	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
+B
+B
	7B
	7B
JB
�B
 �B
'�B
+B
/B
6FB
;dB
B�B
G�B
L�B
P�B
W
B
]/B
bNB
gmB
l�B
q�B
t�B
w�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B>zB?�B>zB>|B=tB>|B>zB>|B>xB?�B?�B?�B?�B>xB=xB=tB=rB=vB=tB=vB=vB<oB<oB<mB<mB<oB:eB22B2/B7SB=tBC�BK�BM�BM�BN�BN�BQ�BQ�BQ�BP�BN�BM�BM�BL�BO�BXBXBZ"BZ BWBR�BK�BI�BI�BI�BH�BG�BE�BD�BA�B@�B>xB<pB:aB36B.B%�B �B �B�B�B,B%�B�BrBNB��B�eB��B��B�>B~�Bj�B1(B��B�B��B�B��B��B�	Bj�BN�B:`BH�BUBR�Bt�B�ZB�%Bw�BhuB\+BL�B2/B�BEB
�hB
��B
�PB
�AB
�B
��B
�hB
�BB
�$B
z�B
m�B
V	B
C�B
23B
�B
XB
	@B	�B	�qB	�B	�|B	�B	��B	��B	��B	�_B	�?B	w�B	j�B	_HB	WB	P�B	O�B	M�B	H�B	B�B	=~B	8aB	.$B	!�B	�B	�B	�B	bB	$B� B��B��B�B��B�xB�6B�B��B��B��B�zB�RB�>B�B�B��B��B��B��B��B��B��B��B��B�tB�kB�mB�hB�[B�WB�GB�<B�6B�1B�*B�*B�#B�"B�B�BB{�B{�Bz�By�Bx�Bx�Bw�Bu�Bs�Bq�Bp�Bn�Bm�Bl�Bj�Bi�Bh�Bg�Bf~BexBdpBdrBclBa`BaaBa`BbgBaaBbeBdpBdrBh�Bp�Bs�Bu�B{�B�"B�*B�.B�3B�<B�=B�IB�PB�ZB�`B�eB�jB�kB�jB�qB��B��B��B��B��B��B��B��B�B��B�B�B�/B�;B�NB�rB�~B��B��B��B��BéBŴBǿB��B��B��B��B��B�B�B�B�B�*B�5B�AB�TB�jB�B�B�B�B�B�B��B��B��B��B��B�
B	 B	B	,B	/B	0B	.B	6B	;B	CB	
MB	XB	�B	�B	�B	�B	�B	 �B	!�B	"�B	"�B	#�B	$�B	&�B	'�B	+B	.#B	/,B	2:B	6SB	8`B	:oB	;qB	<xB	?�B	E�B	F�B	G�B	J�B	M�B	P�B	TB	XB	Y'B	\8B	^?B	_IB	`OB	bZB	diB	g{B	gyB	k�B	m�B	m�B	o�B	s�B	t�B	u�B	v�B	x�B	|�B	B	B	�
B	�B	�B	�B	�7B	�UB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�	B	�B	�B	�+B	�+B	�1B	�>B	�BB	�IB	�GB	�VB	�WB	�[B	�`B	�qB	�oB	�tB	�yB	��B	��B	ĤB	ŪB	ƲB	ƵB	ƯB	ȾB	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	� B	�'B	�+B	�2B	�7B	�9B	�3B	�7B	�DB	�TB	�]B	�`B	�_B	�bB	�iB	�qB	�qB	�tB	�uB	�uB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B
B
"B
#B
&B
4B
5B
	=B
	>B
QB
�B
 �B
'�B
+B
/#B
6LB
;lB
B�B
G�B
L�B
P�B
WB
]3B
bSB
gqB
l�B
q�B
t�B
w�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.01 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214412016053112144120160531121441  AO  ARCAADJP                                                                    20150131031523    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150131031523  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20150131031523  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121441  IP                  G�O�G�O�G�O�                