CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-04-03T17:02:58Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20180403170258  20190604094145  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�XV��ư1   @�XW�d�@5��C���d�n��O�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DDfDD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dyk�D��D�4{D��HD��{D���D�;3D�nD��
D�qD�J�D���D���D���D�2�D�u�D���D�fD�F�D��D�k3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @8��@\)@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�.B�.B�ǮB���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qCC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DD�DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dyj�D�fD�4)D���D��)D��qD�:�D�m�D�θD�D�J=D���D�ȤD��\D�2=D�uqD���D�D�FfD�\D�j�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�ȴA�ƨAǾwAǅA�bAƸRAƥ�AƍPAƃA�~�A�z�A�r�A�bNA�I�A��A�~�A��A���AļjA�ffA�
=A�t�A�JA7A�C�A��A��;A��jA�l�A�oA�C�A���A��A���A�\)A� �A�33A��^A���A�n�A�C�A�+A�$�A��A���A���A��7A�x�A�I�A�oA���A��\A�bNA�{A��A���A���A�?}A��A�  A���A�7LA�JA��A��-A��A�\)A� �A��wA���A�1A��PA�r�A��A��+A�A�=qA��A��A�?}A�  A�jA��A��9A��yA�O�A�;dA���A�7LA��DA���A�5?A��A��yA�;dA�bNA�;dA��9A�33A��;A��/A�VA��RA�wA{��AzbNAx�Av�`Av�At�AqG�ApZAoS�AlȴAj�!Ai`BAhn�Ag��Af��Ab��A`�A`A]?}AZ5?AX�AV��AUp�AT��AT{AS�AS"�ARffAQG�AP5?AOp�AN�AM�mAM+AKS�AJz�AIXAH9XAF��AE�wAEK�ADZACdZAA`BA@M�A?�A>��A>A�A=��A=S�A<��A;`BA9�A8�DA7�TA6��A5oA3��A0�DA.��A,��A,9XA+|�A*�A)�hA(r�A'O�A%�A$�A#��A"��A"n�A"  A ��A JAG�A�AbA&�A�\A�AM�AC�A �AVA�#AZA��A�wA|�A`BAȴAE�A��A��A��Az�A|�A
�9A��A��A&�A|�A��A��A ~�@�V@�bN@���@��!@��#@�O�@��j@�@��
@�!@�(�@�M�@�`B@�&�@���@��@�j@��@�n�@�n�@�/@�9X@���@�\)@�5?@��@�@�^5@��T@��@ؓu@� �@ם�@�~�@��T@�O�@ԋD@�  @�C�@ѡ�@�%@Л�@���@Ϯ@�o@�v�@Ͳ-@�  @�;d@ʰ!@���@�Q�@Ǿw@�;d@��y@���@�$�@Ĵ9@���@�@+@�ff@�&�@���@�ƨ@���@���@��@�"�@��@��@�-@�p�@��@�  @�+@��@��7@��@��m@��P@�@���@���@�7L@�%@�(�@�t�@�;d@���@��T@��7@�%@��j@���@��@�Z@��@�t�@�K�@�S�@�S�@�n�@��@�?}@��D@�Q�@���@���@�dZ@�33@�ȴ@�~�@���@�@�E�@��T@��h@�p�@�%@��@�z�@�I�@��;@�dZ@�S�@�+@�@���@���@�ff@�5?@��#@��9@�b@���@���@��F@���@���@���@���@���@�dZ@�S�@���@�^5@�$�@��T@���@�p�@�?}@�7L@��@���@�Ĝ@���@��u@�9X@�1'@��@��@��m@�dZ@�o@�33@�33@��y@���@�ff@�-@���@��7@�hs@�/@��`@��/@�Ĝ@�r�@�Q�@�I�@�1'@�  @��@�|�@�o@���@�E�@�{@��@���@���@�?}@���@���@�bN@�9X@�b@���@��m@���@���@�|�@�l�@�K�@�33@��@��y@���@���@�v�@�V@�V@�V@�=q@�J@��@�@��h@�`B@��@��/@��j@���@�Q�@���@��@��;@�K�@�o@�@��@�ȴ@���@��+@�E�@���@���@�@��-@��h@��@�`B@���@��@���@��@�Z@�9X@� �@���@���@��P@�|�@�dZ@�S�@�o@��@���@�M�@�5?@��@���@�hs@�G�@�7L@�%@��@��D@�j@�I�@�b@��@��P@�"�@��@�n�@�{@�F@z:*@r��@i%@cX�@]�@T�_@NC�@H7�@@�O@:�6@3�&@.c @)�9@$Ft@ $@@��@�@5?@
�B111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�A�ȴA�ƨAǾwAǅA�bAƸRAƥ�AƍPAƃA�~�A�z�A�r�A�bNA�I�A��A�~�A��A���AļjA�ffA�
=A�t�A�JA7A�C�A��A��;A��jA�l�A�oA�C�A���A��A���A�\)A� �A�33A��^A���A�n�A�C�A�+A�$�A��A���A���A��7A�x�A�I�A�oA���A��\A�bNA�{A��A���A���A�?}A��A�  A���A�7LA�JA��A��-A��A�\)A� �A��wA���A�1A��PA�r�A��A��+A�A�=qA��A��A�?}A�  A�jA��A��9A��yA�O�A�;dA���A�7LA��DA���A�5?A��A��yA�;dA�bNA�;dA��9A�33A��;A��/A�VA��RA�wA{��AzbNAx�Av�`Av�At�AqG�ApZAoS�AlȴAj�!Ai`BAhn�Ag��Af��Ab��A`�A`A]?}AZ5?AX�AV��AUp�AT��AT{AS�AS"�ARffAQG�AP5?AOp�AN�AM�mAM+AKS�AJz�AIXAH9XAF��AE�wAEK�ADZACdZAA`BA@M�A?�A>��A>A�A=��A=S�A<��A;`BA9�A8�DA7�TA6��A5oA3��A0�DA.��A,��A,9XA+|�A*�A)�hA(r�A'O�A%�A$�A#��A"��A"n�A"  A ��A JAG�A�AbA&�A�\A�AM�AC�A �AVA�#AZA��A�wA|�A`BAȴAE�A��A��A��Az�A|�A
�9A��A��A&�A|�A��A��A ~�@�V@�bN@���@��!@��#@�O�@��j@�@��
@�!@�(�@�M�@�`B@�&�@���@��@�j@��@�n�@�n�@�/@�9X@���@�\)@�5?@��@�@�^5@��T@��@ؓu@� �@ם�@�~�@��T@�O�@ԋD@�  @�C�@ѡ�@�%@Л�@���@Ϯ@�o@�v�@Ͳ-@�  @�;d@ʰ!@���@�Q�@Ǿw@�;d@��y@���@�$�@Ĵ9@���@�@+@�ff@�&�@���@�ƨ@���@���@��@�"�@��@��@�-@�p�@��@�  @�+@��@��7@��@��m@��P@�@���@���@�7L@�%@�(�@�t�@�;d@���@��T@��7@�%@��j@���@��@�Z@��@�t�@�K�@�S�@�S�@�n�@��@�?}@��D@�Q�@���@���@�dZ@�33@�ȴ@�~�@���@�@�E�@��T@��h@�p�@�%@��@�z�@�I�@��;@�dZ@�S�@�+@�@���@���@�ff@�5?@��#@��9@�b@���@���@��F@���@���@���@���@���@�dZ@�S�@���@�^5@�$�@��T@���@�p�@�?}@�7L@��@���@�Ĝ@���@��u@�9X@�1'@��@��@��m@�dZ@�o@�33@�33@��y@���@�ff@�-@���@��7@�hs@�/@��`@��/@�Ĝ@�r�@�Q�@�I�@�1'@�  @��@�|�@�o@���@�E�@�{@��@���@���@�?}@���@���@�bN@�9X@�b@���@��m@���@���@�|�@�l�@�K�@�33@��@��y@���@���@�v�@�V@�V@�V@�=q@�J@��@�@��h@�`B@��@��/@��j@���@�Q�@���@��@��;@�K�@�o@�@��@�ȴ@���@��+@�E�@���@���@�@��-@��h@��@�`B@���@��@���@��@�Z@�9X@� �@���@���@��P@�|�@�dZ@�S�@�o@��@���@�M�@�5?@��@���@�hs@�G�@�7L@�%@��@��D@�j@�I�@�b@��@��P@�"�@��@�n�G�O�@�F@z:*@r��@i%@cX�@]�@T�_@NC�@H7�@@�O@:�6@3�&@.c @)�9@$Ft@ $@@��@�@5?@
�B111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
��B
��B
��B
��B
��B
��B
��B  BB%B	7BJBbB�B#�B,B33B5?BF�BYBo�Bz�B�+B�DB�bB�uB��B��B��B�dB��B�mB��BB+B�B�B�B �B"�B$�B$�B)�B/B6FBD�BG�BS�B`BBgmBt�B}�B�1B�VB�VB�JB�%B~�Bu�Bo�Bo�Bn�Bm�Bm�Bm�Bl�Bk�BiyBcTBYBK�B?}B'�BoBB��B�;BɺBB��B�wB�FB��B�DBt�BffB]/BJ�B5?B(�B �BVB
�B
�#B
��B
��B
B
�B
�hB
�B
m�B
bNB
L�B
6FB
,B
 �B
{B
JB	��B	�B	�`B	�/B	��B	�}B	�FB	�!B	��B	��B	�7B	|�B	u�B	e`B	T�B	L�B	B�B	;dB	8RB	49B	2-B	/B	+B	%�B	 �B	�B	�B	{B	bB	
=B	B	B��B��B�B�B�B�fB�BB�)B�B�B��B��B��B��BǮBĜBB�}B�jB�XB�9B�3B�!B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�bB�\B�PB�JB�=B�1B�%B�B�B~�B|�Bz�Bw�Bt�Br�Br�Bq�Bp�Bp�Bn�Bm�Bl�Bk�BjBiyBffBe`Be`Be`BdZBcTBbNBaHB`BB_;B_;B_;B_;B^5B^5B]/B_;B`BBbNBbNBcTBcTBdZBcTBcTBcTBaHBaHBcTBdZBe`Be`BgmBiyBn�Bo�Bq�Bs�Bu�Bv�Bw�By�Bz�Bz�B{�B{�B|�B� B�B�B�B�B�%B�7B�JB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�?B�XB�XB��B��B��B��BBÖBĜBȴB��B��B��B�B�B�/B�;B�TB�`B�fB�yB�B�B�B��B��B��B	B	B	B	B		7B	PB	\B	oB	{B	oB	hB	�B	�B	�B	�B	 �B	"�B	$�B	)�B	,B	49B	<jB	@�B	D�B	F�B	G�B	J�B	L�B	M�B	M�B	P�B	T�B	W
B	YB	ZB	[#B	]/B	_;B	aHB	cTB	cTB	ffB	hsB	jB	m�B	n�B	o�B	p�B	r�B	s�B	s�B	s�B	s�B	u�B	x�B	y�B	z�B	{�B	|�B	|�B	}�B	� B	�+B	�7B	�=B	�DB	�PB	�\B	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�9B	�FB	�RB	�^B	�^B	�dB	�jB	�}B	��B	B	ŢB	ƨB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�#B	�)B	�)B	�5B	�BB	�BB	�BB	�TB	�ZB	�ZB	�`B	�`B	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
+B

�B
�B
$�B
,�B
5?B
:�B
=VB
CGB
K�B
Q4B
WYB
[qB
`�B
e`B
j�B
o�B
r�B
xB
|�B
�iB
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�+B
�.B
�8B
�:B
�SB
�gB
�xB
�rB
�pB
�xB
�B
��B
��B
��B �B�BPB{B#�B%�B7BI�B`BkQBw�B{�B��B��B��B�!B�QB��B�;B��B�NB�B��B�BBB!B/B;B8BYBvB&�B4�B8BDMBP�BW�BeBnDBx�B~�B~�B|�BvuBoMBfB_�B_�B^�B]�B]�B]�B\�B[�BY�BS�BItB<!B/�BPB�B�|B�5BϪB�,B� B��B��B��B�PB{�Be6BV�BM�B;CB%�B|BLB
��B
�9B
˳B
�hB
�YB
�$B
��B
�B
r�B
^.B
R�B
=pB
&�B
�B
oB
'B	��B	�B	�DB	�B	��B	��B	�6B	��B	��B	��B	��B	y�B	m�B	f�B	V"B	E�B	=�B	3ZB	,.B	)B	%
B	"�B	�B	�B	�B	�B	�B		dB	LB	5B�B��B��B�B�B�B�tB�\B�?B�B�B��B��B��B��B��B��B��B�{B�jB�[B�IB�8B�B�B�B��B��B��B��B��B��B��B��B�uB�jB�bB�]B�[B�MB�EB~;B}5B{)ByBwBuBr�Bo�Bm�Bk�Bh�Be�Bc�Bc�Bb�Ba�Ba�B_�B^}B]zB\vB[qBZfBWWBVSBVSBVPBUOBTJBSCBR;BQ5BP,BP2BP/BP/BO,BO/BN'BP3BQ9BSEBSHBTJBTLBUQBTMBTJBTMBR=BR@BTMBUPBVSBVVBXcBZnB_�B`�Bb�Bd�Bf�Bg�Bh�Bj�Bk�Bk�Bl�Bl�Bm�Bp�Bq�Bs Bt	BuBwBz,B}?B�sB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�0B�GB�KB�sB�tB�zB�vB��B��B��B��B��B��B��B�B�B�B�$B�?B�QB�QB�cB�rB�sB�B�B�B��B��B��B��B�B�!B�8B	 DB	XB	fB	WB	QB	fB	
�B	�B	�B	�B	�B	�B	�B	�B	%B	-OB	1gB	5}B	7�B	8�B	;�B	=�B	>�B	>�B	A�B	E�B	G�B	I�B	J�B	LB	NB	PB	R)B	T5B	T2B	WHB	YXB	[^B	^oB	_uB	`zB	a�B	c�B	d�B	d�B	d�B	d�B	f�B	i�B	j�B	k�B	l�B	m�B	m�B	n�B	p�B	xB	zB	{B	|"B	~*B	�:B	�EB	�QB	�YB	�_B	�jB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�,B	�5B	�8B	�;B	�?B	�VB	�`B	�eB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�/B	�-B	�1B	�/B	�8B	�<B	�>B	�MB	�SB	�QB	�SB	�ZB	�VB	�aB	�cB	�mB	�nB	�wB	�qB	�~B	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B
�B
�B
�B
&B
+`B
.$B
4B
<bB
BB
H)B
L<B
Q�B
V.B
[�B
`OB
c�B
h�B
mkB
q4B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.01 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.015(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040941452019060409414520190604094145  AO  ARCAADJP                                                                    20180403170258    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180403170258  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180403170258  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094145  IP                  G�O�G�O�G�O�                