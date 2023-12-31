CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:30Z AOML 3.0 creation; 2016-06-01T00:08:13Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230830  20160531170813  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               1A   AO  4055_7112_049                   2C  D   APEX                            5374                            041511                          846 @֨M��1   @֨N[���@:�Q��c��\)1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    1A   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDyl�D��D�FfD��3D���D�	�D�,�D���D�� D�3D�FfD�s3D�ɚD���D�33D�s3D�3D�3D�I�D�i�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @?\)@\)@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B\)B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B�.B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dte�Dyl)D�{D�FD���D��{D�	HD�,{D��{D�ϮD��D�FD�r�D��HD��{D�2�D�r�D��D��D�IHD�iHD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�z�AˁAˁA�~�A�jA��`AʬA��A�ƨA��yA��HA�oA�A�A���A\A�O�A�&�A���A���A�~�A�%A�33A���A�5?A�=qA���A��A�ffA���A���A�`BA���A��A�E�A��-A�7LA�r�A�M�A��wA�VA�
=A�?}A��FA��A��RA�?}A��A��A��;A��FA���A��\A�G�A�-A��HA�v�A���A��^A���A�hsA�VA�(�A���A���A���A�|�A��A���A�oA���A�S�A���A��9A�r�A���A�S�A��yA�O�A���A�ĜA�XA��A��7A�;dA���A��TA�Q�A��A���A�~�A�v�A�t�A��A��uA�1A���A�E�AXA|��Aw��At��Asl�Ar~�Ap��AnbAlbNAj��Af��Ac�FAb�+AaA^5?A[x�AZ=qAX��AXVAWp�AU7LAT=qAS�ARJAQ|�AP�APbNAN�`AM�ALv�AL(�AK��AK�AKAI�AHZAH-AG��AGAE��ADQ�ACXAB^5AAt�A@�\A?�A>��A=��A<��A;XA;
=A:�RA9�wA9C�A8��A7�FA6�DA3�A3C�A2��A1�A1&�A0M�A0JA/S�A.��A.��A.Q�A.�A-�#A-`BA,��A+��A++A)��A(n�A&�uA%�^A%��A%|�A$z�A$^5A#��A"jA!x�A �AA��A1'AƨA�HA�A�jA�^A�\A5?A�PA�yAA�A��AQ�A�A7LA�AoA��A^5A�A1'A?}A�DAZA"�A	ƨA	�hA	l�A	7LA�jA�A=qA;dA��A��AjA��Ax�A�A��A��A1'A?}A j@��@��h@��
@�n�@�X@�Ĝ@��@�ff@��@���@�~�@�bN@�~�@�@�%@��@�;d@�~�@�-@� �@��@���@�b@�b@⟾@�^@��D@ޟ�@ݺ^@܃@�E�@ٲ-@٩�@ٙ�@�V@�9X@�o@Ձ@���@�9X@ӕ�@�J@љ�@��T@щ7@��@���@Χ�@���@�bN@˾w@�t�@�^5@��@��;@�ƨ@�o@�@���@�Z@�=q@��;@��@���@��T@�p�@��@�7L@�A�@��H@�"�@�l�@�dZ@�S�@��y@�V@�J@���@�`B@��/@�A�@��@���@�J@�G�@�t�@��H@��@��@�
=@��@�Q�@�S�@�+@�7L@���@�=q@��-@�M�@��+@�@���@�G�@��
@��
@�ƨ@�\)@�
=@��H@�ȴ@�E�@���@�hs@�p�@�`B@��j@� �@��F@�;d@���@�=q@�7L@���@�r�@� �@��;@�K�@���@�v�@�-@��@��h@�p�@�hs@�hs@�O�@�r�@��@�ƨ@��P@�K�@��@�x�@���@��u@�Q�@�b@��@��@���@�n�@���@��h@�`B@�/@���@��D@� �@��@���@��@�;d@��@��\@�V@��@�O�@�V@���@�Ĝ@��j@��@�bN@�1'@��@�b@�b@�  @�ƨ@��P@�dZ@�;d@�
=@��!@�ff@�-@��@��-@���@�?}@���@���@��/@���@�Z@�  @�|�@�;d@�+@��!@��\@�ff@���@��#@���@���@��7@�?}@��`@�Ĝ@���@�A�@�  @��F@�t�@�K�@��@���@���@�M�@�5?@�J@���@�hs@�&�@���@���@��@�z�@�9X@�1@��@+@~v�@~{@}�-@}`B@|��@|�/@|z�@|I�@|I�@|I�@{�F@{dZ@{C�@z�H@z~�@zM�@zJ@y�^@yx�@yX@y&�@xr�@x1'@xb@w�@w��@w�P@t�/@l�j@e`B@^v�@Vv�@Nff@F�R@@�@<��@7�@2��@,9X@&v�@!�@��@|�@�@��@�@+@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�z�AˁAˁA�~�A�jA��`AʬA��A�ƨA��yA��HA�oA�A�A���A\A�O�A�&�A���A���A�~�A�%A�33A���A�5?A�=qA���A��A�ffA���A���A�`BA���A��A�E�A��-A�7LA�r�A�M�A��wA�VA�
=A�?}A��FA��A��RA�?}A��A��A��;A��FA���A��\A�G�A�-A��HA�v�A���A��^A���A�hsA�VA�(�A���A���A���A�|�A��A���A�oA���A�S�A���A��9A�r�A���A�S�A��yA�O�A���A�ĜA�XA��A��7A�;dA���A��TA�Q�A��A���A�~�A�v�A�t�A��A��uA�1A���A�E�AXA|��Aw��At��Asl�Ar~�Ap��AnbAlbNAj��Af��Ac�FAb�+AaA^5?A[x�AZ=qAX��AXVAWp�AU7LAT=qAS�ARJAQ|�AP�APbNAN�`AM�ALv�AL(�AK��AK�AKAI�AHZAH-AG��AGAE��ADQ�ACXAB^5AAt�A@�\A?�A>��A=��A<��A;XA;
=A:�RA9�wA9C�A8��A7�FA6�DA3�A3C�A2��A1�A1&�A0M�A0JA/S�A.��A.��A.Q�A.�A-�#A-`BA,��A+��A++A)��A(n�A&�uA%�^A%��A%|�A$z�A$^5A#��A"jA!x�A �AA��A1'AƨA�HA�A�jA�^A�\A5?A�PA�yAA�A��AQ�A�A7LA�AoA��A^5A�A1'A?}A�DAZA"�A	ƨA	�hA	l�A	7LA�jA�A=qA;dA��A��AjA��Ax�A�A��A��A1'A?}A j@��@��h@��
@�n�@�X@�Ĝ@��@�ff@��@���@�~�@�bN@�~�@�@�%@��@�;d@�~�@�-@� �@��@���@�b@�b@⟾@�^@��D@ޟ�@ݺ^@܃@�E�@ٲ-@٩�@ٙ�@�V@�9X@�o@Ձ@���@�9X@ӕ�@�J@љ�@��T@щ7@��@���@Χ�@���@�bN@˾w@�t�@�^5@��@��;@�ƨ@�o@�@���@�Z@�=q@��;@��@���@��T@�p�@��@�7L@�A�@��H@�"�@�l�@�dZ@�S�@��y@�V@�J@���@�`B@��/@�A�@��@���@�J@�G�@�t�@��H@��@��@�
=@��@�Q�@�S�@�+@�7L@���@�=q@��-@�M�@��+@�@���@�G�@��
@��
@�ƨ@�\)@�
=@��H@�ȴ@�E�@���@�hs@�p�@�`B@��j@� �@��F@�;d@���@�=q@�7L@���@�r�@� �@��;@�K�@���@�v�@�-@��@��h@�p�@�hs@�hs@�O�@�r�@��@�ƨ@��P@�K�@��@�x�@���@��u@�Q�@�b@��@��@���@�n�@���@��h@�`B@�/@���@��D@� �@��@���@��@�;d@��@��\@�V@��@�O�@�V@���@�Ĝ@��j@��@�bN@�1'@��@�b@�b@�  @�ƨ@��P@�dZ@�;d@�
=@��!@�ff@�-@��@��-@���@�?}@���@���@��/@���@�Z@�  @�|�@�;d@�+@��!@��\@�ff@���@��#@���@���@��7@�?}@��`@�Ĝ@���@�A�@�  @��F@�t�@�K�@��@���@���@�M�@�5?@�J@���@�hs@�&�@���@���@��@�z�@�9X@�1@��@+@~v�@~{@}�-@}`B@|��@|�/@|z�@|I�@|I�@|I�@{�F@{dZ@{C�@z�H@z~�@zM�@zJ@y�^@yx�@yX@y&�@xr�@x1'@xb@w�@w��@w�P@t�/@l�j@e`B@^v�@Vv�@Nff@F�R@@�@<��@7�@2��@,9X@&v�@!�@��@|�@�@��@�@+@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB/B/B/B.B.B0!B0!B0!B+B�B�BuBbBB%BVBbBbB\BPBB�B�TB��B�XB�B��B�uB�\B�7Bl�BaHB\)B[#BXBS�BP�BF�BQ�BK�B9XB-B#�BoB
=BBB��B��B��BB1B%BB��B��B\BbBVB
=BB�mB�5B�B�)B�B��BƨB�qB�?B�B��B��B��B�bB�Bu�BffB[#B?}B�B��B�
B��B�B\)B7LB�B
��B
�TB
��B
�wB
��B
�hB
s�B
ZB
>wB
 �B
+B	�ZB	��B	ȴB	�}B	�!B	��B	�uB	�1B	ffB	N�B	E�B	<jB	-B	�B	�B	hB	PB	1B	  B��B��B��B��B��B��B��B��B��B��B��B��B	  B��B��B��B��B��B�B�B�mB�ZB�NB�5B�B��B��B��B��BɺBȴBǮBƨBƨBĜB�wB�RB�FB�9B�-B�!B�B�B�B�B�B��B��B��B��B��B��B��B��B�VB�%B�B�B�%B�+B�%B�Bv�Br�Bo�Bl�Bk�Bk�BjBgmBdZB_;B\)BYBXBT�BR�BP�BM�BL�BK�BH�BF�BE�BD�BB�B>wB;dB9XB7LB5?B2-B1'B0!B0!B/B.B-B+B)�B)�B(�B&�B%�B#�B �B�B�B�B�B�B�B{BuBoBoBoBoBoBoBhB\BJBJBJBDB
=B
=BDB
=B	7B%BBB	7B+BBB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BB
=BDBJBVB\BVBJBDBDB\BhB{B�B�B{B�BuB�B�B{BoBhB�B�B�B�B�B#�B+B.B0!B0!B2-B33B33B49B5?B5?B5?B6FB6FB49B2-B33B<jB>wB>wB=qB;dB<jB?}B;dB9XB9XB>wBI�BN�BN�BO�BP�BR�B\)Be`BjBk�Bk�Bk�Bl�Br�Bs�Bt�Bw�B|�B� B�B�B�+B�=B�hB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�-B�3B�?B�LB�^B�qB��BBÖBȴBɺB��B��B��B��B��B�
B�B�#B�5B�HB�NB�TB�fB�B�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	B	B		7B	JB	VB	bB	oB	uB	�B	�B	�B	�B	�B	�B	!�B	%�B	'�B	'�B	+B	-B	/B	1'B	1'B	2-B	6FB	8RB	<jB	?}B	?}B	A�B	D�B	G�B	J�B	L�B	M�B	O�B	P�B	S�B	XB	YB	ZB	^5B	aHB	dZB	ffB	hsB	iyB	k�B	o�B	q�B	s�B	u�B	y�B	{�B	}�B	~�B	�B	�B	�B	�B	�B	�B	�1B	�=B	�=B	�PB	�\B	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ŢB	�5B	�B
B
{B
 �B
(�B
.B
49B
;dB
C�B
I�B
O�B
VB
\)B
bNB
gmB
jB
p�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B/*B/&B/(B.B.#B0/B0-B0-B+B�B�B{BjB)B2BdBoBmBdBYB+B�B�^B��B�_B�B��B�{B�`B�;Bl�BaJB\-B[(BXBS�BP�BF�BQ�BK�B9^B-B#�BuB
?BBB��B��B��BB4B(B!B��B��B]BhBZB
?BB�pB�8B�B�)B�B��BƨB�tB�DB�B��B��B��B�eB�	Bu�BfjB[#B?}B�B��B�
B��B�B\,B7MB�B
��B
�ZB
��B
�|B
��B
�mB
s�B
Z$B
>B
 �B
4B	�eB	��B	ȾB	��B	�.B	��B	��B	�?B	ftB	N�B	E�B	<xB	-B	�B	�B	yB	bB	BB	 B��B��B��B��B�B��B��B��B��B� B�B� B	 B��B��B��B��B��B��B�B�B�oB�bB�FB�+B�B��B��B��B��B��B��BƼBƽBıB��B�hB�\B�NB�CB�6B�1B�*B�#B�B�B�B�B�B��B��B��B��B��B�mB�=B�,B�7B�:B�DB�>B�!Bv�Br�Bo�Bl�Bk�Bk�Bj�Bg�BdrB_TB\@BY0BX)BUBSBP�BM�BL�BK�BH�BF�BE�BD�BB�B>�B;}B9qB7fB5ZB2GB1BB0<B0<B/B..B-,B+B*B*B)B'B%�B#�B �B�B�B�B�B�B�B~BuBpBoBnBpB�BpBgBxBKBMBKBaB
@B
=BDB
<B	SB%BB B	RB)BBB��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B!B
?BDBdBWBxBUBJBEB`BuBhB�B�B�B�B�B�B�B�B{B�BhB�B�B�B�B�B#�B+B..B0:B0=B2GB3NB3MB4RB5XB5YB5YB6\B6^B4RB2HB3MB<�B>�B>�B=�B;}B<�B?�B;B9qB9rB>�BI�BN�BN�BO�BP�BSB\ABevBj�Bk�Bk�Bk�Bl�Br�Bs�Bt�Bw�B}B�B�#B�/B�BB�RB�~B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�"B� B�(B�(B�6B�?B�GB�QB�_B�pB��B��B£BêB��B��B��B��B��B��B�B�B�(B�4B�IB�ZB�aB�iB�wB�B�B�B�B��B��B��B��B��B��B��B��B�B	 B	B	*B	.B		GB	YB	eB	qB	|B	�B	�B	�B	�B	�B	�B	�B	!�B	%�B	'�B	'�B	+B	-B	/*B	14B	15B	2:B	6SB	8_B	<wB	?�B	?�B	A�B	D�B	G�B	J�B	L�B	M�B	O�B	P�B	TB	XB	Y#B	Z)B	^BB	aTB	dgB	fqB	h�B	i�B	k�B	o�B	q�B	s�B	u�B	y�B	{�B	}�B		B	�B	�B	�&B	�-B	�+B	�+B	�=B	�HB	�KB	�ZB	�dB	�rB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ūB	�@B	�B
!B
�B
 �B
(�B
.B
4@B
;iB
C�B
I�B
O�B
V	B
\-B
bTB
gpB
j�B
p�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.01 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708132016053117081320160531170813  AO  ARCAADJP                                                                    20140721230830    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230830  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230830  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170813  IP                  G�O�G�O�G�O�                