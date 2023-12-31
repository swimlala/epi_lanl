CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:18Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170918  20220204114421  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               kA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @���Dt��1   @����b��@5�z�G��c1����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    kA   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bq33Bw��B�  B�  B�  B�  B���B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5fD5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D<��D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Di��Djy�Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy��D�*=D�_
D���D��HD�"�D�^�D��)D��D��D�P�D���D��=D��D�T)Dډ�D��D�fD�G�D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�G�@��@��A�
A;�
A[�
A{�
A��A��A��A��A��A޸RA��A��B��B��B��B��B&��B.��B6��B>��BF��BN��BV��B^��Bf��Bp(�Bv�]B~��B�z�B�z�B�z�B�G�B�z�B�z�B�z�B��B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�G�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�C]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C���C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸D o\D �\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\D	o\D	�\D
o\D
�\Do\D�\Do\D�\Do\D�\Do\D��Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\D o\D �\D!o\D!�\D"o\D"�\D#o\D#�\D$o\D$�\D%o\D%�\D&o\D&�\D'o\D'�\D(o\D(�\D)o\D)�\D*o\D*�\D+o\D+�\D,o\D,�\D-o\D-�\D.o\D.�\D/o\D/�\D0o\D0�\D1o\D1�\D2o\D2�\D3o\D3�\D4o\D4��D5o\D5�\D6o\D6�\D7o\D7�\D8o\D8�\D9o\D9�\D:o\D:�\D;o\D;�\D<o\D<��D=o\D=�\D>o\D>�\D?o\D?�\D@o\D@�\DAo\DA�\DBo\DB�\DCo\DC�\DDo\DD�\DEo\DE�\DFo\DF�\DGo\DG�\DHo\DH�\DIo\DI�\DJo\DJ�\DKo\DK�\DLo\DL�\DMo\DM�\DNo\DN�\DOo\DO�\DPo\DP�\DQo\DQ�\DRo\DR�\DSo\DS�\DTo\DT�\DUo\DU�\DVo\DV�\DWo\DW�\DXo\DX�\DYo\DY�\DZo\DZ�\D[o\D[�\D\o\D\�\D]o\D]�\D^o\D^�\D_o\D_�\D`o\D`�\Dao\Da�\Dbo\Db�\Dco\Dc�\Ddo\Dd�\Deo\De�\Dfo\Df�\Dgo\Dg�\Dho\Dh�\Dio\Di��Djh�Dj�\Dko\Dk�\Dlo\Dl�\Dmo\Dm�\Dno\Dn�\Doo\Do�\Dpo\Dp�\Dqh�Dq�\Dro\Dr�\Dso\Ds�\Dto\Dt�\Dy�)D�!�D�V�D��=D���D��D�VfD���D��HD�{D�HRD��3D���D�RD�K�DځHD���D�D�?\D�=D�Ȥ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�(�A�&�A�+A�(�A�-A�+A�+A�(�A�(�A�-A�5?A�5?A�7LA�C�A�I�A�M�A�S�A�^5A�t�A�z�A�v�A�\)A�?}A�5?A�(�A�"�A��A��A�(�A�C�A���A֥�AӃA�VA�K�A���A���AŮA���A�-A��HA�bNA�ZA�$�A���A��+A��;A�ZA��!A�(�A�  A��RA��uA�VA�(�A�7LA���A��;A��A�A�S�A��9A�%A���A�hsA�E�A�1'A���A��PA�"�A�G�A�Q�A�
=A���A���A��jA�{A��A��A�~�A��7A���A�oA�VA�E�A���A�+A�ȴA�1'A�A�x�A�5?A�oA���A���A��-A���A��A�5?A��A���A��A�Q�A���A��FA��A�VA���A~��Az��Ax5?AvZAs`BAo�TAn��Al�Ak?}Ai��AgS�Af-Aep�Ac\)A`9XA[��AY��AXM�AV�jASG�ARM�AQ�7AP�/ANv�AK7LAI�AI%AGXAE&�ABffA@�A>�A<��A;XA:��A9�A7l�A6$�A5O�A4�A4 �A2��A2(�A0��A.ĜA.��A.�+A.$�A-�;A-�A-oA+�A)�wA(�\A'7LA%`BA$I�A${A#A"�A"�!A"�A!O�A �A ^5A��AĜA�A��A�9AG�A�-A��AĜA�A�A^5A�TAA�RA��A�
A|�AoA��Av�AVA��A�+A`BA��A�A+A
z�A	XAz�A;dA �A��AoA��A��A  A�7A V@���@�+@�$�@���@� �@��
@��@�&�@�ƨ@��@�@�!@��#@�`B@��@�@�5?@�(�@�S�@�o@�\@��@��@�X@��@�;d@��@�E�@�O�@�@���@�%@�9@���@��`@�9@�9X@���@�X@�1@۾w@�9X@�V@���@�z�@���@��@�Q�@֟�@���@�|�@��@�E�@�hs@��@���@׍P@�|�@�33@ְ!@�ff@�{@�&�@�+@�E�@ϝ�@θR@��@��
@�;d@�K�@�dZ@�S�@��`@��@�1'@ʸR@�J@�J@ɲ-@ț�@��@ũ�@�&�@�bN@� �@�
=@�x�@�ƨ@��m@��+@��@��^@�?}@��@�^5@�\)@�O�@�1@�K�@��+@���@��@���@�t�@��@��7@�V@��@��@��;@�"�@�$�@�/@���@�Q�@�(�@��@�t�@��!@�=q@���@��@�@�J@��@���@��@��H@�+@��@��m@�@��w@�I�@��@��T@��9@�$�@��w@���@�+@�x�@��w@�@��@�E�@���@��@�1'@�z�@���@�7L@�1'@�S�@��@��+@�^5@�$�@��@���@���@�p�@�X@��@���@���@��u@��u@�z�@�j@�9X@��m@�ƨ@��w@��P@�dZ@�S�@�33@���@���@��@��R@���@�~�@�ff@�M�@�M�@�5?@��@��-@��7@���@���@���@�Z@� �@��@�1@���@��@�dZ@�;d@�K�@�C�@���@�ȴ@��R@���@�^5@�{@���@���@�x�@�`B@�?}@��@�%@���@��@���@��D@�j@�A�@�1@���@�l�@���@���@�t�@�33@�@��@��\@�V@�E�@�{@��#@���@��7@�`B@�?}@�&�@�&�@��@���@��`@�Ĝ@��9@��@���@��@�Q�@�9X@�9X@�9X@��
@�K�@�@���@���@�V@�=q@�{@��T@���@��^@�x�@�X@�?}@�/@���@�A�@��;@��@���@�?�@w��@o�q@gS@b@\/�@T��@M�M@Fu%@>��@9�^@2��@,��@(��@#+@��@~�@1�@C�@K^@7�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�(�A�&�A�+A�(�A�-A�+A�+A�(�A�(�A�-A�5?A�5?A�7LA�C�A�I�A�M�A�S�A�^5A�t�A�z�A�v�A�\)A�?}A�5?A�(�A�"�A��A��A�(�A�C�A���A֥�AӃA�VA�K�A���A���AŮA���A�-A��HA�bNA�ZA�$�A���A��+A��;A�ZA��!A�(�A�  A��RA��uA�VA�(�A�7LA���A��;A��A�A�S�A��9A�%A���A�hsA�E�A�1'A���A��PA�"�A�G�A�Q�A�
=A���A���A��jA�{A��A��A�~�A��7A���A�oA�VA�E�A���A�+A�ȴA�1'A�A�x�A�5?A�oA���A���A��-A���A��A�5?A��A���A��A�Q�A���A��FA��A�VA���A~��Az��Ax5?AvZAs`BAo�TAn��Al�Ak?}Ai��AgS�Af-Aep�Ac\)A`9XA[��AY��AXM�AV�jASG�ARM�AQ�7AP�/ANv�AK7LAI�AI%AGXAE&�ABffA@�A>�A<��A;XA:��A9�A7l�A6$�A5O�A4�A4 �A2��A2(�A0��A.ĜA.��A.�+A.$�A-�;A-�A-oA+�A)�wA(�\A'7LA%`BA$I�A${A#A"�A"�!A"�A!O�A �A ^5A��AĜA�A��A�9AG�A�-A��AĜA�A�A^5A�TAA�RA��A�
A|�AoA��Av�AVA��A�+A`BA��A�A+A
z�A	XAz�A;dA �A��AoA��A��A  A�7A V@���@�+@�$�@���@� �@��
@��@�&�@�ƨ@��@�@�!@��#@�`B@��@�@�5?@�(�@�S�@�o@�\@��@��@�X@��@�;d@��@�E�@�O�@�@���@�%@�9@���@��`@�9@�9X@���@�X@�1@۾w@�9X@�V@���@�z�@���@��@�Q�@֟�@���@�|�@��@�E�@�hs@��@���@׍P@�|�@�33@ְ!@�ff@�{@�&�@�+@�E�@ϝ�@θR@��@��
@�;d@�K�@�dZ@�S�@��`@��@�1'@ʸR@�J@�J@ɲ-@ț�@��@ũ�@�&�@�bN@� �@�
=@�x�@�ƨ@��m@��+@��@��^@�?}@��@�^5@�\)@�O�@�1@�K�@��+@���@��@���@�t�@��@��7@�V@��@��@��;@�"�@�$�@�/@���@�Q�@�(�@��@�t�@��!@�=q@���@��@�@�J@��@���@��@��H@�+@��@��m@�@��w@�I�@��@��T@��9@�$�@��w@���@�+@�x�@��w@�@��@�E�@���@��@�1'@�z�@���@�7L@�1'@�S�@��@��+@�^5@�$�@��@���@���@�p�@�X@��@���@���@��u@��u@�z�@�j@�9X@��m@�ƨ@��w@��P@�dZ@�S�@�33@���@���@��@��R@���@�~�@�ff@�M�@�M�@�5?@��@��-@��7@���@���@���@�Z@� �@��@�1@���@��@�dZ@�;d@�K�@�C�@���@�ȴ@��R@���@�^5@�{@���@���@�x�@�`B@�?}@��@�%@���@��@���@��D@�j@�A�@�1@���@�l�@���@���@�t�@�33@�@��@��\@�V@�E�@�{@��#@���@��7@�`B@�?}@�&�@�&�@��@���@��`@�Ĝ@��9@��@���@��@�Q�@�9X@�9X@�9X@��
@�K�@�@���@���@�V@�=q@�{@��T@���@��^@�x�@�X@�?}@�/@���@�A�@��;@��G�O�@�?�@w��@o�q@gS@b@\/�@T��@M�M@Fu%@>��@9�^@2��@,��@(��@#+@��@~�@1�@C�@K^@7�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
iyB
iyB
iyB
iyB
iyB
iyB
jB
iyB
jB
iyB
jB
iyB
jB
m�B
n�B
o�B
t�B
�+B
�B
�dB
�NB
�B
�B
�B
�B
�B
�B
�B
��B
��B	7B
�B
�ZB
�B�B%BB9XB2-B&�B0!B-B2-BN�BN�B^5BiyBn�BffBz�B�1B�%B�B�bB��B��B�B��B�hB{�BaHB]/B\)BR�BF�B9XB<jBQ�Bm�BjB��B��B�B�B�B-BB�B<jB7LB-B�B�BPBB�`BB��B�{B�B/B#�B �B!�B�BB
�yB
�ZB
��B
ĜB
�9B
��B
��B
�VB
�B
u�B
e`B
W
B
M�B
;dB
�B
B	��B	�fB	ǮB	�wB	�B	��B	��B	�=B	� B	z�B	m�B	[#B	?}B	0!B	%�B	�B	JB	B	  B��B��B�sB�BB�B��B�wB�B��B��B��B��B��B��B�uB�bB�bB�\B�\B�VB�PB�VB�DB�=B�7B�=B�7B�1B�+B�1B�B�B�B�B�B�B�B�B� B� B}�B}�B}�B|�B|�Bz�By�By�B|�Bx�Bw�Bs�Bt�Bp�Bo�Bq�Bs�Bq�Br�Bp�Bp�Bp�Bo�Bu�B�B�B�B�B� B�B�B�B�B� B� Bz�Bz�Bz�Bx�Bx�Bw�Bv�Bu�Bs�Br�Bt�Bt�Bt�Bt�Bu�Bv�Bx�Bw�Bz�B{�B|�B}�B�B�B�%B�DB�DB�JB�JB�PB�bB�uB��B��B��B��B�oB�hB�PB�bB��B��B��B�B�-B�3B�!B�B�B�-B�dBB��BBĜBŢBB��B��B��BB�B�fB�yB�B�B��B��B��B��B��B�B�B�B�B�B�mB�`B�fB�sB�yB��B��B��B��B��B��B	B	1B	%B	  B��B��B��B��B�B�fB�ZB�B�HB�BB�fB�yB�ZB�B��B	B��B��B��B��B��B��B	B	+B	B		7B	VB	PB	
=B		7B		7B		7B		7B		7B		7B	JB	VB	\B	hB	{B	�B	!�B	1'B	2-B	2-B	33B	5?B	<jB	>wB	8RB	0!B	1'B	9XB	5?B	2-B	:^B	H�B	I�B	I�B	E�B	?}B	<jB	C�B	@�B	=qB	>wB	&�B	-B	)�B	%�B	"�B	$�B	#�B	$�B	)�B	,B	.B	0!B	33B	5?B	7LB	<jB	?}B	B�B	C�B	C�B	D�B	G�B	K�B	Q�B	R�B	T�B	W
B	XB	YB	\)B	_;B	aHB	e`B	k�B	n�B	p�B	q�B	r�B	t�B	u�B	x�B	z�B	|�B	~�B	� B	�B	�B	�%B	�%B	�1B	�DB	�PB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�!B	�!B	�!B	�'B	�9B	�LB	�XB	�^B	�jB	�qB	�wB	�}B	�}B	��B	��B	B	ÖB	ÖB	ĜB	ƨB	ƨB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�/B	�5B	�;B	�;B	�;B	�B
 4B
�B
�B
%B
.}B
4B
9�B
>wB
E9B
I�B
N�B
U�B
Y�B
^B
bB
g�B
j�B
n�B
q�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
_XB
_XB
_XB
_XB
_XB
_XB
`]B
_XB
`]B
_XB
`]B
_WB
`]B
coB
dvB
e|B
j�B
}B
��B
�<B
�#B
�B
�xB
�lB
�eB
�eB
�rB
�~B
�B
��B
�	B
�sB
�1B
�\BbB
��B
��B/+B(B�B%�B"�B(BD�BD�BTB_KBdjB\9Bp�B~B{�By�B�3B��B��B��B��B�:Bq�BWBSBR BH�B<�B/3B2EBG�BchB`WB�tB�BLBjBjB"�B8VB22B-B"�B�BMBB��B�0B�bB�}B�SBx�B$�B�B�B�B�B
��B
�`B
�BB
·B
��B
�&B
��B
��B
�FB
yB
k�B
[TB
L�B
C�B
1\B
�B	�B	��B	�fB	��B	�{B	� B	��B	��B	�FB	v
B	p�B	c�B	Q1B	5�B	&3B	�B	�B	`B�5B�B�B��BލB�\B�8B��B��B�-B�B��B��B��B��B��B��B��B��B��B��B�zB�tB�{B�iB�bB\B�bB\B~WB}QB~WB{EBz@Bz@Bz@Bx4Bw.Bx4Bw.Bv(Bv(BtBtBtBsBsBq
BpBpBsBn�Bm�Bi�Bj�Bf�Be�Bg�Bi�Bg�Bh�Bf�Bf�Bf�Be�Bk�Bw1Bx7Bw1Bx7Bv,Bx8BzEBx8Bx8Bv,Bv-BqBqBqBoBoBm�Bl�Bk�Bi�Bh�Bj�Bj�Bj�Bj�Bk�Bl�BoBm�BqBrBsBt#Bw5B{NB|TB�sB�sB�yB�yB�B��B��B��B��B��B��B��B��B��B��B��B��B�B�6B�ZB�`B�OB�BB�<B�ZB��B��B��B��B��B��B��B��B��B��B��B�BBܐBߣB��B��B��B��B��B��B��B��B��B��B��B��BݘBۋBܑBޞBߤB��B�B�B��B��B�B�5B�ZB�NB�*B�B�B��B��B�BܒBچB��B�uB�oBܓBߦBڇB�B�B�1B�B�B�B�B�B�B�7B�VB�JB�bB	�B	{B	 hB�bB�bB�bB�bB�bB�bB	uB	�B	�B	�B	
�B	�B	�B	'OB	(UB	(UB	)[B	+gB	2�B	4�B	.zB	&JB	'PB	/�B	+hB	(VB	0�B	>�B	?�B	?�B	;�B	5�B	2�B	9�B	6�B	3�B	4�B	B	#8B	 &B	B	�B	B	B	B	 'B	"3B	$?B	&LB	)]B	+iB	-vB	2�B	5�B	8�B	9�B	9�B	:�B	=�B	A�B	HB	IB	K&B	M2B	N8B	O?B	RPB	UbB	WoB	[�B	a�B	d�B	f�B	g�B	h�B	j�B	k�B	n�B	qB	sB	uB	v%B	x1B	z>B	|JB	|JB	~UB	�hB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�$B	�$B	�*B	�*B	�0B	�6B	�=B	�CB	�CB	�CB	�CB	�CB	�CB	�IB	�ZB	�mB	�yB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�/B	�/B	�<B	�<B	�<B	�BB	�BB	�HB	�HB	�NB	�TB	�ZB	�ZG�O�B	ܠB	�QB
�B
�B
-B
$�B
*9B
/�B
4�B
;SB
?�B
D�B
K�B
O�B
T4B
X2B
]�B
aB
d�B
g�B
l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.26 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.01(+/-0.004) in PSS-78.                                                                                                                                                                                        Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144212022020411442120220204114421  AO  ARCAADJP                                                                    20200619170918    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170918  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170918  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114421  IP                  G�O�G�O�G�O�                