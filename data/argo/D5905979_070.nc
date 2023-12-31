CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:10Z creation      
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
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �0   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �D   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �T   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �X   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �\   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20200619170910  20220204114417  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               FA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ص�-��R1   @ص��l& @6i7KƧ��c���`A�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    FA   B   B   @���@���A   A   A@  A`  A~ffA�33A�33A�  A�  A���A���A�  A�33B��BffB  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
�fD  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D.��D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DHy�DH��DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DWfDW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�Dt  Dts3Dy�D�( D�Y�D���D��D�{D�V�D��qD��fD�{D�_�D�� D��=D��D�M�Dڋ�D��D��D�T)D�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�G�@�z�@��A�
A;�
A[�
Az=pA��A��A��A��AθRA޸RA��A��B�]B\)B��B��B&��B.��B6��B?\)BF��BN��BV��B^��Bf��Bn��Bv��B~��B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�BϮB�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�C3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]��C_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C��C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸D o\D �\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\D	o\D	�\D
u�D
�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D��Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\D o\D �\D!o\D!�\D"o\D"�\D#o\D#�\D$o\D$�\D%o\D%�\D&o\D&�\D'o\D'�\D(o\D(�\D)o\D)�\D*o\D*�\D+o\D+�\D,o\D,�\D-o\D-�\D.o\D.��D/o\D/�\D0o\D0�\D1o\D1�\D2o\D2�\D3o\D3�\D4o\D4�\D5o\D5�\D6o\D6�\D7o\D7�\D8o\D8�\D9o\D9�\D:o\D:�\D;o\D;�\D<o\D<�\D=o\D=�\D>o\D>�\D?o\D?�\D@o\D@�\DAo\DA�\DBo\DB�\DCo\DC�\DDo\DD�\DEo\DE�\DFo\DF�\DGo\DG�\DHh�DH��DIo\DI�\DJo\DJ�\DKo\DK�\DLo\DL�\DMo\DM�\DNo\DN�\DOo\DO�\DPo\DP�\DQo\DQ�\DRo\DR�\DSo\DS�\DTo\DT�\DUo\DU�\DVo\DV��DWo\DW�\DXo\DX�\DYo\DY�\DZo\DZ�\D[o\D[�\D\o\D\�\D]o\D]�\D^o\D^�\D_o\D_�\D`o\D`�\Dao\Da�\Dbo\Db�\Dco\Dc�\Ddo\Dd�\Deo\De�\Dfo\Df�\Dgo\Dg�\Dho\Dh�\Dio\Di�\Djo\Dj�\Dko\Dk�\Dlo\Dl�\Dmo\Dm�\Dno\Dn�\Doo\Do�\Dpo\Dp�\Dqo\Dq�\Dro\Dr�\Dsh�Ds�\Dtb�Dyt{D��D�QHD��{D��\D�)D�NfD��D��D�)D�W\D���D���D�fD�EqDڃ�D��3D��D�K�D�=D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��#A��/A��/A��;A��;A��HA��HA��HA��TA��TA��TA��mA��HA��`A��HA��/A��`A�ƨAhA�dZA�z�A��9A�ĜA�1A��A���A��hA�9XA��A��A�M�A��TA���A�Q�A���A�`BA��A��+A�~�A�jA� �A���A��RA�hsA�dZA���A�^5A��!A�9XA�$�A��
A�E�A��RA���A�^5A�1A���A��A�1'A�K�A�  A���A�C�A�A���A�~�A�XA��uA���A��-A�~�A�v�A�bNA�\)A�?}A�7LA�{A�\)A�$�A�jA��A�ffA���A��RA�  A���A���A���A�/A���A��wA�|�A���A��/A�
=A�$�A���A��uA�
=A�jA�p�A��A���A�$�A��HA��A��AzM�AwƨAv9XAr��Ap�AnZAi33AfjAcXAb1'Aa��AbAa�^A`��A_O�A[�7AW;dATĜASO�AQ33AOƨAO33AN��AM�wAKS�AJn�AI&�AHM�AE"�AB�DAA�AA��AA�PAAt�AA?}A>��A;�A;l�A;"�A9G�A7�^A4�/A21A0ZA.�A,ffA*�RA*r�A*A�A)A&�!A&ffA$�uA#dZA!�-A!A �HA ��A 9XAAXA�A�jA�!AA�A�7A~�A��Ar�A�AA�AG�An�A`BAAAp�A�A�RAbNAr�A�AO�A"�AĜAoA �A|�A33A�AA��A��A�Av�AAz�A  A�-A��AXA ��A v�@�|�@�Ĝ@��y@�I�@��H@�%@�  @�ff@���@��@��m@�V@�I�@�|�@��#@�V@�1'@޸R@�-@ݲ-@ݡ�@��m@ڧ�@���@؋D@���@�J@��#@ղ-@Ցh@Չ7@Ձ@���@�A�@Ӿw@�|�@�33@ҟ�@�J@Ѻ^@��/@�^5@�x�@���@��
@�o@���@�V@�hs@ȃ@�I�@���@��
@��@ě�@ÍP@§�@�E�@���@�`B@��j@�z�@��@�;d@�~�@��7@��9@�1'@�l�@�ȴ@��@��@�/@�j@�K�@��H@��@��@���@��7@��@�A�@���@�33@���@�ȴ@�ȴ@��R@��\@��+@�^5@��@�`B@�V@���@�1'@�~�@��-@�7L@��/@���@��@��;@�ƨ@�t�@��@�$�@��@��9@�Z@� �@��w@�S�@��!@�{@�X@���@�Ĝ@��D@� �@��;@�|�@��@��y@���@�=q@��@��@���@�O�@��@��@��@�I�@�b@��m@�|�@�+@�@��y@��!@��+@�ff@�@��^@���@�V@���@��@�Q�@���@�\)@�C�@�"�@�"�@��@���@���@�M�@�{@���@�p�@�?}@�&�@���@�r�@�bN@�A�@��
@��@�\)@���@��!@��!@���@��\@�~�@�E�@���@�hs@��@��`@�Ĝ@��@��u@��D@�r�@�I�@��P@���@��R@�n�@�V@�V@�E�@�$�@�{@�J@�@��@��T@���@�&�@��@�Ĝ@�z�@�A�@��m@��w@�dZ@��y@��\@��\@�hs@���@��@��#@���@��^@���@��7@�`B@��@��9@���@�r�@�A�@��@�1@��@�ƨ@�l�@�o@�$�@�@��^@�p�@�O�@��@���@���@��@��@��@��9@��u@�z�@�9X@�1@�ƨ@�;d@��@��@��h@�x�@�x�@�p�@�X@��@��@�&�@�/@�7L@��@��9@��@�j@��m@�
=@�v�@�$�@��#@���@�@��^@�x�@�/@�Ĝ@>�@{ i@p-�@g�@]��@V6�@P�z@J�@A��@:@�@46@.n�@*@�@%�C@"V@e,@?�@A�@��@�@%F11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A��#A��/A��/A��;A��;A��HA��HA��HA��TA��TA��TA��mA��HA��`A��HA��/A��`A�ƨAhA�dZA�z�A��9A�ĜA�1A��A���A��hA�9XA��A��A�M�A��TA���A�Q�A���A�`BA��A��+A�~�A�jA� �A���A��RA�hsA�dZA���A�^5A��!A�9XA�$�A��
A�E�A��RA���A�^5A�1A���A��A�1'A�K�A�  A���A�C�A�A���A�~�A�XA��uA���A��-A�~�A�v�A�bNA�\)A�?}A�7LA�{A�\)A�$�A�jA��A�ffA���A��RA�  A���A���A���A�/A���A��wA�|�A���A��/A�
=A�$�A���A��uA�
=A�jA�p�A��A���A�$�A��HA��A��AzM�AwƨAv9XAr��Ap�AnZAi33AfjAcXAb1'Aa��AbAa�^A`��A_O�A[�7AW;dATĜASO�AQ33AOƨAO33AN��AM�wAKS�AJn�AI&�AHM�AE"�AB�DAA�AA��AA�PAAt�AA?}A>��A;�A;l�A;"�A9G�A7�^A4�/A21A0ZA.�A,ffA*�RA*r�A*A�A)A&�!A&ffA$�uA#dZA!�-A!A �HA ��A 9XAAXA�A�jA�!AA�A�7A~�A��Ar�A�AA�AG�An�A`BAAAp�A�A�RAbNAr�A�AO�A"�AĜAoA �A|�A33A�AA��A��A�Av�AAz�A  A�-A��AXA ��A v�@�|�@�Ĝ@��y@�I�@��H@�%@�  @�ff@���@��@��m@�V@�I�@�|�@��#@�V@�1'@޸R@�-@ݲ-@ݡ�@��m@ڧ�@���@؋D@���@�J@��#@ղ-@Ցh@Չ7@Ձ@���@�A�@Ӿw@�|�@�33@ҟ�@�J@Ѻ^@��/@�^5@�x�@���@��
@�o@���@�V@�hs@ȃ@�I�@���@��
@��@ě�@ÍP@§�@�E�@���@�`B@��j@�z�@��@�;d@�~�@��7@��9@�1'@�l�@�ȴ@��@��@�/@�j@�K�@��H@��@��@���@��7@��@�A�@���@�33@���@�ȴ@�ȴ@��R@��\@��+@�^5@��@�`B@�V@���@�1'@�~�@��-@�7L@��/@���@��@��;@�ƨ@�t�@��@�$�@��@��9@�Z@� �@��w@�S�@��!@�{@�X@���@�Ĝ@��D@� �@��;@�|�@��@��y@���@�=q@��@��@���@�O�@��@��@��@�I�@�b@��m@�|�@�+@�@��y@��!@��+@�ff@�@��^@���@�V@���@��@�Q�@���@�\)@�C�@�"�@�"�@��@���@���@�M�@�{@���@�p�@�?}@�&�@���@�r�@�bN@�A�@��
@��@�\)@���@��!@��!@���@��\@�~�@�E�@���@�hs@��@��`@�Ĝ@��@��u@��D@�r�@�I�@��P@���@��R@�n�@�V@�V@�E�@�$�@�{@�J@�@��@��T@���@�&�@��@�Ĝ@�z�@�A�@��m@��w@�dZ@��y@��\@��\@�hs@���@��@��#@���@��^@���@��7@�`B@��@��9@���@�r�@�A�@��@�1@��@�ƨ@�l�@�o@�$�@�@��^@�p�@�O�@��@���@���@��@��@��@��9@��u@�z�@�9X@�1@�ƨ@�;d@��@��@��h@�x�@�x�@�p�@�X@��@��@�&�@�/@�7L@��@��9@��@�j@��m@�
=@�v�@�$�@��#@���@�@��^@�x�@�/G�O�@>�@{ i@p-�@g�@]��@V6�@P�z@J�@A��@:@�@46@.n�@*@�@%�C@"V@e,@?�@A�@��@�@%F11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B+B)�B+B+B+B+B+B,B-B0!B7LB>wB]/BiyBs�B~�B�1B�PB�uB��B��B��B�B�-B�^BŢB��B�B�
B�B�HB�`B�yB�BB\B{B"�B)�B<jB<jB;dBI�BO�BQ�BQ�BP�BR�BM�BJ�BG�BE�BC�BB�B?}B>wB=qB7LB5?B,B)�B)�B(�B'�B&�B%�B#�B�BoBDB  B�fB��B�wB��B�=B�BffBXBW
BS�BO�BI�B@�B%�BbB	7B
�B
��B
�XB
��B
�B
x�B
r�B
^5B
K�B
A�B
�B
  B	�B	�
B	��B	�LB	��B	~�B	q�B	iyB	hsB	jB	jB	ffB	`BB	S�B	;dB	,B	#�B	�B	hB	JB		7B	%B��B�B�fB�TB��BŢBĜBĜBŢBǮBɺBƨB�XB��BŢBB�^B�B��B�PB�%B�B}�B}�B|�B~�B|�Bz�Bx�Bs�Bm�BjBiyBhsBgmBffBe`BdZBcTBcTBcTBaHB`BB_;B]/BYBXBT�BS�BS�BP�BO�BO�BN�BK�BI�BI�BD�BC�BB�BA�BC�BA�BB�BB�BB�BB�BO�BQ�BP�BP�BQ�BQ�BQ�BP�BS�BS�BT�BT�BVB[#B_;BdZBhsBl�Bk�Bm�Bl�Bm�Bm�Bl�Bl�BjBiyBp�Br�Bw�Bv�Bx�Bx�B|�B{�Bz�B}�B|�B{�Bz�Bz�Bz�By�By�Bz�Bz�B|�B|�B}�B�B�B�B�B�B�B�B� B}�B}�B}�B~�B�B�B�B�B�+B�PB�bB�hB�{B��B��B��B��B��B��B��B��B��B��B�B�B�'B�3B�3B�XB�dB�jB�qB�qB�wB�wB�}B��BBŢBǮBǮBǮBȴBɺBɺBɺB��B��B��B��B��B�5B�NB�ZB�fB�mB�yB�B�B�B�B��B��B��B��B	  B	B	B	1B	JB	bB	uB	{B	�B	�B	�B	�B	�B	 �B	#�B	%�B	&�B	'�B	(�B	-B	0!B	2-B	33B	5?B	6FB	7LB	:^B	<jB	=qB	>wB	?}B	@�B	A�B	B�B	D�B	E�B	I�B	I�B	J�B	N�B	S�B	VB	W
B	XB	XB	XB	YB	\)B	^5B	`BB	cTB	e`B	ffB	gmB	iyB	l�B	l�B	m�B	q�B	t�B	u�B	x�B	{�B	|�B	}�B	~�B	� B	�B	�%B	�7B	�7B	�=B	�DB	�DB	�DB	�DB	�DB	�DB	�PB	�VB	�\B	�hB	�hB	�oB	�oB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�-B	�3B	�LB	�dB	�dB	�jB	�jB	�jB	�qB	�qB	�wB	��B	B	ÖB	ÖB	ĜB	ĜB	ŢB	ŢB	ŢB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�#B	�/B	�5B	�;B	�BB	�BB	�BB	�BB	�NB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�ZB	�ZB	�ZB	�NB	�NB	�NB	�TB	�TB	�ZB	�ZB	�`B	�fB	�mB	�B
 OB
�B
�B
# B
*�B
0!B
7�B
?HB
FYB
LB
R:B
V�B
Z�B
^�B
c�B
m�B
r�B
u�B
{�B
~B11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B"AB"AB"AB"AB"AB"AB"AB"AB"AB"AB#FB"AB#FB#FB#FB#FB#FB$LB%RB(eB/�B6�BUqBa�Bk�Bw;B�qB��B��B��B��B�/B�FB�kB��B��B�B�@B�FB�SBلBݜB�B��B�FB�B�B
B"4B4�B4�B3�BA�BHBJ#BJ#BIBK*BFBB�B?�B=�B;�B:�B7�B6�B5�B/�B-zB$DB"8B"8B!2B ,B%BBB�B
�B�B�?BާB�/B��B�B��B{ZB^�BP\BOVBLDBH+BBB8�B2B�B�B
��B
�AB
��B
�B
}zB
q1B
kB
V�B
D&B
9�B
�B	�eB	�B	�rB	�0B	��B	��B	whB	jB	a�B	`�B	b�B	b�B	^�B	X�B	LjB	3�B	$}B	MB	B		�B	�B	�B��B�=B�B��B��B�hB� B�B�B� B�,B�7B�&B��B�B� B�B��B��B�B��B~�B|�BvyBvyBusBwBusBsgBq[Bl<BfBcBb B`�B_�B^�B]�B\�B[�B[�B[�BY�BX�BW�BU�BQ�BP�BM�BL�BL�BIpBHjBHjBGdBDRBBEBBFB=(B<"B;B:B<"B:B;B;B;B;BHkBJxBIrBIrBJyBJyBJyBIrBL�BL�BM�BM�BN�BS�BW�B\�Ba BeBdBfBeBfBfBeBeBcBbBi2Bk>Bp]BoWBqcBqcBu|BtuBsoBv�Bu|BtuBsoBsoBsoBriBriBsoBsoBu|Bu|Bv�By�Bz�Bz�B|�B}�B{�Bz�Bx�Bv�Bv�Bv�Bw�By�B|�B}�B|�B�B��B��B��B�	B�B�B�(B�(B�4B�FB�eB�qB�wB��B��B��B��B��B��B��B��B��B��B��B�B�B�
B�B�B�/B�;B�;B�;B�AB�GB�FB�GB�SB�eB�kB�kB͊B��B��B��B��B��B�B�B�B�B�/B�GB�ZB�qB�~B��B��B��B	 �B	�B	�B	�B	B	B	B	)B	5B	GB	NB	`B	kB	qB	 xB	!~B	%�B	(�B	*�B	+�B	-�B	.�B	/�B	2�B	4�B	5�B	6�B	8B	9
B	:B	;B	=#B	>)B	BAB	BAB	CHB	G`B	L~B	N�B	O�B	P�B	P�B	P�B	Q�B	T�B	V�B	X�B	[�B	]�B	^�B	_�B	a�B	eB	eB	fB	j/B	mAB	nHB	qYB	tkB	urB	vxB	w~B	x�B	z�B	~�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�5B	�;B	�AB	�NB	�TB	�YB	�eB	�kB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�5B	�5B	�BB	�HB	�NB	�TB	�_B	�lB	�xB	�xB	�~B	�~B	ϊB	ϊB	ϊB	їB	ӣB	կB	ֵB	׻B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	�)B	��B
QB
B
�B
#0B
(�B
0LB
7�B
>�B
D�B
J�B
O8B
SjB
V�B
\RB
f[B
kB
nrB
tGB
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.26 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.007(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144172022020411441720220204114417  AO  ARCAADJP                                                                    20200619170910    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170910  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170910  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114417  IP                  G�O�G�O�G�O�                