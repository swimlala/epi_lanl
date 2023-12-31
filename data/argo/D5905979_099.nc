CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:16Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170916  20220204114420  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               cA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @���C�<(1   @���s���@6��+�csn��P1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    cA   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH��BP  BW��B_��Bh  Bp��Bv  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D"��D#y�D$  D$� D%  D%y�D&  D&� D'  D'y�D(  D(� D)  D)� D*  D*� D+fD+� D,  D,� D-  D-� D-��D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DGfDG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV�fDW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy�qD�D�` D��RD��qD� D�_�D�� D�ҏD�( D�^D��
D��\D�&D�T�Dژ�D��
D��D�]qD�D��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�G�@��@��A�
A;�
A[�
A{�
A��A��A��A��A��A��A��A��B��B��B��B��B&��B.��B6��B>��BGBN��BV�]B^�]Bf��BoBt��B~�]B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�BǮB�z�B�z�B�z�B�G�B�G�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�C�qC�qC�qC�qC	�qC�qC�C�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1��C3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C��C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C��C�޸C�޸C���C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸D o\D �\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Dh�D�\Do\D�\D	o\D	�\D
o\D
�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\D o\D �\D!o\D!�\D"o\D"��D#h�D#�\D$o\D$�\D%h�D%�\D&o\D&�\D'h�D'�\D(o\D(�\D)o\D)�\D*o\D*��D+o\D+�\D,o\D,�\D-o\D-��D.o\D.�\D/o\D/�\D0o\D0�\D1o\D1�\D2o\D2�\D3o\D3�\D4o\D4�\D5o\D5�\D6o\D6�\D7o\D7�\D8o\D8�\D9o\D9�\D:o\D:�\D;o\D;�\D<o\D<�\D=o\D=�\D>o\D>�\D?o\D?�\D@o\D@�\DAo\DA�\DBo\DB�\DCo\DC�\DDo\DD�\DEo\DE�\DFo\DF��DGo\DG�\DHo\DH�\DIo\DI�\DJo\DJ�\DKo\DK�\DLo\DL�\DMo\DM�\DNo\DN�\DOo\DO�\DPo\DP�\DQo\DQ�\DRo\DR�\DSo\DS�\DTo\DT�\DUo\DU�\DVu�DV�\DWo\DW�\DXo\DX�\DYo\DY�\DZo\DZ�\D[o\D[�\D\o\D\�\D]o\D]�\D^o\D^�\D_o\D_�\D`o\D`�\Dao\Da�\Dbo\Db�\Dco\Dc�\Ddo\Dd�\Deo\De�\Dfo\Df�\Dgo\Dg�\Dho\Dh�\Dio\Di�\Djo\Dj�\Dko\Dk�\Dlo\Dl�\Dmo\Dm�\Dno\Dn�\Doo\Do�\Dpo\Dp�\Dqo\Dq�\Dro\Dr�\Dso\Ds�\Dto\Dt��Dy��D���D�W�D�� D��D��D�W\D��D��=D��D�U�D���D��
D��D�L{Dڐ�D�ָD�\D�UD�HD�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��TA��;A���A���A��
A��
A���AܸRAܝ�Aܧ�AܼjAܣ�A�1'A�&�A��A�VA���A��/A��TA���A��`A��A�33A��;A���A�VAδ9A���A���A��A�l�A�5?A���A��HA�%A��HA��A�=qA��A��A�ffA�|�A�VA�"�A��;A��+A��A��+A��+A�ZA���A��A���A��^A�{A���A��A�jA�%A���A�-A���A��/A��hA��HA�5?A���A�5?A���A��
A���A��A�?}A���A��A�-A���A���A�A�5?A�A�A�1A�\)A�A�ZA�"�A�VA��A�ȴA�Q�A��A���A��TA�|�A��A��A�dZA�G�A��yA�+A���A�JA�n�A�^5A�t�A�  A��A�&�A|��A{�TAx�+Au�As��AsO�AqƨAm�
AhĜAg`BAe/Aa��A` �A_%A]�-A\5?AZ��AYx�AX~�AV�AT^5AR�RAOhsAMG�AK�PAJbNAHffAG+AD�AC?}AB��AA�AA�A@^5A?|�A>1A;��A9ƨA8��A7�FA6n�A4ĜA2��A2JA1�^A1`BA0�A/�^A//A.��A-��A-`BA,JA++A*9XA)\)A'`BA%�mA%x�A$��A#�A#G�A#�A"^5A!`BA �RA 9XA��A��AdZA;dA�9AbA�A��A~�A?}A��A�wA��A�uAAE�A�uA��AG�A+A��A��A�RAVA��A
��A
��A
�+A
ZA
A	|�A�RA?}A+A�\A��A/A��A�+A33@�-@�v�@�1'@���@��@��T@��@�D@�@�ff@��@�@�p�@�$�@�Q�@�w@�l�@��@旍@���@�9@�^5@��@�G�@ם�@���@� �@�z�@��H@�ff@�-@���@�p�@�V@�(�@�1'@�A�@�ƨ@�ȴ@ɡ�@�b@Ə\@�@ř�@�O�@�A�@��@��H@���@��/@�  @��F@���@�l�@�;d@�@��@��@���@��@��@��@�;d@�v�@���@�X@�V@���@��@��@�@��@�bN@�33@�?}@���@�b@��@�|�@�
=@���@�=q@�x�@�I�@�@���@��+@��@���@��@���@�5?@�5?@�@��T@��/@��j@�9X@�t�@��h@�hs@�X@�7L@��/@���@�I�@��@��w@�t�@���@�E�@��@���@�X@�?}@�/@��`@�z�@� �@�  @��w@���@��y@��!@���@��\@���@��-@���@�&�@�A�@���@�C�@��@���@���@��+@�J@���@�hs@���@��D@��D@��D@�r�@�j@�r�@��u@��9@�%@��@��/@��`@���@��@��@��@�G�@�X@��@��@���@���@��j@���@���@���@�Z@��@��m@��;@��F@�\)@�S�@��@���@�n�@�J@���@�X@��/@���@�z�@�j@��@��@���@�o@��y@���@�~�@�^5@��@�hs@�/@���@��@��u@��u@��D@��T@��@��h@�O�@�X@�`B@�O�@�&�@��@�%@��`@��j@��D@�Z@��@���@�t�@��y@��+@�^5@�V@�E�@�V@�=q@���@���@�x�@�?}@��@��@���@�j@�A�@�(�@� �@��@��@���@���@�t�@�K�@��@���@��\@�E�@�$�@�@��@��#@��^@���@��h@�`B@��@��@���@���@�Q�@� �@���@�S�@�C�@�;d@�+@��@���@���@��@��y@���@��R@��\@�V@�$�@�	@x7�@nff@c�W@\�I@W�@Q&�@Kl�@E��@>�@7�@1+�@)k�@#j�@�4@\�@��@S&@�@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��TA��;A���A���A��
A��
A���AܸRAܝ�Aܧ�AܼjAܣ�A�1'A�&�A��A�VA���A��/A��TA���A��`A��A�33A��;A���A�VAδ9A���A���A��A�l�A�5?A���A��HA�%A��HA��A�=qA��A��A�ffA�|�A�VA�"�A��;A��+A��A��+A��+A�ZA���A��A���A��^A�{A���A��A�jA�%A���A�-A���A��/A��hA��HA�5?A���A�5?A���A��
A���A��A�?}A���A��A�-A���A���A�A�5?A�A�A�1A�\)A�A�ZA�"�A�VA��A�ȴA�Q�A��A���A��TA�|�A��A��A�dZA�G�A��yA�+A���A�JA�n�A�^5A�t�A�  A��A�&�A|��A{�TAx�+Au�As��AsO�AqƨAm�
AhĜAg`BAe/Aa��A` �A_%A]�-A\5?AZ��AYx�AX~�AV�AT^5AR�RAOhsAMG�AK�PAJbNAHffAG+AD�AC?}AB��AA�AA�A@^5A?|�A>1A;��A9ƨA8��A7�FA6n�A4ĜA2��A2JA1�^A1`BA0�A/�^A//A.��A-��A-`BA,JA++A*9XA)\)A'`BA%�mA%x�A$��A#�A#G�A#�A"^5A!`BA �RA 9XA��A��AdZA;dA�9AbA�A��A~�A?}A��A�wA��A�uAAE�A�uA��AG�A+A��A��A�RAVA��A
��A
��A
�+A
ZA
A	|�A�RA?}A+A�\A��A/A��A�+A33@�-@�v�@�1'@���@��@��T@��@�D@�@�ff@��@�@�p�@�$�@�Q�@�w@�l�@��@旍@���@�9@�^5@��@�G�@ם�@���@� �@�z�@��H@�ff@�-@���@�p�@�V@�(�@�1'@�A�@�ƨ@�ȴ@ɡ�@�b@Ə\@�@ř�@�O�@�A�@��@��H@���@��/@�  @��F@���@�l�@�;d@�@��@��@���@��@��@��@�;d@�v�@���@�X@�V@���@��@��@�@��@�bN@�33@�?}@���@�b@��@�|�@�
=@���@�=q@�x�@�I�@�@���@��+@��@���@��@���@�5?@�5?@�@��T@��/@��j@�9X@�t�@��h@�hs@�X@�7L@��/@���@�I�@��@��w@�t�@���@�E�@��@���@�X@�?}@�/@��`@�z�@� �@�  @��w@���@��y@��!@���@��\@���@��-@���@�&�@�A�@���@�C�@��@���@���@��+@�J@���@�hs@���@��D@��D@��D@�r�@�j@�r�@��u@��9@�%@��@��/@��`@���@��@��@��@�G�@�X@��@��@���@���@��j@���@���@���@�Z@��@��m@��;@��F@�\)@�S�@��@���@�n�@�J@���@�X@��/@���@�z�@�j@��@��@���@�o@��y@���@�~�@�^5@��@�hs@�/@���@��@��u@��u@��D@��T@��@��h@�O�@�X@�`B@�O�@�&�@��@�%@��`@��j@��D@�Z@��@���@�t�@��y@��+@�^5@�V@�E�@�V@�=q@���@���@�x�@�?}@��@��@���@�j@�A�@�(�@� �@��@��@���@���@�t�@�K�@��@���@��\@�E�@�$�@�@��@��#@��^@���@��h@�`B@��@��@���@���@�Q�@� �@���@�S�@�C�@�;d@�+@��@���@���@��@��y@���@��R@��\@�VG�O�@�	@x7�@nff@c�W@\�I@W�@Q&�@Kl�@E��@>�@7�@1+�@)k�@#j�@�4@\�@��@S&@�@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
F�B
E�B
A�B
B�B
C�B
B�B
B�B
?}B
<jB
<jB
?}B
?}B
0!B
/B
.B
,B
(�B
#�B
B	��B	�qB	�B	��B	��B	��B	�B	�B	ɺB	�B	�B
;dB
dZB
x�B
�\B
�RB
�`B+B �BP�Bn�B��B�!B�wB�NB�yB�B��B  B��B��B��B��B��B��B��B��B��B��B��B��B  BB\B1B�mB��B�VBdZBP�BC�B1'B.B.B[#B|�B�=Bv�BP�B5?BD�BL�B�B�?B��B�qB�9B�{BiyB6FB:^B!�B
��B
�B
��B  B�B+B
�yB
�B
�BB
��B
��B
�XB
�B
��B
�PB
dZB
Q�B
33B
&�B
VB	��B	�sB	�NB	�B	�wB	�oB	�1B	v�B	e`B	ZB	VB	L�B	C�B	;dB	33B	/B	(�B	 �B	�B	B�B�;B�B��B��BĜB�XB�FB�'B�B�B�B��B��B��B��B�oB�PB�PB�VB�DB�DB�=B�=B�=B�7B�1B�1B�+B�+B�B�B�B�B{�By�By�By�Bu�Bu�Bw�Bs�Bp�Bn�Bl�Bl�Bq�Bp�Bv�Bu�Bo�BjBbNBbNBdZBcTB_;B]/B\)BXBP�BI�BH�BH�BI�BJ�BK�BK�BL�BL�BL�BM�BN�BN�BM�BM�BI�BJ�BH�BI�BG�BG�BF�BF�BG�BF�BB�B@�BA�B?}B>wB=qB;dB;dB9XB6FB6FB8RB49B5?B5?B7LB6FB7LB8RB=qB9XB2-B"�B�B�B�B�B!�B"�B$�B(�B/B33B49B5?B7LB9XBA�BG�BH�BI�BH�BH�BJ�BJ�BM�BW
BYB\)B]/B^5B^5B^5B_;B_;B`BBbNBffBffBjBo�Bt�Bu�Bw�Bw�Bw�Bw�Bz�B|�B� B�B� B~�B�B�%B�VB�{B��B��B��B��B��B��B��B��B��B��B�B�B�!B�3B�LB�RB�dB�dB�jB��BǮBɺB��B��B��B��B��B��B�B�B�HB�NB�TB�ZB�fB�fB�mB�yB�B�B�B��B��B��B��B��B��B��B��B��B	B	B	JB	VB	bB	oB	oB	uB	�B	�B	�B	�B	#�B	)�B	-B	-B	.B	.B	/B	49B	8RB	>wB	?}B	A�B	B�B	C�B	D�B	D�B	F�B	F�B	G�B	H�B	J�B	M�B	M�B	N�B	Q�B	VB	\)B	bNB	dZB	e`B	k�B	r�B	t�B	w�B	z�B	� B	�B	�7B	�=B	�DB	�DB	�DB	�DB	�DB	�JB	�PB	�bB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�3B	�3B	�?B	�LB	�RB	�XB	�^B	�^B	�jB	�wB	�}B	�}B	��B	��B	��B	ÖB	ĜB	ŢB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�5B	�;B	�;B	�;B	�BB	�BB	�HB	�`B	�fB	�mB	�fB	�fB	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�(B
	lB
B
kB
!�B
*�B
-�B
4�B
>B
EB
K�B
Q B
ZkB
a-B
gB
k�B
pB
q[B
v�B
y$B
.111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
=xB
<rB
8YB
9_B
:fB
9_B
9_B
6MB
3;B
3;B
6MB
6MB
&�B
%�B
$�B
"�B
�B
�B	��B	��B	�KB	��B	��B	�_B	�qB	��B	��B	��B	��B	�eB
2:B
[-B
o�B
�-B
� B
�+B
��B�BG�Be[B�lB��B�5B�	B�4B�FB�B��B�B�B�B�B�B�B��B��B�B��B��B��B��B��BB��B�,B��B�B[#BG�B:bB'�B$�B$�BQ�Bs�B�Bm�BG�B,B;iBC�B{�B�B�MB�6B��B�CB`EB-B1.B�B
�B
�tB
�B
��BsB
� B
�PB
�VB
�B
ŲB
�cB
�3B
��B
�kB
�/B
[<B
H�B
*B
�B
?B	�B	�`B	�;B	��B	�gB	�cB	&B	m�B	\XB	QB	L�B	C�B	:�B	2`B	*0B	&B	�B	�B	�B�B�B�@B�"B�B��B��B�aB�OB�0B�B�B�B��B��B��B��B�|B�]B�^B�dB�RB�RB�KB�KB�LB�FB@B@B~:B~:B|/B|/BxByBr�Bp�Bp�Bp�Bl�Bl�Bn�Bj�Bg�Be�Bc�Bc�Bh�Bg�Bm�Bl�Bf�Ba�BYcBYcB[oBZiBVQBTEBS?BO'BG�B@�B?�B?�B@�BA�BB�BB�BC�BC�BC�BD�BE�BE�BD�BD�B@�BA�B?�B@�B>�B>�B=�B=�B>�B=�B9�B7�B8�B6�B5�B4�B2�B2�B0uB-dB-dB/pB+WB,]B,]B.jB-dB.jB/pB4�B0wB)LB�B�B�B�B�B�B�B�B B&<B*TB+ZB,`B.lB0xB8�B>�B?�B@�B?�B?�BA�BA�BD�BN)BP6BSHBTNBUTBUTBUTBVZBVZBWaBYmB]�B]�Ba�Bf�Bk�Bl�Bn�Bn�Bn�Bn�Bq�BtBwBx#BwBvBy*B}CB�sB��B��B��B��B��B��B��B��B��B��B�B�B�7B�=B�NB�gB�mB�B�B��B��B��B��B��B��B��B�B�B�B�B�6B�aB�gB�mB�sB�B�BކB��B�B�B��B��B��B��B��B��B��B�B�B�B�)B�6B	aB	lB	xB		�B		�B	
�B	�B	�B	�B	�B	�B	!B	$#B	$#B	%(B	%(B	&/B	+MB	/fB	5�B	6�B	8�B	9�B	:�B	;�B	;�B	=�B	=�B	>�B	?�B	A�B	D�B	D�B	E�B	H�B	MB	S:B	Y_B	[kB	\qB	b�B	i�B	k�B	n�B	q�B	wB	|.B	�EB	�KB	�RB	�RB	�RB	�RB	�RB	�XB	�^B	�pB	�vB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�9B	�?B	�?B	�KB	�XB	�]B	�cB	�iB	�iB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�-B	�8B	�>B	�>B	�DB	�DB	�DB	�KB	�KB	�QB	�iB	�oB	�vB	�oB	�oB	�oB	�vB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��G�O�B	�0B
 sB
	B
rB
�B
!�B
$�B
+�B
5B
<
B
B�B
HB
QoB
X1B
^"B
b�B
g
B
h^B
m�B
p'B
v1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.26 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.009(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144212022020411442120220204114421  AO  ARCAADJP                                                                    20200619170916    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170916  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170916  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114421  IP                  G�O�G�O�G�O�                