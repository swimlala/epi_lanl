CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:01Z creation      
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
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20200619170901  20220204114414  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               %A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ؑ��;�1   @ؑ�K�"@8k��Q��c��;dZ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    %A   B   B   @�  @�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bo��Bx  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C-�fC/�fC2  C4  C6  C8�C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  Dy�D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJy�DJ��DK� DL  DL� DM  DM� DN  DN� DO  DO� DO��DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[�fD\fD\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Djy�Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Doy�Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy{�D�)HD�[�D��
D���D�	�D�e�D��3D���D�#3D�^�D�� D�� D��D�T�Dڊ�D��=D�%qD�PRD��D��H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@�z�A�
A;�
A[�
A{�
A��A��A��A��A��A��A��A��B��B��B��B��B&��B.��B6��B>��BF��BN��BV��B^��Bf��Bn�]Bv��B~��B�z�B�z�B��B��B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-��C/��C1�qC3�qC5�qC7�C9�C;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C��C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C��C��C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸D o\D �\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D��Do\D�\Do\D�\D	o\D	�\D
o\D
�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Dh�D�\Do\D�\Do\D�\Dh�D�\Do\D�\D o\D �\D!o\D!�\D"o\D"�\D#o\D#�\D$o\D$�\D%o\D%�\D&o\D&�\D'o\D'�\D(o\D(�\D)o\D)�\D*o\D*�\D+o\D+�\D,o\D,�\D-o\D-�\D.o\D.�\D/o\D/�\D0o\D0�\D1o\D1�\D2o\D2�\D3o\D3�\D4o\D4�\D5o\D5�\D6o\D6�\D7o\D7�\D8o\D8�\D9o\D9�\D:o\D:�\D;o\D;�\D<o\D<�\D=o\D=�\D>o\D>�\D?o\D?�\D@o\D@�\DAo\DA�\DBo\DB�\DCo\DC�\DDo\DD�\DEo\DE�\DFo\DF�\DGo\DG�\DHo\DH�\DIo\DI�\DJh�DJ��DKo\DK�\DLo\DL�\DMo\DM�\DNo\DN�\DOo\DO��DPo\DP�\DQo\DQ�\DRo\DR�\DSo\DS�\DTo\DT�\DUo\DU�\DVo\DV�\DWo\DW�\DXo\DX�\DYo\DY�\DZo\DZ�\D[u�D[��D\o\D\�\D]o\D]�\D^o\D^�\D_o\D_�\D`o\D`�\Dao\Da�\Dbo\Db�\Dco\Dc�\Ddo\Dd�\Deo\De�\Dfo\Df�\Dgo\Dg�\Dho\Dh�\Dio\Di�\Djh�Dj�\Dko\Dk�\Dlo\Dl�\Dmo\Dm�\Dno\Dn�\Doh�Do�\Dpo\Dp�\Dqo\Dq�\Dro\Dr�\Dso\Ds�\Dth�Dyj�D� �D�S3D���D��qD��D�]qD���D���D��D�VfD���D�ϮD�HD�L{Dڂ=D���D�D�H D�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�S�A�XA�VA�VA�\)A�ZA�ZA�\)A�\)A�\)A�\)A�^5A�bNA�bNA�`BA�`BA�bNA�dZA�ffA�ffA�hsA�jA�jA�jA�ffA�K�A�$�Aʴ9A�p�AŃA��mA��A��RA�5?A��A���A�?}A���A� �A��jA�/A��A�ȴA�ȴA�A�v�A�ffA��A���A�1'A�`BA���A��-A���A�=qA�ȴA��7A�+A��!A�+A���A�"�A��yA��DA��A�ZA�%A��PA���A�XA�(�A��;A�M�A���A�K�A�ĜA���A�1A��!A�l�A��#A�hsA�ȴA�hsA�$�A�Q�A�-A���A�E�A��;A���A���A��!A�\)A��A�;dA���A��A���A�?}A�  A��A��A�O�A�%A�-A�C�A�hsA�p�A�{A��jA��A���A�-A��A���A��
A��\A�-A��A��uAC�A}p�A{�TAz �Axn�Au�AsdZApĜAoƨAo�Am�Aj��AiK�Ae��A_�PA[�
AY�7AW��AU�#AS�AR1APAO��AO�AN��AN�AL��AK?}AI�AH�HAH  AGoAD�yADABbNA@�uA?&�A=�
A<M�A;O�A:VA8��A89XA7��A6�9A5��A57LA4��A3x�A1&�A/dZA.�A-VA,E�A)��A(v�A'&�A&��A%��A$�A#��A"�9A!��A!�PA!+A $�A+AVA�^AC�A�/A{A�^A�PA/A�9A��AG�A��A��A��A�AVA5?A�A�9A��A��AI�A��A%Az�A�A�
A
5?A	p�A��A��A�A/A�DAA��A�A5?Ax�A bN@��@��-@�b@�@��/@��@�-@�Z@��@�w@�@���@�F@�M�@�@�`B@�@��
@�-@�r�@�|�@��@߅@�@��y@���@���@�^5@ܓu@۶F@�"�@ّh@�7L@ش9@��@�=q@���@���@��@�/@�r�@� �@���@�@��@�/@̼j@˕�@��T@�(�@ǍP@�K�@�+@ƸR@Ų-@���@��m@�dZ@���@�@�M�@�{@�G�@���@�1@�;d@�E�@���@�?}@��@���@�Q�@���@�M�@�J@��^@���@��@�bN@��@�ȴ@�=q@�&�@��9@�z�@� �@�33@�x�@��@�bN@�(�@��w@�+@��R@�{@���@�`B@���@�r�@�1'@���@�K�@��@�v�@�-@�O�@��D@��w@�t�@��@�\)@���@���@��R@�K�@�ȴ@�V@��@��@��@�|�@��D@�%@��9@���@�j@�z�@�z�@�j@���@�=q@�M�@��+@�+@���@�/@���@�@���@���@��R@���@���@��R@���@�^5@�V@�V@�E�@��@���@��^@���@��^@�O�@�G�@�?}@���@��`@�b@��@���@�;d@��\@��+@�~�@�V@�p�@�Q�@�ƨ@�@���@��@�@�~�@�=q@�@��^@��@��@�x�@�X@�%@���@��9@�b@��
@�K�@�v�@��@��@���@��^@��@�O�@��@�X@��@��7@���@��@��D@� �@�  @��;@���@�|�@�;d@��@���@��+@�^5@�-@���@�@���@���@��
@�|�@�"�@�
=@��@��H@��@�ȴ@��R@��\@��@�X@�O�@�?}@�V@��/@��u@���@�(�@��w@��w@�t�@��@�S�@�"�@�o@�"�@��@��@���@�5?@��@��@���@��T@���@�x�@�x�@�`B@�X@�X@�X@�X@�.�@��6@za|@n�R@h�@[t�@Rff@J4@C�
@>�1@5rG@0U2@,�u@'X�@#�@B[@w�@��@/�@�g@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�S�A�XA�VA�VA�\)A�ZA�ZA�\)A�\)A�\)A�\)A�^5A�bNA�bNA�`BA�`BA�bNA�dZA�ffA�ffA�hsA�jA�jA�jA�ffA�K�A�$�Aʴ9A�p�AŃA��mA��A��RA�5?A��A���A�?}A���A� �A��jA�/A��A�ȴA�ȴA�A�v�A�ffA��A���A�1'A�`BA���A��-A���A�=qA�ȴA��7A�+A��!A�+A���A�"�A��yA��DA��A�ZA�%A��PA���A�XA�(�A��;A�M�A���A�K�A�ĜA���A�1A��!A�l�A��#A�hsA�ȴA�hsA�$�A�Q�A�-A���A�E�A��;A���A���A��!A�\)A��A�;dA���A��A���A�?}A�  A��A��A�O�A�%A�-A�C�A�hsA�p�A�{A��jA��A���A�-A��A���A��
A��\A�-A��A��uAC�A}p�A{�TAz �Axn�Au�AsdZApĜAoƨAo�Am�Aj��AiK�Ae��A_�PA[�
AY�7AW��AU�#AS�AR1APAO��AO�AN��AN�AL��AK?}AI�AH�HAH  AGoAD�yADABbNA@�uA?&�A=�
A<M�A;O�A:VA8��A89XA7��A6�9A5��A57LA4��A3x�A1&�A/dZA.�A-VA,E�A)��A(v�A'&�A&��A%��A$�A#��A"�9A!��A!�PA!+A $�A+AVA�^AC�A�/A{A�^A�PA/A�9A��AG�A��A��A��A�AVA5?A�A�9A��A��AI�A��A%Az�A�A�
A
5?A	p�A��A��A�A/A�DAA��A�A5?Ax�A bN@��@��-@�b@�@��/@��@�-@�Z@��@�w@�@���@�F@�M�@�@�`B@�@��
@�-@�r�@�|�@��@߅@�@��y@���@���@�^5@ܓu@۶F@�"�@ّh@�7L@ش9@��@�=q@���@���@��@�/@�r�@� �@���@�@��@�/@̼j@˕�@��T@�(�@ǍP@�K�@�+@ƸR@Ų-@���@��m@�dZ@���@�@�M�@�{@�G�@���@�1@�;d@�E�@���@�?}@��@���@�Q�@���@�M�@�J@��^@���@��@�bN@��@�ȴ@�=q@�&�@��9@�z�@� �@�33@�x�@��@�bN@�(�@��w@�+@��R@�{@���@�`B@���@�r�@�1'@���@�K�@��@�v�@�-@�O�@��D@��w@�t�@��@�\)@���@���@��R@�K�@�ȴ@�V@��@��@��@�|�@��D@�%@��9@���@�j@�z�@�z�@�j@���@�=q@�M�@��+@�+@���@�/@���@�@���@���@��R@���@���@��R@���@�^5@�V@�V@�E�@��@���@��^@���@��^@�O�@�G�@�?}@���@��`@�b@��@���@�;d@��\@��+@�~�@�V@�p�@�Q�@�ƨ@�@���@��@�@�~�@�=q@�@��^@��@��@�x�@�X@�%@���@��9@�b@��
@�K�@�v�@��@��@���@��^@��@�O�@��@�X@��@��7@���@��@��D@� �@�  @��;@���@�|�@�;d@��@���@��+@�^5@�-@���@�@���@���@��
@�|�@�"�@�
=@��@��H@��@�ȴ@��R@��\@��@�X@�O�@�?}@�V@��/@��u@���@�(�@��w@��w@�t�@��@�S�@�"�@�o@�"�@��@��@���@�5?@��@��@���@��T@���@�x�@�x�@�`B@�X@�X@�XG�O�@�.�@��6@za|@n�R@h�@[t�@Rff@J4@C�
@>�1@5rG@0U2@,�u@'X�@#�@B[@w�@��@/�@�g@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBD�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BD�BB�BA�BE�BH�BH�BP�BO�BR�BP�BO�BW
B^5B^5B^5BaHBbNBbNBhsBhsBiyBq�Bq�Br�Bu�Bw�Bx�By�B|�B}�B}�B}�B|�B}�B|�B}�B~�B~�Bz�B{�Bz�By�By�Bx�Bu�Bt�Br�Bo�BjBe`BaHBYBM�BG�BC�B<jB5?B(�B#�BhB%B��B�B�fB�HB�BǮBB�3B��B��B��B�\B�JB�%B�By�Bn�BgmBcTBYB6FB�B
��B
�TB
��B
�dB
��B
��B
�hB
�PB
�+B
�B
}�B
y�B
u�B
iyB
VB
L�B
<jB
)�B
VB	�B	�B	��B	��B	ÖB	��B	��B	u�B	<jB	(�B	�B	oB	B��B�yB�/B�)B�#B�B��B��BÖB�jB�^B�FB�?B�B��B��B�uB�VB�7B�B�B�B|�By�Bw�Bv�Br�Bq�Bo�Bn�Bk�BffBdZBbNB`BBbNBbNBjBl�Bl�Bm�Bm�Bm�Bm�Bm�Bm�Bn�Bk�BjBhsBgmBe`Be`BcTBbNBaHB_;B\)BXBVBXBT�BS�BQ�BO�BN�BG�BC�B@�B?}BB�BB�B@�BA�BB�BA�B?}B@�B>wB?}B=qB=qB;dB;dB:^B;dB9XB;dB8RB8RB8RB8RB7LB7LB6FB6FB6FB7LB6FB7LB7LB8RB9XB9XB9XB8RB8RB8RB8RB8RB<jB;dB;dB;dB:^B:^B=qB<jB=qB>wB=qB>wB@�B@�BB�BE�BF�BG�BH�BH�BI�BJ�BL�BM�BM�BP�BT�BYBZBZBZB[#B]/B_;BbNBcTBe`Be`BffBffBiyBjBjBm�Bp�Br�Bs�Bs�Bt�Bu�B{�B{�B{�B|�B|�B~�B�B�B�+B�7B�PB�\B�bB�hB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�-B�9B�FB�RB�XB�dB�jB�jB�qBB��B��B��B�#B�HB�TB�ZB�yB�B�B��B	  B	
=B	PB	{B	�B	�B	�B	�B	�B	PB	VB	uB	�B	&�B	-B	0!B	33B	1'B	.B	/B	/B	0!B	2-B	7LB	9XB	:^B	=qB	C�B	E�B	F�B	F�B	F�B	I�B	J�B	M�B	N�B	N�B	P�B	R�B	Q�B	Q�B	Q�B	T�B	VB	W
B	XB	W
B	R�B	R�B	S�B	XB	^5B	e`B	gmB	hsB	jB	k�B	m�B	o�B	p�B	r�B	t�B	u�B	w�B	z�B	|�B	|�B	{�B	z�B	{�B	|�B	|�B	}�B	�B	�B	�1B	�VB	�hB	�uB	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�3B	�3B	�9B	�?B	�?B	�FB	�FB	�FB	�LB	�XB	�jB	�jB	�qB	�qB	�wB	�wB	�}B	B	ÖB	ĜB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�)B	�/B	�5B	�5B	�5B	�RB	��B	�HB
DB
:B
!HB
&LB
0!B
9�B
?}B
E�B
N"B
R�B
WYB
Y�B
_B
d�B
l"B
p!B
s�B
x11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B<B= B= B= B= B= B= B= B= B= B= B= B= B= B= B= B= B= B= B= B= B= B= B= B= B<B:B9B= B@3B@3BHdBG^BJqBHeBG_BN�BU�BU�BU�BX�BY�BY�B_�B_�B`�Bi*Bi*Bj0BmCBoOBpUBq[BtnButButButBtnButBtoBuuBv{Bv{BrbBshBrcBq]Bq]BpWBmEBl>Bj3Bg!BbB\�BX�BP�BEYB?4B;B3�B,�B B`B�B��B�IB�+B��B��BЦB�>B� B��B�dB�LB�'B��B��B}�Bx�BqrBf/B_BZ�BP�B-�BB
�B
��B
ǃB
�
B
�~B
�HB
�B
��B
~�B
x�B
u�B
q�B
mnB
a%B
M�B
D|B
4B
!�B
	B	�YB	��B	ąB	ɤB	�OB	��B	�hB	m�B	4-B	 �B	rB	
6B��B�B�CB��B��B��B��BɸBB�dB�8B�-B�B�B��B��B�qB�GB�)B�
B|�By�Bx�Bt�Bq�Bo�Bn�Bj�Bi�BgtBfnBc\B^=B\2BZ&BXBZ&BZ'BbWBdcBdcBejBejBejBejBejBejBfqBc^BbYB`MB_GB]:B]:B[/BZ)BY#BWBTBO�BM�BO�BL�BK�BI�BG�BF�B?�B;tB8bB7\B:nB:nB8bB9hB:nB9hB7]B8cB6WB7]B5QB5QB3EB3EB2?B3EB19B3EB04B04B04B04B/.B/.B.(B.)B.)B//B.)B//B//B05B1;B1;B1;B05B06B06B06B06B4NB3HB3HB3HB2BB2BB5UB4NB5UB6[B5UB6[B8gB8gB:sB=�B>�B?�B@�B@�BA�BB�BD�BE�BE�BH�BL�BP�BRBRBRBSBUBWBZ2B[8B]DB]DB^JB^JBa\BbbBbbBetBh�Bj�Bk�Bk�Bl�Bm�Bs�Bs�Bs�Bt�Bt�Bv�By�B{�BB�B�2B�>B�DB�JB�iB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�&B�2B�8B�DB�JB�JB�QB�oBĬB��B��B�B�&B�2B�8B�WB�B�B��B��B	B	+B	VB	hB	{B	�B	�B	{B	,B	2B	QB	nB	�B	$�B	'�B	+B	)B	%�B	&�B	&�B	'�B	*B	/%B	11B	27B	5JB	;oB	=zB	>�B	>�B	>�B	A�B	B�B	E�B	F�B	F�B	H�B	J�B	I�B	I�B	I�B	L�B	M�B	N�B	O�B	N�B	J�B	J�B	K�B	O�B	VB	]7B	_DB	`JB	bVB	c\B	ehB	guB	hzB	j�B	l�B	m�B	o�B	r�B	t�B	t�B	s�B	r�B	s�B	t�B	t�B	u�B	x�B	{�B	�B	�+B	�=B	�JB	�PB	�PB	�PB	�PB	�VB	�nB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�=B	�=B	�DB	�DB	�JB	�JB	�OB	�aB	�hB	�nB	��B	��B	B	ÙB	ÙB	ğB	ťB	ǱB	ȷB	ɽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�G�O�B	�"B	�B	�B
B

	B
B
B
'�B
1�B
7JB
=oB
E�B
J�B
O%B
Q�B
V�B
\uB
c�B
g�B
k�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.26 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144142022020411441420220204114414  AO  ARCAADJP                                                                    20200619170901    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170901  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170901  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114414  IP                  G�O�G�O�G�O�                