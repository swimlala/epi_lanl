CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:47Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143433  20190522121828  1727_5046_201                   2C  D   APEX                            2143                            040306                          846 @�_�;@1   @�`m�?�@7M�hr�!�c�A�7K�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   B   @333@�  @�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B7��B@  BHffBPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp  Cr  Ct  Cv  Cw�fCy�fC|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D��Dy�D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)y�D*  D*� D+fD+� D,  D,� D-  D-� D.fD.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D3��D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do�fDp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy��D�@ D�ffD�� D�� D�)�D�ffD���D��fD�6fD�\�D��fD���D�#3D�S3Dڙ�D�� D�0 D�ffD� D�ɚ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@9��@�33@�33A��A!��A@  Aa��A���A���A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8  B@ffBH��BP��BXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl33Cn�Cp�Cr�Ct�Cv�Cx  Cz  C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fD  D� DfD�fDfD�fDfD�fD�D�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)� D*fD*�fD+�D+�fD,fD,�fD-fD-�fD.�D.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4  D4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo��DpfDp�fDqfDq�fDrfDr�fDsfDs�fDy� D�C3D�i�D��3D��3D�,�D�i�D�� D��D�9�D�` D���D�� D�&fD�VfDڜ�D��3D�33D�i�D�3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A�A�A�A�A�A�A�A�A�A�%A�%A�%A�A�A�A�%A�A���A�  A���A���A���A���A���A�JA�1A�%A�%A�1A�%A�A���A���A���A�
=A�VA�
=A�
=A�JA�
=A���A���A��A���A��A��A���A�ĜA���A�9XA���A��A�z�A��HA���A�bA�/A��
A��A�r�A��A�z�A�  A���A��!A��mA�C�A�5?A��A��-A��A�Q�A�bA���A�\)A���A���A�dZA�A���A��A���A��DA���A�I�A�
=A��^A�ĜA��A��A���A�A�"�A�G�A���A���A�;dA��A�C�A���A��A��A�S�A�(�A��;A�"�A�ffA�ƨA��^A��9A�\)A��A�XA�hsA~�A|�jA{�AzbAx{Av��AuG�As"�Ao�AmVAkt�Ai`BAhAf~�AeG�A`ffA]p�A\jA[�#AZ�!AY�AX�`AVĜAU33ATz�AS�;AS7LAQS�AP^5AO�AM/AH��AF-AD��AChsAB��AA�hA?�;A>=qA=/A<�uA;`BA:�A:1'A8��A7�-A6ȴA5�A4E�A3�A3A1��A0bA.�!A-��A,Q�A+�A*n�A)�
A(n�A'�A'��A&��A%�TA$�!A$�A#��A!�A�;A"�A�/Ap�Al�A �A��A{A�;A�Al�AG�A�yAĜA�uA(�A��A�AbNAƨA7LA�DA1'A�A�AhsA�RA1'A;dA��A�yA��A��A��AI�A�hA
ĜA
(�A	p�A	�A��A��A�A�!A�#AhsA�AhsA ��@��;@�r�@�|�@��@��@��@�9X@��w@���@��j@�@�%@�
=@��#@�`B@�j@��;@�n�@陚@���@�r�@��@�u@�F@�l�@◍@�`B@�1'@�\)@��T@�1@ڧ�@��@�ff@���@��/@��
@�33@���@��@պ^@�/@�  @�\)@��@���@���@ҟ�@�Z@Ο�@·+@�@� �@��@�E�@�Ĝ@�I�@��;@�V@�O�@�/@�V@��`@ă@öF@�33@�Ĝ@��
@�K�@�"�@��R@��@� �@�@��H@���@��@�1@�+@��+@�=q@���@�z�@�l�@��@���@��@��@�b@��F@�\)@�^5@�p�@��@��j@��@�j@�  @��@�\)@���@���@�=q@��^@�p�@�X@�/@���@��@��@���@��D@� �@��@�1@��
@�"�@�n�@��@�7L@���@��@�bN@�I�@�1@��F@���@�S�@�+@�o@��y@�ȴ@�n�@��@���@���@�O�@���@�r�@�r�@�I�@�1@��@�
=@���@���@�ff@�E�@�$�@���@���@��7@�7L@�V@���@��u@�9X@��;@��w@�|�@���@�^5@��@��#@�p�@�O�@�/@��@�%@��`@��9@��u@�j@�Q�@�A�@�9X@�1@��F@��H@��@�n�@��@��7@��@�x�@�X@��@��@���@��9@�A�@��@�  @��@���@�dZ@�C�@��@��H@�n�@��@�p�@��j@�  @�|�@�\)@�"�@��@�ff@�@���@���@���@��-@��@�?}@�?}@�G�@�?}@�/@��j@��@���@��P@�t�@�\)@�"�@��H@���@�~�@�@���@�/@���@���@�j@�I�@��@��@�Q�@��u@��j@��/@��`@��@��`@���@�I�@� �@��m@��w@�|�@�C�@���@�C�@��@w�P@ol�@g�w@b^5@Y��@M��@E�@=��@7�@1��@+�
@%�T@!�@V@J@��@��@9X@
M�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A�A�A�A�A�A�A�A�A�A�%A�%A�%A�A�A�A�%A�A���A�  A���A���A���A���A���A�JA�1A�%A�%A�1A�%A�A���A���A���A�
=A�VA�
=A�
=A�JA�
=A���A���A��A���A��A��A���A�ĜA���A�9XA���A��A�z�A��HA���A�bA�/A��
A��A�r�A��A�z�A�  A���A��!A��mA�C�A�5?A��A��-A��A�Q�A�bA���A�\)A���A���A�dZA�A���A��A���A��DA���A�I�A�
=A��^A�ĜA��A��A���A�A�"�A�G�A���A���A�;dA��A�C�A���A��A��A�S�A�(�A��;A�"�A�ffA�ƨA��^A��9A�\)A��A�XA�hsA~�A|�jA{�AzbAx{Av��AuG�As"�Ao�AmVAkt�Ai`BAhAf~�AeG�A`ffA]p�A\jA[�#AZ�!AY�AX�`AVĜAU33ATz�AS�;AS7LAQS�AP^5AO�AM/AH��AF-AD��AChsAB��AA�hA?�;A>=qA=/A<�uA;`BA:�A:1'A8��A7�-A6ȴA5�A4E�A3�A3A1��A0bA.�!A-��A,Q�A+�A*n�A)�
A(n�A'�A'��A&��A%�TA$�!A$�A#��A!�A�;A"�A�/Ap�Al�A �A��A{A�;A�Al�AG�A�yAĜA�uA(�A��A�AbNAƨA7LA�DA1'A�A�AhsA�RA1'A;dA��A�yA��A��A��AI�A�hA
ĜA
(�A	p�A	�A��A��A�A�!A�#AhsA�AhsA ��@��;@�r�@�|�@��@��@��@�9X@��w@���@��j@�@�%@�
=@��#@�`B@�j@��;@�n�@陚@���@�r�@��@�u@�F@�l�@◍@�`B@�1'@�\)@��T@�1@ڧ�@��@�ff@���@��/@��
@�33@���@��@պ^@�/@�  @�\)@��@���@���@ҟ�@�Z@Ο�@·+@�@� �@��@�E�@�Ĝ@�I�@��;@�V@�O�@�/@�V@��`@ă@öF@�33@�Ĝ@��
@�K�@�"�@��R@��@� �@�@��H@���@��@�1@�+@��+@�=q@���@�z�@�l�@��@���@��@��@�b@��F@�\)@�^5@�p�@��@��j@��@�j@�  @��@�\)@���@���@�=q@��^@�p�@�X@�/@���@��@��@���@��D@� �@��@�1@��
@�"�@�n�@��@�7L@���@��@�bN@�I�@�1@��F@���@�S�@�+@�o@��y@�ȴ@�n�@��@���@���@�O�@���@�r�@�r�@�I�@�1@��@�
=@���@���@�ff@�E�@�$�@���@���@��7@�7L@�V@���@��u@�9X@��;@��w@�|�@���@�^5@��@��#@�p�@�O�@�/@��@�%@��`@��9@��u@�j@�Q�@�A�@�9X@�1@��F@��H@��@�n�@��@��7@��@�x�@�X@��@��@���@��9@�A�@��@�  @��@���@�dZ@�C�@��@��H@�n�@��@�p�@��j@�  @�|�@�\)@�"�@��@�ff@�@���@���@���@��-@��@�?}@�?}@�G�@�?}@�/@��j@��@���@��P@�t�@�\)@�"�@��H@���@�~�@�@���@�/@���@���@�j@�I�@��@��@�Q�@��u@��j@��/@��`@��@��`@���@�I�@� �@��m@��w@�|�@�C�@���@�C�@��@w�P@ol�@g�w@b^5@Y��@M��@E�@=��@7�@1��@+�
@%�T@!�@V@J@��@��@9X@
M�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB>wB>wB>wB=qB=qB=qB=qB=qB=qB=qB=qB=qB=qB=qB=qB=qB=qB=qB>wB>wB>wB>wB>wB>wB=qB>wB>wB>wB>wB>wB>wB>wB>wB=qB=qB=qB>wB>wB>wB>wB=qB=qB<jB<jB;dB:^B:^B:^B8RB7LB5?B0!B+B �B�BuB
=BB��B�B�B�B�B�`B�5B��B��BĜB�wB�XB�?B�-B�!B�B�B��B��Bx�BhsBO�BE�BC�B:^B5?B0!B �B1B�;BǮB�XB�3B��B��B�\B�Bz�Bu�BffBO�BD�B=qB,BJB
�B
�mB
�TB
�#B
��B
��B
��B
�{B
�uB
�PB
z�B
p�B
cTB
VB
G�B
>wB
33B
%�B
�B
\B
  B	�B	�#B	��B	ÖB	�dB	�?B	�B	�DB	z�B	t�B	n�B	dZB	[#B	R�B	C�B	8RB	6FB	49B	1'B	-B	(�B	"�B	�B	B��B�B�B�yB�`B�;B�B�B��B��B��B��BȴBŢB��B�wB�dB�XB�?B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�\B�JB�JB�=B�+B�B�B|�B|�B|�B{�B{�B{�Bz�Bz�By�By�Bx�Bv�Bv�Bu�Bt�Bt�Bt�Bs�Bt�Bs�Bs�Bs�Bt�Bt�Bt�Bt�Bt�Bs�Br�Br�Bq�Bp�Bp�Bo�Bm�Bm�Bm�Bl�BjBk�Bl�Bm�Bm�Bl�Bo�Bp�Bo�Bp�Bp�Bq�Bq�Bp�Bq�Bo�Bo�Bp�Bt�B}�B}�B|�B{�Bz�By�By�Bx�Bx�Bz�Bz�Bz�Bz�B{�Bz�Bt�Bm�Bk�Bl�Bt�B}�B� B�B� B}�B}�B~�B�B�+B�7B�=B�=B�=B�7B�VB�hB�bB�bB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�'B�9B�FB�FB�FB�LB�wBBBȴB��B��B��B��B��B��B��B�B�B�NB�fB�mB�B�B�B��B��B	B	B	B	B	1B	
=B	PB	hB	�B	�B	�B	�B	�B	�B	 �B	"�B	"�B	$�B	%�B	'�B	(�B	+B	-B	1'B	6FB	8RB	<jB	>wB	A�B	A�B	C�B	D�B	F�B	G�B	H�B	I�B	J�B	K�B	K�B	L�B	O�B	Q�B	VB	W
B	ZB	]/B	_;B	aHB	cTB	cTB	ffB	iyB	l�B	n�B	n�B	n�B	p�B	r�B	t�B	v�B	w�B	x�B	z�B	|�B	}�B	~�B	~�B	�B	�B	�+B	�1B	�DB	�JB	�VB	�\B	�\B	�\B	�hB	�oB	�uB	�{B	��B	�{B	�{B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�!B	�!B	�-B	�9B	�FB	�FB	�9B	�9B	�9B	�?B	�FB	�XB	�XB	�XB	�XB	�XB	�XB	�XB	�^B	�jB	�wB	�}B	��B	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�HB	�NB	�TB	�TB	�TB	�TB	�HB	�B	��B	�B
bB
�B
�B
#�B
&�B
.B
7LB
?}B
E�B
K�B
R�B
VB
ZB
]/B
cTB
gmB
l�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B>wB>wB>wB=qB=qB=qB=qB=qB=qB=qB=qB=qB=qB=qB=qB=qB=qB=qB>wB>wB>wB>wB>wB>wB=qB>wB>wB>wB>wB>wB>wB>wB>wB=qB=qB=qB>wB>wB>wB>wB=qB=qB<jB<jB;dB:^B:^B;dB8RB8RB7LB2-B.B"�B�B�BVB+B��B��B�B�B�B�sB�`B�B��BǮBÖB�wB�LB�3B�'B�!B�B��B��B�Bp�BS�BK�BH�B<jB6FB6FB)�B�B�mB��B�dB�^B�B��B��B�7B}�By�Bm�BR�BE�BD�B5?B�B
�B
�sB
�`B
�TB
��B
��B
��B
�{B
��B
�{B
~�B
u�B
iyB
\)B
K�B
C�B
9XB
)�B
 �B
�B
DB	��B	�HB	�
B	ȴB	��B	�dB	�wB	��B	~�B	v�B	r�B	hsB	^5B	ZB	H�B	:^B	8RB	6FB	5?B	/B	+B	(�B	 �B	
=B��B��B�B�B�B�ZB�/B�B�B��B��B��B��BȴBŢBB�jB�jB�XB�FB�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�uB�VB�PB�VB�PB�1B�%B�B}�B}�B|�B|�B|�B{�B{�B{�Bz�Bz�Bz�Bx�Bw�Bv�Bu�Bu�Bu�Bu�Bv�Bu�Bv�Bu�Bt�Bt�Bt�Bu�Bt�Bu�Bu�Bs�Bs�Bq�Bp�Bp�Bp�Bo�Bo�Br�Bp�Bn�Bp�Bp�Br�Bq�Bq�Bq�Br�Br�Br�Bs�Bs�Bs�Bs�Br�Br�Bu�B~�B~�B~�B|�B{�Bz�B|�Bz�By�B{�B{�B|�B|�B}�B}�Bw�Bo�Bl�Bk�Bu�B� B�B�B�B� B~�B� B�B�1B�=B�=B�DB�DB�PB�hB�hB�bB�bB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�?B�FB�LB�RB�dB��BBĜBɺB��B��B��B��B��B��B��B�B�)B�TB�mB�sB�B�B�B��B��B	B	B	B	B		7B	DB	VB	oB	�B	�B	�B	�B	�B	�B	 �B	"�B	"�B	%�B	&�B	'�B	(�B	+B	.B	2-B	7LB	9XB	=qB	>wB	A�B	A�B	C�B	D�B	F�B	G�B	H�B	I�B	J�B	K�B	L�B	M�B	P�B	Q�B	W
B	XB	[#B	]/B	_;B	aHB	dZB	dZB	ffB	iyB	l�B	n�B	n�B	o�B	p�B	r�B	u�B	v�B	w�B	x�B	{�B	}�B	}�B	~�B	� B	�B	�B	�+B	�7B	�DB	�JB	�VB	�\B	�\B	�\B	�hB	�oB	�uB	�{B	��B	�{B	��B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�!B	�'B	�3B	�?B	�LB	�LB	�?B	�9B	�9B	�?B	�LB	�^B	�^B	�XB	�XB	�XB	�XB	�XB	�^B	�jB	�wB	�}B	��B	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�)B	�HB	�NB	�TB	�TB	�TB	�ZB	�HB	�B	��G�O�B
bB
�B
�B
#�B
&�B
.B
7LB
?}B
E�B
K�B
R�B
VB
ZB
]/B
cTB
gmB
l�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447442012010314474420120103144744  AO  ARGQ                                                                        20111130143433  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143433  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144744  IP                  G�O�G�O�G�O�                