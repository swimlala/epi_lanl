CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:34Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142635  20190522121827  1727_5046_153                   2C  D   APEX                            2143                            040306                          846 @��l�� 1   @��m:� @5�����c��7Kƨ1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&y�D&��D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DDy�DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DLfDL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� DefDe� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy� D�33D�i�D��3D��D�#3D�VfD��fD��3D�)�D�P D��3D���D�)�D�C3Dڳ3D��fD�)�D�ffD�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�33A��A!��AA��Aa��A���A���A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF  CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C�  C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD� DfD�fDfD�fDfD�fDfD�fDfD�fD�D�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&� D'  D'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD� DEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDL�DL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDe�De�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDtfDt� Dy�fD�6fD�l�D��fD���D�&fD�Y�D���D��fD�,�D�S3D��fD�� D�,�D�FfDڶfD�ٚD�,�D�i�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�K�A�M�A�M�A�K�A�E�A�A�A�9XA�33A�7LA�A��;AȸRAȡ�AȍPA�z�A�r�A�dZA�`BA�ZA�K�A�A�ȴA�A�K�A���A���A��A�^5A��;A��+A�33A��DA�5?A�JA���A���A�p�A�$�A��`A��9A�33A���A��A�S�A�9XA��^A�O�A���A��A�A�A��TA���A�-A���A��A�=qA���A��A��9A�jA�E�A��A���A���A�dZA��A��A���A��A��A���A��;A��#A�oA�I�A�VA��hA��A��A��hA��TA�x�A�M�A�5?A�"�A��PA��TA��A�?}A���A�E�A�(�A���A��yA�?}A��PA��A�(�A�dZA��mA��hA�p�A�bNA�9XA�(�A�1A���A��
A�v�A��A�I�A��FA� �A��!A�n�A�33A�l�A��\A���A���A�$�A�oA���A���A�C�A���A{|�Au�As?}Ar(�Ap�An��Ak��Ai��Ah�/Ae�Aa+A\�AZ�\AY�FAY\)AY"�AX��AW�7AU��AT�+AO%AI/AHQ�AG��AD1'ABZAA"�A@�/A@��A?�A>�DA:�jA9"�A81'A7��A7dZA6�A5�A3�7A3%A2�A2bNA2A1hsA0ĜA0VA/�A/G�A.I�A-&�A,r�A*  A(5?A'�A%�TA%p�A%G�A%
=A$ffA#�PA#A!�^A!C�A ��A�-A
=A �Ax�A�FA7LA��A�;A��A5?A33A��A�;AdZA�A$�A7LA��AK�A��A��A�hA
�A
�A	p�Ar�AG�A��A~�A5?A�Az�AjAn�An�An�AZA-Ax�A ��A M�@��m@�bN@��H@�{@���@��@��w@���@�Q�@��@�/@�C�@陚@�1'@�R@�-@�O�@�F@��@�|�@ޏ\@��@�p�@�@�@�G�@ܬ@�9X@ۍP@���@ڏ\@���@�V@�l�@Ձ@�9X@�dZ@ҸR@��@ёh@���@�~�@�&�@�@�J@�{@�{@��@̣�@��y@�ff@���@ɑh@Ɂ@�&�@�(�@�dZ@�x�@�|�@�ȴ@�M�@���@���@�A�@�n�@�O�@�7L@�O�@�%@�z�@��
@�
=@�~�@��-@�z�@�\)@�v�@��T@�~�@�J@���@��F@�^5@��@��h@�7L@���@�1'@��@���@�M�@��@�/@�9X@�Q�@�A�@�I�@�1'@�K�@��H@���@���@���@��R@���@�v�@�{@�p�@�%@��/@��9@���@�z�@�Z@�9X@� �@�b@��m@��7@�K�@�+@��H@��R@�n�@�J@��@���@��@�X@�Ĝ@��u@�(�@��@�
=@��@���@��!@���@��+@�ff@�-@���@�X@�O�@�/@�V@��`@���@�z�@�9X@�b@��F@�t�@��@�1@���@�C�@�v�@�J@��T@�`B@���@��@���@�Z@�r�@��@��/@���@��-@��-@��u@�|�@���@�ƨ@��@��m@��
@��
@��F@�33@���@�@��7@��@�J@�E�@�-@�@���@��^@�hs@�%@��j@���@��;@���@��@�|�@�dZ@�33@��@��+@�ff@�=q@�$�@��@��^@�`B@�&�@��@���@��D@�A�@�(�@�b@�  @���@���@��@�t�@�K�@�$�@�1@�l�@���@�v�@�^5@��#@��-@�`B@��@��@�&�@�O�@�?}@�?}@�7L@���@�b@��w@��H@���@��\@�ff@�M�@�J@�$�@�=q@�E�@��@�@��@�&�@��9@�A�@���@���@~�y@{dZ@r�!@j�@d�D@\�@S�m@JJ@Co@;C�@5?}@1�@*n�@$��@!��@?}@  @dZ@{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�K�A�M�A�M�A�K�A�E�A�A�A�9XA�33A�7LA�A��;AȸRAȡ�AȍPA�z�A�r�A�dZA�`BA�ZA�K�A�A�ȴA�A�K�A���A���A��A�^5A��;A��+A�33A��DA�5?A�JA���A���A�p�A�$�A��`A��9A�33A���A��A�S�A�9XA��^A�O�A���A��A�A�A��TA���A�-A���A��A�=qA���A��A��9A�jA�E�A��A���A���A�dZA��A��A���A��A��A���A��;A��#A�oA�I�A�VA��hA��A��A��hA��TA�x�A�M�A�5?A�"�A��PA��TA��A�?}A���A�E�A�(�A���A��yA�?}A��PA��A�(�A�dZA��mA��hA�p�A�bNA�9XA�(�A�1A���A��
A�v�A��A�I�A��FA� �A��!A�n�A�33A�l�A��\A���A���A�$�A�oA���A���A�C�A���A{|�Au�As?}Ar(�Ap�An��Ak��Ai��Ah�/Ae�Aa+A\�AZ�\AY�FAY\)AY"�AX��AW�7AU��AT�+AO%AI/AHQ�AG��AD1'ABZAA"�A@�/A@��A?�A>�DA:�jA9"�A81'A7��A7dZA6�A5�A3�7A3%A2�A2bNA2A1hsA0ĜA0VA/�A/G�A.I�A-&�A,r�A*  A(5?A'�A%�TA%p�A%G�A%
=A$ffA#�PA#A!�^A!C�A ��A�-A
=A �Ax�A�FA7LA��A�;A��A5?A33A��A�;AdZA�A$�A7LA��AK�A��A��A�hA
�A
�A	p�Ar�AG�A��A~�A5?A�Az�AjAn�An�An�AZA-Ax�A ��A M�@��m@�bN@��H@�{@���@��@��w@���@�Q�@��@�/@�C�@陚@�1'@�R@�-@�O�@�F@��@�|�@ޏ\@��@�p�@�@�@�G�@ܬ@�9X@ۍP@���@ڏ\@���@�V@�l�@Ձ@�9X@�dZ@ҸR@��@ёh@���@�~�@�&�@�@�J@�{@�{@��@̣�@��y@�ff@���@ɑh@Ɂ@�&�@�(�@�dZ@�x�@�|�@�ȴ@�M�@���@���@�A�@�n�@�O�@�7L@�O�@�%@�z�@��
@�
=@�~�@��-@�z�@�\)@�v�@��T@�~�@�J@���@��F@�^5@��@��h@�7L@���@�1'@��@���@�M�@��@�/@�9X@�Q�@�A�@�I�@�1'@�K�@��H@���@���@���@��R@���@�v�@�{@�p�@�%@��/@��9@���@�z�@�Z@�9X@� �@�b@��m@��7@�K�@�+@��H@��R@�n�@�J@��@���@��@�X@�Ĝ@��u@�(�@��@�
=@��@���@��!@���@��+@�ff@�-@���@�X@�O�@�/@�V@��`@���@�z�@�9X@�b@��F@�t�@��@�1@���@�C�@�v�@�J@��T@�`B@���@��@���@�Z@�r�@��@��/@���@��-@��-@��u@�|�@���@�ƨ@��@��m@��
@��
@��F@�33@���@�@��7@��@�J@�E�@�-@�@���@��^@�hs@�%@��j@���@��;@���@��@�|�@�dZ@�33@��@��+@�ff@�=q@�$�@��@��^@�`B@�&�@��@���@��D@�A�@�(�@�b@�  @���@���@��@�t�@�K�@�$�@�1@�l�@���@�v�@�^5@��#@��-@�`B@��@��@�&�@�O�@�?}@�?}@�7L@���@�b@��w@��H@���@��\@�ff@�M�@�J@�$�@�=q@�E�@��@�@��@�&�@��9@�A�@���@���@~�y@{dZ@r�!@j�@d�D@\�@S�m@JJ@Co@;C�@5?}@1�@*n�@$��@!��@?}@  @dZ@{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BƨBȴBɺB��B�HB�yB�B�B��BB�BQ�B]/BhsBk�BcTBiyBn�Bu�By�B�B�1B�VB��B��B��B��B��B��B��B��B�B�B��B�B��B�^B�3B��B��B��B��B�DB�%B^5B5?BI�B^5B�B�bB��B��B��B�By�Bn�BQ�B?}BuB  B��B%B%BBB��B�B�fB�HB�)B��B��B��B�}B�FB�B��B�=B|�BiyBcTBaHB_;B\)B[#BYBW
BS�BM�BD�B9XB/BuB
��B
��B
�B
�ZB
��B
ƨB
�XB
�B
��B
�+B
r�B
e`B
E�B
�B
B	��B	�B	�yB	�HB	��B	ŢB	�XB	��B	v�B	^5B	XB	W
B	T�B	S�B	P�B	D�B	5?B	$�B	B�HB�B��B��B��B��B�B�B�B��B�LB�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�uB�oB�bB�VB�DB�=B�7B�1B�1B�%B�+B�B�1B�JB�PB�bB�oB�hB�=B�B�B� B~�B~�B{�Bx�Bu�Bm�BffBgmBffBffBe`BbNB_;B^5B]/BZBQ�BQ�BR�BS�BS�BS�BS�BS�BT�BT�BS�BQ�BP�BP�BO�BO�BN�BL�BI�BK�BI�BH�BH�BH�BF�BH�BH�BG�BG�BH�BJ�BJ�BS�B`BBcTBffBjBo�Bp�Br�Bt�Bt�Bv�Bu�Bw�B|�B�B�1B�DB�JB�VB�=B�=B�7B�bB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�!B�3B�LB�XB�wBĜBǮBƨBŢB��B��B��B��B�B�
B�5B�ZB�fB�mB�mB�B��B��B��B��B	B	B	1B	DB	PB	VB	bB	hB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	#�B	$�B	&�B	-B	.B	1'B	8RB	<jB	=qB	=qB	>wB	>wB	>wB	@�B	B�B	I�B	L�B	L�B	L�B	N�B	O�B	P�B	Q�B	S�B	T�B	YB	[#B	_;B	dZB	ffB	gmB	ffB	ffB	ffB	ffB	iyB	p�B	s�B	v�B	w�B	z�B	� B	�1B	�7B	�JB	�DB	�JB	�\B	�bB	�oB	�oB	�uB	�oB	�{B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�3B	�3B	�9B	�?B	�RB	�dB	�jB	�jB	�qB	�qB	�wB	�}B	��B	ÖB	ÖB	ŢB	ƨB	ǮB	ǮB	ǮB	ɺB	��B	��B	��B	ɺB	ŢB	��B	��B	��B	��B	ĜB	ĜB	ĜB	ĜB	ŢB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�)B	�mB	��B
B
VB
bB
'�B
+B
2-B
:^B
B�B
L�B
P�B
W
B
[#B
_;B
bNB
hsB
k�B
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B�NB�B�B�B��B%B�BR�B^5BiyBp�BjBjBo�Bv�B|�B�B�=B�bB��B��B��B��B��B��B��B��B�B�-B��B�B��B�qB�?B��B��B��B��B�DB�JBffB8RBJ�B]/B�B�oB��B��B��B�1B� Bx�BW
BJ�B�BB��B+B%BBB��B�B�yB�ZB�5B�B��B��BB�^B�-B��B�VB�Bk�BdZBaHB`BB\)B\)BYBXBVBP�BG�B=qB8RB�B
��B
��B
��B
�yB
�B
��B
�qB
�9B
��B
�JB
t�B
m�B
Q�B
'�B
B	��B	��B	�B	�yB	��B	ǮB	��B	��B	�B	cTB	ZB	XB	VB	T�B	T�B	I�B	9XB	49B	oB�ZB�/B�5B�B�B��B�B�#B�BB�#B�jB�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�uB�bB�\B�JB�DB�DB�=B�7B�7B�=B�=B�VB�bB�{B�{B��B�hB�+B�B�B�B�B�Bz�Bz�Bq�BjBk�BhsBiyBhsBffBaHB_;B^5B_;BVBQ�BR�BS�BS�BS�BT�BVBW
BVBT�BW
BR�BQ�BQ�BP�BO�BP�BP�BL�BM�BK�BK�BJ�BH�BJ�BI�BJ�BJ�BL�BL�BL�BS�B`BBcTBgmBk�Bp�Bq�Bs�Bu�Bu�Bw�Bx�Bz�B~�B�B�7B�JB�PB�hB�JB�JB�1B�bB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�B�B�B�B�!B�-B�?B�RB�^B�wBŢBɺBȴBǮB��B��B��B�B�B�B�;B�`B�mB�sB�yB�B��B��B��B��B	B	B	1B	DB	PB	VB	bB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	#�B	$�B	'�B	-B	/B	2-B	9XB	<jB	=qB	=qB	>wB	>wB	>wB	@�B	C�B	I�B	L�B	L�B	L�B	N�B	O�B	P�B	Q�B	S�B	VB	YB	[#B	_;B	dZB	gmB	hsB	gmB	ffB	gmB	ffB	hsB	r�B	s�B	v�B	w�B	z�B	~�B	�1B	�7B	�VB	�PB	�JB	�\B	�bB	�oB	�oB	�uB	�oB	��B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�3B	�3B	�9B	�FB	�RB	�dB	�jB	�jB	�qB	�qB	�}B	�}B	��B	ÖB	ÖB	ŢB	ƨB	ǮB	ǮB	ǮB	ɺB	��B	��B	��B	��B	ȴB	��B	��B	��B	��B	ŢB	ĜB	ŢB	ĜB	ŢB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	��B	��B	�)B	�mB	��B
B
VB
bB
'�B
+B
2-B
:^B
B�B
K�B
P�B
W
B
[#B
_;B
bNB
gmB
k�B
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<D��<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447272012010314472720120103144727  AO  ARGQ                                                                        20111130142635  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142635  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144727  IP                  G�O�G�O�G�O�                