CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:32Z UW 3.1 conversion   
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
_FillValue                 �  Kl   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mh   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  UP   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  z�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �\   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �l   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �p   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142514  20190522121827  1727_5046_147                   2C  D   APEX                            2143                            040306                          846 @�Ӫ"� 1   @�Ӫ���@6Qhr� ��c�M���1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�fC  C  C   C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D ��D!y�D!��D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� DrfDr� Ds  Ds� Dy�3D�)�D�i�D���D��3D�&fD�s3D��3D��3D�,�D�Y�D���D���D��D�` Dڜ�D���D�)�D�VfD�|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@�33A��A!��AA��Aa��A���A���A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C�C�C�C�C
�C�C�C�C�C�C�C�C  C�C�C �C"33C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C�  C�  C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!  D!� D"  D"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDr�Dr�fDsfDs�fDyٚD�,�D�l�D���D��fD�)�D�vfD��fD��fD�0 D�\�D���D�� D�  D�c3Dڠ D�� D�,�D�Y�D� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�=qA�-A�A�A���A�~�Aŕ�Aŧ�AŅA�7LA��Aģ�A�dZA�C�A�&�A�JA���A�bNA�E�A���A�JA�~�A�A���A�t�A�  A��A��
A�C�A��\A�ƨA���A���A�bNA�/A�{A��
A� �A��!A�"�A��!A��^A��A���A���A��A�{A��#A��uA�~�A��DA��jA�JA���A�A�I�A��DA�&�A�\)A���A��#A�A�z�A�33A��A��+A�A���A���A�VA���A�ƨA���A�z�A�O�A��A���A�|�A��9A�ȴA��A�(�A�|�A��\A���A�E�A�Q�A�I�A���A�+A��wA�ffA�-A��DA�p�A��A��^A�G�A��A���A�jA�|�A�ffA�"�A��A�9XA�A~�jA}%A{l�Az1Aw�hAuS�AtE�AqdZAq?}Ap�RApQ�Ap{Ao�mAodZAl�uAc&�A`I�A_�A_��A^-AZ��AY|�AXv�AW\)AV��AU��AU%ATbNAR�AQ?}AP��AO�AN�HANAMt�AM�AL�AJ�`AJJAI
=AH9XAF��AE�7AE&�AD��ADjAC�hABI�A@r�A?�TA?oA=�FA:9XA9"�A8�yA8�!A8M�A7;dA5�A5VA3/A1��A/l�A.�A.^5A.JA-S�A,jA)�PA(n�A'��A%hsA#�PA#`BA"�DA ZAbA�;A�AXA��A7LAJA+A$�A�wA��AO�A��A�jAffA�#A;dA�An�A��An�AbA�^AI�A�A\)AG�A
=A	7LA��A��A��A��A��A?}AAȴA�A E�@�$�@��`@�bN@�t�@��y@��@���@���@�C�@��@�@��#@� �@�~�@�x�@���@���@�X@��@�%@��@�G�@�@�/@�@��@�=q@�V@��u@��m@�M�@��`@ە�@�?}@���@�z�@�33@���@�bN@Ӿw@�C�@�@ѡ�@��@�9X@��@͡�@�p�@���@�1@�|�@�dZ@���@�E�@�J@���@ɑh@�X@ȴ9@ȋD@�z�@�j@���@�ȴ@��@�&�@ă@�(�@���@�^5@��j@� �@�/@�X@��7@���@�X@��/@��j@�I�@��;@���@�K�@��@�o@�
=@��H@�M�@��u@���@��R@�?}@�r�@�9X@�  @��;@��F@�C�@���@�=q@��@��@�(�@���@���@�n�@��@��^@�X@�O�@��@�Ĝ@��@��m@�;d@���@��@��-@���@��@��/@��@���@��9@���@��@��F@���@�v�@��^@���@�A�@��;@��@�\)@�"�@�
=@��y@�ȴ@��R@���@�{@�?}@�%@��@��/@���@��9@���@�r�@� �@��@���@�ff@�{@���@��-@���@���@��h@��@�&�@���@�A�@�1@���@�  @��@�\)@��y@���@�5?@�x�@�&�@��@��@�A�@�(�@�(�@� �@� �@� �@��@��;@�33@��@�v�@�@��h@�G�@�%@���@��;@��H@��@�ȴ@���@���@�~�@�~�@���@�-@��T@��#@���@��@�`B@�?}@��j@��;@���@���@���@���@���@��7@��T@��@��#@�x�@�x�@�O�@�7L@�/@��D@�1'@�1@���@���@���@�ƨ@�ƨ@��w@��F@��F@���@�dZ@���@��h@�X@��j@�A�@�I�@�I�@�1'@���@�
=@���@�^5@�E�@�$�@��@��@�O�@��@��/@�j@�Z@�A�@��@�ƨ@��@��H@�J@���@���@z�@o�@eV@]�@S�@Ihs@C��@>$�@7l�@0��@*~�@&E�@"�@=q@��@��@ȴ@�u@`B11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�=qA�-A�A�A���A�~�Aŕ�Aŧ�AŅA�7LA��Aģ�A�dZA�C�A�&�A�JA���A�bNA�E�A���A�JA�~�A�A���A�t�A�  A��A��
A�C�A��\A�ƨA���A���A�bNA�/A�{A��
A� �A��!A�"�A��!A��^A��A���A���A��A�{A��#A��uA�~�A��DA��jA�JA���A�A�I�A��DA�&�A�\)A���A��#A�A�z�A�33A��A��+A�A���A���A�VA���A�ƨA���A�z�A�O�A��A���A�|�A��9A�ȴA��A�(�A�|�A��\A���A�E�A�Q�A�I�A���A�+A��wA�ffA�-A��DA�p�A��A��^A�G�A��A���A�jA�|�A�ffA�"�A��A�9XA�A~�jA}%A{l�Az1Aw�hAuS�AtE�AqdZAq?}Ap�RApQ�Ap{Ao�mAodZAl�uAc&�A`I�A_�A_��A^-AZ��AY|�AXv�AW\)AV��AU��AU%ATbNAR�AQ?}AP��AO�AN�HANAMt�AM�AL�AJ�`AJJAI
=AH9XAF��AE�7AE&�AD��ADjAC�hABI�A@r�A?�TA?oA=�FA:9XA9"�A8�yA8�!A8M�A7;dA5�A5VA3/A1��A/l�A.�A.^5A.JA-S�A,jA)�PA(n�A'��A%hsA#�PA#`BA"�DA ZAbA�;A�AXA��A7LAJA+A$�A�wA��AO�A��A�jAffA�#A;dA�An�A��An�AbA�^AI�A�A\)AG�A
=A	7LA��A��A��A��A��A?}AAȴA�A E�@�$�@��`@�bN@�t�@��y@��@���@���@�C�@��@�@��#@� �@�~�@�x�@���@���@�X@��@�%@��@�G�@�@�/@�@��@�=q@�V@��u@��m@�M�@��`@ە�@�?}@���@�z�@�33@���@�bN@Ӿw@�C�@�@ѡ�@��@�9X@��@͡�@�p�@���@�1@�|�@�dZ@���@�E�@�J@���@ɑh@�X@ȴ9@ȋD@�z�@�j@���@�ȴ@��@�&�@ă@�(�@���@�^5@��j@� �@�/@�X@��7@���@�X@��/@��j@�I�@��;@���@�K�@��@�o@�
=@��H@�M�@��u@���@��R@�?}@�r�@�9X@�  @��;@��F@�C�@���@�=q@��@��@�(�@���@���@�n�@��@��^@�X@�O�@��@�Ĝ@��@��m@�;d@���@��@��-@���@��@��/@��@���@��9@���@��@��F@���@�v�@��^@���@�A�@��;@��@�\)@�"�@�
=@��y@�ȴ@��R@���@�{@�?}@�%@��@��/@���@��9@���@�r�@� �@��@���@�ff@�{@���@��-@���@���@��h@��@�&�@���@�A�@�1@���@�  @��@�\)@��y@���@�5?@�x�@�&�@��@��@�A�@�(�@�(�@� �@� �@� �@��@��;@�33@��@�v�@�@��h@�G�@�%@���@��;@��H@��@�ȴ@���@���@�~�@�~�@���@�-@��T@��#@���@��@�`B@�?}@��j@��;@���@���@���@���@���@��7@��T@��@��#@�x�@�x�@�O�@�7L@�/@��D@�1'@�1@���@���@���@�ƨ@�ƨ@��w@��F@��F@���@�dZ@���@��h@�X@��j@�A�@�I�@�I�@�1'@���@�
=@���@�^5@�E�@�$�@��@��@�O�@��@��/@�j@�Z@�A�@��@�ƨ@��@��H@�J@���@���@z�@o�@eV@]�@S�@Ihs@C��@>$�@7l�@0��@*~�@&E�@"�@=q@��@��@ȴ@�u@`B11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB`BBVBP�BR�B49BG�BW
B]/BaHBbNBbNBbNBbNBbNBbNBdZBdZBl�Br�Bx�B{�Bw�B�B��B��B��B�HB�B�fB(�B?}B?}B?}B+BoB��B�B�B�yB�B��B��B%B��B�B��B�TB�FB�B�}BŢB�B�\B��B�'B��B��B�LB�3B�}B�-B�B�B�B�'B�FB��B�bB�+Bz�BiyBI�B49B0!B$�BuB��B��B�qB�3B��B��B�hB�+Bz�Bk�BVBL�BE�B?}B:^B5?B(�B	7B
��B
�B
�sB
�HB
�B
�9B
�{B
�B
hsB
\)B
Q�B
L�B
C�B
9XB
-B
 �B
oB
B	��B	�B	�B	�mB	�TB	�HB	�5B	�
B	�}B	�JB	� B	{�B	x�B	m�B	]/B	e`B	y�B	~�B	~�B	{�B	w�B	u�B	l�B	dZB	aHB	^5B	YB	VB	S�B	Q�B	N�B	L�B	J�B	F�B	D�B	A�B	?}B	>wB	=qB	:^B	6FB	1'B	,B	(�B	"�B	�B	VB	DB	
=B		7B	+B	B��B��B�B�mB�5B�#B�B�B��B��BÖB�wB�RB�B��B��B��B��B��B�bB�bB�VB�DB�%B�B� B}�B}�B|�B|�B{�Bz�By�Bw�Bu�Bs�Bn�Bn�Bm�Bl�BjBiyBhsBgmBffBcTB`BB]/BZB[#B[#BZBZBYBW
BS�BVBVBVBT�BVBVBT�BS�BQ�BO�BN�BR�BO�BO�BP�BR�BS�BVB[#B_;BaHBdZBhsBn�Bn�Bm�Bp�Bv�By�By�By�Bz�B|�B{�B� B�B�7B�JB�VB�bB�bB�bB�\B�\B�VB�PB�PB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�3B�wB��B��BÖBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�)B�5B�HB�ZB�`B�sB�B�B�B�B�B��B��B	B	B	%B	%B	1B	PB	VB	bB	oB	�B	�B	�B	�B	�B	#�B	(�B	+B	-B	33B	5?B	6FB	7LB	7LB	8RB	8RB	8RB	;dB	D�B	G�B	H�B	I�B	I�B	J�B	K�B	L�B	M�B	R�B	ZB	\)B	]/B	]/B	]/B	]/B	]/B	]/B	]/B	^5B	_;B	aHB	bNB	cTB	dZB	ffB	gmB	hsB	jB	jB	m�B	o�B	p�B	q�B	t�B	u�B	u�B	v�B	v�B	v�B	v�B	x�B	~�B	~�B	�B	�B	�B	�B	�B	�B	�+B	�PB	�VB	�\B	�bB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�!B	�!B	�'B	�'B	�-B	�'B	�!B	�'B	�!B	�-B	�9B	�9B	�9B	�?B	�?B	�LB	�LB	�XB	�XB	�^B	�^B	�dB	�wB	��B	��B	ÖB	ĜB	ÖB	ÖB	ÖB	B	B	ÖB	ĜB	��B	�B	��B
\B
�B
%�B
.B
49B
9XB
A�B
H�B
P�B
T�B
YB
aHB
gmB
jB
m�B
t�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BdZB]/BR�BXB49BG�BXB^5BbNBcTBcTBbNBbNBbNBcTBffBiyBn�Bu�B~�B}�Bx�B�B��B�B��B�ZB�/B�`B(�B@�BA�BF�B1'B�B��B�B�B�B�B��B��BDB��B�B  B�sB�^B�BBȴB�?B�uB�{B�!B��B��B�^B�XBĜB�?B�B�B�B�?B�}B�3B�{B�=B�Bt�BP�B5?B33B+B�BPB�5B��B�FB�B��B�{B�=B�Bp�BYBN�BG�BA�B;dB9XB5?BhB
��B
�B
�B
�ZB
�ZB
��B
��B
�bB
o�B
bNB
S�B
P�B
I�B
>wB
1'B
'�B
�B
%B
B	�B	�B	�sB	�ZB	�HB	�;B	�/B	��B	�uB	�B	|�B	|�B	u�B	aHB	hsB	|�B	� B	�B	~�B	y�B	|�B	o�B	ffB	cTB	aHB	\)B	XB	T�B	T�B	R�B	O�B	N�B	I�B	I�B	E�B	@�B	?}B	?}B	=qB	;dB	7LB	.B	,B	'�B	%�B	oB	JB	DB	DB	DB	1B	B	B��B�B�BB�)B�#B�B�
B��BƨB��B�wB�3B�B�B��B��B��B�hB�hB�bB�\B�7B�B�B~�B}�B}�B}�B|�B{�B{�By�Bw�By�Bs�Bo�Bn�Bm�Bn�Bl�BiyBgmBgmBiyBe`BaHBaHB^5B]/B[#B[#BZB[#BZB[#BXBW
BW
BW
BVBVBVBT�BR�BP�BT�BQ�BQ�BQ�BS�BW
BXB[#B_;BaHBdZBhsBo�Bo�Bq�Bu�Bx�Bz�Bz�B|�B|�B~�B� B�B�B�DB�\B�bB�hB�hB�hB�oB�bB�\B�\B�\B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�-B�wB��B��BĜB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��B�B��B�B�B�/B�;B�NB�`B�fB�yB�B�B�B�B�B��B��B	B	%B	%B	%B		7B	PB	VB	bB	oB	�B	�B	�B	�B	 �B	$�B	)�B	,B	-B	33B	5?B	6FB	7LB	7LB	8RB	8RB	9XB	<jB	D�B	G�B	H�B	I�B	I�B	J�B	K�B	M�B	N�B	S�B	ZB	]/B	]/B	]/B	]/B	]/B	]/B	]/B	^5B	_;B	`BB	aHB	bNB	cTB	e`B	gmB	hsB	iyB	k�B	k�B	n�B	o�B	q�B	r�B	t�B	u�B	u�B	v�B	v�B	v�B	w�B	y�B	~�B	� B	�B	�B	�B	�B	�%B	�+B	�7B	�PB	�VB	�\B	�bB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�!B	�!B	�'B	�'B	�3B	�3B	�'B	�'B	�'B	�3B	�9B	�9B	�9B	�?B	�LB	�RB	�LB	�XB	�XB	�^B	�^B	�jB	�wB	��B	B	ÖB	ĜB	ĜB	ÖB	ĜB	B	ÖB	ÖB	ĜB	��B	�B	��B
\B
�B
%�B
.B
49B
9XB
A�B
H�B
P�B
T�B
YB
aHB
gmB
jB
m�B
t�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<#�
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
<D��<#�
<#�
<#�
<#�
<#�
<e`B<T��<#�
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
<��
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447252012010314472520120103144725  AO  ARGQ                                                                        20111130142514  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142514  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144725  IP                  G�O�G�O�G�O�                