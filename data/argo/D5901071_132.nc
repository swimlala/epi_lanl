CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:28Z UW 3.1 conversion   
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
_FillValue                 �  Kt   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mp   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  U\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _D   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g0   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �0   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �8   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �@   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142155  20190522121827  1727_5046_132                   2C  D   APEX                            2143                            040306                          846 @��FPg/�1   @��F�5�@7D���S��d�
=p�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A��A!��A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D��Dy�D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*fD*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE�fDF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQfDQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D_��D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� DhfDh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dm��Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy�3D�&fD�vfD��fD��fD��D�S3D��3D��D�,�D�VfD�� D���D�)�D�c3Dڬ�D��fD�#3D�Y�D� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�33A33A#33AA��Aa��A���A���A���A�  A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33C �C�C�C�C�C
�C�C�C  C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C033C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf  Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fD  D� D  D�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD��DfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*�D*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE��DFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQ�DQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`  D`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDh�Dh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDn  Dn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDy��D�)�D�y�D���D���D��D�VfD��fD���D�0 D�Y�D��3D�� D�,�D�ffDڰ D�ٚD�&fD�\�D�3D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��AÁA�ADA� �A���A�hsA��A�5?A���A�E�A�1'A�(�A�"�A�{A��-A���A�9XA��A��!A�/A���A��9A�ZA�C�A�5?A�(�A��A��/A��FA��!A���A��\A�r�A�XA�A���A��A��+A�p�A�dZA�?}A�(�A���A��9A���A���A���A��DA���A�  A���A���A��9A��mA�1A��A��yA���A���A���A�+A�5?A���A��-A�+A��A��FA��A��A��A��/A�jA��A���A�z�A��+A���A�hsA�`BA��hA��;A�&�A��A�XA���A��FA��A���A�9XA�1A�G�A�&�A��9A��7A���A��A�%A��PA��A��A��mA���A�(�A�{A��A��mA�bA��FA��A��PA��A��A�`BA� �A��A�dZA��`A�~�A�"�A�p�A�jA�E�A���A�n�A��hA��A~�uA}|�A{ƨAxbNAu33As�
Aq��Ap1'Anv�Ak�^Aj��Ai��Ah�\Af-Ae�Ac��Ab��Abz�Aa�A`��A_�#A^�jA]��A]+AY�-AYoAX��AW?}AU��AU"�AT�AShsAR�!ARv�AR1'AQ��AQ�AQ�AP��AP1AN�AMS�AK�AI�AI+AHI�AE�mADz�AC��AB�DA@��A=ƨA<I�A;�;A;/A:JA8�!A7�A6=qA5�PA533A3��A3"�A1�PA0ffA/+A.��A.A-/A+`BA*9XA)�7A(��A(n�A'�-A&�HA&A%7LA$JA#��A#
=A"��A"I�A!p�A ��A��A|�A��Ap�A�\A�A��Al�A�!A-A|�A��A|�AAhsA��A�hAĜA�mA��A�hA�A9XA�A
��A	�wA	hsA	"�A~�A��AC�A|�A��AE�AVAffA-A��A�#Al�A ��@��T@�I�@���@��@��w@���@�  @�&�@�o@�5?@��@�bN@�5?@���@�~�@��`@��@�^5@�X@��@��@�j@�o@�-@ݡ�@�7L@ܛ�@�9X@۝�@٩�@؛�@�S�@Ԭ@��@��H@��#@���@ύP@��H@��#@���@�j@˥�@���@�$�@�7L@ȃ@�A�@��;@�"�@�C�@�ȴ@���@��@� �@���@�V@��P@�n�@��@��w@���@�9X@�33@��y@���@���@�ƨ@�^5@���@���@�Q�@���@�p�@�33@���@�%@��#@�v�@�dZ@��!@���@�ȴ@�ȴ@�ȴ@�ȴ@�=q@�J@�&�@���@��9@� �@�1@���@�ƨ@��y@�ȴ@�ȴ@��\@�5?@��#@�`B@�Ĝ@�Q�@�  @���@�"�@�l�@�o@�^5@�{@��@�@��@���@�Ĝ@�z�@�I�@�(�@��@��
@�S�@��@��H@��H@�ff@���@�%@��@�1'@���@���@�|�@�C�@��@��\@���@��^@��7@�G�@�%@���@�Z@� �@�1@��;@��P@���@�v�@�M�@�@��@��T@�@��7@�V@��D@�j@�(�@��;@��w@�"�@��H@���@���@�X@��`@�Ĝ@��F@���@�~�@�n�@��y@��@���@���@��@�x�@��@�A�@��@��P@�@��\@�v�@�V@�{@���@�x�@�`B@�`B@�X@�G�@�?}@��@�%@��@��`@��/@���@�z�@�Q�@�(�@�1@�ƨ@�t�@�;d@�"�@��@��H@���@���@��+@�v�@�^5@�-@��@���@��@���@���@��@� �@�ƨ@�dZ@�@��H@���@�-@�$�@���@��#@���@�A�@~ff@t(�@ix�@b�@\I�@S�
@M�@G�@@bN@:��@3��@-�h@(  @!�#@E�@��@r�@j@\)@	�^111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��AÁA�ADA� �A���A�hsA��A�5?A���A�E�A�1'A�(�A�"�A�{A��-A���A�9XA��A��!A�/A���A��9A�ZA�C�A�5?A�(�A��A��/A��FA��!A���A��\A�r�A�XA�A���A��A��+A�p�A�dZA�?}A�(�A���A��9A���A���A���A��DA���A�  A���A���A��9A��mA�1A��A��yA���A���A���A�+A�5?A���A��-A�+A��A��FA��A��A��A��/A�jA��A���A�z�A��+A���A�hsA�`BA��hA��;A�&�A��A�XA���A��FA��A���A�9XA�1A�G�A�&�A��9A��7A���A��A�%A��PA��A��A��mA���A�(�A�{A��A��mA�bA��FA��A��PA��A��A�`BA� �A��A�dZA��`A�~�A�"�A�p�A�jA�E�A���A�n�A��hA��A~�uA}|�A{ƨAxbNAu33As�
Aq��Ap1'Anv�Ak�^Aj��Ai��Ah�\Af-Ae�Ac��Ab��Abz�Aa�A`��A_�#A^�jA]��A]+AY�-AYoAX��AW?}AU��AU"�AT�AShsAR�!ARv�AR1'AQ��AQ�AQ�AP��AP1AN�AMS�AK�AI�AI+AHI�AE�mADz�AC��AB�DA@��A=ƨA<I�A;�;A;/A:JA8�!A7�A6=qA5�PA533A3��A3"�A1�PA0ffA/+A.��A.A-/A+`BA*9XA)�7A(��A(n�A'�-A&�HA&A%7LA$JA#��A#
=A"��A"I�A!p�A ��A��A|�A��Ap�A�\A�A��Al�A�!A-A|�A��A|�AAhsA��A�hAĜA�mA��A�hA�A9XA�A
��A	�wA	hsA	"�A~�A��AC�A|�A��AE�AVAffA-A��A�#Al�A ��@��T@�I�@���@��@��w@���@�  @�&�@�o@�5?@��@�bN@�5?@���@�~�@��`@��@�^5@�X@��@��@�j@�o@�-@ݡ�@�7L@ܛ�@�9X@۝�@٩�@؛�@�S�@Ԭ@��@��H@��#@���@ύP@��H@��#@���@�j@˥�@���@�$�@�7L@ȃ@�A�@��;@�"�@�C�@�ȴ@���@��@� �@���@�V@��P@�n�@��@��w@���@�9X@�33@��y@���@���@�ƨ@�^5@���@���@�Q�@���@�p�@�33@���@�%@��#@�v�@�dZ@��!@���@�ȴ@�ȴ@�ȴ@�ȴ@�=q@�J@�&�@���@��9@� �@�1@���@�ƨ@��y@�ȴ@�ȴ@��\@�5?@��#@�`B@�Ĝ@�Q�@�  @���@�"�@�l�@�o@�^5@�{@��@�@��@���@�Ĝ@�z�@�I�@�(�@��@��
@�S�@��@��H@��H@�ff@���@�%@��@�1'@���@���@�|�@�C�@��@��\@���@��^@��7@�G�@�%@���@�Z@� �@�1@��;@��P@���@�v�@�M�@�@��@��T@�@��7@�V@��D@�j@�(�@��;@��w@�"�@��H@���@���@�X@��`@�Ĝ@��F@���@�~�@�n�@��y@��@���@���@��@�x�@��@�A�@��@��P@�@��\@�v�@�V@�{@���@�x�@�`B@�`B@�X@�G�@�?}@��@�%@��@��`@��/@���@�z�@�Q�@�(�@�1@�ƨ@�t�@�;d@�"�@��@��H@���@���@��+@�v�@�^5@�-@��@���@��@���@���@��@� �@�ƨ@�dZ@�@��H@���@�-@�$�@���@��#@���@�A�@~ff@t(�@ix�@b�@\I�@S�
@M�@G�@@bN@:��@3��@-�h@(  @!�#@E�@��@r�@j@\)@	�^111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBT�BR�BO�BN�BO�BQ�BZBbNBiyB{�B}�B|�B|�B{�By�Br�Bt�B|�B}�B� B�B�B�1B�PB�PB�\B�bB�hB��B��B��B��B��B��B��B��B�B�3B�FB�FB�FB�9B�FB�XB�RB�XB�dB�wBB��B�`B+B�B�B#�B-B6FB>wBC�BO�B\)BbNBjBjBjB�B�Bu�BR�B=qBDB�yB�/B��B�qB�-B�\B� Bz�Bk�BS�B=qB2-B,B%�B�BVB%B  B��B�B�5B��BȴB�RB�B��B�VB�1B� Bq�B_;BN�BD�B33B"�B\BB
��B
�B
�mB
�BB
�5B
�B
�B
��B
��B
ȴB
�9B
��B
�\B
|�B
y�B
t�B
ZB
J�B
<jB
0!B
'�B
�B
  B	�B	�NB	��B	ɺB	�wB	�!B	��B	��B	��B	�VB	�DB	�1B	�%B	�B	�B	}�B	z�B	w�B	q�B	l�B	`BB	[#B	T�B	I�B	A�B	?}B	>wB	?}B	:^B	9XB	7LB	5?B	49B	9XB	7LB	2-B	,B	%�B	�B	�B	�B	\B	%B	B��B��B�B�BB�/B�#B�
B��B��B��BƨBĜBB�}B�^B�FB�3B�FB�!B�B��B��B��B��B��B��B��B��B��B��B�bB�hB�\B�hB�\B�PB�=B�1B�+B�B�B�B�B�B�B� B}�B|�Bx�Bs�Bq�Bn�Bl�Bk�BiyBgmBffBe`BcTBaHBcTBgmBjBjBjBk�BiyBgmBcTBcTB`BB_;B^5B]/B\)BZBXBR�BM�BH�BF�BE�BD�BE�BE�BE�BE�BE�BE�BF�BJ�BN�BL�BI�BH�BH�BJ�BP�BVBZB\)B^5B`BBaHBaHBaHB_;B_;B_;B^5BdZBgmBhsBhsBk�Bm�Bn�Bm�Bo�Bn�Bp�Bp�Bt�Bv�Bv�Bx�Bx�By�B|�B|�B� B�B�B�B�+B�B�B~�B� B�B�=B�1B�DB�JB�VB�bB�oB��B��B�bB�DB�=B�1B�=B�\B��B��B�B�'B�3B�3B�3B�9B�FB�XB�qB�wB�}B�wB��BBĜBŢB��B��B�B�)B�/B�5B�BB�TB�`B�fB�yB�B�B��B��B��B��B��B��B	B	B	B	B	+B		7B	JB	\B	�B	�B	�B	!�B	,B	/B	0!B	2-B	33B	49B	49B	5?B	7LB	;dB	@�B	B�B	C�B	D�B	F�B	G�B	I�B	K�B	K�B	L�B	M�B	S�B	VB	W
B	ZB	ZB	[#B	[#B	]/B	aHB	ffB	ffB	hsB	jB	k�B	o�B	p�B	q�B	t�B	v�B	v�B	v�B	v�B	y�B	y�B	~�B	�B	�+B	�1B	�1B	�1B	�1B	�1B	�1B	�+B	�1B	�DB	�VB	�\B	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�-B	�3B	�9B	�?B	�?B	�FB	�LB	�RB	�XB	�dB	�jB	�qB	�qB	�wB	��B	��B	ÖB	ǮB	ȴB	��B	��B	��B	��B	��B	�B	�B	��B
	7B
bB
�B
!�B
,B
0!B
8RB
>wB
G�B
N�B
S�B
[#B
^5B
bNB
ffB
k�B
o�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BW
BT�BQ�BO�BP�BR�B[#BdZBo�B|�B}�B|�B|�B{�B{�Bv�Bu�B}�B� B�B�B�%B�=B�PB�PB�\B�bB�oB��B��B��B��B��B��B��B��B�B�9B�FB�FB�LB�?B�LB�dB�RB�XB�dB�wB��B��B�HB%B�B�B"�B-B7LB>wBC�BP�B^5Be`Bl�Bm�Bl�B�B�B}�BXBF�B{B�B�HB��B��B�jB�{B�B�Bu�B^5BA�B5?B.B(�B�BhB1BB��B�B�TB��B��B�jB�'B��B�bB�DB�Bw�Be`BQ�BJ�B8RB)�B{B%BB
��B
�B
�NB
�HB
�)B
�B
��B
��B
��B
�qB
��B
��B
}�B
|�B
� B
`BB
N�B
@�B
49B
.B
#�B
	7B	�B	�mB	�
B	��B	ĜB	�3B	�B	��B	��B	�bB	�\B	�DB	�%B	�%B	�B	�B	}�B	y�B	t�B	u�B	bNB	]/B	YB	N�B	C�B	B�B	@�B	A�B	;dB	:^B	9XB	5?B	49B	;dB	:^B	6FB	1'B	,B	$�B	�B	�B	�B	
=B	B��B��B�B�ZB�5B�/B�B��B��B��BȴBŢBŢB��B�}B�XB�LB�RB�-B�!B�B��B��B��B��B��B��B��B��B��B�hB�uB�bB�oB�oB�bB�PB�7B�=B�7B�%B�B�B�B�B�B� B� B|�Bz�By�Bq�Bp�Bn�Bl�Bk�Bk�BhsBffBe`Be`Bk�Bk�Bk�Bm�Bm�Bl�Bm�BffBe`Be`BbNB_;B^5B]/B[#B[#BXBO�BJ�BH�BG�BG�BG�BI�BH�BF�BF�BG�BI�BL�BR�BN�BK�BJ�BJ�BK�BP�BW
B\)B]/B_;BaHBbNBbNBbNBbNBaHBaHB^5BdZBgmBjBjBm�Bn�Bp�Bn�Bp�Bo�Bq�Bq�Bu�Bw�Bv�By�By�By�B}�B}�B�B�B�B�1B�7B�B�B� B� B�B�=B�7B�DB�VB�hB�oB�uB��B��B�uB�PB�VB�DB�DB�VB�{B��B�!B�'B�3B�3B�3B�9B�FB�^B�}B�wB��B�}B��BBĜBƨB��B��B�B�/B�5B�;B�HB�ZB�fB�mB�B�B�B��B��B��B��B��B	  B	B	B	B	B	+B	
=B	PB	bB	�B	�B	�B	"�B	-B	0!B	1'B	2-B	49B	49B	49B	6FB	8RB	<jB	@�B	B�B	C�B	D�B	F�B	H�B	I�B	K�B	K�B	M�B	N�B	T�B	VB	W
B	ZB	ZB	[#B	[#B	^5B	bNB	ffB	ffB	iyB	jB	l�B	o�B	q�B	q�B	u�B	w�B	v�B	w�B	w�B	y�B	y�B	}�B	�B	�+B	�1B	�7B	�7B	�7B	�7B	�1B	�1B	�7B	�JB	�VB	�\B	�\B	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�-B	�3B	�9B	�?B	�?B	�FB	�LB	�XB	�^B	�dB	�jB	�qB	�qB	�}B	��B	B	ÖB	ȴB	ɺB	��B	��B	��B	��B	��B	�B	�B	��B
	7B
bB
�B
!�B
,B
0!B
7LB
=qB
G�B
N�B
S�B
[#B
^5B
bNB
ffB
k�B
o�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447202012010314472020120103144720  AO  ARGQ                                                                        20111130142155  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142155  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144720  IP                  G�O�G�O�G�O�                