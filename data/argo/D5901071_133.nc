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
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T8   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  eP   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  vh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xP   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �h   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142207  20190522121827  1727_5046_133                   2C  D   APEX                            2143                            040306                          846 @����q` 1   @���Q��@7H1&�x��d�C��1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�fC  C  C
  C  C  C  C  C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D4��D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Db��Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�33A��A!��AA��Aa��A���A���A���A�  A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C�C  C�C�C
�C�C�C�C�C33C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@  CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD� DfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5  D5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDc  Dc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A�|�A�M�A��A�t�A�oA��
A���A�dZA�A�G�A�l�A��A���A���A�t�A�E�A���A�ĜA��-A��uA�t�A�x�A��jA�ĜA��PA�K�A�(�A���A���A�A��;A��A�VA���A�ƨA��^A��+A�S�A�7LA�&�A��A�JA��A��mA��A��A���A�Q�A�VA���A���A��A�+A��A�1A���A���A�n�A�I�A��A��FA�jA�ƨA�VA�"�A��A�ȴA��A�dZA�S�A�bA�p�A��!A��wA��hA�G�A��A�l�A��A���A��A�/A���A�ȴA��!A���A��uA��DA�v�A���A�"�A�VA�K�A�hsA�l�A�r�A��`A�^5A��A��RA�ZA��A�M�A�1A�;dA��A�A��HA���A��A��9A�^5A�$�A��;A���A�ffA�bNA��mA� �A�1A���A�`BA��;A�/A�jA���A�bA��#A�9XA~�DA|5?Ay��Aut�Ar��Aq+An�Al��Ai�TAh��AgXAe�^Aax�A_�A^�RA^=qA^bA]��A]x�A\��A[�-AZ~�AY;dAV�uATz�AS�-AS33AR��ARE�AQ|�AQ�AO�TAK�-AIt�AG�mAF$�AE�hAC��ABȴAC�ACx�ACdZACS�AB��AB��ABI�AB=qAA��A@r�A?��A>��A=�A<A<^5A<JA;�A;7LA:�A9dZA9%A8ȴA8^5A7&�A4��A3�^A2$�A0=qA.��A-A-
=A,JA+/A)ƨA(�A'\)A&-A$��A$=qA"��A"r�A!�PA �A JA��Av�A��AE�AAC�A�A�/A�A��AI�AA��A�wA�yA��A �AS�AbAoA��A�FA%An�A��A"�A
�uA	��A��A��At�A��A�hAAn�A�A�wA�PAVA�A��A�A ff@�t�@�o@��\@��@�%@�dZ@���@��H@�ff@�hs@�1@�{@��@���@���@���@�ƨ@柾@�`B@���@�\)@�&�@�9X@�  @߶F@�
=@�{@�/@�I�@۝�@�ȴ@���@��@�r�@�9X@�=q@�p�@�j@�S�@��@Ѓ@�b@�K�@��@�-@�V@��@��#@��@Ǿw@�o@�v�@�{@��@�Q�@�@��@���@���@���@���@��@��\@�ff@���@�@�%@��y@��\@�hs@�n�@��@�ȴ@���@�ƨ@�j@�(�@�33@���@��h@�(�@��@��@�\)@��
@� �@��/@���@�J@��/@�33@���@�~�@�@��h@��@��@�Q�@��@�
=@���@���@���@���@�~�@�ff@�E�@��@��@��^@�G�@��u@��m@���@�|�@�C�@�;d@��@�ȴ@��!@��\@�5?@��@�J@���@�/@�V@��@���@�Ĝ@���@��@�Q�@�ƨ@�t�@�K�@��@��\@�M�@��-@�O�@�7L@��`@�z�@�1'@���@���@�M�@���@�x�@�?}@���@�Q�@�ƨ@�33@��@��y@�ȴ@�$�@�@��#@�?}@��D@�b@��@���@�C�@�33@�o@��R@�=q@�M�@�=q@��T@��-@�&�@�I�@��F@���@�+@��@��H@�ȴ@�~�@�E�@�J@�@�@��T@���@���@��7@�hs@�%@��u@��D@�bN@�1@��;@���@�|�@�S�@�;d@�"�@�@��@���@���@�n�@�J@���@��7@�hs@�X@��@��@�r�@�I�@��m@��@�|�@�33@���@���@���@���@��+@�~�@�n�@�n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A�|�A�M�A��A�t�A�oA��
A���A�dZA�A�G�A�l�A��A���A���A�t�A�E�A���A�ĜA��-A��uA�t�A�x�A��jA�ĜA��PA�K�A�(�A���A���A�A��;A��A�VA���A�ƨA��^A��+A�S�A�7LA�&�A��A�JA��A��mA��A��A���A�Q�A�VA���A���A��A�+A��A�1A���A���A�n�A�I�A��A��FA�jA�ƨA�VA�"�A��A�ȴA��A�dZA�S�A�bA�p�A��!A��wA��hA�G�A��A�l�A��A���A��A�/A���A�ȴA��!A���A��uA��DA�v�A���A�"�A�VA�K�A�hsA�l�A�r�A��`A�^5A��A��RA�ZA��A�M�A�1A�;dA��A�A��HA���A��A��9A�^5A�$�A��;A���A�ffA�bNA��mA� �A�1A���A�`BA��;A�/A�jA���A�bA��#A�9XA~�DA|5?Ay��Aut�Ar��Aq+An�Al��Ai�TAh��AgXAe�^Aax�A_�A^�RA^=qA^bA]��A]x�A\��A[�-AZ~�AY;dAV�uATz�AS�-AS33AR��ARE�AQ|�AQ�AO�TAK�-AIt�AG�mAF$�AE�hAC��ABȴAC�ACx�ACdZACS�AB��AB��ABI�AB=qAA��A@r�A?��A>��A=�A<A<^5A<JA;�A;7LA:�A9dZA9%A8ȴA8^5A7&�A4��A3�^A2$�A0=qA.��A-A-
=A,JA+/A)ƨA(�A'\)A&-A$��A$=qA"��A"r�A!�PA �A JA��Av�A��AE�AAC�A�A�/A�A��AI�AA��A�wA�yA��A �AS�AbAoA��A�FA%An�A��A"�A
�uA	��A��A��At�A��A�hAAn�A�A�wA�PAVA�A��A�A ff@�t�@�o@��\@��@�%@�dZ@���@��H@�ff@�hs@�1@�{@��@���@���@���@�ƨ@柾@�`B@���@�\)@�&�@�9X@�  @߶F@�
=@�{@�/@�I�@۝�@�ȴ@���@��@�r�@�9X@�=q@�p�@�j@�S�@��@Ѓ@�b@�K�@��@�-@�V@��@��#@��@Ǿw@�o@�v�@�{@��@�Q�@�@��@���@���@���@���@��@��\@�ff@���@�@�%@��y@��\@�hs@�n�@��@�ȴ@���@�ƨ@�j@�(�@�33@���@��h@�(�@��@��@�\)@��
@� �@��/@���@�J@��/@�33@���@�~�@�@��h@��@��@�Q�@��@�
=@���@���@���@���@�~�@�ff@�E�@��@��@��^@�G�@��u@��m@���@�|�@�C�@�;d@��@�ȴ@��!@��\@�5?@��@�J@���@�/@�V@��@���@�Ĝ@���@��@�Q�@�ƨ@�t�@�K�@��@��\@�M�@��-@�O�@�7L@��`@�z�@�1'@���@���@�M�@���@�x�@�?}@���@�Q�@�ƨ@�33@��@��y@�ȴ@�$�@�@��#@�?}@��D@�b@��@���@�C�@�33@�o@��R@�=q@�M�@�=q@��T@��-@�&�@�I�@��F@���@�+@��@��H@�ȴ@�~�@�E�@�J@�@�@��T@���@���@��7@�hs@�%@��u@��D@�bN@�1@��;@���@�|�@�S�@�;d@�"�@�@��@���@���@�n�@�J@���@��7@�hs@�X@��@��@�r�@�I�@��m@��@�|�@�33@���@���@���@���@��+@�~�@�n�@�n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�{B�{B��B��B��B��B��B��B�B�B�}B�B�)B�#B�5B�fB�B�B��B��B��B��BB{B�B�B�B�B�B"�B,B-B1'B9XBF�BS�B^5BaHB`BBe`BgmBhsBhsBiyBiyBdZB]/Be`BgmBgmBgmBiyBjBjBjBl�Bl�Bk�BhsBgmBffBbNBaHB_;B[#BZBXBT�BP�BW
BYBM�B?}B8RB)�B�B�BuBB�fB�-B��B��B�oB�bB�hB�VBq�B_;BW
BJ�B@�B.B{B��B�B�;BɺB��B�jB�3B��B�bB�+Bw�Bl�B\)BN�B@�B33B+B%�B �B�B�BuB%B
��B
�sB
�;B
�5B
�B
��B
��B
ŢB
�qB
�?B
��B
��B
�B
gmB
O�B
5?B
	7B	�B	�)B	��B	�jB	��B	��B	��B	��B	v�B	k�B	hsB	ffB	dZB	bNB	`BB	]/B	W
B	Q�B	I�B	=qB	9XB	7LB	49B	2-B	1'B	-B	,B	&�B	 �B	1B	B��B��B��B��B	oB	�B	�B	�B	�B	�B	&�B	-B	+B	%�B	 �B	�B	\B		7B	�B	�B	�B	�B	uB	hB	VB	JB		7B	B��B�B�`B�5B�B��B��BB�^B�FB�RB�B��B��B��B�oB�\B�=B�%B�B~�B{�B}�Bx�Bw�Bu�Br�Bs�Br�Br�Bq�Bo�Bl�Bk�Bk�BjBjBffBdZBdZBcTBbNBaHB`BB_;B`BB\)BYBW
BT�BT�BR�BR�BQ�BQ�BS�BT�BW
BZB[#B[#BZBYBXBXBXBW
BT�BT�BQ�BO�BN�BL�BJ�BJ�BI�BH�BG�BC�BB�B@�B>wB<jB:^B;dB>wBA�BC�BH�BJ�BN�BO�BN�BO�BP�BP�BS�BYB\)B]/B^5BaHBe`BiyBjBk�Bl�Bl�Bm�Bu�Bv�Bw�B{�B}�B�B�B�%B�+B�7B�JB�PB�PB�PB�\B�uB��B��B��B��B��B�oB�7B�DB��B��B��B��B�!B�?B�FB�LB�XB�XB�?B�B��B��B�?B�^BĜB��B��B��B��B�B�#B�/B�;B�BB�HB�TB�fB�mB�yB�B�B�B��B��B��B��B��B��B��B	B	%B		7B	DB	oB	uB	�B	�B	�B	�B	�B	�B	�B	�B	#�B	%�B	%�B	&�B	(�B	+B	-B	0!B	7LB	9XB	9XB	;dB	=qB	?}B	C�B	E�B	F�B	G�B	I�B	J�B	M�B	Q�B	VB	XB	YB	ZB	]/B	aHB	cTB	e`B	ffB	k�B	k�B	p�B	q�B	q�B	r�B	u�B	w�B	w�B	x�B	y�B	y�B	z�B	{�B	}�B	�B	�B	�B	�B	�B	�%B	�1B	�1B	�JB	�PB	�VB	�VB	�\B	�hB	�oB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�'B	�3B	�9B	�FB	�XB	�jB	�jB	�qB	�qB	�qB	�}B	��B	��B	B	ÖB	ĜB	ĜB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B�B�B�3BÖB�B�/B�)B�;B�mB�B�B��B��B��B��BB{B�B�B�B�B�B"�B-B-B0!B9XBG�BS�B_;BbNBaHBe`BgmBhsBhsBiyBk�BgmB^5BgmBhsBhsBhsBjBl�BjBk�Bm�Bm�Bl�BiyBhsBhsBdZBe`BbNB\)B[#BYBW
BQ�BXB[#BP�BC�B<jB/B �B�B�BB�B�dB��B��B�uB�hB�{B�{Bv�BdZB[#BM�BD�B33B�B  B�B�mB��BÖB�}B�dB��B�{B�\B}�Bu�BbNBW
BG�B8RB.B'�B"�B�B�B�BJB  B
�B
�;B
�;B
�)B
�
B
��B
ȴB
��B
�RB
�!B
��B
�PB
m�B
W
B
@�B
hB	�B	�ZB	��B	ĜB	��B	�B	��B	��B	|�B	n�B	jB	gmB	e`B	cTB	bNB	aHB	[#B	VB	Q�B	C�B	;dB	9XB	5?B	49B	33B	.B	/B	0!B	%�B	DB	%B��B��B��B��B	oB	�B	�B	�B	�B	 �B	&�B	/B	.B	'�B	"�B	�B	oB	1B	�B	�B	�B	�B	�B	oB	\B	VB	PB	1B��B�B�B�TB�)B��B��BŢB�}B�^B�jB�-B�B��B��B�uB�hB�JB�1B�%B�B� B� By�By�By�Bu�Bs�Bs�Bs�Bs�Bs�Bo�Bm�Bl�Bl�Bm�BjBgmBffBffBdZBcTBcTBaHBbNB_;B[#BYBYBXBW
BT�BS�BS�BT�BVBYB]/B]/B]/B]/B[#BYBYBYBYBXBXBW
BP�BP�BO�BM�BL�BL�BM�BM�BG�BD�BB�B?}B?}B=qB<jB>wBB�BD�BI�BK�BO�BP�BO�BP�BQ�BQ�BT�B\)B]/B_;B`BBcTBgmBjBk�Bl�Bm�Bn�Bm�Bw�Bx�By�B|�B~�B�B�B�+B�7B�DB�PB�\B�VB�VB�oB��B��B��B��B��B��B��B�DB�=B��B��B��B��B�B�?B�LB�RB�^B�dB�LB�B��B��B�?B�XBÖB��B��B��B�B�B�)B�5B�BB�BB�HB�ZB�sB�mB�yB�B�B�B��B��B��B��B��B��B��B	B	+B		7B	DB	oB	uB	�B	�B	�B	�B	�B	�B	�B	�B	#�B	%�B	%�B	&�B	(�B	+B	-B	1'B	8RB	9XB	:^B	<jB	>wB	@�B	D�B	E�B	G�B	H�B	J�B	K�B	N�B	R�B	W
B	XB	YB	[#B	]/B	bNB	dZB	e`B	ffB	k�B	l�B	p�B	q�B	r�B	s�B	v�B	w�B	x�B	y�B	y�B	y�B	{�B	|�B	}�B	�B	�B	�B	�B	�B	�+B	�1B	�7B	�JB	�PB	�VB	�\B	�\B	�hB	�oB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�'B	�3B	�9B	�LB	�^B	�jB	�jB	�qB	�wB	�wB	�}B	��B	��B	B	ÖB	ŢB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447202012010314472020120103144720  AO  ARGQ                                                                        20111130142207  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142207  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144720  IP                  G�O�G�O�G�O�                