CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:35Z UW 3.1 conversion   
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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  TD   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ed   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  gL   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xl   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �L   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �\   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �`   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �d   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142730  20190522121827  1727_5046_157                   2C  D   APEX                            2143                            040306                          846 @������1   @���/h@@6Qhr� ��c�M���1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]�fD^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Db��Dc� Dd  Ddy�De  De� Df  Df� Dg  Dg� DhfDh� Di  Diy�Dj  Dj� Dk  Dk� Dk��Dly�Dm  Dm� Dm��Dn� DofDo� Dp  Dp� Dq  Dq� Dr  Dr� Dr��Dsy�Dy�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�33A��A!��AA��Aa��A���A���A���A���A���A���A���A���B ffB��BffBffB ffB(ffB0ffB8ffB@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C 33C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD� DfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD  D�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]��D^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDc  Dc�fDdfDd� DefDe�fDffDf�fDgfDg�fDh�Dh�fDifDi� DjfDj�fDkfDk�fDl  Dl� DmfDm�fDn  Dn�fDo�Do�fDpfDp�fDqfDq�fDrfDr�fDs  Ds� Dy��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�VA�VA�XA�XA�\)A�\)A�`BA�Q�A�O�A�K�A���AϮA��A̺^A�33A�9XAɑhA�?}A�x�A�ƨA�C�A��AŶFA�1Aĺ^A�1A���A�5?A�ȴA�Q�A���A���A�ƨA�$�A�Q�A�VA���A�M�A�~�A���A�l�A�S�A�JA�ZA��A��jA��`A��A��A�A�l�A���A�?}A��RA�(�A�dZA��mA�p�A��;A��hA�ZA�C�A�K�A�S�A�"�A���A��A�1A���A��A�XA��-A�VA�&�A���A��A�I�A�A�r�A��A�XA��A��;A���A�5?A�~�A��A�z�A���A�  A��A��A���A�`BA��PA��A��\A�dZA�"�A�JA�dZA�jA���A��A�x�A�A�A�/A��HA�M�A���A���A�1A���A��FA�ƨA�O�A�`BA��A� �A��jA�%A��hA�(�A���A�+A{VAyx�Ay/Au��Ap��Al��Ak"�Ai��Ai��AihsAi;dAg�Af9XAd��Ad=qAb��Abr�Aa|�A`-A]�^AZ��AX~�AW�AV1'AT9XARA�AP��AOO�ANz�AM�AL��AK�wAJ{AI�AG�#AE��AEG�AD��AC�hABbNA@VA>VA<��A;XA:�\A:1'A9�^A9?}A8��A7��A6�9A5�^A4�`A4 �A2�DA1�mA1�A0=qA.�A-��A-�A+�#A*jA*  A)��A(1'A'��A'O�A&�9A%x�A$�+A#�A��A\)A"�A�A��A`BA�wA�A�AE�A(�A��An�A��A��AȴA7LA~�At�AC�A
=A��AS�A��A1A�wA"�A�RAbNA5?AA�^A+A��A�-AhsA?}AA�A ^5@��@�hs@�  @��@��^@��@���@���@�33@�-@�@���@���@�j@�;d@��@�X@�l�@�j@�w@�l�@�;d@�@�h@�7L@�p�@�1'@�\)@֏\@�=q@ղ-@���@�K�@�o@җ�@�hs@Ϯ@�`B@��
@�"�@�^5@ɑh@ȣ�@�J@�`B@���@ļj@�Q�@Õ�@�@���@�ff@��-@��@�A�@�|�@���@��@�+@�o@���@��u@��@� �@��P@�"�@��H@��+@�-@���@��7@�V@�r�@�1'@�b@��w@��^@���@��/@�?}@�G�@��j@�j@�&�@�ƨ@���@�n�@�n�@�
=@��@��y@�|�@�|�@��\@�O�@�+@���@��w@���@�7L@��9@�X@�V@��@���@�bN@�|�@�"�@��@��@��m@��m@�9X@��j@�bN@�r�@���@�p�@�V@�$�@�@�o@��y@��9@��@��@���@�bN@���@�z�@�(�@��;@��F@���@�(�@���@��j@��@���@�A�@���@���@�-@���@���@��`@�J@�j@�z�@�I�@��y@��+@�5?@���@�x�@��u@���@��m@�9X@���@�Ĝ@��D@�Q�@�I�@�bN@��D@���@�bN@� �@�1@��;@��
@��;@���@�dZ@�33@��H@�~�@��@��@���@��^@���@���@�hs@�V@��@��`@���@�&�@�?}@�O�@�p�@�hs@�X@�x�@��h@�p�@�hs@�7L@��/@�I�@�Z@�z�@���@��
@��@�ƨ@���@�"�@�ȴ@�~�@�ff@��@��^@��-@��-@��-@��h@�`B@��@�bN@�A�@��@���@���@��F@��F@��@���@�\)@�33@�+@�
=@��@��!@�v�@�$�@�@��T@���@�`B@�7L@�&�@�V@��@���@��9@��u@�Z@�hs1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�VA�VA�XA�XA�\)A�\)A�`BA�Q�A�O�A�K�A���AϮA��A̺^A�33A�9XAɑhA�?}A�x�A�ƨA�C�A��AŶFA�1Aĺ^A�1A���A�5?A�ȴA�Q�A���A���A�ƨA�$�A�Q�A�VA���A�M�A�~�A���A�l�A�S�A�JA�ZA��A��jA��`A��A��A�A�l�A���A�?}A��RA�(�A�dZA��mA�p�A��;A��hA�ZA�C�A�K�A�S�A�"�A���A��A�1A���A��A�XA��-A�VA�&�A���A��A�I�A�A�r�A��A�XA��A��;A���A�5?A�~�A��A�z�A���A�  A��A��A���A�`BA��PA��A��\A�dZA�"�A�JA�dZA�jA���A��A�x�A�A�A�/A��HA�M�A���A���A�1A���A��FA�ƨA�O�A�`BA��A� �A��jA�%A��hA�(�A���A�+A{VAyx�Ay/Au��Ap��Al��Ak"�Ai��Ai��AihsAi;dAg�Af9XAd��Ad=qAb��Abr�Aa|�A`-A]�^AZ��AX~�AW�AV1'AT9XARA�AP��AOO�ANz�AM�AL��AK�wAJ{AI�AG�#AE��AEG�AD��AC�hABbNA@VA>VA<��A;XA:�\A:1'A9�^A9?}A8��A7��A6�9A5�^A4�`A4 �A2�DA1�mA1�A0=qA.�A-��A-�A+�#A*jA*  A)��A(1'A'��A'O�A&�9A%x�A$�+A#�A��A\)A"�A�A��A`BA�wA�A�AE�A(�A��An�A��A��AȴA7LA~�At�AC�A
=A��AS�A��A1A�wA"�A�RAbNA5?AA�^A+A��A�-AhsA?}AA�A ^5@��@�hs@�  @��@��^@��@���@���@�33@�-@�@���@���@�j@�;d@��@�X@�l�@�j@�w@�l�@�;d@�@�h@�7L@�p�@�1'@�\)@֏\@�=q@ղ-@���@�K�@�o@җ�@�hs@Ϯ@�`B@��
@�"�@�^5@ɑh@ȣ�@�J@�`B@���@ļj@�Q�@Õ�@�@���@�ff@��-@��@�A�@�|�@���@��@�+@�o@���@��u@��@� �@��P@�"�@��H@��+@�-@���@��7@�V@�r�@�1'@�b@��w@��^@���@��/@�?}@�G�@��j@�j@�&�@�ƨ@���@�n�@�n�@�
=@��@��y@�|�@�|�@��\@�O�@�+@���@��w@���@�7L@��9@�X@�V@��@���@�bN@�|�@�"�@��@��@��m@��m@�9X@��j@�bN@�r�@���@�p�@�V@�$�@�@�o@��y@��9@��@��@���@�bN@���@�z�@�(�@��;@��F@���@�(�@���@��j@��@���@�A�@���@���@�-@���@���@��`@�J@�j@�z�@�I�@��y@��+@�5?@���@�x�@��u@���@��m@�9X@���@�Ĝ@��D@�Q�@�I�@�bN@��D@���@�bN@� �@�1@��;@��
@��;@���@�dZ@�33@��H@�~�@��@��@���@��^@���@���@�hs@�V@��@��`@���@�&�@�?}@�O�@�p�@�hs@�X@�x�@��h@�p�@�hs@�7L@��/@�I�@�Z@�z�@���@��
@��@�ƨ@���@�"�@�ȴ@�~�@�ff@��@��^@��-@��-@��-@��h@�`B@��@�bN@�A�@��@���@���@��F@��F@��@���@�\)@�33@�+@�
=@��@��!@�v�@�$�@�@��T@���@�`B@�7L@�&�@�V@��@���@��9@��u@�Z@�hs1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�
B�B��B��B��B�`B�B�`B1B  B�fB�B��BbB�B&�B!�B33B7LB,B�BuB�B�B'�B6FBD�BG�BC�BD�B]/BffBdZBgmBffBgmBm�B{�B�1B��B��B�-BŢBȴBBBǮBƨBɺBǮBƨB�XB�?B�9B�LB�XB�^B�qB�dB�3B�3B�B��B��B��B��B��B��B�uB�bB�\B�7B~�Bp�BjBbNB^5BYBP�BK�BD�B:^B'�B!�B7LB<jBT�BdZBgmBgmBffBH�B-BB�
B��BÖB�3B��B�uB�Bx�Bn�B]/B<jB)�B�B1B  B
�B
ŢB
�3B
��B
��B
�hB
�B
[#B
0!B
B	��B	�B	�
B	�?B	��B	�uB	�PB	�DB	�DB	�PB	�\B	�uB	�\B	�JB	�7B	�VB	�+B	|�B	o�B	cTB	XB	Q�B	K�B	C�B	:^B	33B	2-B	0!B	-B	(�B	(�B	$�B	%�B	�B	�B	{B	hB	
=B	B��B�B�fB�TB�HB�;B�;B�/B�B�
B��B��B��B��BƨB��B�jB�9B�B�B��B��B��B��B��B��B��B��B�{B�bB�JB�%B�B�B� B}�By�By�Bz�By�Bx�B|�B�%B�%B�=B�7B�B�B� B�B�B�B�%B�B�1B�DB�DB�=B�7B�7B�7B�1B�+B�%B�%B�B�B�B�B� B{�Bz�Bv�Bt�Br�Bq�Bp�Bo�Bn�Bl�Bl�Bl�BiyBdZBaHBe`BgmBl�Bm�Bp�Bp�Bo�Bn�Bl�BhsBdZBgmBhsBhsBjBjBk�Bq�Bs�Bt�Bs�Bt�Bu�Bu�Bv�Bx�By�B{�B}�B�B�B�B�B�%B�\B��B��B��B��B��B�B�B�!B�-B�dB�wBŢB��B�5B�fB�B�B�B�B�B��B��B��B	B	B	%B	B	%B		7B	\B	{B	�B	�B	�B	DB	
=B		7B	JB	hB	�B	�B	�B	#�B	$�B	!�B	�B	�B	uB	VB	DB	VB	hB	�B	�B	!�B	 �B	 �B	"�B	#�B	%�B	,B	/B	2-B	5?B	9XB	9XB	;dB	?}B	B�B	F�B	Q�B	ZB	\)B	^5B	`BB	ffB	dZB	dZB	k�B	p�B	p�B	p�B	q�B	r�B	v�B	�B	�%B	�+B	�1B	�1B	�7B	�7B	�1B	�%B	�1B	�+B	�B	�7B	� B	q�B	ffB	dZB	iyB	jB	k�B	k�B	n�B	s�B	v�B	y�B	� B	�B	�B	�B	�B	�B	�%B	�%B	�1B	�1B	�1B	�1B	�7B	�=B	�DB	�JB	�VB	�\B	�\B	�\B	�bB	�bB	�bB	�bB	�bB	�hB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�-B	�3B	�LB	�RB	�XB	�XB	�dB	�qB	�wB	�}B	��B	��B	��B	��B	��B	��B	�}B	�}B	�}B	��B	��B	B	ÖB	ĜB	ĜB	ŢB	ŢB	ǮB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�;1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B�B�
B�B�
B��B��B�B�B�yB
=BB�sB�B��BbB�B(�B"�B5?B;dB.B�B�B�B!�B,B9XBI�BL�BF�BG�BaHBiyBe`BgmBgmBjBo�B{�B�+B��B��B�3BƨB��BĜBĜBɺBɺB��BɺBɺB�^B�FB�9B�LB�XB�dB�}B�}B�^B�^B�B��B��B��B��B��B��B��B�oB�uB�VB�+Bt�Bn�BcTB`BB\)BQ�BN�BG�B@�B,B!�B7LB:^BS�BffBjBhsBm�BO�B7LBPB�#B��B��B�dB��B��B�%B{�Br�Be`B@�B/B�B
=BB
��B
��B
�?B
�B
��B
�uB
�PB
gmB
<jB
	7B	��B	��B	�ZB	��B	��B	��B	�VB	�JB	�JB	�hB	�{B	��B	�oB	�hB	�=B	�hB	�DB	�B	x�B	k�B	[#B	VB	P�B	H�B	=qB	6FB	49B	1'B	0!B	+B	-B	&�B	(�B	"�B	�B	�B	{B	PB	%B��B�B�B�`B�NB�BB�HB�;B�)B�#B�
B��B��B��BȴBĜB�}B�dB�B�B�B��B��B��B��B��B��B��B��B�uB�oB�oB�1B�B�B�B�B� B}�B{�By�B|�B�+B�7B�JB�DB�=B�%B�B�B�B�B�+B�1B�VB�VB�JB�JB�=B�=B�=B�7B�1B�1B�1B�+B�B�B�B�%B�B~�Bx�Bv�Bt�Br�Bq�Bp�Bo�Bn�Bm�Bm�Bk�BiyBdZBgmBiyBo�Br�Br�Bq�Bo�Bo�Bo�Bp�BjBiyBiyBiyBk�Bk�Bm�Br�Bs�Bt�Bu�Bw�Bx�Bw�Bw�By�Bz�B}�B�B�B�B�B�B�+B�bB��B��B��B��B�B�B�B�!B�-B�dB�qBĜB��B�;B�mB�B�B�B�B�B��B��B��B	B	B	+B	1B	+B		7B	VB	{B	�B	�B	�B	PB	DB		7B	JB	bB	�B	�B	�B	#�B	%�B	#�B	 �B	�B	�B	hB	JB	\B	bB	�B	�B	"�B	!�B	!�B	#�B	#�B	$�B	+B	/B	2-B	49B	:^B	9XB	:^B	>wB	C�B	D�B	P�B	ZB	]/B	bNB	_;B	hsB	e`B	cTB	jB	q�B	q�B	q�B	q�B	r�B	u�B	�B	�%B	�+B	�1B	�7B	�=B	�=B	�7B	�B	�7B	�7B	�B	�JB	�+B	y�B	hsB	e`B	jB	k�B	l�B	m�B	p�B	s�B	u�B	x�B	� B	�B	�B	�B	�B	�B	�%B	�%B	�1B	�1B	�1B	�1B	�7B	�=B	�DB	�JB	�\B	�bB	�bB	�\B	�bB	�bB	�bB	�bB	�bB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�3B	�3B	�LB	�RB	�XB	�^B	�jB	�wB	�wB	��B	��B	��B	��B	��B	��B	��B	�}B	��B	�}B	��B	��B	B	ÖB	ĜB	ĜB	ŢB	ŢB	ǮB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�;1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<#�
<#�
<#�
<#�
<#�
<#�
<D��<D��<#�
<#�
<#�
<T��<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447292012010314472920120103144729  AO  ARGQ                                                                        20111130142730  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142730  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144729  IP                  G�O�G�O�G�O�                