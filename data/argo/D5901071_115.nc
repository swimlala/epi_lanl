CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:23Z UW 3.1 conversion   
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
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               sA   AO  20111130141800  20190522121826  1727_5046_115                   2C  D   APEX                            2143                            040306                          846 @ԪU��_�1   @ԪV�8��@6��x����d �\)1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8�fD9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^�fD_fD_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dly�Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dzf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33@�33A��A!��AA��Aa��A���A���A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&  C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD33CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD�D�fDfD� DfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8��D9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^��D_�D_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl� DmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDz�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AōPAŉ7AŅAŃAŅAŇ+Aŉ7AŋDAōPAŉ7AŅAŉ7AŅAœuAř�Aś�Aŧ�AžwA�ĜA��;A��mA��mA��A���A���A�1A�JA�VA�VA�  A��TAũ�A��mA�=qA�p�A�v�A��!A�l�A�JA�oA�$�A�{A��FA�|�A��A�p�A�-A���A��RA�S�A�t�A���A�VA�ȴA��PA�dZA��A���A��A���A�oA�x�A�
=A�|�A�\)A��HA�33A�\)A��\A�C�A��;A�~�A�-A��-A�5?A��A���A�C�A�=qA��FA��jA��uA��A��mA�7LA��\A��RA�VA���A�ĜA��A��A���A�A���A�n�A�I�A�`BA���A���A���A���A�33A�{A��A�ĜA��9A�;dA�v�A���A��A�ffA���A�\)A���A�A~�RA|�Az(�Aw��Au��Au
=As��Aq�Ao��An�AmG�AkVAh�Ad�9Ab(�A`{A^�A\�+A[VAXbNAV�AT1'AR��ARE�AQ�hAP�DAOdZAM��AK��AH��AF�`ADn�AB�`AA;dA@JA?C�A>��A=��A;�A:�uA9"�A7��A5�TA4��A4VA3�^A2�!A1�A0=qA.9XA+�TA*�A)&�A'��A&�9A%|�A${A"��A!��A JA��A�-A��A{AG�A��A�A9XA�AA�AG�A�yA�/A�A�AĜA�DA�mA`BAȴA�RA�A&�A�`A�uAZA��A��A�9A�A�A�A��A�!A1'A	�A	�hA	S�A	33A	%A1A=qA�7AoA�mA��A;dA%A �A bN@���@���@��@�l�@�`B@�ƨ@�K�@���@�J@��/@�M�@�ȴ@�Z@�ȴ@�`B@���@�@�r�@�C�@�@�Q�@ۅ@ڟ�@��H@�O�@�=q@�%@�  @�K�@�~�@�5?@�X@��/@�|�@ʏ\@�O�@�o@��@��@�t�@�X@��@�9X@��P@�=q@�O�@��@�1'@�ff@��@�5?@��@�@�E�@��@�\)@���@��\@�n�@�-@�@��h@�/@���@���@��@�o@�ȴ@��\@�M�@���@���@�-@��@���@���@���@���@�X@��^@�O�@��`@��/@���@�@�5?@���@�V@��#@��@�v�@�-@��@��^@�x�@���@���@���@�Q�@�C�@�n�@���@�`B@�&�@��/@��@�I�@�1@��F@�S�@�dZ@�33@���@���@�^5@��@���@��@��@�9X@��m@�|�@���@�"�@��@�G�@�z�@��@��@��@�\)@�33@�+@��@�
=@��H@���@�^5@�=q@�$�@��@��#@��^@��^@���@��@�/@�r�@�  @���@�o@�-@�@�@�p�@�z�@� �@�1@�  @�  @�  @��F@���@��j@�5?@��@��;@���@��w@��@�\)@�S�@�
=@��T@�j@�I�@�I�@��
@�o@��\@��T@�?}@�bN@�;d@���@��7@�hs@�G�@�&�@���@���@���@��u@��@�A�@�  @��@�t�@�C�@�"�@�ȴ@�M�@��@��@��-@��7@�x�@�/@���@���@��D@�Z@�b@�ƨ@��F@��F@��P@��@��H@�ȴ@���@��R@��!@�v�@�E�@�=q@�$�@��@���@��h@��h@�X@�/@��@��@�V@�%@���@���@�z�@�Z@�9X@�  @��m@��@�o@��y@��R@��+@�^5@�-@��@��^@���@��7@�?}@���@��`@���@��j@|9X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AōPAŉ7AŅAŃAŅAŇ+Aŉ7AŋDAōPAŉ7AŅAŉ7AŅAœuAř�Aś�Aŧ�AžwA�ĜA��;A��mA��mA��A���A���A�1A�JA�VA�VA�  A��TAũ�A��mA�=qA�p�A�v�A��!A�l�A�JA�oA�$�A�{A��FA�|�A��A�p�A�-A���A��RA�S�A�t�A���A�VA�ȴA��PA�dZA��A���A��A���A�oA�x�A�
=A�|�A�\)A��HA�33A�\)A��\A�C�A��;A�~�A�-A��-A�5?A��A���A�C�A�=qA��FA��jA��uA��A��mA�7LA��\A��RA�VA���A�ĜA��A��A���A�A���A�n�A�I�A�`BA���A���A���A���A�33A�{A��A�ĜA��9A�;dA�v�A���A��A�ffA���A�\)A���A�A~�RA|�Az(�Aw��Au��Au
=As��Aq�Ao��An�AmG�AkVAh�Ad�9Ab(�A`{A^�A\�+A[VAXbNAV�AT1'AR��ARE�AQ�hAP�DAOdZAM��AK��AH��AF�`ADn�AB�`AA;dA@JA?C�A>��A=��A;�A:�uA9"�A7��A5�TA4��A4VA3�^A2�!A1�A0=qA.9XA+�TA*�A)&�A'��A&�9A%|�A${A"��A!��A JA��A�-A��A{AG�A��A�A9XA�AA�AG�A�yA�/A�A�AĜA�DA�mA`BAȴA�RA�A&�A�`A�uAZA��A��A�9A�A�A�A��A�!A1'A	�A	�hA	S�A	33A	%A1A=qA�7AoA�mA��A;dA%A �A bN@���@���@��@�l�@�`B@�ƨ@�K�@���@�J@��/@�M�@�ȴ@�Z@�ȴ@�`B@���@�@�r�@�C�@�@�Q�@ۅ@ڟ�@��H@�O�@�=q@�%@�  @�K�@�~�@�5?@�X@��/@�|�@ʏ\@�O�@�o@��@��@�t�@�X@��@�9X@��P@�=q@�O�@��@�1'@�ff@��@�5?@��@�@�E�@��@�\)@���@��\@�n�@�-@�@��h@�/@���@���@��@�o@�ȴ@��\@�M�@���@���@�-@��@���@���@���@���@�X@��^@�O�@��`@��/@���@�@�5?@���@�V@��#@��@�v�@�-@��@��^@�x�@���@���@���@�Q�@�C�@�n�@���@�`B@�&�@��/@��@�I�@�1@��F@�S�@�dZ@�33@���@���@�^5@��@���@��@��@�9X@��m@�|�@���@�"�@��@�G�@�z�@��@��@��@�\)@�33@�+@��@�
=@��H@���@�^5@�=q@�$�@��@��#@��^@��^@���@��@�/@�r�@�  @���@�o@�-@�@�@�p�@�z�@� �@�1@�  @�  @�  @��F@���@��j@�5?@��@��;@���@��w@��@�\)@�S�@�
=@��T@�j@�I�@�I�@��
@�o@��\@��T@�?}@�bN@�;d@���@��7@�hs@�G�@�&�@���@���@���@��u@��@�A�@�  @��@�t�@�C�@�"�@�ȴ@�M�@��@��@��-@��7@�x�@�/@���@���@��D@�Z@�b@�ƨ@��F@��F@��P@��@��H@�ȴ@���@��R@��!@�v�@�E�@�=q@�$�@��@���@��h@��h@�X@�/@��@��@�V@�%@���@���@�z�@�Z@�9X@�  @��m@��@�o@��y@��R@��+@�^5@�-@��@��^@���@��7@�?}@���@��`@���@��j@|9X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBBÖB��B��B��B��BB��B��B��B��B��B��BÖBÖBƨB��B��B�
B�BB�ZB�mB�B�B��B+BJB\BoB�B#�B49BL�BcTBffBw�Bw�Bt�Bn�Bn�Bs�Bw�B|�B|�Bl�Bq�Bt�Bt�Bw�Bz�B�7B�!BĜBǮBŢBŢBŢBŢBŢBĜBB��B�}B�dB�?B�-B�B��B��B��B��B�bB�PB�+B� Bx�Bq�BgmB]/BQ�BD�B5?B)�B�B
=BB��B�B�HB��B��B�'B��B��B�hB�1Bv�BgmBS�B5?B �B
=B
�B
ŢB
��B
�DB
{�B
r�B
ffB
]/B
W
B
T�B
L�B
B�B
:^B
1'B
&�B
{B
B	�B	�NB	�/B	��B	��B	�dB	�B	��B	�uB	~�B	e`B	VB	B�B	0!B	 �B	hB��B�B�TB�/B�B��B��B��BB�LB�B�!B��B��B��B��B��B��B�uB�VB�+B� Bx�Bs�Bq�Bo�Bm�Bk�BgmBbNB_;B_;B_;B_;B`BB_;B_;B`BBaHBaHBcTBdZBgmBiyBjBhsBgmBgmBgmBcTBaHBbNBbNBcTBdZBdZBcTBcTBbNBaHB_;B_;B_;B_;B_;B^5B]/B\)B[#BZBYBXBXBXBW
BS�BT�BQ�BP�BP�BN�BK�BH�BG�BE�BE�B?}B?}B?}B=qB:^B6FB7LB6FB5?B49B33B33B2-B0!B.B,B,B%�B#�B"�B!�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B!�B#�B&�B)�B)�B+B/B7LB;dB?}BA�BD�BH�BE�BC�BE�BF�BG�BH�BI�BJ�BJ�BJ�BK�BL�BM�BM�BQ�BXBYBZB\)B`BBdZBk�Bu�B{�B� B�B�B�JB�oB��B��B��B��B��B�B�?B�dB�jB��BǮB��B��B��B��B��B��B�B�
B�#B�HB�fB�yB�B�B�B��B��B��B	B	JB	\B	hB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	uB	oB	�B	�B	�B	!�B	"�B	#�B	%�B	%�B	&�B	&�B	(�B	)�B	0!B	33B	33B	5?B	6FB	7LB	7LB	8RB	;dB	<jB	@�B	A�B	A�B	C�B	E�B	F�B	F�B	F�B	H�B	I�B	I�B	I�B	I�B	J�B	K�B	P�B	ZB	cTB	iyB	m�B	r�B	s�B	s�B	s�B	s�B	x�B	z�B	z�B	z�B	{�B	z�B	y�B	y�B	w�B	u�B	r�B	n�B	k�B	l�B	n�B	p�B	r�B	w�B	y�B	z�B	{�B	� B	�B	�B	�B	�+B	�=B	�JB	�\B	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�3B	�9B	�9B	�?B	�?B	�LB	�XB	�XB	�XB	�dB	�wB	��B	��B	��B	��B	B	B	B	B	ÖB	ĜB	ƨB	ƨB	ǮB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�)B	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BBÖB��B��B��B��BB��B��B��B��B��B��BÖBÖBƨB��B��B�B�BB�ZB�mB�B�B��B+BJB\BoB�B$�B7LBR�BffBr�B� B}�B{�Bs�Bx�By�By�B}�B�Bn�Br�Bv�Bx�By�B}�B�1B�-BƨBȴB��B��B��BȴBƨBǮBŢBÖBB��B�LB�FB�!B��B��B��B��B�hB�\B�7B�Bz�Bt�Bl�B`BBW
BJ�B8RB/B�BPB%B��B�B�`B�BŢB�?B��B��B�uB�PBy�BiyBZB8RB#�BVB
�B
��B
��B
�\B
}�B
v�B
hsB
^5B
YB
XB
O�B
F�B
>wB
49B
,B
�B
JB	��B	�ZB	�HB	�B	ŢB	�}B	�'B	��B	��B	�7B	k�B	[#B	G�B	49B	$�B	�B	B�B�mB�;B�)B�B��B��BȴB��B�?B�RB�B�B��B��B��B��B��B�uB�JB�B� Bv�Bs�Bq�Bp�Bn�BjBffBdZBbNBbNBbNBbNBbNBbNBcTBdZBe`BgmBgmBiyBl�Bm�BiyBhsBiyBk�BffBe`BcTBbNBcTBdZBe`BdZBe`BdZBdZBgmBcTB`BB`BB`BB_;B_;B_;B\)B]/B\)BYBXBXBYBZBVBR�BP�BQ�BQ�BP�BJ�BH�BI�BJ�BA�B@�B@�B?}B@�B;dB9XB7LB8RB6FB49B49B33B2-B2-B1'B0!B(�B&�B&�B#�B#�B!�B �B!�B�B�B!�B�B�B�B�B�B�B�B�B�B�B�B �B"�B!�B!�B#�B&�B'�B+B+B-B0!B8RB<jBB�BB�BD�BI�BI�BD�BG�BI�BH�BH�BI�BK�BJ�BK�BL�BL�BN�BO�BR�BXBZB[#B\)B`BBdZBl�Bv�B{�B� B�B�B�JB�uB��B��B��B��B��B�B�FB�jB�jB��BȴB��B��B��B��B��B��B�B�B�)B�NB�fB�yB�B�B�B��B��B��B	B	JB	\B	oB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	uB	�B	�B	�B	!�B	"�B	#�B	%�B	%�B	&�B	&�B	(�B	+B	0!B	33B	33B	5?B	6FB	7LB	7LB	8RB	<jB	=qB	A�B	B�B	B�B	D�B	F�B	F�B	F�B	G�B	I�B	I�B	I�B	I�B	I�B	J�B	K�B	O�B	XB	aHB	iyB	m�B	r�B	s�B	s�B	s�B	t�B	z�B	|�B	z�B	z�B	|�B	{�B	z�B	z�B	x�B	v�B	t�B	p�B	l�B	l�B	n�B	p�B	s�B	w�B	y�B	z�B	{�B	� B	�B	�B	�B	�+B	�=B	�PB	�bB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�3B	�9B	�9B	�?B	�?B	�LB	�XB	�XB	�XB	�dB	�wB	��B	��B	��B	��B	B	B	B	B	ÖB	ŢB	ƨB	ƨB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�)B	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447142012010314471420120103144714  AO  ARGQ                                                                        20111130141800  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141800  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144714  IP                  G�O�G�O�G�O�                