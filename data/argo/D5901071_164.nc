CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:37Z UW 3.1 conversion   
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
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142905  20190522121827  1727_5046_164                   2C  D   APEX                            2143                            040306                          846 @��=@?�1   @���s��@6��\)�c�+I�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`ffBh  Bp  BxffB�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C[�fC]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D y�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=�fD>  D>� D?  D?� D@  D@y�DA  DA� DB  DB� DC  DC�fDDfDD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� DsfDsy�Dy��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�33A��A!��AA��Aa��A���A���A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@ffBHffBPffBX��B`��BhffBpffBx��B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C 33C"�C$�C&�C(�C*�C,�C.�C0�C2�C433C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\  C^  C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn33Cp33Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD � DfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD�D�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=��D>fD>�fD?fD?�fD@fD@� DAfDA�fDBfDB�fDCfDC��DD�DD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDs�Ds� Dy�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�XA�XA�jA�hsA�hsA�hsA�l�A�n�A�l�A�n�A�r�A�r�A�n�A�n�A�p�A�p�A�p�A�r�A�r�A�r�A�v�A�t�A�n�A�hsA�jA�l�A�C�A�&�A���A��TAʍPA��AƃAř�A��A���AÇ+A�bNA�r�A��!A�VA���A�K�A��RA�9XA��`A��wA��PA��A��jA�E�A�`BA��A���A�
=A�Q�A�1'A�=qA�  A�ZA���A��A��A�A�A��A���A���A���A�jA�=qA��A��9A���A���A�$�A��DA�=qA��A�ĜA��A���A�XA��A�XA��+A���A���A��A���A���A��-A��A�VA��A�ĜA�VA���A�"�A���A��DA�I�A���A��A��A�-A���A�t�A��A�ĜA��A�t�A���A�XA��A�|�A�dZA�p�A�ƨA��`A�-A�1A�;dA��;A�bA��A�I�A��A���A�x�A���A�bA��/A��A~=qA{\)Ax�Au��AtZAr�Ap9XAn1'Am"�Ak��Aj�uAhE�Ab�/A^��A[�^AV��AT��AT9XAP^5AL��AKK�AJE�AI�7AH��AG\)AE�
AE�AD��AD �AB�HAB�!AB  AA��AA\)AAA@n�A?�hA?dZA=?}A;�#A8�+A7;dA6�+A4��A4$�A3`BA2�A1�PA.��A+�A)XA(��A(bNA'��A&��A%�hA%XA$�A$�A"ȴA"I�A!�;A!C�A �/A �A�7A��A=qA�TAt�A+A�A�AA�A��A�A�AdZA?}AĜA�DAbNAZAM�AbA�PA�;A/A�9A$�A��AoA�+A�#A�RA��A�A$�A;dA
�9A	\)AbA�uA�AM�A�A�`A��A ��A �A �/A ��A ��A j@�|�@���@�M�@� �@���@�@�ƨ@�;d@�"�@��@�v�@��@��#@��^@�x�@��@�z�@�l�@�@�z�@�P@���@�@���@���@߾w@�@�$�@܋D@�"�@�ff@�^5@��@׾w@�+@���@�^5@�x�@��@�@��@�@Դ9@�"�@҇+@�$�@�@��@ѩ�@�/@�|�@��@�%@�V@Л�@�t�@ΰ!@̼j@�\)@�^5@�O�@�Q�@�E�@�hs@���@��T@�M�@Ƨ�@�o@ǥ�@ǍP@�~�@�/@� �@�K�@�@��@��/@�"�@��/@��@��/@���@��T@��h@�%@��@�"�@���@�n�@��T@���@�5?@��@���@�(�@��w@��@�O�@�A�@���@� �@�z�@��@�r�@��F@�O�@�z�@�r�@�bN@�9X@��@��P@��H@�5?@�~�@���@�ff@�@���@��@��@��9@�J@�E�@���@��@���@��@�1@�Z@�z�@���@�5?@�{@�E�@�&�@� �@��
@��@�ƨ@���@���@��w@�|�@��y@�ȴ@��y@�@�+@�C�@�K�@�;d@�
=@���@���@�$�@�x�@���@�@��-@���@�O�@��@�V@���@�(�@��@�C�@�C�@�
=@���@��\@�~�@�n�@�ff@�ff@�V@�5?@�{@��#@��^@��h@��@� �@�1@���@��;@��P@���@�^5@�5?@��@���@�X@���@� �@��y@��#@�O�@�&�@���@�Ĝ@��D@�I�@���@���@�t�@�;d@��@��!@��#@�`B@�%@���@��@�9X@�1'@�(�@�1@���@��@���@��w@��@�K�@��H@�5?@��@�7L@��@��@�A�@� �@�1@��m@��
@�|�@�b1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�XA�XA�jA�hsA�hsA�hsA�l�A�n�A�l�A�n�A�r�A�r�A�n�A�n�A�p�A�p�A�p�A�r�A�r�A�r�A�v�A�t�A�n�A�hsA�jA�l�A�C�A�&�A���A��TAʍPA��AƃAř�A��A���AÇ+A�bNA�r�A��!A�VA���A�K�A��RA�9XA��`A��wA��PA��A��jA�E�A�`BA��A���A�
=A�Q�A�1'A�=qA�  A�ZA���A��A��A�A�A��A���A���A���A�jA�=qA��A��9A���A���A�$�A��DA�=qA��A�ĜA��A���A�XA��A�XA��+A���A���A��A���A���A��-A��A�VA��A�ĜA�VA���A�"�A���A��DA�I�A���A��A��A�-A���A�t�A��A�ĜA��A�t�A���A�XA��A�|�A�dZA�p�A�ƨA��`A�-A�1A�;dA��;A�bA��A�I�A��A���A�x�A���A�bA��/A��A~=qA{\)Ax�Au��AtZAr�Ap9XAn1'Am"�Ak��Aj�uAhE�Ab�/A^��A[�^AV��AT��AT9XAP^5AL��AKK�AJE�AI�7AH��AG\)AE�
AE�AD��AD �AB�HAB�!AB  AA��AA\)AAA@n�A?�hA?dZA=?}A;�#A8�+A7;dA6�+A4��A4$�A3`BA2�A1�PA.��A+�A)XA(��A(bNA'��A&��A%�hA%XA$�A$�A"ȴA"I�A!�;A!C�A �/A �A�7A��A=qA�TAt�A+A�A�AA�A��A�A�AdZA?}AĜA�DAbNAZAM�AbA�PA�;A/A�9A$�A��AoA�+A�#A�RA��A�A$�A;dA
�9A	\)AbA�uA�AM�A�A�`A��A ��A �A �/A ��A ��A j@�|�@���@�M�@� �@���@�@�ƨ@�;d@�"�@��@�v�@��@��#@��^@�x�@��@�z�@�l�@�@�z�@�P@���@�@���@���@߾w@�@�$�@܋D@�"�@�ff@�^5@��@׾w@�+@���@�^5@�x�@��@�@��@�@Դ9@�"�@҇+@�$�@�@��@ѩ�@�/@�|�@��@�%@�V@Л�@�t�@ΰ!@̼j@�\)@�^5@�O�@�Q�@�E�@�hs@���@��T@�M�@Ƨ�@�o@ǥ�@ǍP@�~�@�/@� �@�K�@�@��@��/@�"�@��/@��@��/@���@��T@��h@�%@��@�"�@���@�n�@��T@���@�5?@��@���@�(�@��w@��@�O�@�A�@���@� �@�z�@��@�r�@��F@�O�@�z�@�r�@�bN@�9X@��@��P@��H@�5?@�~�@���@�ff@�@���@��@��@��9@�J@�E�@���@��@���@��@�1@�Z@�z�@���@�5?@�{@�E�@�&�@� �@��
@��@�ƨ@���@���@��w@�|�@��y@�ȴ@��y@�@�+@�C�@�K�@�;d@�
=@���@���@�$�@�x�@���@�@��-@���@�O�@��@�V@���@�(�@��@�C�@�C�@�
=@���@��\@�~�@�n�@�ff@�ff@�V@�5?@�{@��#@��^@��h@��@� �@�1@���@��;@��P@���@�^5@�5?@��@���@�X@���@� �@��y@��#@�O�@�&�@���@�Ĝ@��D@�I�@���@���@�t�@�;d@��@��!@��#@�`B@�%@���@��@�9X@�1'@�(�@�1@���@��@���@��w@��@�K�@��H@�5?@��@�7L@��@��@�A�@� �@�1@��m@��
@�|�@�b1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B}�B}�B}�B}�B}�B}�B}�B~�B~�B~�B}�B}�B}�B|�Bz�Bx�BjB\)BN�B<jB49B0!B.B,B+B(�B+B.B0!B49B@�BH�BP�BVBXB[#BbNBhsBm�Bz�Bz�Bu�Bz�B�B�1B�hB��B��B��B�-B�?B�XB�XB�^B�dB�jB�qB�wB��BBBƨB��B��B��B��BȴBĜBBÖBB��BȴB�#B�`B�yB�yB�yB�B�yB�TB�NB�5B�BɺB��B�=B�%B}�BcTBT�BI�B0!B{BB��B��B�mB��BǮBB�^B��Bz�BgmBN�B?}B33B�BbB
��B
�B
��B
�!B
��B
�VB
o�B
R�B
I�B
F�B
9XB
+B
�B
B	��B	�sB	�B	ȴB	�^B	�'B	��B	��B	�1B	o�B	I�B	33B	%�B	 �B	uB	B	oB	VB	\B	JB	DB	%B	
=B	DB	1B	B��B	JB	DB	DB		7B	+B	B	B��B��B�B�BB�#B�B��B��B��BƨB��B�FB�B��B��B��B��B��B��B��B��B�uB�hB�\B�\B�PB�DB�7B�+B�%B�B�B�B�B�B�B�B�B�B|�B{�B{�Bz�Bz�Bz�By�By�Bx�Bw�Bt�Bu�Bt�Bs�Br�Br�Bp�Bo�Bn�Bk�BjBn�Bo�Bo�Bm�Bk�Bk�BiyBiyBjBiyBhsBhsBhsBhsBgmBgmBffBffBgmBcTBcTBbNB`BBbNBdZBdZBe`Be`Be`BffBhsBhsBhsBjBjBhsBiyBl�Bn�Bn�BiyBcTBk�Bq�Br�Bs�Br�Bu�Bw�B}�B�B|�B}�B}�B� B�%B�JB�uB�{B�{B�uB�uB��B��B��B��B��B��B��B�3B�XB��BȴB��B��BɺBȴBɺB��BǮBƨBȴB��B��B�B�B�#B�;B�TB�B�B�B�B�B�sB�fB�ZB�HB�5B�#B�B�B��B��B�
B�
B�B�B�B�/B�BB�sB�B�B��B	B	%B	+B	DB	\B	uB	{B	uB	hB	uB	{B	{B	{B	{B	�B	�B	�B	#�B	'�B	+B	.B	1'B	49B	5?B	9XB	B�B	O�B	S�B	XB	\)B	]/B	aHB	l�B	u�B	z�B	|�B	z�B	{�B	{�B	z�B	y�B	x�B	y�B	{�B	}�B	~�B	~�B	�B	�B	�1B	�=B	�JB	�\B	�hB	�hB	�oB	�uB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�'B	�3B	�?B	�FB	�?B	�?B	�?B	�LB	�RB	�RB	�XB	�XB	�RB	�RB	�RB	�^B	�}B	��B	��B	B	ÖB	ÖB	ÖB	ĜB	ĜB	ĜB	ĜB	ĜB	ĜB	ƨB	ƨB	ƨB	ǮB	ȴB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B}�B}�B}�B}�B}�B}�B}�B~�B~�B~�B}�B}�B}�B}�B{�B~�Br�BaHBXB@�B7LB2-B2-B-B,B-B.B0!B2-B7LBC�BK�BR�BW
BYB^5BdZBk�Br�B}�B� Bz�B~�B�B�1B�oB��B��B��B�3B�LB�^B�^B�dB�jB�qB�wB�}B��BÖBƨB��B��B��B��B��B��BɺBƨBƨBƨBƨB��B�;B�B�B�yB�yB�B�B�ZB�TB�;B�B��B��B�DB�1B�%BgmBYBR�B9XB�BB  BB�B�BɺBŢBŢB�^B�Bq�BS�BB�B7LB �B�B
��B
�B
�5B
�3B
��B
��B
x�B
VB
J�B
K�B
>wB
33B
�B
JB	��B	�B	�HB	��B	�qB	�FB	��B	��B	�hB	t�B	M�B	:^B	(�B	!�B	�B		7B	�B	bB	hB	VB	\B	
=B	JB	JB	
=B	+B��B	VB	JB	JB	
=B		7B	%B	B	B��B��B�ZB�/B�B��B��B��BɺBǮB�qB�-B��B��B��B��B��B��B��B��B��B�uB�hB�hB�\B�VB�JB�=B�1B�+B�%B�B�B�B�B�B�B�B�B�B|�B|�B{�B{�By�By�By�Bz�Bz�Bx�Bv�Bu�Bt�Bt�Br�Bq�Bq�Bn�Bn�Br�Bq�Bp�Bp�Bn�Bo�Bm�Bk�Bl�Bk�Bk�BjBhsBhsBgmBgmBhsBhsBl�BgmBgmBgmBhsBdZBe`BdZBffBffBffBgmBhsBiyBiyBk�Bl�Bk�Bk�Bn�Bq�Br�Bm�BdZBk�Br�Bs�Bu�Bt�Bv�Bw�B~�B�B}�B}�B~�B�B�+B�DB�uB�{B��B��B�{B��B��B��B��B��B��B��B�3B�XB��B��B��B��B��B��B��B��B��BȴBǮB��B��B��B�
B�#B�BB�`B�B�B�B�B�B�B�B�yB�fB�HB�5B�)B�
B�B�
B�B�
B�B�B�B�/B�NB�yB�B�B��B	1B	1B	+B	DB	\B	uB	{B	{B	{B	{B	{B	{B	{B	�B	�B	�B	�B	#�B	'�B	+B	/B	2-B	5?B	6FB	8RB	@�B	O�B	R�B	XB	\)B	]/B	_;B	l�B	u�B	|�B	� B	z�B	{�B	{�B	|�B	z�B	x�B	y�B	{�B	}�B	~�B	~�B	�B	�B	�1B	�=B	�JB	�\B	�hB	�hB	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�-B	�9B	�?B	�FB	�?B	�FB	�FB	�RB	�RB	�RB	�XB	�^B	�XB	�XB	�^B	�jB	��B	��B	��B	B	ÖB	ÖB	ĜB	ĜB	ĜB	ĜB	ŢB	ŢB	ŢB	ǮB	ǮB	ƨB	ȴB	ɺB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447312012010314473120120103144731  AO  ARGQ                                                                        20111130142905  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142905  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144731  IP                  G�O�G�O�G�O�                