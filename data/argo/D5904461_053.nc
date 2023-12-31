CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-06-08T19:16:41Z AOML 3.0 creation; 2016-08-07T21:36:36Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150608191641  20160807143636  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               5A   AO  5286_8897_053                   2C  D   APEX                            6531                            072314                          846 @�V�+��1   @�V軻��@1�S����c����S�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    5A   B   B   @���@�  A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE�fDF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�3D��D�<�D�i�D��fD�  D�` D�vfD���D��3D�6fD���D�ɚD��3D�)�D�|�D�� D�  D�<�D�p D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�33A��A!��AA��Aa��A���A���A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8��B@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�ffB�33B�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE��DFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDtfDt�fDt�3Dy��D��D�@ D�l�D�əD�3D�c3D�y�D�� D��fD�9�D�� D���D��fD�,�Dڀ D��3D�3D�@ D�s3D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�9XA�"�A�"�A�"�A�-A�=qA�5?A�5?A�7LA��A��`A�O�A�;dAң�A�v�A�A���A�r�A���A���A�ĜA�&�A���A�v�A�bA̛�A�S�A�l�A��mAʮA�K�A���A��`A�p�AȁA� �Aş�AĴ9A�%A\A��mA�A�1A��A�`BA��-A�;dA�33A�n�A���A��wA�{A��HA��#A�`BA�oA��mA�O�A�A�E�A�ȴA��FA���A�/A�G�A�A�A�A��A��TA�~�A��^A�ZA��PA���A��TA���A�VA��A�/A�ĜA��\A�z�A�I�A���A�ĜA�{A�5?A�(�A�1A�ƨA���A�VA���A���A�ffA���A�=qA���A�bNA�jA��A��RA�E�A��A��A���A��-A���A���A���A}l�At�yAp^5Ao/Am�Aj�Ae�^A_�A]�A[�7AY�-AW��AT��ATbAQXAO�FAN�HAN1AL��AK33AJz�AI7LAH=qAF5?AC&�A@�A>Q�A<A�A:��A:A�A7dZA4�yA4E�A3�FA1��A/��A.{A+�A*{A)�wA(�A&�A$jA"�9A!��A ��A��A�A|�A��A�-A7LAv�A��A��A`BA��AĜA��AI�A�PAjA?}AffA�;AhsA33AjA�-A(�A�FA?}A��AȴA-At�A	��A	��A	%AbNA�A7LA9XA?}A�!AI�AAdZAQ�A�FA;dA ��A ��@�\)@��@�&�@�I�@�ff@��F@�+@���@���@���@� �@�V@�Ĝ@�w@�"�@�&�@�A�@�F@旍@�-@��@�w@���@�ff@��@��@�Z@ާ�@���@ݡ�@�$�@�l�@ޟ�@ܼj@ڧ�@�X@�Ĝ@�1'@׶F@ם�@�t�@ם�@��@���@��m@׾w@׍P@��y@�ff@��@�hs@��`@�V@��/@�9X@ЋD@Л�@Ѓ@�bN@�Q�@��@��@ϥ�@�dZ@Ο�@ͺ^@�?}@�z�@˕�@ʟ�@�5?@�{@�&�@ǶF@���@�ȴ@�n�@Ə\@ư!@Ɵ�@ȣ�@ȋD@�bN@�Z@�A�@�1@��;@Ǯ@ǅ@�C�@�
=@��T@ř�@�7L@ă@�bN@�Z@�Q�@��@��@��@�1@�33@�K�@�\)@�1@�Q�@��;@��;@���@�C�@�@�ȴ@�v�@��^@�7L@�O�@�&�@��@��@�+@��@��\@���@���@���@��@�(�@��m@��m@���@���@�@���@���@���@�~�@�n�@�V@�{@��T@��`@��@���@��@�t�@�;d@�
=@��y@��@��y@�n�@���@�bN@��;@�  @��w@�
=@�-@�hs@�/@��`@��9@��u@�z�@� �@�1@�  @���@�C�@��y@���@�E�@�7L@��@�G�@�%@���@��@�G�@�x�@�x�@�X@�O�@�/@��/@���@�I�@��
@�S�@�@��y@��H@��!@�V@���@��`@�bN@��m@�|�@�\)@�C�@��@�
=@�@��@��R@���@���@���@���@���@��\@�~�@�ff@���@�G�@��@�Ĝ@��u@�r�@�Q�@�9X@��m@�l�@���@�v�@�E�@���@�x�@��@���@��@�Ĝ@�Ĝ@�Q�@��m@��
@��w@�|�@�\)@�K�@���@��\@�n�@�=q@�E�@�E�@�=q@��@���@��T@��^@�X@��u@�A�@�K�@�33@�o@���@��@�~�@�E�@�@��h@��7@�X@��/@���@�Z@�Q�@�I�@�A�@�(�@�b@���@��P@�+@�
=@��@��H@��@�ff@��@��-@�p�@���@��@��@�Z@x�u@k��@a��@VE�@O�@Fv�@?��@;C�@5�@-��@'�P@"�!@{@�#@�h@��@5?111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�9XA�"�A�"�A�"�A�-A�=qA�5?A�5?A�7LA��A��`A�O�A�;dAң�A�v�A�A���A�r�A���A���A�ĜA�&�A���A�v�A�bA̛�A�S�A�l�A��mAʮA�K�A���A��`A�p�AȁA� �Aş�AĴ9A�%A\A��mA�A�1A��A�`BA��-A�;dA�33A�n�A���A��wA�{A��HA��#A�`BA�oA��mA�O�A�A�E�A�ȴA��FA���A�/A�G�A�A�A�A��A��TA�~�A��^A�ZA��PA���A��TA���A�VA��A�/A�ĜA��\A�z�A�I�A���A�ĜA�{A�5?A�(�A�1A�ƨA���A�VA���A���A�ffA���A�=qA���A�bNA�jA��A��RA�E�A��A��A���A��-A���A���A���A}l�At�yAp^5Ao/Am�Aj�Ae�^A_�A]�A[�7AY�-AW��AT��ATbAQXAO�FAN�HAN1AL��AK33AJz�AI7LAH=qAF5?AC&�A@�A>Q�A<A�A:��A:A�A7dZA4�yA4E�A3�FA1��A/��A.{A+�A*{A)�wA(�A&�A$jA"�9A!��A ��A��A�A|�A��A�-A7LAv�A��A��A`BA��AĜA��AI�A�PAjA?}AffA�;AhsA33AjA�-A(�A�FA?}A��AȴA-At�A	��A	��A	%AbNA�A7LA9XA?}A�!AI�AAdZAQ�A�FA;dA ��A ��@�\)@��@�&�@�I�@�ff@��F@�+@���@���@���@� �@�V@�Ĝ@�w@�"�@�&�@�A�@�F@旍@�-@��@�w@���@�ff@��@��@�Z@ާ�@���@ݡ�@�$�@�l�@ޟ�@ܼj@ڧ�@�X@�Ĝ@�1'@׶F@ם�@�t�@ם�@��@���@��m@׾w@׍P@��y@�ff@��@�hs@��`@�V@��/@�9X@ЋD@Л�@Ѓ@�bN@�Q�@��@��@ϥ�@�dZ@Ο�@ͺ^@�?}@�z�@˕�@ʟ�@�5?@�{@�&�@ǶF@���@�ȴ@�n�@Ə\@ư!@Ɵ�@ȣ�@ȋD@�bN@�Z@�A�@�1@��;@Ǯ@ǅ@�C�@�
=@��T@ř�@�7L@ă@�bN@�Z@�Q�@��@��@��@�1@�33@�K�@�\)@�1@�Q�@��;@��;@���@�C�@�@�ȴ@�v�@��^@�7L@�O�@�&�@��@��@�+@��@��\@���@���@���@��@�(�@��m@��m@���@���@�@���@���@���@�~�@�n�@�V@�{@��T@��`@��@���@��@�t�@�;d@�
=@��y@��@��y@�n�@���@�bN@��;@�  @��w@�
=@�-@�hs@�/@��`@��9@��u@�z�@� �@�1@�  @���@�C�@��y@���@�E�@�7L@��@�G�@�%@���@��@�G�@�x�@�x�@�X@�O�@�/@��/@���@�I�@��
@�S�@�@��y@��H@��!@�V@���@��`@�bN@��m@�|�@�\)@�C�@��@�
=@�@��@��R@���@���@���@���@���@��\@�~�@�ff@���@�G�@��@�Ĝ@��u@�r�@�Q�@�9X@��m@�l�@���@�v�@�E�@���@�x�@��@���@��@�Ĝ@�Ĝ@�Q�@��m@��
@��w@�|�@�\)@�K�@���@��\@�n�@�=q@�E�@�E�@�=q@��@���@��T@��^@�X@��u@�A�@�K�@�33@�o@���@��@�~�@�E�@�@��h@��7@�X@��/@���@�Z@�Q�@�I�@�A�@�(�@�b@���@��P@�+@�
=@��@��H@��@�ff@��@��-G�O�@���@��@��@�Z@x�u@k��@a��@VE�@O�@Fv�@?��@;C�@5�@-��@'�P@"�!@{@�#@�h@��@5?111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	p�B	o�B	o�B	o�B	o�B	r�B	o�B	o�B	q�B	x�B	�7B	�}B
�B
|�B
�jB
��B
��B
�B
��B�B �B-B/B.B33B?}BE�BbNB|�B�VB�B��BĜB�
B�B�BDB8RBS�B_;BiyB� B��B��B��B��B�?B�dB�}BƨBȴB��B�BB�B�B�fB�ZB�B��B��B�B��B�TB�ZB�B�B�TBȴB�RB��B��B�dB�XB�B��B�B�-B��B�B�JB�{B�bB�1BbNB<jB+BoB
=B��B��B��B�B��B�}B�wB��Bu�BjBe`BVB6FB%�B
��B
�HB
��B
�B
�VB
ffB
49B
�B	�B	�FB	�uB	�DB	z�B	\)B	I�B	,B	 �B	�B	\B		7B	  B��B��B�B�B�B�sB�TB�BB�#B�
B��BǮBÖB�}B�qB�dB�LB�RB�RB�LB�FB�LB�XB�^B�qB�}B�wB�qB�dB�qB�qB�qB�jB�^B�jB��B��BÖBÖBŢBǮBǮBǮBǮBǮBǮBǮBȴB��B��B��B��B��B��B�
B�)B�/B�#B�B�
B�B�B�
B�)B�B�)B�)B�)B�5B�/B�)B�#B�B�B�#B�`B�`B�`B�`B�ZB�ZB�`B�ZB�TB�TB�fB�fB�ZB�TB�ZB�ZB�B�yB�`B�ZB�HB�5B�/B�#B�B��B��B��B��B��B��B��B�B�
B�B�/B�B�B�yB�sB�B��B��B��B	B	B		7B	hB	�B	�B	�B	�B	"�B	"�B	$�B	&�B	)�B	6FB	49B	8RB	B�B	H�B	K�B	M�B	N�B	N�B	O�B	Q�B	T�B	ZB	W
B	VB	T�B	T�B	T�B	T�B	T�B	S�B	R�B	R�B	S�B	XB	\)B	`BB	dZB	v�B	|�B	}�B	}�B	}�B	� B	� B	�B	�B	�B	�B	�B	�B	�+B	�DB	�DB	�DB	�=B	�DB	�=B	�7B	�B	�B	�B	�B	�DB	�oB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�'B	�'B	�'B	�'B	�3B	�?B	�LB	�RB	�RB	�XB	�XB	�^B	�dB	�dB	�^B	�XB	�RB	�dB	�}B	�}B	�wB	�dB	�XB	�XB	�RB	�LB	�LB	�FB	�LB	�RB	�RB	�XB	�RB	�RB	�dB	�dB	�^B	�dB	�wB	B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�;B	�;B	�BB	�BB	�BB	�BB	�HB	�HB	�HB	�HB	�HB	�HB	�HB	�HB	�HB	�HB	�HB	�NB	�TB	�ZB	�ZB	�`B	�`B	�`B	�`B	�fB	�fB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
  B
  B
B
B
B
B
PB
uB
�B
 �B
%�B
.B
5?B
;dB
B�B
G�B
J�B
N�B
VB
]/B
`BB
e`B
iyB
m�B
p�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	p�B	o�B	o�B	o�B	o�B	r�B	o�B	o�B	q�B	x�B	�;B	��B
�B
|�B
�fB
��B
��B
�
B
��B�B �B-B/B.B3.B?xBE�BbFB|�B�MB�	B�{BĕB�B�wB�B:B8GBS�B_4BinB�B�xB��B��B��B�:B�[B�vBƦBȪB��B�;B�B�B�]B�UB�B��B��B�}B��B�OB�UB�B�B�OBȮB�MB��B��B�[B�OB�B��B�B�$B��B�
B�@B�rB�YB�(BbCB<cB*�BeB
6B��B��B��B�B��B�uB�kB��Bu�BjxBeYBU�B6?B%�B
��B
�CB
��B
�B
�QB
fbB
47B
�B	�B	�EB	�xB	�GB	z�B	\,B	I�B	,B	 �B	�B	bB		?B	 B��B��B�B�B�B�xB�^B�HB�+B�B��BǶBßB��B�zB�lB�TB�[B�\B�SB�MB�UB�`B�eB�wB��B�}B�zB�lB�yB�xB�yB�qB�gB�sB��B��BÞBÜBũBǴBǴBǴBǲBǵBǳBǳBȺB��B��B��B��B��B��B�B�/B�3B�'B�B�B�B�
B�B�/B�#B�.B�/B�-B�;B�3B�/B�%B�$B�B�'B�cB�eB�eB�dB�_B�^B�fB�^B�XB�XB�kB�lB�^B�XB�_B�_B�B�|B�cB�_B�MB�:B�4B�(B�B��B��B��B��B��B��B��B�B�B�B�2B�B�B�B�wB�B��B��B��B		B	B		9B	kB	�B	�B	�B	�B	"�B	"�B	$�B	&�B	)�B	6FB	4:B	8RB	B�B	H�B	K�B	M�B	N�B	N�B	O�B	Q�B	T�B	ZB	WB	VB	T�B	T�B	T�B	T�B	T�B	S�B	R�B	R�B	S�B	XB	\'B	`?B	dXB	v�B	|�B	}�B	}�B	}�B	�B	�B	�B	�B	�
B	�
B	�B	�B	�'B	�@B	�BB	�BB	�<B	�@B	�<B	�3B	�B	�B	�	B	�B	�BB	�jB	�wB	�wB	�wB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�"B	�1B	�:B	�FB	�MB	�MB	�SB	�SB	�YB	�`B	�bB	�[B	�RB	�JB	�_B	�wB	�vB	�uB	�_B	�RB	�SB	�NB	�HB	�GB	�CB	�HB	�NB	�KB	�SB	�KB	�NB	�_B	�`B	�ZB	�_B	�vB	B	ɶB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�+B	�/B	�6B	�8B	�<B	�=B	�=B	�:B	�BB	�BB	�CB	�@B	�CB	�BB	�BB	�BB	�BB	�CB	�AB	�IB	�NB	�QB	�UB	�[B	�ZB	�ZB	�[B	�aB	�bB	�nB	�pB	�tB	�xB	�wB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 B
 �B
 B
 B	��B	��B
 �B
 �G�O�B
B
JB
oB
�B
 �B
%�B
.B
55B
;^B
B�B
G�B
J�B
N�B
U�B
](B
`9B
eYB
ipB
m�B
p�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436362016080714363620160807143636  AO  ARCAADJP                                                                    20150608191641    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150608191641  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150608191641  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143636  IP                  G�O�G�O�G�O�                