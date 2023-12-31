CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:28Z AOML 3.0 creation; 2016-06-01T00:08:13Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230828  20160531170813  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               .A   AO  4055_7112_046                   2C  D   APEX                            5374                            041511                          846 @֠� ���1   @֠��� @:5?|��cwn��P1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    .A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B ��B33B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
fD
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDys3D�	�D�@ D��3D�� D��D�I�D��fD��3D� D�VfD��fDǶfD�fD�I�DچfD�fD�	�D�FfD�y�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�33A��A!��AA��Aa��A���A���A���A���A���A���A���A�B33B��BffBffB ffB(ffB0ffB8ffB@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
�D
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDtfDt�fDt��Dyy�D��D�C3D��fD��3D� D�L�D���D��fD�3D�Y�D���Dǹ�D�	�D�L�Dډ�D๙D��D�I�D�|�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��/A��A��#A��#A��yA��HA��;A���A���A���A���A��;A��;A���A�XA��A���A�"�A�
=A�%A�E�A��TA��A�ZA�1'A�%A�ZA�C�A��yA�I�A���A�1A�ĜA�bNA��A�v�A�JA�x�A��A�/A�1A��
A���A�|�A�hsA�M�A��;A��hA��A�ĜA�ZA�JA�ƨA�|�A�;dA��^A�p�A�oA���A�n�A�ffA�"�A���A��;A�M�A���A�+A���A�JA�S�A�7LA�+A���A�A�A��A��`A�A�A�|�A���A�z�A�ƨA�O�A���A��A��A�A���A���A�A�A���A�~�A�oA���A���A��+AƨA{�7AzĜAzI�Ay�AyAxbNAv �Aq�PAn��Am�mAl�RAh��AeXAd1'Ac%A`A�A]XA[�AZZAX��AT��AQ�;AO\)ANjAN�AM�AL�!ALQ�AJ�AJv�AJffAJAIC�AH�9AGAF�\AE�AD��AC�TAC��ACS�AB�AB�!ABv�AA�A@ĜA?XA>�A=�A=C�A<��A<�+A<v�A<{A;+A9��A8��A8I�A7�;A6VA4��A3A2��A2ZA1��A0�DA.��A.VA-�wA,�\A,JA*�A*ffA)��A)oA(�HA(�\A(  A'��A'O�A&�/A&(�A%�^A%hsA$�A#�A"�A"��A"M�A"5?A ��AK�A��A�-A��A�At�AS�A�AA�A  A�A%A-AK�Az�A(�A{A�mA�A��A�A�AoAz�A��AjAĜA�wAt�A/A�A
ffA	|�Ap�A�!A��A$�A�+A9XA�mA��A �@��@��#@��;@�dZ@�n�@��@��@���@�1@�r�@��@�X@���@�@��;@�$�@�b@�\)@�E�@���@�  @�dZ@�n�@ݑh@ܬ@�\)@�o@��H@�n�@�O�@�%@��;@�@���@�  @�\)@�5?@�G�@ϕ�@Χ�@��@ͺ^@��`@˾w@���@�5?@�/@ǥ�@�=q@�X@�9X@�  @��y@�%@��@�"�@�V@��T@�p�@�Ĝ@���@���@��/@���@�bN@�b@�t�@�@��@�z�@��@�M�@�7L@�j@��;@�o@�M�@��T@�`B@���@�j@�A�@�1@�K�@�ȴ@���@�5?@��#@�O�@��/@���@� �@���@�S�@���@�V@��@�7L@�  @�t�@�C�@�o@���@�@�&�@���@�A�@��@�o@��!@�n�@�ff@�^5@�5?@��T@���@�?}@�Ĝ@���@��P@�K�@��y@�ȴ@�=q@��#@�x�@��@��j@�j@�A�@��@��m@���@�t�@��@��\@�E�@��T@�G�@��u@�1'@�1@��
@��@�l�@���@��@��@��^@�x�@�G�@��u@�1@��;@���@�dZ@�"�@��R@�-@��T@�`B@�&�@��@���@�A�@���@���@�33@��y@���@�~�@�M�@��@��@��h@�p�@�X@��@��@��j@� �@��m@��
@���@�S�@��H@���@��!@�n�@�5?@�{@�J@�@���@��T@��h@�p�@�hs@�/@���@��j@���@�bN@�1@��@�ƨ@��F@�|�@�+@�o@��R@�~�@��\@�^5@�5?@�J@���@�?}@�V@��@��@���@��@�r�@�9X@�@��@K�@~�R@~5?@}��@}O�@|��@|I�@|�@{�m@{C�@{"�@z��@zM�@y��@yx�@yhs@yhs@yX@yG�@y�@xĜ@xA�@w��@wK�@w;d@v��@v�@vff@u��@u�h@u�@t��@q��@i�@c�@\�@U`B@O
=@G�w@CS�@>�+@81'@2�@,��@&{@"-@��@&�@�@l�@��@bN@�/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��/A��A��#A��#A��yA��HA��;A���A���A���A���A��;A��;A���A�XA��A���A�"�A�
=A�%A�E�A��TA��A�ZA�1'A�%A�ZA�C�A��yA�I�A���A�1A�ĜA�bNA��A�v�A�JA�x�A��A�/A�1A��
A���A�|�A�hsA�M�A��;A��hA��A�ĜA�ZA�JA�ƨA�|�A�;dA��^A�p�A�oA���A�n�A�ffA�"�A���A��;A�M�A���A�+A���A�JA�S�A�7LA�+A���A�A�A��A��`A�A�A�|�A���A�z�A�ƨA�O�A���A��A��A�A���A���A�A�A���A�~�A�oA���A���A��+AƨA{�7AzĜAzI�Ay�AyAxbNAv �Aq�PAn��Am�mAl�RAh��AeXAd1'Ac%A`A�A]XA[�AZZAX��AT��AQ�;AO\)ANjAN�AM�AL�!ALQ�AJ�AJv�AJffAJAIC�AH�9AGAF�\AE�AD��AC�TAC��ACS�AB�AB�!ABv�AA�A@ĜA?XA>�A=�A=C�A<��A<�+A<v�A<{A;+A9��A8��A8I�A7�;A6VA4��A3A2��A2ZA1��A0�DA.��A.VA-�wA,�\A,JA*�A*ffA)��A)oA(�HA(�\A(  A'��A'O�A&�/A&(�A%�^A%hsA$�A#�A"�A"��A"M�A"5?A ��AK�A��A�-A��A�At�AS�A�AA�A  A�A%A-AK�Az�A(�A{A�mA�A��A�A�AoAz�A��AjAĜA�wAt�A/A�A
ffA	|�Ap�A�!A��A$�A�+A9XA�mA��A �@��@��#@��;@�dZ@�n�@��@��@���@�1@�r�@��@�X@���@�@��;@�$�@�b@�\)@�E�@���@�  @�dZ@�n�@ݑh@ܬ@�\)@�o@��H@�n�@�O�@�%@��;@�@���@�  @�\)@�5?@�G�@ϕ�@Χ�@��@ͺ^@��`@˾w@���@�5?@�/@ǥ�@�=q@�X@�9X@�  @��y@�%@��@�"�@�V@��T@�p�@�Ĝ@���@���@��/@���@�bN@�b@�t�@�@��@�z�@��@�M�@�7L@�j@��;@�o@�M�@��T@�`B@���@�j@�A�@�1@�K�@�ȴ@���@�5?@��#@�O�@��/@���@� �@���@�S�@���@�V@��@�7L@�  @�t�@�C�@�o@���@�@�&�@���@�A�@��@�o@��!@�n�@�ff@�^5@�5?@��T@���@�?}@�Ĝ@���@��P@�K�@��y@�ȴ@�=q@��#@�x�@��@��j@�j@�A�@��@��m@���@�t�@��@��\@�E�@��T@�G�@��u@�1'@�1@��
@��@�l�@���@��@��@��^@�x�@�G�@��u@�1@��;@���@�dZ@�"�@��R@�-@��T@�`B@�&�@��@���@�A�@���@���@�33@��y@���@�~�@�M�@��@��@��h@�p�@�X@��@��@��j@� �@��m@��
@���@�S�@��H@���@��!@�n�@�5?@�{@�J@�@���@��T@��h@�p�@�hs@�/@���@��j@���@�bN@�1@��@�ƨ@��F@�|�@�+@�o@��R@�~�@��\@�^5@�5?@�J@���@�?}@�V@��@��@���@��@�r�@�9X@�@��@K�@~�R@~5?@}��@}O�@|��@|I�@|�@{�m@{C�@{"�@z��@zM�@y��@yx�@yhs@yhs@yX@yG�@y�@xĜ@xA�@w��@wK�@w;d@v��@v�@vff@u��@u�h@u�@t��@q��@i�@c�@\�@U`B@O
=@G�w@CS�@>�+@81'@2�@,��@&{@"-@��@&�@�@l�@��@bN@�/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBBBBBB��B��B��B��B��B��B��B��B�qB�RB��B�B�JB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�JB�+B�Bw�BhsBdZBcTBbNBaHB_;B^5B\)BXB[#BW
BVBP�BK�BE�B?}B9XB0!B+B$�B �B�B�B�BDB��B�B�5B��BŢB�RB��B�PBw�BXBI�B6FB%�B{B  B��B�!B��B~�Bk�BI�B0!B�B
��B
�`B
�B
�^B
��B
�PB
t�B
e`B
N�B
33B
�B
bB
JB
+B
B	��B	�B	��B	�}B	�LB	�B	�bB	|�B	q�B	ffB	O�B	;dB	/B	!�B	oB	  B�B�B�yB�sB�mB�mB�yB�sB�mB�fB�ZB�NB�;B�)B�B�B�B�B�B�
B��B��B��B��B��B��B��BȴBÖB��B��B��B��B��B�FB�-B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�uB�hB�\B�DB�%B�B�B�B�B�B� B}�By�Bv�Br�Bp�Bn�BjBgmBffBe`Be`BcTBaHB^5B[#BZBW
BT�BP�BK�BF�BC�BA�B?}B:^B6FB1'B-B)�B'�B#�B"�B!�B �B�B�B�B�B�B�B�BoBhBhB\BDB	7B	7B+B%B%B%B%B%B1B1B+B%B+B+B+B1B1B+B+B1B+B+B1B	7B
=B
=BDBDBVB\BbB\BbBoBoBuB{B�B�B�B�B�B�B�B"�B"�B$�B%�B%�B&�B'�B)�B.B/B/B/B/B33B5?B6FB7LB:^B<jB>wB?}BA�BD�BE�BF�BH�BI�BI�BI�BK�BM�BM�BN�BO�BQ�BS�BT�BW
BYBZB[#B^5B_;BaHBffBiyBiyBiyBjBm�Br�Br�Bv�By�B{�B~�B� B� B� B�B�B�B�%B�1B�VB�bB�hB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�'B�9B�LB�jB�}B��B��BBÖBǮBɺB��B��B��B��B�B�/B�5B�BB�NB�ZB�fB�B�B�B�B��B��B��B��B	  B	B	%B	1B	
=B	DB	VB	oB	{B	�B	�B	�B	�B	�B	!�B	$�B	'�B	+B	,B	-B	.B	0!B	33B	5?B	8RB	9XB	9XB	:^B	;dB	>wB	@�B	@�B	A�B	C�B	F�B	G�B	H�B	M�B	M�B	O�B	P�B	Q�B	S�B	T�B	W
B	ZB	[#B	_;B	`BB	`BB	`BB	bNB	ffB	k�B	m�B	n�B	o�B	o�B	s�B	t�B	w�B	z�B	{�B	}�B	�B	�B	�B	�B	�%B	�1B	�DB	�JB	�JB	�VB	�oB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	��B	�TB	��B
%B
oB
�B
%�B
.B
6FB
>wB
E�B
M�B
R�B
YB
_;B
dZB
jB
n�B
r�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BBBBBB��B��B��B��B��B��B��B��B�mB�OB��B�B�FB��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�wB�dB�IB�$B�Bw�BhlBdTBcPBbIBaBB_8B^2B\"BXB[BWBV BP�BK�BE�B?xB9PB0B*�B$�B �B�B�B~B:B��B�B�.B��BŘB�JB��B�IBw�BXBI�B6=B%�BtB��B˽B�B��B~�Bk}BI�B0B�B
��B
�YB
�B
�UB
��B
�IB
t�B
e\B
N�B
3/B
�B
^B
FB
*B
B	��B	�|B	��B	�}B	�JB	�B	�eB	|�B	q�B	fjB	O�B	;hB	/ B	!�B	vB	 B�B�B�B�|B�rB�tB�B�{B�vB�lB�`B�VB�DB�1B�#B�B�%B�B�B�B� B��B��B��B��B��B��BȽBÞB��B��B��B��B��B�OB�6B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�~B�~B�qB�dB�OB�0B�B�B�B�B�B�B}�By�Bv�Br�Bp�Bn�Bj�BgwBftBeiBejBc`BaUB^AB[0BZ+BWBUBP�BK�BF�BC�BA�B?�B:jB6RB13B-B*
B'�B#�B"�B!�B �B�B�B�B�B�B�BzB|B[B^BOB9B	+B	*B8BBB3BBB#B&B8BB9B7BB@B#BB9B(B8BB$B	+B
0B
/B6BRBdBkBVBiBUBzBaB�B�ByB�B�B�B�B�B�B"�B"�B$�B%�B%�B&�B'�B*B. B/&B/'B/'B/(B3?B5KB6RB7XB:jB<sB>�B?�BA�BD�BE�BF�BH�BI�BI�BI�BK�BM�BM�BN�BO�BQ�BT BUBWBY BZ%B[+B^>B_DBaPBfmBi�Bi�Bi�Bj�Bm�Br�Br�Bv�By�B{�BB�B�B�	B�B�B�B�,B�7B�]B�hB�oB�B��B��B��B��B��B��B��B��B��B��B��B�B�B�$B�,B�=B�QB�nB�B��B��BBÙBǱBɿB��B��B��B��B�B�/B�9B�EB�QB�\B�gB�B�B�B�B��B��B��B��B	 B	B	'B	4B	
>B	EB	XB	oB	|B	�B	�B	�B	�B	�B	!�B	$�B	'�B	+B	,B	-B	.B	0B	36B	5=B	8QB	9WB	9VB	:^B	;bB	>vB	@�B	@�B	A�B	C�B	F�B	G�B	H�B	M�B	M�B	O�B	P�B	Q�B	S�B	T�B	WB	ZB	[!B	_9B	`@B	`?B	`AB	bMB	fcB	k�B	m�B	n�B	o�B	o�B	s�B	t�B	w�B	z�B	{�B	}�B	�B	�
B	�B	�B	� B	�2B	�?B	�EB	�DB	�QB	�kB	�qB	�uB	�xB	�xB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�(B	��B	�LB	��B
!B
gB
�B
%�B
.B
6>B
>oB
E�B
M�B
R�B
YB
_4B
dRB
juB
n�B
r�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708132016053117081320160531170813  AO  ARCAADJP                                                                    20140721230828    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230828  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230828  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170813  IP                  G�O�G�O�G�O�                