CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-04-27T21:12:42Z AOML 3.0 creation; 2016-08-07T21:36:34Z UW 3.1 conversion     
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
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20150427211242  20160807143634  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               -A   AO  5286_8897_045                   2C  D   APEX                            6531                            072314                          846 @�LUU��1   @�LV�/�@2�-V�c���"��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    -A   B   B   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz�C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	y�D
  D
�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D��D�Y�D���D���D�	�D�L�D�|�D��3D�fD�0 D��fD�� D��D�FfDچfD�ٚD�  D�6fD�3D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @Fff@�33@�33A��A!��AA��Aa��A���A���A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C�C�C�C�C
�C�C�C�C34C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz34C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	� D
fD
��DfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#�D#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDtfDt�fDtٙDy� D� D�\�D�� D���D��D�P D�� D��fD�	�D�33D���D��3D� D�I�Dډ�D���D�3D�9�D�fD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��;AѼjA��A�G�A� �A�{A�%A���A��A��mA��/A���Aϝ�A�=qA�z�A�JA͡�A�XA�(�A�bA���A��A��A��/A��
A��A��#A��#A��#A��#A��
A�ȴA�~�A���Aˇ+A�7LA��yAʼjA�ffA�bNA�t�AʅA�dZA�+A�$�A��A��AɮAɕ�AɁA�S�A��A��mAȧ�A�|�A�v�A�K�A��`AǃA�K�A�33A��A��yAƁA�9XA�"�A�K�AËDA+A��;A�1'A�S�A��#A��DA�G�A��A���A���A�bNA��\A���A�l�A���A�t�A�E�A�ȴA�9XA��`A���A�oA��A�dZA��A��PA�JA���A�S�A�
=A�
=A�A�z�A��#A�/A�ĜA�ȴA��A�n�A�{A���A��A�r�A��
A��+A�ffA�M�A�A�A� �A��A���A�^5A�XA�E�A���A��A��A�&�A}�
A|�+AyS�Au�PAp�/Ap�DAo��AnM�Ag`BAe�AcVA^E�AX(�AUl�AS�mARv�AN�ALĜAK33AJn�AI��AIC�AG�AG�AB��AA�A@��A>bA<Q�A;S�A:A89XA7|�A6�A5�wA4A�A3K�A1�A1|�A0jA.�A*M�A(�!A'`BA#A"VA"=qA �A��A"�A��A�AAA�TAE�Az�A=qAJA��Ax�A�!A�RA�A�`A�A%A�`A  A��A1'A�A�AȴA��A9XA�TAA�A
�A	��A	��A	t�A��A  A��A��A�9AE�A�#A�FA;dA�A�At�A ��A ��A ��A @��@�&�@��@��@�G�@� �@��!@�hs@��@�j@��#@�Z@�J@�n�@�(�@�^@�dZ@�v�@���@�x�@�7L@���@�r�@ܬ@�o@�33@�&�@�M�@���@���@ڰ!@�O�@�(�@�&�@�7L@�v�@�I�@� �@Լj@Ұ!@��m@�G�@�A�@�t�@�dZ@�C�@�+@��H@ʸR@��y@�1'@��H@Ƈ+@�$�@��@ļj@�Z@�|�@�"�@°!@�^5@�J@�X@��@���@��u@��u@���@�|�@�K�@���@�n�@�{@���@���@���@�&�@��@�Z@��;@��;@���@��y@���@���@��H@���@�v�@�v�@�^5@�x�@�(�@���@��P@��T@�&�@���@���@���@�$�@��T@�@���@�G�@���@���@�  @��@�ȴ@��R@�n�@��R@�v�@�@��7@��@��D@�j@�I�@�  @�ƨ@�l�@��@���@��@�bN@���@�33@�@��@��+@�@�%@��D@��m@�;d@�E�@���@�x�@�X@�?}@�%@��D@��m@�ƨ@��;@��@��@�j@�Q�@�1@�
=@�-@�hs@�Ĝ@��@�(�@��w@�dZ@�C�@��!@��\@���@���@�ff@�$�@��@��^@��-@�`B@��@�Ĝ@��@��@��@�\)@�+@���@���@��+@�=q@���@�x�@�7L@�V@��/@���@�z�@�1'@��m@�dZ@�+@���@���@�5?@���@��@��#@�@��^@�hs@��D@� �@�b@��m@��F@�K�@���@�E�@��@�@��-@�`B@���@�Ĝ@���@��@�1'@�t�@�K�@�+@��@�@���@���@���@��@���@���@���@��\@�^5@�J@��#@���@��7@�G�@�%@���@�Ĝ@�r�@�j@�1'@��@��F@�|�@�dZ@�\)@�S�@�33@�+@�o@��@��R@�n�@�{@�J@�@��@���@���@���@���@��@�hs@���@�%@�b@t�D@mV@f5?@]`B@So@IX@D�j@?�@9��@4�@+�@&��@ 1'@Z@��@@�@�`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A��;AѼjA��A�G�A� �A�{A�%A���A��A��mA��/A���Aϝ�A�=qA�z�A�JA͡�A�XA�(�A�bA���A��A��A��/A��
A��A��#A��#A��#A��#A��
A�ȴA�~�A���Aˇ+A�7LA��yAʼjA�ffA�bNA�t�AʅA�dZA�+A�$�A��A��AɮAɕ�AɁA�S�A��A��mAȧ�A�|�A�v�A�K�A��`AǃA�K�A�33A��A��yAƁA�9XA�"�A�K�AËDA+A��;A�1'A�S�A��#A��DA�G�A��A���A���A�bNA��\A���A�l�A���A�t�A�E�A�ȴA�9XA��`A���A�oA��A�dZA��A��PA�JA���A�S�A�
=A�
=A�A�z�A��#A�/A�ĜA�ȴA��A�n�A�{A���A��A�r�A��
A��+A�ffA�M�A�A�A� �A��A���A�^5A�XA�E�A���A��A��A�&�A}�
A|�+AyS�Au�PAp�/Ap�DAo��AnM�Ag`BAe�AcVA^E�AX(�AUl�AS�mARv�AN�ALĜAK33AJn�AI��AIC�AG�AG�AB��AA�A@��A>bA<Q�A;S�A:A89XA7|�A6�A5�wA4A�A3K�A1�A1|�A0jA.�A*M�A(�!A'`BA#A"VA"=qA �A��A"�A��A�AAA�TAE�Az�A=qAJA��Ax�A�!A�RA�A�`A�A%A�`A  A��A1'A�A�AȴA��A9XA�TAA�A
�A	��A	��A	t�A��A  A��A��A�9AE�A�#A�FA;dA�A�At�A ��A ��A ��A @��@�&�@��@��@�G�@� �@��!@�hs@��@�j@��#@�Z@�J@�n�@�(�@�^@�dZ@�v�@���@�x�@�7L@���@�r�@ܬ@�o@�33@�&�@�M�@���@���@ڰ!@�O�@�(�@�&�@�7L@�v�@�I�@� �@Լj@Ұ!@��m@�G�@�A�@�t�@�dZ@�C�@�+@��H@ʸR@��y@�1'@��H@Ƈ+@�$�@��@ļj@�Z@�|�@�"�@°!@�^5@�J@�X@��@���@��u@��u@���@�|�@�K�@���@�n�@�{@���@���@���@�&�@��@�Z@��;@��;@���@��y@���@���@��H@���@�v�@�v�@�^5@�x�@�(�@���@��P@��T@�&�@���@���@���@�$�@��T@�@���@�G�@���@���@�  @��@�ȴ@��R@�n�@��R@�v�@�@��7@��@��D@�j@�I�@�  @�ƨ@�l�@��@���@��@�bN@���@�33@�@��@��+@�@�%@��D@��m@�;d@�E�@���@�x�@�X@�?}@�%@��D@��m@�ƨ@��;@��@��@�j@�Q�@�1@�
=@�-@�hs@�Ĝ@��@�(�@��w@�dZ@�C�@��!@��\@���@���@�ff@�$�@��@��^@��-@�`B@��@�Ĝ@��@��@��@�\)@�+@���@���@��+@�=q@���@�x�@�7L@�V@��/@���@�z�@�1'@��m@�dZ@�+@���@���@�5?@���@��@��#@�@��^@�hs@��D@� �@�b@��m@��F@�K�@���@�E�@��@�@��-@�`B@���@�Ĝ@���@��@�1'@�t�@�K�@�+@��@�@���@���@���@��@���@���@���@��\@�^5@�J@��#@���@��7@�G�@�%@���@�Ĝ@�r�@�j@�1'@��@��F@�|�@�dZ@�\)@�S�@�33@�+@�o@��@��R@�n�@�{@�J@�@��@���@���@���@���@��G�O�@���@�%@�b@t�D@mV@f5?@]`B@So@IX@D�j@?�@9��@4�@+�@&��@ 1'@Z@��@@�@�`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	m�B	l�B	l�B	l�B	k�B	k�B	l�B	k�B	l�B	l�B	l�B	l�B	l�B	r�B	�JB	��B	��B	�!B	�9B	�RB	�qB	�}B	��B	��B	B	ÖB	ĜB	ĜB	ŢB	ƨB	ƨB	��B	�NB
JB
.B
E�B
\)B
hsB
�7B
��B
��B
��B
�B
��BJB33BS�BcTBe`BffBl�B}�B�hB��B�FB�XBĜB�B�`B�B�B�B��B\B�B"�B8RBVBcTBiyBo�Bv�B{�B}�B�+B�JB�VB�oB�{B��B��B�PB�Bw�Bl�Bo�Bn�Bl�B[#BG�B?}B)�BoB��B��B�yB�B�qB�B�7Bk�BZB!�B
��B
�dB
��B
��B
��B
�{B
�1B
dZB
ZB
VB
S�B
R�B
Q�B
N�B
L�B
C�B
"�B
oB
%B	��B	��B	�HB	��B	�qB	�?B	�B	��B	�7B	�+B	� B	q�B	H�B	<jB	(�B	\B��B�B�`B�TB�#B��B��B��B��B��B��B��BŢBB��B�}B�qB�qB�qB�wB�wB�qB�wB��BÖBǮBƨBƨBƨBɺBǮBĜBŢBÖB��B�}B�jB�^B�LB�XB�XB��BĜB�wB�jB�dB�dB�^B�RB�LB�RBÖBƨBȴB��B��B��B��B��B�
B�B�B�B�B�B�B�B�5B�5B�;B�5B�;B�/B�#B�B�B�B�B�B�
B�B��B��B��B��B��B��B��B��B��B�B�B�B�B�#B�)B�B��BɺBĜB�XB�-B�'B�'B�-B�3B�9B�FB�LB�^BƨB�ZB�B�B�fB�NB�B�B�B�sB�B��B�B�sB�B�B�B�mB�TB�ZB�mB�mB�mB�fB�fB�sB�B�B�B�B�B�B�B�B��B��B��B��B	  B	DB	bB	{B	�B	�B	�B	�B	 �B	!�B	!�B	#�B	'�B	(�B	+B	/B	49B	7LB	;dB	>wB	K�B	[#B	[#B	\)B	`BB	bNB	e`B	gmB	hsB	hsB	e`B	m�B	k�B	hsB	gmB	hsB	hsB	ffB	k�B	m�B	n�B	n�B	n�B	n�B	o�B	q�B	u�B	v�B	w�B	y�B	}�B	�B	�B	�B	�1B	�=B	�JB	�PB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�-B	�-B	�3B	�?B	�LB	�XB	�XB	�^B	�dB	�jB	�qB	�wB	ĜB	��B	��B	��B	�B	�B	�B	�B	�#B	�B	�B	�)B	�#B	�5B	�BB	�BB	�BB	�NB	�TB	�ZB	�fB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
1B
+B
+B
+B
+B
+B
1B
1B
1B
	7B
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B
DB

=B

=B

=B
DB
DB
DB
DB
DB
JB
PB
uB
�B
 �B
$�B
(�B
/B
8RB
?}B
C�B
G�B
J�B
O�B
XB
]/B
bNB
ffB
jB
m�B
s�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 B	m�B	l�B	l�B	l�B	k�B	k�B	l�B	k�B	l�B	l�B	l�B	l�B	l�B	r�B	�NB	��B	��B	�$B	�:B	�VB	�vB	�B	��B	��B	B	ÚB	ğB	ğB	ţB	ƮB	ƪB	��B	�QB
JB
.B
E�B
\&B
hoB
�2B
��B
��B
��B
�yB
��BDB3-BS�BcOBe\Bf]Bl�B}�B�_B��B�AB�MBēB��B�XB�B�B�B��BWB�B"�B8IBU�BcOBirBo�Bv�B{�B}�B�$B�DB�NB�hB�uB��B��B�FB��Bw�BlBo�Bn�Bl�B[BG�B?tB)�BgB��B��B�nB�B�iB��B�/Bk{BZB!�B
��B
�^B
��B
��B
��B
�vB
�,B
dUB
ZB
V B
S�B
R�B
Q�B
N�B
L�B
C�B
"�B
kB
$B	��B	��B	�IB	��B	�oB	�?B	�B	��B	�9B	�-B	�B	q�B	H�B	<oB	(�B	cB��B�B�fB�_B�*B��B��B��B��B��B��B��BūBB��B��B�wB�zB�{B��B��B�{B�B��BÞBǴBƯBƮBưB��BǵBĢBŨBÜB��B��B�rB�fB�RB�`B�aB��BģB�~B�pB�lB�lB�eB�ZB�TB�YBÞBƮBȻB��B��B��B��B��B�B�B�B�B�B�B�B�B�;B�:B�AB�<B�AB�4B�(B�B�B�B�B�B�B�B��B��B��B��B��B�B��B��B��B�B�B�#B�B�&B�.B�
B��B��BĢB�]B�4B�*B�/B�4B�:B�@B�KB�SB�dBƭB�^B�B�B�jB�WB�B�B�B�uB�B��B�B�wB�B�B�B�pB�XB�\B�pB�pB�qB�jB�jB�vB�B�B�B�B�B�B�B�B��B��B��B��B	 B	CB	dB	}B	�B	�B	�B	�B	 �B	!�B	!�B	#�B	'�B	(�B	+B	/B	46B	7LB	;cB	>wB	K�B	[ B	[!B	\'B	`BB	bKB	eaB	gkB	hqB	huB	e^B	m�B	k�B	hsB	gkB	htB	hrB	fdB	k�B	m�B	n�B	n�B	n�B	n�B	o�B	q�B	u�B	v�B	w�B	y�B	}�B	�
B	�B	�B	�/B	�:B	�GB	�NB	�dB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�(B	�)B	�(B	�1B	�<B	�IB	�TB	�RB	�[B	�_B	�eB	�lB	�uB	ęB	ʼB	��B	��B	�B	�B	�B	�B	�B	�B	�B	�"B	�B	�1B	�<B	�=B	�=B	�GB	�PB	�UB	�aB	�gB	�pB	�nB	�tB	�tB	�uB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
'B
&B
'B
'B
)B
'B
&B
(B
$B
%B
.B
,B
*B
	1B
,B
*B
+B
*B
+B
	1B
	1B
	/B
	.B
	2B
	/B

6B

6B

5B
=B

7B

7B

5B
<B
=B
<B
<B
>G�O�B
HB
nB
�B
 �B
$�B
(�B
/B
8JB
?uB
C�B
G�B
J�B
O�B
XB
]&B
bFB
f]B
jwB
m�B
s�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436342016080714363420160807143634  AO  ARCAADJP                                                                    20150427211242    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150427211242  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150427211242  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143634  IP                  G�O�G�O�G�O�                